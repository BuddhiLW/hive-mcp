(ns hive-mcp.telemetry.logging
  "Structured logging with MDC context for Loki labels.

   CLARITY-T: Telemetry first - all requests get correlation IDs,
   all logs carry context for distributed tracing.

   MDC (Mapped Diagnostic Context) fields appear as:
   - Top-level JSON keys in LogstashEncoder output
   - Labels in Loki/Grafana queries
   - Correlation keys for request tracing

   Key MDC fields:
   - agent-id: Swarm agent identifier (e.g., 'ling-task-123')
   - correlation-id: Request UUID for tracing
   - event-type: Semantic event type (e.g., 'tool-call', 'swarm-spawn')
   - tool-name: MCP tool being executed
   - project-id: Project scope for memory operations"
  (:require [clojure.tools.logging :as log])
  (:import [org.slf4j MDC]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; MDC Context Management
;;; =============================================================================

(defn set-context!
  "Set MDC context values. Keys should be keywords, values converted to strings.
   Returns nil.

   Example:
     (set-context! {:agent-id \"ling-123\" :tool-name \"swarm_spawn\"})"
  [context-map]
  (doseq [[k v] context-map]
    (MDC/put (name k) (str v))))

(defn clear-context!
  "Clear specific MDC context keys.

   Example:
     (clear-context! [:agent-id :tool-name])"
  [keys]
  (doseq [k keys]
    (MDC/remove (name k))))

(defn clear-all-context!
  "Clear all MDC context. Use sparingly - prefer clear-context! for specific keys."
  []
  (MDC/clear))

(defn get-context
  "Get current MDC context as a map. Useful for debugging."
  []
  (into {} (MDC/getCopyOfContextMap)))

(defmacro with-context
  "Execute body with MDC context for structured logging.
   Context is automatically cleared after body executes.
   Context appears as labels in Loki queries.

   Example:
     (with-context {:agent-id \"ling-123\" :tool-name \"swarm_spawn\"}
       (log/info \"Processing request\")
       (do-work))

   In Loki, query with: {agent_id=\"ling-123\"} |= \"Processing request\""
  [context & body]
  `(let [ctx# ~context
         keys# (keys ctx#)]
     (try
       (set-context! ctx#)
       ~@body
       (finally
         (clear-context! keys#)))))

;;; =============================================================================
;;; Correlation IDs
;;; =============================================================================

(defn generate-correlation-id
  "Generate a new UUID for request correlation.
   Format: 8-4-4-4-12 hex digits (standard UUID)."
  []
  (str (java.util.UUID/randomUUID)))

(defn current-correlation-id
  "Get current correlation ID from MDC, or nil if not set."
  []
  (MDC/get "correlation-id"))

(defmacro with-correlation-id
  "Execute body with a correlation ID in MDC.
   If correlation-id is nil, generates a new one.

   Example:
     (with-correlation-id nil
       (log/info \"Starting request\")  ; correlation-id auto-generated
       (process-request))

     (with-correlation-id existing-id
       (log/info \"Continuing request\")  ; uses existing ID
       (continue-work))"
  [correlation-id & body]
  `(let [cid# (or ~correlation-id (generate-correlation-id))]
     (with-context {:correlation-id cid#}
       ~@body)))

(defmacro with-request-context
  "Convenience macro for MCP request handling.
   Sets correlation-id, tool-name, and optional agent-id.

   Example:
     (with-request-context {:tool \"swarm_spawn\" :agent-id \"ling-123\"}
       (log/info \"Handling tool call\")
       (handle-request))"
  [{:keys [tool agent-id correlation-id]} & body]
  `(let [cid# (or ~correlation-id (generate-correlation-id))
         ctx# (cond-> {:correlation-id cid#}
                ~tool (assoc :tool-name ~tool)
                ~agent-id (assoc :agent-id ~agent-id))]
     (with-context ctx#
       ~@body)))

;;; =============================================================================
;;; Structured Logging Helpers
;;; =============================================================================

(defn log-tool-call
  "Log an MCP tool call with standard structure.

   Example:
     (log-tool-call {:tool \"swarm_spawn\"
                     :args {:name \"task-1\" :presets [\"tdd\"]}
                     :agent-id \"coordinator\"})"
  [{:keys [tool args agent-id]}]
  (with-context (cond-> {:event-type "tool-call"
                         :tool-name tool}
                  agent-id (assoc :agent-id agent-id))
    (log/info "MCP tool call" {:tool tool :args (pr-str args)})))

(defn log-tool-result
  "Log an MCP tool result with standard structure.

   Example:
     (log-tool-result {:tool \"swarm_spawn\"
                       :success true
                       :duration-ms 150
                       :agent-id \"coordinator\"})"
  [{:keys [tool success duration-ms error agent-id]}]
  (with-context (cond-> {:event-type "tool-result"
                         :tool-name tool}
                  agent-id (assoc :agent-id agent-id))
    (if success
      (log/info "MCP tool success" {:tool tool :duration-ms duration-ms})
      (log/warn "MCP tool failure" {:tool tool :duration-ms duration-ms :error error}))))

(defn log-swarm-event
  "Log a swarm coordination event.

   Example:
     (log-swarm-event {:event-type \"ling-spawned\"
                       :agent-id \"ling-task-123\"
                       :data {:presets [\"tdd\" \"clarity\"]}})"
  [{:keys [event-type agent-id data]}]
  (with-context {:event-type event-type
                 :agent-id agent-id}
    (log/info "Swarm event" (merge {:event event-type :agent agent-id} data))))

(defn log-memory-operation
  "Log a memory/Chroma operation.

   Example:
     (log-memory-operation {:operation \"query\"
                            :project-id \"hive-mcp\"
                            :type :decision
                            :count 5})"
  [{:keys [operation project-id type count duration-ms]}]
  (with-context {:event-type "memory-op"
                 :project-id project-id}
    (log/info "Memory operation" {:op operation
                                  :type type
                                  :count count
                                  :duration-ms duration-ms})))

;;; =============================================================================
;;; Error Logging with Context
;;; =============================================================================

(defn log-error-with-context
  "Log an error with full MDC context preserved.
   Ensures correlation-id and agent-id are logged even in error cases.

   Example:
     (log-error-with-context {:message \"Failed to spawn ling\"
                              :error ex
                              :context {:attempted-name \"task-1\"}})"
  [{:keys [message error context]}]
  (let [current-ctx (get-context)
        error-data (merge {:message message
                           :error-type (some-> error class .getName)
                           :error-message (some-> error .getMessage)}
                          current-ctx
                          context)]
    (log/error error "Error with context" error-data)))

;;; =============================================================================
;;; Initialization
;;; =============================================================================

(defn init!
  "Initialize logging module. Sets up default MDC values.
   Call once at application startup."
  []
  ;; Set default service identifier
  (MDC/put "service" "hive-mcp")
  (log/info "Structured logging initialized" {:mdc-fields ["agent-id"
                                                           "correlation-id"
                                                           "event-type"
                                                           "tool-name"
                                                           "project-id"]}))

(comment
  ;; Usage examples

  ;; Basic context
  (with-context {:agent-id "ling-123" :tool-name "swarm_spawn"}
    (log/info "Processing request"))

  ;; Correlation ID for request tracing
  (with-correlation-id nil
    (log/info "Request started")
    (Thread/sleep 100)
    (log/info "Request completed"))

  ;; Full request context
  (with-request-context {:tool "magit_status" :agent-id "coordinator"}
    (log/info "Checking git status")
    {:status "clean"})

  ;; Structured logging helpers
  (log-tool-call {:tool "swarm_spawn"
                  :args {:name "task-1" :presets ["tdd"]}
                  :agent-id "coordinator"})

  (log-swarm-event {:event-type "ling-completed"
                    :agent-id "ling-task-123"
                    :data {:duration-ms 5000 :files-modified 3}})

  ;; Check current context
  (get-context)

  ;; Initialize at startup
  (init!))
