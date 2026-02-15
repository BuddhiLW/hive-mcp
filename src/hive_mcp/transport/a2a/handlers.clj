(ns hive-mcp.transport.a2a.handlers
  "A2A JSON-RPC method handlers.

   Translates A2A methods (SendMessage, GetTask, CancelTask,
   SendStreamingMessage) to existing IAgent protocol calls.
   No new domain logic — reuses dispatch, query, and kill patterns."

  (:require [clojure.string :as str]
            [hive-mcp.transport.a2a.schema :as schema]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- extract-text-from-parts
  "Extract concatenated text from A2A message parts."
  [parts]
  (->> parts
       (filter #(= "text" (:type %)))
       (map :text)
       (str/join "\n")))

(defn- resolve-agent
  "Look up agent in DataScript and build IAgent instance.
   Returns [agent agent-data] or nil if not found."
  [agent-id]
  (when-let [agent-data (or (ds-queries/get-slave agent-id)
                            (ds-queries/get-slave-by-name agent-id))]
    (let [sid (:slave/id agent-data)
          agent-type (if (= 1 (:slave/depth agent-data)) :ling :drone)
          agent (case agent-type
                  :ling (ling/->ling sid {:cwd (:slave/cwd agent-data)
                                          :presets (:slave/presets agent-data)
                                          :project-id (:slave/project-id agent-data)
                                          :spawn-mode (or (:ling/spawn-mode agent-data) :vterm)})
                  :drone (drone/->drone sid {:cwd (:slave/cwd agent-data)
                                             :parent-id (:slave/parent agent-data)
                                             :project-id (:slave/project-id agent-data)}))]
      [agent agent-data])))

(defn- generate-task-id
  "Generate a unique task ID for A2A."
  []
  (str "a2a-" (System/currentTimeMillis) "-" (rand-int 10000)))

(defn- generate-context-id
  "Generate or reuse a context ID for A2A task grouping."
  [params]
  (or (:contextId params)
      (str "ctx-" (System/currentTimeMillis))))

;; =============================================================================
;; Lazy require for delegate-fn (avoid circular dep)
;; =============================================================================

(defn- get-delegate-fn
  "Lazily resolve hive-mcp.agent.core/delegate-agentic-drone!"
  []
  (require 'hive-mcp.agent.core)
  (resolve 'hive-mcp.agent.core/delegate-agentic-drone!))

;; =============================================================================
;; SendMessage Handler
;; =============================================================================

(defn handle-send-message
  "Handle A2A SendMessage method.

   Validates agent exists, extracts text from message parts,
   dispatches task via IAgent/dispatch!, returns A2A Task.

   Params:
     :message       - A2A Message with role + parts
     :configuration - {:agentId \"...\"}

   Reuses pattern from tools/agent/dispatch.clj:handle-dispatch."
  [{:keys [message configuration] :as params}]
  (let [agent-id (:agentId configuration)
        parts (:parts message)]
    (cond
      (empty? agent-id)
      {:error {:code (:invalid-params schema/error-codes)
               :message "configuration.agentId is required"}}

      (empty? parts)
      {:error {:code (:invalid-params schema/error-codes)
               :message "message.parts is required"}}

      :else
      (if-let [[agent agent-data] (resolve-agent agent-id)]
        (try
          (let [prompt (extract-text-from-parts parts)
                ctx (dispatch-ctx/ensure-context prompt)
                resolved-prompt (:prompt (dispatch-ctx/resolve-context ctx))
                task-opts (cond-> {:task resolved-prompt
                                   :dispatch-context ctx
                                   :priority :normal}
                            (= :drone (proto/agent-type agent))
                            (assoc :delegate-fn (get-delegate-fn)))
                task-id (proto/dispatch! agent task-opts)
                context-id (generate-context-id params)
                a2a-task (schema/make-task
                          (or task-id (generate-task-id))
                          context-id
                          "working"
                          (str "Task dispatched to " (:slave/id agent-data))
                          :messages [(schema/make-message (:role message) parts)]
                          :metadata {:hive-agent-id (:slave/id agent-data)
                                     :hive-task-id task-id})]
            (log/info "A2A SendMessage dispatched" {:agent-id (:slave/id agent-data)
                                                    :task-id task-id})
            {:result a2a-task})
          (catch Exception e
            (log/error "A2A SendMessage failed" {:agent-id agent-id :error (ex-message e)})
            {:error {:code (:internal-error schema/error-codes)
                     :message (str "Dispatch failed: " (ex-message e))}}))
        {:error {:code (:agent-not-found schema/error-codes)
                 :message (str "Agent not found: " agent-id)}}))))

;; =============================================================================
;; GetTask Handler
;; =============================================================================

(defn handle-get-task
  "Handle A2A GetTask method.

   Checks event journal first (push-first pattern), falls back
   to DataScript query for agent status.

   Params:
     :id - Task ID to look up"
  [{:keys [id]}]
  (cond
    (empty? id)
    {:error {:code (:invalid-params schema/error-codes)
             :message "id is required"}}

    :else
    (try
      ;; Try event journal first (fast path for completed tasks)
      (let [check-journal (requiring-resolve 'hive-mcp.tools.swarm.channel/check-event-journal)
            journal-entry (check-journal id)]
        (if journal-entry
          ;; Found in journal — build task from event
          (let [state (if (:error journal-entry) "failed" "completed")
                msg (or (:result journal-entry)
                        (:error journal-entry)
                        "Task completed")]
            {:result (schema/make-task id (or (:context-id journal-entry) id)
                                       state (str msg))})
          ;; Not in journal — return generic "working" task
          ;; (we don't track A2A task-id -> agent-id mapping yet)
          {:result (schema/make-task id id "working" "Task in progress")}))
      (catch Exception e
        (log/error "A2A GetTask failed" {:task-id id :error (ex-message e)})
        {:error {:code (:internal-error schema/error-codes)
                 :message (str "GetTask failed: " (ex-message e))}}))))

;; =============================================================================
;; CancelTask Handler
;; =============================================================================

(defn handle-cancel-task
  "Handle A2A CancelTask method.

   Looks up agent by task metadata, calls IAgent/kill!.

   Params:
     :id - Task ID to cancel
     :metadata - Optional {:hive-agent-id \"...\"} for direct lookup"
  [{:keys [id metadata]}]
  (cond
    (empty? id)
    {:error {:code (:invalid-params schema/error-codes)
             :message "id is required"}}

    :else
    (let [agent-id (or (:hive-agent-id metadata)
                       ;; Attempt to extract agent-id from task-id convention
                       ;; a2a-<ts>-<rand> doesn't encode agent, so we need metadata
                       nil)]
      (if agent-id
        (if-let [[agent _] (resolve-agent agent-id)]
          (try
            (proto/kill! agent)
            (log/info "A2A CancelTask killed agent" {:agent-id agent-id :task-id id})
            {:result (schema/make-task id id "canceled" "Task canceled")}
            (catch Exception e
              (log/error "A2A CancelTask failed" {:agent-id agent-id :error (ex-message e)})
              {:error {:code (:internal-error schema/error-codes)
                       :message (str "Cancel failed: " (ex-message e))}}))
          {:error {:code (:agent-not-found schema/error-codes)
                   :message (str "Agent not found: " agent-id)}})
        {:error {:code (:invalid-params schema/error-codes)
                 :message "Cannot resolve agent for task. Provide metadata.hive-agent-id"}}))))

;; =============================================================================
;; SendStreamingMessage Handler (SSE)
;; =============================================================================

(defn handle-send-streaming-message
  "Handle A2A SendStreamingMessage method.

   Dispatches task like SendMessage, then returns a deferred SSE setup.
   The actual SSE stream is handled by the transport layer (a2a.clj)
   using channel/subscribe! for task events.

   Returns {:result task :stream? true :task-id id} to signal
   that the transport should set up SSE for this task."
  [params]
  (let [result (handle-send-message params)]
    (if (:error result)
      result
      (assoc result
             :stream? true
             :task-id (get-in result [:result :id])))))
