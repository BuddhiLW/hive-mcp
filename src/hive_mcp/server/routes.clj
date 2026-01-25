(ns hive-mcp.server.routes
  "MCP server route definitions and tool dispatch.

   CLARITY-L: Layers stay pure - routing logic separated from server lifecycle.
   SRP: Single responsibility for tool route construction and dispatch.

   This module handles:
   - Tool definition conversion to SDK format
   - Piggyback message embedding for hivemind communication
   - Server spec building with capability-based filtering
   - Hot-reload support for tools"
  (:require [hive-mcp.tools :as tools]
            [hive-mcp.docs :as docs]
            [hive-mcp.agent.context :as ctx]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Specs for Tool Definitions
;; =============================================================================

(s/def ::tool-def
  (s/keys :req-un [::name ::description ::inputSchema ::handler]))

(s/def ::name string?)
(s/def ::description string?)
(s/def ::inputSchema map?)
(s/def ::handler fn?)

(s/def ::tool-response
  (s/keys :req-un [::content]))

(s/def ::content (s/coll-of map?))

;; =============================================================================
;; SRP Helpers for Content Normalization
;; =============================================================================

(defn normalize-content
  "Normalize handler result to content array.
   SRP: Single responsibility for content normalization.
   Handles: sequential (passthrough), map (wrap), other (text wrap)."
  [result]
  (cond
    (sequential? result) (vec result)
    (map? result) [result]
    :else [{:type "text" :text (str result)}]))

(defn find-last-text-idx
  "Find index of last text-type item in content (searching from end).
   SRP: Single responsibility for text item location.
   Returns nil if no text item found."
  [content]
  (some (fn [[idx item]]
          (when (= "text" (:type item)) idx))
        (map-indexed vector (reverse content))))

(defn wrap-piggyback
  "Append piggyback messages to content with HIVEMIND delimiters.
   SRP: Single responsibility for piggyback embedding.
   Appends to last text item if exists, otherwise adds new text item.

   Format:
   ---HIVEMIND---
   [{:a \"agent-id\" :e \"event-type\" :m \"message\"}]
   ---/HIVEMIND---"
  [content piggyback]
  (if (and piggyback (seq piggyback))
    (let [piggyback-text (str "\n\n---HIVEMIND---\n"
                              (pr-str piggyback)
                              "\n---/HIVEMIND---")]
      (if-let [last-text-idx (find-last-text-idx content)]
        (let [actual-idx (- (count content) 1 last-text-idx)
              last-item (nth content actual-idx)]
          (assoc content actual-idx
                 (update last-item :text str piggyback-text)))
        (conj content {:type "text" :text piggyback-text})))
    content))

;; =============================================================================
;; Agent ID and Project ID Extraction
;; =============================================================================

(defn extract-agent-id
  "Extract agent-id from args map, handling both snake_case and kebab-case keys.

   DRY: Consolidates 4-way fallback pattern for JSON key variation handling.
   MCP tools may receive agent_id or agent-id in either keyword or string form.

   Returns default if no agent-id found in args."
  [args default]
  (or (:agent_id args)
      (:agent-id args)
      (get args "agent_id")
      (get args "agent-id")
      default))

(defn extract-project-id
  "Extract project-id from args map, handling various key formats.

   Tries directory-based derivation if explicit project-id not found.
   Falls back to ctx/current-directory if no directory in args.
   Returns nil only if no project context available anywhere.

   Key priority:
   1. Explicit project_id/project-id
   2. Derived from directory parameter via scope/get-current-project-id
   3. Derived from ctx/current-directory (request context fallback)"
  [args]
  (or (:project_id args)
      (:project-id args)
      (get args "project_id")
      (get args "project-id")
      ;; Derive from directory if present, with ctx fallback
      (when-let [dir (or (:directory args)
                         (get args "directory")
                         (ctx/current-directory))]
        (require 'hive-mcp.tools.memory.scope)
        ((resolve 'hive-mcp.tools.memory.scope/get-current-project-id) dir))))

;; =============================================================================
;; Composable Handler Wrappers (SRP: Each wrapper single responsibility)
;; =============================================================================

(def ^:const hot-reload-retry-delay-ms
  "Delay between retries when hot-reload might have invalidated handlers."
  100)

(def ^:const hot-reload-max-retries
  "Maximum retries for hot-reload recovery."
  3)

(defn- hot-reload-error?
  "Check if exception indicates stale var references from hot-reload.
   
   CLARITY-Y: Identify transient errors that can be recovered via retry."
  [^Throwable e]
  (let [msg (str (.getMessage e))]
    (or (re-find #"(?i)var.*not.*found" msg)
        (re-find #"(?i)unbound|undefined" msg)
        (re-find #"(?i)no.*protocol.*method" msg)
        (instance? IllegalStateException e))))

(defn wrap-handler-retry
  "Wrap handler with retry logic for hot-reload resilience.
   
   CLARITY-Y: Yield safe failure via automatic retry on transient errors.
   
   When hot-reload occurs, in-flight tool calls may fail because:
   - Var references point to old, unloaded namespaces
   - Protocol implementations are temporarily unavailable
   
   This wrapper catches these transient errors and retries, giving
   time for refresh-tools! to complete."
  [handler]
  (fn [args]
    (loop [attempt 1]
      (let [result (try
                     {:ok (handler args)}
                     (catch Exception e
                       (if (and (hot-reload-error? e)
                                (< attempt hot-reload-max-retries))
                         {:retry e}
                         (throw e))))]
        (if (:retry result)
          (do
            (log/warn "Hot-reload retry" {:attempt attempt 
                                          :max hot-reload-max-retries
                                          :error (ex-message (:retry result))})
            (Thread/sleep hot-reload-retry-delay-ms)
            (recur (inc attempt)))
          (:ok result))))))

(defn wrap-handler-context
  "Wrap handler to bind request context for tool execution.
   SRP: Single responsibility - context binding only.

   Extracts agent-id, project-id, directory from args and binds
   them via hive-mcp.agent.context/with-request-context.

   This enables tool handlers to access context via:
   - (ctx/current-agent-id)
   - (ctx/current-project-id)
   - (ctx/current-directory)"
  [handler]
  (fn [args]
    (let [agent-id (extract-agent-id args nil)
          project-id (extract-project-id args)
          directory (or (:directory args) (get args "directory"))]
      (ctx/with-request-context {:agent-id agent-id
                                 :project-id project-id
                                 :directory directory}
        (handler args)))))

(defn wrap-handler-normalize
  "Wrap handler to normalize its result to content array.
   SRP: Single responsibility - content normalization only.

   (wrap-handler-normalize handler) returns handler that:
   - Calls original handler
   - Normalizes result via normalize-content"
  [handler]
  (fn [args]
    (normalize-content (handler args))))

(defn- get-piggyback-messages
  "Get hivemind piggyback messages for agent+project.
   SRP: Single responsibility - piggyback retrieval.
   Encapsulates dynamic require/resolve pattern.

   CRITICAL: project-id scoping prevents cross-project shout leakage.
   Without it, coordinator-Y would consume shouts meant for coordinator-X."
  [agent-id project-id]
  (require 'hive-mcp.channel.piggyback)
  ((resolve 'hive-mcp.channel.piggyback/get-messages) agent-id :project-id project-id))

(defn wrap-handler-piggyback
  "Wrap handler to attach hivemind piggyback messages.
   SRP: Single responsibility - piggyback embedding only.

   Expects handler to return normalized content (vector of items).
   Extracts agent-id and project-id from args, retrieves piggyback,
   embeds in content.

   CRITICAL: project-id scoping ensures coordinators only see their
   project's shouts, preventing cross-project message consumption.

   CURSOR ISOLATION FIX: When no explicit agent-id provided, use
   'coordinator-{project-id}' to prevent cursor sharing across projects."
  [handler]
  (fn [args]
    (let [content (handler args)
          project-id (extract-project-id args)
          ;; Generate project-scoped coordinator ID to prevent cursor sharing
          ;; Without this, all coordinators would share cursor ["coordinator" "global"]
          default-agent-id (if project-id
                             (str "coordinator-" project-id)
                             "coordinator")
          agent-id (extract-agent-id args default-agent-id)
          piggyback (get-piggyback-messages agent-id project-id)]
      (wrap-piggyback content piggyback))))

(defn wrap-handler-response
  "Wrap handler to build SDK response format.
   SRP: Single responsibility - response building only.

   Wraps handler result in {:content ...} map."
  [handler]
  (fn [args]
    {:content (handler args)}))

;; =============================================================================
;; Tool Definition Conversion
;; =============================================================================

(s/fdef make-tool
  :args (s/cat :tool-def ::tool-def)
  :ret ::tool-response)

(defn make-tool
  "Convert a tool definition with :handler to SDK format.
   Wraps handler to attach pending hivemind messages via content embedding.

   Uses composable handler wrappers (SRP: each wrapper single responsibility):
   - wrap-handler-retry: auto-retry on hot-reload transient errors (CLARITY-Y)
   - wrap-handler-context: binds request context for tool execution
   - wrap-handler-normalize: converts result to content array
   - wrap-handler-piggyback: embeds hivemind messages with agent-id extraction
   - wrap-handler-response: builds {:content ...} response

   Composition via -> threading enables clear data flow:
   handler -> retry -> context -> normalize -> piggyback -> response
   
   CLARITY-Y: wrap-handler-retry is innermost to catch handler exceptions
   before context/normalize processing."
  [{:keys [name description inputSchema handler]}]
  {:name name
   :description description
   :inputSchema inputSchema
   :handler (-> handler
                wrap-handler-retry      ; CLARITY-Y: Hot-reload resilience
                wrap-handler-context
                wrap-handler-normalize
                wrap-handler-piggyback
                wrap-handler-response)})

;; =============================================================================
;; Server Spec Building
;; =============================================================================

(defn build-server-spec
  "Build MCP server spec with capability-based tool filtering.

   MUST be called AFTER init-embedding-provider! to get accurate Chroma status.

   Uses tools/get-filtered-tools for dynamic kanban tool switching:
   - Chroma available -> mcp_mem_kanban_* tools
   - Chroma unavailable -> org_kanban_native_* tools (fallback)"
  []
  (let [filtered-tools (tools/get-filtered-tools)]
    (log/info "Building server spec with" (count filtered-tools) "tools (capability-filtered)")
    {:name "hive-mcp"
     :version "0.1.0"
     :tools (mapv make-tool (concat filtered-tools docs/docs-tools))}))

;; DEPRECATED: Static spec kept for backward compatibility with tests
;; Prefer build-server-spec for capability-aware tool list
(def emacs-server-spec
  {:name "hive-mcp"
   :version "0.1.0"
   ;; hivemind/tools already included in tools/tools aggregation
   :tools (mapv make-tool (concat tools/tools docs/docs-tools))})

;; =============================================================================
;; Hot-Reload Support
;; =============================================================================

(defn refresh-tools!
  "Hot-reload all tools in the running server.
   CLARITY: Open for extension - allows runtime tool updates without restart.

   Uses capability-based filtering - re-checks Chroma availability
   to dynamically switch between mem-kanban and org-kanban-native tools.

   Parameters:
     server-context-atom - atom containing the server context with :tools key

   Returns:
     count of tools refreshed, or nil if no context"
  [server-context-atom]
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          filtered-tools (tools/get-filtered-tools)
          new-tools (mapv make-tool (concat filtered-tools docs/docs-tools))]
      ;; Clear and re-register all tools
      (reset! tools-atom {})
      (doseq [tool new-tools]
        (swap! tools-atom assoc (:name tool) {:tool (dissoc tool :handler)
                                              :handler (:handler tool)}))
      (log/info "Hot-reloaded" (count new-tools) "tools (capability-filtered)")
      (count new-tools))))

(defn debug-tool-handler
  "Get info about a registered tool handler (for debugging).

   Parameters:
     server-context-atom - atom containing the server context
     tool-name - string name of the tool to inspect

   Returns:
     map with :name, :handler-class, :tool-keys or nil if not found"
  [server-context-atom tool-name]
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          tool-entry (get @tools-atom tool-name)]
      (when tool-entry
        {:name tool-name
         :handler-class (str (type (:handler tool-entry)))
         :tool-keys (keys (:tool tool-entry))}))))

;; =============================================================================
;; Tool Registration for Agent Delegation
;; =============================================================================

(defn register-tools-for-delegation!
  "Register filtered tools for agent delegation.

   Delegates to hive-mcp.agent/register-tools! with capability-filtered tools.

   Returns:
     count of tools registered"
  []
  (require 'hive-mcp.agent)
  (let [register-tools! (resolve 'hive-mcp.agent/register-tools!)
        filtered-tools (tools/get-filtered-tools)]
    (register-tools! filtered-tools)
    (log/info "Registered" (count filtered-tools) "tools for agent delegation (capability-filtered)")
    (count filtered-tools)))
