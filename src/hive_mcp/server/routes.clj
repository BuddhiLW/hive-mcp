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
            [hive-mcp.channel.async-result :as async-buf]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))
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

(defn wrap-delimited-block
  "Append a delimited block to content.
   SRP: Single responsibility for delimiter-wrapped embedding.
   Appends to last text item if exists, otherwise adds new text item.

   Format:
   ---TAG---
   <body>
   ---/TAG---"
  [content tag body]
  (if (and body (seq (str body)))
    (let [block-text (str "\n\n---" tag "---\n"
                          body
                          "\n---/" tag "---")]
      (if-let [last-text-idx (find-last-text-idx content)]
        (let [actual-idx (- (count content) 1 last-text-idx)
              last-item (nth content actual-idx)]
          (assoc content actual-idx
                 (update last-item :text str block-text)))
        (conj content {:type "text" :text block-text})))
    content))

(defn wrap-piggyback
  "Append piggyback messages to content with HIVEMIND delimiters.
   SRP: Single responsibility for piggyback embedding.
   Appends to last text item if exists, otherwise adds new text item.

   Format:
   ---HIVEMIND---
   [{:a \"agent-id\" :e \"event-type\" :m \"message\"}]
   ---/HIVEMIND---"
  [content piggyback]
  (wrap-delimited-block content "HIVEMIND" (when (seq piggyback) (pr-str piggyback))))

;; =============================================================================
;; Agent ID and Project ID Extraction
;; =============================================================================

(defn extract-agent-id
  "Extract agent-id from args map, handling both snake_case and kebab-case keys.

   NOTE: Args are keywordized by wrap-handler-context, so we only check keyword keys.
   MCP tools may use agent_id or agent-id naming convention.

   Returns default if no agent-id found in args."
  [args default]
  (or (:agent_id args)
      (:agent-id args)
      default))

(defn extract-project-id
  "Extract project-id from args map.

   NOTE: Args are keywordized by wrap-handler-context, so we only check keyword keys.

   Tries directory-based derivation if explicit project-id not found.
   Falls back to ctx/current-directory, then server's cwd as last resort.
   Returns nil only if no project context available anywhere.

   Key priority:
   1. Explicit project_id/project-id
   2. Derived from directory parameter via scope/get-current-project-id
   3. Derived from ctx/current-directory (request context fallback)
   4. Derived from server's working directory (System/getProperty user.dir)"
  [args]
  (or (:project_id args)
      (:project-id args)
      ;; Derive from directory if present, with ctx and user.dir fallbacks
      (when-let [dir (or (:directory args)
                         (ctx/current-directory)
                         (System/getProperty "user.dir"))]
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
   SRP: Single responsibility - context binding + args normalization.

   Normalizes args by keywordizing string keys (MCP SDK passes JSON with
   string keys, but handlers expect keyword keys for destructuring).

   Extracts agent-id, project-id, directory from args and binds
   them via hive-mcp.agent.context/with-request-context.

   Directory fallback chain (CLARITY-R: explicit context flow):
   1. Explicit :directory in args
   2. Server's working directory (System/getProperty user.dir)

   This ensures tools always have a directory context, preventing
   scope leakage where entries get stored as 'global' when callers
   don't explicitly pass directory.

   This enables tool handlers to access context via:
   - (ctx/current-agent-id)
   - (ctx/current-project-id)
   - (ctx/current-directory)

   BUG FIX: MCP clients send JSON args with string keys (e.g. {\"directory\" ...})
   but Clojure {:keys [directory]} destructuring only matches keyword keys.
   Keywordizing args ensures handlers work correctly with both MCP and direct calls."
  [handler]
  (fn [args]
    ;; Keywordize args to handle both MCP (string keys) and direct calls (keyword keys)
    (let [args (walk/keywordize-keys args)
          agent-id (extract-agent-id args nil)
          project-id (extract-project-id args)
          directory (or (:directory args)
                        (System/getProperty "user.dir"))]
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

(defn- drain-memory-piggyback
  "Drain next batch of memory entries for agent+project.
   SRP: Single responsibility - memory piggyback retrieval.
   Returns drain result map or nil if nothing pending."
  [agent-id project-id]
  (require 'hive-mcp.channel.memory-piggyback)
  ((resolve 'hive-mcp.channel.memory-piggyback/drain!) agent-id project-id))

(defn wrap-memory-piggyback-content
  "Append memory piggyback batch to content with MEMORY delimiters.
   SRP: Single responsibility for memory piggyback embedding.

   Format:
   ---MEMORY---
   {:batch [...] :remaining N :total M :delivered D :seq S}
   ---/MEMORY---"
  [content drain-result]
  (wrap-delimited-block content "MEMORY" (when drain-result (pr-str drain-result))))

(defn wrap-handler-memory-piggyback
  "Wrap handler to attach memory piggyback entries.
   SRP: Single responsibility - memory piggyback embedding only.

   Drains next batch of buffered memory entries (axioms, conventions)
   within 32K char budget. Zero-cost when no entries pending.

   Runs BEFORE hivemind piggyback in the middleware chain so both
   channels can append to content independently.

   CURSOR FIX: Always uses session-level coordinator identity, NOT
   extract-agent-id from args or ctx. Both args and ctx may contain
   a target agent_id (e.g. agent dispatch sets ctx from args), which
   would create spurious cursor keys per target agent."
  [handler]
  (fn [args]
    (let [content (handler args)
          project-id (extract-project-id args)
          ;; Always use coordinator identity for piggyback cursor.
          ;; ctx/current-agent-id and args both polluted by target agent_id
          ;; on dispatch-type tools. Piggyback cursor must be session-stable.
          agent-id (if project-id
                     (str "coordinator-" project-id)
                     "coordinator")
          drain-result (drain-memory-piggyback agent-id project-id)]
      (wrap-memory-piggyback-content content drain-result))))

(defn wrap-handler-piggyback
  "Wrap handler to attach hivemind piggyback messages.
   SRP: Single responsibility - piggyback embedding only.

   Expects handler to return normalized content (vector of items).
   Uses reader identity for cursor tracking, retrieves piggyback,
   embeds in content.

   CRITICAL: project-id scoping ensures coordinators only see their
   project's shouts, preventing cross-project message consumption.

   CURSOR ISOLATION FIX: When no explicit agent-id provided, use
   'coordinator-{project-id}' to prevent cursor sharing across projects.

   CURSOR IDENTITY FIX: Always uses session-level coordinator identity,
   NOT extract-agent-id from args or ctx. Both may contain target
   agent_id (e.g. agent dispatch sets ctx from args), which would
   create spurious cursor keys causing ALL accumulated shouts to be
   re-delivered from timestamp 0 on every dispatch to a new target."
  [handler]
  (fn [args]
    (let [content (handler args)
          project-id (extract-project-id args)
          ;; Always use coordinator identity for piggyback cursor.
          ;; ctx/current-agent-id and args both polluted by target agent_id
          ;; on dispatch-type tools. Piggyback cursor must be session-stable.
          agent-id (if project-id
                     (str "coordinator-" project-id)
                     "coordinator")
          piggyback (get-piggyback-messages agent-id project-id)]
      (wrap-piggyback content piggyback))))

(defn wrap-handler-async
  "Wrap handler to intercept async tool calls.
   SRP: Single responsibility - async interception only.

   When args contain :async true, returns immediate ack and spawns
   a future for the real handler execution. The future's result is
   enqueued into the async-result buffer for piggyback delivery.

   When :async is absent or false, passes through to handler normally.

   Position in chain: AFTER normalize (so ack goes through piggyback chain)
   but BEFORE piggybacks (so ack still gets hivemind/memory blocks).

   The :_tool-name key is injected by wrap-handler-context-with-toolname
   (or extracted from args) for result attribution."
  [handler tool-name]
  (fn [args]
    (if (:async args)
      (let [task-id (str "atask-" (random-uuid))
            project-id (extract-project-id args)
            ;; Always use coordinator identity for buffer key alignment
            ;; with drain wrapper. See CURSOR IDENTITY FIX in piggyback wrappers.
            agent-id (if project-id
                       (str "coordinator-" project-id)
                       "coordinator")]
        ;; Spawn background execution
        (future
          (try
            (let [;; Remove :async flag before passing to real handler
                  clean-args (dissoc args :async)
                  result (handler clean-args)]
              (async-buf/enqueue-result! agent-id project-id
                                         {:task-id task-id
                                          :tool tool-name
                                          :status :completed
                                          :result result}))
            (catch Exception e
              (log/error e "async-result: background execution failed for task" task-id)
              (async-buf/enqueue-result! agent-id project-id
                                         {:task-id task-id
                                          :tool tool-name
                                          :status :error
                                          :error (.getMessage e)}))))
        ;; Return immediate ack (goes through normalize → piggyback chain)
        [{:type "text"
          :text (pr-str {:queued true :task-id task-id :tool tool-name})}])
      ;; No async flag → pass through
      (handler args))))

(defn wrap-handler-async-piggyback
  "Wrap handler to drain async results as piggyback content.
   SRP: Single responsibility - async result delivery only.

   Drains completed async results for the calling agent+project
   and appends them as ---TOOLRESULT--- delimited blocks.

   Zero-cost when no async results pending.

   Runs alongside memory-piggyback and hivemind-piggyback in the
   middleware chain so all three channels can append independently.

   Format:
   ---TOOLRESULT---
   {:results [...] :remaining N :total M :delivered D}
   ---/TOOLRESULT---"
  [handler]
  (fn [args]
    (let [content (handler args)
          project-id (extract-project-id args)
          ;; Always use coordinator identity for buffer key alignment
          ;; with async enqueue. See CURSOR IDENTITY FIX in piggyback wrappers.
          agent-id (if project-id
                     (str "coordinator-" project-id)
                     "coordinator")
          drain-result (async-buf/drain! agent-id project-id)]
      (wrap-delimited-block content "TOOLRESULT"
                            (when drain-result (pr-str drain-result))))))

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
   - wrap-handler-async: intercept async:true calls, return ack, spawn future
   - wrap-handler-normalize: converts result to content array
   - wrap-handler-async-piggyback: drains async results (completed futures)
   - wrap-handler-memory-piggyback: drains memory entries (axioms, conventions)
   - wrap-handler-piggyback: embeds hivemind messages with agent-id extraction
   - wrap-handler-context: binds request context for tool execution
   - wrap-handler-response: builds {:content ...} response

   Composition via -> threading enables clear data flow:
   handler -> retry -> async-intercept -> normalize -> async-piggyback -> memory-piggyback -> hivemind-piggyback -> context -> response

   The async interceptor sits AFTER retry (so it can retry on hot-reload errors)
   but BEFORE normalize (so the ack [{:type text}] passes through the piggyback chain).

   Memory/async/hivemind piggybacks all run in parallel, each appending
   their own delimited blocks to content independently.

   CLARITY-Y: wrap-handler-retry is innermost to catch handler exceptions
   before context/normalize processing.

   CRITICAL: context must wrap all piggybacks so ctx/current-directory is bound
   when extract-project-id runs."
  [{:keys [name description inputSchema handler deprecated]}]
  (cond-> {:name name
           :description description
           :inputSchema inputSchema
           :handler (-> handler
                        wrap-handler-retry              ; CLARITY-Y: Hot-reload resilience
                        (wrap-handler-async name)       ; async:true → ack + future
                        wrap-handler-normalize
                        wrap-handler-async-piggyback    ; async results channel
                        wrap-handler-memory-piggyback   ; memory channel (needs ctx bound)
                        wrap-handler-piggyback          ; hivemind channel (needs ctx bound)
                        wrap-handler-context            ; binds ctx for all piggybacks
                        wrap-handler-response)}
    deprecated (assoc :deprecated true)))

;; =============================================================================
;; Server Spec Building
;; =============================================================================

(defn build-server-spec
  "Build MCP server spec with capability-based tool filtering.

   MUST be called AFTER init-embedding-provider! to get accurate Chroma status.

   PHASE 2 STRANGLE: Includes ALL tools (deprecated tools have :deprecated true).
   The server.clj multimethod override filters deprecated from tools/list response.

   Uses tools/get-all-tools for dynamic kanban tool switching:
   - Chroma available -> mcp_mem_kanban_* tools
   - Chroma unavailable -> org_kanban_native_* tools (fallback)

   Deprecated tools are:
   - INCLUDED in spec with :deprecated true (callable via tools/call)
   - FILTERED by server.clj multimethod override (hidden from tools/list)"
  []
  (let [all-tools (tools/get-all-tools :include-deprecated? true)
        deprecated-count (count (filter :deprecated all-tools))
        visible-count (- (count all-tools) deprecated-count)]
    (log/info "Building server spec with" (count all-tools) "tools"
              "(" visible-count "visible," deprecated-count "deprecated)")
    {:name "hive-mcp"
     :version "0.1.0"
     :tools (mapv make-tool (concat all-tools docs/docs-tools))}))

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

   PHASE 2 STRANGLE: Registers ALL tools (including deprecated) for dispatch.
   Deprecated tools are excluded from tools/list but remain callable for
   backward compatibility during the grace period (sunset: 2026-04-01).

   Parameters:
     server-context-atom - atom containing the server context with :tools key

   Returns:
     count of tools refreshed, or nil if no context"
  [server-context-atom]
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          ;; Include deprecated tools for dispatch (tools/call still works)
          all-tools (tools/get-all-tools :include-deprecated? true)
          new-tools (mapv make-tool (concat all-tools docs/docs-tools))
          ;; Track deprecated count for logging
          deprecated-count (count (filter :deprecated all-tools))]
      ;; Clear and re-register all tools
      (reset! tools-atom {})
      (doseq [tool new-tools]
        (swap! tools-atom assoc (:name tool) {:tool (dissoc tool :handler)
                                              :handler (:handler tool)}))
      (log/info "Hot-reloaded" (count new-tools) "tools"
                "(including" deprecated-count "deprecated shims for backward compat)")
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
  "Register tools for agent delegation including deprecated shims.

   PHASE 2 STRANGLE: Includes deprecated tools for backward compatibility.
   Agents can still delegate to deprecated tools during grace period.

   Delegates to hive-mcp.agent/register-tools! with all tools.

   Returns:
     count of tools registered"
  []
  (require 'hive-mcp.agent)
  (let [register-tools! (resolve 'hive-mcp.agent/register-tools!)
        ;; Include deprecated tools for delegation (backward compat)
        all-tools (tools/get-all-tools :include-deprecated? true)
        deprecated-count (count (filter :deprecated all-tools))]
    (register-tools! all-tools)
    (log/info "Registered" (count all-tools) "tools for agent delegation"
              "(including" deprecated-count "deprecated shims)")
    (count all-tools)))
