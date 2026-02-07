(ns hive-mcp.tools.consolidated.session
  "Consolidated Session CLI tool.

   Subcommands: complete, wrap, whoami, catchup, context-put, context-get,
                context-query, context-evict, context-stats, context-reconstruct

   Usage via MCP: session {\"command\": \"complete\", \"commit_msg\": \"feat: done\"}
                  session {\"command\": \"whoami\"}
                  session {\"command\": \"catchup\", \"directory\": \"/path/to/project\"}
                  session {\"command\": \"context-put\", \"data\": {...}, \"tags\": [...]}
                  session {\"command\": \"context-get\", \"ctx_id\": \"ctx-...\"}

   SOLID: Facade pattern - single tool entry point for session lifecycle.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.session-complete :as session-handlers]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.crystal :as crystal]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.channel.context-store :as ctx-store]
            [hive-mcp.context.reconstruction :as reconstruction]
            [hive-mcp.tools.memory.scope :as scope]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Context Auto-Eviction (session cleanup on wrap/complete)
;; =============================================================================

(defn- evict-agent-context!
  "Evict context-store entries for a completing agent.
   Queries for entries tagged with the agent's ID and evicts them.
   This prevents the context store from growing unbounded across sessions.

   Tags checked:
   - \"agent:<agent-id>\"  (standard agent attribution tag)
   - \"session:<agent-id>\" (session-scoped entries)

   Returns {:evicted N} or {:evicted 0} if no entries found."
  [agent-id]
  (when (and agent-id (not= agent-id "unknown") (not= agent-id "coordinator"))
    (try
      (let [tags #{(str "agent:" agent-id)
                   (str "session:" agent-id)
                   agent-id}
            n (ctx-store/evict-by-tags! tags)]
        (when (pos? n)
          (log/info "evict-agent-context! evicted" n "entries for" agent-id))
        {:evicted n})
      (catch Exception e
        (log/warn "evict-agent-context! failed for" agent-id ":" (.getMessage e))
        {:evicted 0 :error (.getMessage e)}))))

;; =============================================================================
;; Whoami Handler - Return agent identity context
;; =============================================================================

(defn handle-whoami
  "Return the calling agent's identity context.

   Lings can call this to verify their identity since they cannot read
   shell environment variables directly (the MCP server runs in the
   coordinator's JVM, not the ling's terminal).

   Returns:
     {:agent-id   \"swarm-task-name-1234567890\"  ; From context or param
      :project-id \"hive-mcp\"                    ; Derived from directory
      :cwd        \"/home/user/project\"          ; Working directory}

   The agent_id parameter is optional - if provided (from system prompt injection),
   it takes precedence. Otherwise falls back to request context."
  [{:keys [agent_id directory]}]
  (let [;; Directory: explicit param > context > server cwd
        effective-dir (or directory
                          (ctx/current-directory)
                          (System/getProperty "user.dir"))
        ;; Agent-id: explicit param > context > env var > unknown
        effective-agent-id (or agent_id
                               (ctx/current-agent-id)
                               (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                               "unknown")
        ;; Project-id: derive from directory
        project-id (when effective-dir
                     (scope/get-current-project-id effective-dir))]
    (log/info "session-whoami" {:agent effective-agent-id :project project-id :cwd effective-dir})
    (mcp-json {:agent-id   effective-agent-id
               :project-id project-id
               :cwd        effective-dir})))

;; =============================================================================
;; Wrap Handler - Delegates to crystal wrap
;; =============================================================================

(defn handle-wrap
  "Wrap session - crystallize learnings without commit.
   Use session_complete for full lifecycle with git commit.
   After crystallization, evicts context-store entries for this agent."
  [{:keys [agent_id directory]}]
  (log/info "session-wrap" {:agent agent_id})
  (try
    ;; Delegate to crystal wrap_crystallize
    (let [result (crystal/handle-wrap-crystallize {:directory directory
                                                   :agent_id agent_id})]
      ;; Evict agent's cached context after successful wrap
      (let [effective-agent (or agent_id
                                (ctx/current-agent-id)
                                (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
            eviction (evict-agent-context! effective-agent)]
        (when eviction
          (log/info "session-wrap: context eviction" eviction)))
      result)
    (catch Exception e
      (mcp-error (str "Wrap failed: " (.getMessage e))))))

;; =============================================================================
;; Catchup Handler - Delegates to catchup/handle-native-catchup
;; =============================================================================

(defn handle-catchup
  "Run catchup to restore session context from Chroma memory.
   Gathers axioms, conventions, decisions, sessions, git status.
   Delegates to hive-mcp.tools.catchup/handle-native-catchup."
  [{:keys [directory] :as params}]
  (log/info "session-catchup" {:directory directory})
  (try
    (catchup/handle-native-catchup params)
    (catch Exception e
      (mcp-error (str "Catchup failed: " (.getMessage e))))))

;; =============================================================================
;; Context Store Handlers (pass-by-reference for agents)
;; =============================================================================

(defn handle-context-put
  "Store data in ephemeral context store. Returns ctx-id for pass-by-reference.
   Agents store context once, pass IDs in messages (~50 tokens vs 2000+)."
  [{:keys [data tags ttl_ms]}]
  (try
    (when-not data
      (throw (ex-info "Missing required field: data" {})))
    (let [args (cond-> []
                 tags   (into [:tags (set tags)])
                 ttl_ms (into [:ttl-ms (long ttl_ms)]))
          id (apply ctx-store/context-put! data args)]
      (mcp-json {:ctx-id id :ttl-ms (or ttl_ms ctx-store/default-ttl-ms)}))
    (catch Exception e
      (mcp-error (str "context-put failed: " (.getMessage e))))))

(defn handle-context-get
  "Retrieve entry from context store by ID. Increments access count."
  [{:keys [ctx_id]}]
  (try
    (when-not ctx_id
      (throw (ex-info "Missing required field: ctx_id" {})))
    (if-let [entry (ctx-store/context-get ctx_id)]
      (mcp-json entry)
      (mcp-json {:not-found true :ctx-id ctx_id}))
    (catch Exception e
      (mcp-error (str "context-get failed: " (.getMessage e))))))

(defn handle-context-query
  "Query context store entries by tags. Returns matching, non-expired entries."
  [{:keys [tags limit]}]
  (try
    (let [args (cond-> []
                 tags  (into [:tags (set tags)])
                 limit (into [:limit (long limit)]))
          results (apply ctx-store/context-query args)]
      (mcp-json {:count (count results) :entries results}))
    (catch Exception e
      (mcp-error (str "context-query failed: " (.getMessage e))))))

(defn handle-context-evict
  "Remove entry from context store by ID. Returns eviction status."
  [{:keys [ctx_id]}]
  (try
    (when-not ctx_id
      (throw (ex-info "Missing required field: ctx_id" {})))
    (let [removed? (ctx-store/context-evict! ctx_id)]
      (mcp-json {:evicted removed? :ctx-id ctx_id}))
    (catch Exception e
      (mcp-error (str "context-evict failed: " (.getMessage e))))))

(defn handle-context-stats
  "Return context store statistics: total entries, oldest/newest timestamps, approx bytes."
  [_params]
  (try
    (mcp-json (ctx-store/context-stats))
    (catch Exception e
      (mcp-error (str "context-stats failed: " (.getMessage e))))))

;; =============================================================================
;; Context Reconstruct Handler - KG-compressed context reconstruction
;; =============================================================================

(defn handle-context-reconstruct
  "Reconstruct compressed context from context-store refs + KG traversal.

   Takes context-store reference IDs and KG node IDs, reconstructs a bounded
   compressed context markdown string (~750 tokens max).

   Used by agents to hydrate pass-by-reference context on demand, producing
   a compact prompt from structural KG edges + ref data summaries.

   Parameters:
     ctx_id    - Context-store entry ID containing ref map (category->ctx-id).
                 Alternative to passing ctx_refs directly.
     ctx_refs  - Map of category->ctx-id for direct ref specification.
                 e.g. {\"axioms\": \"ctx-123\", \"decisions\": \"ctx-456\"}
     kg_node_ids - Vector of KG node IDs for graph traversal seeds.
     scope     - Project scope string (e.g. \"hive-mcp\"). Auto-derived if omitted.
     directory - Working directory for scope derivation.

   Returns:
     Compressed context markdown string (max 3000 chars)."
  [{:keys [ctx_id ctx_refs kg_node_ids scope directory]}]
  (try
    (let [;; Resolve refs: either from ctx_id lookup or direct ctx_refs param
          effective-refs (cond
                           ;; Direct refs map provided
                           (seq ctx_refs)
                           (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} ctx_refs)

                           ;; Lookup from context-store entry (e.g. catchup cached refs)
                           ctx_id
                           (when-let [entry (ctx-store/context-get ctx_id)]
                             (let [data (:data entry)]
                               (if (map? data) data {})))

                           :else {})
          ;; KG node IDs
          effective-kg-ids (vec (or kg_node_ids []))
          ;; Scope: explicit > derived from directory > nil
          effective-scope (or scope
                              (when directory
                                (scope/get-current-project-id directory)))
          ;; Run reconstruction pipeline
          result (reconstruction/reconstruct-context
                  (or effective-refs {})
                  effective-kg-ids
                  effective-scope)]
      (mcp-json {:reconstructed result
                 :chars (count result)
                 :refs-count (count effective-refs)
                 :kg-nodes-count (count effective-kg-ids)}))
    (catch Exception e
      (mcp-error (str "context-reconstruct failed: " (.getMessage e))))))

;; =============================================================================
;; Complete Handler - Wraps session-complete with context eviction
;; =============================================================================

(defn handle-complete
  "Complete a ling session with context eviction.
   Delegates to session-complete handler, then evicts agent's cached context."
  [{:keys [agent_id] :as params}]
  (let [result (session-handlers/handle-session-complete params)
        ;; Evict agent's cached context after session complete
        effective-agent (or agent_id
                            (ctx/current-agent-id)
                            (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        eviction (evict-agent-context! effective-agent)]
    (when eviction
      (log/info "session-complete: context eviction" eviction))
    result))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:complete             handle-complete
   :wrap                 handle-wrap
   :whoami               handle-whoami
   :catchup              handle-catchup
   :context-put          handle-context-put
   :context-get          handle-context-get
   :context-query        handle-context-query
   :context-evict        handle-context-evict
   :context-stats        handle-context-stats
   :context-reconstruct  handle-context-reconstruct})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-session
  "Unified CLI handler for session lifecycle."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated session command."
  {:name "session"
   :consolidated true
   :description "Session lifecycle: complete (commit + kanban + wrap + shout), wrap (crystallize only without commit), whoami (get agent identity context), catchup (restore session context from memory). Context store: context-put (store data, get ID), context-get (retrieve by ID), context-query (search by tags), context-evict (remove by ID), context-stats (store metrics), context-reconstruct (KG-compressed reconstruction from refs). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["complete" "wrap" "whoami" "catchup"
                                                "context-put" "context-get" "context-query"
                                                "context-evict" "context-stats"
                                                "context-reconstruct" "help"]
                                         :description "Session operation to perform"}
                              ;; complete params
                              "commit_msg" {:type "string"
                                            :description "Git commit message (required for complete)"}
                              "task_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Kanban task IDs to mark done"}
                              "agent_id" {:type "string"
                                          :description "Ling's slave-id (CLAUDE_SWARM_SLAVE_ID)"}
                              "directory" {:type "string"
                                           :description "Working directory for git/kanban scoping"}
                              ;; context-store params
                              "data" {:description "[context-put] Structured data to store (any JSON value)"}
                              "ctx_id" {:type "string"
                                        :description "[context-get/context-evict] Context entry ID"}
                              "tags" {:type "array"
                                      :items {:type "string"}
                                      :description "[context-put/context-query] Tags for filtering"}
                              "ttl_ms" {:type "integer"
                                        :description "[context-put] Time-to-live in milliseconds (default: 300000 = 5 min)"}
                              "limit" {:type "integer"
                                       :description "[context-query] Max entries to return (default: 100)"}
                              ;; context-reconstruct params
                              "ctx_refs" {:type "object"
                                          :description "[context-reconstruct] Map of category->ctx-id for ref resolution"}
                              "kg_node_ids" {:type "array"
                                             :items {:type "string"}
                                             :description "[context-reconstruct] KG node IDs for graph traversal seeds"}
                              "scope" {:type "string"
                                       :description "[context-reconstruct] Project scope for KG traversal"}}
                 :required ["command"]}
   :handler handle-session})

(def tools
  "Tool definitions for registration."
  [tool-def])
