(ns hive-mcp.tools.consolidated.session
  "Consolidated Session CLI tool for lifecycle and context store operations."
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

(defn- evict-agent-context!
  "Evict context-store entries for a completing agent."
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

(defn handle-whoami
  "Return the calling agent's identity context."
  [{:keys [agent_id directory]}]
  (let [effective-dir (or directory
                          (ctx/current-directory)
                          (System/getProperty "user.dir"))
        effective-agent-id (or agent_id
                               (ctx/current-agent-id)
                               (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                               "unknown")
        project-id (when effective-dir
                     (scope/get-current-project-id effective-dir))]
    (log/info "session-whoami" {:agent effective-agent-id :project project-id :cwd effective-dir})
    (mcp-json {:agent-id   effective-agent-id
               :project-id project-id
               :cwd        effective-dir})))

(defn handle-wrap
  "Wrap session -- crystallize learnings without commit."
  [{:keys [agent_id directory]}]
  (log/info "session-wrap" {:agent agent_id})
  (try
    (let [result (crystal/handle-wrap-crystallize {:directory directory
                                                   :agent_id agent_id})]
      (let [effective-agent (or agent_id
                                (ctx/current-agent-id)
                                (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
            eviction (evict-agent-context! effective-agent)]
        (when eviction
          (log/info "session-wrap: context eviction" eviction)))
      result)
    (catch Exception e
      (mcp-error (str "Wrap failed: " (.getMessage e))))))

(defn handle-catchup
  "Restore session context from Chroma memory."
  [{:keys [directory] :as params}]
  (log/info "session-catchup" {:directory directory})
  (try
    (catchup/handle-native-catchup params)
    (catch Exception e
      (mcp-error (str "Catchup failed: " (.getMessage e))))))

(defn handle-context-put
  "Store data in ephemeral context store, returns ctx-id for pass-by-reference."
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
  "Retrieve entry from context store by ID."
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
  "Query context store entries by tags."
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
  "Remove entry from context store by ID."
  [{:keys [ctx_id]}]
  (try
    (when-not ctx_id
      (throw (ex-info "Missing required field: ctx_id" {})))
    (let [removed? (ctx-store/context-evict! ctx_id)]
      (mcp-json {:evicted removed? :ctx-id ctx_id}))
    (catch Exception e
      (mcp-error (str "context-evict failed: " (.getMessage e))))))

(defn handle-context-stats
  "Return context store statistics."
  [_params]
  (try
    (mcp-json (ctx-store/context-stats))
    (catch Exception e
      (mcp-error (str "context-stats failed: " (.getMessage e))))))

(defn handle-context-reconstruct
  "Reconstruct compressed context from context-store refs and KG traversal."
  [{:keys [ctx_id ctx_refs kg_node_ids scope directory]}]
  (try
    (let [effective-refs (cond
                           (seq ctx_refs)
                           (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} ctx_refs)

                           ctx_id
                           (when-let [entry (ctx-store/context-get ctx_id)]
                             (let [data (:data entry)]
                               (if (map? data) data {})))

                           :else {})
          effective-kg-ids (vec (or kg_node_ids []))
          effective-scope (or scope
                              (when directory
                                (scope/get-current-project-id directory)))
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

(defn handle-complete
  "Complete a ling session with context eviction."
  [{:keys [agent_id] :as params}]
  (let [result (session-handlers/handle-session-complete params)
        effective-agent (or agent_id
                            (ctx/current-agent-id)
                            (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        eviction (evict-agent-context! effective-agent)]
    (when eviction
      (log/info "session-complete: context eviction" eviction))
    result))

(def handlers
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

(def handle-session
  (make-cli-handler handlers))

(def tool-def
  {:name "session"
   :consolidated true
   :description "Session lifecycle: complete (commit + kanban + wrap + shout), wrap (crystallize only without commit), whoami (get agent identity context), catchup (restore session context from memory). Context store: context-put (store data, get ID), context-get (retrieve by ID), context-query (search by tags), context-evict (remove by ID), context-stats (store metrics), context-reconstruct (compressed reconstruction from refs). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["complete" "wrap" "whoami" "catchup"
                                                "context-put" "context-get" "context-query"
                                                "context-evict" "context-stats"
                                                "context-reconstruct" "help"]
                                         :description "Session operation to perform"}
                              "commit_msg" {:type "string"
                                            :description "Git commit message (required for complete)"}
                              "task_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Kanban task IDs to mark done"}
                              "agent_id" {:type "string"
                                          :description "Ling's slave-id (CLAUDE_SWARM_SLAVE_ID)"}
                              "directory" {:type "string"
                                           :description "Working directory for git/kanban scoping"}
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
                              "ctx_refs" {:type "object"
                                          :description "[context-reconstruct] Map of category->ctx-id for ref resolution"}
                              "kg_node_ids" {:type "array"
                                             :items {:type "string"}
                                             :description "[context-reconstruct] KG node IDs for graph traversal seeds"}
                              "scope" {:type "string"
                                       :description "[context-reconstruct] Project scope for KG traversal"}}
                 :required ["command"]}
   :handler handle-session})

(def tools [tool-def])
