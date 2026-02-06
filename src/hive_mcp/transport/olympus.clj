(ns hive-mcp.transport.olympus
  "Dedicated WebSocket server for Olympus UI on port 7911.

   ADR: Separate from Emacs channel (port 9999) because:
   1. Different protocol: keyword types (:agents) vs string ('hivemind-progress')
   2. Snapshot on connect: browser refresh needs full state
   3. Different consumers: React UI vs elisp

   Event Protocol (what Olympus expects):
   {:type :agents :data [{:id \"...\" :status :working ...}]}
   {:type :wave-update :wave-id \"...\" :task-idx 0 :status :completed}
   {:type :kg-snapshot :entries [...] :edges [...]}

   Lifecycle:
   - start! called from server.clj during MCP startup
   - stop! called on shutdown
   - Emits snapshot on each new client connection

   CLARITY: Composition over modification - reuses Aleph patterns from channel/websocket.clj"
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [datascript.core :as d-core]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.swarm.datascript.connection :as ds-conn]
            [hive-mcp.project.tree :as project-tree]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private server-atom (atom nil))
(defonce ^:private clients (atom #{}))

;; =============================================================================
;; JSON Serialization Helpers
;; =============================================================================

(defn- serialize-date
  "Convert java.util.Date to ISO timestamp string for JSON serialization."
  [d]
  (when d
    (if (instance? java.util.Date d)
      (.format java.time.format.DateTimeFormatter/ISO_INSTANT
               (.toInstant ^java.util.Date d))
      (str d))))

(defn- serialize-ref
  "Convert DataScript entity ref to ID string."
  [ref]
  (cond
    (nil? ref) nil
    (map? ref) (or (:db/id ref) (str ref))
    :else (str ref)))

;; =============================================================================
;; Snapshot Builders (Query DataScript for current state)
;; =============================================================================

(defn- build-agents-snapshot
  "Build agents snapshot from DataScript.
   Returns vector of agent maps for Olympus UI.
   All values are JSON-serializable (dates as ISO strings, refs as IDs)."
  []
  (try
    (->> (ds-queries/get-all-slaves)
         (map (fn [slave]
                {:id (:slave/id slave)
                 :name (:slave/name slave)
                 :type (if (= 0 (:slave/depth slave)) :coordinator :ling)
                 :status (some-> (:slave/status slave) name)
                 :project-id (:slave/project-id slave)
                 :cwd (:slave/cwd slave)
                 :presets (vec (:slave/presets slave))
                 :parent (:slave/parent slave)
                 :current-task (:slave/current-task slave)
                 :tasks-completed (or (:slave/tasks-completed slave) 0)
                 :created-at (serialize-date (:slave/created-at slave))}))
         vec)
    (catch Exception e
      (log/warn "Failed to build agents snapshot:" (.getMessage e))
      [])))

(defn- build-waves-snapshot
  "Build waves snapshot from DataScript.
   Returns map of wave-id -> wave state.
   All values are JSON-serializable."
  []
  (try
    ;; Query all waves from DataScript
    (let [c (ds-conn/ensure-conn)
          db @c
          wave-eids (d-core/q '[:find [?e ...]
                                :where [?e :wave/id _]]
                              db)]
      (->> wave-eids
           (map #(d-core/entity db %))
           (map (fn [e]
                  (let [wave-id (:wave/id e)]
                    [wave-id
                     {:id wave-id
                      :plan-id (serialize-ref (:wave/plan e))
                      :total-tasks (:wave/total-tasks e)
                      :concurrency (:wave/concurrency e)
                      :active-count (or (:wave/active-count e) 0)
                      :completed-count (or (:wave/completed-count e) 0)
                      :failed-count (or (:wave/failed-count e) 0)
                      :status (some-> (:wave/status e) name)
                      :started-at (serialize-date (:wave/started-at e))
                      :completed-at (serialize-date (:wave/completed-at e))}])))
           (into {})))
    (catch Exception e
      (log/warn "Failed to build waves snapshot:" (.getMessage e))
      {})))

(defn- build-kg-snapshot
  "Build knowledge graph snapshot.
   Returns {:entries [...] :edges [...]} for recent KG state.
   Note: This is a lightweight snapshot - full KG may be too large."
  []
  (try
    ;; Query recent memory entries (last 50) via memory tools
    (let [query-fn (requiring-resolve 'hive-mcp.tools.memory.crud/query-by-metadata)
          recent-entries (when query-fn
                           (take 50 (query-fn {:limit 50})))]
      {:entries (or recent-entries [])
       :edges []  ;; KG edges would come from kg/edges query - simplified for now
       :entry-count (count recent-entries)})
    (catch Exception e
      (log/warn "Failed to build KG snapshot:" (.getMessage e))
      {:entries [] :edges [] :entry-count 0})))

(defn- build-project-tree-snapshot
  "Build project tree snapshot from project.tree DataScript.
   Returns hierarchical structure with ling counts per project.

   HCR Wave 5: Enables Olympus UI project tree navigator."
  []
  (try
    (let [all-projects (project-tree/query-all-projects)
          tree-data (project-tree/build-project-tree all-projects)
          ;; Get agents to count lings per project
          agents (build-agents-snapshot)
          ling-counts (reduce (fn [acc a]
                                (if-let [pid (:project-id a)]
                                  (update acc pid (fnil inc 0))
                                  acc))
                              {}
                              agents)]
      {:projects (mapv (fn [p]
                         {:id (:project/id p)
                          :path (:project/path p)
                          :type (some-> (:project/type p) name)
                          :parent-id (:project/parent-id p)
                          :tags (vec (or (:project/tags p) []))
                          :git-root (:project/git-root p)
                          :ling-count (get ling-counts (:project/id p) 0)
                          :last-scanned (serialize-date (:project/last-scanned p))})
                       all-projects)
       :roots (:roots tree-data)
       :children (:children tree-data)
       :total (count all-projects)})
    (catch Exception e
      (log/warn "Failed to build project tree snapshot:" (.getMessage e))
      {:projects [] :roots [] :children {} :total 0})))

(defn build-full-snapshot
  "Build complete state snapshot for new client connection.

   Returns:
   {:type :init-snapshot
    :timestamp <ms>
    :data {:agents [...] :waves {...} :kg {...} :project-tree {...}}}"
  []
  {:type :init-snapshot
   :timestamp (System/currentTimeMillis)
   :data {:agents (build-agents-snapshot)
          :waves (build-waves-snapshot)
          :kg (build-kg-snapshot)
          :project-tree (build-project-tree-snapshot)}})

;; =============================================================================
;; WebSocket Handler
;; =============================================================================

(defn- send-to-client!
  "Send JSON message to a specific client."
  [client msg]
  (when-not (s/closed? client)
    (d/catch
     (s/put! client (json/write-str msg))
     (fn [e]
       (log/debug "Send to client failed:" (.getMessage e))
       (swap! clients disj client)))))

(defn- handle-client-message
  "Handle incoming message from Olympus client.
   
   Supported commands:
   {:type :subscribe :views [:agents :waves]}  - Filter events (future)
   {:type :request-snapshot :view :kg}         - Request fresh snapshot"
  [client msg]
  (try
    (let [parsed (json/read-str msg :key-fn keyword)]
      (case (:type parsed)
        ;; Request snapshot for specific view
        "request-snapshot"
        (let [view (keyword (:view parsed))
              snapshot (case view
                         :agents {:type :agents :data (build-agents-snapshot)}
                         :waves {:type :waves :data (build-waves-snapshot)}
                         :kg {:type :kg-snapshot :data (build-kg-snapshot)}
                         :project-tree {:type :project-tree :data (build-project-tree-snapshot)}
                         {:type :error :message (str "Unknown view: " view)})]
          (send-to-client! client snapshot))

        ;; Ping/pong for keepalive
        "ping"
        (send-to-client! client {:type :pong :timestamp (System/currentTimeMillis)})

        ;; Unknown - log and ignore
        (log/debug "Unknown Olympus message type:" (:type parsed))))
    (catch Exception e
      (log/debug "Failed to parse Olympus message:" (.getMessage e)))))

(defn- ws-connection-handler
  "Handle WebSocket connection from Olympus UI.
   Sends full state snapshot immediately on connect."
  [req]
  (d/let-flow [socket (http/websocket-connection req)]
              (let [client-id (str "olympus-" (System/currentTimeMillis) "-" (rand-int 10000))]
                (log/info "Olympus client connected:" client-id)
                (swap! clients conj socket)

      ;; Send full snapshot immediately (browser refresh = needs full state)
                (send-to-client! socket (build-full-snapshot))

      ;; Handle incoming messages
                (s/consume (fn [raw]
                             (cond
                               (= raw "ping") (send-to-client! socket {:type :pong})
                               :else (handle-client-message socket raw)))
                           socket)

      ;; Cleanup on disconnect
                (s/on-closed socket
                             (fn []
                               (log/info "Olympus client disconnected:" client-id)
                               (swap! clients disj socket)))

                socket)))

(defn- json-response
  "Helper to create JSON HTTP response."
  [status data]
  {:status status
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
             "Access-Control-Allow-Headers" "Content-Type"}
   :body (json/write-str data)})

(defn- http-handler
  "HTTP handler for Olympus WS server.
   Routes:
     GET /health       -> Health check
     GET /api/snapshot -> Full state snapshot
     GET /api/agents   -> Agents only
     GET /api/waves    -> Waves only
     GET /api/kg       -> KG/Memory snapshot
     GET /api/stats    -> DataScript statistics
     OPTIONS *         -> CORS preflight
     *                 -> WebSocket upgrade"
  [req]
  (let [uri (:uri req)
        method (:request-method req)]
    (cond
      ;; CORS preflight
      (= method :options)
      {:status 204
       :headers {"Access-Control-Allow-Origin" "*"
                 "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
                 "Access-Control-Allow-Headers" "Content-Type"}}

      ;; Health check endpoint
      (and (= method :get) (= uri "/health"))
      (json-response 200
                     {:status "healthy"
                      :service "olympus-ws"
                      :clients (count @clients)
                      :timestamp (System/currentTimeMillis)})

      ;; REST API: Full snapshot (same as init-snapshot)
      (and (= method :get) (= uri "/api/snapshot"))
      (json-response 200 (build-full-snapshot))

      ;; REST API: Agents only
      (and (= method :get) (= uri "/api/agents"))
      (json-response 200
                     {:type :agents
                      :timestamp (System/currentTimeMillis)
                      :data (build-agents-snapshot)})

      ;; REST API: Waves only
      (and (= method :get) (= uri "/api/waves"))
      (json-response 200
                     {:type :waves
                      :timestamp (System/currentTimeMillis)
                      :data (build-waves-snapshot)})

      ;; REST API: KG/Memory snapshot
      (and (= method :get) (= uri "/api/kg"))
      (json-response 200
                     {:type :kg-snapshot
                      :timestamp (System/currentTimeMillis)
                      :data (build-kg-snapshot)})

      ;; REST API: Project tree (HCR Wave 5)
      (and (= method :get) (= uri "/api/project-tree"))
      (json-response 200
                     {:type :project-tree
                      :timestamp (System/currentTimeMillis)
                      :data (build-project-tree-snapshot)})

      ;; REST API: DataScript statistics
      (and (= method :get) (= uri "/api/stats"))
      (json-response 200
                     {:type :stats
                      :timestamp (System/currentTimeMillis)
                      :data (try
                              (ds-queries/db-stats)
                              (catch Exception e
                                {:error (.getMessage e)}))})

      ;; WebSocket upgrade (default)
      :else
      (ws-connection-handler req))))

;; =============================================================================
;; Public API - Server Lifecycle
;; =============================================================================

(defn start!
  "Start Olympus WebSocket server on port 7911.
   
   Options:
     :port - Port number (default: 7911, env: HIVE_MCP_OLYMPUS_WS_PORT)
   
   Returns the actual port number."
  ([] (start! {}))
  ([{:keys [port]}]
   (let [port (or port
                  (some-> (System/getenv "HIVE_MCP_OLYMPUS_WS_PORT") parse-long)
                  7911)]
     (if @server-atom
       (do
         (log/warn "Olympus WS server already running on port" (:port @server-atom))
         (:port @server-atom))
       (try
         (let [server (http/start-server http-handler {:port port})
               actual-port (netty/port server)]
           (reset! server-atom {:server server :port actual-port})
           (log/info "Olympus WebSocket server started on port" actual-port)
           actual-port)
         (catch Exception e
           (log/error "Failed to start Olympus WS server:" (.getMessage e))
           nil))))))

(defn stop!
  "Stop the Olympus WebSocket server."
  []
  (when-let [{:keys [server port]} @server-atom]
    (.close server)
    (reset! server-atom nil)
    (reset! clients #{})
    (log/info "Olympus WebSocket server stopped (was on port" port ")")
    true))

(defn status
  "Get Olympus WS server status."
  []
  {:running? (boolean @server-atom)
   :port (:port @server-atom)
   :clients (count @clients)
   :connected? (pos? (count @clients))})

;; =============================================================================
;; Public API - Event Broadcasting
;; =============================================================================

(defn broadcast!
  "Broadcast event to all connected Olympus clients.
   
   Event should match Olympus protocol:
   {:type :agents :data [...]}
   {:type :wave-update :wave-id \"...\" :task-idx 0 :status :completed}
   {:type :hivemind-shout :agent-id \"...\" :event-type \"progress\" :message \"...\"}
   {:type :kg-entry-added :entry {...}}"
  [event]
  (let [json-msg (json/write-str event)
        active-clients @clients]
    (when (seq active-clients)
      (log/debug "Olympus broadcast to" (count active-clients) "clients:" (:type event))
      (doseq [client active-clients]
        (when-not (s/closed? client)
          (d/catch
           (s/put! client json-msg)
           (fn [e]
             (log/debug "Olympus broadcast failed:" (.getMessage e))
             (swap! clients disj client))))))))

(defn emit!
  "Emit a typed event to all Olympus clients.
   Convenience wrapper that adds timestamp."
  [event-type data]
  (broadcast! (merge {:type event-type
                      :timestamp (System/currentTimeMillis)}
                     data)))

;; =============================================================================
;; Event Adapters (Transform hivemind events to Olympus protocol)
;; =============================================================================

(defn emit-agent-event!
  "Emit agent lifecycle event in Olympus protocol.
   
   Event types:
   - :agent-spawned  -> full agent data
   - :agent-status   -> status update
   - :agent-killed   -> removal notification"
  [event-type agent-data]
  (emit! event-type {:agent agent-data}))

(defn emit-wave-event!
  "Emit wave lifecycle event in Olympus protocol.
   
   Event types:
   - :wave-dispatched -> full wave data
   - :wave-task-update -> task progress
   - :wave-completed -> final status"
  [event-type wave-data]
  (emit! event-type wave-data))

(defn emit-hivemind-shout!
  "Emit hivemind shout in Olympus protocol.
   Called from hivemind/shout! to keep Olympus in sync."
  [{:keys [agent-id event-type message task data]}]
  (emit! :hivemind-shout
         {:agent-id agent-id
          :event-type event-type
          :message message
          :task task
          :data data}))

(defn emit-kg-event!
  "Emit KG change event in Olympus protocol."
  [event-type kg-data]
  (emit! event-type kg-data))

;; =============================================================================
;; Integration Hooks (Wire into existing event sources)
;; =============================================================================

(defn wire-hivemind-events!
  "Wire internal events to Olympus broadcast.

   Called from server.clj during startup. Registers the :olympus-broadcast
   effect with the re-frame-style event system so that event handlers
   (wave, drone, ling, KG, memory) can include {:olympus-broadcast {...}}
   in their effects map and have it automatically broadcast to all
   connected Olympus WebSocket clients.

   Also wires memory change hooks via channel pub/sub for events that
   bypass the re-frame event system (direct chroma writes)."
  []
  (try
    ;; 1. Register :olympus-broadcast effect (safety net)
    ;;    effects.clj also registers this, but wire! may be called first
    ;;    in some startup orderings. Safe to call reg-fx multiple times.
    (when-let [reg-fx (requiring-resolve 'hive-mcp.events.core/reg-fx)]
      (reg-fx :olympus-broadcast
              (fn [event-data]
                (broadcast! event-data))))

    ;; 2. Subscribe to memory-added events via channel pub/sub
    ;;    Memory CRUD (handle-add) publishes :memory-added events to channel.
    ;;    We subscribe and forward to Olympus.
    (when-let [subscribe-fn (requiring-resolve 'hive-mcp.channel/subscribe!)]
      (let [sub-ch (subscribe-fn :memory-added)]
        (future
          (loop []
            (when-let [event (async/<!! sub-ch)]
              (try
                (emit-kg-event! :memory-entry-added
                                {:entry-id (:id event)
                                 :type (:type event)
                                 :tags (:tags event)
                                 :project-id (:project-id event)})
                (catch Exception e
                  (log/debug "Olympus memory event forward failed:" (.getMessage e))))
              (recur))))))

    (log/info "Olympus event wiring complete - :olympus-broadcast effect + memory change subscription")
    (catch Exception e
      (log/warn "Failed to wire Olympus events (non-fatal):" (.getMessage e)))))

(comment
  ;; REPL testing
  (start!)
  (status)
  (broadcast! {:type :agents :data [{:id "test-1" :status :working}]})
  (stop!)

  ;; Test snapshot
  (build-full-snapshot)
  (build-agents-snapshot)
  (build-waves-snapshot))
