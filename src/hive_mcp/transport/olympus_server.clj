(ns hive-mcp.transport.olympus-server
  "Dedicated Olympus WebSocket server on port 7911.

   Extends the base olympus transport with bidirectional command handling,
   allowing the Olympus Web UI to not only receive events but also send
   commands (agent spawn/kill, kanban moves, task dispatch).

   Architecture:
   - Reuses olympus.clj for snapshot building, broadcasting, and event emission
   - Adds command routing for incoming WebSocket messages from web UI
   - Delegates to existing consolidated handlers (agent, kanban, hivemind)
   - Provides authentication hook (no-op by default, extensible)

   Event Protocol (server -> client):
     {:type :agents       :data [{:id ... :status ...}]}
     {:type :wave-update  :wave-id ... :task-idx 0 :status :completed}
     {:type :kg-snapshot  :entries [...] :edges [...]}
     {:type :init-snapshot :data {:agents [...] :waves {...} :kg {...}}}

   Command Protocol (client -> server):
     {:type :command :command :agent/spawn  :params {:type \"ling\" :cwd \"/project\"}}
     {:type :command :command :agent/kill   :params {:agent_id \"ling-123\"}}
     {:type :command :command :agent/dispatch :params {:agent_id \"ling-1\" :prompt \"...\"}}
     {:type :command :command :kanban/move  :params {:task_id \"t-1\" :new_status \"done\"}}
     {:type :command :command :kanban/create :params {:title \"...\"}}
     {:type :command :command :kanban/list  :params {:status \"inprogress\"}}
     {:type :command :command :hivemind/shout :params {:event_type \"progress\" :message \"...\"}}
     {:type :command :command :ling-output/get :params {:ling_id \"ling-123\" :since 1234567890}}
     {:type :command :command :ling-output/subscribe :params {:ling_id \"ling-123\"}}
     {:type :command :command :ling-output/unsubscribe :params {:ling_id \"ling-123\"}}

   Lifecycle:
   - start! called from server.clj during MCP startup (replaces olympus/start!)
   - stop! called on shutdown
   - Delegates to olympus.clj for snapshot/broadcast infrastructure

   CLARITY: Composition over modification - extends olympus.clj, doesn't replace it."
  (:require [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [hive-mcp.transport.olympus :as olympus]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Command Registry
;; =============================================================================

(defonce ^:private command-handlers (atom {}))
(defonce ^:private auth-handler (atom nil))

;; =============================================================================
;; Ling Output Subscription State
;; =============================================================================

(defonce ^{:private true
           :doc "Active ling output subscriptions.
   Map of ling-id -> #{send-fn ...}
   Each send-fn is a (fn [data] ...) that pushes output events to a WS client."}
  ling-output-subscriptions
  (atom {}))

(defonce ^{:private true
           :doc "Tracks which ling-ids have active ring buffer watchers.
   Map of ling-id -> watcher-key (keyword).
   Watchers are added/removed from the headless stdout ring buffer atom."}
  ling-output-watchers
  (atom {}))

;; =============================================================================
;; Authentication Hook
;; =============================================================================

(defn set-auth-handler!
  "Set authentication handler for incoming commands.

   Handler signature: (fn [client-info command params] -> {:authorized? bool :reason str})

   If not set, all commands are authorized (development mode).
   Set this in production to validate tokens/sessions."
  [handler-fn]
  (reset! auth-handler handler-fn)
  (log/info "Olympus auth handler configured"))

(defn- authorize-command
  "Check if a command is authorized.
   Returns {:authorized? true} if no auth handler is set (dev mode)."
  [client-info command params]
  (if-let [handler @auth-handler]
    (try
      (handler client-info command params)
      (catch Exception e
        (log/warn "Auth handler error:" (.getMessage e))
        {:authorized? false :reason "Auth handler error"}))
    {:authorized? true}))

;; =============================================================================
;; Command Handlers (delegate to existing consolidated handlers)
;; =============================================================================

(defn- resolve-handler
  "Lazily resolve a handler function to avoid circular dependencies.
   Returns nil if resolution fails."
  [sym]
  (try
    (requiring-resolve sym)
    (catch Exception e
      (log/warn "Failed to resolve handler:" sym (.getMessage e))
      nil)))

(defn- execute-agent-command
  "Execute an agent lifecycle command.
   Delegates to consolidated agent handlers."
  [sub-command params]
  (if-let [handler (resolve-handler 'hive-mcp.tools.consolidated.agent/handle-agent)]
    (try
      (let [;; Map sub-command to agent handler's command format
            command-name (name sub-command)
            agent-params (assoc params :command command-name)
            result (handler agent-params)]
        {:success true
         :command (str "agent/" command-name)
         :result result})
      (catch Exception e
        (log/error "Agent command failed:" sub-command (.getMessage e))
        {:success false
         :error (.getMessage e)
         :command (str "agent/" (name sub-command))}))
    {:success false :error "Agent handler not available"}))

(defn- execute-kanban-command
  "Execute a kanban command.
   Delegates to consolidated kanban handlers."
  [sub-command params]
  (if-let [handler (resolve-handler 'hive-mcp.tools.consolidated.kanban/handle-kanban)]
    (try
      (let [command-name (name sub-command)
            kanban-params (assoc params :command command-name)
            result (handler kanban-params)]
        {:success true
         :command (str "kanban/" command-name)
         :result result})
      (catch Exception e
        (log/error "Kanban command failed:" sub-command (.getMessage e))
        {:success false
         :error (.getMessage e)
         :command (str "kanban/" (name sub-command))}))
    {:success false :error "Kanban handler not available"}))

(defn- execute-hivemind-command
  "Execute a hivemind command (shout, status).
   Delegates to hivemind module."
  [sub-command params]
  (case sub-command
    :shout
    (try
      (let [shout-fn (resolve-handler 'hive-mcp.hivemind/shout!)
            agent-id (or (:agent_id params) (:agent-id params) "olympus-web-ui")
            event-type (keyword (or (:event_type params) (:event-type params) "progress"))
            data (dissoc params :agent_id :agent-id :event_type :event-type)]
        (if shout-fn
          (do
            (shout-fn agent-id event-type data)
            {:success true :command "hivemind/shout"})
          {:success false :error "Hivemind shout handler not available"}))
      (catch Exception e
        {:success false :error (.getMessage e) :command "hivemind/shout"}))

    :status
    (try
      (let [status-fn (resolve-handler 'hive-mcp.hivemind/status)]
        (if status-fn
          {:success true :command "hivemind/status" :result (status-fn)}
          {:success false :error "Hivemind status handler not available"}))
      (catch Exception e
        {:success false :error (.getMessage e) :command "hivemind/status"}))

    ;; Unknown sub-command
    {:success false :error (str "Unknown hivemind command: " (name sub-command))}))

(defn- get-snapshot-view
  "Extract a specific view from a full snapshot.
   Uses build-full-snapshot (public) and extracts the sub-view.
   This avoids referencing private snapshot builder functions."
  [view]
  (let [full (olympus/build-full-snapshot)
        data (:data full)]
    (case view
      :agents {:type :agents :data (:agents data)}
      :waves  {:type :waves :data (:waves data)}
      :kg     {:type :kg-snapshot :data (:kg data)}
      :project-tree {:type :project-tree :data (:project-tree data)}
      nil)))

(defn- execute-snapshot-command
  "Execute a snapshot request command.
   Returns fresh snapshots for specific views or full state."
  [sub-command _params]
  (try
    (case sub-command
      :full    {:success true :command "snapshot/full"
                :result (olympus/build-full-snapshot)}
      :agents  {:success true :command "snapshot/agents"
                :result (get-snapshot-view :agents)}
      :waves   {:success true :command "snapshot/waves"
                :result (get-snapshot-view :waves)}
      :kg      {:success true :command "snapshot/kg"
                :result (get-snapshot-view :kg)}
      :project-tree {:success true :command "snapshot/project-tree"
                     :result (get-snapshot-view :project-tree)}
      ;; Unknown
      {:success false :error (str "Unknown snapshot command: " (name sub-command))})
    (catch Exception e
      {:success false :error (.getMessage e) :command (str "snapshot/" (name sub-command))})))

;; =============================================================================
;; Ling Output Command Handler
;; =============================================================================

(defn- resolve-headless-fn
  "Resolve a function from hive-mcp.agent.headless namespace.
   Uses requiring-resolve to avoid circular deps."
  [fn-name]
  (resolve-handler (symbol "hive-mcp.agent.headless" fn-name)))

(defn- execute-ling-output-get
  "Get stdout output from a headless ling's ring buffer.

   Params:
     :ling_id  - (required) ID of the headless ling
     :since    - (optional) Epoch ms timestamp; returns lines after this time
     :last_n   - (optional) Only return last N lines (ignored if :since is set)

   Returns:
     {:lines [...] :cursor <last-timestamp> :ling_id \"...\" :stats {...}}"
  [params]
  (let [ling-id (or (:ling_id params) (:ling-id params))
        since (some-> (or (:since params)) long)
        last-n (some-> (or (:last_n params) (:last-n params)) long)]
    (when-not ling-id
      (throw (ex-info "Missing required parameter: ling_id" {:params params})))
    ;; Check if the ling is a headless process
    (let [headless? (resolve-headless-fn "headless?")]
      (when (and headless? (not (headless? ling-id)))
        (throw (ex-info "Ling is not a headless process or does not exist"
                        {:ling_id ling-id}))))
    (if since
      ;; Time-based: get lines since timestamp
      (let [get-since (resolve-headless-fn "get-stdout-since")]
        (if get-since
          (let [entries (get-since ling-id since)]
            (if entries
              (let [cursor (if (seq entries) (:ts (last entries)) since)]
                {:lines (mapv :text entries)
                 :entries entries
                 :cursor cursor
                 :ling_id ling-id
                 :count (count entries)})
              (throw (ex-info "Ling not found in headless registry"
                              {:ling_id ling-id}))))
          (throw (ex-info "get-stdout-since not available" {}))))
      ;; Count-based: get last N lines (or all)
      (let [get-stdout (resolve-headless-fn "get-stdout")]
        (if get-stdout
          (let [lines (get-stdout ling-id (if last-n {:last-n last-n} {}))]
            (if lines
              (let [get-stats (resolve-headless-fn "ring-buffer-stats")
                    get-buf (resolve-headless-fn "get-stdout-buffer")
                    stats (when (and get-stats get-buf)
                            (when-let [buf (get-buf ling-id)]
                              (get-stats buf)))]
                {:lines (vec lines)
                 :ling_id ling-id
                 :count (count lines)
                 :stats stats})
              (throw (ex-info "Ling not found in headless registry"
                              {:ling_id ling-id}))))
          (throw (ex-info "get-stdout not available" {})))))))

(defn- install-buffer-watcher!
  "Install a watch on a headless ling's stdout ring buffer atom.
   When the buffer changes (new lines appended), all subscribers
   for that ling-id receive the new lines.

   Arguments:
     ling-id - ID of the headless ling

   Returns:
     The watcher key, or nil if buffer not found."
  [ling-id]
  (let [get-buf (resolve-headless-fn "get-stdout-buffer")]
    (when get-buf
      (when-let [buf (get-buf ling-id)]
        (let [watcher-key (keyword (str "olympus-sub-" ling-id))]
          (add-watch buf watcher-key
                     (fn [_key _ref old-state new-state]
                       (let [old-count (count (:lines old-state))
                             new-count (count (:lines new-state))]
                         (when (> new-count old-count)
                           ;; Extract new lines
                           (let [new-lines (subvec (:lines new-state) old-count)
                                 new-ts (subvec (or (:timestamps new-state) [])
                                                (min old-count
                                                     (count (or (:timestamps new-state) []))))
                                 entries (mapv (fn [line ts] {:text line :ts ts})
                                               new-lines new-ts)
                                 event {:type :ling-output-stream
                                        :ling_id ling-id
                                        :entries entries
                                        :cursor (if (seq new-ts) (last new-ts) (System/currentTimeMillis))
                                        :timestamp (System/currentTimeMillis)}
                                 subs (get @ling-output-subscriptions ling-id)]
                             (doseq [send-fn subs]
                               (try
                                 (send-fn event)
                                 (catch Exception e
                                   (log/debug "Failed to push ling output to subscriber:" (.getMessage e))
                                   ;; Remove dead subscriber
                                   (swap! ling-output-subscriptions
                                          update ling-id disj send-fn)))))))))
          (swap! ling-output-watchers assoc ling-id watcher-key)
          watcher-key)))))

(defn- remove-buffer-watcher!
  "Remove the ring buffer watcher for a ling-id.
   Called when the last subscriber unsubscribes."
  [ling-id]
  (when-let [watcher-key (get @ling-output-watchers ling-id)]
    (let [get-buf (resolve-headless-fn "get-stdout-buffer")]
      (when get-buf
        (when-let [buf (get-buf ling-id)]
          (remove-watch buf watcher-key))))
    (swap! ling-output-watchers dissoc ling-id)))

(defn subscribe-ling-output!
  "Subscribe a client to push-based ling output streaming.

   Arguments:
     ling-id  - ID of the headless ling
     send-fn  - Function (fn [event-map] ...) to push events to client

   Returns:
     {:subscribed true :ling_id ling-id}"
  [ling-id send-fn]
  ;; Validate ling exists
  (let [headless? (resolve-headless-fn "headless?")]
    (when (and headless? (not (headless? ling-id)))
      (throw (ex-info "Ling is not a headless process or does not exist"
                      {:ling_id ling-id}))))
  ;; Add subscriber
  (swap! ling-output-subscriptions update ling-id (fnil conj #{}) send-fn)
  ;; Install watcher if this is the first subscriber for this ling
  (when (= 1 (count (get @ling-output-subscriptions ling-id)))
    (install-buffer-watcher! ling-id))
  {:subscribed true :ling_id ling-id})

(defn unsubscribe-ling-output!
  "Unsubscribe a client from ling output streaming.

   Arguments:
     ling-id  - ID of the headless ling
     send-fn  - The same send-fn used for subscribing

   Returns:
     {:unsubscribed true :ling_id ling-id}"
  [ling-id send-fn]
  (swap! ling-output-subscriptions update ling-id disj send-fn)
  ;; Remove watcher if no more subscribers
  (when (empty? (get @ling-output-subscriptions ling-id))
    (swap! ling-output-subscriptions dissoc ling-id)
    (remove-buffer-watcher! ling-id))
  {:unsubscribed true :ling_id ling-id})

(defn cleanup-ling-subscriptions!
  "Remove all subscriptions and watchers. Called on server stop."
  []
  (doseq [ling-id (keys @ling-output-watchers)]
    (remove-buffer-watcher! ling-id))
  (reset! ling-output-subscriptions {})
  (reset! ling-output-watchers {}))

(defn- execute-ling-output-command
  "Execute a ling-output command.

   Sub-commands:
     :get          - Get stdout lines (poll-based)
     :subscribe    - Start push-based streaming
     :unsubscribe  - Stop push-based streaming
     :list         - List headless lings available for output streaming"
  [sub-command params]
  (try
    (case sub-command
      :get
      {:success true
       :command "ling-output/get"
       :result (execute-ling-output-get params)}

      :subscribe
      (let [ling-id (or (:ling_id params) (:ling-id params))]
        (when-not ling-id
          (throw (ex-info "Missing required parameter: ling_id" {:params params})))
        ;; For WS command-based subscription, we use olympus broadcast as the push mechanism.
        ;; The actual send-fn will be wired by the WS connection handler.
        ;; Here we just validate and record the intent.
        (let [sub-id (str "cmd-sub-" (System/currentTimeMillis) "-" (rand-int 10000))
              send-fn (fn [event]
                        (olympus/broadcast! (assoc event :subscription-id sub-id)))]
          (subscribe-ling-output! ling-id send-fn)
          {:success true
           :command "ling-output/subscribe"
           :result {:subscribed true
                    :ling_id ling-id
                    :subscription-id sub-id}}))

      :unsubscribe
      (let [ling-id (or (:ling_id params) (:ling-id params))]
        (when-not ling-id
          (throw (ex-info "Missing required parameter: ling_id" {:params params})))
        ;; Remove all broadcast-based subs for this ling
        (swap! ling-output-subscriptions dissoc ling-id)
        (remove-buffer-watcher! ling-id)
        {:success true
         :command "ling-output/unsubscribe"
         :result {:unsubscribed true :ling_id ling-id}})

      :list
      (let [list-fn (resolve-headless-fn "list-headless")]
        (if list-fn
          {:success true
           :command "ling-output/list"
           :result {:headless-lings (list-fn)}}
          {:success false :error "Headless module not available"}))

      ;; Unknown sub-command
      {:success false
       :error (str "Unknown ling-output command: " (name sub-command)
                   ". Available: get, subscribe, unsubscribe, list")})
    (catch clojure.lang.ExceptionInfo e
      {:success false
       :error (.getMessage e)
       :command (str "ling-output/" (name sub-command))
       :details (ex-data e)})
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :command (str "ling-output/" (name sub-command))})))

;; =============================================================================
;; Command Router
;; =============================================================================

(def ^:private builtin-commands
  "Built-in command routing table.
   Maps command namespace to executor function."
  {"agent"       execute-agent-command
   "kanban"      execute-kanban-command
   "hivemind"    execute-hivemind-command
   "snapshot"    execute-snapshot-command
   "ling-output" execute-ling-output-command})

(defn register-command!
  "Register a custom command handler.

   Command format: \"namespace/action\" (e.g., \"wave/dispatch\")
   Handler signature: (fn [action-keyword params] -> result-map)

   Example:
     (register-command! \"wave\"
       (fn [action params]
         (case action :dispatch (dispatch-wave params))))"
  [namespace handler-fn]
  (swap! command-handlers assoc namespace handler-fn)
  (log/info "Registered custom Olympus command namespace:" namespace))

(defn- route-command
  "Route a command to the appropriate handler.

   Command format: :namespace/action (e.g., :agent/spawn, :kanban/move)
   Returns result map with :success, :command, :result/:error keys."
  [command params client-info]
  (let [;; Convert keyword to full string including namespace
        command-str (cond
                      (and (keyword? command) (namespace command))
                      (str (namespace command) "/" (name command))

                      (keyword? command)
                      (name command)

                      :else (str command))
        [ns-part action] (if (.contains command-str "/")
                           (clojure.string/split command-str #"/" 2)
                           [command-str nil])
        action-kw (when action (keyword action))]
    ;; Check authorization
    (let [{:keys [authorized? reason]} (authorize-command client-info command params)]
      (if-not authorized?
        {:success false
         :error (str "Unauthorized: " (or reason "Access denied"))
         :command command-str}
        ;; Route to handler
        (if-let [handler (or (get @command-handlers ns-part)
                             (get builtin-commands ns-part))]
          (try
            (handler (or action-kw (keyword ns-part)) params)
            (catch Exception e
              (log/error "Command execution failed:" command-str (.getMessage e))
              {:success false
               :error (.getMessage e)
               :command command-str}))
          {:success false
           :error (str "Unknown command namespace: " ns-part
                       ". Available: " (clojure.string/join ", "
                                                            (concat (keys builtin-commands)
                                                                    (keys @command-handlers))))
           :command command-str})))))

;; =============================================================================
;; WebSocket Message Handler (extends olympus.clj)
;; =============================================================================

(defn handle-command-message
  "Handle an incoming command message from Olympus Web UI client.

   Expected message format:
   {:type \"command\"
    :command \"agent/spawn\"
    :params {:type \"ling\" :cwd \"/project\"}
    :request-id \"req-123\"}  ;; optional, for response correlation

   Returns response map to send back to client."
  [msg client-info]
  (let [{:keys [command params request-id]} msg
        command-kw (if (string? command) (keyword command) command)
        result (route-command command-kw (or params {}) client-info)]
    (cond-> {:type :command-response
             :timestamp (System/currentTimeMillis)
             :command (str command)}
      request-id (assoc :request-id request-id)
      (:success result) (assoc :success true :data (:result result))
      (not (:success result)) (assoc :success false :error (:error result)))))

(defn process-incoming-message
  "Process any incoming WebSocket message from Olympus client.

   Extends the base olympus.clj message handling with command support.
   Routes based on :type field:
   - \"command\"          -> route-command (new)
   - \"request-snapshot\" -> existing olympus handler
   - \"ping\"            -> pong response
   - \"subscribe\"       -> event filtering (future)

   Returns response map or nil (for messages that don't need response)."
  [raw-msg client-info]
  (try
    (let [msg (if (string? raw-msg)
                (json/read-str raw-msg :key-fn keyword)
                raw-msg)]
      (case (str (:type msg))
        ;; New: Command handling for web UI actions
        "command"
        (handle-command-message msg client-info)

        ;; Existing: Request snapshot
        "request-snapshot"
        (let [view (keyword (:view msg))
              snapshot-data (if (= view :full)
                              (olympus/build-full-snapshot)
                              (get-snapshot-view view))]
          {:type :snapshot-response
           :view view
           :timestamp (System/currentTimeMillis)
           :data (or snapshot-data {:error (str "Unknown view: " (name view))})})

        ;; Ping/pong keepalive
        "ping"
        {:type :pong :timestamp (System/currentTimeMillis)}

        ;; Subscribe to specific event types (future enhancement)
        "subscribe"
        {:type :subscribe-ack
         :views (:views msg)
         :timestamp (System/currentTimeMillis)}

        ;; Unknown message type
        (do
          (log/debug "Unknown Olympus message type:" (:type msg))
          nil)))
    (catch Exception e
      (log/debug "Failed to process Olympus message:" (.getMessage e))
      {:type :error
       :error (.getMessage e)
       :timestamp (System/currentTimeMillis)})))

;; =============================================================================
;; Server Lifecycle (Delegates to olympus.clj)
;; =============================================================================

(defn start!
  "Start the Olympus WebSocket server on port 7911.

   Delegates to olympus.clj for the actual Aleph server but wires
   command handling into the message processing pipeline.

   Options:
     :port - Port number (default: 7911, env: HIVE_MCP_OLYMPUS_WS_PORT)

   Returns the actual port number."
  ([] (start! {}))
  ([opts]
   (let [port (olympus/start! opts)]
     (when port
       (log/info "Olympus server started with command handling on port" port))
     port)))

(defn stop!
  "Stop the Olympus WebSocket server.
   Cleans up ling output subscriptions and watchers."
  []
  (cleanup-ling-subscriptions!)
  (olympus/stop!))

(defn status
  "Get Olympus server status including command handler info."
  []
  (let [base-status (olympus/status)]
    (assoc base-status
           :command-namespaces (vec (concat (keys builtin-commands)
                                            (keys @command-handlers)))
           :auth-configured? (boolean @auth-handler))))

;; =============================================================================
;; Public API - Event Broadcasting (re-export from olympus.clj)
;; =============================================================================

(def broadcast! olympus/broadcast!)
(def emit! olympus/emit!)
(def emit-agent-event! olympus/emit-agent-event!)
(def emit-wave-event! olympus/emit-wave-event!)
(def emit-hivemind-shout! olympus/emit-hivemind-shout!)
(def emit-kg-event! olympus/emit-kg-event!)
(def wire-hivemind-events! olympus/wire-hivemind-events!)
(def build-full-snapshot olympus/build-full-snapshot)

;; =============================================================================
;; Available Commands Summary
;; =============================================================================

(defn list-commands
  "List all available commands with their descriptions.
   Useful for client-side command palette / help."
  []
  {:builtin
   {:agent {:spawn    "Spawn a new agent (ling or drone)"
            :kill     "Terminate an agent"
            :dispatch "Send task to an agent"
            :status   "Query agent status"
            :list     "List all agents"
            :claims   "Get file claims"
            :cleanup  "Remove orphan agents"}
    :kanban {:list   "List kanban tasks"
             :create "Create new task"
             :move   "Move task to new status"
             :status "Board overview"}
    :hivemind {:shout  "Broadcast hivemind message"
               :status "Get hivemind status"}
    :snapshot {:full         "Full state snapshot"
               :agents       "Agents snapshot"
               :waves        "Waves snapshot"
               :kg           "Knowledge graph snapshot"
               :project-tree "Project tree snapshot"}
    :ling-output {:get         "Get stdout output from headless ling (poll)"
                  :subscribe   "Subscribe to push-based ling output streaming"
                  :unsubscribe "Unsubscribe from ling output streaming"
                  :list        "List headless lings available for streaming"}}
   :custom (into {} (map (fn [[k _]] [k "Custom command namespace"]) @command-handlers))})

(comment
  ;; REPL testing
  (start!)
  (status)
  (list-commands)

  ;; Test command routing
  (route-command :agent/list {} nil)
  (route-command :kanban/status {} nil)
  (route-command :snapshot/full {} nil)

  ;; Test message processing
  (process-incoming-message
   "{\"type\":\"command\",\"command\":\"agent/list\",\"params\":{}}"
   {:client-id "test"})

  (process-incoming-message
   "{\"type\":\"command\",\"command\":\"kanban/status\",\"params\":{}}"
   {:client-id "test"})

  (stop!))
