(ns hive-mcp.agent.sdk.lifecycle
  "SDK session lifecycle management: spawn, dispatch, kill, interrupt, status.

   Orchestrates the full lifecycle of SDK ling sessions:
   - spawn-headless-sdk!    : Create session with persistent client
   - dispatch-headless-sdk! : Send tasks (SAA cycle or raw query)
   - kill-headless-sdk!     : Graceful teardown (disconnect -> stop -> unregister)
   - interrupt-headless-sdk!: Interrupt current query via client.interrupt()
   - sdk-status-for         : Session status including multi-turn info
   - list-sdk-sessions      : List all active sessions
   - sdk-session?           : Check if ling-id has SDK session
   - kill-all-sdk!          : Kill all sessions (cleanup/testing)"
  (:require [clojure.core.async :as async :refer [chan >!! <!! close!]]
            [hive-mcp.agent.sdk.availability :as avail]
            [hive-mcp.agent.sdk.event-loop :as event-loop]
            [hive-mcp.agent.sdk.execution :as exec]
            [hive-mcp.agent.sdk.options :as opts]
            [hive-mcp.agent.sdk.phase-compress :as phase-compress]
            [hive-mcp.agent.sdk.session :as session]
            [hive-mcp.agent.context-envelope :as ctx-envelope]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Spawn
;;; =============================================================================

(defn spawn-headless-sdk!
  "Spawn a headless ling using the Claude Agent SDK.

   Initializes a new SDK session and prepares for SAA phases.
   Does NOT start execution - call dispatch-headless-sdk! for that.

   Arguments:
     ling-id - Unique identifier for this ling
     opts    - Map with:
               :cwd           - Working directory (required)
               :system-prompt - Base system prompt (optional)
               :mcp-servers   - MCP server configurations (optional)
               :presets       - Preset names (optional)
               :agents        - Subagent definitions map (optional)

   Returns:
     {:ling-id ling-id :status :spawned :backend :agent-sdk :phase :idle}

   Throws:
     ExceptionInfo if SDK not available or ling-id already exists"
  [ling-id {:keys [cwd system-prompt mcp-servers presets agents] :as _opts}]
  {:pre [(string? ling-id)
         (string? cwd)]}
  ;; Check availability
  (let [status (avail/sdk-status)]
    (when-not (= :available status)
      (throw (ex-info "Claude Agent SDK not available"
                      {:ling-id ling-id
                       :sdk-status status
                       :hint (case status
                               :no-libpython "Add clj-python/libpython-clj to deps.edn"
                               :no-sdk "Run: pip install claude-code-sdk"
                               :not-initialized "Python initialization failed"
                               "Unknown issue")}))))
  ;; Check for duplicate
  (when (session/get-session ling-id)
    (throw (ex-info "SDK session already exists with this ID"
                    {:ling-id ling-id})))
  ;; Register session with persistent loop + client (P3-T2)
  (let [message-ch (chan 4096)
        result-ch (chan 1)
        safe-id (session/ling-id->safe-id ling-id)
        ;; Start persistent event loop in background thread
        {:keys [loop-var _thread-var]} (event-loop/start-session-loop! safe-id)
        ;; Build base options (Act-phase perms, auto-obs hooks)
        base-opts (opts/build-base-options-obj {:cwd cwd
                                                :system-prompt system-prompt
                                                :agents agents})
        ;; Connect client on the persistent loop
        client-var (event-loop/connect-session-client! safe-id base-opts loop-var)
        session-data {:ling-id ling-id
                      :phase :idle
                      :phase-history []
                      :observations []
                      :plan nil
                      :message-ch message-ch
                      :result-ch result-ch
                      :started-at (System/currentTimeMillis)
                      :cwd cwd
                      :system-prompt system-prompt
                      :mcp-servers mcp-servers
                      :presets presets
                      :agents agents
                      :session-id nil
                      ;; P3-T2: Persistent client/loop refs
                      :client-ref client-var
                      :py-loop-var loop-var
                      :py-safe-id safe-id
                      :turn-count 0}]
    (session/register-session! ling-id session-data)
    (log/info "[sdk.lifecycle] Spawned SDK ling with persistent client"
              {:ling-id ling-id :cwd cwd :client-var client-var :loop-var loop-var})
    {:ling-id ling-id
     :status :spawned
     :backend :agent-sdk
     :phase :idle}))

;;; =============================================================================
;;; Dispatch Helpers (SLAP: one abstraction level per function)
;;; =============================================================================

(defn- forward-phase-messages!
  "Forward all messages from phase-ch to out-ch, tagging with phase-key."
  [phase-ch out-ch phase-key]
  (loop []
    (when-let [msg (<!! phase-ch)]
      (>!! out-ch (assoc msg :saa-phase phase-key))
      (recur))))

(defn- build-kg-context-prefix
  "Build compressed context prefix for SAA silence phase.
   Extracts ctx-refs and kg-node-ids from dispatch-context and builds
   an L2 envelope to prepend to the silence prompt.
   Returns string prefix or nil (CLARITY-Y graceful degradation)."
  [dispatch-context]
  (when dispatch-context
    (try
      (when (= :ref (dispatch-ctx/context-type dispatch-context))
        (let [{:keys [ctx-refs kg-node-ids scope]} dispatch-context]
          (ctx-envelope/build-l2-envelope ctx-refs kg-node-ids scope
                                          {:mode :inline})))
      (catch Exception e
        (log/debug "[sdk.lifecycle] Context prefix failed (non-fatal):" (ex-message e))
        nil))))

(defn- compress-and-transition!
  "Compress observations from the completed phase and prepare session for the next.

   Uses the resolved IPhaseCompressor (NoOp or custom) to compress the
   observations collected during from-phase into a compact context string.
   Updates session state with the compressed context and resets observations.

   Arguments:
     ling-id    - Ling identifier
     from-phase - Phase that just completed (keyword)
     to-phase   - Phase about to start (keyword)

   Returns:
     Compression result map with :compressed-context, :entries-created, :compressor"
  [ling-id from-phase to-phase]
  (let [sess (session/get-session ling-id)
        compressor (phase-compress/resolve-compressor)
        result (phase-compress/compress-phase compressor
                                              (name from-phase)
                                              (or (:observations sess) [])
                                              {:project-id (:project-id sess)
                                               :cwd (:cwd sess)})]
    (session/update-session! ling-id
                             {:phase to-phase
                              :compressed-context (:compressed-context result)
                              :observations []})
    (log/info "[sdk.lifecycle] Phase transition compressed"
              {:ling-id ling-id
               :from from-phase
               :to to-phase
               :compressor (:compressor result)
               :entries-created (:entries-created result)})
    result))

(defn- run-saa-silence!
  "Run SAA silence phase: explore codebase and collect observations.
   When dispatch-context is available in session, prepends compressed
   context to the silence prompt for richer exploration grounding."
  [ling-id task out-ch]
  (let [sess (session/get-session ling-id)
        kg-prefix (build-kg-context-prefix (:dispatch-context sess))
        prompt (str (when kg-prefix (str kg-prefix "\n\n---\n\n"))
                    "TASK: " task
                    "\n\nExplore the codebase and collect context. "
                    "List all relevant files, patterns, and observations.")
        phase-ch (exec/execute-phase! ling-id prompt :silence)]
    (loop []
      (when-let [msg (<!! phase-ch)]
        (>!! out-ch (assoc msg :saa-phase :silence))
        (when (= :message (:type msg))
          (session/update-session!
           ling-id
           {:observations (conj (or (:observations (session/get-session ling-id)) [])
                                (:data msg))}))
        (recur)))))

(defn- run-saa-abstract!
  "Run SAA abstract phase: synthesize observations into a plan."
  [ling-id task out-ch]
  (let [observations (:observations (session/get-session ling-id))
        prompt (str "Based on these observations from the Silence phase:\n"
                    (pr-str observations)
                    "\n\nSynthesize these into a concrete action plan for: " task)
        phase-ch (exec/execute-phase! ling-id prompt :abstract)]
    (forward-phase-messages! phase-ch out-ch :abstract)))

(defn- run-saa-act!
  "Run SAA act phase: execute the plan with full tool access."
  [ling-id task out-ch]
  (let [prompt (str "Execute the plan for: " task
                    "\n\nFollow the plan precisely. Make changes file by file.")
        phase-ch (exec/execute-phase! ling-id prompt :act)]
    (forward-phase-messages! phase-ch out-ch :act)))

(defn- emit-dispatch-complete!
  "Emit SAA completion message and log."
  [ling-id out-ch]
  (let [sess (session/get-session ling-id)]
    (>!! out-ch {:type :saa-complete
                 :ling-id ling-id
                 :turn-count (:turn-count sess)
                 :observations-count (count (:observations sess))})
    (log/info "[sdk.lifecycle] Dispatch complete"
              {:ling-id ling-id :turn-count (:turn-count sess)})))

;;; =============================================================================
;;; Dispatch
;;; =============================================================================

(defn dispatch-headless-sdk!
  "Dispatch a task to an SDK ling via the persistent client (P3-T2).

   Supports two modes:
   1. SAA cycle: Runs silence -> abstract -> act phases sequentially
   2. Raw dispatch: Sends task as a single query (no SAA wrapping)

   Arguments:
     ling-id - ID of the spawned SDK ling
     task    - Task description string
     opts    - Optional map:
               :skip-silence?  - Skip silence phase (default: false)
               :skip-abstract? - Skip abstract phase (default: false)
               :phase          - Run only this specific phase
               :raw?           - Skip SAA, send task directly as query

   Returns:
     core.async channel that will receive all messages from all phases."
  [ling-id task & [{:keys [skip-silence? skip-abstract? phase raw? dispatch-context] :as _opts}]]
  {:pre [(string? ling-id)
         (string? task)]}
  (let [sess (session/get-session ling-id)]
    (when-not sess
      (throw (ex-info "SDK session not found" {:ling-id ling-id})))
    (when-not (:client-ref sess)
      (throw (ex-info "No persistent client (was spawn successful?)"
                      {:ling-id ling-id})))
    ;; Store dispatch-context in session for SAA silence phase KG enrichment
    (when dispatch-context
      (session/update-session! ling-id {:dispatch-context dispatch-context}))
    (let [out-ch (chan 4096)]
      (async/thread
        (try
          (if raw?
            (forward-phase-messages!
             (exec/execute-phase! ling-id task :dispatch) out-ch :dispatch)
            (do
              (when-not (or skip-silence? (and phase (not= phase :silence)))
                (run-saa-silence! ling-id task out-ch))
              ;; Compress silence observations before abstract phase
              (when (and (not (or skip-silence? (and phase (not= phase :silence))))
                         (not (or skip-abstract? (and phase (not= phase :abstract)))))
                (compress-and-transition! ling-id :silence :abstract))
              (when-not (or skip-abstract? (and phase (not= phase :abstract)))
                (run-saa-abstract! ling-id task out-ch))
              ;; Compress abstract output before act phase
              (when (and (not (or skip-abstract? (and phase (not= phase :abstract))))
                         (not (and phase (not= phase :act))))
                (compress-and-transition! ling-id :abstract :act))
              (when-not (and phase (not= phase :act))
                (run-saa-act! ling-id task out-ch))))
          (emit-dispatch-complete! ling-id out-ch)
          (catch Exception e
            (log/error "[sdk.lifecycle] Dispatch failed"
                       {:ling-id ling-id :error (ex-message e)})
            (>!! out-ch {:type :error :error (ex-message e)}))
          (finally
            (close! out-ch))))
      out-ch)))

;;; =============================================================================
;;; Kill
;;; =============================================================================

(defn kill-headless-sdk!
  "Terminate an SDK ling session (P3-T2: graceful disconnect).

   Lifecycle: disconnect client -> stop event loop -> close channels -> unregister.

   Arguments:
     ling-id - ID of the SDK ling

   Returns:
     {:killed? true :ling-id ling-id}"
  [ling-id]
  (if-let [sess (session/get-session ling-id)]
    (let [safe-id (or (:py-safe-id sess) (session/ling-id->safe-id ling-id))
          client-var (:client-ref sess)
          loop-var (:py-loop-var sess)]
      ;; Graceful teardown: disconnect client, then stop loop
      (when (and client-var loop-var)
        (event-loop/disconnect-session-client! safe-id loop-var client-var)
        (event-loop/stop-session-loop! safe-id loop-var))
      ;; Close async channels
      (when-let [msg-ch (:message-ch sess)] (close! msg-ch))
      (when-let [res-ch (:result-ch sess)] (close! res-ch))
      ;; Remove from registry
      (session/unregister-session! ling-id)
      (log/info "[sdk.lifecycle] SDK ling killed (graceful)" {:ling-id ling-id})
      {:killed? true :ling-id ling-id})
    (throw (ex-info "SDK session not found" {:ling-id ling-id}))))

;;; =============================================================================
;;; Interrupt
;;; =============================================================================

(defn interrupt-headless-sdk!
  "Interrupt the current query of an SDK ling session.

   Delegates to event-loop/interrupt-session-client! for the Python bridge.
   Safe to call from any thread.
   Does NOT throw -- returns error map on failure (CLARITY-Y)."
  [ling-id]
  (if-let [sess (session/get-session ling-id)]
    (let [{:keys [client-ref py-loop-var phase]} sess]
      (if (and client-ref py-loop-var)
        (let [{:keys [success? error]} (event-loop/interrupt-session-client! client-ref py-loop-var)]
          (if success?
            (do (log/info "[sdk.lifecycle] Interrupt sent" {:ling-id ling-id :phase phase})
                {:success? true :ling-id ling-id :phase phase})
            (do (log/warn "[sdk.lifecycle] Interrupt failed"
                          {:ling-id ling-id :phase phase :error error})
                {:success? false
                 :ling-id ling-id
                 :errors [(or error "Client or event loop not available")]})))
        {:success? false
         :ling-id ling-id
         :errors [(str "No active phase to interrupt (current phase: "
                       (name (or phase :idle)) ")")]}))
    {:success? false
     :ling-id ling-id
     :errors ["SDK session not found"]}))

;;; =============================================================================
;;; Status & Queries
;;; =============================================================================

(defn sdk-status-for
  "Get the status of an SDK ling, including interrupt and multi-turn info."
  [ling-id]
  (when-let [sess (session/get-session ling-id)]
    {:ling-id ling-id
     :phase (:phase sess)
     :phase-history (:phase-history sess)
     :observations-count (count (:observations sess))
     :started-at (:started-at sess)
     :uptime-ms (- (System/currentTimeMillis) (:started-at sess))
     :cwd (:cwd sess)
     :backend :agent-sdk
     :session-id (:session-id sess)
     ;; P3-T2: Multi-turn tracking
     :turn-count (or (:turn-count sess) 0)
     :has-persistent-client? (boolean (:client-ref sess))
     ;; P3-T3: Interrupt capability
     :interruptable? (boolean (and (:client-ref sess)
                                   (:py-loop-var sess)))}))

(defn list-sdk-sessions
  "List all active SDK sessions."
  []
  (->> @(session/session-registry-ref)
       keys
       (map sdk-status-for)
       (remove nil?)
       vec))

(defn sdk-session?
  "Check if a ling-id corresponds to an SDK session."
  [ling-id]
  (contains? @(session/session-registry-ref) ling-id))

(defn kill-all-sdk!
  "Kill all SDK sessions. For cleanup/testing."
  []
  (let [ids (keys @(session/session-registry-ref))
        results (for [id ids]
                  (try
                    (kill-headless-sdk! id)
                    {:success true :id id}
                    (catch Exception e
                      {:success false :id id :error (ex-message e)})))]
    {:killed (count (filter :success results))
     :errors (count (remove :success results))}))
