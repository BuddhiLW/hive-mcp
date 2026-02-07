(ns hive-mcp.agent.headless-sdk
  "Headless ling process management via Claude Agent SDK (Python).

   Uses libpython-clj to bridge Clojure ↔ Python, running the Claude Agent SDK
   in the same JVM process. This gives direct DataScript access without
   serialization boundaries (unlike ProcessBuilder approach in headless.clj).

   SAA Strategy (Silence-Abstract-Act):
   - Silence: Observe quietly via read-only tools, collect context from memory/KG/files
   - Abstract: Synthesize observations into a plan using crystal/core scoring
   - Act: Execute the plan with full tool access

   Each phase maps to a Claude Agent SDK ClaudeSDKClient session with different
   tool sets and permissions. ClaudeSDKClient enables multi-turn session
   persistence across SAA phases (vs query() which is one-shot).

   Python Bridge Architecture:
   - ClaudeAgentOptions constructed as real Python objects via py-call-kw
   - Options injected into Python __main__ namespace via py-set-global!
   - Async execution via asyncio.run() bridge (required because libpython-clj
     cannot directly bridge Python async generators/coroutines)
   - Results extracted via py-get-global + py->clj

   Graceful Degradation:
   - If libpython-clj is not on classpath → returns :unavailable
   - If claude-agent-sdk not installed → returns :sdk-not-found
   - Falls back to ProcessBuilder headless.clj if SDK unavailable"
  (:require [clojure.core.async :as async :refer [go go-loop chan >! <! >!! <!! close! put!]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Availability Detection (Graceful Degradation)
;;; =============================================================================

;; Cached availability of libpython-clj + claude-agent-sdk.
;; Checked once at first use, not at namespace load time.
(defonce ^:private sdk-available? (atom nil))

(defn- check-libpython-available?
  "Check if libpython-clj2 is on the classpath.
   Returns true if the namespace can be required."
  []
  (try
    (require 'libpython-clj2.python)
    true
    (catch Exception _
      (log/debug "[headless-sdk] libpython-clj2 not on classpath")
      false)))

(defn- check-sdk-available?
  "Check if claude-agent-sdk Python package is importable.
   Must be called AFTER libpython-clj is initialized."
  []
  (try
    (let [py-fn (requiring-resolve 'libpython-clj2.python/run-simple-string)]
      (py-fn "import claude_agent_sdk")
      true)
    (catch Exception _
      (log/debug "[headless-sdk] claude-agent-sdk Python package not found")
      false)))

(defn sdk-status
  "Check SDK availability. Returns one of:
   :available        - Both libpython-clj and claude-agent-sdk ready
   :no-libpython     - libpython-clj not on classpath
   :no-sdk           - libpython-clj available but claude-agent-sdk not installed
   :not-initialized  - libpython-clj available but Python not yet initialized"
  []
  (if-let [cached @sdk-available?]
    cached
    (let [status (cond
                   (not (check-libpython-available?))
                   :no-libpython

                   :else
                   ;; Try to initialize and check SDK
                   (try
                     (let [init! (requiring-resolve 'libpython-clj2.python/initialize!)]
                       (init!)
                       (if (check-sdk-available?)
                         :available
                         :no-sdk))
                     (catch Exception e
                       (log/warn "[headless-sdk] Failed to initialize Python" (ex-message e))
                       :not-initialized)))]
      (reset! sdk-available? status)
      status)))

(defn available?
  "Returns true if the SDK backend is fully available."
  []
  (= :available (sdk-status)))

(defn reset-availability!
  "Reset cached availability check. For testing."
  []
  (reset! sdk-available? nil))

;;; =============================================================================
;;; SAA Phase Definitions
;;; =============================================================================

(def saa-phases
  "SAA phase configuration.
   Each phase defines the tools allowed and the system prompt modifier."
  {:silence
   {:name :silence
    :description "Observe quietly - read files, query memory, traverse KG"
    :allowed-tools ["Read" "Glob" "Grep" "WebSearch" "WebFetch"]
    :permission-mode "bypassPermissions"
    :system-prompt-suffix
    (str "You are in SILENCE phase (SAA Strategy).\n"
         "Your goal: Observe and collect context WITHOUT acting.\n"
         "- Read files, search code, query memory\n"
         "- Record what you find as structured observations\n"
         "- Do NOT edit files or run commands\n"
         "- At the end, produce a JSON summary of observations")}

   :abstract
   {:name :abstract
    :description "Synthesize observations into a plan"
    :allowed-tools ["Read" "Glob" "Grep"]
    :permission-mode "bypassPermissions"
    :system-prompt-suffix
    (str "You are in ABSTRACT phase (SAA Strategy).\n"
         "Your goal: Synthesize the observations from Silence phase into an action plan.\n"
         "- Prioritize findings by importance and relevance\n"
         "- Identify patterns and connections\n"
         "- Produce a structured plan with specific steps\n"
         "- Each step should name the file, the change, and the rationale")}

   :act
   {:name :act
    :description "Execute the plan with full tool access"
    :allowed-tools ["Read" "Edit" "Write" "Bash" "Glob" "Grep"]
    :permission-mode "acceptEdits"
    :system-prompt-suffix
    (str "You are in ACT phase (SAA Strategy).\n"
         "Your goal: Execute the plan from the Abstract phase.\n"
         "- Follow the plan step by step\n"
         "- Make precise, focused changes\n"
         "- Verify each change before moving to the next")}})

;;; =============================================================================
;;; SDK Session Registry
;;; =============================================================================

;; Registry of active SDK sessions.
;; Key: ling-id (String)
;; Value: {:session-id   String (Claude SDK session ID)
;;         :phase        keyword (:silence :abstract :act)
;;         :phase-history [{:phase :kw :started-at inst :ended-at inst}]
;;         :observations  vector (collected during silence phase)
;;         :plan          string (produced during abstract phase)
;;         :message-ch    core.async channel (streaming messages)
;;         :result-ch     core.async channel (final result)
;;         :started-at    long
;;         :cwd           string
;;         :system-prompt string}
(defonce ^:private session-registry (atom {}))

(defn- register-session!
  "Register a new SDK session."
  [ling-id session-data]
  (swap! session-registry assoc ling-id session-data)
  (log/info "[headless-sdk] Session registered" {:ling-id ling-id
                                                 :phase (:phase session-data)}))

(defn- update-session!
  "Update an existing SDK session."
  [ling-id updates]
  (swap! session-registry update ling-id merge updates))

(defn- unregister-session!
  "Remove a session from registry."
  [ling-id]
  (when-let [session (get @session-registry ling-id)]
    ;; Close channels
    (when-let [ch (:message-ch session)] (close! ch))
    (when-let [ch (:result-ch session)] (close! ch)))
  (swap! session-registry dissoc ling-id)
  (log/info "[headless-sdk] Session unregistered" {:ling-id ling-id}))

(defn get-session
  "Get session data for a ling."
  [ling-id]
  (get @session-registry ling-id))

;;; =============================================================================
;;; Python Bridge Helpers (libpython-clj interop)
;;; =============================================================================

(defn- py-import
  "Safely import a Python module via libpython-clj.
   Returns the module object or nil if unavailable."
  [module-name]
  (try
    (let [import-fn (requiring-resolve 'libpython-clj2.python/import-module)]
      (import-fn module-name))
    (catch Exception e
      (log/warn "[headless-sdk] Failed to import Python module"
                {:module module-name :error (ex-message e)})
      nil)))

(defn- py-call
  "Call a Python function with args.
   Wraps libpython-clj2.python/py. for method calls."
  [obj method & args]
  (try
    (let [call-fn (requiring-resolve 'libpython-clj2.python/py.)]
      (apply call-fn obj method args))
    (catch Exception e
      (log/error "[headless-sdk] Python call failed"
                 {:method method :error (ex-message e)})
      (throw (ex-info "Python call failed"
                      {:method method :error (ex-message e)}
                      e)))))

(defn- py-attr
  "Get a Python object attribute."
  [obj attr]
  (try
    (let [attr-fn (requiring-resolve 'libpython-clj2.python/py.-)]
      (attr-fn obj attr))
    (catch Exception e
      (log/warn "[headless-sdk] Failed to get Python attribute"
                {:attr attr :error (ex-message e)})
      nil)))

(defn- py-call-kw
  "Call a Python callable with positional and keyword arguments.
   Uses libpython-clj2.python/call-kw for proper kwargs passing."
  [callable positional-args kw-args]
  (try
    (let [call-kw-fn (requiring-resolve 'libpython-clj2.python/call-kw)]
      (call-kw-fn callable positional-args kw-args))
    (catch Exception e
      (log/error "[headless-sdk] Python keyword call failed"
                 {:error (ex-message e)})
      (throw (ex-info "Python keyword call failed"
                      {:error (ex-message e)} e)))))

(defn- py->clj
  "Convert a Python object to Clojure data."
  [py-obj]
  (try
    (let [convert-fn (requiring-resolve 'libpython-clj2.python/->jvm)]
      (convert-fn py-obj))
    (catch Exception _
      py-obj)))

(defn- py-run
  "Run a Python string and return the last value.
   Wraps libpython-clj2.python/run-simple-string."
  [code]
  (let [run-fn (requiring-resolve 'libpython-clj2.python/run-simple-string)]
    (run-fn code)))

(defn- py-set-global!
  "Set a variable in Python's __main__ namespace.
   Used to inject Clojure-constructed Python objects into scripts."
  [var-name value]
  (try
    (let [set-fn (requiring-resolve 'libpython-clj2.python/set-attr!)
          main-mod (py-import "__main__")]
      (set-fn main-mod var-name value))
    (catch Exception e
      (log/error "[headless-sdk] Failed to set Python global"
                 {:var-name var-name :error (ex-message e)})
      (throw (ex-info "Failed to set Python global"
                      {:var-name var-name :error (ex-message e)} e)))))

(defn- py-get-global
  "Get a variable from Python's __main__ namespace.
   Used to extract results from async bridge scripts."
  [var-name]
  (try
    (let [main-mod (py-import "__main__")]
      (py-attr main-mod var-name))
    (catch Exception e
      (log/warn "[headless-sdk] Failed to get Python global"
                {:var-name var-name :error (ex-message e)})
      nil)))

;;; =============================================================================
;;; SDK Query Execution (ClaudeSDKClient multi-turn)
;;; =============================================================================

(defn- build-agents-dict
  "Build a Python dict of AgentDefinition objects from Clojure agent specs.

   Converts Clojure agent definitions map into Python AgentDefinition instances
   suitable for ClaudeAgentOptions.agents parameter.

   Each agent spec is a map with:
     :description - Agent description string (required)
     :prompt      - Agent system prompt string (required)
     :tools       - List of tool name strings (optional)
     :model       - Model name: 'sonnet', 'opus', 'haiku', 'inherit' (optional)

   Arguments:
     agents - Map of agent-name -> agent-spec, e.g.:
              {\"code-reviewer\" {:description \"Reviews code\"
                                 :prompt \"You are a reviewer\"
                                 :tools [\"Read\" \"Grep\"]
                                 :model \"sonnet\"}}

   Returns:
     Python dict of {name: AgentDefinition(...)} or nil if agents is empty."
  [agents]
  (when (seq agents)
    (let [sdk-mod (py-import "claude_agent_sdk")
          agent-def-class (py-attr sdk-mod "AgentDefinition")]
      (reduce-kv
       (fn [m agent-name agent-spec]
         (let [{:keys [description prompt tools model]} agent-spec
               ;; Only include non-nil fields
               kw-args (cond-> {:description (or description "")
                                :prompt (or prompt "")}
                         tools (assoc :tools (vec tools))
                         model (assoc :model model))]
           (assoc m agent-name (py-call-kw agent-def-class [] kw-args))))
       {} agents))))

(defn- build-options-obj
  "Build a ClaudeAgentOptions Python object using proper libpython-clj interop.

   Creates the options object as a real Python dataclass instance via call-kw,
   instead of interpolating into a Python string template. This is safer
   (no injection), more correct (proper types), and more maintainable.

   Arguments:
     phase - SAA phase keyword (:silence :abstract :act)
     opts  - Map with :cwd :system-prompt :session-id :mcp-servers :agents

   The :agents field is a map of agent-name -> agent-spec that gets converted
   to Python AgentDefinition objects. See build-agents-dict for spec format."
  [phase {:keys [cwd system-prompt session-id _mcp-servers agents]}]
  (let [phase-config (get saa-phases phase)
        full-prompt (str (or system-prompt "")
                         "\n\n"
                         (:system-prompt-suffix phase-config))
        sdk-mod (py-import "claude_agent_sdk")
        opts-class (py-attr sdk-mod "ClaudeAgentOptions")
        agents-dict (build-agents-dict agents)]
    (py-call-kw opts-class []
                (cond-> {:allowed_tools (:allowed-tools phase-config)
                         :permission_mode (:permission-mode phase-config)
                         :system_prompt full-prompt}
                  cwd         (assoc :cwd cwd)
                  session-id  (assoc :resume session-id)
                  agents-dict (assoc :agents agents-dict)))))

(defn- execute-phase!
  "Execute a single SAA phase via Claude Agent SDK ClaudeSDKClient.

   Architecture:
   1. Constructs ClaudeAgentOptions as a real Python object (build-options-obj)
   2. Injects options into Python __main__ namespace (py-set-global!)
   3. Runs async bridge script that uses ClaudeSDKClient (multi-turn capable)
   4. Extracts results via py-get-global + py->clj

   ClaudeSDKClient is used instead of query() because:
   - Multi-turn: Same session across SAA phases (connect → query → receive → disconnect)
   - Interactive: Can interrupt or send follow-ups
   - Stateful: Maintains conversation context between phases

   The async bridge via asyncio.run() is required because libpython-clj
   cannot directly bridge Python async generators/coroutines.

   Arguments:
     ling-id - Ling identifier
     prompt  - Task prompt for this phase
     phase   - SAA phase keyword
     opts    - Phase options (cwd, system-prompt, session-id, mcp-servers)

   Returns:
     core.async channel that will receive messages from the phase.
     Channel closes when phase completes."
  [ling-id prompt phase opts]
  (let [out-ch (chan 1024)]
    (log/info "[headless-sdk] Starting phase" {:ling-id ling-id
                                               :phase phase
                                               :prompt (subs prompt 0 (min 100 (count prompt)))})
    ;; Update session phase
    (update-session! ling-id {:phase phase
                              :phase-started-at (System/currentTimeMillis)})
    ;; Execute query in background thread
    (async/thread
      (try
        (let [options-obj (build-options-obj phase opts)]
          ;; Inject pre-built options object into Python namespace
          (py-set-global! "_hive_phase_options" options-obj)
          ;; Async bridge: ClaudeSDKClient with pre-built options object
          ;; The options object is a real Python ClaudeAgentOptions instance,
          ;; not a string template — proper types, no injection risk.
          (py-run (str "import asyncio\n"
                       "from claude_agent_sdk import ClaudeSDKClient\n"
                       "\n"
                       "async def _hive_run_phase():\n"
                       "    results = []\n"
                       "    session_id = None\n"
                       "    client = ClaudeSDKClient(options=_hive_phase_options)\n"
                       "    await client.connect()\n"
                       "    try:\n"
                       "        await client.query(" (pr-str prompt) ")\n"
                       "        async for msg in client.receive_response():\n"
                       "            results.append(str(msg))\n"
                       "            if hasattr(msg, 'session_id'):\n"
                       "                session_id = msg.session_id\n"
                       "    finally:\n"
                       "        await client.disconnect()\n"
                       "    return results, session_id\n"
                       "\n"
                       "_hive_phase_results, _hive_phase_session_id = asyncio.run(_hive_run_phase())\n"))
          ;; Extract results via proper Python object access
          (let [results (py->clj (py-get-global "_hive_phase_results"))
                session-id (py->clj (py-get-global "_hive_phase_session_id"))]
            ;; Store session ID for phase chaining
            (when session-id
              (update-session! ling-id {:session-id session-id}))
            ;; Put results on channel
            (doseq [msg (if (sequential? results) results [results])]
              (>!! out-ch {:type :message :phase phase :data msg}))))
        (catch Exception e
          (log/error "[headless-sdk] Phase execution failed"
                     {:ling-id ling-id :phase phase :error (ex-message e)})
          (>!! out-ch {:type :error :phase phase :error (ex-message e)}))
        (finally
          (close! out-ch)
          (update-session! ling-id {:phase-ended-at (System/currentTimeMillis)}))))
    out-ch))

;;; =============================================================================
;;; SAA Orchestrator
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
                                Map of name -> {:description :prompt :tools :model}
                                Passed to ClaudeAgentOptions.agents for custom
                                subagent definitions in the Claude SDK session.

   Returns:
     {:ling-id    ling-id
      :status     :spawned
      :backend    :agent-sdk
      :phase      :idle}

   Throws:
     ExceptionInfo if SDK not available or ling-id already exists"
  [ling-id {:keys [cwd system-prompt mcp-servers presets agents] :as _opts}]
  {:pre [(string? ling-id)
         (string? cwd)]}
  ;; Check availability
  (let [status (sdk-status)]
    (when-not (= :available status)
      (throw (ex-info "Claude Agent SDK not available"
                      {:ling-id ling-id
                       :sdk-status status
                       :hint (case status
                               :no-libpython "Add clj-python/libpython-clj to deps.edn"
                               :no-sdk "Run: pip install claude-agent-sdk"
                               :not-initialized "Python initialization failed"
                               "Unknown issue")}))))
  ;; Check for duplicate
  (when (get @session-registry ling-id)
    (throw (ex-info "SDK session already exists with this ID"
                    {:ling-id ling-id})))
  ;; Register session
  (let [message-ch (chan 4096)
        result-ch (chan 1)
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
                      :session-id nil}]
    (register-session! ling-id session-data)
    (log/info "[headless-sdk] Spawned SDK ling" {:ling-id ling-id :cwd cwd})
    {:ling-id ling-id
     :status :spawned
     :backend :agent-sdk
     :phase :idle}))

(defn dispatch-headless-sdk!
  "Dispatch a task to an SDK ling, executing the full SAA cycle.

   Runs all three phases sequentially:
   1. Silence: Observe with read-only tools
   2. Abstract: Synthesize observations into plan
   3. Act: Execute the plan

   Arguments:
     ling-id - ID of the spawned SDK ling
     task    - Task description string
     opts    - Optional map:
               :skip-silence?  - Skip silence phase (default: false)
               :skip-abstract? - Skip abstract phase (default: false)
               :phase          - Run only this specific phase

   Returns:
     core.async channel that will receive all messages from all phases.
     Final message will be {:type :saa-complete :result ...}"
  [ling-id task & [{:keys [skip-silence? skip-abstract? phase] :as _opts}]]
  {:pre [(string? ling-id)
         (string? task)]}
  (let [session (get-session ling-id)]
    (when-not session
      (throw (ex-info "SDK session not found" {:ling-id ling-id})))
    (let [out-ch (chan 4096)
          session-opts {:cwd (:cwd session)
                        :system-prompt (:system-prompt session)
                        :mcp-servers (:mcp-servers session)
                        :agents (:agents session)}]
      ;; Run SAA phases in a background thread
      (async/thread
        (try
          ;; === SILENCE PHASE ===
          (when-not (or skip-silence? (and phase (not= phase :silence)))
            (let [silence-prompt (str "TASK: " task
                                      "\n\nExplore the codebase and collect context. "
                                      "List all relevant files, patterns, and observations.")
                  phase-ch (execute-phase! ling-id silence-prompt :silence session-opts)]
              ;; Collect observations from silence phase
              (loop []
                (when-let [msg (<!! phase-ch)]
                  (>!! out-ch (assoc msg :saa-phase :silence))
                  ;; Store observations
                  (when (= :message (:type msg))
                    (update-session! ling-id
                                     {:observations (conj (or (:observations (get-session ling-id)) [])
                                                          (:data msg))}))
                  (recur)))))

          ;; === ABSTRACT PHASE ===
          (when-not (or skip-abstract? (and phase (not= phase :abstract)))
            (let [observations (:observations (get-session ling-id))
                  abstract-prompt (str "Based on these observations from the Silence phase:\n"
                                       (pr-str observations)
                                       "\n\nSynthesize these into a concrete action plan for: " task)
                  phase-ch (execute-phase! ling-id abstract-prompt :abstract
                                           (assoc session-opts
                                                  :session-id (:session-id (get-session ling-id))))]
              (loop []
                (when-let [msg (<!! phase-ch)]
                  (>!! out-ch (assoc msg :saa-phase :abstract))
                  (recur)))))

          ;; === ACT PHASE ===
          (when-not (and phase (not= phase :act))
            (let [act-prompt (str "Execute the plan for: " task
                                  "\n\nFollow the plan precisely. Make changes file by file.")
                  phase-ch (execute-phase! ling-id act-prompt :act
                                           (assoc session-opts
                                                  :session-id (:session-id (get-session ling-id))))]
              (loop []
                (when-let [msg (<!! phase-ch)]
                  (>!! out-ch (assoc msg :saa-phase :act))
                  (recur)))))

          ;; SAA cycle complete
          (>!! out-ch {:type :saa-complete
                       :ling-id ling-id
                       :observations-count (count (:observations (get-session ling-id)))})
          (log/info "[headless-sdk] SAA cycle complete" {:ling-id ling-id})

          (catch Exception e
            (log/error "[headless-sdk] SAA cycle failed"
                       {:ling-id ling-id :error (ex-message e)})
            (>!! out-ch {:type :error :error (ex-message e)}))
          (finally
            (close! out-ch))))
      out-ch)))

;;; =============================================================================
;;; Lifecycle
;;; =============================================================================

(defn kill-headless-sdk!
  "Terminate an SDK ling session.

   Closes channels and removes from registry.
   Python resources are GC'd automatically by libpython-clj.

   Arguments:
     ling-id - ID of the SDK ling

   Returns:
     {:killed? true :ling-id ling-id}"
  [ling-id]
  (if (get-session ling-id)
    (do
      (unregister-session! ling-id)
      (log/info "[headless-sdk] SDK ling killed" {:ling-id ling-id})
      {:killed? true :ling-id ling-id})
    (throw (ex-info "SDK session not found" {:ling-id ling-id}))))

(defn sdk-status-for
  "Get the status of an SDK ling."
  [ling-id]
  (when-let [session (get-session ling-id)]
    {:ling-id ling-id
     :phase (:phase session)
     :phase-history (:phase-history session)
     :observations-count (count (:observations session))
     :started-at (:started-at session)
     :uptime-ms (- (System/currentTimeMillis) (:started-at session))
     :cwd (:cwd session)
     :backend :agent-sdk
     :session-id (:session-id session)}))

(defn list-sdk-sessions
  "List all active SDK sessions."
  []
  (->> @session-registry
       keys
       (map sdk-status-for)
       (remove nil?)
       vec))

(defn sdk-session?
  "Check if a ling-id corresponds to an SDK session."
  [ling-id]
  (contains? @session-registry ling-id))

(defn kill-all-sdk!
  "Kill all SDK sessions. For cleanup/testing."
  []
  (let [ids (keys @session-registry)
        results (for [id ids]
                  (try
                    (kill-headless-sdk! id)
                    {:success true :id id}
                    (catch Exception e
                      {:success false :id id :error (ex-message e)})))]
    {:killed (count (filter :success results))
     :errors (count (remove :success results))}))

;;; =============================================================================
;;; Integration with silence.clj
;;; =============================================================================

(defn- resolve-silence-fn
  "Dynamically resolve silence.clj functions."
  [fn-sym]
  (try
    (requiring-resolve (symbol "hive-mcp.hot.silence" (name fn-sym)))
    (catch Exception _ nil)))

(defn with-silence-tracking
  "Wrap a phase execution with silence.clj tracking.
   Records file reads and discoveries during the Silence phase."
  [ling-id task body-fn]
  (if-let [start-fn (resolve-silence-fn 'start-exploration!)]
    (let [_session-id (start-fn {:task task :agent-id ling-id})]
      (try
        (body-fn)
        (finally
          (when-let [end-fn (resolve-silence-fn 'end-exploration!)]
            (let [summary (end-fn)]
              (update-session! ling-id {:silence-summary summary}))))))
    ;; silence.clj not available, just run
    (body-fn)))

;;; =============================================================================
;;; Crystal/Core Integration (Recall Scoring)
;;; =============================================================================

(defn score-observations
  "Score observations using crystal/core promotion scoring.
   Filters and prioritizes what matters for the Abstract phase.

   Uses requiring-resolve stub pattern for L3+ scoring."
  [observations]
  (if-let [score-fn (try (requiring-resolve 'hive-knowledge.scoring/score-observations)
                         (catch Exception _ nil))]
    ;; L3+ proprietary scoring
    (score-fn observations)
    ;; L1/L2 fallback: simple heuristic scoring
    (let [score-entry (fn [obs]
                        (let [content (str (or (:data obs) obs))
                              ;; Simple heuristics
                              has-pattern? (re-find #"pattern|convention|decision" content)
                              has-issue? (re-find #"bug|error|issue|fix" content)
                              has-test? (re-find #"test|spec|assert" content)
                              base-score 1.0]
                          (cond-> base-score
                            has-pattern? (+ 2.0)
                            has-issue? (+ 3.0)
                            has-test? (+ 1.5))))]
      (->> observations
           (map (fn [obs] {:observation obs :score (score-entry obs)}))
           (sort-by :score >)
           vec))))

(comment
  ;; Usage examples

  ;; Check if SDK is available
  ;; (sdk-status)  ;; => :available | :no-libpython | :no-sdk

  ;; Spawn an SDK ling
  ;; (spawn-headless-sdk! "sdk-ling-1" {:cwd "/home/user/project"})

  ;; Dispatch SAA cycle
  ;; (let [ch (dispatch-headless-sdk! "sdk-ling-1" "Fix the auth bug")]
  ;;   (go-loop []
  ;;     (when-let [msg (<! ch)]
  ;;       (println "SAA msg:" (:type msg) (:saa-phase msg))
  ;;       (recur))))

  ;; Check status
  ;; (sdk-status-for "sdk-ling-1")

  ;; Kill session
  ;; (kill-headless-sdk! "sdk-ling-1")

  ;; Score observations
  ;; (score-observations [{:data "found a bug in auth.clj"}
  ;;                      {:data "read the test file"}
  ;;                      {:data "discovered a pattern for validation"}])

  ;; List all
  ;; (list-sdk-sessions)
  )
