(ns hive-mcp.tools.consolidated.workflow
  "Consolidated Workflow CLI tool — Forja Belt automation.

   Canonical commands:
     catchup               — delegates to session/catchup
     wrap                  — delegates to session/wrap
     complete              — delegates to session/complete
     forge-strike          — ONE FSM-driven cycle: smite→survey→spark
     forge-strike-legacy   — DEPRECATED imperative path (behind :legacy-forge config flag)
     forge-status          — belt dashboard (includes FSM trace data)
     forge-quench          — graceful stop, no new spawns

   The forge-strike command implements the Forja Belt cycle via FSM:
   1. SMITE  — kill completed/zombie lings
   2. SURVEY — kanban(todo), check readiness, rank by priority
   3. SPARK  — spawn lings up to max_slots, dispatch tasks

   Config flag :legacy-forge (services.forge.legacy) — when true, 'forge strike'
   delegates to the imperative path instead of FSM. For emergency rollback only.

   Usage via MCP: workflow {\"command\": \"forge strike\", \"max_slots\": 10}

   SOLID: Facade pattern - single tool entry point for workflow lifecycle.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.consolidated.session :as c-session]
            [hive-mcp.tools.consolidated.agent :as c-agent]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.config :as config]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.workflows.forge-belt :as forge-belt]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Forge State (belt running/quenched)
;; =============================================================================

(defonce ^:private forge-state
  (atom {:quenched? false
         :last-strike nil
         :last-fsm-result nil
         :total-smited 0
         :total-sparked 0
         :total-strikes 0}))

;; =============================================================================
;; SMITE — Kill completed/zombie lings
;; =============================================================================

(defn- smite!
  "Kill completed, idle-done, or error lings in the current project.
   Returns {:smited [...] :failed [...] :count N}.

   CLARITY: Y - Safe failure, only kills lings with terminal statuses."
  [{:keys [directory]}]
  (let [all-agents (queries/get-all-slaves)
        project-id (when directory (scope/get-current-project-id directory))
        ;; Filter to lings (depth=1) with terminal statuses or idle forja lings
        terminal? #{:completed :done :error :zombie}
        forja? (fn [agent] (some-> (:slave/id agent) (str/starts-with? "swarm-forja-")))
        smitable? (fn [agent]
                    (let [status (:slave/status agent)]
                      (or (terminal? status)
                          (and (= :idle status) (forja? agent)))))
        candidates (->> all-agents
                        (filter #(= 1 (:slave/depth %)))
                        (filter smitable?)
                        (filter #(or (nil? project-id)
                                     (nil? (:slave/project-id %))
                                     (= project-id (:slave/project-id %)))))]
    (if (empty? candidates)
      {:smited [] :failed [] :count 0}
      (let [results (doall
                     (for [agent candidates]
                       (let [id (:slave/id agent)]
                         (try
                           (let [ling-agent (ling/->ling id {:cwd (:slave/cwd agent)
                                                             :presets (or (:slave/presets agent) [])
                                                             :project-id (:slave/project-id agent)
                                                             :spawn-mode (or (:ling/spawn-mode agent) :vterm)})
                                 _result (proto/kill! ling-agent)]
                             (log/info "SMITE: killed" {:id id :status (:slave/status agent)})
                             {:id id :status (name (:slave/status agent)) :killed true})
                           (catch Exception e
                             (log/warn "SMITE: failed to kill" {:id id :error (ex-message e)})
                             {:id id :error (ex-message e) :killed false})))))]
        {:smited (filterv :killed results)
         :failed (filterv (complement :killed) results)
         :count (count (filter :killed results))}))))

;; =============================================================================
;; SURVEY — Kanban todo tasks, ranked by priority
;; =============================================================================

(defn- survey
  "Query kanban for todo tasks, check readiness, rank by priority.
   Returns {:tasks [...] :count N}.

   CLARITY: R - Read-only survey of available work."
  [{:keys [directory]}]
  (try
    (let [;; Get todo tasks via kanban list handler
          result (c-kanban/handle-kanban {:command "list" :status "todo" :directory directory})
          ;; Parse the result — it's an MCP response with :text containing JSON
          text (if (map? result) (:text result) result)
          parsed (when (string? text)
                   (try (json/read-str text :key-fn keyword)
                        (catch Exception _ nil)))
          tasks (cond
                  (sequential? parsed) parsed        ;; kanban list returns flat array
                  (map? parsed)        (or (:tasks parsed) [])
                  :else                [])
          ;; Sort by priority: high > medium > low
          priority-order {"high" 0 "priority-high" 0
                          "medium" 1 "priority-medium" 1
                          "low" 2 "priority-low" 2}
          sorted-tasks (->> tasks
                            (sort-by #(get priority-order
                                           (or (:priority %) "medium")
                                           1)))]
      {:tasks (vec sorted-tasks)
       :count (count sorted-tasks)})
    (catch Exception e
      (log/warn "SURVEY failed" {:error (ex-message e)})
      {:tasks [] :count 0 :error (ex-message e)})))

;; =============================================================================
;; Ling Readiness Poll (two-phase: DataScript + CLI readiness)
;; =============================================================================

(def ^:private ling-ready-timeout-ms
  "Maximum time to wait for ling readiness in milliseconds.
   Default 5000ms (5s). Claude Code CLI typically initializes in 3-15 seconds.
   Fast lings dispatch in <1s, slow ones get the full 5s window.
   Not ^:const so tests can with-redefs."
  5000)

(def ^:private ling-ready-poll-ms
  "Fixed poll interval in milliseconds. No backoff — short timeout makes
   backoff unnecessary, and fixed intervals give consistent responsiveness.
   Not ^:const so tests can with-redefs."
  50)

(defn- vterm-ready?
  "Check if a vterm ling's CLI is ready for input.
   Evaluates elisp `hive-mcp-swarm--slave-ready-p` to detect the prompt
   marker in the vterm buffer's last 1500 characters.

   CLARITY: I - Input validation before dispatch."
  [agent-id]
  (try
    (let [elisp (format "(if (hive-mcp-swarm--slave-ready-p \"%s\") \"t\" \"nil\")" agent-id)
          result (ec/eval-elisp-with-timeout elisp 2000)]
      (and (:success result)
           (= "t" (:result result))))
    (catch Exception e
      (log/debug "vterm-ready? check failed" {:agent-id agent-id :error (ex-message e)})
      false)))

(defn- headless-ready?
  "Check if a headless ling's process is alive and has produced stdout.
   Stdout output indicates the CLI has started and is processing.

   CLARITY: I - Input validation before dispatch."
  [agent-id]
  (try
    (when-let [status (headless/headless-status agent-id)]
      (and (:alive? status)
           (pos? (get-in status [:stdout :total-lines-seen] 0))))
    (catch Exception _
      false)))

(defn- ling-cli-ready?
  "Mode-dispatch readiness check. Returns truthy when the ling's CLI
   is ready to receive a dispatch.

   - :vterm      — polls elisp for prompt marker in buffer
   - :headless   — checks process alive + stdout output
   - :openrouter — always ready (API-based, no CLI startup)
   - :agent-sdk  — always ready (in-process, no CLI startup)

   Not ^:const so tests can with-redefs."
  [agent-id spawn-mode]
  (case spawn-mode
    :headless   (headless-ready? agent-id)
    :openrouter true
    :agent-sdk  true
    ;; default: vterm
    (vterm-ready? agent-id)))

(defn- wait-for-ling-ready
  "Poll for ling readiness before dispatching (two-phase).

   Each iteration checks:
     1. DataScript registration — slave entry exists (usually instant)
     2. CLI readiness — mode-specific check that the process can receive input

   For vterm:      polls elisp for prompt marker in buffer
   For headless:   polls process alive + stdout output
   For openrouter: ready immediately after DS registration
   For agent-sdk:  ready immediately after DS registration

   Uses elapsed-time-based timeout (default 5s) with fixed 50ms poll interval.
   Fast for quick-starting lings, robust timeout for slow ones.

   Returns:
     {:ready? true  :slave map :attempts N :elapsed-ms N :phase :cli-ready}
     {:ready? false :slave map :attempts N :elapsed-ms N :phase :ds-timeout|:cli-timeout}

   CLARITY: I - Validates precondition (ling ready) before dispatch."
  [agent-id spawn-mode]
  (let [start-ms (System/currentTimeMillis)]
    (loop [attempt 1]
      (let [slave (queries/get-slave agent-id)
            cli-ok? (when slave (ling-cli-ready? agent-id spawn-mode))
            elapsed (- (System/currentTimeMillis) start-ms)]
        (cond
          ;; Both checks pass — fully ready for dispatch
          (and slave cli-ok?)
          (do
            (log/debug "SPARK: ling ready" {:agent-id agent-id
                                            :attempts attempt
                                            :elapsed-ms elapsed
                                            :spawn-mode spawn-mode})
            {:ready? true
             :slave slave
             :attempts attempt
             :elapsed-ms elapsed
             :phase :cli-ready})

          ;; Time-based timeout exhausted
          (>= elapsed ling-ready-timeout-ms)
          (do
            (log/warn "SPARK: ling readiness timeout"
                      {:agent-id agent-id
                       :attempts attempt
                       :elapsed-ms elapsed
                       :spawn-mode spawn-mode
                       :ds-found? (some? slave)
                       :phase (if slave :cli-timeout :ds-timeout)})
            {:ready? false
             :slave slave
             :attempts attempt
             :elapsed-ms elapsed
             :phase (if slave :cli-timeout :ds-timeout)})

          ;; Not ready yet — fixed interval poll
          :else
          (do
            (Thread/sleep ling-ready-poll-ms)
            (recur (inc attempt))))))))

;; =============================================================================
;; SPARK — Spawn lings for ready tasks
;; =============================================================================

(defn- spark!
  "Spawn lings up to max_slots for ready tasks.
   Returns {:spawned [...] :failed [...] :count N :slots-used N}.

   Convention: Spawn then dispatch separately (spawn+task param unreliable).
   CLARITY: I - Validates slot availability before spawning."
  [{:keys [directory max_slots presets tasks spawn_mode spawn-mode model]}]
  (let [max-slots (or max_slots 10)
        ;; Resolve spawn mode (FSM uses :spawn-mode, MCP uses :spawn_mode)
        effective-spawn-mode (or spawn_mode spawn-mode)
        ;; Count currently active lings
        active-agents (queries/get-all-slaves)
        active-lings (->> active-agents
                          (filter #(= 1 (:slave/depth %)))
                          (filter #(#{:active :running :working :idle :spawning} (:slave/status %))))
        active-count (count active-lings)
        available-slots (max 0 (- max-slots active-count))
        ;; Take only as many tasks as we have slots for
        tasks-to-spawn (take available-slots tasks)
        ;; Default presets for forja belt lings
        default-presets (or presets ["ling" "mcp-first" "saa"])
        effective-dir (or directory
                          (ctx/current-directory)
                          (System/getProperty "user.dir"))]
    (if (empty? tasks-to-spawn)
      {:spawned [] :failed [] :count 0
       :slots-used 0 :active-before active-count :max-slots max-slots}
      (let [results
            (doall
             (for [task tasks-to-spawn]
               (let [title (or (:title task) (:id task) "untitled")
                     task-id (:id task)
                     agent-name (str "forja-" (System/currentTimeMillis))]
                 (try
                   ;; Spawn ling
                   (let [spawn-result (c-agent/handle-spawn
                                       (cond-> {:type "ling"
                                                :name agent-name
                                                :cwd effective-dir
                                                :presets default-presets
                                                :kanban_task_id task-id}
                                         effective-spawn-mode (assoc :spawn_mode (name effective-spawn-mode))
                                         model (assoc :model model)))
                         ;; Extract agent-id from spawn result
                         spawn-text (when (map? spawn-result) (:text spawn-result))
                         spawn-parsed (when (string? spawn-text)
                                        (try (json/read-str spawn-text :key-fn keyword)
                                             (catch Exception _ nil)))
                         agent-id (or (:agent-id spawn-parsed) agent-name)
                         spawn-mode (keyword (or (:spawn-mode spawn-parsed) "vterm"))]
                     ;; Wait for ling CLI readiness before dispatching
                     (let [ready (wait-for-ling-ready agent-id spawn-mode)]
                       (when-not (:ready? ready)
                         (log/warn "SPARK: dispatching despite readiness timeout"
                                   {:agent-id agent-id :elapsed-ms (:elapsed-ms ready)}))
                       (c-agent/handle-dispatch
                        {:agent_id agent-id
                         :prompt (str title
                                      (when-let [desc (:description task)]
                                        (str "\n\n" desc))
                                      "\n\nDO NOT spawn drones. Implement directly.")}))
                     ;; Move kanban task to inprogress to prevent duplicate assignment
                     (when task-id
                       (try (c-kanban/handle-kanban {:command "update" :task_id task-id
                                                     :new_status "inprogress"
                                                     :directory effective-dir})
                            (catch Exception _ nil)))
                     (log/info "SPARK: spawned+dispatched" {:agent agent-id :task title :model (or model "claude")})
                     {:agent-id agent-id :task-title title :task-id task-id :spawned true
                      :model (or model "claude")})
                   (catch Exception e
                     (log/warn "SPARK: failed" {:task title :error (ex-message e)})
                     {:agent-id agent-name :task-title title :task-id task-id
                      :spawned false :error (ex-message e)})))))]
        {:spawned (filterv :spawned results)
         :failed (filterv (complement :spawned) results)
         :count (count (filter :spawned results))
         :slots-used available-slots
         :active-before active-count
         :max-slots max-slots}))))

;; =============================================================================
;; FSM Resource Adapter — bridges FSM handlers to imperative implementations
;; =============================================================================

(defn build-fsm-resources
  "Build the resources map for the Forge Belt FSM.

   Wraps the imperative smite!/survey/spark! implementations into the
   pure resource function contracts that FSM handlers expect.

   Args:
     params — {:directory string, :max_slots int, :presets [string],
               :spawn_mode string, :model string}

   Returns a resources map conforming to forge_belt.clj's contract:
     {:agent-ops, :kanban-ops, :scope-fn, :clock-fn, :config, :directory}

   SOLID: D — FSM depends on abstractions (resource fns), not concretions.
   CLARITY: L — Adapter at boundary between pure FSM and imperative tools."
  [{:keys [directory max_slots presets spawn_mode model]}]
  (let [effective-spawn-mode (when spawn_mode (keyword spawn_mode))]
    {:directory  directory
     :config     {:max-slots  (or max_slots 10)
                  :presets    (or presets ["ling" "mcp-first" "saa"])
                  :spawn-mode effective-spawn-mode
                  :model      model}
     :agent-ops  {:kill-fn  (fn [dir _project-id]
                              (smite! {:directory dir}))
                  :spawn-fn (fn [{:keys [directory max-slots presets tasks] :as opts}]
                              (spark! (cond-> {:directory directory
                                               :max_slots max-slots
                                               :presets presets
                                               :tasks tasks}
                                        ;; Forward spawn-mode from opts (set by FSM handle-spark)
                                        ;; or fall back to config-level spawn mode
                                        (or (:spawn-mode opts) effective-spawn-mode)
                                        (assoc :spawn-mode (or (:spawn-mode opts) effective-spawn-mode))
                                        ;; Forward model from config if not overridden in opts
                                        (or (:model opts) model)
                                        (assoc :model (or (:model opts) model)))))}
     :kanban-ops {:list-fn   (fn [dir] (survey {:directory dir}))
                  :update-fn (fn [_opts] nil)}
     :scope-fn   (fn [dir]
                   (when dir (scope/get-current-project-id dir)))
     :clock-fn   #(java.time.Instant/now)}))

;; =============================================================================
;; Forge Strike (Legacy) — DEPRECATED, use 'forge strike' (FSM) instead
;; =============================================================================

(defn handle-forge-strike-legacy
  "DEPRECATED: Execute ONE forge cycle via imperative smite!/survey/spark!.

   Retained behind config flag {:services {:forge {:legacy true}}} for
   emergency rollback. Use 'forge strike' (FSM-driven) instead.
   This legacy path will be removed in a future release.

   CLARITY: A - Legacy orchestration, superseded by FSM path."
  [{:keys [directory max_slots presets spawn_mode model] :as params}]
  (log/warn "DEPRECATED: forge strike-legacy called. Use 'forge strike' (FSM) instead."
            {:directory directory})
  (if (:quenched? @forge-state)
    (mcp-json {:success false
               :message "Forge is quenched. Use forge-status to check or restart."
               :quenched? true})
    (try
      (log/info "FORGE STRIKE (legacy): Starting cycle" {:directory directory :max-slots max_slots
                                                         :spawn-mode (or spawn_mode "vterm")
                                                         :model (or model "claude")})

      ;; Phase 1: SMITE
      (let [smite-result (smite! params)
            _ (log/info "FORGE STRIKE: SMITE complete" {:killed (:count smite-result)})

            ;; Phase 2: SURVEY
            survey-result (survey params)
            _ (log/info "FORGE STRIKE: SURVEY complete" {:tasks (:count survey-result)})

            ;; Phase 3: SPARK
            spark-result (spark! (assoc params
                                        :tasks (:tasks survey-result)
                                        :max_slots max_slots
                                        :presets presets
                                        :model model))
            _ (log/info "FORGE STRIKE: SPARK complete" {:spawned (:count spark-result)})]

        ;; Update forge state
        (swap! forge-state
               (fn [s]
                 (-> s
                     (update :total-smited + (:count smite-result))
                     (update :total-sparked + (:count spark-result))
                     (update :total-strikes inc)
                     (assoc :last-strike (str (java.time.Instant/now))))))

        (mcp-json {:success true
                   :mode :imperative
                   :deprecated true
                   :spawn-mode (or spawn_mode "vterm")
                   :model (or model "claude")
                   :smite smite-result
                   :survey {:todo-count (:count survey-result)
                            :task-titles (mapv :title (:tasks survey-result))}
                   :spark spark-result
                   :summary (str "DEPRECATED legacy: Smited " (:count smite-result)
                                 ", surveyed " (:count survey-result) " tasks"
                                 ", sparked " (:count spark-result) " lings"
                                 " (mode: " (or spawn_mode "vterm")
                                 ", model: " (or model "claude") ")")}))
      (catch Exception e
        (log/error "FORGE STRIKE (legacy) failed" {:error (ex-message e)})
        (mcp-error (str "Forge strike (legacy) failed: " (ex-message e)))))))

;; =============================================================================
;; Forge Strike — FSM-driven cycle (DEFAULT since 0.12.0)
;; =============================================================================

(defn- fsm-forge-strike
  "FSM-driven forge strike implementation (internal).
   Called by handle-forge-strike when not in legacy mode."
  [{:keys [directory max_slots spawn_mode model] :as params}]
  (try
    (log/info "FORGE STRIKE: Starting FSM cycle" {:directory directory :max-slots max_slots
                                                  :spawn-mode (or spawn_mode "vterm")
                                                  :model (or model "claude")})
    (let [resources (build-fsm-resources params)
          result (forge-belt/run-single-strike resources)]
      ;; Update forge state from FSM result
      (swap! forge-state
             (fn [s]
               (-> s
                   (update :total-smited + (:total-smited result 0))
                   (update :total-sparked + (:total-sparked result 0))
                   (update :total-strikes inc)
                   (assoc :last-strike (:last-strike result)))))
      (mcp-json {:success true
                 :mode :fsm
                 :spawn-mode (or spawn_mode "vterm")
                 :model (or model "claude")
                 :smite (:smite-result result)
                 :survey {:todo-count (get-in result [:survey-result :count] 0)
                          :task-titles (mapv :title (get-in result [:survey-result :tasks] []))}
                 :spark (:spark-result result)
                 :summary (str "Smited " (:total-smited result 0)
                               ", surveyed " (get-in result [:survey-result :count] 0) " tasks"
                               ", sparked " (:total-sparked result 0) " lings"
                               " (mode: " (or spawn_mode "vterm")
                               ", model: " (or model "claude") ")")}))
    (catch Exception e
      (log/error "FORGE STRIKE failed" {:error (ex-message e)})
      (mcp-error (str "Forge strike failed: " (ex-message e))))))

(defn handle-forge-strike
  "Execute ONE forge cycle. FSM-driven by default since 0.12.0.

   Config flag {:services {:forge {:legacy true}}} enables the deprecated
   imperative path for emergency rollback. Otherwise routes to FSM engine.

   Parameters:
     directory   - Working directory for project scoping (optional)
     max_slots   - Max concurrent lings (default: 10)
     presets     - Ling presets (default: [ling mcp-first saa])
     spawn_mode  - Spawn mode: 'vterm' (default) or 'headless'
                   'headless' maps to :agent-sdk (default headless since 0.12.0).
     model       - Model override for spawned lings: 'claude' (default) or
                   OpenRouter model ID (e.g. 'deepseek/deepseek-chat').
                   Non-claude models auto-force headless/openrouter spawn mode.

   Returns combined result of all three phases.

   CLARITY: C - Composes FSM runner with resource adapter + config-gated legacy fallback."
  [params]
  (if (:quenched? @forge-state)
    (mcp-json {:success false
               :message "Forge is quenched. Use forge-status to check or restart."
               :quenched? true})
    (if (config/get-service-value :forge :legacy :default false)
      (do
        (log/warn "FORGE STRIKE: legacy mode enabled via config. Using imperative path.")
        (handle-forge-strike-legacy params))
      (fsm-forge-strike params))))

;; Backward-compat aliases for callers that reference old names
(def handle-forge-strike-fsm
  "DEPRECATED alias: FSM is now the default path via handle-forge-strike.
   Retained for backward compatibility with tests and callers."
  handle-forge-strike)

(def handle-forge-strike-imperative
  "DEPRECATED alias: renamed to handle-forge-strike-legacy.
   Retained for backward compatibility with callers using the old name."
  handle-forge-strike-legacy)

;; =============================================================================
;; Forge Status — Belt dashboard
;; =============================================================================

(defn handle-forge-status
  "Belt dashboard: show forge state, active lings, kanban summary.

   CLARITY: R - Read-only status query."
  [{:keys [directory] :as _params}]
  (try
    (let [state @forge-state
          ;; Get active ling count
          all-agents (queries/get-all-slaves)
          lings (->> all-agents
                     (filter #(= 1 (:slave/depth %))))
          active-lings (filter #(#{:active :running :idle :spawning} (:slave/status %)) lings)
          terminal-lings (filter #(#{:completed :done :error :zombie} (:slave/status %)) lings)
          ;; Get kanban stats
          kanban-result (try
                          (c-kanban/handle-kanban {:command "status" :directory directory})
                          (catch Exception _ nil))]
      (mcp-json {:forge (assoc state :modes {:fsm true
                                             :legacy (boolean (config/get-service-value :forge :legacy :default false))})
                 :lings {:total (count lings)
                         :active (count active-lings)
                         :terminal (count terminal-lings)
                         :ids (mapv :slave/id active-lings)}
                 :kanban kanban-result}))
    (catch Exception e
      (mcp-error (str "Forge status failed: " (ex-message e))))))

;; =============================================================================
;; Forge Quench — Graceful stop
;; =============================================================================

(defn handle-forge-quench
  "Gracefully stop the forge belt. No new spawns allowed until unquenched.
   Active lings continue to completion. Pass restart=true to unquench.

   CLARITY: Y - Safe, doesn't kill active lings."
  [{:keys [restart]}]
  (if restart
    (do
      (swap! forge-state assoc :quenched? false)
      (log/info "FORGE RESTART: Belt restarted. forge-strike is available again.")
      (mcp-json {:success true
                 :message "Forge restarted. Ready for forge-strike."
                 :state @forge-state}))
    (do
      (swap! forge-state assoc :quenched? true)
      (log/info "FORGE QUENCH: Belt stopped. Active lings will continue to completion.")
      (mcp-json {:success true
                 :message "Forge quenched. Active lings will finish. No new spawns."
                 :state @forge-state}))))

;; =============================================================================
;; Handlers Map
;; =============================================================================

(def canonical-handlers
  "Core command → handler map. Forge commands are nested under :forge.
   Supports n-depth dispatch: 'forge strike', 'forge status', 'forge quench'.

   Since 0.12.0: 'forge strike' IS the FSM path (default).
   Config {:services {:forge {:legacy true}}} enables imperative fallback.
   'forge strike-imperative' kept as alias → handle-forge-strike-legacy."
  {:catchup  c-session/handle-catchup
   :wrap     c-session/handle-wrap
   :complete (fn [params] (c-session/handle-session (assoc params :command "complete")))
   :forge    {:strike             handle-forge-strike           ;; FSM (default) with legacy config gate
              :strike-imperative  handle-forge-strike-imperative ;; alias → legacy (backward compat)
              :status             handle-forge-status
              :quench             handle-forge-quench
              :_handler           handle-forge-status}})

(def handlers
  "Canonical + deprecated aliases."
  canonical-handlers)

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-workflow
  "Unified CLI handler for workflow operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated workflow command."
  {:name "workflow"
   :consolidated true
   :description "Forja Belt workflow: catchup (restore context), wrap (crystallize), complete (full lifecycle), forge-strike (FSM-driven smite→survey→spark cycle), forge-strike-imperative (DEPRECATED legacy path), forge-status (belt dashboard), forge-quench (graceful stop). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["catchup" "wrap" "complete"
                                                "forge strike"
                                                "forge strike-imperative"
                                                "forge status" "forge quench"
                                                "help"]
                                         :description "Workflow operation to perform"}
                              ;; session params (catchup/wrap/complete)
                              "commit_msg" {:type "string"
                                            :description "Git commit message (for complete)"}
                              "task_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Kanban task IDs to mark done (for complete)"}
                              "agent_id" {:type "string"
                                          :description "Agent ID for session attribution"}
                              "directory" {:type "string"
                                           :description "Working directory for project scoping"}
                              ;; forge params
                              "max_slots" {:type "integer"
                                           :description "Max concurrent lings for forge-strike (default: 10)"}
                              "presets" {:type "array"
                                         :items {:type "string"}
                                         :description "Ling presets for forge-strike (default: [ling, mcp-first, saa])"}
                              "spawn_mode" {:type "string"
                                            :enum ["vterm" "headless"]
                                            :description "Spawn mode for lings: 'vterm' (default, Emacs buffer) or 'headless' (OS subprocess, no Emacs required). Note: 'headless' maps to :agent-sdk (Claude Agent SDK) since 0.12.0."}
                              "model" {:type "string"
                                       :description "Model override for spawned lings: 'claude' (default, Claude Code CLI) or OpenRouter model ID (e.g., 'deepseek/deepseek-chat'). Non-claude models auto-force headless/openrouter spawn mode for cost-efficient bulk work."}
                              ;; quench params
                              "restart" {:type "boolean"
                                         :description "Pass true to unquench/restart the forge belt"}}
                 :required ["command"]}
   :handler handle-workflow})

(def tools
  "Tool definitions for registration."
  [tool-def])
