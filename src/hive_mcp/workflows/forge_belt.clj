(ns hive-mcp.workflows.forge-belt
  "hive-events FSM spec for the Forge Belt (Vulcan Mode) workflow.

   The forge belt is a continuous production cycle:
     ::start → ::smite → ::survey → ::spark → (loop back to ::smite or ::end)

   Design constraints:
   - Handlers are PURE functions: (resources, data) → data'
   - Side effects flow through the resources map (L1 territory)
   - The FSM is the L2 map — deterministic state transitions
   - Dispatch predicates are pure functions of state data
   - Subscriptions observe metrics for telemetry
   - Handler results flow through hive-events fx system

   Resources map (injected at run time):
     :ds-conn          — DataScript connection (swarm state)
     :directory         — Working directory for project scoping
     :config            — {:max-slots N, :presets [...], :spawn-mode kw}
     :agent-ops         — {:kill-fn, :spawn-fn, :dispatch-fn, :wait-ready-fn}
     :kanban-ops        — {:list-fn, :update-fn}
     :scope-fn          — (directory) → project-id
     :clock-fn          — () → instant (for testability)

   State data shape:
     {:phase          kw        ;; current phase name for telemetry
      :quenched?      bool      ;; belt stopped?
      :strike-count   int       ;; total cycles completed
      :smite-result   map       ;; {:smited [] :failed [] :count N}
      :survey-result  map       ;; {:tasks [] :count N}
      :spark-result   map       ;; {:spawned [] :failed [] :count N}
      :error          any       ;; error info if in error state
      :total-smited   int       ;; cumulative smited count
      :total-sparked  int       ;; cumulative sparked count
      :last-strike    string    ;; ISO instant of last strike
      :continuous?    bool}     ;; loop or single-shot?

   SOLID: SRP — FSM spec only, no side effects.
   CLARITY: L — Pure layer, side effects via resources.
   CLARITY: R — States represent domain intent."
  (:require [hive.events.fsm :as fsm]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Dispatch Predicates (pure functions of state data)
;; =============================================================================

(defn quenched?
  "Check if the forge belt has been quenched (graceful stop)."
  [data]
  (true? (:quenched? data)))

(defn has-tasks?
  "Check if survey found any todo tasks."
  [data]
  (pos? (get-in data [:survey-result :count] 0)))

(defn no-tasks?
  "Check if survey found zero todo tasks."
  [data]
  (zero? (get-in data [:survey-result :count] 0)))

(defn continuous?
  "Check if the belt should loop back for another cycle."
  [data]
  (true? (:continuous? data)))

(defn single-shot?
  "Check if the belt should end after one cycle."
  [data]
  (not (:continuous? data)))

(defn always [_data] true)

;; =============================================================================
;; Handlers (pure functions: resources × data → data')
;;
;; Each handler receives the resources map and the current state data.
;; It returns updated state data. Side effects are performed by the
;; caller (via resources map functions), NOT by handlers directly.
;; Handler results flow through hive-events fx system.
;; =============================================================================

(defn handle-start
  "Initialize a forge strike cycle.
   Sets phase marker and validates preconditions."
  [resources data]
  (let [clock-fn (or (:clock-fn resources) #(java.time.Instant/now))]
    (assoc data
           :phase ::smite
           :cycle-start (str (clock-fn))
           :smite-result nil
           :survey-result nil
           :spark-result nil
           :error nil)))

(defn handle-smite
  "Kill completed/zombie lings in the current project.

   Uses resources:
     :agent-ops {:kill-fn (fn [directory project-id] → {:smited [] :failed [] :count N})}
     :scope-fn  (fn [directory] → project-id)
     :directory  string"
  [resources data]
  (let [{:keys [agent-ops scope-fn directory]} resources
        kill-fn (:kill-fn agent-ops)
        project-id (when (and scope-fn directory) (scope-fn directory))
        result (kill-fn directory project-id)]
    (-> data
        (assoc :phase ::survey
               :smite-result result)
        (update :total-smited + (:count result 0)))))

(defn handle-survey
  "Query kanban for todo tasks, ranked by priority.

   Uses resources:
     :kanban-ops {:list-fn (fn [directory] → {:tasks [] :count N})}
     :directory   string"
  [resources data]
  (let [{:keys [kanban-ops directory]} resources
        list-fn (:list-fn kanban-ops)
        result (list-fn directory)]
    (assoc data
           :phase ::spark
           :survey-result result)))

(defn handle-spark
  "Spawn lings up to max_slots for ready tasks.

   Uses resources:
     :agent-ops  {:spawn-fn, :dispatch-fn, :wait-ready-fn}
     :kanban-ops {:update-fn}
     :config     {:max-slots N, :presets [...], :spawn-mode kw, :model string-or-nil}
     :directory   string"
  [resources data]
  (let [{:keys [agent-ops kanban-ops config directory]} resources
        {:keys [spawn-fn dispatch-fn wait-ready-fn]} agent-ops
        {:keys [update-fn]} kanban-ops
        {:keys [max-slots presets spawn-mode model]} config
        tasks (get-in data [:survey-result :tasks] [])
        result (spawn-fn (cond-> {:directory directory
                                  :max-slots (or max-slots 10)
                                  :presets (or presets ["ling" "mcp-first" "saa"])
                                  :tasks tasks
                                  :dispatch-fn dispatch-fn
                                  :wait-ready-fn wait-ready-fn
                                  :update-fn update-fn}
                           spawn-mode (assoc :spawn-mode spawn-mode)
                           model (assoc :model model)))]
    (let [clock-fn (or (:clock-fn resources) #(java.time.Instant/now))]
      (-> data
          (assoc :phase ::cycle-complete
                 :spark-result result
                 :last-strike (str (clock-fn)))
          (update :total-sparked + (:count result 0))
          (update :strike-count (fnil inc 0))))))

(defn handle-end
  "Terminal state handler. Returns final summary."
  [_resources {:keys [data]}]
  (select-keys data [:strike-count :total-smited :total-sparked
                     :last-strike :smite-result :survey-result
                     :spark-result :quenched? :continuous?]))

(defn handle-halt
  "Halt state for quench. Returns FSM state for potential restart."
  [_resources fsm]
  (-> fsm
      (assoc-in [:data :phase] ::halted)
      (dissoc :fsm)))

(defn handle-error
  "Error state handler. Captures error context."
  [_resources {:keys [error data] :as _fsm}]
  (throw (ex-info "Forge belt error"
                  {:phase (:phase data)
                   :data data
                   :error error})))

;; =============================================================================
;; Subscription Handlers (side-effect callbacks for metrics)
;; =============================================================================

(defn on-smite-count-change
  "Subscription handler for :total-smited changes.
   Will be wired to hive-events fx dispatch or prometheus."
  [_path _old-value _new-value]
  nil)

(defn on-spark-count-change
  "Subscription handler for :total-sparked changes.
   Will be wired to hive-events fx dispatch or prometheus."
  [_path _old-value _new-value]
  nil)

;; =============================================================================
;; FSM Spec (the map)
;; =============================================================================

(def forge-belt-spec
  "hive-events FSM spec for the forge belt cycle.

   State graph:
   ```
   ::start ──→ ::smite ──→ ::survey ──→ ::spark ──┐
       │                                           │
       └──────── (continuous?) ─────────── YES ────┘
                                            │
                                            NO ──→ ::end

   Any state ──→ ::halt   (when quenched?)
   Any state ──→ ::error  (on exception)
   ```"
  {:fsm
   {::fsm/start
    {:handler    handle-start
     :dispatches [[::fsm/halt quenched?]
                  [::smite    always]]}

    ::smite
    {:handler    handle-smite
     :dispatches [[::fsm/halt quenched?]
                  [::survey   always]]}

    ::survey
    {:handler    handle-survey
     :dispatches [[::fsm/halt quenched?]
                  [::spark    has-tasks?]
                  [::fsm/end  no-tasks?]]}

    ::spark
    {:handler    handle-spark
     :dispatches [[::fsm/halt    quenched?]
                  [::fsm/start   continuous?]
                  [::fsm/end     single-shot?]]}

    ::fsm/end
    {:handler handle-end}

    ::fsm/halt
    {:handler handle-halt}

    ::fsm/error
    {:handler handle-error}}

   :opts
   {:max-trace 100

    :subscriptions
    {[:total-smited]  {:handler on-smite-count-change}
     [:total-sparked] {:handler on-spark-count-change}}

    :pre
    (fn [{:keys [current-state-id] :as fsm} _resources]
      ;; Telemetry: log state entry
      (update-in fsm [:data :trace-log] (fnil conj [])
                 {:state current-state-id
                  :at (str (java.time.Instant/now))
                  :direction :enter}))

    :post
    (fn [{:keys [current-state-id] :as fsm} _resources]
      ;; Telemetry: log state exit
      (update-in fsm [:data :trace-log] (fnil conj [])
                 {:state current-state-id
                  :at (str (java.time.Instant/now))
                  :direction :exit}))}})

;; =============================================================================
;; Compilation & Execution API
;; =============================================================================

(defn compile-belt
  "Compile the forge belt FSM spec. Call once, reuse the compiled FSM."
  []
  (fsm/compile forge-belt-spec))

(defn run-belt
  "Execute a compiled forge belt FSM.

   Args:
     compiled-fsm — Result of compile-belt
     resources    — Map of side-effect functions and config
     opts         — Optional: {:continuous? bool, :quenched? bool}

   Returns:
     Final state data map with strike results.

   Example:
   ```clojure
   (def belt (compile-belt))
   (run-belt belt
            {:ds-conn      @(ds/get-conn)
             :directory    \"/home/user/project\"
             :config       {:max-slots 5 :presets [\"ling\" \"mcp-first\" \"saa\"]}
             :agent-ops    {:kill-fn smite-impl! :spawn-fn spark-impl!
                           :dispatch-fn dispatch-impl! :wait-ready-fn wait-impl!}
             :kanban-ops   {:list-fn survey-impl! :update-fn kanban-update-impl!}
             :scope-fn     scope/get-current-project-id
             :clock-fn     #(java.time.Instant/now)}
            {:continuous? false})
   ```"
  ([compiled-fsm resources]
   (run-belt compiled-fsm resources {}))
  ([compiled-fsm resources opts]
   (fsm/run compiled-fsm
            resources
            {:data (merge {:quenched? false
                           :continuous? false
                           :strike-count 0
                           :total-smited 0
                           :total-sparked 0}
                          opts)})))

(defn run-single-strike
  "Convenience: compile and run a single forge strike cycle."
  [resources]
  (run-belt (compile-belt) resources {:continuous? false}))

(defn run-continuous-belt
  "Convenience: compile and run continuous forge belt (loops until quenched or no tasks)."
  [resources]
  (run-belt (compile-belt) resources {:continuous? true}))
