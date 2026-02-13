(ns hive-mcp.workflows.forge-belt-defaults
  "Default implementations for all :fb/* extension points.

   Registers defaults so forge strike works out of the box.
   Extensions can override when loaded via load-extensions!.

   Registration order:
   1. This module registers defaults at startup
   2. load-extensions! can overwrite with custom implementations
      ext/register! is idempotent — last write wins."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive.events.fsm :as fsm]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Dispatch Predicates (q1-q5) — Pure data lookups on FSM data map
;; =============================================================================

(defn- quenched?*
  "fb/q1: Check if the forge belt has been quenched (graceful stop)."
  [data]
  (true? (:quenched? data)))

(defn- has-tasks?*
  "fb/q2: Check if survey found any todo tasks."
  [data]
  (pos? (get-in data [:survey-result :count] 0)))

(defn- no-tasks?*
  "fb/q3: Check if survey found zero todo tasks."
  [data]
  (zero? (get-in data [:survey-result :count] 0)))

(defn- continuous?*
  "fb/q4: Check if the belt should loop back for another cycle."
  [data]
  (true? (:continuous? data)))

(defn- single-shot?*
  "fb/q5: Check if the belt should end after one cycle."
  [data]
  (not (:continuous? data)))

;; =============================================================================
;; Handlers (h1-h7) — Call resources ops, stay decoupled from workflow.clj
;; =============================================================================

(defn- handle-start*
  "fb/h1: Initialize a forge strike cycle.
   Sets phase marker, cycle-start timestamp, nils previous results."
  [resources data]
  (let [clock-fn (or (:clock-fn resources) #(java.time.Instant/now))]
    (assoc data
           :phase ::smite
           :cycle-start (str (clock-fn))
           :smite-result nil
           :survey-result nil
           :spark-result nil
           :error nil)))

(defn- handle-smite*
  "fb/h2: Kill completed/zombie lings via (:kill-fn (:agent-ops resources))."
  [resources data]
  (let [{:keys [agent-ops scope-fn directory]} resources
        kill-fn (:kill-fn agent-ops)
        project-id (when (and scope-fn directory) (scope-fn directory))
        result (kill-fn directory project-id)]
    (-> data
        (assoc :phase ::survey
               :smite-result result)
        (update :total-smited + (:count result 0)))))

(defn- handle-survey*
  "fb/h3: Query kanban for todo tasks via (:list-fn (:kanban-ops resources))."
  [resources data]
  (let [{:keys [kanban-ops directory]} resources
        list-fn (:list-fn kanban-ops)
        result (list-fn directory)]
    (assoc data
           :phase ::spark
           :survey-result result)))

(defn- handle-spark*
  "fb/h4: Spawn lings for tasks via (:spawn-fn (:agent-ops resources))."
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
                           model (assoc :model model)))
        clock-fn (or (:clock-fn resources) #(java.time.Instant/now))]
    (-> data
        (assoc :phase ::cycle-complete
               :spark-result result
               :last-strike (str (clock-fn)))
        (update :total-sparked + (:count result 0))
        (update :strike-count (fnil inc 0)))))

(defn- handle-end*
  "fb/h5: Terminal state — select-keys summary + :success true."
  [_resources {:keys [data]}]
  (merge (select-keys data [:strike-count :total-smited :total-sparked
                            :last-strike :smite-result :survey-result
                            :spark-result :quenched? :continuous?])
         {:success true}))

(defn- handle-halt*
  "fb/h6: Halt state for quench — return FSM state for resume."
  [_resources fsm]
  (-> fsm
      (assoc-in [:data :phase] ::halted)
      (dissoc :fsm)))

(defn- handle-error*
  "fb/h7: Error state — throw ex-info with phase + error data."
  [_resources {:keys [error data] :as _fsm}]
  (throw (ex-info "Forge belt error"
                  {:phase (:phase data)
                   :data data
                   :error error})))

;; =============================================================================
;; Subscription Handlers (s1-s2) — Log-only (no metrics in FOSS)
;; =============================================================================

(defn- on-smite-count-change*
  "fb/s1: Log smite count changes."
  [_path old-value new-value]
  (log/debug "smite-count" old-value "->" new-value))

(defn- on-spark-count-change*
  "fb/s2: Log spark count changes."
  [_path old-value new-value]
  (log/debug "spark-count" old-value "->" new-value))

;; =============================================================================
;; FSM Spec — State graph:
;;   ::start → ::smite → ::survey → ::spark →─┬─ ::end   (single-shot)
;;                                              ├─ ::halt  (quenched)
;;                                              ├─ ::start (continuous, loop)
;;                                              └─ ::end   (fallback)
;; =============================================================================

(def ^:private forge-belt-spec
  {:fsm
   {::fsm/start {:handler    handle-start*
                 :dispatches [[::smite (constantly true)]]}

    ::smite     {:handler    handle-smite*
                 :dispatches [[::survey (constantly true)]]}

    ::survey    {:handler    handle-survey*
                 :dispatches [[::spark (constantly true)]]}

    ::spark     {:handler    handle-spark*
                 :dispatches [[::fsm/end   single-shot?*]
                              [::fsm/halt  quenched?*]
                              [::fsm/start continuous?*]
                              [::fsm/end   (constantly true)]]}

    ::fsm/end   {:handler handle-end*}

    ::fsm/halt  {:handler handle-halt*}

    ::fsm/error {:handler handle-error*}}

   :opts
   {:max-trace 100
    :subscriptions
    {[:total-smited]  {:handler on-smite-count-change*}
     [:total-sparked] {:handler on-spark-count-change*}}}})

;; =============================================================================
;; Compilation & Execution (compile, run, strike, cont)
;; =============================================================================

(defn- compile-belt*
  "fb/compile: Build + compile the FOSS forge belt FSM spec."
  []
  (fsm/compile forge-belt-spec))

(defn- run-belt*
  "fb/run: Execute a compiled forge belt FSM with initial data."
  [compiled resources opts]
  (fsm/run compiled resources
           {:data (merge {:quenched?    false
                          :continuous?  false
                          :strike-count 0
                          :total-smited 0
                          :total-sparked 0}
                         opts)}))

(defn- run-single-strike*
  "fb/strike: Compile + run a single forge strike cycle."
  [resources]
  (run-belt* (compile-belt*) resources {:continuous? false}))

(defn- run-continuous-belt*
  "fb/cont: Compile + run continuous forge belt (loops until quenched or no tasks)."
  [resources]
  (run-belt* (compile-belt*) resources {:continuous? true}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-forge-belt-defaults!
  "Register default implementations for all :fb/* extension points.
   Called at startup before load-extensions! so extensions can override."
  []
  (ext/register-many!
   {;; Predicates (q1-q5)
    :fb/q1 quenched?*
    :fb/q2 has-tasks?*
    :fb/q3 no-tasks?*
    :fb/q4 continuous?*
    :fb/q5 single-shot?*
    ;; Handlers (h1-h7)
    :fb/h1 handle-start*
    :fb/h2 handle-smite*
    :fb/h3 handle-survey*
    :fb/h4 handle-spark*
    :fb/h5 handle-end*
    :fb/h6 handle-halt*
    :fb/h7 handle-error*
    ;; Subscriptions (s1-s2)
    :fb/s1 on-smite-count-change*
    :fb/s2 on-spark-count-change*
    ;; Compile & Run
    :fb/compile compile-belt*
    :fb/run     run-belt*
    :fb/strike  run-single-strike*
    :fb/cont    run-continuous-belt*})
  (log/info "Registered forge belt defaults (18 :fb/* extension points)"))
