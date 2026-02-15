(ns hive-mcp.tools.consolidated.workflow.forge-cycle
  "Forge cycle orchestration: FSM resource building and strike execution.

   Handles the forge-strike lifecycle (smite→survey→spark) in both
   FSM and legacy (imperative) modes. build-fsm-resources creates the
   closures needed by forge-belt/run-single-strike.

   Extracted from forge-ops to reduce cyclomatic complexity."
  (:require [hive-mcp.tools.consolidated.workflow.forge-ops :as forge-ops]
            [hive-mcp.tools.consolidated.workflow.spawn :as spawn]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.workflows.forge-belt :as forge-belt]
            [hive-mcp.dns.result :as result]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ── FSM Resource Builder Helpers ──────────────────────────────────────────────

(defn- merge-spawn-opts
  "Merge default config values into spawn opts map. Avoids repeated cond-> chains."
  [opts defaults]
  (reduce-kv (fn [m k v]
               (if (and v (not (get m k)))
                 (assoc m k v)
                 m))
             opts defaults))

(defn- make-agent-ops
  "Build the agent-ops map for forge-belt FSM resources."
  [{:keys [spawn-mode model preset seeds ctx-refs kg-node-ids]}]
  (let [defaults {:spawn-mode spawn-mode :model model :preset preset
                  :seeds seeds :ctx-refs ctx-refs :kg-node-ids kg-node-ids}]
    {:kill-fn  (fn [dir _project-id]
                 (forge-ops/smite! {:directory dir}))
     :spawn-fn (fn [opts]
                 (spawn/spark!
                  (-> opts
                      (merge-spawn-opts {:spawn-mode (:spawn-mode defaults)
                                         :model      (:model defaults)
                                         :preset     (:preset defaults)
                                         :seeds      (:seeds defaults)})
                      (cond->
                        (or (:ctx-refs opts) (:ctx-refs defaults))
                        (assoc :ctx_refs (or (:ctx-refs opts) (:ctx-refs defaults)))
                        (or (:kg-node-ids opts) (:kg-node-ids defaults))
                        (assoc :kg_node_ids (or (:kg-node-ids opts) (:kg-node-ids defaults)))))))
     :drone-dispatch-fn
     (fn [opts]
       (spawn/dispatch-drone-tasks!
        (merge-spawn-opts opts {:preset preset :model model :seeds seeds
                                :ctx_refs ctx-refs :kg_node_ids kg-node-ids})))
     :dispatch-fn   (fn [_agent-id _task] true)
     :wait-ready-fn (fn [_agent-id] true)}))

(defn- make-kanban-ops
  "Build the kanban-ops map for forge-belt FSM resources."
  [survey-opts]
  {:list-fn   (fn [dir]
                (forge-ops/survey (merge survey-opts {:directory dir})))
   :update-fn (fn [_opts] nil)})

;; ── FSM Resource Builder ──────────────────────────────────────────────────────

(defn build-fsm-resources
  "Build the resources map for the Forge Belt FSM.
   Extra keys in params flow through to survey via kanban-ops/list-fn (OCP)."
  [{:keys [directory max_slots presets spawn_mode model
           preset seeds ctx_refs kg_node_ids] :as params}]
  (let [effective-spawn-mode (when spawn_mode (keyword spawn_mode))
        survey-opts          (dissoc params :directory :max_slots :presets :spawn_mode :model
                                     :preset :seeds :ctx_refs :kg_node_ids)]
    {:directory directory
     :config    {:max-slots   (or max_slots 10)
                 :presets     (or presets ["ling" "mcp-first" "saa"])
                 :spawn-mode  effective-spawn-mode
                 :model       model
                 :preset      preset
                 :seeds       seeds
                 :ctx-refs    ctx_refs
                 :kg-node-ids kg_node_ids}
     :agent-ops (make-agent-ops {:spawn-mode effective-spawn-mode :model model
                                 :preset preset :seeds seeds
                                 :ctx-refs ctx_refs :kg-node-ids kg_node_ids})
     :kanban-ops (make-kanban-ops survey-opts)
     :scope-fn   (fn [dir]
                   (when dir (scope/get-current-project-id dir)))
     :clock-fn   #(java.time.Instant/now)}))

;; ── Forge Strike: Legacy (Imperative) ─────────────────────────────────────────

(defn- update-forge-state-after-strike!
  "Swap forge-state atom after a strike cycle completes."
  [forge-state smite-count spark-count]
  (swap! forge-state
         (fn [s]
           (-> s
               (update :total-smited + smite-count)
               (update :total-sparked + spark-count)
               (update :total-strikes inc)
               (assoc :last-strike (str (java.time.Instant/now)))))))

(defn forge-strike-legacy*
  "Execute a legacy forge cycle (imperative smite->survey->spark).
   Returns Result. Accepts forge-state atom for state updates."
  [{:keys [directory max_slots presets spawn_mode model task_ids task_filter] :as params}
   forge-state]
  (log/info "FORGE STRIKE (legacy): Starting cycle" {:directory  directory
                                                     :max-slots  max_slots
                                                     :spawn-mode (or spawn_mode "vterm")
                                                     :model      (or model "claude")})
  (let [smite-result  (forge-ops/smite! params)
        _             (log/info "FORGE STRIKE: SMITE complete" {:killed (:count smite-result)})
        survey-result (forge-ops/survey (cond-> {:directory directory}
                                          (seq task_ids)       (assoc :task_ids task_ids)
                                          (some? task_filter)  (assoc :task_filter task_filter)))
        _             (log/info "FORGE STRIKE: SURVEY complete" {:tasks (:count survey-result)})
        spark-result  (spawn/spark! (assoc params
                                          :tasks     (:tasks survey-result)
                                          :max_slots max_slots
                                          :presets   presets
                                          :model     model))
        _             (log/info "FORGE STRIKE: SPARK complete" {:spawned (:count spark-result)})]
    (update-forge-state-after-strike! forge-state (:count smite-result) (:count spark-result))
    (result/ok {:success    true
                :mode       :imperative
                :deprecated true
                :spawn-mode (or spawn_mode "vterm")
                :model      (or model "claude")
                :smite      smite-result
                :survey     {:todo-count   (:count survey-result)
                             :task-titles  (mapv :title (:tasks survey-result))}
                :spark      spark-result
                :summary    (str "DEPRECATED legacy: Smited " (:count smite-result)
                                 ", surveyed " (:count survey-result) " tasks"
                                 ", sparked " (:count spark-result) " lings"
                                 " (mode: " (or spawn_mode "vterm")
                                 ", model: " (or model "claude") ")")})))

;; ── Forge Strike: FSM ─────────────────────────────────────────────────────────

(defn fsm-forge-strike*
  "Execute an FSM forge strike cycle. Returns Result.
   Accepts forge-state atom for state updates."
  [{:keys [directory max_slots spawn_mode model] :as params} forge-state]
  (log/info "FORGE STRIKE: Starting FSM cycle" {:directory  directory
                                                :max-slots  max_slots
                                                :spawn-mode (or spawn_mode "vterm")
                                                :model      (or model "claude")})
  (let [resources  (build-fsm-resources params)
        fsm-result (forge-belt/run-single-strike resources)
        outcome    (or (:outcome fsm-result) :clean)]
    (swap! forge-state
           (fn [s]
             (-> s
                 (update :total-smited + (:total-smited fsm-result 0))
                 (update :total-sparked + (:total-sparked fsm-result 0))
                 (update :total-strikes inc)
                 (assoc :last-strike (:last-strike fsm-result)))))
    (result/ok {:success    (:success fsm-result true)
                :outcome    outcome
                :mode       :fsm
                :spawn-mode (or spawn_mode "vterm")
                :model      (or model "claude")
                :smite      (:smite-result fsm-result)
                :survey     {:todo-count  (get-in fsm-result [:survey-result :count] 0)
                             :task-titles (mapv :title (get-in fsm-result [:survey-result :tasks] []))}
                :spark      (:spark-result fsm-result)
                :summary    (str (when (= :partial outcome) "PARTIAL: ")
                                 (when (= :failure outcome) "FAILED: ")
                                 "Smited " (:total-smited fsm-result 0)
                                 ", surveyed " (get-in fsm-result [:survey-result :count] 0) " tasks"
                                 ", sparked " (:total-sparked fsm-result 0) " lings"
                                 " (mode: " (or spawn_mode "vterm")
                                 ", model: " (or model "claude") ")")})))
