(ns hive-mcp.tools.consolidated.workflow.spawn
  "Spawn engine for forge workflows: ling spawning and the spark! orchestrator.

   Key refactorings from parent workflow.clj:
   - spawn-one! unifies spawn-one-vterm! and spawn-one-headless! (DRY)
   - spark! decomposed into compute-route-batches + execute-spawn-batches
   - Drone dispatch extracted to workflow.drone namespace

   Extracted from workflow.clj to reduce cyclomatic complexity."
  (:require [hive-mcp.tools.consolidated.workflow.readiness :as ready]
            [hive-mcp.tools.consolidated.workflow.drone :as drone]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.agent.dispatch :as dispatch]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.config.core :as config]
            [hive-mcp.agent.budget-router :as budget-router]
            [hive-mcp.agent.spawn-mode-registry :as spawn-registry]
            [hive-mcp.dns.result :as result]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ── Re-exports for backward compat ──────────────────────────────────────────

(def dispatch-drone-tasks!
  "Re-export from workflow.drone for callers that expect it here."
  drone/dispatch-drone-tasks!)

;; ── Constants ───────────────────────────────────────────────────────────────

(def ^:private vterm-max-slots
  "Hard cap for vterm lings per Emacs daemon."
  (spawn-registry/slot-limit :vterm))

(def ^:private headless-modes
  "Set of spawn modes that don't require Emacs."
  spawn-registry/headless-modes)

;; ── Budget Routing ──────────────────────────────────────────────────────────

(defn- budget-route-model
  "When budget-routing config is enabled, use budget-router to pick model.
   When disabled (default), returns the requested model unchanged."
  [requested-model]
  (if (config/get-service-value :forge :budget-routing :default false)
    (let [suggestion (budget-router/suggest-model {:model requested-model})
          selected   (:model suggestion)]
      (when (:downgraded? suggestion false)
        (log/info "SPARK: budget-router downgraded model"
                  {:requested requested-model :selected selected
                   :tier (:tier suggestion) :reason (:reason suggestion)}))
      selected)
    requested-model))

;; ── Unified Spawn-One ───────────────────────────────────────────────────────

(defn- make-spawn-params
  "Build spawn handler params. Headless modes include :spawn_mode."
  [{:keys [agent-name effective-dir default-presets model route spawn-mode-kw task-id]}]
  (cond-> {:type "ling" :name agent-name :cwd effective-dir :presets default-presets}
    task-id              (assoc :kanban_task_id task-id)
    model                (assoc :model model)
    (not= :vterm route)  (assoc :spawn_mode (name spawn-mode-kw))))

(defn- parse-spawn-result
  "Extract agent-id and spawn-mode from spawn handler response."
  [spawn-result fallback-name]
  (let [spawn-text   (when (map? spawn-result) (:text spawn-result))
        spawn-parsed (when (string? spawn-text)
                       (result/rescue nil (json/read-str spawn-text :key-fn keyword)))]
    {:agent-id   (or (:agent-id spawn-parsed) fallback-name)
     :spawn-mode (when-let [sm (:spawn-mode spawn-parsed)] (keyword sm))}))

(defn- dispatch-to-ling!
  "Send task prompt to a ready ling and update kanban status."
  [{:keys [agent-id task effective-dir]}]
  (dispatch/handle-dispatch
   {:agent_id agent-id
    :prompt   (str (or (:title task) (:id task) "untitled")
                   (when-let [desc (:description task)]
                     (str "\n\n" desc))
                   "\n\nDO NOT spawn drones. Implement directly.")})
  (when-let [task-id (:id task)]
    (result/rescue nil
                   (c-kanban/handle-kanban {:command "update" :task_id task-id
                                            :new_status "inprogress"
                                            :directory effective-dir}))))

(defn- spawn-one!
  "Spawn and dispatch a single ling. Unified from spawn-one-vterm! and
   spawn-one-headless! — route determined by :route parameter."
  [{:keys [task effective-dir default-presets model route spawn-mode-kw]}]
  (let [title       (or (:title task) (:id task) "untitled")
        task-id     (:id task)
        prefix      (if (= :vterm route) "forja-vt-" "forja-hl-")
        agent-name  (str prefix (System/currentTimeMillis))
        ready-mode  (or spawn-mode-kw (if (= :vterm route) :vterm :headless))
        base-result {:agent-id agent-name :task-title title :task-id task-id
                     :spawned false :route route}
        r (result/rescue
           base-result
           (let [spawn-result  (spawn/handle-spawn
                                (make-spawn-params {:agent-name      agent-name
                                                    :effective-dir   effective-dir
                                                    :default-presets default-presets
                                                    :model           model
                                                    :route           route
                                                    :spawn-mode-kw   ready-mode
                                                    :task-id         task-id}))
                 parsed        (parse-spawn-result spawn-result agent-name)
                 agent-id      (:agent-id parsed)
                 reported-mode (or (:spawn-mode parsed) ready-mode)
                 ready         (ready/wait-for-ling-ready agent-id reported-mode)]
             (if (:ready? ready)
               (do
                 (dispatch-to-ling! {:agent-id agent-id :task task :effective-dir effective-dir})
                 (log/info (str "SPARK[" (name route) "]: spawned+dispatched")
                           {:agent agent-id :task title :model (or model "claude")})
                 (cond-> {:agent-id agent-id :task-title title :task-id task-id
                          :spawned true :route route :model (or model "claude")}
                   (not= :vterm route) (assoc :spawn-mode ready-mode)))
               (do
                 (log/warn (str "SPARK[" (name route) "]: ling not ready, skipping dispatch")
                           {:agent-id agent-id :elapsed-ms (:elapsed-ms ready) :phase (:phase ready)})
                 (assoc base-result :agent-id agent-id
                        :error (str "Readiness timeout (" (name (or (:phase ready) :unknown)) ")"))))))]
    (cond-> r
      (and (not (:spawned r)) (not (:error r)) (::result/error (meta r)))
      (assoc :error (get-in (meta r) [::result/error :message] "unknown")))))

;; ── Spark Helpers ───────────────────────────────────────────────────────────

(defn- count-project-lings
  "Count active lings for the given project, split by route type."
  [project-id]
  (let [active-status #{:active :running :working :idle :spawning}
        project-lings (->> (queries/get-all-slaves)
                           (filter #(= 1 (:slave/depth %)))
                           (filter #(active-status (:slave/status %)))
                           (filter (fn [a] (if project-id
                                             (= project-id (:slave/project-id a))
                                             true))))
        active-vterm    (count (remove #(headless-modes (:ling/spawn-mode %)) project-lings))
        active-headless (count (filter #(headless-modes (:ling/spawn-mode %)) project-lings))]
    {:active-vterm active-vterm :active-headless active-headless
     :active-total (+ active-vterm active-headless)}))

(defn- compute-route-batches
  "Allocate ling tasks to vterm/headless batches based on mode and slots.
   Returns [vterm-tasks headless-tasks]."
  [{:keys [effective-spawn-mode ling-tasks max-slots active-counts]}]
  (let [{:keys [active-vterm active-total]} active-counts]
    (case effective-spawn-mode
      :vterm
      (let [cap   (min (or max-slots vterm-max-slots) vterm-max-slots)
            avail (max 0 (- cap active-total))]
        [(vec (take avail ling-tasks)) []])
      (:headless :agent-sdk :openrouter)
      (let [avail (max 0 (- (or max-slots 10) active-total))]
        [[] (vec (take avail ling-tasks))])
      ;; :mixed — fill vterm, overflow to headless
      (let [total-cap   (or max-slots 10)
            total-avail (max 0 (- total-cap active-total))
            vt-avail    (min (max 0 (- vterm-max-slots active-vterm)) total-avail)
            vt-batch    (vec (take vt-avail ling-tasks))
            hl-avail    (max 0 (- total-avail (count vt-batch)))
            hl-batch    (vec (take hl-avail (drop vt-avail ling-tasks)))]
        [vt-batch hl-batch]))))

(defn- execute-spawn-batches
  "Spawn lings for vterm and headless task batches."
  [{:keys [vterm-tasks headless-tasks effective-dir default-presets model
           headless-spawn-mode]}]
  {:vt-results (doall (for [task vterm-tasks]
                        (spawn-one! {:task task :effective-dir effective-dir
                                     :default-presets default-presets :model model
                                     :route :vterm})))
   :hl-results (doall (for [task headless-tasks]
                        (spawn-one! {:task task :effective-dir effective-dir
                                     :default-presets default-presets :model model
                                     :route :headless :spawn-mode-kw headless-spawn-mode})))})

(defn- build-spark-response
  "Merge spawn batch results into unified spark! response."
  [{:keys [vt-results hl-results drone-result active-counts max-slots]}]
  (let [all-ling    (concat vt-results hl-results)
        dr-spawned  (get drone-result :spawned [])
        all-results (concat all-ling dr-spawned)]
    {:spawned       (filterv :spawned all-results)
     :failed        (into (filterv (complement :spawned) all-ling) (get drone-result :failed []))
     :count         (+ (count (filter :spawned all-ling)) (get drone-result :count 0))
     :routes        {:vterm    {:spawned (filterv :spawned vt-results)
                                :count (count (filter :spawned vt-results))
                                :active-before (:active-vterm active-counts)
                                :max-slots vterm-max-slots}
                     :headless {:spawned (filterv :spawned hl-results)
                                :count (count (filter :spawned hl-results))
                                :active-before (:active-headless active-counts)
                                :max-slots (or max-slots 10)}
                     :drone    {:count (get drone-result :count 0)
                                :wave-id (:wave-id drone-result)
                                :spawned dr-spawned}}
     :slots-used    (count (filter :spawned all-ling))
     :active-before (:active-total active-counts)
     :max-slots     (or max-slots 10)}))

(defn- empty-spark-response
  "Empty spark response when no tasks to spawn."
  [active-counts max-slots]
  {:spawned [] :failed [] :count 0
   :routes {:vterm {:count 0 :active-before (:active-vterm active-counts)
                    :max-slots vterm-max-slots}
            :headless {:count 0 :active-before (:active-headless active-counts)
                       :max-slots (or max-slots 10)}
            :drone {:count 0}}
   :slots-used 0
   :active-before (:active-total active-counts)
   :max-slots (or max-slots 10)})

;; ── Spark! Orchestrator ─────────────────────────────────────────────────────

(defn spark!
  "Spawn lings or dispatch drones for ready tasks.
   Routes: :drone (wave all), :vterm, :headless/:agent-sdk/:openrouter,
   :mixed (default, classify + split)."
  [{:keys [directory max_slots presets tasks spawn_mode spawn-mode model
           preset seeds ctx_refs kg_node_ids]}]
  (let [model                (budget-route-model model)
        effective-spawn-mode (keyword (or spawn_mode spawn-mode :mixed))
        drone-params         {:directory directory :tasks tasks :model model
                              :preset (or preset (first presets) "drone-worker")
                              :seeds seeds :ctx_refs ctx_refs :kg_node_ids kg_node_ids
                              :max_slots max_slots}]
    (if (= :drone effective-spawn-mode)
      (drone/dispatch-drone-tasks! drone-params)
      (let [{:keys [drone-tasks ling-tasks]}
            (if (= :mixed effective-spawn-mode)
              (drone/classify-and-split-tasks tasks)
              {:drone-tasks [] :ling-tasks tasks})

            drone-result    (when (seq drone-tasks)
                              (drone/dispatch-drone-tasks! (assoc drone-params :tasks drone-tasks)))
            effective-dir   (or directory (ctx/current-directory) (System/getProperty "user.dir"))
            project-id      (when effective-dir (scope/get-current-project-id effective-dir))
            active-counts   (count-project-lings project-id)
            default-presets (or presets ["ling" "mcp-first" "saa"])

            [vterm-tasks headless-tasks]
            (compute-route-batches {:effective-spawn-mode effective-spawn-mode
                                    :ling-tasks ling-tasks :max-slots max_slots
                                    :active-counts active-counts})

            headless-spawn-mode (if (headless-modes effective-spawn-mode)
                                  effective-spawn-mode :headless)]
        (if (and (empty? vterm-tasks) (empty? headless-tasks) (nil? drone-result))
          (empty-spark-response active-counts max_slots)
          (let [batch-results (execute-spawn-batches
                               {:vterm-tasks vterm-tasks :headless-tasks headless-tasks
                                :effective-dir effective-dir :default-presets default-presets
                                :model model :headless-spawn-mode headless-spawn-mode})]
            (build-spark-response (assoc batch-results
                                         :drone-result drone-result
                                         :active-counts active-counts
                                         :max-slots max_slots))))))))
