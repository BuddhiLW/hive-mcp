(ns hive-mcp.tools.consolidated.workflow
  "Consolidated Workflow CLI tool for Forja Belt automation.

   Includes defense-in-depth guard: child lings (spawned agents) are
   denied from executing forge-strike to prevent recursive self-call
   chains."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.consolidated.session :as c-session]
            [hive-mcp.tools.consolidated.agent :as c-agent]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.agent.dispatch :as dispatch]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.config.core :as config]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.sdk.session :as sdk-session]
            [hive-mcp.agent.sdk.python :as sdk-py]
            [hive-mcp.workflows.forge-belt :as forge-belt]
            [hive-mcp.scheduler.vulcan :as vulcan]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.agent.budget-router :as budget-router]
            [hive-mcp.agent.task-classifier :as task-classifier]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.swarm.wave.execution :as wave-execution]
            [hive-mcp.server.guards :as guards]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private forge-state
  (atom {:quenched? false
         :last-strike nil
         :last-fsm-result nil
         :total-smited 0
         :total-sparked 0
         :total-strikes 0}))

(defn- smite!
  "Kill completed, idle-done, or error lings in the current project."
  [{:keys [directory]}]
  (let [all-agents (queries/get-all-slaves)
        project-id (when directory (scope/get-current-project-id directory))
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

(defn- parse-kanban-tasks
  "Parse kanban list handler response into a vector of task maps."
  [result]
  (let [text (if (map? result) (:text result) result)
        parsed (when (string? text)
                 (try (json/read-str text :key-fn keyword)
                      (catch Exception _ nil)))]
    (cond
      (sequential? parsed) parsed
      (map? parsed)        (or (:tasks parsed) [])
      :else                [])))

(defn- sort-by-priority-then-created
  "Sort tasks by priority DESC then creation-date ASC."
  [tasks]
  (let [priority-order {"high" 0 "priority-high" 0
                        "medium" 1 "priority-medium" 1
                        "low" 2 "priority-low" 2}]
    (vec (sort (fn [a b]
                 (let [pa (get priority-order (or (:priority a) "medium") 1)
                       pb (get priority-order (or (:priority b) "medium") 1)]
                   (if (= pa pb)
                     (compare (str (:id a)) (str (:id b)))
                     (compare pa pb))))
               tasks))))

(defn- survey
  "Query kanban for todo tasks, ranked and optionally filtered by KG dependencies.
   Extra keys in opts are passed through to vulcan/prioritize-tasks (OCP)."
  [{:keys [directory vulcan-mode] :as opts}]
  (try
    (let [result (c-kanban/handle-kanban {:command "list" :status "todo" :directory directory})
          tasks (parse-kanban-tasks result)]
      (if vulcan-mode
        (let [prioritized (vulcan/prioritize-tasks tasks #{} (dissoc opts :directory :vulcan-mode))]
          (log/info "SURVEY (vulcan): ready" (:count prioritized)
                    "blocked" (:blocked-count prioritized)
                    "of" (count tasks) "total")
          prioritized)
        (let [sorted-tasks (sort-by-priority-then-created tasks)]
          {:tasks sorted-tasks
           :count (count sorted-tasks)})))
    (catch Exception e
      (log/warn "SURVEY failed" {:error (ex-message e)})
      {:tasks [] :count 0 :error (ex-message e)})))

(def ^:private ling-ready-timeout-ms 5000)

(def ^:private ling-ready-poll-ms 50)

(defn- vterm-ready?
  "Check if a vterm ling's CLI is ready for input."
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
  "Check if a headless ling's process is alive and has produced stdout."
  [agent-id]
  (try
    (when-let [status (headless/headless-status agent-id)]
      (and (:alive? status)
           (pos? (get-in status [:stdout :total-lines-seen] 0))))
    (catch Exception _
      false)))

(defn- agent-sdk-ready?
  "Check if an agent-sdk ling's session is idle and event loop thread is alive."
  [agent-id]
  (try
    (when-let [sess (sdk-session/get-session agent-id)]
      (and (= :idle (:phase sess))
           (some? (:client-ref sess))
           (let [safe-id (:py-safe-id sess)]
             (if safe-id
               (let [thread-obj (sdk-py/py-get-global
                                 (str "_hive_loop_thread_" safe-id))]
                 (boolean (and thread-obj
                               (sdk-py/py-call thread-obj "is_alive"))))
               false))))
    (catch Exception e
      (log/debug "agent-sdk-ready? check failed"
                 {:agent-id agent-id :error (ex-message e)})
      false)))

(defn- ling-cli-ready?
  "Mode-dispatch readiness check for a ling's CLI."
  [agent-id spawn-mode]
  (case spawn-mode
    :headless   (headless-ready? agent-id)
    :openrouter true
    :agent-sdk  (agent-sdk-ready? agent-id)
    ;; default: vterm
    (vterm-ready? agent-id)))

(defn- wait-for-ling-ready
  "Poll for ling readiness before dispatching (two-phase: DataScript + CLI)."
  [agent-id spawn-mode]
  (let [start-ms (System/currentTimeMillis)]
    (loop [attempt 1]
      (let [slave (queries/get-slave agent-id)
            cli-ok? (when slave (ling-cli-ready? agent-id spawn-mode))
            elapsed (- (System/currentTimeMillis) start-ms)]
        (cond
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

          :else
          (do
            (Thread/sleep ling-ready-poll-ms)
            (recur (inc attempt))))))))

(def ^:private vterm-max-slots
  "Hard cap for vterm lings per Emacs daemon. 7+ crashes Emacs."
  6)

(defn- spawn-one-vterm!
  "Spawn and dispatch a single ling via vterm route (Emacs-bound).
   No spawn_mode param — defaults to vterm in spawn handler."
  [{:keys [task effective-dir default-presets model]}]
  (let [title   (or (:title task) (:id task) "untitled")
        task-id (:id task)
        agent-name (str "forja-vt-" (System/currentTimeMillis))]
    (try
      (let [spawn-result (spawn/handle-spawn
                          (cond-> {:type "ling"
                                   :name agent-name
                                   :cwd effective-dir
                                   :presets default-presets
                                   :kanban_task_id task-id}
                            model (assoc :model model)))
            spawn-text   (when (map? spawn-result) (:text spawn-result))
            spawn-parsed (when (string? spawn-text)
                           (try (json/read-str spawn-text :key-fn keyword)
                                (catch Exception _ nil)))
            agent-id (or (:agent-id spawn-parsed) agent-name)]
        (let [ready (wait-for-ling-ready agent-id :vterm)]
          (if (:ready? ready)
            (do
              (dispatch/handle-dispatch
               {:agent_id agent-id
                :prompt (str title
                             (when-let [desc (:description task)]
                               (str "\n\n" desc))
                             "\n\nDO NOT spawn drones. Implement directly.")})
              (when task-id
                (try (c-kanban/handle-kanban {:command "update" :task_id task-id
                                              :new_status "inprogress"
                                              :directory effective-dir})
                     (catch Exception _ nil)))
              (log/info "SPARK[vterm]: spawned+dispatched" {:agent agent-id :task title})
              {:agent-id agent-id :task-title title :task-id task-id :spawned true
               :route :vterm :model (or model "claude")})
            (do
              (log/warn "SPARK[vterm]: ling not ready, skipping dispatch"
                        {:agent-id agent-id :elapsed-ms (:elapsed-ms ready)
                         :phase (:phase ready)})
              {:agent-id agent-id :task-title title :task-id task-id
               :spawned false
               :error (str "Readiness timeout (" (name (or (:phase ready) :unknown)) ")")
               :route :vterm}))))
      (catch Exception e
        (log/warn "SPARK[vterm]: failed" {:task title :error (ex-message e)})
        {:agent-id agent-name :task-title title :task-id task-id
         :spawned false :error (ex-message e) :route :vterm}))))

(defn- spawn-one-headless!
  "Spawn and dispatch a single ling via headless route (Emacs-independent).
   Explicitly sets spawn_mode so spawn handler picks the right backend."
  [{:keys [task effective-dir default-presets effective-spawn-mode model]}]
  (let [title   (or (:title task) (:id task) "untitled")
        task-id (:id task)
        agent-name (str "forja-hl-" (System/currentTimeMillis))]
    (try
      (let [spawn-result (spawn/handle-spawn
                          (cond-> {:type "ling"
                                   :name agent-name
                                   :cwd effective-dir
                                   :presets default-presets
                                   :kanban_task_id task-id
                                   :spawn_mode (name effective-spawn-mode)}
                            model (assoc :model model)))
            spawn-text   (when (map? spawn-result) (:text spawn-result))
            spawn-parsed (when (string? spawn-text)
                           (try (json/read-str spawn-text :key-fn keyword)
                                (catch Exception _ nil)))
            agent-id      (or (:agent-id spawn-parsed) agent-name)
            reported-mode (keyword (or (:spawn-mode spawn-parsed)
                                       (name effective-spawn-mode)))]
        (let [ready (wait-for-ling-ready agent-id reported-mode)]
          (if (:ready? ready)
            (do
              (dispatch/handle-dispatch
               {:agent_id agent-id
                :prompt (str title
                             (when-let [desc (:description task)]
                               (str "\n\n" desc))
                             "\n\nDO NOT spawn drones. Implement directly.")})
              (when task-id
                (try (c-kanban/handle-kanban {:command "update" :task_id task-id
                                              :new_status "inprogress"
                                              :directory effective-dir})
                     (catch Exception _ nil)))
              (log/info "SPARK[headless]: spawned+dispatched"
                        {:agent agent-id :task title :model (or model "claude")
                         :spawn-mode effective-spawn-mode})
              {:agent-id agent-id :task-title title :task-id task-id :spawned true
               :route :headless :spawn-mode effective-spawn-mode
               :model (or model "claude")})
            (do
              (log/warn "SPARK[headless]: ling not ready, skipping dispatch"
                        {:agent-id agent-id :elapsed-ms (:elapsed-ms ready)
                         :phase (:phase ready) :spawn-mode reported-mode})
              {:agent-id agent-id :task-title title :task-id task-id
               :spawned false
               :error (str "Readiness timeout (" (name (or (:phase ready) :unknown)) ")")
               :route :headless :spawn-mode effective-spawn-mode}))))
      (catch Exception e
        (log/warn "SPARK[headless]: failed" {:task title :error (ex-message e)})
        {:agent-id agent-name :task-title title :task-id task-id
         :spawned false :error (ex-message e)
         :route :headless :spawn-mode effective-spawn-mode}))))

(defn- budget-route-model
  "When :forge.budget-routing config gate is enabled, use budget-router
   to select a tier-appropriate model based on fleet spend.
   When disabled (default), returns the requested model unchanged."
  [requested-model]
  (if (config/get-service-value :forge :budget-routing :default false)
    (let [suggestion (budget-router/suggest-model {:model requested-model})
          selected   (:model suggestion)]
      (when (:downgraded? suggestion false)
        (log/info "SPARK: budget-router downgraded model"
                  {:requested requested-model
                   :selected  selected
                   :tier      (:tier suggestion)
                   :reason    (:reason suggestion)}))
      (log/debug "SPARK: budget-routed" {:model selected :tier (:tier suggestion)})
      selected)
    requested-model))

(defn- kanban-task->wave-tasks
  "Convert a kanban task to wave task spec(s).
   Returns empty vec if task lacks :file or :files reference."
  [{:keys [title description file files]}]
  (let [target-files (or (when file [file])
                         (seq files)
                         [])
        task-text (str (or title "untitled")
                       (when description (str "\n\n" description)))]
    (mapv (fn [f] {:file f :task task-text}) target-files)))

(defn- dispatch-drone-tasks!
  "Dispatch kanban tasks as a drone wave. Tasks without :file are skipped.
   Returns spark-compatible result with wave metadata."
  [{:keys [directory tasks model preset seeds ctx_refs kg_node_ids max_slots]}]
  (let [effective-dir (or directory
                          (ctx/current-directory)
                          (System/getProperty "user.dir"))
        dispatchable  (filterv #(seq (kanban-task->wave-tasks %)) tasks)
        skipped-tasks (filterv #(empty? (kanban-task->wave-tasks %)) tasks)
        wave-tasks    (->> dispatchable
                           (mapcat kanban-task->wave-tasks)
                           (take (or max_slots 20))
                           vec)]
    (if (empty? wave-tasks)
      {:spawned [] :failed [] :count 0 :route :drone
       :skipped (count skipped-tasks)
       :reason "No tasks with :file reference for drone dispatch"}
      (try
        (let [plan-id (ds/create-plan! wave-tasks (or preset "drone-worker"))
              {:keys [wave-id item-count]}
              (wave-execution/execute-wave-async!
               plan-id
               (cond-> {:cwd effective-dir :trace true}
                 model              (assoc :model model)
                 (seq seeds)        (assoc :seeds (vec seeds))
                 (seq ctx_refs)     (assoc :ctx-refs ctx_refs)
                 (seq kg_node_ids)  (assoc :kg-node-ids (vec kg_node_ids))))]
          ;; Mark dispatched kanban tasks as inprogress
          (doseq [task dispatchable
                  :let [task-id (:id task)]
                  :when task-id]
            (try (c-kanban/handle-kanban {:command "update" :task_id task-id
                                          :new_status "inprogress"
                                          :directory effective-dir})
                 (catch Exception _ nil)))
          (log/info "SPARK[drone]: wave dispatched"
                    {:wave-id wave-id :plan-id plan-id
                     :count item-count :skipped (count skipped-tasks)
                     :model (or model "default")})
          {:spawned (mapv (fn [t] {:task-title (:title t) :task-id (:id t)
                                   :spawned true :route :drone})
                          dispatchable)
           :failed []
           :count item-count
           :route :drone
           :wave-id wave-id
           :plan-id plan-id
           :skipped (count skipped-tasks)
           :model (or model "default")})
        (catch Exception e
          (log/warn "SPARK[drone]: wave dispatch failed" {:error (ex-message e)})
          {:spawned [] :failed [{:error (ex-message e)}]
           :count 0 :route :drone :skipped (count skipped-tasks)})))))

(defn- classify-and-split-tasks
  "When budget routing is enabled, classify each task via task-classifier
   and split into drone-eligible and ling-eligible batches.
   When disabled, all tasks route to :ling (safe default)."
  [tasks]
  (if-not (budget-router/enabled?)
    {:drone-tasks [] :ling-tasks (vec tasks)}
    (let [classified (mapv (fn [task]
                             (let [route (:route (task-classifier/classify-task
                                                  {:title       (or (:title task) "")
                                                   :description (:description task)
                                                   :files       (or (:files task)
                                                                    (when-let [f (:file task)] [f]))}))]
                               (assoc task ::route (or route :ling))))
                           tasks)]
      (log/debug "SPARK: task classification"
                 {:total (count tasks)
                  :drone (count (filter #(= :drone (::route %)) classified))
                  :ling  (count (filter #(not= :drone (::route %)) classified))})
      {:drone-tasks (filterv #(= :drone (::route %)) classified)
       :ling-tasks  (filterv #(not= :drone (::route %)) classified)})))

(defn- spark!
  "Spawn lings or dispatch drones for ready tasks. Bifurcates by route:
   - :vterm — Emacs-bound, capped at vterm-max-slots (daemon stability)
   - :headless/:agent-sdk/:openrouter — subprocess, scales freely
   - :mixed (default) — when budget-routing enabled, classify each task
     via task-classifier and split drone-eligible tasks to wave dispatch,
     ling-eligible tasks to vterm+headless overflow
   - :drone — wave dispatch all tasks as drone wave"
  [{:keys [directory max_slots presets tasks spawn_mode spawn-mode model
           preset seeds ctx_refs kg_node_ids]}]
  (let [model          (budget-route-model model)
        effective-spawn-mode (keyword (or spawn_mode spawn-mode :mixed))]
    (if (= :drone effective-spawn-mode)
      ;; Explicit :drone — all tasks go to wave dispatch
      (dispatch-drone-tasks! {:directory  directory
                              :tasks      tasks
                              :model      model
                              :preset     (or preset (first presets) "drone-worker")
                              :seeds      seeds
                              :ctx_refs   ctx_refs
                              :kg_node_ids kg_node_ids
                              :max_slots  max_slots})
      ;; Ling paths with optional drone bifurcation
      (let [;; When :mixed, classify tasks and split by route
            {:keys [drone-tasks ling-tasks]}
            (if (= :mixed effective-spawn-mode)
              (classify-and-split-tasks tasks)
              {:drone-tasks [] :ling-tasks tasks})

            ;; Dispatch drone-eligible tasks first (if any)
            drone-result (when (seq drone-tasks)
                           (dispatch-drone-tasks!
                            {:directory   directory
                             :tasks       drone-tasks
                             :model       model
                             :preset      (or preset (first presets) "drone-worker")
                             :seeds       seeds
                             :ctx_refs    ctx_refs
                             :kg_node_ids kg_node_ids
                             :max_slots   max_slots}))

            headless-modes #{:headless :agent-sdk :openrouter}
            effective-dir  (or directory
                               (ctx/current-directory)
                               (System/getProperty "user.dir"))
            project-id     (when effective-dir (scope/get-current-project-id effective-dir))
            active-agents  (queries/get-all-slaves)
            project-lings  (->> active-agents
                                (filter #(= 1 (:slave/depth %)))
                                (filter #(#{:active :running :working :idle :spawning} (:slave/status %)))
                                (filter (fn [agent]
                                          (if project-id
                                            (= project-id (:slave/project-id agent))
                                            true))))
            ;; Partition active lings by route for per-route slot accounting
            active-vterm    (->> project-lings
                                 (remove #(headless-modes (:ling/spawn-mode %)))
                                 count)
            active-headless (->> project-lings
                                 (filter #(headless-modes (:ling/spawn-mode %)))
                                 count)
            active-total    (+ active-vterm active-headless)
            default-presets (or presets ["ling" "mcp-first" "saa"])

            ;; Route bifurcation — compute per-route ling task batches
            ;; max_slots is always a GLOBAL cap (total active across all routes)
            [vterm-tasks headless-tasks]
            (case effective-spawn-mode
              :vterm
              (let [cap   (min (or max_slots vterm-max-slots) vterm-max-slots)
                    avail (max 0 (- cap active-total))]
                [(vec (take avail ling-tasks)) []])

              (:headless :agent-sdk :openrouter)
              (let [avail (max 0 (- (or max_slots 10) active-total))]
                [[] (vec (take avail ling-tasks))])

              ;; :mixed (default) — fill vterm up to daemon cap, overflow to headless
              (let [total-cap   (or max_slots 10)
                    total-avail (max 0 (- total-cap active-total))
                    vt-avail    (min (max 0 (- vterm-max-slots active-vterm)) total-avail)
                    vt-batch    (vec (take vt-avail ling-tasks))
                    remaining   (drop vt-avail ling-tasks)
                    hl-avail    (max 0 (- total-avail (count vt-batch)))
                    hl-batch    (vec (take hl-avail remaining))]
                [vt-batch hl-batch]))

            headless-spawn-mode (if (headless-modes effective-spawn-mode)
                                  effective-spawn-mode
                                  :headless)]
        (if (and (empty? vterm-tasks) (empty? headless-tasks) (nil? drone-result))
          {:spawned [] :failed [] :count 0
           :routes {:vterm    {:count 0 :active-before active-vterm    :max-slots vterm-max-slots}
                    :headless {:count 0 :active-before active-headless :max-slots (or max_slots 10)}
                    :drone    {:count 0}}
           :slots-used 0 :active-before active-total :max-slots (or max_slots 10)}
          (let [vt-results
                (doall
                 (for [task vterm-tasks]
                   (spawn-one-vterm! {:task            task
                                      :effective-dir   effective-dir
                                      :default-presets default-presets
                                      :model           model})))
                hl-results
                (doall
                 (for [task headless-tasks]
                   (spawn-one-headless! {:task                  task
                                         :effective-dir         effective-dir
                                         :default-presets       default-presets
                                         :effective-spawn-mode  headless-spawn-mode
                                         :model                 model})))
                all-ling-results (concat vt-results hl-results)
                ;; Merge drone results into unified response
                drone-spawned  (get drone-result :spawned [])
                drone-failed   (get drone-result :failed [])
                all-results    (concat all-ling-results drone-spawned)]
            {:spawned     (filterv :spawned all-results)
             :failed      (into (filterv (complement :spawned) all-ling-results)
                                drone-failed)
             :count       (+ (count (filter :spawned all-ling-results))
                             (get drone-result :count 0))
             :routes      {:vterm    {:spawned (filterv :spawned vt-results)
                                      :count   (count (filter :spawned vt-results))
                                      :active-before active-vterm
                                      :max-slots     vterm-max-slots}
                           :headless {:spawned (filterv :spawned hl-results)
                                      :count   (count (filter :spawned hl-results))
                                      :active-before active-headless
                                      :max-slots     (or max_slots 10)}
                           :drone    {:count   (get drone-result :count 0)
                                      :wave-id (:wave-id drone-result)
                                      :spawned drone-spawned}}
             :slots-used  (count (filter :spawned all-ling-results))
             :active-before active-total
             :max-slots   (or max_slots 10)}))))))

(defn build-fsm-resources
  "Build the resources map for the Forge Belt FSM.
   Extra keys in params flow through to survey via kanban-ops/list-fn (OCP)."
  [{:keys [directory max_slots presets spawn_mode model vulcan_mode
           preset seeds ctx_refs kg_node_ids] :as params}]
  (let [effective-spawn-mode (when spawn_mode (keyword spawn_mode))
        effective-vulcan? (boolean vulcan_mode)
        survey-opts (dissoc params :directory :max_slots :presets :spawn_mode :model :vulcan_mode
                            :preset :seeds :ctx_refs :kg_node_ids)]
    {:directory  directory
     :config     {:max-slots    (or max_slots 10)
                  :presets      (or presets ["ling" "mcp-first" "saa"])
                  :spawn-mode   effective-spawn-mode
                  :model        model
                  :preset       preset
                  :seeds        seeds
                  :ctx-refs     ctx_refs
                  :kg-node-ids  kg_node_ids
                  :vulcan-mode  effective-vulcan?}
     :agent-ops  {:kill-fn  (fn [dir _project-id]
                              (smite! {:directory dir}))
                  :spawn-fn (fn [{:keys [directory max-slots presets tasks] :as opts}]
                              (spark! (cond-> {:directory directory
                                               :max_slots max-slots
                                               :presets presets
                                               :tasks tasks}
                                        (or (:spawn-mode opts) effective-spawn-mode)
                                        (assoc :spawn-mode (or (:spawn-mode opts) effective-spawn-mode))
                                        (or (:model opts) model)
                                        (assoc :model (or (:model opts) model))
                                        ;; drone config passthrough
                                        (or (:preset opts) preset)
                                        (assoc :preset (or (:preset opts) preset))
                                        (or (:seeds opts) seeds)
                                        (assoc :seeds (or (:seeds opts) seeds))
                                        (or (:ctx-refs opts) ctx_refs)
                                        (assoc :ctx_refs (or (:ctx-refs opts) ctx_refs))
                                        (or (:kg-node-ids opts) kg_node_ids)
                                        (assoc :kg_node_ids (or (:kg-node-ids opts) kg_node_ids)))))
                  :drone-dispatch-fn
                  (fn [opts]
                    (dispatch-drone-tasks!
                     (cond-> opts
                       (and (not (:preset opts)) preset)
                       (assoc :preset preset)
                       (and (not (:model opts)) model)
                       (assoc :model model)
                       (and (not (:seeds opts)) seeds)
                       (assoc :seeds seeds)
                       (and (not (:ctx_refs opts)) ctx_refs)
                       (assoc :ctx_refs ctx_refs)
                       (and (not (:kg_node_ids opts)) kg_node_ids)
                       (assoc :kg_node_ids kg_node_ids))))
                  :dispatch-fn    (fn [_agent-id _task] true)
                  :wait-ready-fn  (fn [_agent-id] true)}
     :kanban-ops {:list-fn   (fn [dir]
                               (survey (merge survey-opts
                                              {:directory dir
                                               :vulcan-mode effective-vulcan?})))
                  :update-fn (fn [_opts] nil)}
     :scope-fn   (fn [dir]
                   (when dir (scope/get-current-project-id dir)))
     :clock-fn   #(java.time.Instant/now)}))

(defn handle-forge-strike-legacy
  "DEPRECATED: Execute ONE forge cycle via imperative smite!/survey/spark!."
  [{:keys [directory max_slots presets spawn_mode model vulcan_mode] :as params}]
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

      (let [smite-result (smite! params)
            _ (log/info "FORGE STRIKE: SMITE complete" {:killed (:count smite-result)})

            survey-result (survey {:directory directory
                                   :vulcan-mode (boolean vulcan_mode)})
            _ (log/info "FORGE STRIKE: SURVEY complete" {:tasks (:count survey-result)})

            spark-result (spark! (assoc params
                                        :tasks (:tasks survey-result)
                                        :max_slots max_slots
                                        :presets presets
                                        :model model))
            _ (log/info "FORGE STRIKE: SPARK complete" {:spawned (:count spark-result)})]

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

(defn- fsm-forge-strike
  "FSM-driven forge strike implementation.
   Extra keys in params flow through to survey (OCP)."
  [{:keys [directory max_slots spawn_mode model] :as params}]
  (try
    (log/info "FORGE STRIKE: Starting FSM cycle" {:directory directory :max-slots max_slots
                                                  :spawn-mode (or spawn_mode "vterm")
                                                  :model (or model "claude")})
    (let [resources (build-fsm-resources params)
          result (forge-belt/run-single-strike resources)]
      (swap! forge-state
             (fn [s]
               (-> s
                   (update :total-smited + (:total-smited result 0))
                   (update :total-sparked + (:total-sparked result 0))
                   (update :total-strikes inc)
                   (assoc :last-strike (:last-strike result)))))
      (let [outcome (or (:outcome result) :clean)]
        (mcp-json {:success (:success result true)
                   :outcome outcome
                   :mode :fsm
                   :spawn-mode (or spawn_mode "vterm")
                   :model (or model "claude")
                   :smite (:smite-result result)
                   :survey {:todo-count (get-in result [:survey-result :count] 0)
                            :task-titles (mapv :title (get-in result [:survey-result :tasks] []))}
                   :spark (:spark-result result)
                   :summary (str (when (= :partial outcome) "PARTIAL: ")
                                 (when (= :failure outcome) "FAILED: ")
                                 "Smited " (:total-smited result 0)
                                 ", surveyed " (get-in result [:survey-result :count] 0) " tasks"
                                 ", sparked " (:total-sparked result 0) " lings"
                                 " (mode: " (or spawn_mode "vterm")
                                 ", model: " (or model "claude") ")")})))
    (catch Exception e
      (log/error "FORGE STRIKE failed" {:error (ex-message e)})
      (mcp-error (str "Forge strike failed: " (ex-message e))))))

(defn handle-forge-strike
  "Execute ONE forge cycle, FSM-driven by default with legacy config gate.

   Defense-in-depth: denies forge-strike when called from a child ling process
   (HIVE_MCP_ROLE=child-ling). Forge-strike spawns agents, which must be
   restricted to the coordinator to prevent recursive self-call chains."
  [params]
  ;; Layer 3: Defense-in-depth forge-strike guard
  (if (guards/child-ling?)
    (do
      (log/warn "Forge-strike denied: child ling attempted forge-strike"
                {:role (guards/get-role) :depth (guards/ling-depth)})
      (mcp-error (str "FORGE DENIED: Child lings cannot execute forge strikes.\n\n"
                      "You are running as a child ling (HIVE_MCP_ROLE=child-ling, depth="
                      (guards/ling-depth) ").\n"
                      "Forge-strike spawns agents, which is restricted to the coordinator\n"
                      "to prevent recursive self-call chains (Ling→forge→spawn→Ling→∞).\n\n"
                      "If you need forge work, use hivemind_shout to request the coordinator\n"
                      "to execute forge-strike on your behalf.")))
    (if (:quenched? @forge-state)
      (mcp-json {:success false
                 :message "Forge is quenched. Use forge-status to check or restart."
                 :quenched? true})
      (if (config/get-service-value :forge :legacy :default false)
        (do
          (log/warn "FORGE STRIKE: legacy mode enabled via config. Using imperative path.")
          (handle-forge-strike-legacy params))
        (fsm-forge-strike params)))))

(def handle-forge-strike-fsm
  "DEPRECATED alias: FSM is now the default path via handle-forge-strike."
  handle-forge-strike)

(def handle-forge-strike-imperative
  "DEPRECATED alias: renamed to handle-forge-strike-legacy."
  handle-forge-strike-legacy)

(defn handle-forge-status
  "Belt dashboard: show forge state, active lings, kanban summary."
  [{:keys [directory] :as _params}]
  (try
    (let [state @forge-state
          all-agents (queries/get-all-slaves)
          lings (->> all-agents
                     (filter #(= 1 (:slave/depth %))))
          active-lings (filter #(#{:active :running :idle :spawning} (:slave/status %)) lings)
          terminal-lings (filter #(#{:completed :done :error :zombie} (:slave/status %)) lings)
          kanban-result (try
                          (c-kanban/handle-kanban {:command "status" :directory directory})
                          (catch Exception _ nil))]
      (let [budget-routing? (boolean (config/get-service-value :forge :budget-routing :default false))
            budget-summary  (when budget-routing?
                              (try (budget-router/fleet-budget-summary)
                                   (catch Exception e
                                     {:error (ex-message e)})))]
        (mcp-json (cond-> {:forge (assoc state :modes {:fsm true
                                                       :legacy (boolean (config/get-service-value :forge :legacy :default false))
                                                       :budget-routing budget-routing?})
                           :lings {:total (count lings)
                                   :active (count active-lings)
                                   :terminal (count terminal-lings)
                                   :ids (mapv :slave/id active-lings)}
                           :kanban kanban-result}
                    budget-summary (assoc :budget budget-summary)))))
    (catch Exception e
      (mcp-error (str "Forge status failed: " (ex-message e))))))

(defn handle-forge-quench
  "Gracefully stop or restart the forge belt."
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

(def canonical-handlers
  {:catchup  c-session/handle-catchup
   :wrap     c-session/handle-wrap
   :complete (fn [params] (c-session/handle-session (assoc params :command "complete")))
   :forge    {:strike             handle-forge-strike
              :strike-imperative  handle-forge-strike-imperative
              :status             handle-forge-status
              :quench             handle-forge-quench
              :_handler           handle-forge-status}})

(def handlers canonical-handlers)

(def handle-workflow
  (make-cli-handler handlers))

(def tool-def
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
                              "commit_msg" {:type "string"
                                            :description "Git commit message (for complete)"}
                              "task_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Kanban task IDs. For complete: marks done. For forge-strike: survey whitelist (composable with vulcan_mode)."}
                              "agent_id" {:type "string"
                                          :description "Agent ID for session attribution"}
                              "directory" {:type "string"
                                           :description "Working directory for project scoping"}}
                 :required ["command"]}
   :handler handle-workflow})

(def tools [tool-def])
