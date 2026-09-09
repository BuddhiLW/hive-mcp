(ns hive-mcp.tools.consolidated.workflow.forge-ops
  "Forge operational primitives: smite (reap terminal agents), survey
   (query + prioritize kanban tasks), and forge status dashboard.

   Extracted from workflow.clj to reduce cyclomatic complexity.
   Pure operational logic — no MCP handler concerns.

   Forge cycle logic (build-fsm-resources, forge-strike*) lives in
   workflow.forge-cycle to keep CC per-file under 50."
  (:require [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.config.core :as config]
            [hive-mcp.agent.budget-router :as budget-router]
            [hive-mcp.scheduler.vulcan :as vulcan]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.dns.result :as result]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-mcp.vectordb.facade :as memory]
            [hive-mcp.vectordb.kanban-facade :as kanban]
            [hive-mcp.plan.parser :as plan-parser]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ── Smite (reap terminal agents) ────────────────────────────────────────────

(def ^:private terminal-statuses
  "Agent statuses considered terminal for smiting."
  #{:completed :done :error :zombie})

(defn- forja-agent?
  "True if agent was spawned by the forja belt."
  [agent]
  (some-> (:slave/id agent) (str/starts-with? "swarm-forja-")))

(defn- smitable?
  "True if an agent is eligible for smiting (terminal or idle-forja)."
  [agent]
  (let [status (:slave/status agent)]
    (or (terminal-statuses status)
        (and (= :idle status) (forja-agent? agent)))))

(defn- kill-agent!
  "Attempt to kill a single agent. Returns map with :id, :killed, :error."
  [agent]
  (let [id (:slave/id agent)
        r  (result/rescue
            {:id id :killed false}
            (let [ling-agent (ling/->ling id {:cwd        (:slave/cwd agent)
                                              :presets    (or (:slave/presets agent) [])
                                              :project-id (:slave/project-id agent)
                                              :spawn-mode (or (:ling/spawn-mode agent) :claude)})
                  _result    (proto/kill! ling-agent)]
              (log/info "SMITE: killed" {:id id :status (:slave/status agent)})
              {:id id :status (name (:slave/status agent)) :killed true}))]
    (cond-> r
      (and (not (:killed r)) (not (:error r)) (::result/error (meta r)))
      (assoc :error (get-in (meta r) [::result/error :message] "unknown")))))

(declare plan-task-ids)

(defn smite!
  "Reap eligible agents; explicit plan/task scopes require exact project and task ownership."
  ([opts] (smite! opts {}))
  ([{:keys [directory plan_id task_ids] :as opts}
    {:keys [agents-fn kill-fn plan-ids-fn project-id-fn]
     :or {agents-fn queries/get-all-slaves
          kill-fn kill-agent!
          plan-ids-fn #(plan-task-ids % {})
          project-id-fn scope/get-current-project-id}}]
   (let [scoped? (or (contains? opts :plan_id) (contains? opts :task_ids))
         project-id (when directory (project-id-fn directory))
         _ (when (and scoped? (or (not (string? project-id)) (str/blank? project-id)))
             (throw (ex-info "Scoped forge cleanup requires an explicit project directory" {})))
         _ (when (and (contains? opts :task_ids)
                      (not (and (sequential? task_ids)
                                (every? #(and (string? %) (not (str/blank? %))) task_ids))))
             (throw (ex-info "Forge task_ids must be a sequence of non-blank IDs" {})))
         plan-ids (when (contains? opts :plan_id) (set (plan-ids-fn plan_id)))
         task-scope (cond
                      (and plan-ids (contains? opts :task_ids)) (set (filter plan-ids task_ids))
                      plan-ids plan-ids
                      (contains? opts :task_ids) (set task_ids))
         candidates (->> (agents-fn)
                         (filter #(= 1 (:slave/depth %)))
                         (filter smitable?)
                         (filter #(if scoped?
                                    (and (= project-id (:slave/project-id %))
                                         (contains? task-scope (:slave/kanban-task-id %)))
                                    (or (nil? project-id)
                                        (nil? (:slave/project-id %))
                                        (= project-id (:slave/project-id %))))))
         results (mapv kill-fn candidates)]
     {:smited (filterv :killed results)
      :failed (filterv (complement :killed) results)
      :count (count (filter :killed results))})))

;; ── Survey (query + prioritize kanban) ──────────────────────────────────────

(defn parse-kanban-tasks
  "Parse kanban list handler response into a vector of task maps."
  [result]
  (let [text   (if (map? result) (:text result) result)
        parsed (when (string? text)
                 (result/rescue nil (json/read-str text :key-fn keyword)))]
    (cond
      (sequential? parsed) parsed
      (map? parsed)        (or (:tasks parsed) [])
      :else                [])))

(defn sort-by-priority-then-created
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

(defn- apply-milestone-boundary-filter
  "Post-filter: exclude tasks from milestones whose prerequisites are incomplete.
   Config-gated via [:forge :milestone-boundary]. Graceful degradation."
  [prioritized]
  (if (config/get-service-value :forge :milestone-boundary :default false)
    (result/rescue prioritized
                   (when-let [filter-fn (ext/get-extension :fb/milestone-boundary-filter)]
                     (let [{:keys [tasks excluded-count reason]} (filter-fn (:tasks prioritized))]
                       (when (pos? (or excluded-count 0))
                         (log/info "SURVEY: milestone-boundary excluded" excluded-count "tasks:" reason))
                       (-> prioritized
                           (assoc :tasks tasks)
                           (assoc :count (count tasks))
                           (update :blocked-count + (or excluded-count 0))))))
    prioritized))

(defn plan-task-ids
  "Resolve a complete converted plan to its kanban IDs. Missing membership or dependency edges fail closed.
   Until cards carry stable step IDs, unique titles provide the conversion witness."
  [plan-id {:keys [plan-entry-fn task-entry-fn deps-fn]
            :or {plan-entry-fn memory/get-entry-by-id
                 task-entry-fn kanban/get-entry-by-id
                 deps-fn vulcan/get-task-deps}}]
  (when (or (not (string? plan-id)) (str/blank? plan-id))
    (throw (ex-info "Forge plan_id must be a non-blank memory ID" {:plan-id plan-id})))
  (let [entry (plan-entry-fn plan-id)
        _ (when-not (#{:plan "plan"} (:type entry))
            (throw (ex-info "Forge plan not found or not type plan" {:plan-id plan-id})))
        parsed (plan-parser/parse-plan (:content entry))
        steps (get-in parsed [:plan :steps])
        ids (set (deps-fn plan-id))
        cards (mapv (fn [id]
                      (let [entry (task-entry-fn id)
                            raw (:content entry)
                            content (if (string? raw) (json/read-str raw :key-fn keyword) raw)]
                        (when (#{:missing :invalid} (vulcan/entry-state entry))
                          (throw (ex-info "Forge plan references missing or invalid kanban card"
                                          {:plan-id plan-id :task-id id})))
                        (assoc content :id id)))
                    ids)
        by-title (group-by :title cards)]
    (when-not (and (:success parsed) (seq steps)
                   (= (count steps) (count ids))
                   (= (count steps) (count (set (map :title steps))))
                   (every? #(= 1 (count (get by-title (:title %)))) steps))
      (throw (ex-info "Forge plan conversion is incomplete or ambiguous; repair plan-to-kanban links before dispatch"
                      {:plan-id plan-id :step-count (count steps) :task-count (count ids)
                       :parse-error (:error parsed)})))
    (let [mapping (into {} (map (fn [step]
                                 [(:id step) (:id (first (get by-title (:title step))))]))
                        steps)]
      (doseq [step steps
              :let [id (get mapping (:id step))
                    actual (set (deps-fn id))]
              dependency (:depends-on step)
              :when (not (contains? actual (get mapping dependency)))]
        (throw (ex-info "Forge plan dependency edge missing; repair conversion before dispatch"
                        {:plan-id plan-id :task-id id :dependency-step dependency}))))
    ids))

(defn survey
  "Query ready todo tasks. A supplied plan_id must resolve a complete conversion.
   task_ids intersects the plan scope; an explicit empty whitelist selects nothing.
   Selection status distinguishes ready, blocked, no-ready, and proven plan completion."
  [{:keys [directory plan_id task_ids task_filter] :as opts}]
  (try
    (let [plan-ids (when (contains? opts :plan_id) (plan-task-ids plan_id opts))
          entry-fn (or (:task-entry-fn opts) kanban/get-entry-by-id)
          plan-states (when plan-ids (into {} (map (fn [id] [id (vulcan/entry-state (entry-fn id))])) plan-ids))
          plan-complete? (boolean (and (seq plan-states) (every? #{:done} (vals plan-states))))
          response (c-kanban/handle-kanban {:command "list" :status "todo" :directory directory})
          _ (when (:isError response)
              (throw (ex-info "Forge kanban lookup failed" {:response response})))
          all-tasks (parse-kanban-tasks response)
          tasks (cond->> all-tasks
                  (some? plan-ids) (filterv #(contains? plan-ids (:id %)))
                  (some? task_ids) (filterv #(contains? (set task_ids) (:id %)))
                  (some? task_filter) (filterv #(str/starts-with? (or (:title %) "") task_filter)))
          prioritized (vulcan/prioritize-tasks
                       tasks #{} (dissoc opts :directory :plan_id :task_ids :task_filter))
          filtered (apply-milestone-boundary-filter prioritized)
          selection-status (cond
                             (pos? (:count filtered 0)) :ready
                             (pos? (:blocked-count filtered 0)) :blocked
                             plan-complete? :complete
                             :else :no-ready)]
      (cond-> (assoc filtered :selection-status selection-status)
        (contains? opts :plan_id)
        (assoc :plan-id plan_id :plan-task-count (count plan-ids)
               :plan-states plan-states
               :plan-completed-count (count (filter #{:done} (vals plan-states)))
               :plan-complete? plan-complete?)))
    (catch Exception e
      (if (contains? opts :plan_id)
        (throw (ex-info "Forge plan survey failed; no tasks selected"
                        {:plan-id plan_id :error (ex-message e)} e))
        {:tasks [] :count 0 :selection-status :error :error (ex-message e)}))))

;; ── Forge Status ────────────────────────────────────────────────────────────

(defn forge-status*
  "Compute forge status dashboard data. Returns Result.
   Accepts forge-state atom for reading current state."
  [{:keys [directory]} forge-state]
  (let [state          @forge-state
        all-agents     (queries/get-all-slaves)
        lings          (->> all-agents (filter #(= 1 (:slave/depth %))))
        active-lings   (filter #(#{:active :running :idle :spawning} (:slave/status %)) lings)
        terminal-lings (filter #(terminal-statuses (:slave/status %)) lings)
        kanban-result  (result/rescue nil
                                      (c-kanban/handle-kanban {:command "status" :directory directory}))
        budget-routing? (boolean (config/get-service-value :forge :budget-routing :default false))
        budget-summary  (when budget-routing?
                          (result/rescue {:error "budget-summary-failed"}
                                         (budget-router/fleet-budget-summary)))]
    (result/ok (cond-> {:forge  (assoc state :modes {:fsm            true
                                                     :legacy         (boolean (config/get-service-value :forge :legacy :default false))
                                                     :budget-routing budget-routing?})
                        :lings  {:total    (count lings)
                                 :active   (count active-lings)
                                 :terminal (count terminal-lings)
                                 :ids      (mapv :slave/id active-lings)}
                        :kanban kanban-result}
                 budget-summary (assoc :budget budget-summary)))))
