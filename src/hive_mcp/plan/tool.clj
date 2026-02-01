(ns hive-mcp.plan.tool
  "MCP tool for converting plan memory entries to kanban tasks with KG edges.

   Implements the exploration-to-kanban pipeline:
   1. Fetch plan from memory by ID
   2. Parse plan steps using plan.parser (EDN or markdown)
   3. Create kanban tasks via memory system
   4. Create KG edges: plan --depends-on--> tasks
   5. Create KG edges: task --depends-on--> task (from step dependencies)

   SOLID: SRP - Single responsibility for plan-to-kanban conversion.
   CLARITY: L - Layers stay pure with clear domain separation.
   CLARITY: I - Inputs validated at tool boundary."
  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.plan.schema :as schema]
            [hive-mcp.plan.parser :as parser]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Topological Sort / Wave Computation
;;; =============================================================================

(defn compute-waves
  "Compute DAG wave numbers for plan steps based on dependencies.

   Returns a map of step-id -> wave-number where:
   - Wave 0: Steps with no dependencies
   - Wave N: Steps whose dependencies are all in waves < N

   This enables parallel execution within each wave and sequential
   ordering across waves for the DAG-Wave swarm pattern.

   Example:
     Steps: A (no deps), B (deps: A), C (no deps), D (deps: B, C)
     Result: {\"A\" 0, \"C\" 0, \"B\" 1, \"D\" 2}"
  [steps]
  (let [step-ids (set (map :id steps))
        step-index (into {} (map (juxt :id identity) steps))

        ;; Iteratively assign wave numbers
        compute-wave (fn [assigned step-id]
                       (let [step (get step-index step-id)
                             deps (filter step-ids (:depends-on step []))]
                         (if (empty? deps)
                           0
                           (inc (apply max (map #(get assigned % 0) deps))))))

        ;; Kahn's algorithm: assign waves in dependency order
        assign-waves (fn [assigned remaining]
                       (if (empty? remaining)
                         assigned
                         (let [;; Find steps whose deps are all assigned
                               ready (filter (fn [sid]
                                               (let [deps (:depends-on (get step-index sid) [])]
                                                 (every? #(contains? assigned %) deps)))
                                             remaining)
                               ;; Assign waves to ready steps
                               new-assigned (reduce (fn [acc sid]
                                                      (assoc acc sid (compute-wave acc sid)))
                                                    assigned
                                                    ready)
                               new-remaining (remove (set ready) remaining)]
                           (if (empty? ready)
                             ;; No progress - cycle detection should have caught this
                             (reduce #(assoc %1 %2 0) assigned remaining)
                             (recur new-assigned new-remaining)))))]
    (assign-waves {} (map :id steps))))

;;; =============================================================================
;;; Kanban Task Creation
;;; =============================================================================

(defn- create-kanban-task!
  "Create a kanban task for a plan step.
   Returns {:ok task-id} or {:error message}"
  [{:keys [title priority]} directory]
  (try
    (let [priority-str (if (keyword? priority) (name priority) (str priority))
          result (mem-kanban/handle-mem-kanban-create
                  {:title title
                   :priority priority-str
                   :directory directory})]
      (if (:isError result)
        {:error (:text result)}
        ;; Parse the result to get the task ID
        (let [parsed (try (json/read-str (:text result) :key-fn keyword)
                          (catch Exception _ nil))]
          (if-let [task-id (or (:id parsed) (get parsed "id"))]
            {:ok task-id}
            {:error "Failed to get task ID from kanban create response"}))))
    (catch Exception e
      {:error (str "Failed to create kanban task: " (.getMessage e))})))

;;; =============================================================================
;;; KG Edge Creation
;;; =============================================================================

(defn- create-plan-decision-edge!
  "Create KG edge: Plan --derived-from--> Decision.

   Links the plan memory entry to its source decision (exploration result).
   Returns the edge ID or nil if no decision-id provided."
  [plan-id decision-id scope agent-id]
  (when decision-id
    (kg-edges/add-edge!
     {:from plan-id
      :to decision-id
      :relation :derived-from
      :scope scope
      :confidence 1.0
      :source-type :manual
      :created-by (str "plan_to_kanban" (when agent-id (str ":" agent-id)))})))

(defn- create-plan-task-edges!
  "Create KG edges from plan to its tasks.
   plan --depends-on--> task (plan depends on task completion)

   Arguments:
     plan-id  - Plan memory entry ID
     task-ids - Vector of kanban task IDs
     scope    - Project scope
     agent-id - Creating agent
     waves    - Map of step-id -> wave-number (for metadata)
     step-id-to-task-id - Map of step-id -> task-id

   Returns vector of created edge IDs."
  [plan-id task-ids scope agent-id waves step-id-to-task-id]
  (let [task-id-to-step-id (into {} (map (fn [[k v]] [v k]) step-id-to-task-id))]
    (vec
     (for [task-id task-ids
           :let [step-id (get task-id-to-step-id task-id)
                 wave (get waves step-id 0)]]
       (kg-edges/add-edge!
        {:from plan-id
         :to task-id
         :relation :depends-on
         :scope scope
         :confidence 1.0
         :source-type :automated
         :created-by (str "plan_to_kanban" (when agent-id (str ":" agent-id)))})))))

(defn- create-task-dependency-edges!
  "Create KG edges for task-to-task dependencies.
   task-B --depends-on--> task-A (B depends on A completing first)

   Arguments:
     step-id-to-task-id - Map from step ID to kanban task ID
     steps - Plan steps with :depends-on fields
     scope - Project scope
     agent-id - Creating agent
     waves - Map of step-id -> wave-number

   Returns vector of created edge IDs."
  [step-id-to-task-id steps scope agent-id waves]
  (vec
   (for [step steps
         dep-id (:depends-on step)
         :when (and dep-id (contains? step-id-to-task-id dep-id))]
     (let [from-task-id (get step-id-to-task-id (:id step))
           to-task-id (get step-id-to-task-id dep-id)
           from-wave (get waves (:id step) 0)
           to-wave (get waves dep-id 0)]
       (kg-edges/add-edge!
        {:from from-task-id
         :to to-task-id
         :relation :depends-on
         :scope scope
         :confidence 1.0
         :source-type :automated
         :created-by (str "plan_to_kanban" (when agent-id (str ":" agent-id)))})))))

;;; =============================================================================
;;; Main Tool Handler
;;; =============================================================================

(defn plan-to-kanban
  "Convert a plan memory entry to kanban tasks with KG edges.

   Arguments:
     plan-memory-id - Memory entry ID containing the plan
     :directory     - Working directory for project scope (optional)
     :auto-assign?  - Auto-assign tasks to lings (optional, not yet implemented)

   Returns:
     {:task-ids [...] :kg-edges [...] :plan-id ...}

   Side effects:
     - Creates kanban tasks in memory system
     - Creates KG edges: plan --depends-on--> tasks
     - Creates KG edges: task --depends-on--> task (based on step dependencies)"
  [plan-memory-id & {:keys [directory auto-assign?]}]
  (log/info "plan_to_kanban" {:plan-id plan-memory-id :directory directory})
  (try
    (let [directory (or directory (ctx/current-directory))
          agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")]

      ;; 1. Fetch memory entry
      (if-let [entry (chroma/get-entry-by-id plan-memory-id)]
        (let [content (:content entry)
              project-id (:project-id entry)]

          ;; 2. Parse plan content using shared parser
          (let [{:keys [success plan error details]} (parser/parse-plan content {:memory-id plan-memory-id})]
            (if-not success
              (mcp-error (str error (when details (str " - " (pr-str details)))))

              (let [steps (:steps plan)]
                ;; 3. Validate dependencies exist
                (let [dep-check (schema/validate-dependencies plan)]
                  (if-not (:valid dep-check)
                    (mcp-error (str "Invalid dependency references: " (pr-str (:invalid-refs dep-check))))

                    ;; 4. Check for circular dependencies
                    (let [cycle-check (schema/detect-cycles plan)]
                      (if-not (:valid cycle-check)
                        (mcp-error (str "Circular dependency detected: " (:cycle cycle-check)))

                        ;; 5. Compute wave numbers for DAG-Wave execution
                        (let [waves (compute-waves steps)

                              ;; 6. Create kanban tasks for each step
                              task-results (for [step steps]
                                             (let [{:keys [ok error]} (create-kanban-task! step directory)]
                                               (if error
                                                 {:step-id (:id step) :error error}
                                                 {:step-id (:id step) :task-id ok})))
                              errors (filter :error task-results)
                              successes (remove :error task-results)]

                          (if (seq errors)
                            (mcp-error (str "Failed to create some tasks: "
                                            (str/join ", " (map #(str (:step-id %) ": " (:error %)) errors))))

                            ;; 7. Create KG edges
                            (let [step-id-to-task-id (into {} (map (juxt :step-id :task-id) successes))
                                  task-ids (mapv :task-id successes)
                                  decision-id (:decision-id plan)

                                  ;; Plan --derived-from--> Decision (if decision-id present)
                                  decision-edge (create-plan-decision-edge!
                                                 plan-memory-id decision-id project-id agent-id)

                                  ;; Plan --depends-on--> Task edges
                                  plan-task-edges (create-plan-task-edges!
                                                   plan-memory-id task-ids project-id agent-id
                                                   waves step-id-to-task-id)

                                  ;; Task --depends-on--> Task dependency edges
                                  task-dep-edges (create-task-dependency-edges!
                                                  step-id-to-task-id steps project-id agent-id waves)

                                  all-edges (cond-> (vec (concat plan-task-edges task-dep-edges))
                                              decision-edge (conj decision-edge))]

                              (mcp-json {:success true
                                         :plan-id plan-memory-id
                                         :plan-title (:title plan)
                                         :decision-id decision-id
                                         :task-ids task-ids
                                         :task-count (count task-ids)
                                         :waves waves
                                         :max-wave (when (seq waves) (apply max (vals waves)))
                                         :kg-edges all-edges
                                         :edge-count (count all-edges)
                                         :step-mapping step-id-to-task-id}))))))))))))

        (mcp-error (str "Plan memory entry not found: " plan-memory-id))))

    (catch Exception e
      (log/error e "plan_to_kanban failed")
      (mcp-error (str "Failed to convert plan to kanban: " (.getMessage e))))))

(defn handle-plan-to-kanban
  "MCP tool handler for plan_to_kanban."
  [{:keys [plan_id directory auto_assign]}]
  (plan-to-kanban plan_id :directory directory :auto-assign? auto_assign))

;;; =============================================================================
;;; Tool Definition
;;; =============================================================================

(def tools
  [{:name "plan_to_kanban"
    :description "Convert an exploration plan memory entry to kanban tasks with KG linking. Parses plan steps from EDN or markdown format, creates kanban tasks, and creates KG edges for plan->task and task->task dependencies."
    :inputSchema {:type "object"
                  :properties {"plan_id" {:type "string"
                                          :description "Memory entry ID containing the plan (required)"}
                               "directory" {:type "string"
                                            :description "Working directory for project scope (optional)"}
                               "auto_assign" {:type "boolean"
                                              :description "Auto-assign tasks to lings (optional, not yet implemented)"}}
                  :required ["plan_id"]}
    :handler handle-plan-to-kanban}])
