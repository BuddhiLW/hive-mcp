(ns hive-mcp.swarm.datascript
  "DataScript-based state management for swarm hivemind coordination.

   Replaces logic.clj's core.logic pldb with DataScript for:
   - Richer query capabilities (Datalog with refs, aggregates)
   - Entity relationships (slaves → tasks → claims)
   - Transaction semantics with proper retractions

   Entities:
   - Slave: Worker agents in the swarm hierarchy
   - Task: Work items dispatched to slaves
   - Claim: File ownership for conflict detection

   SOLID: Single Responsibility - swarm state only, no graph/memory concerns.
   DDD: Repository pattern for swarm entity persistence."
  (:require [datascript.core :as d]
            [taoensso.timbre :as log]))

;; Forward declarations for functions referenced before definition
(declare release-claims-for-slave! release-claims-for-task!)

;;; =============================================================================
;;; Schema Definition
;;; =============================================================================

(def slave-statuses
  "Valid slave status values.

   :idle      - Ready for work
   :spawning  - Being created
   :starting  - Process starting
   :working   - Executing a task
   :error     - In error state"
  #{:idle :spawning :starting :working :error})

(def task-statuses
  "Valid task status values.

   :queued     - Waiting for dispatch (file conflicts)
   :dispatched - Sent to slave, in progress
   :completed  - Successfully finished
   :timeout    - Timed out waiting
   :error      - Failed with error"
  #{:queued :dispatched :completed :timeout :error})

(def schema
  "DataScript schema for swarm state.

   Design notes:
   - :db.type/ref for entity relationships (enables joins)
   - :db/unique for primary keys
   - :db.cardinality/many for collections (presets, files)"

  {;;; =========================================================================
   ;;; Slave Entity
   ;;; =========================================================================

   :slave/id
   {:db/doc "Unique identifier for the slave (e.g., 'swarm-worker-123')"
    :db/unique :db.unique/identity}

   :slave/name
   {:db/doc "Human-readable name for the slave"}

   :slave/status
   {:db/doc "Current status: :idle :spawning :starting :working :error"}

   :slave/depth
   {:db/doc "Hierarchy depth: 0=hivemind, 1=ling, 2=drone"}

   :slave/parent
   {:db/doc "Reference to parent slave (for hierarchy)"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :slave/presets
   {:db/doc "Applied presets (e.g., 'tdd', 'reviewer')"
    :db/cardinality :db.cardinality/many}

   :slave/cwd
   {:db/doc "Current working directory"}

   :slave/current-task
   {:db/doc "Reference to currently executing task"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :slave/tasks-completed
   {:db/doc "Count of completed tasks"}

   :slave/created-at
   {:db/doc "Timestamp when slave was created"}

   ;;; =========================================================================
   ;;; Task Entity
   ;;; =========================================================================

   :task/id
   {:db/doc "Unique identifier for the task"
    :db/unique :db.unique/identity}

   :task/slave
   {:db/doc "Reference to owning slave"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :task/status
   {:db/doc "Current status: :queued :dispatched :completed :timeout :error"}

   :task/prompt
   {:db/doc "Task description/prompt text"}

   :task/files
   {:db/doc "Files this task operates on"
    :db/cardinality :db.cardinality/many}

   :task/started-at
   {:db/doc "Timestamp when task started"}

   :task/completed-at
   {:db/doc "Timestamp when task completed (nil if pending)"}

   ;;; =========================================================================
   ;;; Claim Entity
   ;;; =========================================================================

   :claim/file
   {:db/doc "File path being claimed (unique - one claim per file)"
    :db/unique :db.unique/identity}

   :claim/slave
   {:db/doc "Reference to slave holding the claim"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :claim/task
   {:db/doc "Reference to task that created this claim"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :claim/created-at
   {:db/doc "Timestamp when claim was created"}

   ;;; =========================================================================
   ;;; Wrap Queue Entity (Crystal Convergence)
   ;;; =========================================================================

   :wrap-queue/id
   {:db/doc "Unique identifier for wrap notification"
    :db/unique :db.unique/identity}

   :wrap-queue/agent-id
   {:db/doc "ID of the ling that wrapped"}

   :wrap-queue/session-id
   {:db/doc "Session tag (e.g., session:2026-01-14:ling-123)"}

   :wrap-queue/created-ids
   {:db/doc "Memory entry IDs created during this wrap"
    :db/cardinality :db.cardinality/many}

   :wrap-queue/stats
   {:db/doc "Map of stats {:notes N :decisions N :conventions N}"}

   :wrap-queue/processed?
   {:db/doc "Whether coordinator has processed this wrap"}

   :wrap-queue/created-at
   {:db/doc "Timestamp when wrap occurred"}

   ;;; =========================================================================
   ;;; Change Plan Entity (dispatch_drone_wave)
   ;;; =========================================================================

   :change-plan/id
   {:db/doc "Unique identifier for the change plan"
    :db/unique :db.unique/identity}

   :change-plan/status
   {:db/doc "Plan status: :pending :in-progress :completed :failed"}

   :change-plan/preset
   {:db/doc "Drone preset for all items (e.g., 'drone-worker')"}

   :change-plan/created-at
   {:db/doc "Timestamp when plan was created"}

   :change-plan/completed-at
   {:db/doc "Timestamp when plan completed (nil if pending)"}

   ;;; =========================================================================
   ;;; Change Item Entity (dispatch_drone_wave items)
   ;;; =========================================================================

   :change-item/id
   {:db/doc "Unique identifier for the change item"
    :db/unique :db.unique/identity}

   :change-item/plan
   {:db/doc "Reference to parent change plan"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :change-item/file
   {:db/doc "File path this item operates on"}

   :change-item/task
   {:db/doc "Task description for this item"}

   :change-item/status
   {:db/doc "Item status: :pending :dispatched :completed :failed"}

   :change-item/drone-id
   {:db/doc "Drone slave-id if dispatched"}

   :change-item/result
   {:db/doc "Result message on completion/failure"}

   :change-item/created-at
   {:db/doc "Timestamp when item was created"}

   :change-item/completed-at
   {:db/doc "Timestamp when item completed"}

   ;;; =========================================================================
   ;;; Wave Entity (dispatch_drone_wave execution)
   ;;; =========================================================================

   :wave/id
   {:db/doc "Unique identifier for the wave execution"
    :db/unique :db.unique/identity}

   :wave/plan
   {:db/doc "Reference to change plan being executed"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :wave/concurrency
   {:db/doc "Max concurrent drones (default: 3)"}

   :wave/active-count
   {:db/doc "Currently active drone count"}

   :wave/completed-count
   {:db/doc "Number of completed items"}

   :wave/failed-count
   {:db/doc "Number of failed items"}

   :wave/status
   {:db/doc "Wave status: :running :completed :partial-failure"}

   :wave/started-at
   {:db/doc "Timestamp when wave started"}

   :wave/completed-at
   {:db/doc "Timestamp when wave completed"}})

;;; =============================================================================
;;; Connection Management (Thread-Safe Atom)
;;; =============================================================================

;; Global DataScript connection atom.
;; Thread-safe via DataScript's internal atom.
(defonce ^:private conn (atom nil))

(defn create-conn
  "Create a new DataScript connection with swarm schema.
   Returns the connection (atom wrapper around db)."
  []
  (d/create-conn schema))

(defn get-conn
  "Get the global swarm connection, creating if needed."
  []
  (or @conn
      (do
        (reset! conn (create-conn))
        (log/info "Created swarm DataScript connection")
        @conn)))

(defn reset-conn!
  "Reset the global connection to empty state."
  []
  (reset! conn (create-conn))
  (log/debug "Swarm DataScript connection reset"))

(defn- ensure-conn
  "Ensure connection exists, return it."
  []
  (get-conn))

;;; =============================================================================
;;; Helper Functions
;;; =============================================================================

(defn- now
  "Current timestamp as java.util.Date."
  []
  (java.util.Date.))

(defn- gen-id
  "Generate a unique ID with optional prefix."
  ([] (str (java.util.UUID/randomUUID)))
  ([prefix] (str prefix "-" (java.util.UUID/randomUUID))))

;;; =============================================================================
;;; Slave CRUD Functions
;;; =============================================================================

(defn add-slave!
  "Add a new slave to the swarm.

   Arguments:
     slave-id  - Unique identifier (required)
     opts      - Map with optional keys:
                 :name    - Human-readable name (defaults to slave-id)
                 :status  - Initial status (default :idle)
                 :depth   - Hierarchy depth (default 1 for ling)
                 :parent  - Parent slave-id string
                 :presets - Collection of preset names
                 :cwd     - Working directory

   Returns:
     Transaction report with :tempids"
  [slave-id {:keys [name status depth parent presets cwd]
             :or {status :idle depth 1}}]
  {:pre [(string? slave-id)
         (contains? slave-statuses status)]}
  (let [c (ensure-conn)
        tx-data (cond-> {:slave/id slave-id
                         :slave/name (or name slave-id)
                         :slave/status status
                         :slave/depth depth
                         :slave/tasks-completed 0
                         :slave/created-at (now)}
                  cwd (assoc :slave/cwd cwd)
                  (seq presets) (assoc :slave/presets (vec presets))
                  parent (assoc :slave/parent [:slave/id parent]))]
    (log/debug "Adding slave:" slave-id "status:" status)
    (d/transact! c [tx-data])))

(defn update-slave!
  "Update an existing slave's attributes.

   Arguments:
     slave-id - Slave to update
     updates  - Map of attributes to update (supports all slave attrs)

   Returns:
     Transaction report or nil if slave not found"
  [slave-id updates]
  (let [c (ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:slave/id slave-id]))]
      (let [tx-data (cond-> (assoc updates :db/id eid)
                      ;; Convert parent string to lookup ref
                      (:slave/parent updates)
                      (update :slave/parent (fn [p] [:slave/id p]))
                      ;; Convert current-task string to lookup ref
                      (:slave/current-task updates)
                      (update :slave/current-task (fn [t] [:task/id t])))]
        (log/debug "Updating slave:" slave-id "with:" (keys updates))
        (d/transact! c [tx-data])))))

(defn remove-slave!
  "Remove a slave from the swarm.
   Also releases any file claims held by this slave.

   Arguments:
     slave-id - Slave to remove

   Returns:
     Transaction report or nil if slave not found"
  [slave-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:slave/id slave-id]))]
      ;; First release claims
      (release-claims-for-slave! slave-id)
      ;; Then retract entity
      (log/debug "Removing slave:" slave-id)
      (d/transact! c [[:db/retractEntity eid]]))))

;;; =============================================================================
;;; Task CRUD Functions
;;; =============================================================================

(defn add-task!
  "Add a new task to the swarm.

   Arguments:
     task-id  - Unique identifier (optional, auto-generated if nil)
     slave-id - Owning slave's id
     opts     - Map with keys:
                :status  - Initial status (default :dispatched)
                :prompt  - Task description
                :files   - Collection of file paths

   Returns:
     Transaction report with :tempids
     The task-id (generated or provided) is in (:tempids report)"
  [task-id slave-id {:keys [status prompt files]
                     :or {status :dispatched}}]
  {:pre [(or (nil? task-id) (string? task-id))
         (string? slave-id)
         (contains? task-statuses status)]}
  (let [c (ensure-conn)
        tid (or task-id (gen-id "task"))
        tx-data (cond-> {:task/id tid
                         :task/slave [:slave/id slave-id]
                         :task/status status
                         :task/started-at (now)}
                  prompt (assoc :task/prompt prompt)
                  (seq files) (assoc :task/files (vec files)))]
    (log/debug "Adding task:" tid "for slave:" slave-id)
    (d/transact! c [tx-data])))

(defn complete-task!
  "Mark a task as completed and update slave stats.

   Arguments:
     task-id - Task to complete

   Returns:
     Transaction report or nil if task not found"
  [task-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [task (d/entity db [:task/id task-id])]
      (let [slave-eid (get-in task [:task/slave :db/id])
            slave (d/entity db slave-eid)
            completed-count (or (:slave/tasks-completed slave) 0)
            ;; Build retraction for current-task if it exists
            current-task-ref (:slave/current-task slave)
            retract-current (when current-task-ref
                              [:db/retract slave-eid :slave/current-task (:db/id current-task-ref)])]
        (log/debug "Completing task:" task-id)
        ;; Release file claims for this task
        (release-claims-for-task! task-id)
        ;; Update task status and slave stats (use retract instead of nil)
        (d/transact! c (cond-> [{:db/id (:db/id task)
                                 :task/status :completed
                                 :task/completed-at (now)}
                                {:db/id slave-eid
                                 :slave/tasks-completed (inc completed-count)}]
                         retract-current (conj retract-current)))))))

(defn fail-task!
  "Mark a task as failed with error or timeout.

   Arguments:
     task-id - Task to fail
     status  - Failure status (:error or :timeout)

   Returns:
     Transaction report or nil if task not found"
  [task-id status]
  {:pre [(contains? #{:error :timeout} status)]}
  (let [c (ensure-conn)
        db @c]
    (when-let [task (d/entity db [:task/id task-id])]
      (log/debug "Failing task:" task-id "with status:" status)
      ;; Release file claims for this task
      (release-claims-for-task! task-id)
      ;; Update task and clear slave's current-task (use retract instead of nil)
      (let [slave-eid (get-in task [:task/slave :db/id])
            slave (d/entity db slave-eid)
            current-task-ref (:slave/current-task slave)
            retract-current (when current-task-ref
                              [:db/retract slave-eid :slave/current-task (:db/id current-task-ref)])]
        (d/transact! c (cond-> [{:db/id (:db/id task)
                                 :task/status status
                                 :task/completed-at (now)}]
                         retract-current (conj retract-current)))))))

;;; =============================================================================
;;; Claim CRUD Functions
;;; =============================================================================

(defn claim-file!
  "Create a file claim for a slave/task.

   Arguments:
     file-path - Path to claim (must be unique)
     slave-id  - Slave making the claim
     task-id   - Task associated with claim (optional)

   Returns:
     Transaction report

   Note: Due to :db/unique on :claim/file, attempting to claim
   an already-claimed file will upsert (update the existing claim).
   Use has-conflict? to check first if you want to prevent this."
  [file-path slave-id & [task-id]]
  {:pre [(string? file-path)
         (string? slave-id)]}
  (let [c (ensure-conn)
        tx-data (cond-> {:claim/file file-path
                         :claim/slave [:slave/id slave-id]
                         :claim/created-at (now)}
                  task-id (assoc :claim/task [:task/id task-id]))]
    (log/debug "Claiming file:" file-path "for slave:" slave-id)
    (d/transact! c [tx-data])))

(defn release-claim!
  "Release a file claim.

   Arguments:
     file-path - Path to release

   Returns:
     Transaction report or nil if claim not found"
  [file-path]
  (let [c (ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:claim/file file-path]))]
      (log/debug "Releasing claim:" file-path)
      (d/transact! c [[:db/retractEntity eid]]))))

(defn release-claims-for-slave!
  "Release all file claims held by a slave.

   Arguments:
     slave-id - Slave whose claims to release

   Returns:
     Number of claims released"
  [slave-id]
  (let [c (ensure-conn)
        db @c
        slave-eid (:db/id (d/entity db [:slave/id slave-id]))
        claims (when slave-eid
                 (d/q '[:find ?file
                        :in $ ?slave-eid
                        :where
                        [?c :claim/slave ?slave-eid]
                        [?c :claim/file ?file]]
                      db slave-eid))]
    (when (seq claims)
      (log/debug "Releasing" (count claims) "claims for slave:" slave-id)
      (doseq [[file] claims]
        (release-claim! file)))
    (count claims)))

(defn release-claims-for-task!
  "Release all file claims associated with a task.

   Arguments:
     task-id - Task whose claims to release

   Returns:
     Number of claims released"
  [task-id]
  (let [c (ensure-conn)
        db @c
        task-eid (:db/id (d/entity db [:task/id task-id]))
        claims (when task-eid
                 (d/q '[:find ?file
                        :in $ ?task-eid
                        :where
                        [?c :claim/task ?task-eid]
                        [?c :claim/file ?file]]
                      db task-eid))]
    (when (seq claims)
      (log/debug "Releasing" (count claims) "claims for task:" task-id)
      (doseq [[file] claims]
        (release-claim! file)))
    (count claims)))

;;; =============================================================================
;;; Query Functions
;;; =============================================================================

(defn get-slave
  "Get a slave by ID.

   Returns:
     Map with slave attributes or nil if not found"
  [slave-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:slave/id slave-id])]
      (-> (into {} e)
          (dissoc :db/id)
          ;; Resolve refs to IDs
          (update :slave/parent #(when % (:slave/id %)))
          (update :slave/current-task #(when % (:task/id %)))))))

(defn get-all-slaves
  "Get all slaves in the swarm.

   Returns:
     Seq of maps with slave attributes"
  []
  (let [c (ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :where [?e :slave/id _]]
                  db)]
    (->> eids
         (map #(d/entity db %))
         (map (fn [e]
                (-> (into {} e)
                    (dissoc :db/id)
                    (update :slave/parent #(when % (:slave/id %)))
                    (update :slave/current-task #(when % (:task/id %)))))))))

(defn get-slaves-by-status
  "Get slaves filtered by status.

   Arguments:
     status - Status to filter by

   Returns:
     Seq of slave maps"
  [status]
  (let [c (ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :in $ ?status
                    :where
                    [?e :slave/id _]
                    [?e :slave/status ?status]]
                  db status)]
    (->> eids
         (map #(d/entity db %))
         (map (fn [e]
                (-> (into {} e)
                    (dissoc :db/id)
                    (update :slave/parent #(when % (:slave/id %)))
                    (update :slave/current-task #(when % (:task/id %)))))))))

(defn get-task
  "Get a task by ID.

   Returns:
     Map with task attributes or nil if not found"
  [task-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:task/id task-id])]
      (-> (into {} e)
          (dissoc :db/id)
          (update :task/slave #(when % (:slave/id %)))))))

(defn get-tasks-for-slave
  "Get all tasks for a slave.

   Arguments:
     slave-id - Slave to get tasks for
     status   - Optional status filter

   Returns:
     Seq of task maps"
  [slave-id & [status]]
  (let [c (ensure-conn)
        db @c
        slave-eid (:db/id (d/entity db [:slave/id slave-id]))]
    (when slave-eid
      (let [query (if status
                    '[:find [?e ...]
                      :in $ ?slave-eid ?status
                      :where
                      [?e :task/slave ?slave-eid]
                      [?e :task/status ?status]]
                    '[:find [?e ...]
                      :in $ ?slave-eid
                      :where
                      [?e :task/slave ?slave-eid]])
            args (if status [db slave-eid status] [db slave-eid])
            eids (apply d/q query args)]
        (->> eids
             (map #(d/entity db %))
             (map (fn [e]
                    (-> (into {} e)
                        (dissoc :db/id)
                        (update :task/slave #(when % (:slave/id %)))))))))))

(defn get-completed-tasks
  "Get all completed tasks, optionally filtered by slave or time range.

   Options:
   - :slave-id - Filter by specific slave
   - :since - Only tasks completed after this timestamp (java.util.Date)
   - :limit - Maximum number to return (default 100)

   Returns:
     Seq of task maps sorted by completion time (most recent first)"
  [& {:keys [slave-id since limit] :or {limit 100}}]
  (let [c (ensure-conn)
        db @c
        ;; Query all completed tasks with slave info
        completed-tasks (d/q '[:find [(pull ?e [* {:task/slave [:slave/id]}]) ...]
                               :in $ ?status
                               :where
                               [?e :task/status ?status]]
                             db :completed)]
    (->> completed-tasks
         ;; Filter by slave-id if provided
         (filter (fn [task]
                   (or (nil? slave-id)
                       (= slave-id (get-in task [:task/slave :slave/id])))))
         ;; Filter by since timestamp if provided
         (filter (fn [task]
                   (or (nil? since)
                       (and (:task/completed-at task)
                            (.after (:task/completed-at task) since)))))
         ;; Sort by completion time (most recent first)
         (sort-by :task/completed-at #(compare %2 %1))
         ;; Apply limit
         (take limit)
         ;; Clean up the output format
         (map (fn [task]
                (-> task
                    (dissoc :db/id)
                    (update :task/slave #(when % (:slave/id %)))))))))

(defn get-claims-for-file
  "Get claim info for a file.

   Returns:
     Map with :slave-id and :task-id or nil if unclaimed"
  [file-path]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:claim/file file-path])]
      {:file file-path
       :slave-id (get-in e [:claim/slave :slave/id])
       :task-id (get-in e [:claim/task :task/id])
       :created-at (:claim/created-at e)})))

(defn get-all-claims
  "Get all current file claims.

   Returns:
     Seq of {:file :slave-id :task-id} maps"
  []
  (let [c (ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :where [?e :claim/file _]]
                  db)]
    (->> eids
         (map #(d/entity db %))
         (map (fn [e]
                {:file (:claim/file e)
                 :slave-id (get-in e [:claim/slave :slave/id])
                 :task-id (get-in e [:claim/task :task/id])
                 :created-at (:claim/created-at e)})))))

(defn has-conflict?
  "Check if a file claim would conflict with existing claims.

   Arguments:
     file-path       - File to check
     requesting-slave - Slave wanting to claim (conflicts if different)

   Returns:
     {:conflict? bool :held-by slave-id} or {:conflict? false}"
  [file-path requesting-slave]
  (let [c (ensure-conn)
        db @c]
    (if-let [e (d/entity db [:claim/file file-path])]
      (let [holder-id (get-in e [:claim/slave :slave/id])]
        (if (= holder-id requesting-slave)
          {:conflict? false}
          {:conflict? true :held-by holder-id}))
      {:conflict? false})))

(defn check-file-conflicts
  "Check for conflicts on multiple files.

   Arguments:
     requesting-slave - Slave wanting to claim files
     files           - Collection of file paths to check

   Returns:
     Seq of {:file :held-by} for conflicting files (empty if no conflicts)"
  [requesting-slave files]
  (when (seq files)
    (->> files
         (map (fn [f]
                (let [result (has-conflict? f requesting-slave)]
                  (when (:conflict? result)
                    {:file f :held-by (:held-by result)}))))
         (filter some?))))

;;; =============================================================================
;;; Statistics & Debugging
;;; =============================================================================

(defn db-stats
  "Get statistics about the current swarm state."
  []
  (let [c (ensure-conn)
        db @c]
    {:slaves (count (d/q '[:find ?e :where [?e :slave/id _]] db))
     :tasks (count (d/q '[:find ?e :where [?e :task/id _]] db))
     :claims (count (d/q '[:find ?e :where [?e :claim/file _]] db))
     :active-tasks (count (d/q '[:find ?e
                                 :where
                                 [?e :task/status :dispatched]]
                               db))
     :wrap-queue (count (d/q '[:find ?e :where [?e :wrap-queue/id _]] db))
     :unprocessed-wraps (count (d/q '[:find ?e
                                      :where
                                      [?e :wrap-queue/processed? false]]
                                    db))}))

(defn dump-db
  "Dump the current database state for debugging."
  []
  {:slaves (get-all-slaves)
   :tasks (d/q '[:find [(pull ?e [*]) ...]
                 :where [?e :task/id _]]
               @(ensure-conn))
   :claims (get-all-claims)
   :stats (db-stats)})

;;; =============================================================================
;;; Wrap Queue Operations (Crystal Convergence)
;;; =============================================================================

(defn add-wrap-notification!
  "Record a ling wrap for coordinator permeation.

   Arguments:
     wrap-id - Unique identifier for this wrap notification
     opts    - Map with keys:
               :agent-id    - ID of the ling that wrapped
               :session-id  - Session tag (e.g., session:2026-01-14:ling-123)
               :created-ids - Collection of memory entry IDs created
               :stats       - Map of stats {:notes N :decisions N :conventions N}

   Returns:
     Transaction report"
  [wrap-id {:keys [agent-id session-id created-ids stats]}]
  {:pre [(string? wrap-id)]}
  (let [c (ensure-conn)]
    (d/transact! c
                 [(cond-> {:wrap-queue/id wrap-id
                           :wrap-queue/processed? false
                           :wrap-queue/created-at (now)}
                    agent-id (assoc :wrap-queue/agent-id agent-id)
                    session-id (assoc :wrap-queue/session-id session-id)
                    (seq created-ids) (assoc :wrap-queue/created-ids (vec created-ids))
                    stats (assoc :wrap-queue/stats stats))])))

(defn get-unprocessed-wraps
  "Get all wrap notifications not yet processed by coordinator.

   Returns:
     Seq of wrap notification maps"
  []
  (let [c (ensure-conn)]
    (d/q '[:find [(pull ?e [*]) ...]
           :where
           [?e :wrap-queue/processed? false]]
         @c)))

(defn mark-wrap-processed!
  "Mark a wrap notification as processed.

   Arguments:
     wrap-id - ID of the wrap notification to mark

   Returns:
     Transaction report or nil if wrap-id not found"
  [wrap-id]
  (let [c (ensure-conn)
        eid (d/q '[:find ?e .
                   :in $ ?id
                   :where [?e :wrap-queue/id ?id]]
                 @c wrap-id)]
    (when eid
      (d/transact! c
                   [[:db/add eid :wrap-queue/processed? true]]))))

;;; =============================================================================
;;; Change Plan Operations (dispatch_drone_wave)
;;; =============================================================================

(def plan-statuses
  "Valid change plan status values."
  #{:pending :in-progress :completed :failed})

(def item-statuses
  "Valid change item status values."
  #{:pending :dispatched :completed :failed})

(def wave-statuses
  "Valid wave status values."
  #{:running :completed :partial-failure})

(defn create-plan!
  "Create a new change plan with items.

   Arguments:
     tasks  - Collection of {:file \"path\" :task \"description\"}
     preset - Drone preset name (default: \"drone-worker\")

   Returns:
     The generated plan-id"
  [tasks preset]
  {:pre [(seq tasks)]}
  (let [c (ensure-conn)
        plan-id (gen-id "plan")
        plan-entity {:change-plan/id plan-id
                     :change-plan/status :pending
                     :change-plan/preset (or preset "drone-worker")
                     :change-plan/created-at (now)}
        item-entities (mapv (fn [{:keys [file task]}]
                              {:change-item/id (gen-id "item")
                               :change-item/plan [:change-plan/id plan-id]
                               :change-item/file file
                               :change-item/task task
                               :change-item/status :pending
                               :change-item/created-at (now)})
                            tasks)]
    (log/debug "Creating plan:" plan-id "with" (count tasks) "items")
    (d/transact! c (into [plan-entity] item-entities))
    plan-id))

(defn get-plan
  "Get a change plan by ID.

   Returns:
     Map with plan attributes or nil if not found"
  [plan-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:change-plan/id plan-id])]
      (-> (into {} e)
          (dissoc :db/id)))))

(defn get-pending-items
  "Get all pending items for a plan.

   Arguments:
     plan-id - Plan to get items for

   Returns:
     Seq of item maps with :pending status"
  [plan-id]
  (let [c (ensure-conn)
        db @c
        plan-eid (:db/id (d/entity db [:change-plan/id plan-id]))]
    (when plan-eid
      (let [eids (d/q '[:find [?e ...]
                        :in $ ?plan-eid
                        :where
                        [?e :change-item/plan ?plan-eid]
                        [?e :change-item/status :pending]]
                      db plan-eid)]
        (->> eids
             (map #(d/entity db %))
             (map (fn [e]
                    (-> (into {} e)
                        (dissoc :db/id)
                        (update :change-item/plan (constantly plan-id))))))))))

(defn get-plan-items
  "Get all items for a plan.

   Arguments:
     plan-id - Plan to get items for

   Returns:
     Seq of item maps"
  [plan-id]
  (let [c (ensure-conn)
        db @c
        plan-eid (:db/id (d/entity db [:change-plan/id plan-id]))]
    (when plan-eid
      (let [eids (d/q '[:find [?e ...]
                        :in $ ?plan-eid
                        :where
                        [?e :change-item/plan ?plan-eid]]
                      db plan-eid)]
        (->> eids
             (map #(d/entity db %))
             (map (fn [e]
                    (-> (into {} e)
                        (dissoc :db/id)
                        (update :change-item/plan (constantly plan-id))))))))))

(defn update-item-status!
  "Update a change item's status.

   Arguments:
     item-id - Item to update
     status  - New status
     opts    - Optional map with :drone-id :result

   Returns:
     Transaction report or nil if item not found"
  [item-id status & [{:keys [drone-id result]}]]
  {:pre [(contains? item-statuses status)]}
  (let [c (ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:change-item/id item-id]))]
      (let [tx-data (cond-> {:db/id eid
                             :change-item/status status}
                      drone-id (assoc :change-item/drone-id drone-id)
                      result (assoc :change-item/result result)
                      (#{:completed :failed} status) (assoc :change-item/completed-at (now)))]
        (log/debug "Updating item:" item-id "to status:" status)
        (d/transact! c [tx-data])))))

(defn update-plan-status!
  "Update a change plan's status.

   Arguments:
     plan-id - Plan to update
     status  - New status

   Returns:
     Transaction report or nil if plan not found"
  [plan-id status]
  {:pre [(contains? plan-statuses status)]}
  (let [c (ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:change-plan/id plan-id]))]
      (let [tx-data (cond-> {:db/id eid
                             :change-plan/status status}
                      (#{:completed :failed} status) (assoc :change-plan/completed-at (now)))]
        (log/debug "Updating plan:" plan-id "to status:" status)
        (d/transact! c [tx-data])))))

;;; =============================================================================
;;; Wave Operations
;;; =============================================================================

(defn create-wave!
  "Create a new wave execution for a plan.

   Arguments:
     plan-id     - Plan to execute
     concurrency - Max concurrent drones (default: 3)

   Returns:
     The generated wave-id"
  [plan-id & [{:keys [concurrency] :or {concurrency 3}}]]
  (let [c (ensure-conn)
        wave-id (gen-id "wave")]
    (d/transact! c [{:wave/id wave-id
                     :wave/plan [:change-plan/id plan-id]
                     :wave/concurrency concurrency
                     :wave/active-count 0
                     :wave/completed-count 0
                     :wave/failed-count 0
                     :wave/status :running
                     :wave/started-at (now)}])
    (log/info "Created wave:" wave-id "for plan:" plan-id)
    wave-id))

(defn get-wave
  "Get a wave by ID.

   Returns:
     Map with wave attributes or nil if not found"
  [wave-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:wave/id wave-id])]
      (-> (into {} e)
          (dissoc :db/id)
          (update :wave/plan #(when % (:change-plan/id %)))))))

(defn update-wave-counts!
  "Update wave execution counts.

   Arguments:
     wave-id - Wave to update
     delta   - Map with delta values {:active +1 :completed +1 :failed 0}

   Returns:
     Transaction report"
  [wave-id {:keys [active completed failed] :or {active 0 completed 0 failed 0}}]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:wave/id wave-id])]
      (let [eid (:db/id e)
            new-active (+ (or (:wave/active-count e) 0) active)
            new-completed (+ (or (:wave/completed-count e) 0) completed)
            new-failed (+ (or (:wave/failed-count e) 0) failed)]
        (d/transact! c [{:db/id eid
                         :wave/active-count new-active
                         :wave/completed-count new-completed
                         :wave/failed-count new-failed}])))))

(defn complete-wave!
  "Mark a wave as completed.

   Arguments:
     wave-id - Wave to complete
     status  - Final status (:completed or :partial-failure)

   Returns:
     Transaction report"
  [wave-id status]
  {:pre [(contains? #{:completed :partial-failure} status)]}
  (let [c (ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:wave/id wave-id]))]
      (log/info "Completing wave:" wave-id "with status:" status)
      (d/transact! c [{:db/id eid
                       :wave/status status
                       :wave/active-count 0
                       :wave/completed-at (now)}]))))
