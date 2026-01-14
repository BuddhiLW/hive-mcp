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
   {:db/doc "Timestamp when claim was created"}})

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
