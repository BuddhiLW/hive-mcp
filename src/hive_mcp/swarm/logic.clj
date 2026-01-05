(ns hive-mcp.swarm.logic
  "Logic programming engine for swarm hivemind coordination.

   Uses core.logic pldb (Prolog-style database) to track:
   - Slave state and ownership
   - Task state and dependencies
   - File claims for conflict detection

   Provides predicates for:
   - File conflict detection
   - Circular dependency / deadlock detection
   - Dependency readiness checks"
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Database Relations (pldb/db-rel)
;; =============================================================================

;; Slave entity: tracks slave state
;; slave-id: unique identifier (e.g., "swarm-worker-123")
;; status: :idle :working :spawning :starting :error
(pldb/db-rel slave ^:index slave-id status)

;; Task entity: tracks task ownership and state
;; task-id: unique identifier
;; slave-id: which slave owns this task
;; status: :dispatched :completed :timeout :error
(pldb/db-rel task ^:index task-id slave-id status)

;; File claim: which slave currently "owns" a file
;; file-path: absolute or relative path to file
;; slave-id: the slave working on this file
(pldb/db-rel claims ^:index file-path slave-id)

;; Task dependency: task-id depends on dep-task-id completing first
(pldb/db-rel depends-on ^:index task-id dep-task-id)

;; Task files: associates a task with files it operates on
;; Used for releasing claims when task completes
(pldb/db-rel task-files ^:index task-id file-path)

;; =============================================================================
;; Database State (Thread-Safe Atom)
;; =============================================================================

(defonce ^:private logic-db
  (atom (pldb/db)))

(defn reset-db!
  "Reset the logic database to empty state."
  []
  (reset! logic-db (pldb/db))
  (log/debug "Logic database reset"))

(defn get-db
  "Get current database (for debugging/testing)."
  []
  @logic-db)

(defmacro with-db
  "Execute a logic query against the current database."
  [& body]
  `(pldb/with-db @logic-db ~@body))

;; =============================================================================
;; Database Mutation Functions
;; =============================================================================

(defn add-slave!
  "Add a slave to the logic database."
  [slave-id status]
  (swap! logic-db pldb/db-fact slave slave-id status)
  (log/debug "Added slave to logic db:" slave-id status))

(defn update-slave-status!
  "Update a slave's status. Removes old fact, adds new."
  [slave-id new-status]
  ;; Find current status
  (let [current-status (first (with-db
                                (l/run 1 [s]
                                       (slave slave-id s))))]
    (when current-status
      (swap! logic-db pldb/db-retraction slave slave-id current-status))
    (swap! logic-db pldb/db-fact slave slave-id new-status)
    (log/debug "Updated slave status:" slave-id new-status)))

(defn remove-slave!
  "Remove a slave from the logic database."
  [slave-id]
  (let [current-status (first (with-db
                                (l/run 1 [s]
                                       (slave slave-id s))))]
    (when current-status
      (swap! logic-db pldb/db-retraction slave slave-id current-status)))
  (log/debug "Removed slave from logic db:" slave-id))

(defn slave-exists?
  "Check if a slave exists in the database."
  [slave-id]
  (not (empty? (with-db
                 (l/run 1 [s]
                        (slave slave-id s))))))

(defn add-task!
  "Add a task to the logic database."
  [task-id slave-id status]
  (swap! logic-db pldb/db-fact task task-id slave-id status)
  (log/debug "Added task to logic db:" task-id slave-id status))

(defn update-task-status!
  "Update a task's status."
  [task-id new-status]
  (let [current (first (with-db
                         (l/run 1 [q]
                                (l/fresh [slave status]
                                         (task task-id slave status)
                                         (l/== q {:slave slave :status status})))))]
    (when current
      (swap! logic-db pldb/db-retraction task task-id (:slave current) (:status current))
      (swap! logic-db pldb/db-fact task task-id (:slave current) new-status)
      (log/debug "Updated task status:" task-id new-status))))

(defn add-claim!
  "Add a file claim for a slave."
  [file-path slave-id]
  (swap! logic-db pldb/db-fact claims file-path slave-id)
  (log/debug "Added file claim:" file-path "→" slave-id))

(defn remove-claim!
  "Remove a specific file claim."
  [file-path slave-id]
  (swap! logic-db pldb/db-retraction claims file-path slave-id)
  (log/debug "Removed file claim:" file-path "→" slave-id))

(defn release-claims-for-slave!
  "Release all file claims for a slave."
  [slave-id]
  (let [files (with-db
                (l/run* [f]
                        (claims f slave-id)))]
    (doseq [f files]
      (remove-claim! f slave-id))
    (log/debug "Released" (count files) "claims for slave:" slave-id)))

(defn add-task-file!
  "Associate a file with a task (for claim tracking)."
  [task-id file-path]
  (swap! logic-db pldb/db-fact task-files task-id file-path))

(defn release-claims-for-task!
  "Release all file claims associated with a task."
  [task-id]
  (let [files (with-db
                (l/run* [f]
                        (task-files task-id f)))
        task-info (first (with-db
                           (l/run 1 [q]
                                  (l/fresh [slave status]
                                           (task task-id slave status)
                                           (l/== q {:slave slave})))))]
    (when task-info
      (doseq [f files]
        (remove-claim! f (:slave task-info))))
    (log/debug "Released task claims for:" task-id)))

(defn add-dependency!
  "Add a task dependency: task-id depends on dep-task-id."
  [task-id dep-task-id]
  (swap! logic-db pldb/db-fact depends-on task-id dep-task-id)
  (log/debug "Added dependency:" task-id "depends on" dep-task-id))

;; =============================================================================
;; Core Predicates (Logic Goals)
;; =============================================================================

(defn file-conflicto
  "Goal: succeeds if file-path is claimed by a DIFFERENT slave.

   Arguments:
   - file-path: the file being checked
   - requesting-slave: the slave wanting to claim the file
   - conflicting-slave: (output) the slave that has conflicting claim"
  [file-path requesting-slave conflicting-slave]
  (l/all
   (claims file-path conflicting-slave)
   (l/!= requesting-slave conflicting-slave)))

(defn task-completedo
  "Goal: succeeds if task-id has status :completed."
  [task-id]
  (l/fresh [slave-id]
           (task task-id slave-id :completed)))

(defn task-pendingo
  "Goal: succeeds if task-id is NOT completed."
  [task-id]
  (l/fresh [slave-id status]
           (task task-id slave-id status)
           (l/!= status :completed)))

;; =============================================================================
;; Circular Dependency Detection
;; =============================================================================

(defn reachable-fromo
  "Goal: succeeds if target is reachable from source via depends-on relation.
   This is the transitive closure of the dependency graph."
  [source target]
  (l/conde
    ;; Direct dependency
   [(depends-on source target)]
    ;; Transitive dependency
   [(l/fresh [mid]
             (depends-on source mid)
             (reachable-fromo mid target))]))

(defn would-deadlocko
  "Goal: succeeds if adding dependency from task-a to task-b would create a cycle.

   A cycle would exist if task-b can already reach task-a (meaning task-a
   somehow depends on task-b, so making task-a depend on task-b creates a loop)."
  [task-a task-b]
  (reachable-fromo task-b task-a))

;; =============================================================================
;; Query Functions (Public API)
;; =============================================================================

(defn check-file-conflicts
  "Check for file conflicts for a proposed set of files.
   Returns list of {:file path :held-by slave-id} conflicts."
  [requesting-slave files]
  (when (seq files)
    (with-db
      (l/run* [q]
              (l/fresh [file other-slave]
                       (l/membero file files)
                       (file-conflicto file requesting-slave other-slave)
                       (l/== q {:file file :held-by other-slave}))))))

(defn check-would-deadlock
  "Check if adding a dependency would create a circular dependency.
   Returns true if deadlock would occur."
  [task-id dep-task-id]
  (not (empty?
        (with-db
          (l/run 1 [q]
                 (would-deadlocko task-id dep-task-id)
                 (l/== q :cycle))))))

(defn check-dependencies-ready
  "Check if all dependencies of a task are completed.
   Returns {:ready? bool :pending [task-ids]}."
  [task-id]
  (let [pending (with-db
                  (l/run* [dep]
                          (depends-on task-id dep)
                          (task-pendingo dep)))]
    {:ready? (empty? pending)
     :pending (vec pending)}))

(defn get-all-claims
  "Get all current file claims. Returns [{:file path :slave-id id} ...]"
  []
  (with-db
    (l/run* [q]
            (l/fresh [f s]
                     (claims f s)
                     (l/== q {:file f :slave-id s})))))

(defn get-all-slaves
  "Get all registered slaves. Returns [{:slave-id id :status status} ...]"
  []
  (with-db
    (l/run* [q]
            (l/fresh [id status]
                     (slave id status)
                     (l/== q {:slave-id id :status status})))))

(defn get-all-tasks
  "Get all registered tasks. Returns [{:task-id id :slave-id sid :status s} ...]"
  []
  (with-db
    (l/run* [q]
            (l/fresh [tid sid status]
                     (task tid sid status)
                     (l/== q {:task-id tid :slave-id sid :status status})))))

;; =============================================================================
;; Debugging Helpers
;; =============================================================================

(defn db-stats
  "Get statistics about the current database state."
  []
  {:slaves (count (get-all-slaves))
   :tasks (count (get-all-tasks))
   :claims (count (get-all-claims))})

(defn dump-db
  "Dump the current database state for debugging."
  []
  {:slaves (get-all-slaves)
   :tasks (get-all-tasks)
   :claims (get-all-claims)})
