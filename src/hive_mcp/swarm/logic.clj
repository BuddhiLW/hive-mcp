(ns hive-mcp.swarm.logic
  "Logic programming engine for swarm hivemind coordination.

   WHY CORE.LOGIC (vs DataScript)?
   ===============================
   This module uses core.logic pldb (Prolog-style in-memory database) for
   **declarative constraint queries** that would be awkward in DataScript:

   1. Transitive closure (reachability) - deadlock detection via reachable-fromo
   2. Negation-as-failure - conflict detection (file claimed by DIFFERENT slave)
   3. Batch computation - Kahn's algorithm with logical conflict grouping

   DataScript (swarm/datascript.clj) handles **entity persistence**:
   - CRUD operations on slaves, tasks, coordinators, wraps
   - Datomic-style pull queries for entity attributes
   - Transaction history and listeners

   WHEN TO USE WHICH:
   - Need to store/query entity state? → DataScript
   - Need transitive/recursive queries? → core.logic (this module)
   - Need conflict/constraint checking? → core.logic (this module)

   RELATIONS (pldb/db-rel)
   =======================
   Re-exported from logic.predicates:
   - slave:       (slave-id, status) - Worker agent state
   - task:        (task-id, slave-id, status) - Task ownership
   - claims:      (file-path, slave-id) - File ownership for conflict detection
   - depends-on:  (task-id, dep-task-id) - Task dependencies
   - task-files:  (task-id, file-path) - Files associated with task
   - edit:        (edit-id, file-path, type) - Drone wave batch planning
   - edit-depends: (edit-a, edit-b) - Edit ordering constraints

   KEY PREDICATES
   ==============
   Re-exported from logic.predicates:
   - file-conflicto:    Does another slave own this file?
   - would-deadlocko:   Would adding this dependency create a cycle?
   - reachable-fromo:   Transitive closure of dependency graph

   BATCH COMPUTATION
   =================
   compute-batches: Modified Kahn's algorithm that groups edits into
   parallel-safe batches respecting both dependencies AND file conflicts.

   THREAD SAFETY
   =============
   All mutations go through atom swap! operations. For atomic check+claim,
   use coordinator/atomic-claim-files! which locks the logic-db atom.

   SEE ALSO
   ========
   - swarm/logic/predicates.clj - Pure relations and predicates
   - swarm/coordinator.clj - High-level API using this module
   - swarm/datascript.clj  - Entity state management
   - tools/swarm/wave.clj  - Batch execution using compute-batches"
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.logic.predicates :as pred]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports from logic.predicates (Relations)
;; =============================================================================

(def slave pred/slave)
(def task pred/task)
(def claims pred/claims)
(def depends-on pred/depends-on)
(def task-files pred/task-files)
(def edit pred/edit)
(def edit-depends pred/edit-depends)

;; =============================================================================
;; Re-exports from logic.predicates (Predicates)
;; =============================================================================

(def file-conflicto pred/file-conflicto)
(def task-completedo pred/task-completedo)
(def task-pendingo pred/task-pendingo)
(def reachable-fromo pred/reachable-fromo)
(def would-deadlocko pred/would-deadlocko)
(def edit-conflicto pred/edit-conflicto)
(def edit-depends-on-o pred/edit-depends-on-o)
(def edit-reachable-fromo pred/edit-reachable-fromo)

;; =============================================================================
;; Database State (Thread-Safe Atom)
;; =============================================================================

(defonce ^:private logic-db
  (atom (pldb/db)))

(defn get-logic-db-atom
  "Get the logic-db atom for external locking.

   Used by coordinator.clj for atomic check+claim operations.
   DO NOT use for direct manipulation - use the provided mutation functions instead."
  []
  logic-db)

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

(defn get-all-claims
  "Get all file claims from logic-db.
   Returns vector of {:file path :slave-id id} maps."
  []
  (vec (with-db
         (l/run* [q]
                 (l/fresh [f s]
                          (claims f s)
                          (l/== q {:file f :slave-id s}))))))

(defn reset-claims!
  "Clear ALL file claims from logic-db.

   GHOST CLAIMS FIX: Use this to clear orphaned claims that weren't
   properly released due to drone failures or server restarts.

   WARNING: This removes ALL claims - only use when cleaning up after
   wave failures or during maintenance."
  []
  (let [all-claims (get-all-claims)
        count-before (count all-claims)]
    (doseq [{:keys [file slave-id]} all-claims]
      (swap! logic-db pldb/db-retraction claims file slave-id))
    (log/info "Reset all claims:" count-before "claims cleared from logic-db")
    {:cleared count-before}))

(defn add-task-file!
  "Associate a file with a task (for claim tracking)."
  [task-id file-path]
  (swap! logic-db pldb/db-fact task-files task-id file-path))

(defn get-files-for-task
  "Get all files associated with a task.
   Returns vector of file paths."
  [task-id]
  (vec (with-db
         (l/run* [f]
                 (task-files task-id f)))))

(defn release-claims-for-task!
  "Release all file claims associated with a task.

   Gets slave-id from claims relation directly, not from task relation.
   This is necessary because atomic-claim-files! populates task-files and claims
   but NOT the task relation."
  [task-id]
  (let [files (with-db
                (l/run* [f]
                        (task-files task-id f)))
        released-count (atom 0)]
    ;; For each file, find and remove its claim from claims relation directly
    (doseq [f files]
      (let [slave-id (first (with-db
                              (l/run 1 [s]
                                     (claims f s))))]
        (when slave-id
          (remove-claim! f slave-id)
          (swap! released-count inc))))
    (log/debug "Released" @released-count "claims for task:" task-id)))

(defn add-dependency!
  "Add a task dependency: task-id depends on dep-task-id."
  [task-id dep-task-id]
  (swap! logic-db pldb/db-fact depends-on task-id dep-task-id)
  (log/debug "Added dependency:" task-id "depends on" dep-task-id))

;; =============================================================================
;; Edit Mutation Functions (for drone wave batching)
;; =============================================================================

(defn add-edit!
  "Register an edit for batch computation.
   edit-type: :create :modify :delete"
  [edit-id file-path edit-type]
  (swap! logic-db pldb/db-fact edit edit-id file-path edit-type)
  (log/debug "Added edit:" edit-id file-path edit-type))

(defn remove-edit!
  "Remove an edit from the database."
  [edit-id]
  (let [edit-info (first (with-db
                           (l/run 1 [q]
                                  (l/fresh [file type]
                                           (edit edit-id file type)
                                           (l/== q {:file file :type type})))))]
    (when edit-info
      (swap! logic-db pldb/db-retraction edit edit-id (:file edit-info) (:type edit-info))))
  (log/debug "Removed edit:" edit-id))

(defn add-edit-dependency!
  "Add a dependency: edit-a must complete before edit-b."
  [edit-a edit-b]
  (swap! logic-db pldb/db-fact edit-depends edit-a edit-b)
  (log/debug "Added edit dependency:" edit-a "→" edit-b))

(defn reset-edits!
  "Clear all edit relations (for cleanup after wave completes)."
  []
  (let [all-edits (with-db
                    (l/run* [q]
                            (l/fresh [id file type]
                                     (edit id file type)
                                     (l/== q {:id id :file file :type type}))))
        all-deps (with-db
                   (l/run* [q]
                           (l/fresh [a b]
                                    (edit-depends a b)
                                    (l/== q {:a a :b b}))))]
    (doseq [{:keys [id file type]} all-edits]
      (swap! logic-db pldb/db-retraction edit id file type))
    (doseq [{:keys [a b]} all-deps]
      (swap! logic-db pldb/db-retraction edit-depends a b))
    (log/debug "Reset edits:" (count all-edits) "edits," (count all-deps) "dependencies")))

(defn reset-task-files!
  "Clear all task-file associations.

   CRITICAL: This must be called after wave/task completion to prevent memory leak.
   The task-files relation grows unbounded if not cleaned up, as release-claims-for-task!
   only releases claims but not the task-files associations themselves."
  []
  (let [all-task-files (with-db
                         (l/run* [q]
                                 (l/fresh [tid fpath]
                                          (task-files tid fpath)
                                          (l/== q {:task-id tid :file fpath}))))]
    (doseq [{:keys [task-id file]} all-task-files]
      (swap! logic-db pldb/db-retraction task-files task-id file))
    (log/debug "Reset task-files:" (count all-task-files) "associations cleared")))

(defn reset-all-transient!
  "Reset all transient data (edits + task-files).

   Call this at wave completion to ensure clean state for next wave.
   Combines reset-edits! and reset-task-files! into a single cleanup operation."
  []
  (reset-edits!)
  (reset-task-files!))

;; =============================================================================
;; Batch Computation (Kahn's algorithm with conflict grouping)
;; =============================================================================

(defn- get-edit-file
  "Get the file path for an edit-id."
  [edit-id]
  (first (with-db
           (l/run 1 [file]
                  (l/fresh [type]
                           (edit edit-id file type))))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- get-edit-dependencies
  "Get all edits that edit-id depends on."
  [edit-id]
  (with-db
    (l/run* [dep]
            (edit-depends dep edit-id))))

(defn- get-all-edit-dependencies
  "Get all edit dependency pairs."
  []
  (with-db
    (l/run* [q]
            (l/fresh [a b]
                     (edit-depends a b)
                     (l/== q [a b])))))

(defn- edits-conflict?
  "Check if two edits conflict (same file)."
  [edit-a edit-b]
  (let [file-a (get-edit-file edit-a)
        file-b (get-edit-file edit-b)]
    (and file-a file-b (= file-a file-b))))

(defn- can-add-to-batch?
  "Check if an edit can be added to a batch (no conflicts with existing batch members)."
  [edit-id batch]
  (not (some #(edits-conflict? edit-id %) batch)))

(defn compute-batches
  "Compute safe parallel batches for edits.

   Returns vector of batches: [[edit-ids-batch-1] [edit-ids-batch-2] ...]
   - Edits within a batch can run in parallel (no file conflicts)
   - Batches must run sequentially (dependencies respected)

   Algorithm: Modified Kahn's topological sort with conflict grouping.
   1. Build in-degree map from edit-depends relation
   2. Process edits with in-degree 0 (no unsatisfied dependencies)
   3. Group into batch only if no file conflicts with batch members
   4. When batch full or no more non-conflicting edits, start new batch
   5. Decrement in-degree of dependents when batch completes"
  [edit-ids]
  (if (empty? edit-ids)
    []
    (let [edit-set (set edit-ids)
          deps (get-all-edit-dependencies)
          ;; Filter to only deps within our edit set
          relevant-deps (filter (fn [[a b]] (and (edit-set a) (edit-set b))) deps)

          ;; Build in-degree map
          initial-in-degree (reduce (fn [m id] (assoc m id 0)) {} edit-ids)
          in-degree (reduce (fn [m [_ b]] (update m b (fnil inc 0)))
                            initial-in-degree
                            relevant-deps)

          ;; Build adjacency list (who depends on whom)
          adj (reduce (fn [m [a b]] (update m a (fnil conj []) b))
                      {}
                      relevant-deps)]

      ;; Kahn's algorithm with conflict grouping
      (loop [remaining-in-degree in-degree
             batches []]
        (if (every? (fn [[_ v]] (pos? v)) remaining-in-degree)
          ;; All remaining have dependencies - check for cycle or done
          (if (empty? remaining-in-degree)
            batches
            (do
              (log/warn "Cycle detected in edit dependencies, forcing remaining:"
                        (keys remaining-in-degree))
              ;; Force remaining into sequential batches
              (into batches (mapv vector (keys remaining-in-degree)))))

          ;; Find edits with in-degree 0 (ready to execute)
          (let [ready (mapv first (filter (fn [[_ v]] (zero? v)) remaining-in-degree))]
            (if (empty? ready)
              batches
              ;; Group ready edits into batches by conflict
              (let [;; Greedy: add non-conflicting edits to current batch
                    current-batch (reduce
                                   (fn [batch edit-id]
                                     (if (can-add-to-batch? edit-id batch)
                                       (conj batch edit-id)
                                       batch))
                                   []
                                   ready)
                    ;; Remove batch members from in-degree map
                    _batch-set (set current-batch)
                    new-in-degree (reduce dissoc remaining-in-degree current-batch)
                    ;; Decrement in-degree of dependents
                    final-in-degree (reduce
                                     (fn [m completed-id]
                                       (reduce (fn [m' dep-id]
                                                 (if (contains? m' dep-id)
                                                   (update m' dep-id dec)
                                                   m'))
                                               m
                                               (get adj completed-id [])))
                                     new-in-degree
                                     current-batch)]
                (recur final-in-degree (conj batches current-batch))))))))))

(defn get-all-edits
  "Get all registered edits. Returns [{:edit-id :file :type} ...]"
  []
  (with-db
    (l/run* [q]
            (l/fresh [id file type]
                     (edit id file type)
                     (l/== q {:edit-id id :file file :type type})))))

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

(defn get-claim-for-file
  "Get claim info for a specific file path.
   Returns {:file path :slave-id id} or nil if not claimed."
  [file-path]
  (first
   (with-db
     (l/run 1 [q]
            (l/fresh [s]
                     (claims file-path s)
                     (l/== q {:file file-path :slave-id s}))))))

(defn release-claim-for-file!
  "Release a claim for a specific file path, regardless of owner.
   Returns true if claim was released, false if file wasn't claimed."
  [file-path]
  (if-let [claim (get-claim-for-file file-path)]
    (do
      (remove-claim! file-path (:slave-id claim))
      true)
    false))

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
;; Dispatch Readiness (Race Condition Fix)
;; =============================================================================
;; These functions ensure dispatch waits for preset completion.
;; A slave is only ready for dispatch when status is :idle (preset loaded).
;; Statuses :spawning and :starting indicate preset is still loading.

(def ^:private dispatch-ready-statuses
  "Slave statuses that indicate readiness for dispatch.
   :idle means the slave has completed initialization and preset loading."
  #{:idle})

(def ^:private dispatch-not-ready-statuses
  "Slave statuses that indicate the slave is NOT ready for dispatch.
   :spawning - slave terminal is being created
   :starting - slave process is starting, preset loading in progress"
  #{:spawning :starting})

(defn get-slave-status
  "Get the current status of a slave.
   Returns status keyword or nil if slave doesn't exist."
  [slave-id]
  (first (with-db
           (l/run 1 [s]
                  (slave slave-id s)))))

(defn slave-ready-for-dispatch?
  "Check if a slave is ready to receive dispatch.

   Returns true only if:
   - Slave exists in logic-db
   - Slave status is :idle (preset fully loaded)

   DISPATCH RACE CONDITION FIX:
   This predicate prevents dispatch from proceeding while preset is loading.
   When a slave is spawned, it starts in :spawning status. The Elisp layer
   updates status to :idle when the terminal is ready and preset is loaded.
   Dispatching to a :spawning or :starting slave may result in prompts
   being lost or executed without the proper system prompt context."
  [slave-id]
  (let [status (get-slave-status slave-id)]
    (contains? dispatch-ready-statuses status)))

(defn check-dispatch-readiness
  "Check if a slave is ready for dispatch with detailed status.

   Returns map with:
     :ready?  - true if slave can receive dispatch
     :status  - current slave status (or nil if not found)
     :reason  - nil if ready, keyword reason if not:
                :slave-not-found - slave doesn't exist
                :preset-loading  - slave is :spawning or :starting
                :slave-busy      - slave is :working on another task

   USAGE IN PRE-FLIGHT:
   Call this before dispatch-or-queue! to add readiness check:

   (let [readiness (logic/check-dispatch-readiness slave-id)]
     (if (:ready? readiness)
       (dispatch! ...)
       (handle-not-ready readiness)))"
  [slave-id]
  (let [status (get-slave-status slave-id)]
    (cond
      ;; Slave doesn't exist
      (nil? status)
      {:ready? false
       :status nil
       :reason :slave-not-found}

      ;; Slave is ready (idle)
      (contains? dispatch-ready-statuses status)
      {:ready? true
       :status status
       :reason nil}

      ;; Slave is spawning or starting (preset loading)
      (contains? dispatch-not-ready-statuses status)
      {:ready? false
       :status status
       :reason :preset-loading}

      ;; Slave is busy (working, blocked, etc.)
      :else
      {:ready? false
       :status status
       :reason :slave-busy})))

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
