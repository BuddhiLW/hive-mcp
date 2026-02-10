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
;; Internal Relations & Predicates (from logic.predicates)
;; =============================================================================

(def ^:private slave pred/slave)
(def ^:private task pred/task)
(def ^:private claims pred/claims)
(def ^:private task-files pred/task-files)
(def ^:private edit pred/edit)
(def ^:private edit-depends pred/edit-depends)

(def ^:private file-conflicto pred/file-conflicto)
(def ^:private would-deadlocko pred/would-deadlocko)

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

(defmacro ^:private with-db
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

(defn slave-exists?
  "Check if a slave exists in the database."
  [slave-id]
  (not (empty? (with-db
                 (l/run 1 [s]
                        (slave slave-id s))))))

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

;; =============================================================================
;; Edit Mutation Functions (for drone wave batching)
;; =============================================================================

(defn add-edit!
  "Register an edit for batch computation.
   edit-type: :create :modify :delete"
  [edit-id file-path edit-type]
  (swap! logic-db pldb/db-fact edit edit-id file-path edit-type)
  (log/debug "Added edit:" edit-id file-path edit-type))

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

(defn- reset-task-files!
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

;; =============================================================================
;; Debugging Helpers
;; =============================================================================

(defn- get-all-slaves
  "Get all registered slaves."
  []
  (with-db
    (l/run* [q]
            (l/fresh [id status]
                     (slave id status)
                     (l/== q {:slave-id id :status status})))))

(defn- get-all-tasks
  "Get all registered tasks."
  []
  (with-db
    (l/run* [q]
            (l/fresh [tid sid status]
                     (task tid sid status)
                     (l/== q {:task-id tid :slave-id sid :status status})))))

(defn db-stats
  "Get statistics about the current database state."
  []
  {:slaves (count (get-all-slaves))
   :tasks (count (get-all-tasks))
   :claims (count (get-all-claims))})
