(ns hive-mcp.swarm.datascript.lings
  "Entity lifecycle operations for lings (slaves, tasks, claims).

   CRUD operations for:
   - Slave entities (add, update, remove)
   - Task entities (add, complete, fail)
   - Claim entities (claim, release, batch release)
   - Critical operations guard (kill protection)

   SOLID-S: Single Responsibility - entity lifecycle only.
   DDD: Repository pattern for swarm entities."
  (:require [datascript.core :as d]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.datascript.schema :as schema]
            [hive-mcp.swarm.datascript.connection :as conn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Forward declarations for functions referenced before definition
(declare release-claims-for-slave! release-claims-for-task!)

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
         (contains? schema/slave-statuses status)]}
  (let [c (conn/ensure-conn)
        tx-data (cond-> {:slave/id slave-id
                         :slave/name (or name slave-id)
                         :slave/status status
                         :slave/depth depth
                         :slave/tasks-completed 0
                         :slave/created-at (conn/now)}
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
  (let [c (conn/ensure-conn)
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
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:slave/id slave-id]))]
      ;; First release claims
      (release-claims-for-slave! slave-id)
      ;; Then retract entity
      (log/debug "Removing slave:" slave-id)
      (d/transact! c [[:db/retractEntity eid]]))))

;;; =============================================================================
;;; Critical Operations Guard (Kill Guard - ADR-003)
;;; =============================================================================

(defn enter-critical-op!
  "Mark a slave as being in a critical operation.
   Prevents swarm_kill from terminating this slave.

   Arguments:
     slave-id - Slave entering critical operation
     op-type  - Type of operation (:wrap :commit :dispatch)

   Returns:
     Transaction report or nil if slave not found"
  [slave-id op-type]
  {:pre [(contains? schema/critical-op-types op-type)]}
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:slave/id slave-id]))]
      (log/debug "Slave" slave-id "entering critical op:" op-type)
      (d/transact! c [[:db/add eid :slave/critical-ops op-type]]))))

(defn exit-critical-op!
  "Mark a slave as having completed a critical operation.

   Arguments:
     slave-id - Slave exiting critical operation
     op-type  - Type of operation (:wrap :commit :dispatch)

   Returns:
     Transaction report or nil if slave not found"
  [slave-id op-type]
  {:pre [(contains? schema/critical-op-types op-type)]}
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:slave/id slave-id]))]
      (log/debug "Slave" slave-id "exiting critical op:" op-type)
      (d/transact! c [[:db/retract eid :slave/critical-ops op-type]]))))

(defn get-critical-ops
  "Get current critical operations for a slave.

   Returns:
     Set of active critical operations, or empty set if none/not found"
  [slave-id]
  (let [c (conn/ensure-conn)
        db @c]
    (if-let [e (d/entity db [:slave/id slave-id])]
      (set (or (:slave/critical-ops e) []))
      #{})))

(defn can-kill?
  "Check if a slave can be killed safely.
   Returns false if slave has any critical operations in progress.

   Arguments:
     slave-id - Slave to check

   Returns:
     {:can-kill? bool :blocking-ops #{...}}"
  [slave-id]
  (let [ops (get-critical-ops slave-id)]
    (if (empty? ops)
      {:can-kill? true :blocking-ops #{}}
      {:can-kill? false :blocking-ops ops})))

(defmacro with-critical-op
  "Execute body while holding a critical operation guard.
   Ensures the critical op is properly released even on exception.

   Usage:
     (with-critical-op slave-id :wrap
       (do-wrap-stuff))

   CLARITY: Y - Yield safe failure with proper cleanup"
  [slave-id op-type & body]
  `(do
     (enter-critical-op! ~slave-id ~op-type)
     (try
       ~@body
       (finally
         (exit-critical-op! ~slave-id ~op-type)))))

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
         (contains? schema/task-statuses status)]}
  (let [c (conn/ensure-conn)
        tid (or task-id (conn/gen-id "task"))
        tx-data (cond-> {:task/id tid
                         :task/slave [:slave/id slave-id]
                         :task/status status
                         :task/started-at (conn/now)}
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
  (let [c (conn/ensure-conn)
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
                                 :task/completed-at (conn/now)}
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
  (let [c (conn/ensure-conn)
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
                                 :task/completed-at (conn/now)}]
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
  (let [c (conn/ensure-conn)
        tx-data (cond-> {:claim/file file-path
                         :claim/slave [:slave/id slave-id]
                         :claim/created-at (conn/now)}
                  task-id (assoc :claim/task [:task/id task-id]))]
    (log/debug "Claiming file:" file-path "for slave:" slave-id)
    (d/transact! c [tx-data])))

(defn release-claim!
  "Release a file claim and dispatch :claim/file-released event.

   When a claim is released, the event system notifies any lings
   that were waiting for access to this file (file-claim event cascade).

   Arguments:
     file-path - Path to release

   Returns:
     Transaction report or nil if claim not found"
  [file-path]
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:claim/file file-path]))]
      (log/debug "Releasing claim:" file-path)
      (let [result (d/transact! c [[:db/retractEntity eid]])]
        ;; Dispatch event for file-claim cascade (impl-1 handles this)
        ;; Use requiring-resolve to avoid cyclic dependency with events.core
        ;; Only dispatch if handler is registered (avoids error during bootstrap)
        (when-let [handler-registered? (requiring-resolve 'hive-mcp.events.core/handler-registered?)]
          (when (handler-registered? :claim/file-released)
            (let [dispatch (requiring-resolve 'hive-mcp.events.core/dispatch)]
              (dispatch [:claim/file-released {:file file-path}]))))
        result))))

(defn release-claims-for-slave!
  "Release all file claims held by a slave.

   Arguments:
     slave-id - Slave whose claims to release

   Returns:
     Number of claims released"
  [slave-id]
  (let [c (conn/ensure-conn)
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
  (let [c (conn/ensure-conn)
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
