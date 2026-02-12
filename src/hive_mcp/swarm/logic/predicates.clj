(ns hive-mcp.swarm.logic.predicates
  "Pure core.logic predicates and relations for swarm coordination.

   This module contains:
   1. Database Relations (pldb/db-rel) - Schema definitions
   2. Core Predicates - Pure logic goals

   CLARITY Compliance:
   - C (Composition): Predicates compose via core.logic conde/all
   - L (Layers): Pure logic, no side effects or state
   - R (Represented Intent): Clear predicate naming with -o suffix

   Naming Convention:
   - Predicates end in -o (core.logic convention for goals)
   - Relations are nouns (slave, task, claims, etc.)

   See Also:
   - swarm/logic.clj - Stateful operations using these predicates"
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
;; Edit Relations (for drone wave batch computation)
;; =============================================================================

;; Edit entity: represents a planned file mutation in a wave
;; edit-id: unique identifier (e.g., "edit-123")
;; file-path: the file being edited
;; edit-type: :create :modify :delete
(pldb/db-rel edit ^:index edit-id file-path edit-type)

;; Edit dependencies: edit-a must complete before edit-b
;; (typically inferred from file read/write patterns)
(pldb/db-rel edit-depends ^:index edit-a edit-b)

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
;; Edit Predicates (for drone wave batching)
;; =============================================================================

(defn edit-conflicto
  "Goal: succeeds if two edits conflict (same file, different edit-id).
   Returns the conflicting file path."
  [edit-a edit-b file]
  (l/all
   (edit edit-a file (l/lvar))
   (edit edit-b file (l/lvar))
   (l/!= edit-a edit-b)))

(defn edit-depends-on-o
  "Goal: succeeds if edit-a must complete before edit-b (direct dependency)."
  [edit-a edit-b]
  (edit-depends edit-a edit-b))

(defn edit-reachable-fromo
  "Goal: succeeds if edit-b is reachable from edit-a via edit-depends.
   Transitive closure of edit dependencies."
  [source target]
  (l/conde
   [(edit-depends source target)]
   [(l/fresh [mid]
             (edit-depends source mid)
             (edit-reachable-fromo mid target))]))
