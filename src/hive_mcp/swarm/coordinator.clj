(ns hive-mcp.swarm.coordinator
  "High-level coordination API for swarm hivemind.

   USES CORE.LOGIC (not DataScript)
   ================================
   This module delegates to swarm/logic.clj for conflict detection
   and claim management. Core.logic pldb provides:

   - check-file-conflicts: Prolog-style query for files owned by OTHER slaves
   - atomic-claim-files!:  Atomic check+claim to prevent race conditions
   - check-would-deadlock: Transitive dependency cycle detection

   WHY NOT DATASCRIPT HERE?
   DataScript is for entity CRUD. This module needs logical predicates:
   'Is file X claimed by a slave OTHER than Y?' requires negation-as-failure,
   which is natural in Prolog/core.logic but awkward in Datalog.

   KEY FUNCTIONS
   =============
   - pre-flight-check:     Check conflicts/deadlocks before dispatch
   - atomic-claim-files!:  Race-free claim acquisition (uses locking)
   - dispatch-or-queue!:   Unified dispatch entry point
   - process-queue!:       Re-check queued tasks when claims release

   FILE CLAIMS FLOW
   ================
   1. swarm_dispatch called with files list
   2. pre-flight-check queries logic/check-file-conflicts
   3. If no conflicts: atomic-claim-files! claims in logic-db
   4. Task executes...
   5. On completion: release-task-claims! removes from logic-db
   6. process-queue! re-checks waiting tasks

   SEE ALSO
   ========
   - swarm/logic.clj     - Core.logic predicates and claims storage
   - swarm/datascript.clj - Entity state (slaves, tasks, coordinators)"
  (:require [hive-mcp.swarm.logic :as logic]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.tools.swarm.claim :as claim-tools]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Task Queue (for conflicting tasks that must wait)
;; =============================================================================

(defonce ^:private task-queue
  (atom []))

(defn queue-task!
  "Add a task to the waiting queue.
   Task spec: {:id :slave-id :prompt :files :queued-at :timeout-ms}"
  [task-spec]
  (let [task (assoc task-spec
                    :id (or (:id task-spec) (str (java.util.UUID/randomUUID)))
                    :queued-at (System/currentTimeMillis))]
    (swap! task-queue conj task)
    (log/info "Queued task" (:id task) "for slave" (:slave-id task)
              "- waiting for files:" (:files task))
    (:id task)))

(defn get-queue
  "Get the current task queue."
  []
  @task-queue)

(defn clear-queue!
  "Clear all queued tasks."
  []
  (reset! task-queue [])
  (log/info "Task queue cleared"))

(defn remove-from-queue!
  "Remove a specific task from the queue."
  [task-id]
  (swap! task-queue (fn [q] (vec (remove #(= (:id %) task-id) q))))
  (log/debug "Removed task from queue:" task-id))

(defn get-ready-tasks
  "Get tasks whose conflicts have cleared.
   Returns list of task specs ready for dispatch."
  []
  (let [queue @task-queue]
    (filter (fn [task]
              (empty? (logic/check-file-conflicts
                       (:slave-id task)
                       (:files task))))
            queue)))

;; =============================================================================
;; Heuristic File Extraction
;; =============================================================================

(def ^:private file-patterns
  "Regex patterns for extracting file paths from prompts."
  [;; Explicit file operations: "edit src/foo.clj", "modify core.clj"
   #"(?i)(?:edit|modify|update|create|delete|read|write|open)\s+([^\s,]+\.[a-zA-Z0-9]+)"
   ;; Backtick quoted: `src/foo.clj`
   #"`([^`]+\.[a-zA-Z0-9]+)`"
   ;; Double quoted paths: "src/foo.clj"
   #"\"([^\"]+\.[a-zA-Z0-9]+)\""
   ;; File path patterns: /absolute/path/file.ext or relative/path/file.ext
   #"(?:^|\s)(/[^\s]+\.[a-zA-Z0-9]+)"
   #"(?:^|\s)((?:\./|\.\./)[\w./-]+\.[a-zA-Z0-9]+)"])

(defn extract-files-from-prompt
  "Heuristically extract file paths from a task prompt.
   Returns a vector of unique file paths."
  [prompt]
  (when (string? prompt)
    (->> file-patterns
         (mapcat #(re-seq % prompt))
         (map second)
         (filter some?)
         ;; Filter out obvious non-files
         (filter #(re-matches #"[\w./_-]+" %))
         ;; Filter out URLs
         (remove #(str/starts-with? % "http"))
         distinct
         vec)))

;; =============================================================================
;; Atomic Claim Acquisition (Race-Free)
;; =============================================================================

(defn atomic-claim-files!
  "Atomically check for conflicts AND acquire claims.

   CRITICAL: This prevents the race condition where:
   1. Drone A checks files - no conflicts
   2. Drone B checks same files - no conflicts
   3. Drone A claims files
   4. Drone B claims same files (conflict not detected!)

   Uses locking on logic-db to ensure check+claim is atomic.

   DUAL DB SYNC FIX: Ensures slave is registered in core.logic pldb BEFORE
   claiming files. This prevents race conditions where drones are in DataScript
   but not in logic-db, causing ghost claims and conflict detection failures.

   CC.3 PRIOR HASH: Computes file content hash at acquire time and stores it
   in DataScript claim record. This enables change detection on release.

   Arguments:
   - task-id:  Unique task identifier
   - slave-id: Slave/drone claiming the files
   - files:    List of file paths to claim

   Returns:
   {:acquired? bool    - true if all files were claimed
    :conflicts [...]   - list of {:file path :held-by slave-id} if conflicts
    :files-claimed N}  - count of files claimed (0 if conflicts)"
  [task-id slave-id files]
  (when (seq files)
    (let [lock-obj (logic/get-logic-db-atom)]
      (locking lock-obj
        ;; DUAL DB SYNC: Register slave in logic-db FIRST if not present
        ;; This ensures conflict detection works correctly
        (when-not (logic/slave-exists? slave-id)
          (logic/add-slave! slave-id :active)
          (log/debug "Registered slave in logic-db during claim:" slave-id))
        (let [conflicts (logic/check-file-conflicts slave-id files)]
          (if (seq conflicts)
            {:acquired? false
             :conflicts (vec conflicts)
             :files-claimed 0}
            (do
              (doseq [f files]
                ;; Core.logic claims for conflict detection
                (logic/add-claim! f slave-id)
                (logic/add-task-file! task-id f)
                (claim-tools/record-claim-timestamp! f slave-id)
                ;; CC.3: Compute prior-hash and store in DataScript
                (let [prior-hash (when (.exists (io/file f))
                                   (:hash (disc/file-content-hash f)))]
                  (lings/claim-file! f slave-id {:task-id task-id
                                                 :prior-hash prior-hash})))
              {:acquired? true
               :conflicts []
               :files-claimed (count files)})))))))

;; =============================================================================
;; Pre-Flight Check API
;; =============================================================================

(defn pre-flight-check
  "Run all pre-flight checks before dispatch.

   Arguments:
   - slave-id: the slave that will execute the task
   - files: explicit list of files (optional)
   - prompt: task prompt (used for heuristic extraction if files empty)
   - dependencies: list of [task-id dep-task-id] pairs (optional)

   Returns:
   {:approved? bool     - true if dispatch can proceed
    :conflicts []       - list of file conflicts
    :queue? bool        - true if task should be queued
    :would-deadlock []  - list of dependency pairs that would deadlock
    :extracted-files [] - files extracted from prompt (if used)}"
  [{:keys [slave-id files prompt dependencies]}]
  (let [;; Use explicit files or extract from prompt
        effective-files (if (seq files)
                          files
                          (extract-files-from-prompt prompt))
        extracted? (and (empty? files) (seq effective-files))

        ;; Check file conflicts
        file-conflicts (logic/check-file-conflicts slave-id effective-files)

        ;; Check for circular dependencies
        deadlock-pairs (when (seq dependencies)
                         (->> dependencies
                              (filter (fn [[task-id dep-id]]
                                        (logic/check-would-deadlock task-id dep-id)))
                              vec))

        ;; Determine outcome
        has-conflicts? (seq file-conflicts)
        has-deadlocks? (seq deadlock-pairs)
        approved? (and (not has-conflicts?) (not has-deadlocks?))]

    (when has-conflicts?
      (log/warn "Pre-flight: file conflicts detected for" slave-id ":" file-conflicts))
    (when has-deadlocks?
      (log/warn "Pre-flight: circular dependencies detected:" deadlock-pairs))

    {:approved? approved?
     :conflicts (vec file-conflicts)
     :queue? has-conflicts?  ; Queue if file conflicts (can clear later)
     :would-deadlock (vec (or deadlock-pairs []))
     :extracted-files (when extracted? effective-files)}))

;; =============================================================================
;; Claim Management
;; =============================================================================

(defn contextual-claim-release!
  "CC.4: Release a file claim with change detection and staleness propagation.
   CC.6: Archive to claim-history when file changed during claim period.

   Compares the prior-hash (captured at acquire time via CC.3) with the
   current file content hash. If the content changed during the claim period,
   propagates staleness to dependent KG entries via disc/propagate-staleness!.

   This enables the knowledge graph to track which files were modified by
   which drones, and automatically marks dependent knowledge as potentially
   stale when source files change.

   Arguments:
     file-path - Path of the file to release

   Returns:
     {:released? bool
      :changed?  bool    - true if file content changed during claim
      :propagated {...}  - staleness propagation result if changed
      :archived?  bool   - true if archived to claim-history (CC.6)}"
  [file-path]
  (let [claim-info (lings/get-claim-info file-path)]
    (if-not claim-info
      {:released? false :reason :no-claim}
      (let [prior-hash (:prior-hash claim-info)
            slave-id (:slave-id claim-info)
            ;; Compute current hash
            {:keys [hash exists?]} (disc/file-content-hash file-path)
            changed? (and exists?
                          prior-hash
                          hash
                          (not= prior-hash hash))]
        ;; CC.5: Wire to propagate-staleness! when content changed
        (let [propagation-result
              (when changed?
                (log/info "File changed during claim, propagating staleness"
                          {:file file-path
                           :prior-hash (when prior-hash (subs prior-hash 0 8))
                           :current-hash (when hash (subs hash 0 8))})
                (disc/propagate-staleness! file-path
                                           (:hash-mismatch disc/base-staleness-values)
                                           :hash-mismatch))
              ;; CC.6: Archive to claim-history when changed
              archived? (when changed?
                          (lings/archive-claim-to-history!
                           file-path
                           {:slave-id slave-id
                            :prior-hash prior-hash
                            :released-hash hash})
                          true)]
          ;; Release the claim (also releases from logic-db via lings)
          (lings/release-claim! file-path)
          (claim-tools/remove-claim-timestamp! file-path)
          {:released? true
           :changed? (boolean changed?)
           :propagated propagation-result
           :archived? (boolean archived?)})))))

(defn register-task-claims!
  "Register file claims for a dispatched task.
   Call this after successful dispatch.

   DUAL DB SYNC: Ensures slave is registered in logic-db before claiming."
  [task-id slave-id files]
  (when (seq files)
    ;; DUAL DB SYNC: Register slave in logic-db FIRST if not present
    (when-not (logic/slave-exists? slave-id)
      (logic/add-slave! slave-id :active)
      (log/debug "Registered slave in logic-db during task claim:" slave-id))
    (doseq [f files]
      (logic/add-claim! f slave-id)
      (logic/add-task-file! task-id f)
      (claim-tools/record-claim-timestamp! f slave-id))
    (log/info "Registered" (count files) "file claims for task" task-id)))

(defn release-task-claims!
  "Release file claims when a task completes.
   Call this on task completion/failure.

   CC.4/CC.5: Uses contextual-claim-release! to detect file changes and
   propagate staleness to the knowledge graph when files were modified
   during the claim period.

   Returns:
     {:released int :changed int :propagated int}"
  [task-id]
  ;; Get files before releasing
  (let [files (logic/get-files-for-task task-id)
        results (doall
                 (for [f files]
                   (let [result (contextual-claim-release! f)]
                     (assoc result :file f))))]
    ;; Release claims in logic-db (core.logic side)
    (logic/release-claims-for-task! task-id)
    ;; Summarize results
    (let [released-count (count (filter :released? results))
          changed-count (count (filter :changed? results))
          propagated-count (count (filter :propagated results))]
      (log/info "Released" released-count "claims for task" task-id
                (when (pos? changed-count)
                  (str "(" changed-count " files changed, staleness propagated)")))
      {:released released-count
       :changed changed-count
       :propagated propagated-count
       :details results})))

;; =============================================================================
;; Queue Processing (call periodically or on events)
;; =============================================================================

(defn process-queue!
  "Check queued tasks and return those ready for dispatch.
   Removes ready tasks from queue.
   Returns list of task specs to dispatch."
  []
  (let [ready (get-ready-tasks)]
    (when (seq ready)
      (doseq [task ready]
        (remove-from-queue! (:id task)))
      (log/info "Queue processing: " (count ready) "tasks ready for dispatch"))
    ready))

;; =============================================================================
;; Convenience API
;; =============================================================================

(defn dispatch-or-queue!
  "Unified dispatch entry point.
   Returns {:action :dispatch|:queue :task-id id :conflicts [...]}

   If approved, caller should proceed with actual dispatch.
   If queued, task is stored and will be returned by process-queue! when ready."
  [{:keys [_slave-id _prompt files _timeout-ms] :as task-spec}]
  (let [check-result (pre-flight-check task-spec)]
    (cond
      ;; Deadlock - cannot proceed at all
      (seq (:would-deadlock check-result))
      {:action :blocked
       :reason :circular-dependency
       :would-deadlock (:would-deadlock check-result)}

      ;; Conflicts - queue for later
      (:queue? check-result)
      (let [task-id (queue-task! (assoc task-spec
                                        :files (or (seq files)
                                                   (:extracted-files check-result))))]
        {:action :queued
         :task-id task-id
         :conflicts (:conflicts check-result)
         :position (count @task-queue)})

      ;; Approved - dispatch now
      :else
      {:action :dispatch
       :files (or (seq files) (:extracted-files check-result))})))

(defn coordinator-status
  "Get current coordinator status."
  []
  {:queue-length (count @task-queue)
   :queued-tasks (mapv #(select-keys % [:id :slave-id :queued-at]) @task-queue)
   :db-stats (logic/db-stats)})
