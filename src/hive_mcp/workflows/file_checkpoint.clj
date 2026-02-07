(ns hive-mcp.workflows.file-checkpoint
  "File checkpointing for SAA rewind support.

   Provides snapshot/restore functionality for files modified during
   SAA Act phase (wave execution, drone diffs, etc.). Enables rewinding
   to a known-good state when a wave produces bad results.

   ## Design

   A **checkpoint** is a named snapshot of file contents at a point in time.
   Checkpoints are stored in-memory (atom) keyed by checkpoint-id.

   Integration points:
   - Before wave execution: create-checkpoint! captures file states
   - On wave failure/bad results: rewind-to-checkpoint! restores files
   - After successful wave: delete-checkpoint! frees memory
   - FSM pre-hook: auto-checkpoint before Act phase transitions

   ## Architecture

   Pure data layer (checkpoint records) + side-effect boundary (I/O fns).
   File I/O is injectable via opts for testability.

   Checkpoint record:
     {:id          string    ;; unique checkpoint ID
      :label       string    ;; human-readable label (e.g., \"pre-wave-42\")
      :created-at  instant   ;; when checkpoint was created
      :files       {path content}  ;; file path -> content at snapshot time
      :metadata    map}      ;; optional: wave-id, plan-id, agent-id, etc.

   SOLID-S: File checkpointing only — no execution logic.
   SOLID-O: Open for extension via metadata and custom I/O fns.
   CLARITY-L: Pure data operations + injectable side effects.
   CLARITY-I: Validate inputs at public API boundary."
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log])
  (:import [java.time Instant]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Checkpoint Store (In-Memory)
;; =============================================================================

;; Atom holding all checkpoints. Map of checkpoint-id -> checkpoint record.
(defonce ^:private checkpoint-store (atom {}))

;; =============================================================================
;; Pure Functions (Data Layer)
;; =============================================================================

(defn generate-checkpoint-id
  "Generate a unique checkpoint ID.
   Format: chk-<timestamp>-<random>"
  ([]
   (generate-checkpoint-id #(Instant/now)))
  ([clock-fn]
   (str "chk-" (.toEpochMilli ^Instant (clock-fn)) "-" (rand-int 100000))))

(defn make-checkpoint
  "Create a checkpoint record (pure data, no I/O).

   Args:
     id       - Unique checkpoint ID
     label    - Human-readable label
     files    - Map of {file-path -> file-content}
     metadata - Optional metadata map

   Returns:
     Checkpoint record map."
  [id label files metadata]
  {:id         id
   :label      (or label id)
   :created-at (str (Instant/now))
   :files      (or files {})
   :metadata   (or metadata {})
   :file-count (count files)})

(defn checkpoint-summary
  "Create a token-efficient summary of a checkpoint (no file contents).

   Args:
     checkpoint - Full checkpoint record

   Returns:
     Summary map with file paths and sizes instead of contents."
  [checkpoint]
  (-> checkpoint
      (dissoc :files)
      (assoc :file-paths (vec (keys (:files checkpoint)))
             :total-bytes (reduce + 0 (map count (vals (:files checkpoint)))))))

;; =============================================================================
;; File I/O (Side-Effect Boundary)
;; =============================================================================

(defn- default-read-file
  "Default file reader. Reads file content as string.
   Returns nil if file doesn't exist."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (slurp f))))

(defn- default-write-file
  "Default file writer. Writes string content to file.
   Creates parent directories if needed."
  [path content]
  (let [f (io/file path)]
    (io/make-parents f)
    (spit f content)))

(defn snapshot-files
  "Read current contents of specified files.

   Args:
     file-paths - Collection of file paths to snapshot
     opts       - Optional map:
                  :read-fn - Custom file reader (fn [path] -> content-or-nil)

   Returns:
     Map of {file-path -> content} for files that exist.
     Missing files are excluded from the map."
  [file-paths & {:keys [read-fn] :or {read-fn default-read-file}}]
  (reduce
   (fn [acc path]
     (try
       (if-let [content (read-fn path)]
         (assoc acc path content)
         (do (log/debug "File not found for checkpoint, skipping" {:path path})
             acc))
       (catch Exception e
         (log/warn "Failed to read file for checkpoint" {:path path :error (ex-message e)})
         acc)))
   {}
   file-paths))

(defn restore-files
  "Write checkpoint file contents back to disk.

   Args:
     files - Map of {file-path -> content} from checkpoint
     opts  - Optional map:
             :write-fn  - Custom file writer (fn [path content])
             :dry-run?  - If true, return what would be written without writing

   Returns:
     {:restored [paths...]
      :failed   [{:path p :error msg}...]
      :skipped  [paths...]}  ;; Only in dry-run mode"
  [files & {:keys [write-fn dry-run?] :or {write-fn default-write-file}}]
  (if dry-run?
    {:restored []
     :failed   []
     :skipped  (vec (keys files))
     :dry-run  true
     :would-restore (vec (keys files))}
    (reduce
     (fn [acc [path content]]
       (try
         (write-fn path content)
         (update acc :restored conj path)
         (catch Exception e
           (log/error "Failed to restore file from checkpoint"
                      {:path path :error (ex-message e)})
           (update acc :failed conj {:path path :error (ex-message e)}))))
     {:restored [] :failed []}
     files)))

;; =============================================================================
;; Checkpoint Lifecycle (Public API)
;; =============================================================================

(defn create-checkpoint!
  "Create a checkpoint by snapshotting the specified files.

   Args:
     file-paths - Collection of file paths to snapshot
     opts       - Optional map:
                  :label    - Human-readable label (default: auto-generated)
                  :metadata - Additional metadata (wave-id, plan-id, agent-id, etc.)
                  :read-fn  - Custom file reader for testability
                  :clock-fn - Custom clock for testability

   Returns:
     Checkpoint summary (without file contents, for token efficiency).

   Side effects:
     - Reads files from disk (or via :read-fn)
     - Stores checkpoint in atom"
  [file-paths & {:keys [label metadata read-fn clock-fn]
                 :or   {clock-fn #(Instant/now)}}]
  {:pre [(sequential? file-paths)]}
  (let [id       (generate-checkpoint-id clock-fn)
        files    (snapshot-files file-paths :read-fn (or read-fn default-read-file))
        checkpoint (make-checkpoint id label files metadata)]
    (swap! checkpoint-store assoc id checkpoint)
    (log/info "Checkpoint created" {:id id :label label :file-count (count files)
                                    :paths (vec (keys files))})
    (checkpoint-summary checkpoint)))

(defn rewind-to-checkpoint!
  "Restore files from a checkpoint (rewind).

   Args:
     checkpoint-id - ID of the checkpoint to restore from
     opts          - Optional map:
                     :write-fn  - Custom file writer for testability
                     :dry-run?  - Preview what would be restored
                     :delete-after? - Delete checkpoint after successful rewind (default: false)

   Returns:
     {:success?  bool
      :checkpoint-id string
      :restored  [paths...]
      :failed    [{:path p :error msg}...]
      :error     string-or-nil}

   Side effects:
     - Writes files to disk (or via :write-fn)
     - Optionally deletes checkpoint from store"
  [checkpoint-id & {:keys [write-fn dry-run? delete-after?]
                    :or   {delete-after? false}}]
  (if-let [checkpoint (get @checkpoint-store checkpoint-id)]
    (let [result (restore-files (:files checkpoint)
                                :write-fn (or write-fn default-write-file)
                                :dry-run? dry-run?)]
      (log/info "Checkpoint rewind" {:id checkpoint-id
                                     :label (:label checkpoint)
                                     :restored-count (count (:restored result))
                                     :failed-count (count (:failed result))
                                     :dry-run? dry-run?})
      (when (and delete-after?
                 (not dry-run?)
                 (empty? (:failed result)))
        (swap! checkpoint-store dissoc checkpoint-id))
      (assoc result
             :success? (empty? (:failed result))
             :checkpoint-id checkpoint-id
             :label (:label checkpoint)))
    {:success? false
     :checkpoint-id checkpoint-id
     :error "Checkpoint not found"
     :restored []
     :failed []}))

(defn get-checkpoint
  "Get a checkpoint by ID. Returns full checkpoint including file contents."
  [checkpoint-id]
  (get @checkpoint-store checkpoint-id))

(defn get-checkpoint-summary
  "Get a token-efficient summary of a checkpoint (no file contents)."
  [checkpoint-id]
  (when-let [cp (get @checkpoint-store checkpoint-id)]
    (checkpoint-summary cp)))

(defn list-checkpoints
  "List all checkpoint summaries.

   Args:
     opts - Optional map:
            :label-pattern - Regex to filter by label
            :metadata-key  - Filter checkpoints containing this metadata key

   Returns:
     Vector of checkpoint summaries (sorted by created-at, newest first)."
  ([] (list-checkpoints {}))
  ([{:keys [label-pattern metadata-key]}]
   (let [all-cps (vals @checkpoint-store)
         filtered (cond->> all-cps
                    label-pattern
                    (filter #(re-find (re-pattern label-pattern) (or (:label %) "")))

                    metadata-key
                    (filter #(contains? (:metadata %) metadata-key)))]
     (->> filtered
          (sort-by :created-at #(compare %2 %1))
          (mapv checkpoint-summary)))))

(defn delete-checkpoint!
  "Delete a checkpoint from the store.

   Args:
     checkpoint-id - ID of checkpoint to delete

   Returns:
     {:success? bool :deleted? bool}"
  [checkpoint-id]
  (if (contains? @checkpoint-store checkpoint-id)
    (do
      (swap! checkpoint-store dissoc checkpoint-id)
      (log/info "Checkpoint deleted" {:id checkpoint-id})
      {:success? true :deleted? true})
    {:success? true :deleted? false}))

(defn delete-all-checkpoints!
  "Delete all checkpoints. Use with caution.

   Returns:
     {:deleted-count int}"
  []
  (let [count (count @checkpoint-store)]
    (reset! checkpoint-store {})
    (log/info "All checkpoints deleted" {:count count})
    {:deleted-count count}))

;; =============================================================================
;; FSM Integration Helpers
;; =============================================================================

(defn checkpoint-before-act
  "FSM pre-hook: create a checkpoint before Act phase transitions.

   Designed to be used as a :pre hook in FSM specs, or called explicitly
   before wave execution.

   Args:
     file-paths - Files that will be modified in the Act phase
     wave-id    - Wave ID for metadata (optional)
     plan-id    - Plan ID for metadata (optional)
     agent-id   - Agent ID for metadata (optional)

   Returns:
     Checkpoint summary map."
  [file-paths & {:keys [wave-id plan-id agent-id]}]
  (create-checkpoint!
   file-paths
   :label (str "pre-act"
               (when wave-id (str "-wave-" wave-id))
               (when plan-id (str "-plan-" plan-id)))
   :metadata (cond-> {}
               wave-id  (assoc :wave-id wave-id)
               plan-id  (assoc :plan-id plan-id)
               agent-id (assoc :agent-id agent-id)
               true     (assoc :phase :act
                               :purpose :saa-rewind))))

(defn rewind-act
  "Rewind the most recent Act phase checkpoint.

   Finds the most recent checkpoint with :phase :act in metadata
   and restores it.

   Args:
     opts - Optional map forwarded to rewind-to-checkpoint!

   Returns:
     Rewind result map or {:error \"No act checkpoint found\"}"
  [& {:keys [dry-run? delete-after?] :as opts}]
  (let [act-checkpoints (->> (list-checkpoints {:metadata-key :phase})
                             (filter #(= :act (get-in % [:metadata :phase]))))]
    (if-let [latest (first act-checkpoints)]
      (rewind-to-checkpoint! (:id latest)
                             :dry-run? dry-run?
                             :delete-after? delete-after?)
      {:success? false
       :error "No act checkpoint found"})))

;; =============================================================================
;; Wave Integration
;; =============================================================================

(defn checkpoint-wave-files!
  "Create a checkpoint for files targeted by a wave dispatch.

   Parses the wave task list to extract file paths and snapshots them.

   Args:
     tasks   - Vector of {:file path :task description} from wave dispatch
     wave-id - Wave ID for metadata linkage

   Returns:
     Checkpoint summary map."
  [tasks wave-id & {:keys [agent-id]}]
  (let [file-paths (mapv :file tasks)]
    (create-checkpoint!
     file-paths
     :label (str "pre-wave-" wave-id)
     :metadata {:wave-id  wave-id
                :agent-id agent-id
                :phase    :act
                :purpose  :wave-rewind
                :task-count (count tasks)})))

(defn rewind-wave!
  "Rewind files modified by a specific wave.

   Finds the checkpoint associated with the given wave-id and restores it.

   Args:
     wave-id - Wave ID to rewind
     opts    - Optional: :dry-run?, :delete-after?

   Returns:
     Rewind result map."
  [wave-id & {:keys [dry-run? delete-after?]}]
  (let [wave-checkpoints (->> (list-checkpoints {:metadata-key :wave-id})
                              (filter #(= wave-id (get-in % [:metadata :wave-id]))))]
    (if-let [checkpoint (first wave-checkpoints)]
      (rewind-to-checkpoint! (:id checkpoint)
                             :dry-run? dry-run?
                             :delete-after? delete-after?)
      {:success? false
       :error (str "No checkpoint found for wave: " wave-id)})))

;; =============================================================================
;; Testing Utilities
;; =============================================================================

(defn reset-store!
  "Reset the checkpoint store. For testing only."
  []
  (reset! checkpoint-store {})
  nil)

(defn store-snapshot
  "Get a snapshot of the current store state. For testing/debugging."
  []
  (reduce-kv
   (fn [acc id cp]
     (assoc acc id (checkpoint-summary cp)))
   {}
   @checkpoint-store))
