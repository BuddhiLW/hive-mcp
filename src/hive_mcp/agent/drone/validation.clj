(ns hive-mcp.agent.drone.validation
  "Pre/Post execution validation for drone agents.

   CLARITY-I (Inputs are Guarded): Validates files before mutation.
   CLARITY-Y (Yield Safe Failure): Returns structured validation results.
   CLARITY-T (Telemetry First): Metrics-friendly validation outcomes.

   Provides:
   - Pre-execution: file exists, not binary, size limits, claim verification
   - Post-execution: modification check, kondo lint, syntax errors, diff stats"
  (:require [hive-mcp.tools.kondo :as kondo]
            [hive-mcp.swarm.logic :as logic]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Constants
;;; ============================================================

(def ^:const max-file-size-bytes
  "Maximum file size for drone mutation (1MB).
   Larger files risk LLM context overflow and slow processing."
  (* 1024 1024))

(def ^:const binary-extensions
  "File extensions considered binary (not safe for text mutation)."
  #{".png" ".jpg" ".jpeg" ".gif" ".ico" ".bmp" ".webp"
    ".pdf" ".doc" ".docx" ".xls" ".xlsx"
    ".zip" ".tar" ".gz" ".rar" ".7z"
    ".jar" ".war" ".ear" ".class"
    ".exe" ".dll" ".so" ".dylib"
    ".mp3" ".mp4" ".avi" ".mov" ".wav"
    ".ttf" ".otf" ".woff" ".woff2"
    ".db" ".sqlite" ".sqlite3"})

;;; ============================================================
;;; Validation Result Structure
;;; ============================================================

(defn make-validation-result
  "Create a validation result map.

   Arguments:
     pre-valid?   - Pre-execution validation passed
     post-valid?  - Post-execution validation passed (nil if not yet run)
     lint-errors  - Vector of lint error maps
     diff-lines   - Number of lines changed (nil if not computed)
     warnings     - Vector of warning strings

   Returns:
     {:pre-valid? true/false
      :post-valid? true/false/nil
      :lint-errors []
      :diff-lines 42
      :warnings []}"
  [{:keys [pre-valid? post-valid? lint-errors diff-lines warnings]
    :or {pre-valid? false
         post-valid? nil
         lint-errors []
         diff-lines nil
         warnings []}}]
  {:pre-valid? pre-valid?
   :post-valid? post-valid?
   :lint-errors (vec lint-errors)
   :diff-lines diff-lines
   :warnings (vec warnings)})

;;; ============================================================
;;; Pre-Execution Validation
;;; ============================================================

(defn- file-extension
  "Extract lowercase file extension from path."
  [path]
  (when path
    (let [idx (str/last-index-of path ".")]
      (when (and idx (pos? idx))
        (str/lower-case (subs path idx))))))

(defn- binary-file?
  "Check if file is likely binary based on extension."
  [path]
  (contains? binary-extensions (file-extension path)))

(defn- file-size
  "Get file size in bytes, or nil if file doesn't exist."
  [path]
  (try
    (let [f (io/file path)]
      (when (.exists f)
        (.length f)))
    (catch Exception _ nil)))

(defn- file-readable?
  "Check if file exists and is readable."
  [path]
  (try
    (let [f (io/file path)]
      (and (.exists f) (.canRead f)))
    (catch Exception _ false)))

(defn- has-valid-claim?
  "Check if drone has a valid claim on the file.

   Arguments:
     task-id - Drone's task ID
     file    - File path to check

   Returns true if:
   - The file is claimed by this drone's task
   - Or no claims exist (backwards compatibility)"
  [task-id file]
  (try
    (let [file-claim (logic/get-claim-for-file file)]
      (or (nil? file-claim)                        ; No claim on this file
          (= task-id (:slave-id file-claim))))     ; Our claim (task-id maps to slave-id)
    (catch Exception e
      (log/warn "Failed to check file claim:" (.getMessage e))
      true))) ; Fail open for claim check errors

(defn validate-pre-execution
  "Validate file before drone mutation.

   Arguments:
     file         - File path to validate
     task-id      - Drone's task ID for claim verification
     allow-create - If true, allow non-existent files (for new file creation)

   Returns validation result map:
     {:pre-valid? true/false
      :post-valid? nil
      :lint-errors []
      :diff-lines nil
      :warnings [\"...\"]}

   Checks:
   1. File exists (unless allow-create)
   2. File is not binary
   3. File size within limits
   4. Drone has valid claim on file"
  [file task-id & [{:keys [allow-create] :or {allow-create false}}]]
  (let [warnings (atom [])
        add-warning! (fn [msg] (swap! warnings conj msg))
        f (io/file file)
        exists? (.exists f)]

    (cond
      ;; Check 1: File existence
      (and (not exists?) (not allow-create))
      (make-validation-result
       {:pre-valid? false
        :warnings [(str "File does not exist: " file)]})

      ;; Check 2: Binary file
      (binary-file? file)
      (make-validation-result
       {:pre-valid? false
        :warnings [(str "Cannot mutate binary file: " file)]})

      ;; Check 3: File size (only for existing files)
      (and exists? (> (or (file-size file) 0) max-file-size-bytes))
      (do
        (add-warning! (format "File exceeds size limit (%d bytes > %d max): %s"
                              (file-size file) max-file-size-bytes file))
        (make-validation-result
         {:pre-valid? false
          :warnings @warnings}))

      ;; Check 4: Valid claim
      (not (has-valid-claim? task-id file))
      (make-validation-result
       {:pre-valid? false
        :warnings [(str "Drone does not have claim on file: " file)]})

      ;; Check 5: File readable (if exists)
      (and exists? (not (file-readable? file)))
      (make-validation-result
       {:pre-valid? false
        :warnings [(str "File exists but is not readable: " file)]})

      ;; All checks passed
      :else
      (do
        ;; Add informational warnings
        (when (not exists?)
          (add-warning! (str "File will be created: " file)))
        (when (and exists? (> (or (file-size file) 0) (* 100 1024)))
          (add-warning! (format "Large file (%d KB), may be slow to process"
                                (quot (file-size file) 1024))))
        (make-validation-result
         {:pre-valid? true
          :warnings @warnings})))))

;;; ============================================================
;;; Post-Execution Validation
;;; ============================================================

(defn- count-diff-lines
  "Count number of changed lines between old and new content.

   Returns count of lines that differ (additions + deletions)."
  [old-content new-content]
  (when (and old-content new-content)
    (let [old-lines (set (str/split-lines old-content))
          new-lines (set (str/split-lines new-content))
          added (count (set/difference new-lines old-lines))
          removed (count (set/difference old-lines new-lines))]
      (+ added removed))))

(defn- run-lint-check
  "Run kondo lint on file and extract errors.

   Arguments:
     file       - File path to lint
     lint-level - Severity threshold (:error, :warning, :info)

   Returns vector of lint finding maps."
  [file lint-level]
  (try
    (let [{:keys [findings]} (kondo/run-analysis file)
          level-kw (keyword (or lint-level "error"))
          filtered (->> findings
                        (filter #(case level-kw
                                   :error (= (:level %) :error)
                                   :warning (#{:error :warning} (:level %))
                                   :info true))
                        (mapv #(select-keys % [:filename :row :col :level :type :message])))]
      filtered)
    (catch Exception e
      (log/warn "Lint check failed for" file ":" (.getMessage e))
      [{:filename file
        :level :error
        :type :lint-failed
        :message (str "Lint check failed: " (.getMessage e))}])))

(defn- file-was-modified?
  "Check if file was actually modified by comparing content.

   Arguments:
     file         - File path
     old-content  - Content before drone execution

   Returns true if file content differs from old-content."
  [file old-content]
  (try
    (let [new-content (slurp file)]
      (not= old-content new-content))
    (catch Exception e
      (log/warn "Could not check file modification:" (.getMessage e))
      false)))

(defn validate-post-execution
  "Validate file after drone mutation.

   Arguments:
     file          - File path to validate
     old-content   - Content before mutation (for diff comparison)
     pre-result    - Pre-validation result to merge with
     opts          - Options map:
                     :lint-level - Severity threshold (default: :error)
                     :require-modification - Fail if file wasn't changed (default: true)

   Returns updated validation result map:
     {:pre-valid? true (from pre-result)
      :post-valid? true/false
      :lint-errors [{:filename :row :col :level :type :message}]
      :diff-lines 42
      :warnings [\"...\"]}

   Checks:
   1. File was actually modified (if require-modification)
   2. No syntax/lint errors above threshold
   3. Computes diff statistics"
  [file old-content pre-result & [{:keys [lint-level require-modification]
                                   :or {lint-level :error
                                        require-modification true}}]]
  (let [warnings (atom (vec (:warnings pre-result)))
        add-warning! (fn [msg] (swap! warnings conj msg))
        f (io/file file)]

    (cond
      ;; File doesn't exist after execution
      (not (.exists f))
      (do
        (add-warning! (str "File does not exist after execution: " file))
        (make-validation-result
         (merge pre-result
                {:post-valid? false
                 :warnings @warnings})))

      ;; File wasn't modified but should have been
      (and require-modification
           old-content
           (not (file-was-modified? file old-content)))
      (do
        (add-warning! (str "File was not modified: " file))
        (make-validation-result
         (merge pre-result
                {:post-valid? false
                 :diff-lines 0
                 :warnings @warnings})))

      ;; Run lint check and evaluate
      :else
      (let [new-content (try (slurp file) (catch Exception _ nil))
            diff-lines (count-diff-lines old-content new-content)
            lint-errors (run-lint-check file lint-level)
            has-errors? (seq lint-errors)]

        (when has-errors?
          (add-warning! (format "Lint found %d error(s) in %s" (count lint-errors) file)))

        (when (and diff-lines (zero? diff-lines) require-modification)
          (add-warning! "No lines were changed"))

        (make-validation-result
         (merge pre-result
                {:post-valid? (not has-errors?)
                 :lint-errors lint-errors
                 :diff-lines (or diff-lines 0)
                 :warnings @warnings}))))))

;;; ============================================================
;;; Combined Validation API
;;; ============================================================

(defn validate-files-pre
  "Validate multiple files before drone execution.

   Arguments:
     files   - Collection of file paths
     task-id - Drone's task ID

   Returns map of file -> validation result:
     {\"src/foo.clj\" {:pre-valid? true ...}
      \"src/bar.clj\" {:pre-valid? false ...}}"
  [files task-id]
  (into {}
        (for [file files]
          [file (validate-pre-execution file task-id)])))

(defn validate-files-post
  "Validate multiple files after drone execution.

   Arguments:
     file-contents - Map of file -> old content before mutation
     pre-results   - Map of file -> pre-validation results
     opts          - Options passed to validate-post-execution

   Returns map of file -> validation result with post-validation added."
  [file-contents pre-results & [opts]]
  (into {}
        (for [[file old-content] file-contents]
          (let [pre-result (get pre-results file (make-validation-result {:pre-valid? true}))]
            [file (validate-post-execution file old-content pre-result opts)]))))

(defn all-valid?
  "Check if all validation results passed.

   Arguments:
     results - Map of file -> validation result
     phase   - :pre or :post (which validation to check)

   Returns true if all files passed the specified validation phase."
  [results phase]
  (let [key (case phase
              :pre :pre-valid?
              :post :post-valid?)]
    (every? #(get % key) (vals results))))

(defn summarize-validation
  "Create a summary of validation results.

   Arguments:
     results - Map of file -> validation result

   Returns:
     {:total-files 5
      :pre-valid 4
      :pre-invalid 1
      :post-valid 3
      :post-invalid 1
      :post-pending 1
      :total-lint-errors 7
      :total-diff-lines 142
      :all-warnings [\"...\"]}"
  [results]
  (let [all-results (vals results)]
    {:total-files (count all-results)
     :pre-valid (count (filter :pre-valid? all-results))
     :pre-invalid (count (filter (comp not :pre-valid?) all-results))
     :post-valid (count (filter #(true? (:post-valid? %)) all-results))
     :post-invalid (count (filter #(false? (:post-valid? %)) all-results))
     :post-pending (count (filter #(nil? (:post-valid? %)) all-results))
     :total-lint-errors (reduce + (map #(count (:lint-errors %)) all-results))
     :total-diff-lines (reduce + (keep :diff-lines all-results))
     :all-warnings (vec (mapcat :warnings all-results))}))
