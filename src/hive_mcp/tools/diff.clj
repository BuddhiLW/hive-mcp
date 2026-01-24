(ns hive-mcp.tools.diff
  "Diff-based workflow tools for drone agents.
   
   Drones (OpenRouter free-tier models) cannot safely use file_write/file_edit
   directly due to their lower capability. This module provides a review-based
   workflow where drones propose diffs that the hivemind can review and apply.
   
   Workflow:
   1. Drone calls propose_diff with old/new content and description
   2. Hivemind reviews with list_proposed_diffs (sees unified diff)
   3. Hivemind calls apply_diff (applies change) or reject_diff (discards)
   
   Architecture (DDD/SOLID):
   - Domain: Diff lifecycle (pending → applied/rejected)
   - Application: Handlers coordinate validation + state updates
   - Infra: File I/O via slurp/spit (mocked in tests)"
  (:require [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.guards :as guards]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Domain: Pending Diffs State
;; =============================================================================

(defn- mcp-error-json
  "Create an error MCP response with JSON-encoded error message."
  [error-message]
  {:type "text"
   :text (json/write-str {:error error-message})
   :isError true})

;; Atom storing pending diff proposals. Map of diff-id -> diff-data.
(defonce pending-diffs (atom {}))

;; Forward declarations for functions defined later but used in wave helpers
(declare handle-apply-diff handle-reject-diff)

;; =============================================================================
;; Auto-Approve Rules Configuration
;; =============================================================================

(def default-auto-approve-rules
  "Default rules for auto-approving diff proposals.

   A diff is auto-approved only if ALL conditions are met:
   - max-lines-changed: Maximum total lines added + deleted
   - no-deletions-only: Reject changes that only delete code
   - require-description: Require non-empty description
   - allowed-path-patterns: Regex patterns for allowed file paths"
  {:max-lines-changed 100
   :no-deletions-only true
   :require-description true
   :allowed-path-patterns [#".*\.clj[sx]?$"    ; Clojure files
                           #".*\.edn$"         ; EDN config
                           #".*\.md$"          ; Markdown docs
                           #".*\.json$"]})     ; JSON config

(defonce auto-approve-rules (atom default-auto-approve-rules))

(defn clear-pending-diffs!
  "Clear pending diffs. GUARDED - no-op if coordinator running.

   CLARITY-Y: Yield safe failure - prevents test fixtures from
   corrupting production diff state."
  []
  (guards/when-not-coordinator
   "clear-pending-diffs! called"
   (reset! pending-diffs {})))

;; =============================================================================
;; Domain: Diff Operations (Pure Functions)
;; =============================================================================

(defn generate-diff-id
  "Generate a unique diff ID."
  []
  (str "diff-" (System/currentTimeMillis) "-" (rand-int 10000)))

(defn compute-unified-diff
  "Compute a unified diff between old and new content.
   Returns a string in unified diff format."
  [old-content new-content file-path]
  (let [old-lines (str/split-lines old-content)
        new-lines (str/split-lines new-content)
        ;; Simple line-by-line diff (not full Myers algorithm, but sufficient)
        header (str "--- a/" file-path "\n+++ b/" file-path "\n@@ -1,"
                    (count old-lines) " +1," (count new-lines) " @@\n")]
    (str header
         (str/join "\n"
                   (concat
                    (map #(str "-" %) old-lines)
                    (map #(str "+" %) new-lines))))))

(defn create-diff-proposal
  "Create a diff proposal map from input parameters.
   Optionally includes wave-id for batch review tracking."
  [{:keys [file_path old_content new_content description drone_id wave_id]}]
  (let [diff-id (generate-diff-id)
        unified (compute-unified-diff old_content new_content file_path)]
    (cond-> {:id diff-id
             :file-path file_path
             :old-content old_content
             :new-content new_content
             :description (or description "No description provided")
             :drone-id (or drone_id "unknown")
             :unified-diff unified
             :status "pending"
             :created-at (java.time.Instant/now)}
      wave_id (assoc :wave-id wave_id))))

(defn validate-propose-params
  "Validate parameters for propose_diff. Returns nil if valid, error message if not.

   CLARITY-Y: Validates new_content is non-empty to prevent 0-byte file writes
   when LLMs return empty responses."
  [{:keys [file_path old_content new_content]}]
  (cond
    (str/blank? file_path) "Missing required field: file_path"
    (nil? old_content) "Missing required field: old_content"
    (nil? new_content) "Missing required field: new_content"
    ;; CLARITY-Y: Prevent empty file writes from LLM empty responses
    (str/blank? new_content) "new_content cannot be empty or whitespace-only - LLM may have returned empty response"
    :else nil))

(defn get-project-root
  "Get project root from Emacs or fall back to current working directory."
  []
  (or (try (ec/project-root)
           (catch Exception _ nil))
      (System/getProperty "user.dir")))

(defn translate-sandbox-path
  "Translate clojure-mcp sandbox paths back to real project paths.
   
   Drones run through clojure-mcp which sandboxes file access to /tmp/fs-<n>/.
   This function detects sandbox paths and translates them to real paths.
   
   Example: /tmp/fs-1/src/foo.clj -> <project-root>/src/foo.clj"
  [file-path]
  (if-let [[_ relative-path] (re-matches #"/tmp/fs-\d+/(.+)" file-path)]
    (let [project-root (get-project-root)]
      (log/debug "Translating sandbox path" {:sandbox file-path :relative relative-path})
      (str (str/replace project-root #"/$" "") "/" relative-path))
    file-path))

(defn validate-diff-path
  "Validate a file path for propose_diff.

   Drones sometimes hallucinate invalid paths like '/hivemind/controller.clj'.
   This function validates that paths:
   1. Are not suspicious absolute paths (absolute paths must exist)
   2. Don't escape the project directory (no ../../../etc/passwd)
   3. Resolve to valid locations within the project

   Arguments:
     file-path    - Path to validate
     project-root - Optional project root override (defaults to get-project-root)

   Returns {:valid true :resolved-path \"...\"} or {:valid false :error \"...\"}."
  ([file-path] (validate-diff-path file-path nil))
  ([file-path project-root-override]
   (let [project-root (or project-root-override (get-project-root))
         file (io/file file-path)]
     (cond
      ;; Check 1: Empty or blank path
       (str/blank? file-path)
       {:valid false :error "File path cannot be empty"}

      ;; Check 2: Suspicious absolute paths (absolute but doesn't exist)
       (and (.isAbsolute file)
            (not (.exists file))
           ;; Also reject if the parent directory doesn't exist
           ;; (clear sign of hallucinated path like /hivemind/foo.clj)
            (not (.exists (.getParentFile file))))
       {:valid false
        :error (str "Invalid absolute path: '" file-path "' - "
                    "neither the file nor its parent directory exists. "
                    "Use relative paths like 'src/hive_mcp/foo.clj' or ensure the path is valid.")}

      ;; Check 3: Path escapes project directory
       (let [resolved (if (.isAbsolute file)
                        file
                        (io/file project-root file-path))
             canonical-path (.getCanonicalPath resolved)
             canonical-root (.getCanonicalPath (io/file project-root))]
         (not (str/starts-with? canonical-path canonical-root)))
       {:valid false
        :error (str "Path escapes project directory: '" file-path "' "
                    "would resolve outside the project root '" project-root "'. "
                    "All paths must be within the project directory.")}

      ;; Check 4: Absolute path that exists - allow it
       (.isAbsolute file)
       {:valid true :resolved-path (.getCanonicalPath file)}

      ;; Check 5: Relative path - resolve against project root
       :else
       (let [resolved (io/file project-root file-path)
             canonical-path (.getCanonicalPath resolved)]
         {:valid true :resolved-path canonical-path})))))

;; =============================================================================
;; Auto-Approve Validation
;; =============================================================================

(defn- count-line-changes
  "Count total lines changed (added + deleted) in a diff."
  [old-content new-content]
  (let [old-lines (count (str/split-lines (or old-content "")))
        new-lines (count (str/split-lines (or new-content "")))]
    (+ (max 0 (- old-lines new-lines))  ; deleted lines
       (max 0 (- new-lines old-lines))))) ; added lines

(defn- deletions-only?
  "Check if the change only deletes content without adding anything."
  [old-content new-content]
  (and (not (str/blank? old-content))
       (or (str/blank? new-content)
           (< (count new-content) (/ (count old-content) 2)))))

(defn- path-matches-patterns?
  "Check if file path matches any of the allowed patterns."
  [file-path patterns]
  (if (empty? patterns)
    true  ; No patterns = allow all
    (some #(re-matches % file-path) patterns)))

(defn auto-approve-diff?
  "Check if a diff meets auto-approve criteria.

   Arguments:
     diff  - Diff proposal map
     rules - Optional rules (defaults to @auto-approve-rules)

   Returns {:approved true} or {:approved false :reason \"...\"}."
  ([diff] (auto-approve-diff? diff @auto-approve-rules))
  ([{:keys [old-content new-content file-path description]} rules]
   (let [{:keys [max-lines-changed no-deletions-only
                 require-description allowed-path-patterns]} rules
         line-changes (count-line-changes old-content new-content)]
     (cond
       ;; Check line count
       (and max-lines-changed (> line-changes max-lines-changed))
       {:approved false
        :reason (str "Too many lines changed: " line-changes " > " max-lines-changed)}

       ;; Check deletions-only
       (and no-deletions-only (deletions-only? old-content new-content))
       {:approved false
        :reason "Change only deletes content - requires manual review"}

       ;; Check description
       (and require-description (str/blank? description))
       {:approved false
        :reason "Missing description - requires manual review"}

       ;; Check path pattern
       (and (seq allowed-path-patterns)
            (not (path-matches-patterns? file-path allowed-path-patterns)))
       {:approved false
        :reason (str "File path not in allowed patterns: " file-path)}

       ;; All checks passed
       :else
       {:approved true}))))

;; =============================================================================
;; Wave Batch Operations
;; =============================================================================

(defn get-wave-diffs
  "Get all pending diffs for a specific wave-id."
  [wave-id]
  (->> (vals @pending-diffs)
       (filter #(= wave-id (:wave-id %)))
       (vec)))

(defn review-wave-diffs
  "Get a summary of all diffs proposed by a wave for review.

   Returns map with:
     :wave-id     - The wave ID
     :count       - Number of diffs
     :diffs       - List of diff summaries (without old/new content)
     :auto-approve-results - Which diffs would pass auto-approve"
  [wave-id]
  (let [diffs (get-wave-diffs wave-id)
        summaries (mapv (fn [d]
                          {:id (:id d)
                           :file-path (:file-path d)
                           :description (:description d)
                           :drone-id (:drone-id d)
                           :unified-diff (:unified-diff d)
                           :status (:status d)
                           :created-at (str (:created-at d))})
                        diffs)
        auto-results (mapv (fn [d]
                             {:id (:id d)
                              :file-path (:file-path d)
                              :auto-approve (auto-approve-diff? d)})
                           diffs)]
    {:wave-id wave-id
     :count (count diffs)
     :diffs summaries
     :auto-approve-results auto-results}))

(defn approve-wave-diffs!
  "Approve and apply all diffs from a wave.

   Arguments:
     wave-id    - Wave ID to approve diffs for
     diff-ids   - Optional specific diff IDs to approve (nil = all)

   Returns map with :applied and :failed lists."
  ([wave-id] (approve-wave-diffs! wave-id nil))
  ([wave-id diff-ids]
   (let [wave-diffs (get-wave-diffs wave-id)
         to-apply (if diff-ids
                    (filter #(contains? (set diff-ids) (:id %)) wave-diffs)
                    wave-diffs)
         results (for [{:keys [id]} to-apply]
                   (let [response (handle-apply-diff {:diff_id id})
                         parsed (try (json/read-str (:text response) :key-fn keyword)
                                     (catch Exception _ nil))]
                     (if (:isError response)
                       {:status :failed :id id :error (:error parsed)}
                       {:status :applied :id id :file (:file-path parsed)})))
         {applied :applied failed :failed} (group-by :status results)]
     (log/info "Approved wave diffs" {:wave-id wave-id
                                      :applied (count applied)
                                      :failed (count failed)})
     {:applied (vec applied)
      :failed (vec failed)})))

(defn reject-wave-diffs!
  "Reject all diffs from a wave.

   Arguments:
     wave-id - Wave ID to reject diffs for
     reason  - Reason for rejection

   Returns count of rejected diffs."
  [wave-id reason]
  (let [wave-diffs (get-wave-diffs wave-id)]
    (doseq [{:keys [id]} wave-diffs]
      (handle-reject-diff {:diff_id id :reason reason}))
    (log/info "Rejected wave diffs" {:wave-id wave-id :count (count wave-diffs) :reason reason})
    {:rejected (count wave-diffs)
     :wave-id wave-id
     :reason reason}))

(defn auto-approve-wave-diffs!
  "Auto-approve diffs that meet criteria, flag others for manual review.

   Arguments:
     wave-id - Wave ID to process

   Returns map with :auto-approved, :manual-review, and :failed lists."
  [wave-id]
  (let [wave-diffs (get-wave-diffs wave-id)
        categorized (for [d wave-diffs]
                      (assoc d :auto-check (auto-approve-diff? d)))
        auto-approvable (filter #(get-in % [:auto-check :approved]) categorized)
        manual-review (remove #(get-in % [:auto-check :approved]) categorized)
        ;; Apply auto-approved diffs
        apply-results (for [{:keys [id]} auto-approvable]
                        (let [response (handle-apply-diff {:diff_id id})
                              parsed (try (json/read-str (:text response) :key-fn keyword)
                                          (catch Exception _ nil))]
                          (if (:isError response)
                            {:status :failed :id id :error (:error parsed)}
                            {:status :applied :id id})))
        {applied :applied failed :failed} (group-by :status apply-results)]
    (log/info "Auto-approved wave diffs" {:wave-id wave-id
                                          :auto-approved (count applied)
                                          :manual-review (count manual-review)
                                          :failed (count failed)})
    {:auto-approved (vec applied)
     :manual-review (mapv (fn [d]
                            {:id (:id d)
                             :file-path (:file-path d)
                             :reason (get-in d [:auto-check :reason])})
                          manual-review)
     :failed (vec failed)}))

;; =============================================================================
;; Multi-Drone Batch Operations
;; =============================================================================

(defn batch-review-diffs
  "Get all pending diffs from multiple drones for batch review.

   Arguments:
     drone-ids - Collection of drone IDs (or nil for all pending diffs)

   Returns list of diffs sorted by timestamp, with auto-approve analysis."
  ([] (batch-review-diffs nil))
  ([drone-ids]
   (let [all-diffs (vals @pending-diffs)
         filtered (if (seq drone-ids)
                    (filter #(contains? (set drone-ids) (:drone-id %)) all-diffs)
                    all-diffs)]
     (->> filtered
          (sort-by :created-at)
          (mapv (fn [d]
                  {:id (:id d)
                   :file-path (:file-path d)
                   :description (:description d)
                   :drone-id (:drone-id d)
                   :wave-id (:wave-id d)
                   :unified-diff (:unified-diff d)
                   :created-at (str (:created-at d))
                   :auto-approve-check (auto-approve-diff? d)}))))))

(defn get-auto-approve-rules
  "Get current auto-approve rules with descriptions.

   Returns the rules map with human-readable format."
  []
  (let [rules @auto-approve-rules]
    {:max-lines-changed (:max-lines-changed rules)
     :must-pass-lint false  ; Not implemented yet - future enhancement
     :no-deletions-only (:no-deletions-only rules)
     :require-description (:require-description rules)
     :allowed-path-patterns (mapv str (:allowed-path-patterns rules))}))

(defn safe-to-auto-approve?
  "Check if diff meets auto-approve criteria.

   Alias for auto-approve-diff? with more descriptive name.
   Returns true if diff can be safely auto-approved."
  ([diff] (safe-to-auto-approve? diff @auto-approve-rules))
  ([diff rules]
   (:approved (auto-approve-diff? diff rules))))

(defn approve-safe-diffs!
  "Auto-approve diffs from multiple drones that meet safety criteria.

   Arguments:
     drone-ids - Collection of drone IDs (or nil for all pending diffs)
     opts      - Optional map with:
                 :rules - Custom rules (defaults to @auto-approve-rules)
                 :dry-run - If true, only report what would be approved

   Returns map with:
     :auto-approved - Diffs that were approved and applied
     :manual-review - Diffs that need manual review (with reasons)
     :failed        - Diffs that failed to apply"
  ([] (approve-safe-diffs! nil {}))
  ([drone-ids] (approve-safe-diffs! drone-ids {}))
  ([drone-ids {:keys [rules dry-run] :or {rules @auto-approve-rules dry-run false}}]
   (let [diffs (batch-review-diffs drone-ids)
         categorized (for [d diffs
                           :let [check (auto-approve-diff?
                                        (get @pending-diffs (:id d))
                                        rules)]]
                       (assoc d :auto-check check))
         auto-approvable (filter #(get-in % [:auto-check :approved]) categorized)
         manual-review (remove #(get-in % [:auto-check :approved]) categorized)]
     (if dry-run
       ;; Dry run - just report what would happen
       {:dry-run true
        :would-approve (mapv #(select-keys % [:id :file-path :drone-id]) auto-approvable)
        :manual-review (mapv #(select-keys % [:id :file-path :drone-id :auto-check]) manual-review)}
       ;; Actual execution - apply safe diffs
       (let [apply-results (for [{:keys [id]} auto-approvable]
                             (let [response (handle-apply-diff {:diff_id id})
                                   parsed (try (json/read-str (:text response) :key-fn keyword)
                                               (catch Exception _ nil))]
                               (if (:isError response)
                                 {:status :failed :id id :error (:error parsed)}
                                 {:status :applied :id id :file-path (:file-path parsed)})))
             {applied :applied failed :failed} (group-by :status apply-results)]
         (log/info "Batch approve-safe-diffs!" {:drones (count (set (map :drone-id diffs)))
                                                :total (count diffs)
                                                :auto-approved (count applied)
                                                :manual-review (count manual-review)
                                                :failed (count failed)})
         {:auto-approved (vec applied)
          :manual-review (mapv (fn [d]
                                 {:id (:id d)
                                  :file-path (:file-path d)
                                  :drone-id (:drone-id d)
                                  :reason (get-in d [:auto-check :reason])})
                               manual-review)
          :failed (vec failed)})))))

;; =============================================================================
;; Application: Handlers
;; =============================================================================

(defn handle-propose-diff
  "Handle propose_diff tool call.
   Stores a proposed diff for review by the hivemind.
   Translates sandbox paths and validates file paths."
  [{:keys [file_path _old_content _new_content _description drone_id] :as params}]
  (log/debug "propose_diff called" {:file file_path :drone drone_id})
  (if-let [error (validate-propose-params params)]
    (do
      (log/warn "propose_diff validation failed" {:error error})
      (mcp-error-json error))
    ;; Translate sandbox paths before validation
    (let [translated-path (translate-sandbox-path file_path)
          _ (when (not= translated-path file_path)
              (log/info "Translated sandbox path" {:from file_path :to translated-path}))
          path-result (validate-diff-path translated-path)]
      (if-not (:valid path-result)
        (do
          (log/warn "propose_diff path validation failed"
                    {:file translated-path :original file_path :error (:error path-result) :drone drone_id})
          (mcp-error-json (:error path-result)))
        (try
          ;; Use the resolved path for the proposal
          (let [resolved-path (:resolved-path path-result)
                proposal (create-diff-proposal (assoc params :file_path resolved-path))]
            (swap! pending-diffs assoc (:id proposal) proposal)
            (log/info "Diff proposed" {:id (:id proposal)
                                       :file resolved-path
                                       :original-path file_path
                                       :drone drone_id})
            (mcp-json {:id (:id proposal)
                       :status "pending"
                       :file-path resolved-path
                       :original-path (when (not= file_path resolved-path) file_path)
                       :description (:description proposal)
                       :message "Diff proposed for review. Hivemind will apply or reject."}))
          (catch Exception e
            (log/error e "Failed to propose diff")
            (mcp-error-json (str "Failed to propose diff: " (.getMessage e)))))))))

(defn handle-list-proposed-diffs
  "Handle list_proposed_diffs tool call.
   Returns all pending diffs, optionally filtered by drone_id."
  [{:keys [drone_id]}]
  (log/debug "list_proposed_diffs called" {:drone_id drone_id})
  (try
    (let [all-diffs (vals @pending-diffs)
          filtered (if (str/blank? drone_id)
                     all-diffs
                     (filter #(= drone_id (:drone-id %)) all-diffs))
          ;; Convert to JSON-safe format (remove Instant objects)
          safe-diffs (map (fn [d]
                            (-> d
                                (update :created-at str)
                                (dissoc :old-content :new-content))) ; Save tokens
                          filtered)]
      (log/info "Listed proposed diffs" {:count (count safe-diffs)})
      (mcp-json {:count (count safe-diffs)
                 :diffs (vec safe-diffs)}))
    (catch Exception e
      (log/error e "Failed to list proposed diffs")
      (mcp-error-json (str "Failed to list diffs: " (.getMessage e))))))

(defn handle-apply-diff
  "Handle apply_diff tool call.
   Applies the diff by finding and replacing old-content within the file.
   If old-content is empty and file doesn't exist, creates a new file.

   CLARITY-Y: Defense-in-depth validation blocks empty new_content even if
   it passed propose_diff, preventing 0-byte file writes from LLM failures."
  [{:keys [diff_id]}]
  (log/debug "apply_diff called" {:diff_id diff_id})
  (cond
    (str/blank? diff_id)
    (do
      (log/warn "apply_diff missing diff_id")
      (mcp-error-json "Missing required field: diff_id"))

    (not (contains? @pending-diffs diff_id))
    (do
      (log/warn "apply_diff diff not found" {:diff_id diff_id})
      (mcp-error-json (str "Diff not found: " diff_id)))

    :else
    (let [{:keys [file-path old-content new-content]} (get @pending-diffs diff_id)
          file-exists? (.exists (io/file file-path))
          creating-new-file? (and (str/blank? old-content) (not file-exists?))]
      ;; CLARITY-Y: Defense-in-depth - block empty content even if it passed propose_diff
      (cond
        ;; Block empty new_content (prevents 0-byte file writes)
        (str/blank? new-content)
        (do
          (log/warn "apply_diff blocked: empty new_content" {:diff_id diff_id :file file-path})
          (swap! pending-diffs dissoc diff_id)
          (mcp-error-json "Cannot apply diff: new_content is empty or whitespace-only. This typically indicates the LLM returned an empty response."))

        ;; Case 1: Creating a new file (old-content empty, file doesn't exist)
        creating-new-file?
        (try
          (let [parent (.getParentFile (io/file file-path))]
            (when (and parent (not (.exists parent)))
              (.mkdirs parent)))
          (spit file-path new-content)
          (swap! pending-diffs dissoc diff_id)
          (log/info "New file created" {:id diff_id :file file-path})
          (mcp-json {:id diff_id
                     :status "applied"
                     :file-path file-path
                     :created true
                     :message "New file created successfully"})
          (catch Exception e
            (log/error e "Failed to create file" {:diff_id diff_id})
            (mcp-error-json (str "Failed to create file: " (.getMessage e)))))

        ;; Case 2: File doesn't exist but old-content is not empty - error
        (not file-exists?)
        (do
          (log/warn "apply_diff file not found" {:file file-path})
          (mcp-error-json (str "File not found: " file-path)))

        ;; Case 3: Normal replacement in existing file
        :else
        (try
          (let [current-content (slurp file-path)]
            (cond
              ;; old-content not found in file
              (not (str/includes? current-content old-content))
              (do
                (log/warn "apply_diff old content not found in file" {:file file-path})
                (mcp-error-json "Old content not found in file. File may have been modified since diff was proposed."))

              ;; Multiple occurrences - ambiguous
              (> (count (re-seq (re-pattern (java.util.regex.Pattern/quote old-content)) current-content)) 1)
              (do
                (log/warn "apply_diff multiple matches found" {:file file-path})
                (mcp-error-json "Multiple occurrences of old content found. Cannot apply safely - diff is ambiguous."))

              ;; Exactly one match - apply the replacement
              :else
              (do
                (spit file-path (str/replace-first current-content old-content new-content))
                (swap! pending-diffs dissoc diff_id)
                (log/info "Diff applied" {:id diff_id :file file-path})
                (mcp-json {:id diff_id
                           :status "applied"
                           :file-path file-path
                           :message "Diff applied successfully"}))))
          (catch Exception e
            (log/error e "Failed to apply diff" {:diff_id diff_id})
            (mcp-error-json (str "Failed to apply diff: " (.getMessage e)))))))))

(defn handle-reject-diff
  "Handle reject_diff tool call.
   Removes the diff from pending without applying."
  [{:keys [diff_id reason]}]
  (log/debug "reject_diff called" {:diff_id diff_id :reason reason})
  (cond
    (str/blank? diff_id)
    (do
      (log/warn "reject_diff missing diff_id")
      (mcp-error-json "Missing required field: diff_id"))

    (not (contains? @pending-diffs diff_id))
    (do
      (log/warn "reject_diff diff not found" {:diff_id diff_id})
      (mcp-error-json (str "Diff not found: " diff_id)))

    :else
    (let [{:keys [file-path drone-id]} (get @pending-diffs diff_id)]
      ;; Remove from pending (don't apply)
      (swap! pending-diffs dissoc diff_id)
      (log/info "Diff rejected" {:id diff_id :file file-path :reason reason})
      (mcp-json {:id diff_id
                 :status "rejected"
                 :file-path file-path
                 :drone-id drone-id
                 :reason (or reason "No reason provided")
                 :message "Diff rejected and discarded"}))))

(defn handle-get-diff-details
  "Handle get_diff_details tool call.
   Returns full details of a specific diff including old/new content."
  [{:keys [diff_id]}]
  (log/debug "get_diff_details called" {:diff_id diff_id})
  (cond
    (str/blank? diff_id)
    (mcp-error-json "Missing required field: diff_id")

    (not (contains? @pending-diffs diff_id))
    (mcp-error-json (str "Diff not found: " diff_id))

    :else
    (let [diff (get @pending-diffs diff_id)]
      (mcp-json (-> diff
                    (update :created-at str))))))

(defn handle-review-wave-diffs
  "Handle review_wave_diffs tool call.
   Returns summary of all diffs from a wave with auto-approve analysis."
  [{:keys [wave_id]}]
  (log/debug "review_wave_diffs called" {:wave_id wave_id})
  (if (str/blank? wave_id)
    (mcp-error-json "Missing required field: wave_id")
    (try
      (let [result (review-wave-diffs wave_id)]
        (mcp-json result))
      (catch Exception e
        (log/error e "Failed to review wave diffs")
        (mcp-error-json (str "Failed to review wave diffs: " (.getMessage e)))))))

(defn handle-approve-wave-diffs
  "Handle approve_wave_diffs tool call.
   Applies all or selected diffs from a wave."
  [{:keys [wave_id diff_ids]}]
  (log/debug "approve_wave_diffs called" {:wave_id wave_id :diff_ids diff_ids})
  (if (str/blank? wave_id)
    (mcp-error-json "Missing required field: wave_id")
    (try
      (let [result (approve-wave-diffs! wave_id diff_ids)]
        (mcp-json result))
      (catch Exception e
        (log/error e "Failed to approve wave diffs")
        (mcp-error-json (str "Failed to approve wave diffs: " (.getMessage e)))))))

(defn handle-reject-wave-diffs
  "Handle reject_wave_diffs tool call.
   Rejects all diffs from a wave."
  [{:keys [wave_id reason]}]
  (log/debug "reject_wave_diffs called" {:wave_id wave_id :reason reason})
  (if (str/blank? wave_id)
    (mcp-error-json "Missing required field: wave_id")
    (try
      (let [result (reject-wave-diffs! wave_id (or reason "Rejected by coordinator"))]
        (mcp-json result))
      (catch Exception e
        (log/error e "Failed to reject wave diffs")
        (mcp-error-json (str "Failed to reject wave diffs: " (.getMessage e)))))))

(defn handle-auto-approve-wave-diffs
  "Handle auto_approve_wave_diffs tool call.
   Auto-approves diffs meeting criteria, flags others for manual review."
  [{:keys [wave_id]}]
  (log/debug "auto_approve_wave_diffs called" {:wave_id wave_id})
  (if (str/blank? wave_id)
    (mcp-error-json "Missing required field: wave_id")
    (try
      (let [result (auto-approve-wave-diffs! wave_id)]
        (mcp-json result))
      (catch Exception e
        (log/error e "Failed to auto-approve wave diffs")
        (mcp-error-json (str "Failed to auto-approve: " (.getMessage e)))))))

(defn handle-batch-review-diffs
  "Handle batch_review_diffs tool call.
   Returns all pending diffs from multiple drones for batch review."
  [{:keys [drone_ids]}]
  (log/debug "batch_review_diffs called" {:drone_ids drone_ids})
  (try
    (let [ids (when (seq drone_ids) (vec drone_ids))
          result (batch-review-diffs ids)]
      (mcp-json {:count (count result)
                 :diffs result
                 :rules (get-auto-approve-rules)}))
    (catch Exception e
      (log/error e "Failed to batch review diffs")
      (mcp-error-json (str "Failed to batch review: " (.getMessage e))))))

(defn handle-approve-safe-diffs
  "Handle approve_safe_diffs tool call.
   Auto-approve diffs from multiple drones that meet safety criteria."
  [{:keys [drone_ids dry_run]}]
  (log/debug "approve_safe_diffs called" {:drone_ids drone_ids :dry_run dry_run})
  (try
    (let [ids (when (seq drone_ids) (vec drone_ids))
          result (approve-safe-diffs! ids {:dry-run (boolean dry_run)})]
      (mcp-json result))
    (catch Exception e
      (log/error e "Failed to approve safe diffs")
      (mcp-error-json (str "Failed to approve safe diffs: " (.getMessage e))))))

(defn handle-get-auto-approve-rules
  "Handle get_auto_approve_rules tool call.
   Returns the current auto-approve rules configuration."
  [_params]
  (log/debug "get_auto_approve_rules called")
  (mcp-json (get-auto-approve-rules)))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "propose_diff"
    :description "Propose a file change for review by the hivemind. Drones should use this instead of file_write/file_edit. The change will be queued for review and the hivemind will apply or reject it."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the file to modify"}
                               "old_content" {:type "string"
                                              :description "The current/expected content of the file (for safety verification)"}
                               "new_content" {:type "string"
                                              :description "The proposed new content for the file"}
                               "description" {:type "string"
                                              :description "Description of what this change does and why"}
                               "drone_id" {:type "string"
                                           :description "ID of the drone proposing this change"}}
                  :required ["file_path" "old_content" "new_content"]}
    :handler handle-propose-diff}

   {:name "list_proposed_diffs"
    :description "List all pending diff proposals awaiting review. Use this to see what changes drones have proposed. Returns unified diff format for easy review."
    :inputSchema {:type "object"
                  :properties {"drone_id" {:type "string"
                                           :description "Optional: filter by drone ID"}}
                  :required []}
    :handler handle-list-proposed-diffs}

   {:name "apply_diff"
    :description "Apply a proposed diff to the file. Only call this after reviewing the diff. Will fail if file content has changed since the diff was proposed (safety check)."
    :inputSchema {:type "object"
                  :properties {"diff_id" {:type "string"
                                          :description "ID of the diff to apply"}}
                  :required ["diff_id"]}
    :handler handle-apply-diff}

   {:name "reject_diff"
    :description "Reject a proposed diff without applying. Use when the proposed change is incorrect, unnecessary, or conflicts with other changes."
    :inputSchema {:type "object"
                  :properties {"diff_id" {:type "string"
                                          :description "ID of the diff to reject"}
                               "reason" {:type "string"
                                         :description "Optional reason for rejection (helpful for drone learning)"}}
                  :required ["diff_id"]}
    :handler handle-reject-diff}

   {:name "get_diff_details"
    :description "Get full details of a specific diff including old and new content. Use when you need to see the complete content, not just the unified diff."
    :inputSchema {:type "object"
                  :properties {"diff_id" {:type "string"
                                          :description "ID of the diff to inspect"}}
                  :required ["diff_id"]}
    :handler handle-get-diff-details}

   {:name "review_wave_diffs"
    :description "Review all diffs proposed by a wave. Returns summary with auto-approve analysis. Use this after dispatch_validated_wave to see what changes drones proposed."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to review diffs for"}}
                  :required ["wave_id"]}
    :handler handle-review-wave-diffs}

   {:name "approve_wave_diffs"
    :description "Approve and apply all or selected diffs from a wave. Call after reviewing with review_wave_diffs."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to approve diffs for"}
                               "diff_ids" {:type "array"
                                           :items {:type "string"}
                                           :description "Optional: specific diff IDs to approve (omit for all)"}}
                  :required ["wave_id"]}
    :handler handle-approve-wave-diffs}

   {:name "reject_wave_diffs"
    :description "Reject all diffs from a wave without applying. Use when the wave produced incorrect changes."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to reject diffs for"}
                               "reason" {:type "string"
                                         :description "Reason for rejection (helpful for learning)"}}
                  :required ["wave_id"]}
    :handler handle-reject-wave-diffs}

   {:name "auto_approve_wave_diffs"
    :description "Auto-approve diffs meeting criteria, flag others for manual review. Uses configurable rules (max lines, no deletions-only, etc.)."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to process"}}
                  :required ["wave_id"]}
    :handler handle-auto-approve-wave-diffs}

   {:name "batch_review_diffs"
    :description "Get all pending diffs from multiple drones for batch review. Returns diffs sorted by timestamp with auto-approve analysis."
    :inputSchema {:type "object"
                  :properties {"drone_ids" {:type "array"
                                            :items {:type "string"}
                                            :description "Optional: drone IDs to filter (omit for all pending diffs)"}}
                  :required []}
    :handler handle-batch-review-diffs}

   {:name "approve_safe_diffs"
    :description "Auto-approve diffs from multiple drones that meet safety criteria. Diffs not meeting criteria are flagged for manual review."
    :inputSchema {:type "object"
                  :properties {"drone_ids" {:type "array"
                                            :items {:type "string"}
                                            :description "Optional: drone IDs to filter (omit for all pending diffs)"}
                               "dry_run" {:type "boolean"
                                          :description "If true, only report what would be approved without actually applying"}}
                  :required []}
    :handler handle-approve-safe-diffs}

   {:name "get_auto_approve_rules"
    :description "Get current auto-approve rules configuration. Shows max-lines-changed, no-deletions-only, require-description, and allowed-path-patterns."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-get-auto-approve-rules}])
