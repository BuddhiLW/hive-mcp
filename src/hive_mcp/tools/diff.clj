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
  (:require [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.emacsclient :as ec]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

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
  "Create a diff proposal map from input parameters."
  [{:keys [file_path old_content new_content description drone_id]}]
  (let [diff-id (generate-diff-id)
        unified (compute-unified-diff old_content new_content file_path)]
    {:id diff-id
     :file-path file_path
     :old-content old_content
     :new-content new_content
     :description (or description "No description provided")
     :drone-id (or drone_id "unknown")
     :unified-diff unified
     :status "pending"
     :created-at (java.time.Instant/now)}))

(defn validate-propose-params
  "Validate parameters for propose_diff. Returns nil if valid, error message if not."
  [{:keys [file_path old_content new_content]}]
  (cond
    (str/blank? file_path) "Missing required field: file_path"
    (nil? old_content) "Missing required field: old_content"
    (nil? new_content) "Missing required field: new_content"
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
   
   Returns {:valid true :resolved-path \"...\"} or {:valid false :error \"...\"}."
  [file-path]
  (let [project-root (get-project-root)
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
        {:valid true :resolved-path canonical-path}))))

;; =============================================================================
;; Application: Handlers
;; =============================================================================

(defn handle-propose-diff
  "Handle propose_diff tool call.
   Stores a proposed diff for review by the hivemind.
   Translates sandbox paths and validates file paths."
  [{:keys [file_path old_content new_content description drone_id] :as params}]
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
   If old-content is empty and file doesn't exist, creates a new file."
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
    (let [{:keys [file-path old-content new-content] :as diff} (get @pending-diffs diff_id)
          file-exists? (.exists (io/file file-path))
          creating-new-file? (and (str/blank? old-content) (not file-exists?))]
      (try
        (cond
          ;; Case 1: Creating a new file (old-content empty, file doesn't exist)
          creating-new-file?
          (do
            ;; Ensure parent directory exists
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
                       :message "New file created successfully"}))

          ;; Case 2: File doesn't exist but old-content is not empty - error
          (not file-exists?)
          (do
            (log/warn "apply_diff file not found" {:file file-path})
            (mcp-error-json (str "File not found: " file-path)))

          ;; Case 3: Normal replacement in existing file
          :else
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
              (let [updated-content (str/replace-first current-content old-content new-content)]
                (spit file-path updated-content)
                (swap! pending-diffs dissoc diff_id)
                (log/info "Diff applied" {:id diff_id :file file-path})
                (mcp-json {:id diff_id
                           :status "applied"
                           :file-path file-path
                           :message "Diff applied successfully"})))))
        (catch Exception e
          (log/error e "Failed to apply diff" {:diff_id diff_id})
          (mcp-error-json (str "Failed to apply diff: " (.getMessage e))))))))

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
    :handler handle-get-diff-details}])
