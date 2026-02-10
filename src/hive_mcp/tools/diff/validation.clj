(ns hive-mcp.tools.diff.validation
  "Diff path validation and parameter checking.

   Handles sandbox path translation, project root resolution,
   and path escape detection.

   SOLID: SRP - All validation logic in one place.
   CLARITY-Y: Yield safe failure with descriptive error messages."
  (:require [hive-mcp.emacs.client :as ec]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
