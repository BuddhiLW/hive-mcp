(ns hive-mcp.tools.magit
  "Magit integration handlers for MCP.

   Provides comprehensive git operations via magit addon:
   - Status, branches, log, diff
   - Stage, commit, push, pull, fetch
   - Feature branch listing for /ship and /ship-pr skills"
  (:require [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.elisp :as el]
            [taoensso.timbre :as log]))

;; ============================================================
;; Working Directory Resolution
;; ============================================================
;;
;; When Claude CLI spawns in a project directory, magit tools should
;; operate on that project by default - not on whatever buffer is
;; active in Emacs. We use the MCP server's working directory as the
;; default when no explicit directory is provided.

(defn- resolve-directory
  "Resolve the directory to use for git operations.
   Uses provided directory or falls back to MCP server's working directory."
  [directory]
  (or directory (System/getProperty "user.dir")))

;; ============================================================
;; Magit Integration Tools (requires hive-mcp-magit addon)
;; ============================================================

(defn magit-addon-available?
  "Check if the magit addon is loaded in Emacs."
  []
  (let [elisp "(progn
                (require 'hive-mcp-magit nil t)
                (if (featurep 'hive-mcp-magit) t nil))"
        {:keys [success result]} (ec/eval-elisp elisp)]
    (and success (= result "t"))))

(defn handle-magit-status
  "Get comprehensive git repository status via magit addon."
  [{:keys [directory]}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-status" {:directory dir})
    (let [elisp (el/require-and-call-json 'hive-mcp-magit 'hive-mcp-magit-api-status dir)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-branches
  "Get branch information including current, upstream, local and remote branches."
  [{:keys [directory]}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-branches" {:directory dir})
    (let [elisp (el/require-and-call-json 'hive-mcp-magit 'hive-mcp-magit-api-branches dir)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-log
  "Get recent commit log."
  [{:keys [count directory]}]
  (let [dir (resolve-directory directory)
        n (or count 10)]
    (log/info "magit-log" {:count n :directory dir})
    (let [elisp (el/require-and-call-json 'hive-mcp-magit 'hive-mcp-magit-api-log n dir)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-diff
  "Get diff for staged, unstaged, or all changes."
  [{:keys [target directory]}]
  (let [dir (resolve-directory directory)
        target-sym (case target
                     "staged" 'staged
                     "unstaged" 'unstaged
                     "all" 'all
                     'staged)]
    (log/info "magit-diff" {:target target :directory dir})
    (let [elisp (el/require-and-call-text 'hive-mcp-magit 'hive-mcp-magit-api-diff target-sym dir)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-stage
  "Stage files for commit. Use 'all' to stage all modified files."
  [{:keys [files directory]}]
  (let [dir (resolve-directory directory)
        file-arg (if (= files "all") 'all files)]
    (log/info "magit-stage" {:files files :directory dir})
    (let [elisp (el/require-and-call 'hive-mcp-magit 'hive-mcp-magit-api-stage file-arg dir)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success (or result "Staged files"))
        (mcp-error (str "Error: " error))))))

(defn handle-magit-commit
  "Create a commit with the given message."
  [{:keys [message all directory]}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-commit" {:message-len (count message) :all all :directory dir})
    (let [options (if all "'(:all t)" "nil")
          dir-arg (pr-str dir)
          elisp (el/format-elisp
                 "(progn
                    (require 'hive-mcp-magit nil t)
                    (if (fboundp 'hive-mcp-magit-api-commit)
                        (hive-mcp-magit-api-commit %s %s %s)
                      \"hive-mcp-magit not loaded\"))"
                 (pr-str message) options dir-arg)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-push
  "Push to remote. Optionally set upstream tracking."
  [{:keys [set_upstream directory]}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-push" {:set_upstream set_upstream :directory dir})
    (let [options (if set_upstream "'(:set-upstream t)" "nil")
          dir-arg (pr-str dir)
          elisp (el/format-elisp
                 "(progn
                    (require 'hive-mcp-magit nil t)
                    (if (fboundp 'hive-mcp-magit-api-push)
                        (hive-mcp-magit-api-push %s %s)
                      \"hive-mcp-magit not loaded\"))"
                 options dir-arg)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-pull
  "Pull from upstream."
  [{:keys [directory]}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-pull" {:directory dir})
    (let [elisp (el/require-and-call-text 'hive-mcp-magit 'hive-mcp-magit-api-pull dir)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-fetch
  "Fetch from remote(s)."
  [{:keys [remote directory]}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-fetch" {:remote remote :directory dir})
    (let [elisp (if remote
                  (el/require-and-call-text 'hive-mcp-magit 'hive-mcp-magit-api-fetch remote dir)
                  (el/require-and-call-text 'hive-mcp-magit 'hive-mcp-magit-api-fetch nil dir))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

(defn handle-magit-feature-branches
  "Get list of feature/fix/feat branches (for /ship and /ship-pr skills)."
  [{:keys [directory]}]
  (let [dir (resolve-directory directory)]
    (log/info "magit-feature-branches" {:directory dir})
    ;; Complex elisp with client-side filtering - use format-elisp
    (let [dir-arg (pr-str dir)
          elisp (el/format-elisp
                 "(progn
                    (require 'hive-mcp-magit nil t)
                    (if (fboundp 'hive-mcp-magit-api-branches)
                        (let* ((default-directory %s)
                               (branches (hive-mcp-magit-api-branches default-directory))
                               (local (plist-get branches :local))
                               (feature-branches
                                 (seq-filter
                                   (lambda (b)
                                     (string-match-p \"^\\\\(feature\\\\|fix\\\\|feat\\\\)/\" b))
                                   local)))
                          (json-encode (list :current (plist-get branches :current)
                                             :feature_branches feature-branches)))
                      (json-encode (list :error \"hive-mcp-magit not loaded\"))))"
                 dir-arg)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))))

;; Tool definitions for magit handlers

;; IMPORTANT: When using a shared hive-mcp server across multiple projects,
;; Claude should ALWAYS pass its current working directory to these tools.
;; The directory can be found in Claude's prompt (e.g., ~/PP/funeraria/sisf-web)
;; or by running `pwd` in bash.

(def ^:private dir-desc
  "IMPORTANT: Pass your current working directory here to ensure git operations target YOUR project, not the MCP server's directory. Get it from your prompt path or run `pwd`.")

(def tools
  [{:name "magit_status"
    :description "Get comprehensive git repository status including branch, staged/unstaged/untracked files, ahead/behind counts, stashes, and recent commits. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-status}

   {:name "magit_branches"
    :description "Get branch information including current branch, upstream, all local branches, and remote branches. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-branches}

   {:name "magit_log"
    :description "Get recent commit log with hash, author, date, and subject. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"count" {:type "integer"
                                        :description "Number of commits to return (default: 10)"}
                               "directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-log}

   {:name "magit_diff"
    :description "Get diff for staged, unstaged, or all changes. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"target" {:type "string"
                                         :enum ["staged" "unstaged" "all"]
                                         :description "What to diff (default: staged)"}
                               "directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-diff}

   {:name "magit_stage"
    :description "Stage files for commit. Use 'all' to stage all modified files, or provide a file path. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"files" {:type "string"
                                        :description "File path to stage, or 'all' for all modified files"}
                               "directory" {:type "string"
                                            :description dir-desc}}
                  :required ["files"]}
    :handler handle-magit-stage}

   {:name "magit_commit"
    :description "Create a git commit with the given message. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"message" {:type "string"
                                          :description "Commit message"}
                               "all" {:type "boolean"
                                      :description "If true, stage all changes before committing"}
                               "directory" {:type "string"
                                            :description dir-desc}}
                  :required ["message"]}
    :handler handle-magit-commit}

   {:name "magit_push"
    :description "Push to remote. Optionally set upstream tracking for new branches. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"set_upstream" {:type "boolean"
                                               :description "Set upstream tracking if not already set"}
                               "directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-push}

   {:name "magit_pull"
    :description "Pull from upstream remote. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-pull}

   {:name "magit_fetch"
    :description "Fetch from remote(s). Fetches all remotes if no specific remote is provided. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"remote" {:type "string"
                                         :description "Specific remote to fetch from (optional)"}
                               "directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-fetch}

   {:name "magit_feature_branches"
    :description "Get list of feature/fix/feat branches for shipping. Used by /ship and /ship-pr skills. IMPORTANT: Pass your working directory to target your project."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description dir-desc}}
                  :required []}
    :handler handle-magit-feature-branches}])
