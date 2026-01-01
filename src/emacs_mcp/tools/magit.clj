(ns emacs-mcp.tools.magit
  "Magit integration handlers for MCP.

   Provides comprehensive git operations via magit addon:
   - Status, branches, log, diff
   - Stage, commit, push, pull, fetch
   - Feature branch listing for /ship and /ship-pr skills"
  (:require [emacs-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [emacs-mcp.emacsclient :as ec]
            [emacs-mcp.elisp :as el]
            [taoensso.timbre :as log]))

;; ============================================================
;; Magit Integration Tools (requires emacs-mcp-magit addon)
;; ============================================================

(defn magit-addon-available?
  "Check if the magit addon is loaded in Emacs."
  []
  (let [elisp "(progn
                (require 'emacs-mcp-magit nil t)
                (if (featurep 'emacs-mcp-magit) t nil))"
        {:keys [success result]} (ec/eval-elisp elisp)]
    (and success (= result "t"))))

(defn handle-magit-status
  "Get comprehensive git repository status via magit addon."
  [_]
  (log/info "magit-status")
  (let [elisp (el/require-and-call-json 'emacs-mcp-magit 'emacs-mcp-magit-api-status)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-branches
  "Get branch information including current, upstream, local and remote branches."
  [_]
  (log/info "magit-branches")
  (let [elisp (el/require-and-call-json 'emacs-mcp-magit 'emacs-mcp-magit-api-branches)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-log
  "Get recent commit log."
  [{:keys [count]}]
  (log/info "magit-log" {:count count})
  (let [n (or count 10)
        elisp (el/require-and-call-json 'emacs-mcp-magit 'emacs-mcp-magit-api-log n)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-diff
  "Get diff for staged, unstaged, or all changes."
  [{:keys [target]}]
  (log/info "magit-diff" {:target target})
  (let [target-sym (case target
                     "staged" 'staged
                     "unstaged" 'unstaged
                     "all" 'all
                     'staged)
        elisp (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-diff target-sym)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-stage
  "Stage files for commit. Use 'all' to stage all modified files."
  [{:keys [files]}]
  (log/info "magit-stage" {:files files})
  (let [file-arg (if (= files "all") 'all files)
        elisp (el/require-and-call 'emacs-mcp-magit 'emacs-mcp-magit-api-stage file-arg)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success (or result "Staged files"))
      (mcp-error (str "Error: " error)))))

(defn handle-magit-commit
  "Create a commit with the given message."
  [{:keys [message all]}]
  (log/info "magit-commit" {:message-len (count message) :all all})
  (let [options (if all "'(:all t)" "nil")
        elisp (el/format-elisp
               "(progn
                  (require 'emacs-mcp-magit nil t)
                  (if (fboundp 'emacs-mcp-magit-api-commit)
                      (emacs-mcp-magit-api-commit %s %s)
                    \"emacs-mcp-magit not loaded\"))"
               (pr-str message) options)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-push
  "Push to remote. Optionally set upstream tracking."
  [{:keys [set_upstream]}]
  (log/info "magit-push" {:set_upstream set_upstream})
  (let [options (if set_upstream "'(:set-upstream t)" "nil")
        elisp (el/format-elisp
               "(progn
                  (require 'emacs-mcp-magit nil t)
                  (if (fboundp 'emacs-mcp-magit-api-push)
                      (emacs-mcp-magit-api-push %s)
                    \"emacs-mcp-magit not loaded\"))"
               options)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-pull
  "Pull from upstream."
  [_]
  (log/info "magit-pull")
  (let [elisp (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-pull)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-fetch
  "Fetch from remote(s)."
  [{:keys [remote]}]
  (log/info "magit-fetch" {:remote remote})
  (let [elisp (if remote
                (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-fetch remote)
                (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-fetch))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-magit-feature-branches
  "Get list of feature/fix/feat branches (for /ship and /ship-pr skills)."
  [_]
  (log/info "magit-feature-branches")
  ;; Complex elisp with client-side filtering - use format-elisp
  (let [elisp (el/format-elisp
               "(progn
                  (require 'emacs-mcp-magit nil t)
                  (if (fboundp 'emacs-mcp-magit-api-branches)
                      (let* ((branches (emacs-mcp-magit-api-branches))
                             (local (plist-get branches :local))
                             (feature-branches
                               (seq-filter
                                 (lambda (b)
                                   (string-match-p \"^\\\\(feature\\\\|fix\\\\|feat\\\\)/\" b))
                                 local)))
                        (json-encode (list :current (plist-get branches :current)
                                           :feature_branches feature-branches)))
                    (json-encode (list :error \"emacs-mcp-magit not loaded\"))))")
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

;; Tool definitions for magit handlers

(def tools
  [{:name "magit_status"
    :description "Get comprehensive git repository status including branch, staged/unstaged/untracked files, ahead/behind counts, stashes, and recent commits. Requires emacs-mcp-magit addon."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-status}

   {:name "magit_branches"
    :description "Get branch information including current branch, upstream, all local branches, and remote branches."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-branches}

   {:name "magit_log"
    :description "Get recent commit log with hash, author, date, and subject."
    :inputSchema {:type "object"
                  :properties {"count" {:type "integer"
                                        :description "Number of commits to return (default: 10)"}}
                  :required []}
    :handler handle-magit-log}

   {:name "magit_diff"
    :description "Get diff for staged, unstaged, or all changes."
    :inputSchema {:type "object"
                  :properties {"target" {:type "string"
                                         :enum ["staged" "unstaged" "all"]
                                         :description "What to diff (default: staged)"}}
                  :required []}
    :handler handle-magit-diff}

   {:name "magit_stage"
    :description "Stage files for commit. Use 'all' to stage all modified files, or provide a file path."
    :inputSchema {:type "object"
                  :properties {"files" {:type "string"
                                        :description "File path to stage, or 'all' for all modified files"}}
                  :required ["files"]}
    :handler handle-magit-stage}

   {:name "magit_commit"
    :description "Create a git commit with the given message."
    :inputSchema {:type "object"
                  :properties {"message" {:type "string"
                                          :description "Commit message"}
                               "all" {:type "boolean"
                                      :description "If true, stage all changes before committing"}}
                  :required ["message"]}
    :handler handle-magit-commit}

   {:name "magit_push"
    :description "Push to remote. Optionally set upstream tracking for new branches."
    :inputSchema {:type "object"
                  :properties {"set_upstream" {:type "boolean"
                                               :description "Set upstream tracking if not already set"}}
                  :required []}
    :handler handle-magit-push}

   {:name "magit_pull"
    :description "Pull from upstream remote."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-pull}

   {:name "magit_fetch"
    :description "Fetch from remote(s). Fetches all remotes if no specific remote is provided."
    :inputSchema {:type "object"
                  :properties {"remote" {:type "string"
                                         :description "Specific remote to fetch from (optional)"}}
                  :required []}
    :handler handle-magit-fetch}

   {:name "magit_feature_branches"
    :description "Get list of feature/fix/feat branches for shipping. Used by /ship and /ship-pr skills."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-feature-branches}])
