(ns hive-mcp.tools.catchup.git
  "Git context gathering for catchup workflow."
  (:require [hive-mcp.emacs.client :as ec]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn gather-git-info
  "Gather git information from Emacs for the given directory."
  [directory]
  (try
    (let [git-elisp (if directory
                      (format "(let ((default-directory %s))
                                 (json-encode
                                  (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                                        :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                                        :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%%h - %%s' 2>/dev/null || echo 'none'\")))))"
                              (pr-str directory))
                      "(json-encode
                         (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                               :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                               :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%h - %s' 2>/dev/null || echo 'none'\"))))")
          {:keys [success result timed-out]} (ec/eval-elisp-with-timeout git-elisp 30000)]
      (when (and success (not timed-out))
        (json/read-str result :key-fn keyword)))
    (catch Exception _
      {:branch "unknown" :uncommitted false :last-commit "unknown"})))
