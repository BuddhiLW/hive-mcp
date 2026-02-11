(ns hive-mcp.tools.diff.auto-approve
  "Auto-approve rules and validation for diff proposals."
  (:require [hive-mcp.tools.diff.state :as state]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- count-line-changes
  "Count total lines changed (added + deleted) in a diff."
  [old-content new-content]
  (let [old-lines (count (str/split-lines (or old-content "")))
        new-lines (count (str/split-lines (or new-content "")))]
    (+ (max 0 (- old-lines new-lines))
       (max 0 (- new-lines old-lines)))))

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
    true
    (some #(re-matches % file-path) patterns)))

(defn auto-approve-diff?
  "Check if a diff meets auto-approve criteria."
  ([diff] (auto-approve-diff? diff @state/auto-approve-rules))
  ([{:keys [old-content new-content file-path description]} rules]
   (let [{:keys [max-lines-changed no-deletions-only
                 require-description allowed-path-patterns]} rules
         line-changes (count-line-changes old-content new-content)]
     (cond
       (and max-lines-changed (> line-changes max-lines-changed))
       {:approved false
        :reason (str "Too many lines changed: " line-changes " > " max-lines-changed)}

       (and no-deletions-only (deletions-only? old-content new-content))
       {:approved false
        :reason "Change only deletes content - requires manual review"}

       (and require-description (str/blank? description))
       {:approved false
        :reason "Missing description - requires manual review"}

       (and (seq allowed-path-patterns)
            (not (path-matches-patterns? file-path allowed-path-patterns)))
       {:approved false
        :reason (str "File path not in allowed patterns: " file-path)}

       :else
       {:approved true}))))

(defn get-auto-approve-rules
  "Get current auto-approve rules."
  []
  (let [rules @state/auto-approve-rules]
    {:max-lines-changed (:max-lines-changed rules)
     :must-pass-lint false
     :no-deletions-only (:no-deletions-only rules)
     :require-description (:require-description rules)
     :allowed-path-patterns (mapv str (:allowed-path-patterns rules))}))

(defn safe-to-auto-approve?
  "Return true if diff can be safely auto-approved."
  ([diff] (safe-to-auto-approve? diff @state/auto-approve-rules))
  ([diff rules]
   (:approved (auto-approve-diff? diff rules))))
