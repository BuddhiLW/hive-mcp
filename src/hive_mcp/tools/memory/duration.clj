(ns hive-mcp.tools.memory.duration
  "Duration management for memory entries."
  (:import [java.time ZonedDateTime]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(def duration-order
  "Duration categories ordered from shortest to longest."
  ["ephemeral" "short" "medium" "long" "permanent"])

(def duration-days
  "Duration category to days mapping; nil means permanent."
  {"ephemeral" 1
   "short" 7
   "medium" 30
   "long" 90
   "permanent" nil})

(defn calculate-expires
  "Calculate expiration timestamp for given duration."
  [duration]
  (when-let [days (get duration-days (or duration "long"))]
    (str (.plusDays (ZonedDateTime/now) days))))

(defn shift-duration
  "Shift duration by delta steps (+1 = promote, -1 = demote)."
  [current-duration delta]
  (let [current (or current-duration "long")
        idx (.indexOf duration-order current)
        new-idx (cond
                  (pos? delta) (min (+ idx delta) (dec (count duration-order)))
                  (neg? delta) (max (+ idx delta) 0)
                  :else idx)
        new-dur (nth duration-order new-idx)]
    {:new-duration new-dur
     :changed? (not= new-dur current)
     :at-boundary? (= new-idx idx)}))

(defn valid-duration?
  "Check if duration string is valid."
  [duration]
  (contains? (set duration-order) duration))
