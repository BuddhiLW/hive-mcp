(ns hive-mcp.tools.memory.duration
  "Duration management for memory entries.
   
   SOLID: SRP - Single responsibility for duration calculations.
   CLARITY: R - Represented intent with clear duration semantics."
  (:import [java.time ZonedDateTime]))

;; ============================================================
;; Constants
;; ============================================================

(def duration-order
  "Duration categories in order from shortest to longest.
   Used for promote/demote operations."
  ["ephemeral" "short" "medium" "long" "permanent"])

(def duration-days
  "Duration category to days mapping. nil = permanent (never expires)."
  {"ephemeral" 1
   "short" 7
   "medium" 30
   "long" 90
   "permanent" nil})

;; ============================================================
;; Functions
;; ============================================================

(defn calculate-expires
  "Calculate expiration timestamp for given duration.
   Returns nil for 'permanent' duration."
  [duration]
  (when-let [days (get duration-days (or duration "long"))]
    (str (.plusDays (ZonedDateTime/now) days))))

(defn shift-duration
  "Shift duration by delta steps (+1 = promote, -1 = demote).
   Returns {:new-duration str :changed? bool :at-boundary? bool}
   
   Examples:
     (shift-duration \"short\" +1) => {:new-duration \"medium\" :changed? true}
     (shift-duration \"permanent\" +1) => {:new-duration \"permanent\" :changed? false :at-boundary? true}"
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
