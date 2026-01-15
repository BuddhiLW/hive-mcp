(ns hive-mcp.tools.swarm.jvm.orphan
  "Orphan process detection for JVM process management.

   Pattern: Higher-order function composition for orphan detection.
   Predicates can be composed to create custom detection strategies.

   CLARITY: Single responsibility - orphan logic only, no I/O."
  (:require [hive-mcp.tools.swarm.jvm.parser :as parser]))

;;; ============================================================
;;; Parent Info Enrichment
;;; ============================================================

(defn enrich-with-parent-info
  "Enrich process with parent info from pre-fetched map.

   all-parents: Map of pid -> {:ppid :comm}
   Returns process with added keys:
     :parent-alive - boolean
     :parent-comm - parent command name
     :parent-is-claude - boolean
     :truly-orphaned - boolean (parent dead or PID 1)"
  [proc all-parents]
  (let [ppid (:ppid proc)
        parent (get all-parents ppid)
        parent-alive (boolean parent)
        parent-comm (:comm parent)
        is-init (= "1" ppid)
        is-claude (= "claude" parent-comm)
        truly-orphaned (or (not parent-alive) is-init)]
    (assoc proc
           :parent-alive parent-alive
           :parent-comm parent-comm
           :parent-is-claude is-claude
           :truly-orphaned truly-orphaned)))

;;; ============================================================
;;; Orphan Detection Predicates
;;; ============================================================

(defn protected-type?
  "Check if process type is in protected set."
  [proc protected-types]
  (contains? protected-types (name (:type proc))))

(defn age-orphan?
  "Check if process is old enough to be considered age-orphaned."
  [proc min-age-minutes]
  (let [age (parser/parse-etime-to-minutes (:etime proc))]
    (>= age min-age-minutes)))

(defn truly-orphaned?
  "Check if process is truly orphaned (parent dead or reparented to init)."
  [proc]
  (:truly-orphaned proc))

;;; ============================================================
;;; Orphan Detector (Higher-Order Function)
;;; ============================================================

(defn orphan-detector
  "Create an orphan detector function with configurable predicates.

   Options:
     :protected-types - Set of type names to never mark as orphan
     :true-orphans-only - Only detect truly orphaned (parent dead)
     :min-age-minutes - Minimum age for age-based orphan detection

   Returns: (fn [proc] -> boolean) - true if proc is orphan

   Pattern: Higher-order function composition"
  [& {:keys [protected-types true-orphans-only min-age-minutes]
      :or {protected-types #{}
           true-orphans-only true
           min-age-minutes 30}}]
  (fn [proc]
    (cond
      ;; Protected types are never orphans
      (protected-type? proc protected-types)
      false

      ;; True orphan mode: only detect if parent is dead
      true-orphans-only
      (truly-orphaned? proc)

      ;; Age-based mode: truly orphaned AND old enough
      :else
      (and (truly-orphaned? proc)
           (age-orphan? proc min-age-minutes)))))

;;; ============================================================
;;; Orphan Identification (Full Result)
;;; ============================================================

(defn identify-orphan
  "Identify if a process is an orphan and compute reason.

   Returns process with added keys:
     :orphan - boolean
     :age-minutes - parsed age
     :reason - human-readable explanation

   Options same as orphan-detector."
  [proc & {:keys [protected-types true-orphans-only min-age-minutes]
           :or {protected-types #{}
                true-orphans-only true
                min-age-minutes 30}}]
  (let [detector (orphan-detector :protected-types protected-types
                                  :true-orphans-only true-orphans-only
                                  :min-age-minutes min-age-minutes)
        age (parser/parse-etime-to-minutes (:etime proc))
        is-protected (protected-type? proc protected-types)
        is-orphan (detector proc)
        reason (cond
                 is-protected "protected-type"
                 (truly-orphaned? proc) (str "truly-orphaned (parent: "
                                              (or (:parent-comm proc) "dead") ")")
                 (:parent-is-claude proc) (str "managed-by-claude (ppid: "
                                                (:ppid proc) ")")
                 :else (str "has-parent: " (:parent-comm proc)))]
    (assoc proc
           :orphan is-orphan
           :age-minutes age
           :reason reason)))

(defn identify-orphans
  "Identify orphans in a collection of processes.

   Applies identify-orphan to each process with shared options."
  [procs & {:keys [protected-types true-orphans-only min-age-minutes]
            :or {protected-types #{}
                 true-orphans-only true
                 min-age-minutes 30}}]
  (map #(identify-orphan %
                         :protected-types protected-types
                         :true-orphans-only true-orphans-only
                         :min-age-minutes min-age-minutes)
       procs))
