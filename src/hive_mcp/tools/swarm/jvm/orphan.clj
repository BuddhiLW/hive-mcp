(ns hive-mcp.tools.swarm.jvm.orphan
  "Orphan process detection using composable predicates."
  (:require [hive-mcp.tools.swarm.jvm.parser :as parser]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn enrich-with-parent-info
  "Enrich process with parent info from pre-fetched map."
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

(defn orphan-detector
  "Create an orphan detector function with configurable predicates."
  [& {:keys [protected-types true-orphans-only min-age-minutes]
      :or {protected-types #{}
           true-orphans-only true
           min-age-minutes 30}}]
  (fn [proc]
    (cond
      (protected-type? proc protected-types)
      false

      true-orphans-only
      (truly-orphaned? proc)

      :else
      (and (truly-orphaned? proc)
           (age-orphan? proc min-age-minutes)))))

(defn identify-orphan
  "Identify if a process is an orphan and compute reason."
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
  "Identify orphans in a collection of processes."
  [procs & {:keys [protected-types true-orphans-only min-age-minutes]
            :or {protected-types #{}
                 true-orphans-only true
                 min-age-minutes 30}}]
  (map #(identify-orphan %
                         :protected-types protected-types
                         :true-orphans-only true-orphans-only
                         :min-age-minutes min-age-minutes)
       procs))
