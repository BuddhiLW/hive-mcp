(ns hive-mcp.org-clj.query
  "Query functions for finding and filtering org headlines."
  (:require [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Tree Traversal
;; =============================================================================

(defn flatten-headlines
  "Flatten a nested headline tree into a sequence.
   Visits all headlines depth-first."
  [headlines]
  (mapcat (fn [hl]
            (cons hl (flatten-headlines (:children hl))))
          headlines))

(defn all-headlines
  "Get all headlines from a document as a flat sequence."
  [doc]
  (flatten-headlines (:headlines doc)))

;; =============================================================================
;; Query by ID
;; =============================================================================

(defn find-by-id
  "Find a headline by its :ID property.
   Searches the entire document tree."
  [doc id]
  (first (filter #(= (get-in % [:properties :ID]) id)
                 (all-headlines doc))))

(defn find-by-vibe-id
  "Find a headline by its :VIBE_ID property."
  [doc vibe-id]
  (first (filter #(= (get-in % [:properties :VIBE_ID]) vibe-id)
                 (all-headlines doc))))

;; =============================================================================
;; Query by Status
;; =============================================================================

(defn find-by-status
  "Find all headlines with a specific TODO keyword.
   Pass nil to find headlines without any keyword."
  [doc status]
  (filter #(= (:keyword %) status)
          (all-headlines doc)))

(defn find-todo
  "Find all TODO items (headlines with keyword TODO)."
  [doc]
  (find-by-status doc "TODO"))

(defn find-done
  "Find all DONE items."
  [doc]
  (find-by-status doc "DONE"))

(defn find-in-progress
  "Find all IN-PROGRESS items."
  [doc]
  (find-by-status doc "IN-PROGRESS"))

;; =============================================================================
;; Query by Property
;; =============================================================================

(defn find-by-property
  "Find headlines where a property matches a value.
   
   Examples:
     (find-by-property doc :EFFORT \"M\")
     (find-by-property doc :PRIORITY \"high\")"
  [doc prop-key prop-value]
  (filter #(= (get-in % [:properties prop-key]) prop-value)
          (all-headlines doc)))

(defn find-by-property-exists
  "Find headlines that have a specific property (any value)."
  [doc prop-key]
  (filter #(contains? (:properties %) prop-key)
          (all-headlines doc)))

;; =============================================================================
;; Query by Level
;; =============================================================================

(defn find-by-level
  "Find all headlines at a specific level."
  [doc level]
  (filter #(= (:level %) level)
          (all-headlines doc)))

(defn find-tasks
  "Find all task-level headlines (level 2 by default).
   These are typically the actionable items in a kanban."
  ([doc] (find-tasks doc 2))
  ([doc level] (find-by-level doc level)))

;; =============================================================================
;; Query by Tags
;; =============================================================================

(defn find-by-tag
  "Find all headlines with a specific tag."
  [doc tag]
  (filter #(some #{tag} (:tags %))
          (all-headlines doc)))

(defn find-by-tags
  "Find headlines that have ALL specified tags."
  [doc tags]
  (filter #(every? (set (:tags %)) tags)
          (all-headlines doc)))

;; =============================================================================
;; Predicate-based Query
;; =============================================================================

(defn find-where
  "Find all headlines matching a predicate function.
   
   Example:
     (find-where doc #(and (= (:keyword %) \"TODO\")
                           (= (:level %) 2)))"
  [doc pred]
  (filter pred (all-headlines doc)))

;; =============================================================================
;; Statistics
;; =============================================================================

(defn status-counts
  "Get count of headlines by TODO keyword."
  [doc]
  (frequencies (map :keyword (all-headlines doc))))

(defn task-stats
  "Get task statistics for kanban-style reporting."
  [doc]
  (let [tasks (find-tasks doc)
        by-status (group-by :keyword tasks)]
    {:total (count tasks)
     :todo (count (get by-status "TODO" []))
     :in-progress (count (get by-status "IN-PROGRESS" []))
     :in-review (count (get by-status "IN-REVIEW" []))
     :done (count (get by-status "DONE" []))
     :cancelled (count (get by-status "CANCELLED" []))}))

;; =============================================================================
;; Tests
;; =============================================================================

(deftest test-flatten-headlines
  (testing "Flattens nested headlines"
    (let [headlines [{:title "A" :children [{:title "A1" :children []}
                                            {:title "A2" :children []}]}
                     {:title "B" :children []}]]
      (is (= ["A" "A1" "A2" "B"]
             (map :title (flatten-headlines headlines)))))))

(deftest test-find-by-status
  (testing "Finds by TODO keyword"
    (let [doc {:headlines [{:keyword "TODO" :title "Task 1" :children []}
                           {:keyword "DONE" :title "Task 2" :children []}
                           {:keyword "TODO" :title "Task 3" :children []}]}]
      (is (= 2 (count (find-by-status doc "TODO"))))
      (is (= 1 (count (find-by-status doc "DONE")))))))

(deftest test-find-by-property
  (testing "Finds by property value"
    (let [doc {:headlines [{:properties {:ID "a"} :children []}
                           {:properties {:ID "b"} :children []}]}]
      (is (= "a" (get-in (first (find-by-property doc :ID "a")) [:properties :ID]))))))

(comment
  ;; Example usage with kanban.org
  (require '[hive-mcp.org-clj.parser :as parser])
  (def doc (parser/parse-document (slurp "kanban.org")))

  (count (all-headlines doc))
  (find-todo doc)
  (find-done doc)
  (task-stats doc)
  (find-by-property doc :EFFORT "M"))
