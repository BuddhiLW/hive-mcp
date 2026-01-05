(ns hive-mcp.org-clj.transform
  "Transform functions for modifying org documents immutably."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.org-clj.query :as query]))

;; =============================================================================
;; Headline Tree Updates
;; =============================================================================

(defn update-headline-in-tree
  "Update a headline in a tree by applying f to matching headlines.
   pred is a function that returns true for headlines to update.
   f is applied to each matching headline."
  [headlines pred f]
  (mapv (fn [hl]
          (let [updated (if (pred hl) (f hl) hl)
                children (:children updated)]
            (if (seq children)
              (assoc updated :children (update-headline-in-tree children pred f))
              updated)))
        headlines))

(defn update-in-document
  "Update headlines in a document matching pred by applying f."
  [doc pred f]
  (update doc :headlines update-headline-in-tree pred f))

;; =============================================================================
;; Update by ID
;; =============================================================================

(defn update-by-id
  "Update a headline by its :ID property.
   
   Example:
     (update-by-id doc \"task-123\" assoc :keyword \"DONE\")"
  [doc id f & args]
  (update-in-document doc
                      #(= (get-in % [:properties :ID]) id)
                      #(apply f % args)))

(defn update-by-vibe-id
  "Update a headline by its :VIBE_ID property."
  [doc vibe-id f & args]
  (update-in-document doc
                      #(= (get-in % [:properties :VIBE_ID]) vibe-id)
                      #(apply f % args)))

;; =============================================================================
;; Status Changes
;; =============================================================================

(defn set-status
  "Set the TODO keyword of a headline by ID.
   
   Example:
     (set-status doc \"task-123\" \"DONE\")"
  [doc id new-status]
  (update-by-id doc id assoc :keyword new-status))

(defn set-status-by-vibe-id
  "Set the TODO keyword of a headline by VIBE_ID."
  [doc vibe-id new-status]
  (update-by-vibe-id doc vibe-id assoc :keyword new-status))

(defn mark-done
  "Mark a task as DONE by ID."
  [doc id]
  (set-status doc id "DONE"))

(defn mark-todo
  "Mark a task as TODO by ID."
  [doc id]
  (set-status doc id "TODO"))

(defn mark-in-progress
  "Mark a task as IN-PROGRESS by ID."
  [doc id]
  (set-status doc id "IN-PROGRESS"))

;; =============================================================================
;; Property Updates
;; =============================================================================

(defn set-property
  "Set or update a property on a headline by ID.
   
   Example:
     (set-property doc \"task-123\" :EFFORT \"L\")"
  [doc id prop-key prop-value]
  (update-by-id doc id assoc-in [:properties prop-key] prop-value))

(defn remove-property
  "Remove a property from a headline by ID."
  [doc id prop-key]
  (update-by-id doc id update :properties dissoc prop-key))

;; =============================================================================
;; Title and Content Updates
;; =============================================================================

(defn set-title
  "Set the title of a headline by ID."
  [doc id new-title]
  (update-by-id doc id assoc :title new-title))

(defn append-content
  "Append content lines to a headline by ID."
  [doc id new-content]
  (update-by-id doc id update :content
                (fnil into [])
                (if (string? new-content) [new-content] new-content)))

;; =============================================================================
;; Tag Operations
;; =============================================================================

(defn add-tag
  "Add a tag to a headline by ID."
  [doc id tag]
  (update-by-id doc id update :tags (fnil conj []) tag))

(defn remove-tag
  "Remove a tag from a headline by ID."
  [doc id tag]
  (update-by-id doc id update :tags #(vec (remove #{tag} %))))

(defn set-tags
  "Replace all tags on a headline by ID."
  [doc id tags]
  (update-by-id doc id assoc :tags (vec tags)))

;; =============================================================================
;; Planning Updates
;; =============================================================================

(defn set-closed
  "Set the CLOSED timestamp on a headline by ID."
  [doc id timestamp]
  (update-by-id doc id assoc-in [:planning :closed] timestamp))

(defn set-scheduled
  "Set the SCHEDULED timestamp on a headline by ID."
  [doc id timestamp]
  (update-by-id doc id assoc-in [:planning :scheduled] timestamp))

(defn set-deadline
  "Set the DEADLINE timestamp on a headline by ID."
  [doc id timestamp]
  (update-by-id doc id assoc-in [:planning :deadline] timestamp))

;; =============================================================================
;; Bulk Operations
;; =============================================================================

(defn mark-all-done
  "Mark all headlines matching a predicate as DONE."
  [doc pred]
  (update-in-document doc pred #(assoc % :keyword "DONE")))

(defn update-all-matching
  "Apply an update function to all headlines matching a predicate."
  [doc pred f]
  (update-in-document doc pred f))

;; =============================================================================
;; Create/Delete Headlines
;; =============================================================================

(defn add-headline
  "Add a new headline to the document at the top level."
  [doc headline]
  (update doc :headlines conj
          (merge {:type :headline
                  :level 1
                  :keyword nil
                  :priority nil
                  :tags []
                  :properties {}
                  :planning {}
                  :content []
                  :children []}
                 headline)))

(defn add-child-headline
  "Add a child headline under a parent identified by ID."
  [doc parent-id child-headline]
  (let [parent (query/find-by-id doc parent-id)
        child-level (inc (:level parent 1))]
    (update-by-id doc parent-id
                  update :children conj
                  (merge {:type :headline
                          :level child-level
                          :keyword nil
                          :priority nil
                          :tags []
                          :properties {}
                          :planning {}
                          :content []
                          :children []}
                         (assoc child-headline :level child-level)))))

(defn remove-headline
  "Remove a headline by ID from the document."
  [doc id]
  (letfn [(remove-from-tree [headlines]
            (reduce (fn [acc hl]
                      (if (= (get-in hl [:properties :ID]) id)
                        acc
                        (conj acc (update hl :children remove-from-tree))))
                    []
                    headlines))]
    (update doc :headlines remove-from-tree)))

;; =============================================================================
;; Tests
;; =============================================================================

(deftest test-set-status
  (testing "Changes TODO keyword"
    (let [doc {:headlines [{:properties {:ID "a"} :keyword "TODO" :children []}]}
          updated (set-status doc "a" "DONE")]
      (is (= "DONE" (get-in updated [:headlines 0 :keyword]))))))

(deftest test-set-property
  (testing "Sets property value"
    (let [doc {:headlines [{:properties {:ID "a"} :children []}]}
          updated (set-property doc "a" :EFFORT "M")]
      (is (= "M" (get-in updated [:headlines 0 :properties :EFFORT]))))))

(deftest test-add-tag
  (testing "Adds tag to headline"
    (let [doc {:headlines [{:properties {:ID "a"} :tags ["work"] :children []}]}
          updated (add-tag doc "a" "urgent")]
      (is (= ["work" "urgent"] (get-in updated [:headlines 0 :tags]))))))

(comment
  ;; Example usage
  (require '[hive-mcp.org-clj.parser :as parser])
  (def doc (parser/parse-document (slurp "kanban.org")))

  ;; Mark a task done
  (def updated (mark-done doc "some-task-id"))

  ;; Add a property
  (def updated (set-property doc "task-id" :EFFORT "L"))

  ;; Add a new headline
  (def updated (add-headline doc {:title "New Task" :keyword "TODO"})))
