(ns hive-mcp.agent.drone.decompose
  "Task decomposition with complexity estimation and step budgets."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const complexity-thresholds
  "Thresholds for decomposition recommendations."
  {:single-drone 5
   :split-2 15})

(def ^:const step-budgets
  "Max steps based on task complexity tier."
  {:trivial 8
   :simple 15
   :medium 25
   :complex 40})

(defn estimate-complexity
  "Estimate task complexity and return score, recommendation, and step budget."
  [task file-path]
  (let [file-size (try (count (slurp file-path)) (catch Exception _ 0))
        task-length (count (or task ""))
        ;; Count modification keywords
        keywords (count (re-seq #"(?i)(add|modify|refactor|implement|update|change|fix|create|delete|remove)" (or task "")))
        ;; Calculate score
        score (+ (/ file-size 1000.0)
                 (/ task-length 100.0)
                 (* keywords 2))
        recommendation (cond
                         (< score (:single-drone complexity-thresholds)) :single-drone
                         (< score (:split-2 complexity-thresholds)) :split-2
                         :else :split-many)
        ;; FRICTION FIX: Map complexity to step budget
        complexity-tier (cond
                          (< score 2) :trivial
                          (< score (:single-drone complexity-thresholds)) :simple
                          (< score (:split-2 complexity-thresholds)) :medium
                          :else :complex)
        max-steps (get step-budgets complexity-tier 25)]
    {:score score
     :file-size file-size
     :task-length task-length
     :operation-count keywords
     :recommendation recommendation
     :complexity-tier complexity-tier
     :max-steps max-steps}))

(defn get-step-budget
  "Return appropriate max-steps value based on estimated task complexity."
  [task files]
  (let [file (first files)
        {:keys [max-steps complexity-tier score]} (when file
                                                    (estimate-complexity task file))]
    (log/debug "Step budget calculated" {:task (subs task 0 (min 50 (count task)))
                                         :complexity-tier complexity-tier
                                         :score score
                                         :max-steps max-steps})
    (or max-steps (:simple step-budgets))))

(defn- extract-clojure-functions
  "Extract function definitions from Clojure source."
  [content]
  (let [lines (str/split-lines content)
        indexed (map-indexed vector lines)]
    (->> indexed
         (filter (fn [[_ line]] (re-find #"^\s*\(defn-?\s" line)))
         (map (fn [[idx line]]
                (let [[_ name] (re-find #"\(defn-?\s+([^\s\[]+)" line)]
                  {:name name :start-line idx})))
         (filter :name))))

(defn- extract-section-markers
  "Extract section markers (;;; comments) from Clojure source."
  [content]
  (let [lines (str/split-lines content)
        indexed (map-indexed vector lines)]
    (->> indexed
         (filter (fn [[_ line]] (re-find #"^;;;+\s*.+" line)))
         (map (fn [[idx line]]
                (let [name (str/trim (str/replace line #"^;;;+\s*" ""))]
                  {:name name :start-line idx}))))))

(defn split-task-by-functions
  "Split a task into subtasks by function definitions mentioned in it."
  [task file-path]
  (try
    (let [content (slurp file-path)
          functions (extract-clojure-functions content)
          task-lower (str/lower-case task)
          ;; Find functions mentioned in task
          mentioned (filter (fn [{:keys [name]}]
                              (str/includes? task-lower (str/lower-case name)))
                            functions)]
      (if (seq mentioned)
        (mapv (fn [{:keys [name]}]
                {:task (str "In function `" name "`: " task)
                 :file file-path
                 :scope {:type :function :name name}})
              mentioned)
        ;; No specific functions mentioned - return original
        [{:task task :file file-path :scope {:type :whole-file}}]))
    (catch Exception e
      (log/warn e "Failed to split by functions, returning original task")
      [{:task task :file file-path :scope {:type :whole-file}}])))

(defn split-task-by-sections
  "Split a task into subtasks by section markers in the file."
  [task file-path n]
  (try
    (let [content (slurp file-path)
          sections (extract-section-markers content)
          section-count (count sections)]
      (if (>= section-count n)
        ;; Distribute sections across n subtasks
        (let [per-task (max 1 (quot section-count n))
              partitioned (partition-all per-task sections)]
          (mapv (fn [section-group]
                  (let [names (map :name section-group)]
                    {:task (str task "\n\nFocus on sections: " (str/join ", " names))
                     :file file-path
                     :scope {:type :sections :names names}}))
                partitioned))
        ;; Not enough sections - return original
        [{:task task :file file-path :scope {:type :whole-file}}]))
    (catch Exception e
      (log/warn e "Failed to split by sections, returning original task")
      [{:task task :file file-path :scope {:type :whole-file}}])))

(defn split-task-by-operations
  "Split a task into subtasks by operation type (add/modify/delete)."
  [task file-path]
  (let [task-lower (str/lower-case task)
        has-add? (re-find #"\b(add|create|implement|new)\b" task-lower)
        has-modify? (re-find #"\b(modify|update|change|fix|refactor)\b" task-lower)
        has-delete? (re-find #"\b(delete|remove|drop)\b" task-lower)
        ops (cond-> []
              has-add? (conj {:op :add :desc "Add/create new code"})
              has-modify? (conj {:op :modify :desc "Modify/update existing code"})
              has-delete? (conj {:op :delete :desc "Delete/remove code"}))]
    (if (> (count ops) 1)
      (mapv (fn [{:keys [op desc]}]
              {:task (str "[" (name op) " operation only] " task "\n\nFocus only on: " desc)
               :file file-path
               :scope {:type :operation :op op}})
            ops)
      [{:task task :file file-path :scope {:type :whole-file}}])))

(defn maybe-decompose
  "Auto-decompose task if complexity warrants it, returning subtask vector."
  [task file {:keys [strategy force?] :as _opts}]
  (let [{:keys [recommendation score]} (estimate-complexity task file)]
    (log/debug "Task complexity" {:score score :recommendation recommendation :file file})

    (if (or force? (not= recommendation :single-drone))
      (let [strategy (or strategy
                         (case recommendation
                           :split-2 :sections
                           :split-many :functions
                           :sections))
            subtasks (case strategy
                       :functions (split-task-by-functions task file)
                       :sections (split-task-by-sections task file
                                                         (if (= recommendation :split-2) 2 4))
                       :operations (split-task-by-operations task file)
                       ;; default
                       [{:task task :file file :scope {:type :whole-file}}])]
        (when (> (count subtasks) 1)
          (log/info "Decomposed task into" (count subtasks) "subtasks"
                    {:strategy strategy :recommendation recommendation}))
        subtasks)
      ;; No decomposition needed
      [{:task task :file file :scope {:type :whole-file}}])))

(defn recombine-results
  "Recombine results from parallel drone subtask execution."
  [results]
  (let [all-modified (mapcat :files-modified results)
        all-failed (mapcat :files-failed results)
        all-completed? (every? #(= :completed (:status %)) results)]
    {:status (if all-completed? :completed :partial)
     :subtask-count (count results)
     :files-modified (vec (distinct all-modified))
     :files-failed (vec all-failed)
     :results results}))
