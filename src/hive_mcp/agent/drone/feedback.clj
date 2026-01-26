(ns hive-mcp.agent.drone.feedback
  "Drone feedback loop - learn from execution results.

   CLARITY-T (Telemetry First): Records execution patterns for analysis.
   CLARITY-Y (Yield Safe Failure): Routes away from failing model/task combos.

   This module:
   1. Classifies drone execution results
   2. Records patterns to project memory (Chroma)
   3. Queries patterns before dispatch for smart routing
   4. Aggregates weekly statistics for pattern analysis

   Extracted for 200 LOC compliance and single responsibility."
  (:require [hive-mcp.tools.memory.crud :as mem-crud]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Result Classification
;;; ============================================================

(def result-classes
  "Canonical result classifications for drone telemetry.
   Used for pattern tracking and smart routing decisions."
  #{:success
    :rate-limited      ; API rate limit hit
    :timeout           ; Execution timeout
    :empty-response    ; No output from model
    :model-error       ; Model returned error
    :file-conflict     ; File lock conflict
    :validation-error  ; Input validation failed
    :connection-error  ; Network/API connection failed
    :unknown-failure}) ; Fallback category

(defn classify-result
  "Classify a drone execution result into a pattern category.

   Arguments:
     drone-result - Map with :status, :result, :error, :output keys

   Returns one of the result-classes keywords."
  [drone-result]
  (let [status (:status drone-result)
        error (or (:error drone-result) "")
        error-lower (str/lower-case (str error))
        output (:output drone-result)
        result (:result drone-result)]
    (cond
      ;; Success cases
      (= status :completed) :success
      (= status :success) :success

      ;; Rate limiting
      (or (re-find #"rate.?limit" error-lower)
          (re-find #"429" error-lower)
          (re-find #"too.?many.?requests" error-lower))
      :rate-limited

      ;; Timeouts
      (or (re-find #"timeout" error-lower)
          (re-find #"timed.?out" error-lower)
          (re-find #"deadline.?exceeded" error-lower))
      :timeout

      ;; Empty response
      (or (and (nil? output) (nil? result))
          (and (string? output) (str/blank? output))
          (and (string? result) (str/blank? result)))
      :empty-response

      ;; Model-specific errors
      (or (re-find #"model.?error" error-lower)
          (re-find #"invalid.?response" error-lower)
          (re-find #"content.?filter" error-lower)
          (re-find #"safety" error-lower))
      :model-error

      ;; File conflicts
      (or (re-find #"conflict" error-lower)
          (re-find #"locked" error-lower)
          (re-find #"claimed" error-lower))
      :file-conflict

      ;; Validation errors
      (or (re-find #"validation" error-lower)
          (re-find #"invalid.?input" error-lower)
          (re-find #"schema" error-lower))
      :validation-error

      ;; Connection errors
      (or (re-find #"connection" error-lower)
          (re-find #"network" error-lower)
          (re-find #"unreachable" error-lower)
          (re-find #"econnrefused" error-lower))
      :connection-error

      ;; Fallback
      :else :unknown-failure)))

;;; ============================================================
;;; Pattern Storage
;;; ============================================================

(defn- make-pattern-content
  "Create content string for pattern memory entry."
  [task-type model result-class duration-ms]
  (str "Drone pattern: " (name task-type)
       " with " model
       " -> " (name result-class)
       (when duration-ms (str " (" duration-ms "ms)"))))

(defn- make-pattern-tags
  "Create tags for pattern memory entry."
  [task-type model result-class]
  ["drone-pattern"
   (str "task:" (name task-type))
   (str "model:" model)
   (str "result:" (name result-class))])

(defn record-pattern!
  "Record a drone execution pattern to memory.

   Arguments:
     task-type    - Keyword describing task category (e.g., :refactor, :implement, :fix)
     model        - Model identifier used (e.g., 'mistralai/devstral-2512:free')
     result-class - Result classification from classify-result
     opts         - Optional map with :duration-ms, :directory, :agent-id

   Stores pattern in Chroma with 'short' duration (7 days) for pattern analysis."
  [task-type model result-class & [{:keys [duration-ms directory agent-id]}]]
  (try
    (let [content (make-pattern-content task-type model result-class duration-ms)
          tags (make-pattern-tags task-type model result-class)
          params {:type "note"
                  :content content
                  :tags tags
                  :duration "short"
                  :directory directory
                  :agent_id agent-id}]
      (mem-crud/handle-add params)
      (log/debug "Recorded drone pattern" {:task-type task-type
                                           :model model
                                           :result result-class}))
    (catch Exception e
      (log/warn e "Failed to record drone pattern"))))

;;; ============================================================
;;; Pattern Querying
;;; ============================================================

(defn query-patterns
  "Query historical patterns for a task type and/or model.

   Arguments:
     opts - Map with optional filters:
            :task-type  - Filter by task type keyword
            :model      - Filter by model name
            :limit      - Max results (default 20)
            :directory  - Project directory for scoping

   Returns vector of pattern records with :result-class counts."
  [{:keys [task-type model limit directory]
    :or {limit 20}}]
  (try
    (let [tags (cond-> ["drone-pattern"]
                 task-type (conj (str "task:" (name task-type)))
                 model (conj (str "model:" model)))
          result (mem-crud/handle-query {:type "note"
                                         :tags tags
                                         :limit limit
                                         :directory directory})
          parsed (when (:text result)
                   (json/read-str (:text result) :key-fn keyword))]
      (or (:entries parsed) []))
    (catch Exception e
      (log/warn e "Failed to query drone patterns")
      [])))

(defn get-success-rate
  "Calculate success rate for a task-type/model combination.

   Returns map with :success-count, :total-count, :success-rate (0.0-1.0).
   Returns nil if no patterns found."
  [task-type model & [{:keys [directory]}]]
  (let [patterns (query-patterns {:task-type task-type
                                  :model model
                                  :directory directory
                                  :limit 100})]
    (when (seq patterns)
      (let [total (count patterns)
            successes (count (filter #(str/includes? (or (:content %) "") "-> :success")
                                     patterns))]
        {:success-count successes
         :total-count total
         :success-rate (if (pos? total)
                         (double (/ successes total))
                         0.0)}))))

;;; ============================================================
;;; Pattern-Based Routing
;;; ============================================================

(def ^:private min-samples-for-routing
  "Minimum pattern samples before making routing decisions."
  3)

(def ^:private failure-threshold
  "Success rate below which a model/task combo is avoided."
  0.3)

(defn should-avoid-combo?
  "Check if a task-type/model combination should be avoided.

   Returns true if:
   - Has enough samples (>= min-samples-for-routing)
   - Success rate is below failure-threshold

   Arguments:
     task-type - Task type keyword
     model     - Model identifier
     opts      - Optional map with :directory"
  [task-type model & [opts]]
  (when-let [stats (get-success-rate task-type model opts)]
    (and (>= (:total-count stats) min-samples-for-routing)
         (< (:success-rate stats) failure-threshold))))

(defn recommend-model
  "Recommend a model for a task type based on historical patterns.

   Arguments:
     task-type      - Task type keyword
     available-models - Sequence of model identifiers to choose from
     opts           - Optional map with :directory

   Returns the model with highest success rate, or first available if no patterns."
  [task-type available-models & [{:keys [directory] :as opts}]]
  (if (empty? available-models)
    nil
    (let [model-stats (for [model available-models]
                        (let [stats (get-success-rate task-type model opts)]
                          {:model model
                           :success-rate (or (:success-rate stats) 0.5) ; Default 50% for unknowns
                           :samples (or (:total-count stats) 0)}))
          ;; Prefer models with actual samples, then by success rate
          sorted (sort-by (juxt #(if (pos? (:samples %)) 0 1)
                                #(- (:success-rate %)))
                          model-stats)]
      (:model (first sorted)))))

;;; ============================================================
;;; Weekly Pattern Analysis
;;; ============================================================

(defn aggregate-weekly-stats
  "Aggregate pattern statistics for analysis.

   Arguments:
     opts - Map with :directory for project scoping

   Returns map with:
     :by-model      - Success rates grouped by model
     :by-task-type  - Success rates grouped by task type
     :by-result     - Counts grouped by result class
     :problematic   - Model/task combos with low success rates
     :top-performers - Best performing combos"
  [& [{:keys [directory]}]]
  (let [patterns (query-patterns {:limit 500 :directory directory})]
    (if (empty? patterns)
      {:message "No patterns recorded yet"
       :by-model {}
       :by-task-type {}
       :by-result {}
       :problematic []
       :top-performers []}

      (let [;; Parse pattern content to extract fields
            parsed (for [p patterns]
                     (let [content (or (:content p) "")
                           task-match (re-find #"task:(\S+)" (str (:tags p)))
                           model-match (re-find #"model:(\S+)" (str (:tags p)))
                           result-match (re-find #"result:(\S+)" (str (:tags p)))]
                       {:task-type (or (second task-match) "unknown")
                        :model (or (second model-match) "unknown")
                        :result (or (second result-match) "unknown")
                        :success? (str/includes? content "-> :success")}))

            ;; Group by model
            by-model (group-by :model parsed)
            model-stats (into {}
                              (for [[model entries] by-model]
                                [model {:total (count entries)
                                        :successes (count (filter :success? entries))
                                        :success-rate (double (/ (count (filter :success? entries))
                                                                 (count entries)))}]))

            ;; Group by task type
            by-task (group-by :task-type parsed)
            task-stats (into {}
                             (for [[task entries] by-task]
                               [task {:total (count entries)
                                      :successes (count (filter :success? entries))
                                      :success-rate (double (/ (count (filter :success? entries))
                                                               (count entries)))}]))

            ;; Group by result
            by-result (frequencies (map :result parsed))

            ;; Find problematic combos (success rate < 30%, >= 3 samples)
            combo-groups (group-by (juxt :task-type :model) parsed)
            problematic (->> combo-groups
                             (map (fn [[[task model] entries]]
                                    {:task-type task
                                     :model model
                                     :total (count entries)
                                     :success-rate (double (/ (count (filter :success? entries))
                                                              (count entries)))}))
                             (filter #(and (>= (:total %) 3)
                                           (< (:success-rate %) 0.3)))
                             (sort-by :success-rate))

            ;; Find top performers (success rate >= 80%, >= 3 samples)
            top-performers (->> combo-groups
                                (map (fn [[[task model] entries]]
                                       {:task-type task
                                        :model model
                                        :total (count entries)
                                        :success-rate (double (/ (count (filter :success? entries))
                                                                 (count entries)))}))
                                (filter #(and (>= (:total %) 3)
                                              (>= (:success-rate %) 0.8)))
                                (sort-by :success-rate >))]

        {:total-patterns (count patterns)
         :by-model model-stats
         :by-task-type task-stats
         :by-result by-result
         :problematic (vec problematic)
         :top-performers (vec top-performers)}))))

(defn generate-recommendations
  "Generate prompt improvement recommendations based on patterns.

   Analyzes problematic combos and suggests improvements."
  [& [opts]]
  (let [stats (aggregate-weekly-stats opts)
        problematic (:problematic stats)]
    (if (empty? problematic)
      [{:recommendation "No problematic patterns detected"
        :action "Continue monitoring drone executions"}]
      (for [{:keys [task-type model success-rate total]} problematic]
        {:task-type task-type
         :model model
         :success-rate success-rate
         :samples total
         :recommendation (cond
                           (< success-rate 0.1)
                           (str "Consider removing " model " from " task-type " tasks - very high failure rate")

                           (< success-rate 0.2)
                           (str "Review prompts for " task-type " tasks with " model " - likely prompt/model mismatch")

                           :else
                           (str "Monitor " task-type "/" model " combo - below acceptable threshold"))
         :action (cond
                   (str/includes? (str task-type) "refactor")
                   "Try simpler refactoring prompts or break into smaller steps"

                   (str/includes? (str task-type) "implement")
                   "Provide more context and examples in implementation prompts"

                   :else
                   "Review task prompts for clarity and specificity")}))))
