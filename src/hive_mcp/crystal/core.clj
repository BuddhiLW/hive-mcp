(ns hive-mcp.crystal.core
  "Crystal core: Progressive crystallization of ephemeral knowledge.
   
   Domain logic for:
   - Promotion score calculation (weighted recalls)
   - Crystallization decisions (should-promote?)
   - Session lineage tracking
   
   SOLID: Single responsibility - promotion scoring only.
   DDD: Pure domain functions, no side effects."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Recall Context Weights
;; =============================================================================

(def recall-weights
  "Weights for different recall contexts.
   Higher = more meaningful signal for promotion."
  {:catchup-structural 0.1 ; Always loaded at catchup - noise
   :wrap-structural 0.1 ; Always checked at wrap - noise
   :explicit-reference 1.0 ; LLM explicitly cited in reasoning
   :cross-session 2.0 ; Referenced from different session
   :cross-project 3.0 ; Referenced from different project
   :user-feedback 5.0}) ; Human marked as helpful

(def promotion-thresholds
  "Score thresholds for promotion between durations."
  {:ephemeral->short 5.0
   :short->medium 10.0
   :medium->long 15.0
   :long->permanent 25.0})

;; =============================================================================
;; Score Calculation
;; =============================================================================

(defn calculate-promotion-score
  "Calculate promotion score from recall history.
   
   recalls: seq of {:context :keyword, :count int, :timestamp string}
   
   Returns: {:score float
             :breakdown [{:context :weight :count :contribution}]}"
  [recalls]
  (let [breakdown (for [{:keys [context count] :or {count 1}} recalls
                        :let [weight (get recall-weights context 1.0)
                              contribution (* weight count)]]
                    {:context context
                     :weight weight
                     :count count
                     :contribution contribution})
        total-score (reduce + 0.0 (map :contribution breakdown))]
    {:score total-score
     :breakdown (vec breakdown)}))

(defn current-duration->next
  "Map current duration to next tier."
  [duration]
  (case (keyword duration)
    :ephemeral :short
    :short :medium
    :medium :long
    :long :permanent
    :permanent :permanent
    ;; Handle string versions
    (case (str duration)
      "ephemeral" :short
      "short-term" :medium
      "short" :medium
      "medium" :long
      "long-term" :permanent
      "long" :permanent
      "permanent" :permanent
      :medium))) ; default

(defn threshold-for-duration
  "Get promotion threshold for current duration."
  [duration]
  (case (keyword duration)
    :ephemeral (:ephemeral->short promotion-thresholds)
    :short (:short->medium promotion-thresholds)
    :short-term (:short->medium promotion-thresholds)
    :medium (:medium->long promotion-thresholds)
    :long (:long->permanent promotion-thresholds)
    :long-term (:long->permanent promotion-thresholds)
    :permanent Double/MAX_VALUE
    10.0)) ; default

(defn should-promote?
  "Determine if a memory entry should be promoted.
   
   entry: {:id :duration :recalls [...]}
   
   Returns: {:promote? bool :current-score float :threshold float :next-duration keyword}"
  [{:keys [duration recalls] :as _entry}]
  (let [{:keys [score]} (calculate-promotion-score recalls)
        threshold (threshold-for-duration duration)
        should? (>= score threshold)]
    {:promote? should?
     :current-score score
     :threshold threshold
     :next-duration (when should? (current-duration->next duration))}))

;; =============================================================================
;; Session Tagging
;; =============================================================================

(defn session-id
  "Generate a session identifier for today."
  []
  (let [now (java.time.LocalDateTime/now)
        fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd")]
    (.format now fmt)))

(defn session-tag
  "Create a session scope tag."
  ([]
   (str "session:" (session-id)))
  ([date-str]
   (str "session:" date-str)))

(defn extract-session-from-tags
  "Extract session identifier from tags."
  [tags]
  (some #(when (str/starts-with? % "session:")
           (subs % 8))
        tags))

;; =============================================================================
;; Crystallization Rules (Pure Predicates)
;; =============================================================================

(defn mechanical-recall?
  "Is this recall context mechanical/structural (low signal)?"
  [context]
  (contains? #{:catchup-structural :wrap-structural} context))

(defn meaningful-recalls
  "Filter to only meaningful recalls."
  [recalls]
  (remove #(mechanical-recall? (:context %)) recalls))

(defn cross-boundary-recalls
  "Get recalls that cross session/project boundaries."
  [recalls]
  (filter #(contains? #{:cross-session :cross-project} (:context %)) recalls))

(defn has-user-endorsement?
  "Check if any recall has user feedback."
  [recalls]
  (some #(= :user-feedback (:context %)) recalls))

;; =============================================================================
;; Progress Note Generation
;; =============================================================================

(defn task-to-progress-note
  "Convert a completed kanban task to a progress note structure.
   
   task: {:title :context :priority :started :completed-at}
   
   Returns: {:type :note
             :content string
             :tags [...]
             :duration :ephemeral}"
  [{:keys [title context priority started] :as task}]
  (let [completed-at (or (:completed-at task)
                         (.toString (java.time.Instant/now)))
        duration-str (when started
                       (str " (started: " started ")"))
        content (str "## Completed: " title "\n\n"
                     (when context (str context "\n\n"))
                     "Priority: " (or priority "medium")
                     duration-str
                     "\nCompleted: " completed-at)]
    {:type :note
     :content content
     :tags [(session-tag) "session-progress" "completed-task"
            (str "priority-" (or priority "medium"))]
     :duration :ephemeral}))

(defn summarize-session-progress
  "Summarize multiple progress notes into a session summary.
   
   notes: seq of progress note maps
   git-commits: seq of commit strings
   
   Returns: session summary map suitable for crystallization"
  [notes git-commits]
  (let [task-count (count (filter #(some #{"completed-task"} (:tags %)) notes))
        session (session-id)]
    {:type :note
     :content (str "## Session Summary: " session "\n\n"
                   "### Completed Tasks: " task-count "\n"
                   (str/join "\n" (map #(str "- " (first (str/split-lines (:content %))))
                                       notes))
                   "\n\n### Commits: " (count git-commits) "\n"
                   (str/join "\n" (map #(str "- " %) git-commits)))
     :tags [(session-tag) "session-summary" "wrap-generated"]
     :duration :short}))

(comment
  ;; Example usage
  (calculate-promotion-score
   [{:context :explicit-reference :count 2}
    {:context :cross-session :count 1}
    {:context :catchup-structural :count 5}])
  ;; => {:score 7.5, :breakdown [...]}

  (should-promote? {:duration :ephemeral
                    :recalls [{:context :explicit-reference :count 3}
                              {:context :cross-session :count 1}]})
  ;; => {:promote? true, :current-score 5.0, :threshold 5.0, :next-duration :short}
  )
