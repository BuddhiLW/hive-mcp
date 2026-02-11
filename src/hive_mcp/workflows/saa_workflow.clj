(ns hive-mcp.workflows.saa-workflow
  "hive-events FSM handlers for the SAA (Silence-Abstract-Act) workflow.

     S (Silence)  -- Ground in territory: read files, query memory, traverse KG
     A (Abstract) -- Create structured EDN plan with steps, dependencies, waves
     A (Act)      -- Execute plan via DAG-Wave, validate with TDD


   State graph:
   ```
   ::fsm/start --> ::catchup --> ::silence <--> ::silence-review
                                                    |
                                                    v
                                               ::abstract <--> ::validate-plan
                                                                    |
                                                                    v
                                                               ::store-plan
                                                                /          \\
                                                    ::fsm/end          ::act-dispatch
                                                  (plan-only?)              |
                                                                       ::act-verify
                                                                            |
                                                                       ::fsm/end
   ```

   This is the Clojure handler implementation matching resources/fsm/saa-workflow.edn.
   The EDN spec uses keyword handlers (:start, :silence, :abstract, etc.) that are
   resolved to these functions at compile time via the handler-map.

   Design constraints (same as forge-belt, wrap-session):
   - Normal handlers are (resources, data) -> data'
   - Terminal handlers (::end, ::error, ::halt) are (resources, fsm) -> result
   - Side effects flow through the resources map (territory)
   - The FSM is the map -- deterministic state transitions
   - Dispatch predicates are pure functions of state data

   Resources map (injected at run time):
     :scope-fn            -- (directory) -> project-id
     :catchup-fn          -- (agent-id, directory) -> context-map
     :explore-fn          -- (task, agent-id, observations) -> {:observations [...] :files-read N :discoveries N}
     :score-grounding-fn  -- (observations, files-read) -> float (0.0-1.0)
     :synthesize-fn       -- (task, observations, context) -> EDN plan map
     :validate-plan-fn    -- (plan) -> {:valid? bool :errors [...]}
     :store-plan-fn       -- (plan, agent-id, directory) -> {:memory-id str :kanban-ids [...] :kg-edges N}
     :dispatch-fn         -- (plan, execution-mode, agent-id) -> {:wave-id str :result map}
     :verify-fn           -- (execution-result, plan) -> {:passed? bool :details map}
     :shout-fn            -- (agent-id, phase, message) -> nil
     :build-summary-fn    -- (data) -> response map
     :error-response-fn   -- (error) -> response map
     :clock-fn            -- () -> instant

   State data shape: See resources/fsm/saa-workflow.edn for full spec.

  (:require [hive.events.fsm :as fsm]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Dispatch Predicates (pure functions of state data)
;; =============================================================================

(defn has-required-fields?
  "Check that agent-id and task are present, and no startup error."
  [data]
  (and (:agent-id data)
       (:task data)
       (not (:error data))))

(defn has-startup-error?
  "Check for missing required fields or startup error."
  [data]
  (or (not (:agent-id data))
      (not (:task data))
      (some? (:error data))))

(defn context-loaded?
  "Check if catchup loaded project context."
  [data]
  (true? (:context-loaded? data)))

(defn has-observations?
  "Check if Silence phase produced observations."
  [data]
  (some? (:observations data)))

(defn has-error?
  "Check if data contains an error."
  [data]
  (some? (:error data)))

(defn grounding-sufficient?
  "Check if observation grounding score meets threshold."
  [data]
  (>= (get data :grounding-score 0.0)
      (get data :grounding-threshold 0.6)))

(defn grounding-insufficient-retryable?
  "Check if grounding is insufficient but retries remain."
  [data]
  (and (< (get data :grounding-score 0.0)
          (get data :grounding-threshold 0.6))
       (< (get data :silence-iterations 0) 3)))

(defn grounding-max-iterations?
  "Check if max silence iterations reached (proceed anyway)."
  [data]
  (>= (get data :silence-iterations 0) 3))

(defn has-plan?
  "Check if Abstract phase produced a plan."
  [data]
  (some? (:plan data)))

(defn plan-valid?
  "Check if plan passed validation."
  [data]
  (true? (:plan-valid? data)))

(defn plan-invalid-retryable?
  "Check if plan is invalid but retries remain."
  [data]
  (and (not (:plan-valid? data))
       (< (get data :abstract-retries 0) 2)))

(defn plan-invalid-final?
  "Check if plan is invalid and no retries remain."
  [data]
  (and (not (:plan-valid? data))
       (>= (get data :abstract-retries 0) 2)))

(defn plan-only?
  "Check if we should skip Act phase (plan-only mode)."
  [data]
  (true? (:plan-only? data)))

(defn full-execution?
  "Check if we should proceed to Act phase."
  [data]
  (not (:plan-only? data)))

(defn has-execution-result?
  "Check if Act dispatch produced a result."
  [data]
  (some? (:execution-result data)))

(defn tests-passed?
  "Check if verification succeeded."
  [data]
  (true? (:tests-passed? data)))

(defn tests-failed?
  "Check if verification failed."
  [data]
  (not (:tests-passed? data)))

(defn always [_data] true)

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- maybe-shout!
  "Call shout-fn if available. Non-critical side effect."
  [resources agent-id phase message]
  (when-let [shout-fn (:shout-fn resources)]
    (try
      (shout-fn agent-id phase message)
      (catch Exception _ nil))))

(defn- now-str
  "Get current time as ISO string using clock-fn from resources."
  [resources]
  (str ((or (:clock-fn resources) #(java.time.Instant/now)))))

;; =============================================================================
;; Normal State Handlers: (resources, data) -> data'
;;
;; EDN handler-map keys: :start, :catchup, :silence, :silence-review,
;;                       :abstract, :validate-plan, :store-plan,
;;                       :act-dispatch, :act-verify
;; =============================================================================

(defn handle-start
  "Initialize SAA session, resolve agent/project context.
   EDN handler key: :start

   Validates required fields (agent-id, task, directory).
   Resolves project-id via scope-fn. Sets default thresholds."
  [resources data]
  (let [scope-fn (or (:scope-fn resources) (constantly nil))
        directory (or (:directory data) (:directory resources))
        agent-id (or (:agent-id data) (:agent-id resources))
        task (:task data)]
    (log/info "[saa-fsm] Starting SAA workflow" {:agent-id agent-id :task task})
    (maybe-shout! resources agent-id :start (str "SAA starting: " task))
    (if (and agent-id task)
      (let [project-id (when (and scope-fn directory)
                         (try (scope-fn directory)
                              (catch Exception e
                                (log/warn "[saa-fsm] scope-fn failed" {:error (ex-message e)})
                                nil)))]
        (assoc data
               :agent-id agent-id
               :directory directory
               :project-id (or project-id "unknown")
               :started-at (now-str resources)
               :phase :start
               :grounding-threshold (get data :grounding-threshold 0.6)
               :silence-iterations 0
               :abstract-retries 0
               :plan-only? (get data :plan-only? false)
               :error nil))
      (assoc data :error "Missing required fields: :agent-id and :task"))))

(defn handle-catchup
  "Load project context from memory (axioms, conventions, decisions, KG).
   EDN handler key: :catchup

   Uses resources:
     :catchup-fn (fn [agent-id directory] -> {:axioms [...] :conventions [...] :decisions [...]})"
  [resources data]
  (let [{:keys [agent-id directory]} data
        catchup-fn (:catchup-fn resources)]
    (log/info "[saa-fsm] Catchup phase" {:agent-id agent-id})
    (maybe-shout! resources agent-id :catchup "Loading project context")
    (if catchup-fn
      (try
        (let [context (catchup-fn agent-id directory)]
          (assoc data
                 :phase :catchup
                 :context-loaded? true
                 :axioms (get context :axioms [])
                 :conventions (get context :conventions [])
                 :decisions (get context :decisions [])))
        (catch Exception e
          (log/error "[saa-fsm] Catchup failed" {:error (ex-message e)})
          (assoc data
                 :phase :catchup
                 :context-loaded? false
                 :error (str "Catchup failed: " (ex-message e)))))
      ;; No catchup-fn -- proceed without context (degraded mode)
      (assoc data
             :phase :catchup
             :context-loaded? true
             :axioms []
             :conventions []
             :decisions []))))

(defn handle-silence
  "Execute Silence phase: explore codebase with read-only tools.
   EDN handler key: :silence

   Ground in the territory. Read files, search code, query memory,
   traverse KG. Record structured observations.

   Uses resources:
     :explore-fn (fn [task agent-id observations] -> {:observations [...] :files-read N :discoveries N})"
  [resources data]
  (let [{:keys [agent-id task observations silence-iterations]} data
        explore-fn (:explore-fn resources)
        iteration (inc (or silence-iterations 0))]
    (log/info "[saa-fsm] Silence phase iteration" {:iteration iteration :agent-id agent-id})
    (maybe-shout! resources agent-id :silence
                  (str "Silence phase (iteration " iteration "): exploring"))
    (if explore-fn
      (try
        (let [result (explore-fn task agent-id (or observations []))]
          (assoc data
                 :phase :silence
                 :observations (get result :observations observations)
                 :files-read (get result :files-read 0)
                 :discoveries (get result :discoveries 0)
                 :silence-started (or (:silence-started data) (now-str resources))
                 :silence-iterations iteration))
        (catch Exception e
          (log/error "[saa-fsm] Silence exploration failed" {:error (ex-message e)})
          (assoc data
                 :phase :silence
                 :silence-iterations iteration
                 :error (str "Silence failed: " (ex-message e)))))
      ;; No explore-fn -- create minimal observations from task
      (assoc data
             :phase :silence
             :observations (or observations [{:type :task-description :content task}])
             :files-read 0
             :discoveries 0
             :silence-started (or (:silence-started data) (now-str resources))
             :silence-iterations iteration))))

(defn handle-silence-review
  "Evaluate observations and decide if grounding is sufficient.
   EDN handler key: :silence-review

   Computes a grounding score (0.0-1.0) from observations.
   If score >= threshold, proceed to Abstract. Otherwise loop back.

   Uses resources:
     :score-grounding-fn (fn [observations files-read] -> float)"
  [resources data]
  (let [{:keys [agent-id observations files-read]} data
        score-fn (or (:score-grounding-fn resources)
                     ;; Default heuristic: files-read > 0 and observations > 2
                     (fn [obs files]
                       (min 1.0
                            (+ (if (seq obs) 0.3 0.0)
                               (min 0.4 (* 0.1 (count obs)))
                               (if (pos? (or files 0)) 0.3 0.0)))))
        score (double (score-fn observations files-read))]
    (log/info "[saa-fsm] Silence review" {:score score :obs-count (count observations)
                                          :files-read files-read})
    (maybe-shout! resources agent-id :silence-review
                  (str "Grounding score: " (format "%.2f" score)
                       " (threshold: " (:grounding-threshold data) ")"
                       " iteration: " (:silence-iterations data)))
    (assoc data
           :phase :silence-review
           :grounding-score score
           :silence-ended (now-str resources))))

(defn handle-abstract
  "Synthesize observations into a structured EDN plan.
   EDN handler key: :abstract

   Takes scored observations and produces a plan with:
   {:id :title :steps [{:id :title :description :files :depends-on :wave}] :waves}

   Uses resources:
     :synthesize-fn (fn [task observations context] -> EDN plan map)"
  [resources data]
  (let [{:keys [agent-id task observations abstract-retries]} data
        synthesize-fn (:synthesize-fn resources)
        context (select-keys data [:axioms :conventions :decisions :project-id])
        retries (inc (or abstract-retries 0))]
    (log/info "[saa-fsm] Abstract phase" {:retry retries :obs-count (count observations)})
    (maybe-shout! resources agent-id :abstract
                  (str "Abstract phase: synthesizing plan"
                       (when (> retries 1) (str " (retry " (dec retries) ")"))))
    (if synthesize-fn
      (try
        (let [plan (synthesize-fn task observations context)]
          (assoc data
                 :phase :abstract
                 :plan plan
                 :abstract-started (or (:abstract-started data) (now-str resources))
                 :abstract-retries retries))
        (catch Exception e
          (log/error "[saa-fsm] Plan synthesis failed" {:error (ex-message e)})
          (assoc data
                 :phase :abstract
                 :plan nil
                 :abstract-retries retries
                 :error (str "Synthesis failed: " (ex-message e)))))
      ;; No synthesize-fn -- cannot create plan
      (assoc data
             :phase :abstract
             :plan nil
             :abstract-retries retries
             :error "No synthesize-fn provided in resources"))))

(defn handle-validate-plan
  "Validate plan integrity: dependencies, files, waves, no cycles.
   EDN handler key: :validate-plan

   Uses resources:
     :validate-plan-fn (fn [plan] -> {:valid? bool :errors [...]})"
  [resources data]
  (let [{:keys [agent-id plan]} data
        validate-fn (or (:validate-plan-fn resources)
                        ;; Default: valid if plan has :steps
                        (fn [p]
                          (if (seq (:steps p))
                            {:valid? true :errors []}
                            {:valid? false :errors ["Plan has no steps"]})))]
    (log/info "[saa-fsm] Validating plan")
    (try
      (let [{:keys [valid? errors]} (validate-fn plan)]
        (maybe-shout! resources agent-id :validate-plan
                      (if valid?
                        "Plan validation passed"
                        (str "Plan validation failed: " (pr-str errors))))
        (assoc data
               :phase :validate-plan
               :plan-valid? (boolean valid?)
               :validation-errors (or errors [])))
      (catch Exception e
        (log/error "[saa-fsm] Plan validation failed" {:error (ex-message e)})
        (assoc data
               :phase :validate-plan
               :plan-valid? false
               :validation-errors [(str "Validation exception: " (ex-message e))])))))

(defn handle-store-plan
  "Store plan in memory and optionally convert to kanban tasks.
   EDN handler key: :store-plan

   Uses resources:
     :store-plan-fn (fn [plan agent-id directory] -> {:memory-id str :kanban-ids [...] :kg-edges N})"
  [resources data]
  (let [{:keys [plan agent-id directory]} data
        store-fn (:store-plan-fn resources)]
    (log/info "[saa-fsm] Storing plan" {:agent-id agent-id})
    (maybe-shout! resources agent-id :store-plan "Storing plan in memory")
    (if store-fn
      (try
        (let [{:keys [memory-id kanban-ids kg-edges]} (store-fn plan agent-id directory)]
          (assoc data
                 :phase :store-plan
                 :plan-memory-id memory-id
                 :kanban-task-ids (or kanban-ids [])
                 :kg-edges-created (or kg-edges 0)))
        (catch Exception e
          (log/error "[saa-fsm] Plan storage failed" {:error (ex-message e)})
          ;; Non-fatal -- plan is in data, just not persisted
          (assoc data
                 :phase :store-plan
                 :plan-memory-id nil
                 :kanban-task-ids []
                 :kg-edges-created 0)))
      ;; No store-fn -- skip storage
      (assoc data
             :phase :store-plan
             :plan-memory-id nil
             :kanban-task-ids []
             :kg-edges-created 0))))

(defn handle-act-dispatch
  "Dispatch plan execution via DAG-Wave, ling spawn, or direct execution.
   EDN handler key: :act-dispatch

   Uses resources:
     :dispatch-fn (fn [plan execution-mode agent-id] -> {:wave-id str :result map})"
  [resources data]
  (let [{:keys [plan agent-id execution-mode]} data
        dispatch-fn (:dispatch-fn resources)
        mode (or execution-mode :direct)]
    (log/info "[saa-fsm] Act dispatch" {:mode mode :agent-id agent-id})
    (maybe-shout! resources agent-id :act-dispatch
                  (str "Act phase: dispatching execution (mode: " (name mode) ")"))
    (if dispatch-fn
      (try
        (let [{:keys [wave-id result]} (dispatch-fn plan mode agent-id)]
          (assoc data
                 :phase :act-dispatch
                 :execution-mode mode
                 :wave-id wave-id
                 :execution-result result
                 :act-started (or (:act-started data) (now-str resources))))
        (catch Exception e
          (log/error "[saa-fsm] Dispatch failed" {:error (ex-message e)})
          (assoc data
                 :phase :act-dispatch
                 :execution-mode mode
                 :error (str "Dispatch failed: " (ex-message e)))))
      ;; No dispatch-fn -- mark as dispatched with noop result
      (assoc data
             :phase :act-dispatch
             :execution-mode mode
             :execution-result {:status :no-dispatch-fn}
             :act-started (or (:act-started data) (now-str resources))))))

(defn handle-act-verify
  "Verify execution results with TDD, lint, integration checks.
   EDN handler key: :act-verify

   Uses resources:
     :verify-fn (fn [execution-result plan] -> {:passed? bool :details map})"
  [resources data]
  (let [{:keys [agent-id execution-result plan]} data
        verify-fn (:verify-fn resources)]
    (log/info "[saa-fsm] Act verify")
    (maybe-shout! resources agent-id :act-verify "Verifying execution results")
    (if verify-fn
      (try
        (let [{:keys [passed? details]} (verify-fn execution-result plan)]
          (assoc data
                 :phase :act-verify
                 :tests-passed? (boolean passed?)
                 :verification details
                 :act-ended (now-str resources)))
        (catch Exception e
          (log/warn "[saa-fsm] Verification failed" {:error (ex-message e)})
          (assoc data
                 :phase :act-verify
                 :tests-passed? false
                 :verification {:error (ex-message e)}
                 :act-ended (now-str resources))))
      ;; No verify-fn -- assume passed (degraded mode)
      (assoc data
             :phase :act-verify
             :tests-passed? true
             :verification {:status :no-verify-fn}
             :act-ended (now-str resources)))))

;; =============================================================================
;; Terminal State Handlers: (resources, fsm) -> result
;;
;; ::end, ::halt, ::error receive the full FSM state map, not just data.
;; =============================================================================

(defn handle-end
  "Terminal state handler. Returns SAA summary.
   EDN handler key: :end"
  [resources {:keys [data]}]
  (let [agent-id (:agent-id data)
        build-fn (:build-summary-fn resources)]
    (log/info "[saa-fsm] SAA workflow complete"
              {:agent-id agent-id
               :plan-only? (:plan-only? data)
               :observations (count (get data :observations []))
               :plan-valid? (:plan-valid? data)
               :tests-passed? (:tests-passed? data)})
    (maybe-shout! resources agent-id :end "SAA workflow complete")
    (if build-fn
      (build-fn data)
      (select-keys data [:agent-id :project-id :task :phase
                         :observations :grounding-score
                         :plan :plan-valid? :plan-memory-id :kanban-task-ids
                         :execution-result :tests-passed? :verification
                         :started-at :silence-started :silence-ended
                         :abstract-started :act-started :act-ended
                         :silence-iterations :abstract-retries
                         :plan-only?]))))

(defn handle-error
  "Error state handler. Captures error context.
   EDN handler key: :error"
  [resources {:keys [error data] :as _fsm}]
  (let [agent-id (:agent-id data)
        error-fn (:error-response-fn resources)]
    (log/error "[saa-fsm] SAA workflow error"
               {:agent-id agent-id :phase (:phase data) :error error})
    (maybe-shout! resources agent-id :error
                  (str "SAA FAILED at phase " (:phase data) ": " error))
    (if error-fn
      (error-fn {:phase (:phase data)
                 :agent-id agent-id
                 :task (:task data)
                 :data data
                 :error error})
      (throw (ex-info "SAA workflow error"
                      {:phase (:phase data)
                       :agent-id agent-id
                       :task (:task data)
                       :data (select-keys data [:error :plan-valid? :validation-errors
                                                :grounding-score :tests-passed?])
                       :error error})))))

;; =============================================================================
;; Handler Map (for EDN spec registration in workflow registry)
;; =============================================================================

(def handler-map
  "Maps EDN keyword handlers to implementation functions.
   Used by registry/register-handlers! for EDN spec compilation."
  {:start          handle-start
   :catchup        handle-catchup
   :silence        handle-silence
   :silence-review handle-silence-review
   :abstract       handle-abstract
   :validate-plan  handle-validate-plan
   :store-plan     handle-store-plan
   :act-dispatch   handle-act-dispatch
   :act-verify     handle-act-verify
   :end            handle-end
   :error          handle-error})

;; =============================================================================
;; In-Code FSM Spec (inline functions, no EDN needed)
;; =============================================================================

(def saa-workflow-spec
  "hive-events FSM spec for the SAA workflow.
   Uses inline functions -- no handler-map needed at compile time.

   State graph:
   ```
   ::fsm/start --> ::catchup --> ::silence <--> ::silence-review
                                                    |
                                               ::abstract <--> ::validate-plan
                                                                    |
                                                               ::store-plan
                                                                /          \\
                                                    ::fsm/end          ::act-dispatch
                                                  (plan-only?)              |
                                                                       ::act-verify
                                                                            |
                                                                       ::fsm/end
   ```"
  {:fsm
   {::fsm/start
    {:handler    handle-start
     :dispatches [[::fsm/error has-startup-error?]
                  [::catchup   has-required-fields?]]}

    ::catchup
    {:handler    handle-catchup
     :dispatches [[::silence   context-loaded?]
                  [::fsm/error (complement context-loaded?)]]}

    ;; --- Silence Phase (S) ---
    ::silence
    {:handler    handle-silence
     :dispatches [[::fsm/error     has-error?]
                  [::silence-review has-observations?]]}

    ::silence-review
    {:handler    handle-silence-review
     :dispatches [[::abstract  grounding-sufficient?]
                  [::silence   grounding-insufficient-retryable?]
                  [::abstract  grounding-max-iterations?]]}

    ;; --- Abstract Phase (A1) ---
    ::abstract
    {:handler    handle-abstract
     :dispatches [[::fsm/error     (fn [d] (and (nil? (:plan d)) (has-error? d)))]
                  [::validate-plan has-plan?]]}

    ::validate-plan
    {:handler    handle-validate-plan
     :dispatches [[::store-plan plan-valid?]
                  [::abstract   plan-invalid-retryable?]
                  [::fsm/error  plan-invalid-final?]]}

    ::store-plan
    {:handler    handle-store-plan
     :dispatches [[::fsm/end      plan-only?]
                  [::act-dispatch full-execution?]]}

    ;; --- Act Phase (A2) ---
    ::act-dispatch
    {:handler    handle-act-dispatch
     :dispatches [[::fsm/error  has-error?]
                  [::act-verify has-execution-result?]]}

    ::act-verify
    {:handler    handle-act-verify
     :dispatches [[::fsm/end   tests-passed?]
                  [::fsm/error tests-failed?]]}

    ;; --- Terminal ---
    ::fsm/end
    {:handler handle-end}

    ::fsm/error
    {:handler handle-error}}

   :opts
   {:max-trace 100

    :subscriptions
    {[:grounding-score]    {:handler (fn [_path _old _new] nil)}
     [:plan-valid?]        {:handler (fn [_path _old _new] nil)}
     [:tests-passed?]      {:handler (fn [_path _old _new] nil)}
     [:silence-iterations] {:handler (fn [_path _old _new] nil)}}

    :pre
    (fn [{:keys [current-state-id] :as fsm} _resources]
      (update-in fsm [:data :trace-log] (fnil conj [])
                 {:state current-state-id
                  :at (str (java.time.Instant/now))
                  :direction :enter}))

    :post
    (fn [{:keys [current-state-id] :as fsm} _resources]
      (update-in fsm [:data :trace-log] (fnil conj [])
                 {:state current-state-id
                  :at (str (java.time.Instant/now))
                  :direction :exit}))}})

;; =============================================================================
;; Compilation & Execution API
;; =============================================================================

(defn compile-saa
  "Compile the SAA workflow FSM spec. Call once, reuse the compiled FSM."
  []
  (fsm/compile saa-workflow-spec))

(defn run-saa
  "Execute a compiled SAA workflow FSM.

   Args:
     compiled-fsm -- Result of compile-saa
     resources    -- Map of side-effect functions and config
     opts         -- Initial data (must include :task and :agent-id)

   Returns:
     Final data map with SAA results.

   Example:
   ```clojure
   (run-saa (compile-saa)
            {:scope-fn         scope/get-current-project-id
             :catchup-fn       load-context
             :explore-fn       run-exploration
             :score-grounding-fn score-observations
             :synthesize-fn    synthesize-plan
             :validate-plan-fn validate-plan
             :store-plan-fn    store-plan-to-memory
             :dispatch-fn      dispatch-execution
             :verify-fn        verify-results
             :shout-fn         shout-progress
             :clock-fn         #(java.time.Instant/now)}
            {:task      \"Fix auth bug in login flow\"
             :agent-id  \"ling-1\"
             :directory \"/home/user/project\"})
   ```"
  ([compiled-fsm resources]
   (run-saa compiled-fsm resources {}))
  ([compiled-fsm resources opts]
   (fsm/run compiled-fsm
            resources
            {:data (merge {:agent-id nil
                           :directory nil
                           :task nil
                           :plan-only? false
                           :grounding-threshold 0.6
                           :silence-iterations 0
                           :abstract-retries 0}
                          opts)})))

(defn run-full-saa
  "Convenience: compile and run a full SAA cycle.

   Args:
     resources -- Map of side-effect functions
     opts      -- Must include :task, :agent-id, :directory

   Returns:
     Final data map."
  [resources opts]
  (run-saa (compile-saa) resources opts))

(defn run-plan-only
  "Convenience: compile and run SAA without Act phase.
   Useful for d[l]e SAA (dispatch-ling-explore) mode where
   the plan is produced but execution is deferred to DAG-Wave.

   Args:
     resources -- Map of side-effect functions
     opts      -- Must include :task, :agent-id, :directory

   Returns:
     Final data map with :plan but no execution results."
  [resources opts]
  (run-saa (compile-saa) resources (assoc opts :plan-only? true)))
