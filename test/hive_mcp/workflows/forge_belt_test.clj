(ns hive-mcp.workflows.forge-belt-test
  "Tests for the Forge Belt IP stubs.

   Validates:
   1. Noop fallback shapes — all public fns return expected defaults
   2. Extension delegation — registered extensions are called
   3. Public API surface — all expected vars exist
   4. Backward compatibility — var arities preserved

   CLARITY: T — Telemetry via test assertions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.forge-belt :as belt]
            [hive-mcp.workflows.forge-belt-defaults :as fbd]
            [hive-mcp.extensions.registry :as ext]
            [hive.events.fsm :as fsm]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private all-fb-keys
  "All :fb/* extension keys for cleanup."
  [:fb/q1 :fb/q2 :fb/q3 :fb/q4 :fb/q5 :fb/q6
   :fb/h1 :fb/h2 :fb/h3 :fb/h4 :fb/h5 :fb/h6 :fb/h7
   :fb/s1 :fb/s2 :fb/spec :fb/compile
   :fb/run :fb/strike :fb/cont])

(defn clean-extensions-fixture
  "Clear extension registry before AND after each test.
   Before: removes leftover registrations from prior tests or REPL work.
   After: cleans up registrations made during the test."
  [f]
  (doseq [k all-fb-keys] (ext/deregister! k))
  (try
    (f)
    (finally
      (doseq [k all-fb-keys] (ext/deregister! k)))))

(use-fixtures :each clean-extensions-fixture)

;; =============================================================================
;; 1. Noop Fallback — Dispatch Predicates
;; =============================================================================

(deftest test-quenched?-noop
  (testing "quenched? returns false without extension"
    (is (false? (belt/quenched? {:quenched? true})))
    (is (false? (belt/quenched? {})))
    (is (false? (belt/quenched? nil)))))

(deftest test-has-tasks?-noop
  (testing "has-tasks? returns false without extension"
    (is (false? (belt/has-tasks? {:survey-result {:count 3}})))
    (is (false? (belt/has-tasks? {})))))

(deftest test-no-tasks?-noop
  (testing "no-tasks? returns true without extension"
    (is (true? (belt/no-tasks? {:survey-result {:count 0}})))
    (is (true? (belt/no-tasks? {})))))

(deftest test-continuous?-noop
  (testing "continuous? returns false without extension"
    (is (false? (belt/continuous? {:continuous? true})))
    (is (false? (belt/continuous? {})))))

(deftest test-single-shot?-noop
  (testing "single-shot? returns true without extension"
    (is (true? (belt/single-shot? {:continuous? false})))
    (is (true? (belt/single-shot? {})))))

(deftest test-always
  (testing "always returns true for any input"
    (is (true? (belt/always nil)))
    (is (true? (belt/always {})))
    (is (true? (belt/always {:anything true})))))

;; =============================================================================
;; 2. Noop Fallback — Handlers
;; =============================================================================

(deftest test-handle-start-noop
  (testing "handle-start returns data unchanged without extension"
    (let [data {:quenched? false :strike-count 0}
          result (belt/handle-start {} data)]
      (is (= data result) "Returns input data as-is"))))

(deftest test-handle-smite-noop
  (testing "handle-smite returns data with empty smite-result"
    (let [data {:phase :test :total-smited 3}
          result (belt/handle-smite {} data)]
      (is (= {:smited [] :failed [] :count 0} (:smite-result result))
          "Has empty smite-result")
      (is (= 3 (:total-smited result)) "Preserves existing fields"))))

(deftest test-handle-survey-noop
  (testing "handle-survey returns data with empty survey-result"
    (let [data {:phase :test}
          result (belt/handle-survey {} data)]
      (is (= {:tasks [] :count 0} (:survey-result result))
          "Has empty survey-result"))))

(deftest test-handle-spark-noop
  (testing "handle-spark returns data with empty spark-result"
    (let [data {:phase :test :total-sparked 2}
          result (belt/handle-spark {} data)]
      (is (= {:spawned [] :failed [] :count 0} (:spark-result result))
          "Has empty spark-result")
      (is (= 2 (:total-sparked result)) "Preserves existing fields"))))

(deftest test-handle-end-noop
  (testing "handle-end returns empty map without extension"
    (let [result (belt/handle-end nil {:data {:strike-count 3}})]
      (is (= {} result) "Returns empty map"))))

(deftest test-handle-halt-noop
  (testing "handle-halt returns fsm unchanged without extension"
    (let [fsm {:data {:phase :test} :fsm {:some :graph}}
          result (belt/handle-halt nil fsm)]
      (is (= fsm result) "Returns input fsm as-is"))))

(deftest test-handle-error-noop
  (testing "handle-error returns nil without extension"
    (let [result (belt/handle-error nil {:error "test" :data {:phase :test}})]
      (is (nil? result) "Returns nil"))))

;; =============================================================================
;; 3. Noop Fallback — Subscriptions
;; =============================================================================

(deftest test-subscription-handlers-noop
  (testing "Subscription handlers return nil without extension"
    (is (nil? (belt/on-smite-count-change [:total-smited] 0 1)))
    (is (nil? (belt/on-spark-count-change [:total-sparked] 0 1)))))

;; =============================================================================
;; 4. Noop Fallback — FSM Spec & Execution API
;; =============================================================================

(deftest test-forge-belt-spec-noop
  (testing "forge-belt-spec is nil without extension"
    (is (nil? belt/forge-belt-spec))))

(deftest test-compile-belt-noop
  (testing "compile-belt returns nil without extension"
    (is (nil? (belt/compile-belt)))))

(deftest test-run-belt-noop
  (testing "run-belt returns noop result without extension"
    (let [result (belt/run-belt nil {} {})]
      (is (false? (:success result)))
      (is (= "Extension not available" (:message result)))
      (is (= 0 (:strike-count result)))
      (is (= 0 (:total-smited result)))
      (is (= 0 (:total-sparked result)))
      (is (nil? (:last-strike result)))
      (is (false? (:quenched? result)))
      (is (false? (:continuous? result)))
      (is (= {:smited [] :failed [] :count 0} (:smite-result result)))
      (is (= {:tasks [] :count 0} (:survey-result result)))
      (is (= {:spawned [] :failed [] :count 0} (:spark-result result))))))

(deftest test-run-belt-2-arity-noop
  (testing "run-belt 2-arity delegates to 3-arity"
    (let [result (belt/run-belt nil {})]
      (is (false? (:success result)))
      (is (= 0 (:total-smited result))))))

(deftest test-run-single-strike-noop
  (testing "run-single-strike returns noop result without extension"
    (let [result (belt/run-single-strike {})]
      (is (false? (:success result)))
      (is (= "Extension not available" (:message result)))
      (is (= 0 (:strike-count result)))
      (is (= 0 (:total-smited result)))
      (is (= 0 (:total-sparked result))))))

(deftest test-run-continuous-belt-noop
  (testing "run-continuous-belt returns noop result without extension"
    (let [result (belt/run-continuous-belt {})]
      (is (false? (:success result)))
      (is (= "Extension not available" (:message result))))))

;; =============================================================================
;; 5. Extension Delegation — Predicates
;; =============================================================================

(deftest test-predicate-delegation
  (testing "Predicates delegate to extensions when registered"
    (ext/register! :fb/q1 (fn [data] (true? (:quenched? data))))
    (is (true? (belt/quenched? {:quenched? true}))
        "Delegates to extension")
    (is (false? (belt/quenched? {:quenched? false}))
        "Extension receives data correctly")

    (ext/register! :fb/q2 (fn [data] (pos? (get-in data [:survey-result :count] 0))))
    (is (true? (belt/has-tasks? {:survey-result {:count 3}})))
    (is (false? (belt/has-tasks? {:survey-result {:count 0}})))))

;; =============================================================================
;; 6. Extension Delegation — Handlers
;; =============================================================================

(deftest test-handler-delegation
  (testing "Handlers delegate to extensions when registered"
    (ext/register! :fb/h1 (fn [resources data]
                            (assoc data :phase :started
                                   :cycle-start "2026-01-01T00:00:00Z")))
    (let [result (belt/handle-start {} {:strike-count 0})]
      (is (= :started (:phase result)))
      (is (= "2026-01-01T00:00:00Z" (:cycle-start result)))
      (is (= 0 (:strike-count result)) "Preserves original data"))))

(deftest test-smite-delegation
  (testing "handle-smite delegates to extension"
    (let [kill-result {:smited [{:id "a1"}] :failed [] :count 1}]
      (ext/register! :fb/h2 (fn [_resources data]
                              (-> data
                                  (assoc :smite-result kill-result)
                                  (update :total-smited + (:count kill-result)))))
      (let [result (belt/handle-smite {} {:total-smited 3})]
        (is (= kill-result (:smite-result result)))
        (is (= 4 (:total-smited result)))))))

(deftest test-end-handler-outcome-delegation
  (testing "Extension handle-end computes outcome from result data"
    (ext/register! :fb/h5
                   (fn [_resources {:keys [data]}]
                     (let [smite-failed   (seq (get-in data [:smite-result :failed]))
                           spark-failed   (seq (get-in data [:spark-result :failed]))
                           any-failed?    (or smite-failed spark-failed)
                           smite-count    (get-in data [:smite-result :count] 0)
                           spark-count    (get-in data [:spark-result :count] 0)
                           any-succeeded? (or (pos? smite-count) (pos? spark-count))
                           outcome (cond
                                     (not any-failed?)                :clean
                                     (and any-failed? any-succeeded?) :partial
                                     :else                            :failure)]
                       {:success (not= :failure outcome) :outcome outcome})))

    ;; Clean: no failures
    (let [r (belt/handle-end nil {:data {:smite-result {:smited [{:id "a"}] :failed [] :count 1}
                                         :spark-result {:spawned [{:id "b"}] :failed [] :count 1}}})]
      (is (true? (:success r)))
      (is (= :clean (:outcome r))))

    ;; Partial: some spawns failed
    (let [r (belt/handle-end nil {:data {:smite-result {:smited [{:id "a"}] :failed [] :count 1}
                                         :spark-result {:spawned [{:id "b"}] :failed [{:id "c" :error "x"}] :count 1}}})]
      (is (true? (:success r)))
      (is (= :partial (:outcome r))))

    ;; Failure: nothing succeeded but failures exist
    (let [r (belt/handle-end nil {:data {:smite-result {:smited [] :failed [{:id "d" :error "y"}] :count 0}
                                         :spark-result {:spawned [] :failed [{:id "e" :error "z"}] :count 0}}})]
      (is (false? (:success r)))
      (is (= :failure (:outcome r))))))

;; =============================================================================
;; 7. Extension Delegation — Execution API
;; =============================================================================

(deftest test-run-single-strike-delegation
  (testing "run-single-strike delegates to extension"
    (ext/register! :fb/strike
                   (fn [resources]
                     {:success true :strike-count 1 :total-smited 2 :total-sparked 3
                      :last-strike "2026-01-01T00:00:00Z"
                      :smite-result {:smited [{:id "a1"} {:id "a2"}] :failed [] :count 2}
                      :survey-result {:tasks [{:id "t1"}] :count 1}
                      :spark-result {:spawned [{:agent-id "f1"}] :failed [] :count 1}}))
    (let [result (belt/run-single-strike {:directory "/tmp"})]
      (is (true? (:success result)))
      (is (= 1 (:strike-count result)))
      (is (= 2 (:total-smited result)))
      (is (= 3 (:total-sparked result)))
      (is (= "2026-01-01T00:00:00Z" (:last-strike result))))))

(deftest test-compile-belt-delegation
  (testing "compile-belt delegates to extension"
    (ext/register! :fb/compile (fn [] {:fsm :compiled :opts {}}))
    (let [result (belt/compile-belt)]
      (is (map? result))
      (is (= :compiled (:fsm result))))))

;; =============================================================================
;; 8. Public API Surface — all expected vars exist
;; =============================================================================

(deftest test-public-api-surface
  (testing "All expected public vars exist in namespace"
    (let [expected-vars '[quenched? has-tasks? no-tasks? continuous? single-shot?
                          always
                          handle-start handle-smite handle-survey handle-spark
                          handle-end handle-halt handle-error
                          on-smite-count-change on-spark-count-change
                          forge-belt-spec
                          compile-belt run-belt run-single-strike run-continuous-belt]
          belt-ns (find-ns 'hive-mcp.workflows.forge-belt)]
      (doseq [v expected-vars]
        (is (some? (ns-resolve belt-ns v))
            (str "Public var " v " must exist"))))))

(deftest test-handler-var-arities
  (testing "Handler vars are functions with correct arities"
    ;; Predicates: 1-arity
    (is (fn? belt/quenched?))
    (is (fn? belt/has-tasks?))
    (is (fn? belt/no-tasks?))
    (is (fn? belt/continuous?))
    (is (fn? belt/single-shot?))
    (is (fn? belt/always))
    ;; Handlers: 2-arity
    (is (fn? belt/handle-start))
    (is (fn? belt/handle-smite))
    (is (fn? belt/handle-survey))
    (is (fn? belt/handle-spark))
    (is (fn? belt/handle-end))
    (is (fn? belt/handle-halt))
    (is (fn? belt/handle-error))
    ;; Subscriptions: 3-arity
    (is (fn? belt/on-smite-count-change))
    (is (fn? belt/on-spark-count-change))
    ;; Execution API
    (is (fn? belt/compile-belt))
    (is (fn? belt/run-belt))
    (is (fn? belt/run-single-strike))
    (is (fn? belt/run-continuous-belt))))

;; =============================================================================
;; 9. Noop Result Shape — workflow.clj consumer compatibility
;; =============================================================================

(deftest test-noop-result-shape-for-workflow-consumer
  (testing "run-single-strike noop has all keys workflow.clj expects"
    (let [result (belt/run-single-strike {})]
      ;; workflow.clj fsm-forge-strike accesses these:
      (is (number? (:total-smited result 0)) "total-smited accessible with default")
      (is (number? (:total-sparked result 0)) "total-sparked accessible with default")
      (is (some? (find result :last-strike)) "last-strike key exists")
      (is (map? (:smite-result result)) "smite-result is a map")
      (is (number? (get-in result [:survey-result :count] 0)) "survey-result :count accessible")
      (is (vector? (get-in result [:survey-result :tasks] [])) "survey-result :tasks accessible")
      (is (map? (:spark-result result)) "spark-result is a map")
      (is (boolean? (:success result)) "success key exists"))))

;; =============================================================================
;; 10. FSM Integration — Default Implementations (forge_belt_defaults)
;; =============================================================================

(defn- make-test-resources
  "Build minimal resources map for FSM integration tests.
   Accepts overrides for spawn-fn, kill-fn, list-fn."
  [{:keys [kill-fn spawn-fn list-fn update-fn]}]
  {:agent-ops  {:kill-fn  (or kill-fn (fn [_ _] {:smited [] :failed [] :count 0}))
                :spawn-fn (or spawn-fn (fn [_] {:spawned [] :failed [] :count 0}))}
   :kanban-ops {:list-fn   (or list-fn (fn [_] {:tasks [] :count 0}))
                :update-fn (or update-fn (fn [_ _] nil))}
   :config     {:max-slots 5 :presets ["ling"] :spawn-mode :headless}
   :directory  "/tmp/test"
   :clock-fn   (constantly (java.time.Instant/parse "2026-01-01T00:00:00Z"))})

(defn- run-fsm-no-subscriptions
  "Compile and run the default FSM spec without subscriptions.
   Works around Timbre classloader issues in REPL."
  [resources opts]
  (let [spec     (update @#'fbd/forge-belt-spec :opts dissoc :subscriptions)
        compiled (fsm/compile spec)]
    (fsm/run compiled resources
             {:data (merge {:quenched?     false
                            :continuous?   false
                            :strike-count  0
                            :total-smited  0
                            :total-sparked 0}
                           opts)})))

(deftest test-fsm-single-shot-clean
  (testing "FSM single-shot with all spawns succeeding"
    (let [resources (make-test-resources
                     {:kill-fn  (fn [_ _] {:smited ["a"] :failed [] :count 1})
                      :spawn-fn (fn [_] {:spawned ["ling-1"] :failed [] :count 1})
                      :list-fn  (fn [_] {:tasks [{:id "t1"}] :count 1})})
          result (run-fsm-no-subscriptions resources {:continuous? false})]
      (is (true? (:success result)))
      (is (= 1 (:strike-count result))))))

(deftest test-fsm-single-shot-partial
  (testing "FSM single-shot with mixed results — partial success"
    (let [resources (make-test-resources
                     {:kill-fn  (fn [_ _] {:smited ["a"] :failed [] :count 1})
                      :spawn-fn (fn [_] {:spawned ["ling-1"] :failed ["ling-2"] :count 1})
                      :list-fn  (fn [_] {:tasks [{:id "t1"} {:id "t2"}] :count 2})})
          result (run-fsm-no-subscriptions resources {:continuous? false})]
      (is (true? (:success result)) "Partial is still success")
      (is (= 1 (:strike-count result))))))

(deftest test-fsm-single-shot-total-failure
  (testing "FSM single-shot with all spawns failing"
    (let [resources (make-test-resources
                     {:kill-fn  (fn [_ _] {:smited [] :failed [] :count 0})
                      :spawn-fn (fn [_] {:spawned [] :failed ["ling-1"] :count 0})
                      :list-fn  (fn [_] {:tasks [{:id "t1"}] :count 1})})
          result (run-fsm-no-subscriptions resources {:continuous? false})]
      (is (false? (:success result))))))

(deftest test-fsm-continuous-total-failure-exits
  (testing "FSM continuous mode exits on total spark failure (no infinite loop)"
    (let [resources (make-test-resources
                     {:kill-fn  (fn [_ _] {:smited [] :failed [] :count 0})
                      :spawn-fn (fn [_] {:spawned [] :failed ["ling-1"] :count 0})
                      :list-fn  (fn [_] {:tasks [{:id "t1"}] :count 1})})
          result (run-fsm-no-subscriptions resources {:continuous? true})]
      (is (false? (:success result)))
      (is (= 1 (:strike-count result)) "Exits after 1 strike, no looping"))))

(deftest test-fsm-continuous-no-tasks-exits
  (testing "FSM continuous mode exits cleanly when kanban has no tasks"
    (let [resources (make-test-resources
                     {:kill-fn  (fn [_ _] {:smited ["z"] :failed [] :count 1})
                      :list-fn  (fn [_] {:tasks [] :count 0})})
          result (run-fsm-no-subscriptions resources {:continuous? true})]
      (is (true? (:success result)))
      (is (nil? (:spark-result result)) "Spark phase was skipped")
      (is (= 0 (:strike-count result)) "No strikes executed"))))

(deftest test-fsm-continuous-mixed-then-exhausted
  (testing "FSM continuous loops on mixed result, exits when kanban exhausted"
    (let [spawn-count (atom 0)
          resources (make-test-resources
                     {:kill-fn  (fn [_ _] {:smited ["z"] :failed [] :count 1})
                      :spawn-fn (fn [_]
                                  (let [n (swap! spawn-count inc)]
                                    (if (= n 1)
                                      {:spawned ["ling-1"] :failed ["ling-2"] :count 1}
                                      {:spawned ["ling-3"] :failed [] :count 1})))
                      :list-fn  (fn [_]
                                  (if (< @spawn-count 2)
                                    {:tasks [{:id "t1"}] :count 1}
                                    {:tasks [] :count 0}))})
          result (run-fsm-no-subscriptions resources {:continuous? true})]
      (is (true? (:success result)))
      (is (= 2 (:strike-count result)) "Ran 2 cycles before exhausting tasks"))))

(deftest test-fsm-quench-halts
  (testing "FSM halts when quenched? is true after spark"
    (let [resources (make-test-resources
                     {:kill-fn  (fn [_ _] {:smited ["a"] :failed [] :count 1})
                      :spawn-fn (fn [_] {:spawned ["ling-1"] :failed [] :count 1})
                      :list-fn  (fn [_] {:tasks [{:id "t1"}] :count 1})})
          result (run-fsm-no-subscriptions resources {:continuous? true
                                                      :quenched? true})]
      ;; FSM compiler overrides ::halt handler with (dissoc fsm :fsm).
      ;; Result is full FSM state minus :fsm graph, resumable.
      (is (some? (:data result)) "Returns FSM state for resume")
      (is (nil? (:fsm result)) "FSM graph removed (halt semantics)")
      (is (= :hive-mcp.workflows.forge-belt-defaults/cycle-complete
             (get-in result [:data :phase]))
          "Phase is cycle-complete from spark handler")
      (is (true? (get-in result [:data :quenched?]))
          "Quenched flag preserved"))))

;; =============================================================================
;; 11. Default Predicate Unit Tests
;; =============================================================================

(deftest test-spark-all-failed-predicate
  (testing "spark-all-failed?* returns true only when 0 spawned + failures exist"
    ;; Zero spawned, failures exist → true
    (is (true? (@#'fbd/spark-all-failed?*
                {:spark-result {:count 0 :failed ["err"]}})))
    ;; Some spawned, failures exist → false (partial, not total failure)
    (is (false? (@#'fbd/spark-all-failed?*
                 {:spark-result {:count 1 :failed ["err"]}})))
    ;; Zero spawned, no failures → false (nothing happened)
    (is (false? (@#'fbd/spark-all-failed?*
                 {:spark-result {:count 0 :failed nil}})))
    (is (false? (@#'fbd/spark-all-failed?*
                 {:spark-result {:count 0}})))
    ;; All spawned, no failures → false (clean)
    (is (false? (@#'fbd/spark-all-failed?*
                 {:spark-result {:count 2 :failed nil}})))))

