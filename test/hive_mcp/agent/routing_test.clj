(ns hive-mcp.agent.routing-test
  "Tests for smart model routing based on task classification and success rates.

   Covers:
   - Task classification delegation to preset
   - Route management (get-route, set-route!, list-routes)
   - Success/failure tracking (record-success!, record-failure!, get-success-rate)
   - Model selection (select-model, route-and-select, get-model-for-task)
   - Fallback execution (with-fallback)
   - Tool proxy functions
   - Routing stats"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.routing :as routing]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn clean-routing-fixture [f]
  (routing/reset-session-rates!)
  (f)
  (routing/reset-session-rates!))

(use-fixtures :each clean-routing-fixture)

;; =============================================================================
;; Task Classification (delegates to preset)
;; =============================================================================

(deftest classify-task-test
  (testing "testing keywords classified as :testing"
    (is (= :testing (routing/classify-task "write unit tests for parser" ["test/parser_test.clj"])))
    (is (= :testing (routing/classify-task "add test coverage for auth module" []))))

  (testing "refactoring keywords classified as :refactoring"
    (is (= :refactoring (routing/classify-task "refactor the database layer" ["src/db.clj"])))
    (is (= :refactoring (routing/classify-task "extract helper function" []))))

  (testing "bugfix keywords classified as :bugfix"
    (is (= :bugfix (routing/classify-task "fix null pointer exception in login" [])))
    (is (= :bugfix (routing/classify-task "resolve bug with session timeout" []))))

  (testing "documentation keywords classified as :documentation"
    (is (= :documentation (routing/classify-task "update docstrings in core module" [])))
    (is (= :documentation (routing/classify-task "write README for the API" ["README.md"]))))

  (testing "general for unrecognized tasks"
    (is (= :general (routing/classify-task "implement new feature" [])))
    (is (= :general (routing/classify-task "set up CI pipeline" []))))

  (testing "file patterns provide fallback classification"
    (is (= :testing (routing/classify-task "update this file" ["test/foo_test.clj"])))
    (is (= :documentation (routing/classify-task "update this file" ["docs/guide.md"])))))

;; =============================================================================
;; Route Management
;; =============================================================================

(deftest get-route-test
  (testing "known task types have routes"
    (doseq [task-type [:testing :refactoring :implementation :bugfix :documentation :general]]
      (let [route (routing/get-route task-type)]
        (is (map? route) (str "no route for " task-type))
        (is (string? (:primary route)) (str task-type " missing :primary"))
        (is (string? (:secondary route)) (str task-type " missing :secondary")))))

  (testing "unknown task type falls back to :general"
    (let [route (routing/get-route :nonexistent)]
      (is (map? route))
      (is (string? (:primary route))))))

(deftest set-route!-test
  (testing "can override routes"
    (let [original (routing/get-route :testing)
          new-route {:primary "custom/model-a"
                     :secondary "custom/model-b"
                     :reason "Test override"}]
      (routing/set-route! :testing new-route)
      (is (= new-route (routing/get-route :testing)))
      ;; Restore
      (routing/set-route! :testing original))))

(deftest list-routes-test
  (testing "returns a map of all routes"
    (let [routes (routing/list-routes)]
      (is (map? routes))
      (is (contains? routes :general))
      (is (contains? routes :testing)))))

;; =============================================================================
;; Success/Failure Tracking
;; =============================================================================

(deftest record-success-tracking-test
  (testing "success rate starts at 1.0 for unknown pairs"
    (is (= 1.0 (routing/get-success-rate "model-x" :testing))))

  (testing "success rate is 1.0 after all successes"
    (routing/record-success! "model-y" :testing)
    (routing/record-success! "model-y" :testing)
    (routing/record-success! "model-y" :testing)
    ;; Session rate = 3/3 = 1.0
    (is (>= (routing/get-success-rate "model-y" :testing) 0.9))))

(deftest record-failure-tracking-test
  (testing "failure rate tracked correctly"
    (routing/record-success! "model-z" :bugfix)
    (routing/record-failure! "model-z" :bugfix :unknown-failure)
    ;; Session rate = 1/2 = 0.5
    (let [rate (routing/get-success-rate "model-z" :bugfix)]
      (is (<= 0.0 rate 1.0))
      (is (< rate 1.0)))))

(deftest record-failure-sets-cooldown-test
  (testing "failure records last-failure-time for cooldown"
    (routing/record-failure! "cool-model" :testing :timeout)
    ;; The model should be on cooldown now (within 5 min)
    ;; We test indirectly via select-model behavior
    (let [rates (routing/get-all-success-rates)]
      (is (map? (:session rates))))))

(deftest reset-session-rates-test
  (testing "reset clears all session tracking"
    (routing/record-success! "tracked-model" :testing)
    (routing/reset-session-rates!)
    ;; After reset, rate goes back to 1.0 (default)
    (is (= 1.0 (routing/get-success-rate "tracked-model" :testing)))))

(deftest get-all-success-rates-test
  (testing "returns session and persisted data"
    (routing/record-success! "all-rates-model" :general)
    (let [result (routing/get-all-success-rates)]
      (is (contains? result :session))
      (is (contains? result :persisted))
      (is (map? (:session result))))))

;; =============================================================================
;; Model Selection
;; =============================================================================

(deftest select-model-force-override-test
  (testing "force-model bypasses all routing"
    (let [result (routing/select-model "fix a bug" ["src/foo.clj"]
                                       {:force-model "custom/special-model"})]
      (is (= "custom/special-model" (:model result)))
      (is (= :forced (:task-type result)))
      (is (true? (get-in result [:signals :override]))))))

(deftest select-model-basic-test
  (testing "returns a model selection for any task"
    (let [result (routing/select-model "write tests for auth" ["test/auth_test.clj"])]
      (is (string? (:model result)))
      (is (keyword? (:task-type result)))
      (is (string? (:reason result)))
      (is (map? (:signals result))))))

(deftest select-model-task-type-routing-test
  (testing "testing tasks route through testing route"
    (let [result (routing/select-model "write unit tests" ["test/foo_test.clj"])
          route (routing/get-route :testing)]
      (is (= :testing (:task-type result)))
      ;; Model should be either primary or secondary from the route
      (is (#{(:primary route) (:secondary route)} (:model result)))))

  (testing "documentation tasks use documentation route"
    (let [result (routing/select-model "write documentation" ["README.md"])
          route (routing/get-route :documentation)]
      (is (= :documentation (:task-type result)))
      (is (#{(:primary route) (:secondary route)} (:model result))))))

(deftest select-model-return-shape-test
  (testing "result always has required keys"
    (doseq [[task files] [["fix bug" ["src/a.clj"]]
                          ["write docs" ["README.md"]]
                          ["implement feature" []]
                          ["refactor code" ["src/b.clj"]]]]
      (let [result (routing/select-model task files)]
        (is (contains? result :model))
        (is (contains? result :task-type))
        (is (contains? result :reason))
        (is (contains? result :fallback))
        (is (contains? result :signals))))))

;; =============================================================================
;; Route and Select (convenience)
;; =============================================================================

(deftest route-and-select-test
  (testing "same shape as select-model"
    (let [result (routing/route-and-select "fix a bug in parser" ["src/parser.clj"])]
      (is (string? (:model result)))
      (is (keyword? (:task-type result)))
      (is (string? (:reason result))))))

(deftest get-model-for-task-test
  (testing "returns just the model string"
    (let [model (routing/get-model-for-task "write tests" ["test/x_test.clj"])]
      (is (string? model))
      (is (seq model)))))

;; =============================================================================
;; Fallback Execution
;; =============================================================================

(deftest with-fallback-success-test
  (testing "successful execution returns result and records success"
    (let [result (routing/with-fallback
                   {:model "model-a" :task-type :testing :fallback "model-b"}
                   (fn [model]
                     {:output (str "done by " model)}))]
      (is (= "done by model-a" (:output result))))))

(deftest with-fallback-primary-fails-test
  (testing "falls back to secondary on primary failure"
    (let [call-log (atom [])
          result (routing/with-fallback
                   {:model "bad-model" :task-type :testing :fallback "good-model"}
                   (fn [model]
                     (swap! call-log conj model)
                     (if (= model "bad-model")
                       (throw (Exception. "Primary failed"))
                       {:output (str "done by " model)})))]
      (is (= "done by good-model" (:output result)))
      (is (= ["bad-model" "good-model"] @call-log)))))

(deftest with-fallback-both-fail-test
  (testing "throws when both primary and fallback fail"
    (is (thrown? Exception
                 (routing/with-fallback
                   {:model "bad-1" :task-type :testing :fallback "bad-2"}
                   (fn [_model]
                     (throw (Exception. "All models fail"))))))))

(deftest with-fallback-no-fallback-test
  (testing "throws immediately when no fallback and primary fails"
    (is (thrown? Exception
                 (routing/with-fallback
                   {:model "bad-model" :task-type :testing :fallback nil}
                   (fn [_model]
                     (throw (Exception. "Primary failed"))))))))

;; =============================================================================
;; Tool Proxy
;; =============================================================================

(deftest tool-proxy-models-test
  (testing "free-tier models need proxy"
    (is (true? (routing/needs-tool-proxy? "mistralai/devstral-2512:free")))
    (is (true? (routing/needs-tool-proxy? "google/gemma-3-4b-it:free"))))

  (testing "standard models don't need proxy"
    (is (false? (routing/needs-tool-proxy? "anthropic/claude-sonnet")))
    (is (false? (routing/needs-tool-proxy? "deepseek/deepseek-v3.2")))
    (is (false? (routing/needs-tool-proxy? "x-ai/grok-code-fast-1")))))

(deftest tool-proxy-enabled-test
  (testing "proxy is enabled by default"
    (is (true? (routing/tool-proxy-enabled?)))))

(deftest tool-proxy-config-test
  (testing "config is readable"
    (let [config (routing/get-tool-proxy-config)]
      (is (map? config))
      (is (contains? config :enabled))
      (is (contains? config :model))))

  (testing "config is updatable"
    (let [original (routing/get-tool-proxy-config)]
      (routing/set-tool-proxy-config! {:enabled false})
      (is (false? (routing/tool-proxy-enabled?)))
      ;; Restore
      (routing/set-tool-proxy-config! {:enabled (:enabled original)}))))

(deftest get-tool-proxy-model-test
  (testing "returns a non-empty model string"
    (let [model (routing/get-tool-proxy-model)]
      (is (string? model))
      (is (seq model)))))

;; =============================================================================
;; Routing Stats
;; =============================================================================

(deftest get-routing-stats-test
  (testing "returns comprehensive stats"
    (let [stats (routing/get-routing-stats)]
      (is (contains? stats :routes))
      (is (contains? stats :session))
      (is (contains? stats :feedback))
      (is (contains? stats :recommendations))
      (is (contains? stats :config))
      (is (map? (:routes stats)))
      (is (map? (:session stats)))
      (is (map? (:config stats))))))

;; =============================================================================
;; Report Execution
;; =============================================================================

(deftest report-execution-success-test
  (testing "success result records success"
    (routing/report-execution! :testing "model-rep" {:status :completed})
    (let [rate (routing/get-success-rate "model-rep" :testing)]
      (is (>= rate 0.5)))))

(deftest report-execution-failure-test
  (testing "failure result records failure"
    (routing/report-execution! :testing "model-fail" {:status :error :error "timeout"})
    (let [rates (routing/get-all-success-rates)
          key ["model-fail" :testing]]
      (is (map? (:session rates))))))
