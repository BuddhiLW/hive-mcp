(ns hive-mcp.workflows.sub-fsms.tool-execution-test
  "Tests for the tool-execution sub-FSM.

   Tests the PURE handler layer -- no side effects, no real sandbox.
   All external dependencies injected via mock resources.

   Test categories:
   1. Dispatch predicates -- pure boolean functions of state data
   2. Handler unit tests -- each handler with mock resources
   3. FSM integration -- full compile+run with mock resources
   4. Permission denial path -- denied tools produce observations
   5. Timeout handling -- execution timeout produces error
   6. Batch execution -- multiple tool calls in sequence
   7. Sub-FSM helper -- run-sub-fsm parent embedding
   8. Graceful degradation -- missing resources, nil inputs

   SOLID: D -- Tests depend on abstractions (resources), not concretions.
   CLARITY: T -- Telemetry via test assertions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.sub-fsms.tool-execution :as te]
            [hive.events.fsm :as fsm]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Data
;; =============================================================================

(def sample-tool-call
  {:id   "call-001"
   :name "read_file"
   :arguments {:path "/src/foo.clj"}})

(def sample-blocked-tool-call
  {:id   "call-002"
   :name "bash"
   :arguments {:command "rm -rf /"}})

(def sample-propose-diff-call
  {:id   "call-003"
   :name "propose_diff"
   :arguments {:file_path "/src/foo.clj"
               :old_content "(defn old [])"
               :new_content "(defn new [])"}})

(def sample-sandbox
  {:allowed-files    #{"/src/foo.clj" "/src/bar.clj"}
   :allowed-dirs     #{"/src"}
   :blocked-patterns []
   :blocked-tools    #{"bash" "file_write" "delegate_drone"}})

(def sample-file-content
  "(ns foo.bar)\n\n(defn hello [x] (str \"hello \" x))")

;; =============================================================================
;; Test Helpers -- Mock Resources
;; =============================================================================

(defn mock-resources
  "Build a mock resources map for handler tests.

   Options:
     :sandbox         -- sandbox spec (default: sample-sandbox)
     :execute-result  -- return value for :execute-tool-fn
     :execute-throws? -- if true, execute-tool-fn throws
     :execute-slow-ms -- if set, execute-tool-fn sleeps this many ms
     :timeout-ms      -- tool execution timeout
     :record-calls    -- atom to capture record-observation-fn calls
     :audit-calls     -- atom to capture audit-fn calls
     :no-sandbox?     -- if true, omit sandbox from resources
     :no-executor?    -- if true, omit execute-tool-fn"
  ([] (mock-resources {}))
  ([{:keys [sandbox execute-result execute-throws? execute-slow-ms
            timeout-ms record-calls audit-calls no-sandbox? no-executor?]
     :or {sandbox        sample-sandbox
          execute-result sample-file-content
          timeout-ms     5000}}]
   (cond-> {:timeout-ms timeout-ms}

     (not no-sandbox?)
     (assoc :sandbox sandbox
            :sandbox-allows-fn
            (fn [sb tool-name _args]
              (if (contains? (:blocked-tools sb) tool-name)
                {:allowed? false
                 :reason (str "Tool '" tool-name "' is blocked for drones")}
                {:allowed? true})))

     (not no-executor?)
     (assoc :execute-tool-fn
            (cond
              execute-throws?
              (fn [_name _args]
                (throw (Exception. "tool execution failed")))

              execute-slow-ms
              (fn [_name _args]
                (Thread/sleep execute-slow-ms)
                execute-result)

              :else
              (constantly execute-result)))

     record-calls
     (assoc :record-observation-fn
            (fn [obs-map]
              (swap! record-calls conj obs-map)))

     audit-calls
     (assoc :audit-fn
            (fn [tool-name args allowed?]
              (swap! audit-calls conj {:tool tool-name :args args :allowed? allowed?}))))))

(defn base-data
  "Build base input data for tests."
  ([] (base-data sample-tool-call))
  ([tool-call]
   {:tool-call tool-call
    :drone-id  "drone-test-001"
    :turn      0}))

;; =============================================================================
;; 1. Dispatch Predicates
;; =============================================================================

(deftest test-permission-checked?
  (testing "returns false when no permission-result"
    (is (not (te/permission-checked? {})))
    (is (not (te/permission-checked? {:tool-call sample-tool-call}))))

  (testing "returns true when permission-result present"
    (is (te/permission-checked? {:permission-result {:allowed? true}}))
    (is (te/permission-checked? {:permission-result {:allowed? false :reason "blocked"}}))))

(deftest test-execution-attempted?
  (testing "returns false before execution"
    (is (not (te/execution-attempted? {})))
    (is (not (te/execution-attempted? {:permission-result {:allowed? true}}))))

  (testing "returns true after execution"
    (is (te/execution-attempted? {:tool-result "some result"}))
    (is (te/execution-attempted? {:tool-result nil :denied? false})))

  (testing "returns true when denied (no execution needed)"
    (is (te/execution-attempted? {:denied? true}))))

(deftest test-observation-captured?
  (testing "returns false before capture"
    (is (not (te/observation-captured? {})))
    (is (not (te/observation-captured? {:tool-result "x"}))))

  (testing "returns true after capture"
    (is (te/observation-captured? {:observation "file contents here"}))
    (is (te/observation-captured? {:observation "(no result)"}))))

;; =============================================================================
;; 2. Handler Unit Tests
;; =============================================================================

(deftest test-handle-validate-permissions-allowed
  (testing "permits tool not in blocked set"
    (let [resources (mock-resources)
          data      (base-data)
          result    (te/handle-validate-permissions resources data)]
      (is (true? (get-in result [:permission-result :allowed?])))
      (is (false? (:denied? result)))
      (is (nil? (:error result))))))

(deftest test-handle-validate-permissions-denied
  (testing "denies tool in blocked set"
    (let [resources (mock-resources)
          data      (base-data sample-blocked-tool-call)
          result    (te/handle-validate-permissions resources data)]
      (is (false? (get-in result [:permission-result :allowed?])))
      (is (true? (:denied? result)))
      (is (clojure.string/includes?
           (get-in result [:permission-result :reason])
           "blocked")))))

(deftest test-handle-validate-permissions-nil-tool-call
  (testing "rejects nil tool call"
    (let [resources (mock-resources)
          data      {:drone-id "d-1" :turn 0}
          result    (te/handle-validate-permissions resources data)]
      (is (false? (get-in result [:permission-result :allowed?])))
      (is (true? (:denied? result)))
      (is (= "No tool call provided" (:error result))))))

(deftest test-handle-validate-permissions-nil-name
  (testing "rejects tool call with nil name"
    (let [resources (mock-resources)
          data      (base-data {:id "c-1" :name nil :arguments {}})
          result    (te/handle-validate-permissions resources data)]
      (is (true? (:denied? result)))
      (is (= "Tool call missing :name" (:error result))))))

(deftest test-handle-validate-permissions-no-sandbox
  (testing "permissive mode when no sandbox provided"
    (let [resources (mock-resources {:no-sandbox? true})
          data      (base-data sample-blocked-tool-call)
          result    (te/handle-validate-permissions resources data)]
      ;; Without sandbox, all tools are allowed (permissive mode)
      (is (true? (get-in result [:permission-result :allowed?])))
      (is (false? (:denied? result))))))

(deftest test-handle-validate-permissions-audit
  (testing "calls audit-fn when provided"
    (let [audit-calls (atom [])
          resources   (mock-resources {:audit-calls audit-calls})
          data        (base-data)
          _result     (te/handle-validate-permissions resources data)]
      (is (= 1 (count @audit-calls)))
      (is (= "read_file" (:tool (first @audit-calls))))
      (is (true? (:allowed? (first @audit-calls)))))))

(deftest test-handle-execute-permitted
  (testing "executes permitted tool and captures result"
    (let [resources (mock-resources {:execute-result "file contents"})
          data      (assoc (base-data)
                           :permission-result {:allowed? true}
                           :denied? false)
          result    (te/handle-execute resources data)]
      (is (= "file contents" (:tool-result result)))
      (is (nil? (:error result)))
      (is (number? (:execution-time-ms result)))
      (is (>= (:execution-time-ms result) 0)))))

(deftest test-handle-execute-denied
  (testing "skips execution for denied tool"
    (let [execute-called? (atom false)
          resources       (mock-resources {:execute-result
                                           (fn [_ _]
                                             (reset! execute-called? true)
                                             "should not run")})
          data            (assoc (base-data sample-blocked-tool-call)
                                 :permission-result {:allowed? false
                                                     :reason "Tool 'bash' is blocked"}
                                 :denied? true)
          result          (te/handle-execute resources data)]
      (is (false? @execute-called?))
      (is (true? (get-in result [:tool-result :isError])))
      (is (clojure.string/includes?
           (get-in result [:tool-result :text])
           "SANDBOX VIOLATION"))
      (is (= 0 (:execution-time-ms result))))))

(deftest test-handle-execute-no-executor
  (testing "graceful degradation when no executor provided"
    (let [resources (mock-resources {:no-executor? true})
          data      (assoc (base-data)
                           :permission-result {:allowed? true}
                           :denied? false)
          result    (te/handle-execute resources data)]
      (is (nil? (:tool-result result)))
      (is (= "No tool executor provided" (:error result)))
      (is (= 0 (:execution-time-ms result))))))

(deftest test-handle-execute-exception
  (testing "catches execution exception"
    (let [resources (mock-resources {:execute-throws? true})
          data      (assoc (base-data)
                           :permission-result {:allowed? true}
                           :denied? false)
          result    (te/handle-execute resources data)]
      (is (true? (get-in result [:tool-result :isError])))
      (is (clojure.string/includes? (:error result) "Execution error"))
      (is (number? (:execution-time-ms result))))))

(deftest test-handle-execute-timeout
  (testing "times out on slow execution"
    (let [resources (mock-resources {:execute-slow-ms 500
                                     :timeout-ms 100})
          data      (assoc (base-data)
                           :permission-result {:allowed? true}
                           :denied? false)
          result    (te/handle-execute resources data)]
      (is (true? (get-in result [:tool-result :isError])))
      (is (clojure.string/includes? (:error result) "Timeout"))
      (is (>= (:execution-time-ms result) 100)))))

(deftest test-handle-capture-result-text-map
  (testing "captures result from map with :text"
    (let [resources (mock-resources)
          data      (assoc (base-data)
                           :tool-result {:type "text" :text "hello world"}
                           :denied? false
                           :execution-time-ms 42)
          result    (te/handle-capture-result resources data)]
      (is (= "hello world" (:observation result)))
      (is (string? (:tool-call-id result))))))

(deftest test-handle-capture-result-content-blocks
  (testing "captures result from map with :content blocks"
    (let [resources (mock-resources)
          data      (assoc (base-data)
                           :tool-result {:content [{:type "text" :text "line 1"}
                                                   {:type "text" :text "line 2"}]}
                           :denied? false
                           :execution-time-ms 10)
          result    (te/handle-capture-result resources data)]
      (is (= "line 1\nline 2" (:observation result))))))

(deftest test-handle-capture-result-string
  (testing "captures string result directly"
    (let [resources (mock-resources)
          data      (assoc (base-data)
                           :tool-result "raw string result"
                           :denied? false
                           :execution-time-ms 5)
          result    (te/handle-capture-result resources data)]
      (is (= "raw string result" (:observation result))))))

(deftest test-handle-capture-result-nil
  (testing "handles nil result"
    (let [resources (mock-resources)
          data      (assoc (base-data)
                           :tool-result nil
                           :denied? false
                           :execution-time-ms 1)
          result    (te/handle-capture-result resources data)]
      (is (= "(no result)" (:observation result))))))

(deftest test-handle-capture-result-records-observation
  (testing "calls record-observation-fn when provided"
    (let [record-calls (atom [])
          resources    (mock-resources {:record-calls record-calls})
          data         (assoc (base-data)
                              :tool-result {:type "text" :text "data here"}
                              :denied? false
                              :execution-time-ms 25)
          _result      (te/handle-capture-result resources data)]
      (is (= 1 (count @record-calls)))
      (let [obs (first @record-calls)]
        (is (= "read_file" (:tool-name obs)))
        (is (= "drone-test-001" (:drone-id obs)))
        (is (= 0 (:turn obs)))
        (is (false? (:denied? obs)))
        (is (= 25 (:execution-time-ms obs)))
        (is (= "data here" (:observation obs)))))))

(deftest test-handle-capture-result-tool-call-id
  (testing "uses tool-call :id when provided"
    (let [resources (mock-resources)
          data      (assoc (base-data {:id "my-custom-id" :name "read_file" :arguments {}})
                           :tool-result "ok"
                           :denied? false
                           :execution-time-ms 1)
          result    (te/handle-capture-result resources data)]
      (is (= "my-custom-id" (:tool-call-id result)))))

  (testing "generates fallback id when no :id in tool-call"
    (let [resources (mock-resources)
          data      (assoc (base-data {:name "grep" :arguments {:pattern "foo"}})
                           :tool-result "match found"
                           :denied? false
                           :execution-time-ms 1
                           :turn 3)
          result    (te/handle-capture-result resources data)]
      (is (= "call-grep-3" (:tool-call-id result))))))

;; =============================================================================
;; 3. FSM Integration
;; =============================================================================

(deftest test-fsm-full-allowed-execution
  (testing "full FSM run: allowed tool executes and produces observation"
    (let [resources (mock-resources {:execute-result sample-file-content})
          data      (base-data)
          result    (te/run-tool-execution data resources)]
      (is (= sample-file-content (:observation result)))
      (is (= sample-file-content (:tool-result result)))
      (is (false? (:denied? result)))
      (is (nil? (:error result)))
      (is (number? (:execution-time-ms result))))))

(deftest test-fsm-full-denied-execution
  (testing "full FSM run: denied tool produces sandbox violation observation"
    (let [resources (mock-resources)
          data      (base-data sample-blocked-tool-call)
          result    (te/run-tool-execution data resources)]
      (is (true? (:denied? result)))
      (is (clojure.string/includes? (:observation result) "SANDBOX VIOLATION"))
      (is (= 0 (:execution-time-ms result))))))

(deftest test-fsm-full-with-recording
  (testing "full FSM run records observation to KG"
    (let [record-calls (atom [])
          resources    (mock-resources {:record-calls record-calls
                                        :execute-result "contents"})
          data         (base-data)
          _result      (te/run-tool-execution data resources)]
      (is (= 1 (count @record-calls)))
      (is (= "read_file" (:tool-name (first @record-calls)))))))

(deftest test-fsm-compile-is-cached
  (testing "compiled FSM is a delay (compiled once, reused)"
    (is (delay? te/compiled))
    (is (some? @te/compiled))))

;; =============================================================================
;; 4. Permission Denial Path
;; =============================================================================

(deftest test-denial-produces-error-observation
  (testing "denied tool creates error observation without executing"
    (let [execute-called? (atom false)
          resources (assoc (mock-resources)
                           :execute-tool-fn
                           (fn [_ _]
                             (reset! execute-called? true)
                             "should not run"))
          data      (base-data sample-blocked-tool-call)
          result    (te/run-tool-execution data resources)]
      (is (false? @execute-called?))
      (is (true? (:denied? result)))
      (is (string? (:observation result)))
      (is (clojure.string/includes? (:observation result) "SANDBOX VIOLATION")))))

(deftest test-denial-multiple-blocked-tools
  (testing "different blocked tools all produce denials"
    (let [resources (mock-resources)]
      (doseq [tool-name ["bash" "file_write" "delegate_drone"]]
        (let [data   (base-data {:id (str "call-" tool-name)
                                 :name tool-name
                                 :arguments {}})
              result (te/run-tool-execution data resources)]
          (is (true? (:denied? result))
              (str tool-name " should be denied"))
          (is (clojure.string/includes? (:observation result) "SANDBOX VIOLATION")
              (str tool-name " should produce violation observation")))))))

;; =============================================================================
;; 5. Timeout Handling
;; =============================================================================

(deftest test-timeout-produces-error
  (testing "slow tool execution triggers timeout error"
    (let [resources (mock-resources {:execute-slow-ms 500
                                     :timeout-ms 100})
          data      (base-data)
          result    (te/run-tool-execution data resources)]
      (is (clojure.string/includes? (:error result) "Timeout"))
      (is (true? (get-in result [:tool-result :isError])))
      (is (>= (:execution-time-ms result) 100)))))

;; =============================================================================
;; 6. Batch Execution
;; =============================================================================

(deftest test-batch-execution
  (testing "batch executes multiple tool calls sequentially"
    (let [call-log   (atom [])
          resources  (mock-resources {:execute-result "ok"})
          resources  (assoc resources
                            :execute-tool-fn
                            (fn [name args]
                              (swap! call-log conj name)
                              (str "result-for-" name)))
          tool-calls [sample-tool-call
                      sample-propose-diff-call
                      {:id "call-004" :name "grep"
                       :arguments {:pattern "defn"}}]
          results    (te/run-tool-execution-batch
                      tool-calls resources
                      {:drone-id "drone-batch" :turn 0})]
      (is (= 3 (count results)))
      (is (= ["read_file" "propose_diff" "grep"] @call-log))
      (is (every? #(false? (:denied? %)) results))
      (is (= "result-for-read_file" (:observation (first results)))))))

(deftest test-batch-with-mixed-permissions
  (testing "batch handles mix of allowed and denied tools"
    (let [resources  (mock-resources {:execute-result "ok"})
          tool-calls [sample-tool-call         ;; allowed
                      sample-blocked-tool-call ;; denied
                      sample-propose-diff-call] ;; allowed
          results    (te/run-tool-execution-batch
                      tool-calls resources
                      {:drone-id "drone-mix" :turn 1})]
      (is (= 3 (count results)))
      (is (false? (:denied? (nth results 0))))
      (is (true?  (:denied? (nth results 1))))
      (is (false? (:denied? (nth results 2)))))))

;; =============================================================================
;; 7. Sub-FSM Helper
;; =============================================================================

(deftest test-run-sub-fsm-merges-into-parent
  (testing "run-sub-fsm extracts tool-call and merges results"
    (let [resources   {:sandbox           sample-sandbox
                       :sandbox-allows-fn (fn [sb name _]
                                            (if (contains? (:blocked-tools sb) name)
                                              {:allowed? false :reason "blocked"}
                                              {:allowed? true}))
                       :execute-tool-fn   (constantly "sub-fsm result")
                       :timeout-ms        5000}
          parent-data {:tool-call sample-tool-call
                       :drone-id  "drone-parent"
                       :turn      2
                       :extra-key "preserved"}
          result      (te/run-sub-fsm resources parent-data)]
      ;; Parent keys preserved
      (is (= "preserved" (:extra-key result)))
      (is (= 2 (:turn result)))
      ;; Sub-FSM keys merged
      (is (= "sub-fsm result" (:observation result)))
      (is (false? (:denied? result)))
      (is (number? (:execution-time-ms result))))))

(deftest test-run-sub-fsm-with-overrides
  (testing "run-sub-fsm accepts overrides for tool-call"
    (let [resources   {:sandbox           sample-sandbox
                       :sandbox-allows-fn (fn [sb name _]
                                            (if (contains? (:blocked-tools sb) name)
                                              {:allowed? false :reason "blocked"}
                                              {:allowed? true}))
                       :execute-tool-fn   (constantly "overridden result")
                       :timeout-ms        5000}
          parent-data {:drone-id "drone-override" :turn 0}
          override-tc {:name "read_file" :arguments {:path "/x.clj"}}
          result      (te/run-sub-fsm resources parent-data
                                      {:tool-call override-tc})]
      (is (= "overridden result" (:observation result)))
      (is (false? (:denied? result))))))

;; =============================================================================
;; 8. Graceful Degradation
;; =============================================================================

(deftest test-graceful-nil-tool-call
  (testing "nil tool call produces error, does not crash"
    (let [resources (mock-resources)
          data      {:drone-id "drone-nil" :turn 0}
          result    (te/run-tool-execution data resources)]
      (is (true? (:denied? result)))
      (is (some? (:error result))))))

(deftest test-graceful-no-executor
  (testing "missing execute-tool-fn produces error observation"
    (let [resources (mock-resources {:no-executor? true})
          data      (base-data)
          result    (te/run-tool-execution data resources)]
      (is (= "No tool executor provided" (:error result)))
      (is (= "(no result)" (:observation result))))))

(deftest test-graceful-no-sandbox
  (testing "missing sandbox allows all tools (permissive mode)"
    (let [resources (mock-resources {:no-sandbox? true
                                     :execute-result "allowed"})
          ;; Even a normally-blocked tool should be allowed without sandbox
          data      (base-data sample-blocked-tool-call)
          result    (te/run-tool-execution data resources)]
      (is (false? (:denied? result)))
      (is (= "allowed" (:observation result))))))

(deftest test-graceful-record-fn-exception
  (testing "record-observation-fn exception does not crash FSM"
    (let [resources (assoc (mock-resources {:execute-result "ok"})
                           :record-observation-fn
                           (fn [_] (throw (Exception. "KG down"))))
          data      (base-data)
          result    (te/run-tool-execution data resources)]
      ;; Should succeed despite record-fn failure
      (is (= "ok" (:observation result)))
      (is (false? (:denied? result))))))

(deftest test-graceful-audit-fn-exception
  (testing "audit-fn exception does not crash FSM"
    (let [resources (assoc (mock-resources {:execute-result "ok"})
                           :audit-fn
                           (fn [_ _ _] (throw (Exception. "audit broken"))))
          data      (base-data)
          result    (te/run-tool-execution data resources)]
      ;; Should succeed despite audit-fn failure
      (is (= "ok" (:observation result)))
      (is (false? (:denied? result))))))
