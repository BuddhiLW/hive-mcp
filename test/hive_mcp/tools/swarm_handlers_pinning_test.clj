(ns hive-mcp.tools.swarm-handlers-pinning-test
  "Pinning tests for Swarm handler return formats.

   Tests verify the MCP response format {:type \"text\" :text \"...\"} is
   consistently returned from Swarm handlers. Uses with-redefs to mock
   emacsclient calls and coordinator functions.

   Covers:
   - handle-swarm-spawn
   - handle-swarm-dispatch
   - handle-swarm-status
   - handle-swarm-collect
   - handle-swarm-kill"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.swarm.coordinator :as coord]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn mock-elisp-timeout-success
  "Creates a mock eval-elisp-with-timeout that returns success."
  [result]
  (fn [_elisp _timeout]
    {:success true :result result :duration-ms 10 :timed-out false}))

(defn mock-elisp-timeout-failure
  "Creates a mock eval-elisp-with-timeout that returns failure."
  [error]
  (fn [_elisp _timeout]
    {:success false :error error :duration-ms 10 :timed-out false}))

(defn mock-elisp-timeout-timed-out
  "Creates a mock eval-elisp-with-timeout that returns timeout."
  []
  (fn [_elisp _timeout]
    {:success false :timed-out true :duration-ms 10000}))

(defn mock-addon-available
  "Mock that makes swarm-addon-available? return true."
  []
  (fn [_elisp _timeout]
    {:success true :result "t" :duration-ms 5 :timed-out false}))

(defn mock-addon-unavailable
  "Mock that makes swarm-addon-available? return false."
  []
  (fn [_elisp _timeout]
    {:success true :result "nil" :duration-ms 5 :timed-out false}))

(defmacro with-addon-available
  "Execute body with swarm addon available and custom response mock."
  [response-mock & body]
  `(let [call-count# (atom 0)]
     (with-redefs [ec/eval-elisp-with-timeout
                   (fn [elisp# timeout#]
                     (swap! call-count# inc)
                     ;; First call is addon check, rest use response mock
                     (if (= @call-count# 1)
                       {:success true :result "t" :duration-ms 5 :timed-out false}
                       (~response-mock elisp# timeout#)))]
       ~@body)))

(defmacro with-addon-unavailable
  "Execute body with swarm addon unavailable."
  [& body]
  `(with-redefs [ec/eval-elisp-with-timeout (mock-addon-unavailable)]
     ~@body))

;; =============================================================================
;; handle-swarm-spawn Tests
;; =============================================================================

(deftest handle-swarm-spawn-success-test
  (testing "Returns proper MCP response format on success"
    (let [spawn-json "{\"slave_id\":\"test-slave\",\"status\":\"spawned\",\"buffer\":\"*claude-test-slave*\"}"]
      (with-addon-available (mock-elisp-timeout-success spawn-json)
        (let [result (swarm/handle-swarm-spawn {:name "test-slave"})]
          (is (= "text" (:type result)))
          (is (string? (:text result)))
          (is (nil? (:isError result)))
          ;; Verify JSON content
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "test-slave" (:slave_id parsed)))
            (is (= "spawned" (:status parsed)))))))))

(deftest handle-swarm-spawn-with-presets-test
  (testing "Returns proper format when spawning with presets"
    (let [spawn-json "{\"slave_id\":\"tdd-slave\",\"status\":\"spawned\",\"presets\":[\"tdd\",\"clarity\"]}"]
      (with-addon-available (mock-elisp-timeout-success spawn-json)
        (let [result (swarm/handle-swarm-spawn {:name "tdd-slave"
                                                :presets ["tdd" "clarity"]})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= ["tdd" "clarity"] (:presets parsed)))))))))

(deftest handle-swarm-spawn-with-cwd-test
  (testing "Returns proper format when spawning with working directory"
    (let [spawn-json "{\"slave_id\":\"slave\",\"status\":\"spawned\",\"cwd\":\"/home/user/project\"}"]
      (with-addon-available (mock-elisp-timeout-success spawn-json)
        (let [result (swarm/handle-swarm-spawn {:name "slave"
                                                :cwd "/home/user/project"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result))))))))

(deftest handle-swarm-spawn-error-test
  (testing "Returns error format when emacsclient fails"
    (with-addon-available (mock-elisp-timeout-failure "Buffer creation failed")
      (let [result (swarm/handle-swarm-spawn {:name "test-slave"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Error:"))
        (is (str/includes? (:text result) "Buffer creation failed"))))))

(deftest handle-swarm-spawn-timeout-test
  (testing "Returns timeout error when operation times out"
    (with-addon-available (mock-elisp-timeout-timed-out)
      (let [result (swarm/handle-swarm-spawn {:name "test-slave"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "timeout" (:status parsed))))))))

(deftest handle-swarm-spawn-addon-not-loaded-test
  (testing "Returns error when hive-mcp-swarm addon not loaded"
    (with-addon-unavailable
      (let [result (swarm/handle-swarm-spawn {:name "test-slave"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "not loaded"))))))

;; =============================================================================
;; handle-swarm-dispatch Tests
;; =============================================================================

(deftest handle-swarm-dispatch-success-test
  (testing "Returns proper MCP response format on successful dispatch"
    (let [dispatch-json "{\"task_id\":\"task-001\",\"status\":\"dispatched\",\"slave_id\":\"slave-1\"}"]
      (with-redefs [swarm/swarm-addon-available? (constantly true)
                    coord/dispatch-or-queue! (constantly {:action :dispatch :files []})
                    ec/eval-elisp-with-timeout (mock-elisp-timeout-success dispatch-json)]
        (let [result (swarm/handle-swarm-dispatch {:slave_id "slave-1"
                                                   :prompt "Run tests"})]
          (is (= "text" (:type result)))
          (is (string? (:text result)))
          (is (nil? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "task-001" (:task_id parsed)))
            (is (= "dispatched" (:status parsed)))))))))

(deftest handle-swarm-dispatch-queued-test
  (testing "Returns queued status when file conflicts exist"
    (with-redefs [swarm/swarm-addon-available? (constantly true)
                  coord/dispatch-or-queue! (constantly {:action :queued
                                                        :task-id "queued-task-001"
                                                        :position 1
                                                        :conflicts ["file.clj"]})]
      (let [result (swarm/handle-swarm-dispatch {:slave_id "slave-1"
                                                 :prompt "Edit file.clj"})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "queued" (:status parsed)))
          (is (= "queued-task-001" (:task_id parsed)))
          (is (= ["file.clj"] (:conflicts parsed))))))))

(deftest handle-swarm-dispatch-blocked-test
  (testing "Returns blocked status when circular dependency detected"
    (with-redefs [swarm/swarm-addon-available? (constantly true)
                  coord/dispatch-or-queue! (constantly {:action :blocked
                                                        :would-deadlock ["slave-1" "slave-2"]})]
      (let [result (swarm/handle-swarm-dispatch {:slave_id "slave-1"
                                                 :prompt "Circular task"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (str/includes? (:error parsed) "circular dependency")))))))

(deftest handle-swarm-dispatch-timeout-test
  (testing "Returns timeout error when dispatch times out"
    (with-redefs [swarm/swarm-addon-available? (constantly true)
                  coord/dispatch-or-queue! (constantly {:action :dispatch :files []})
                  ec/eval-elisp-with-timeout (mock-elisp-timeout-timed-out)]
      (let [result (swarm/handle-swarm-dispatch {:slave_id "slave-1"
                                                 :prompt "Long task"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "timeout" (:status parsed))))))))

(deftest handle-swarm-dispatch-error-test
  (testing "Returns error format when dispatch fails"
    (with-redefs [swarm/swarm-addon-available? (constantly true)
                  coord/dispatch-or-queue! (constantly {:action :dispatch :files []})
                  ec/eval-elisp-with-timeout (mock-elisp-timeout-failure "Slave not found")]
      (let [result (swarm/handle-swarm-dispatch {:slave_id "nonexistent"
                                                 :prompt "Test"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Error:"))))))

(deftest handle-swarm-dispatch-addon-not-loaded-test
  (testing "Returns error when hive-mcp-swarm addon not loaded"
    (with-redefs [swarm/swarm-addon-available? (constantly false)]
      (let [result (swarm/handle-swarm-dispatch {:slave_id "slave-1"
                                                 :prompt "Test"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "not loaded"))))))

;; =============================================================================
;; handle-swarm-status Tests
;; =============================================================================

(deftest handle-swarm-status-success-test
  (testing "Returns proper MCP response format on success"
    (let [status-json "{\"slaves\":[{\"id\":\"slave-1\",\"status\":\"idle\"}],\"total\":1}"]
      (with-addon-available (mock-elisp-timeout-success status-json)
        (let [result (swarm/handle-swarm-status {})]
          (is (= "text" (:type result)))
          (is (string? (:text result)))
          (is (nil? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= 1 (:total parsed)))
            (is (= 1 (count (:slaves parsed))))))))))

(deftest handle-swarm-status-with-slave-id-test
  (testing "Returns status for specific slave when slave_id provided"
    (let [status-json "{\"id\":\"slave-1\",\"status\":\"busy\",\"current_task\":\"task-001\"}"]
      (with-addon-available (mock-elisp-timeout-success status-json)
        (let [result (swarm/handle-swarm-status {:slave_id "slave-1"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "slave-1" (:id parsed)))
            (is (= "busy" (:status parsed)))))))))

(deftest handle-swarm-status-empty-test
  (testing "Returns empty slaves list when no slaves exist"
    (let [status-json "{\"slaves\":[],\"total\":0}"]
      (with-addon-available (mock-elisp-timeout-success status-json)
        (let [result (swarm/handle-swarm-status nil)]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= 0 (:total parsed)))))))))

(deftest handle-swarm-status-error-test
  (testing "Returns error format when status check fails"
    (with-addon-available (mock-elisp-timeout-failure "Swarm manager not initialized")
      (let [result (swarm/handle-swarm-status {})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Error:"))))))

(deftest handle-swarm-status-timeout-test
  (testing "Returns timeout error when status check times out"
    (with-addon-available (mock-elisp-timeout-timed-out)
      (let [result (swarm/handle-swarm-status {})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "timeout" (:status parsed))))))))

(deftest handle-swarm-status-addon-not-loaded-test
  (testing "Returns error when hive-mcp-swarm addon not loaded"
    (with-addon-unavailable
      (let [result (swarm/handle-swarm-status {})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "not loaded"))))))

;; =============================================================================
;; handle-swarm-collect Tests
;; =============================================================================

(deftest handle-swarm-collect-completed-test
  (testing "Returns proper MCP response format when task completed"
    ;; Single JSON string - unwrap-emacs-string already handles emacsclient quoting
    (let [json-str "{\"task_id\":\"task-001\",\"status\":\"completed\",\"result\":\"Success\"}"]
      (with-addon-available (mock-elisp-timeout-success json-str)
        (with-redefs [swarm/check-event-journal (constantly nil)]
          (let [result (swarm/handle-swarm-collect {:task_id "task-001"})
                parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "text" (:type result)))
            (is (string? (:text result)))
            (is (nil? (:isError result)))
            ;; Verify actual content
            (is (= "completed" (:status parsed)))
            (is (= "task-001" (:task_id parsed)))))))))

(deftest handle-swarm-collect-from-journal-test
  (testing "Returns result from event journal (push-based)"
    (with-redefs [swarm/swarm-addon-available? (constantly true)
                  swarm/check-event-journal (constantly {:status "completed"
                                                         :result "Done via push"
                                                         :slave-id "slave-1"
                                                         :timestamp 1234567890})]
      (let [result (swarm/handle-swarm-collect {:task_id "task-001"})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "completed" (:status parsed)))
          (is (= "channel-push" (:via parsed))))))))

(deftest handle-swarm-collect-error-task-test
  (testing "Returns error status when task failed"
    ;; Single JSON string - unwrap-emacs-string already handles emacsclient quoting
    (let [json-str "{\"task_id\":\"task-001\",\"status\":\"error\",\"error\":\"Task crashed\"}"]
      (with-addon-available (mock-elisp-timeout-success json-str)
        (with-redefs [swarm/check-event-journal (constantly nil)]
          (let [result (swarm/handle-swarm-collect {:task_id "task-001"})
                parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "text" (:type result)))
            (is (string? (:text result)))
            ;; Error status in result - verify content
            (is (= "error" (:status parsed)))
            (is (= "Task crashed" (:error parsed)))))))))

(deftest handle-swarm-collect-timeout-test
  (testing "Returns timeout when collection times out"
    ;; Single JSON string - unwrap-emacs-string already handles emacsclient quoting
    (let [json-str "{\"task_id\":\"task-001\",\"status\":\"polling\"}"]
      (with-addon-available (mock-elisp-timeout-success json-str)
        (with-redefs [swarm/check-event-journal (constantly nil)]
          ;; Use very short timeout to trigger timeout
          (let [result (swarm/handle-swarm-collect {:task_id "task-001"
                                                    :timeout_ms 1})
                parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "text" (:type result)))
            (is (string? (:text result)))
            ;; Should return timeout status
            (is (= "timeout" (:status parsed)))))))))

(deftest handle-swarm-collect-elisp-timeout-test
  (testing "Returns error when elisp evaluation times out"
    (with-addon-available (mock-elisp-timeout-timed-out)
      (with-redefs [swarm/check-event-journal (constantly nil)]
        (let [result (swarm/handle-swarm-collect {:task_id "task-001"})]
          (is (= "text" (:type result)))
          (is (true? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "error" (:status parsed)))))))))

(deftest handle-swarm-collect-addon-not-loaded-test
  (testing "Returns error when hive-mcp-swarm addon not loaded"
    (with-addon-unavailable
      (let [result (swarm/handle-swarm-collect {:task_id "task-001"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "not loaded"))))))

;; =============================================================================
;; handle-swarm-kill Tests
;; =============================================================================

(deftest handle-swarm-kill-success-test
  (testing "Returns proper MCP response format on successful kill"
    (let [kill-json "{\"slave_id\":\"slave-1\",\"status\":\"killed\"}"]
      (with-addon-available (mock-elisp-timeout-success kill-json)
        (let [result (swarm/handle-swarm-kill {:slave_id "slave-1"})]
          (is (= "text" (:type result)))
          (is (string? (:text result)))
          (is (nil? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "slave-1" (:slave_id parsed)))
            (is (= "killed" (:status parsed)))))))))

(deftest handle-swarm-kill-all-test
  (testing "Returns proper format when killing all slaves"
    (let [kill-json "{\"status\":\"all_killed\",\"count\":3}"]
      (with-addon-available (mock-elisp-timeout-success kill-json)
        (let [result (swarm/handle-swarm-kill {:slave_id "all"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= "all_killed" (:status parsed)))
            (is (= 3 (:count parsed)))))))))

(deftest handle-swarm-kill-error-test
  (testing "Returns error format when kill fails"
    (with-addon-available (mock-elisp-timeout-failure "Slave not found")
      (let [result (swarm/handle-swarm-kill {:slave_id "nonexistent"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Error:"))
        (is (str/includes? (:text result) "Slave not found"))))))

(deftest handle-swarm-kill-timeout-test
  (testing "Returns timeout error when kill times out"
    (with-addon-available (mock-elisp-timeout-timed-out)
      (let [result (swarm/handle-swarm-kill {:slave_id "slave-1"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "timeout" (:status parsed))))))))

(deftest handle-swarm-kill-addon-not-loaded-test
  (testing "Returns error when hive-mcp-swarm addon not loaded"
    (with-addon-unavailable
      (let [result (swarm/handle-swarm-kill {:slave_id "slave-1"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "not loaded"))))))

;; =============================================================================
;; Elisp Generation Verification Tests
;; =============================================================================

(deftest elisp-spawn-generation-test
  (testing "Verifies correct elisp is generated for spawn"
    (let [captured-elisp (atom nil)]
      (with-redefs [swarm/swarm-addon-available? (constantly true)
                    ec/eval-elisp-with-timeout
                    (fn [elisp _timeout]
                      (reset! captured-elisp elisp)
                      {:success true :result "{}" :duration-ms 10 :timed-out false})]
        (swarm/handle-swarm-spawn {:name "test-slave"
                                   :presets ["tdd"]
                                   :cwd "/home/user"})
        (is (str/includes? @captured-elisp "hive-mcp-swarm-api-spawn"))
        (is (str/includes? @captured-elisp "test-slave"))
        (is (str/includes? @captured-elisp "tdd"))
        (is (str/includes? @captured-elisp "/home/user"))))))

(deftest elisp-status-generation-test
  (testing "Verifies correct elisp is generated for status"
    (let [captured-elisp (atom nil)]
      ;; Test without slave_id
      (with-redefs [swarm/swarm-addon-available? (constantly true)
                    ec/eval-elisp-with-timeout
                    (fn [elisp _timeout]
                      (reset! captured-elisp elisp)
                      {:success true :result "{}" :duration-ms 10 :timed-out false})]
        (swarm/handle-swarm-status {})
        (is (str/includes? @captured-elisp "hive-mcp-swarm-api-status")))

      ;; Test with slave_id
      (with-redefs [swarm/swarm-addon-available? (constantly true)
                    ec/eval-elisp-with-timeout
                    (fn [elisp _timeout]
                      (reset! captured-elisp elisp)
                      {:success true :result "{}" :duration-ms 10 :timed-out false})]
        (swarm/handle-swarm-status {:slave_id "slave-1"})
        (is (str/includes? @captured-elisp "hive-mcp-swarm-status"))
        (is (str/includes? @captured-elisp "slave-1"))))))

(deftest elisp-kill-generation-test
  (testing "Verifies correct elisp is generated for kill"
    (let [captured-elisp (atom nil)]
      ;; Test single slave kill
      (with-redefs [swarm/swarm-addon-available? (constantly true)
                    ec/eval-elisp-with-timeout
                    (fn [elisp _timeout]
                      (reset! captured-elisp elisp)
                      {:success true :result "{}" :duration-ms 10 :timed-out false})]
        (swarm/handle-swarm-kill {:slave_id "slave-1"})
        (is (str/includes? @captured-elisp "hive-mcp-swarm-api-kill"))
        (is (str/includes? @captured-elisp "slave-1")))

      ;; Test kill all slaves
      (with-redefs [swarm/swarm-addon-available? (constantly true)
                    ec/eval-elisp-with-timeout
                    (fn [elisp _timeout]
                      (reset! captured-elisp elisp)
                      {:success true :result "{}" :duration-ms 10 :timed-out false})]
        (swarm/handle-swarm-kill {:slave_id "all"})
        (is (str/includes? @captured-elisp "hive-mcp-swarm-api-kill-all"))))))

;; =============================================================================
;; Response Format Consistency Tests
;; =============================================================================

(deftest response-format-consistency-test
  (testing "All swarm handlers return consistent response format"
    (with-redefs [swarm/swarm-addon-available? (constantly true)
                  coord/dispatch-or-queue! (constantly {:action :dispatch :files []})
                  swarm/check-event-journal (constantly {:status "completed"
                                                         :result "ok"
                                                         :slave-id "s1"
                                                         :timestamp 0})
                  ec/eval-elisp-with-timeout (mock-elisp-timeout-success "{}")]
      ;; Test each handler returns :type "text"
      (doseq [handler-fn [#(swarm/handle-swarm-spawn {:name "s"})
                          #(swarm/handle-swarm-dispatch {:slave_id "s" :prompt "p"})
                          #(swarm/handle-swarm-status {})
                          #(swarm/handle-swarm-collect {:task_id "t"})
                          #(swarm/handle-swarm-kill {:slave_id "s"})]]
        (let [result (handler-fn)]
          (is (= "text" (:type result))
              "All handlers must return :type \"text\"")
          (is (string? (:text result))
              "All handlers must return :text as string"))))))

(deftest error-format-consistency-test
  (testing "All swarm handlers return consistent error format"
    (with-addon-unavailable
      ;; Test each handler returns proper error format when addon not loaded
      (doseq [[name handler-fn] [["swarm-spawn" #(swarm/handle-swarm-spawn {:name "s"})]
                                 ["swarm-status" #(swarm/handle-swarm-status {})]
                                 ["swarm-collect" #(swarm/handle-swarm-collect {:task_id "t"})]
                                 ["swarm-kill" #(swarm/handle-swarm-kill {:slave_id "s"})]]]
        (let [result (handler-fn)]
          (is (= "text" (:type result))
              (str name " must return :type \"text\" on error"))
          (is (true? (:isError result))
              (str name " must return :isError true on error"))
          (is (str/includes? (:text result) "not loaded")
              (str name " must indicate addon not loaded")))))))
