(ns hive-mcp.addons.pool-e2e-test
  "E2E integration tests for the MCP bridge connection pool.

   Proves the full stack: pool creation → 5 concurrent lings calling tools
   simultaneously via PooledBridge → connection reuse → saturation → drain.

   Uses echo-mcp-server.py (instant) and slow-echo-mcp-server.py (500ms delay)
   as external MCP servers spawned via StdioBridge.

   Run with:
     clojure -M:test -n hive-mcp.addons.pool-e2e-test
     clojure -M:test-integration"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.addons.protocol :as proto]
            [hive-mcp.addons.core :as addon]
            [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.addons.stdio-bridge :as stdio]
            [hive-mcp.addons.pool :as pool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def echo-server-command
  "Command to launch the instant echo MCP server."
  ["python3" "scripts/echo-mcp-server.py"])

(def slow-echo-server-command
  "Command to launch the slow (500ms delay) echo MCP server."
  ["python3" "scripts/slow-echo-mcp-server.py"])

;; =============================================================================
;; Factory Functions
;; =============================================================================

(defn echo-bridge-factory
  "Create and start a StdioBridge connected to echo-mcp-server.py.
   Each call spawns a fresh subprocess — used as pool factory-fn."
  []
  (let [b (stdio/->stdio-bridge (keyword (str "bridge/echo-" (System/nanoTime))))]
    (let [result (bridge/start-bridge! b {:command echo-server-command})]
      (when-not (:success? result)
        (throw (ex-info "Failed to start echo bridge"
                        {:errors (:errors result)}))))
    b))

(defn slow-echo-bridge-factory
  "Create and start a StdioBridge connected to slow-echo-mcp-server.py."
  []
  (let [b (stdio/->stdio-bridge (keyword (str "bridge/slow-" (System/nanoTime))))]
    (let [result (bridge/start-bridge! b {:command slow-echo-server-command})]
      (when-not (:success? result)
        (throw (ex-info "Failed to start slow echo bridge"
                        {:errors (:errors result)}))))
    b))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn reset-fixture
  "Reset addon + bridge registries before/after each test."
  [f]
  (addon/reset-registry!)
  (f)
  (doseq [{:keys [name]} (bridge/list-bridges)]
    (bridge/unregister-bridge! name))
  (addon/reset-registry!))

(use-fixtures :each reset-fixture)

;; =============================================================================
;; Section 1: Pool Creation & Status
;; =============================================================================

(deftest ^:integration test-pool-creation-and-status
  (testing "Create pooled bridge with 5 connections, verify status"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 5
               :transport :stdio
               :health-check-on-borrow false})]
      (try
        (let [status (bridge/bridge-status pb)]
          (is (true? (:connected? status))
              "pooled bridge should report connected")
          (is (= :stdio (:transport status)))

          (let [ps (:pool status)]
            (is (= 5 (:pool-size ps))
                "pool-size should be 5")
            (is (= 5 (:available ps))
                "all 5 connections should be available")
            (is (= 5 (:total ps))
                "total should be 5")
            (is (= 0 (:borrowed ps))
                "none should be borrowed")
            (is (false? (:drained? ps))
                "pool should not be drained")))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 2: Single Tool Call via Pool
;; =============================================================================

(deftest ^:integration test-single-tool-call-via-pool
  (testing "Single echo call through pooled bridge"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 5
               :transport :stdio
               :health-check-on-borrow false})]
      (try
        (let [result (bridge/call-tool pb "echo" {"message" "hello pool"})]
          (is (not (:isError result))
              "call should succeed")
          (is (= "hello pool" (-> result :content first :text))
              "echo should return input message"))

        ;; Verify stats: 1 borrow + 1 return
        (let [ps (:pool (bridge/bridge-status pb))]
          (is (= 1 (get-in ps [:stats :borrows]))
              "should have 1 borrow")
          (is (= 1 (get-in ps [:stats :returns]))
              "should have 1 return")
          (is (= 5 (:available ps))
              "all connections should be back in pool"))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 3: Proxy Tool Discovery
;; =============================================================================

(deftest ^:integration test-proxy-tool-discovery
  (testing "addon-tools discovers echo and add tools with namespaced names"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 2
               :transport :stdio
               :health-check-on-borrow false})]
      (try
        (let [tools (proto/tools pb)]
          (is (= 2 (count tools))
              "should discover 2 tools (echo + add)")
          (is (= #{"echo:echo" "echo:add"}
                 (set (map :name tools)))
              "tool names should be namespaced with bridge prefix")
          (is (every? fn? (map :handler tools))
              "each tool should have a handler function")
          (is (every? #(= "echo" (:bridge-source %)) tools)
              "each tool should have bridge-source 'echo'"))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 4: Single Proxy Handler Call
;; =============================================================================

(deftest ^:integration test-single-proxy-handler-call
  (testing "Proxy handler for echo works end-to-end"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 2
               :transport :stdio
               :health-check-on-borrow false})]
      (try
        (let [tools     (proto/tools pb)
              echo-tool (first (filter #(= "echo:echo" (:name %)) tools))
              add-tool  (first (filter #(= "echo:add" (:name %)) tools))]

          (testing "echo proxy"
            (let [result ((:handler echo-tool) {"message" "proxied!"})]
              (is (= "text" (:type result)))
              (is (= "proxied!" (:text result)))))

          (testing "add proxy"
            (let [result ((:handler add-tool) {"a" 100 "b" 200})]
              (is (= "text" (:type result)))
              (is (= "300" (:text result))))))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 5: 5 Concurrent Direct Tool Calls (Core Test)
;; =============================================================================

(deftest ^:integration test-5-concurrent-direct-tool-calls
  (testing "5 concurrent lings calling echo via pooled bridge simultaneously"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 5
               :transport :stdio
               :health-check-on-borrow false})]
      (try
        (let [n-lings 5
              ;; Each ling sends a unique message
              futures (mapv
                       (fn [i]
                         (future
                           (Thread/sleep (rand-int 50)) ;; jitter
                           (bridge/call-tool pb "echo"
                                             {"message" (str "ling-" i)})))
                       (range n-lings))
              ;; Collect results with timeout
              results (mapv #(deref % 10000 :timeout) futures)]

          (testing "no timeouts"
            (is (not-any? #{:timeout} results)
                "all 5 calls should complete within timeout"))

          (testing "all results correct"
            (doseq [i (range n-lings)]
              (let [r (nth results i)]
                (is (not (:isError r))
                    (str "ling-" i " call should succeed"))
                (is (= (str "ling-" i) (-> r :content first :text))
                    (str "ling-" i " should get its own message back")))))

          (testing "pool stats reflect 5 borrows and returns"
            (let [ps (:pool (bridge/bridge-status pb))]
              (is (= 5 (get-in ps [:stats :borrows]))
                  "should have 5 borrows")
              (is (= 5 (get-in ps [:stats :returns]))
                  "should have 5 returns")
              (is (= 0 (get-in ps [:stats :timeouts]))
                  "should have 0 timeouts")
              (is (= 5 (:available ps))
                  "all connections should be returned"))))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 6: 5 Concurrent Proxy Handler Calls
;; =============================================================================

(deftest ^:integration test-5-concurrent-proxy-handler-calls
  (testing "5 concurrent lings calling add via proxy handlers"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 5
               :transport :stdio
               :health-check-on-borrow false})]
      (try
        (let [tools    (proto/tools pb)
              add-tool (first (filter #(= "echo:add" (:name %)) tools))
              handler  (:handler add-tool)
              n-lings  5
              ;; Each ling computes a unique sum: i + (i * 10)
              futures  (mapv
                        (fn [i]
                          (future
                            (Thread/sleep (rand-int 50))
                            {:ling-id  i
                             :expected (str (+ i (* i 10)))
                             :result   (handler {"a" i "b" (* i 10)})}))
                        (range n-lings))
              results  (mapv #(deref % 10000 :timeout) futures)]

          (testing "no timeouts"
            (is (not-any? #{:timeout} results)
                "all proxy calls should complete"))

          (testing "all sums correct"
            (doseq [{:keys [ling-id expected result]} results]
              (is (= "text" (:type result))
                  (str "ling-" ling-id " should get text result"))
              (is (= expected (:text result))
                  (str "ling-" ling-id " expected " expected
                       " got " (:text result)))))

          (testing "pool fully returned"
            (let [ps (:pool (bridge/bridge-status pb))]
              (is (= 5 (:available ps))
                  "all 5 connections should be available again"))))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 7: Connection Reuse After Burst
;; =============================================================================

(deftest ^:integration test-connection-reuse-after-burst
  (testing "Connections are reusable across multiple concurrent bursts"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 5
               :transport :stdio
               :health-check-on-borrow false})]
      (try
        ;; Burst 1: 5 concurrent calls
        (let [futures-1 (mapv (fn [i]
                                (future
                                  (bridge/call-tool pb "echo"
                                                    {"message" (str "burst1-" i)})))
                              (range 5))
              results-1 (mapv #(deref % 10000 :timeout) futures-1)]
          (is (every? #(not= :timeout %) results-1)
              "burst 1 should complete"))

        ;; Verify pool recovered
        (is (= 5 (:available (:pool (bridge/bridge-status pb))))
            "all connections back after burst 1")

        ;; Burst 2: 3 concurrent calls (reusing same connections)
        (let [futures-2 (mapv (fn [i]
                                (future
                                  (bridge/call-tool pb "add"
                                                    {"a" i "b" 100})))
                              (range 3))
              results-2 (mapv #(deref % 10000 :timeout) futures-2)]
          (is (every? #(not= :timeout %) results-2)
              "burst 2 should complete with reused connections")
          ;; Verify correct results
          (doseq [i (range 3)]
            (is (= (str (+ i 100)) (-> (nth results-2 i) :content first :text))
                (str "burst 2 result " i " should be " (+ i 100)))))

        ;; Cumulative stats: 5 + 3 = 8 borrows
        (let [ps (:pool (bridge/bridge-status pb))]
          (is (= 8 (get-in ps [:stats :borrows]))
              "cumulative borrows should be 8 (5 + 3)")
          (is (= 8 (get-in ps [:stats :returns]))
              "cumulative returns should be 8")
          (is (= 5 (:available ps))
              "all connections available after both bursts"))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 8: Pool Saturation (Slow Server)
;; =============================================================================

(deftest ^:integration test-pool-saturation-timeout
  (testing "When all pool slots are busy, extra borrows timeout"
    (let [pb (pool/->pooled-bridge
              :bridge/slow
              slow-echo-bridge-factory
              {:pool-size 2
               :transport :stdio
               :borrow-timeout-ms 200
               :health-check-on-borrow false})]
      (try
        ;; Launch 3 concurrent calls against pool-size=2
        ;; 2 will get connections, 1 will timeout on borrow
        (let [results-atom (atom [])
              futures (mapv
                       (fn [i]
                         (future
                           (try
                             (let [r (bridge/call-tool pb "echo"
                                                       {"message" (str "sat-" i)})]
                               (swap! results-atom conj {:id i :result r :error nil}))
                             (catch Exception e
                               (swap! results-atom conj {:id i :result nil
                                                         :error (.getMessage e)})))))
                       (range 3))]

          ;; Wait for all futures
          (doseq [f futures]
            (deref f 15000 :timeout))

          ;; Give pool stats a moment to settle
          (Thread/sleep 100)

          (let [results  @results-atom
                successes (filterv #(some? (:result %)) results)
                errors    (filterv #(some? (:error %)) results)]

            (testing "2 calls succeed, 1 fails"
              (is (= 2 (count successes))
                  (str "expected 2 successes, got " (count successes)))
              (is (= 1 (count errors))
                  (str "expected 1 error (timeout), got " (count errors))))

            (testing "successful results are correct"
              (doseq [{:keys [result]} successes]
                (is (not (:isError result))
                    "successful calls should not be errors")
                (is (str/starts-with? (-> result :content first :text) "sat-")
                    "result should start with sat-")))

            (testing "error is a timeout/borrow failure"
              (let [err-msg (:error (first errors))]
                (is (or (str/includes? err-msg "timed out")
                        (str/includes? err-msg "no available"))
                    (str "error should be timeout-related, got: " err-msg))))))

        ;; Wait for slow calls to finish returning
        (Thread/sleep 1000)

        (testing "pool stats reflect timeouts"
          (let [ps (:pool (bridge/bridge-status pb))]
            (is (pos? (get-in ps [:stats :timeouts]))
                "should have at least 1 timeout")))
        (finally
          (bridge/stop-bridge! pb))))))

;; =============================================================================
;; Section 9: Drain & Cleanup
;; =============================================================================

(deftest ^:integration test-drain-stops-all-connections
  (testing "drain! stops all processes and prevents further borrows"
    (let [pb (pool/->pooled-bridge
              :bridge/echo
              echo-bridge-factory
              {:pool-size 5
               :transport :stdio
               :health-check-on-borrow false})]

      ;; Verify pool is alive first
      (let [result (bridge/call-tool pb "echo" {"message" "pre-drain"})]
        (is (= "pre-drain" (-> result :content first :text))
            "should work before drain"))

      ;; Drain
      (let [drain-result (pool/drain! (:pool pb))]
        (is (true? (:drained drain-result))
            "drain should report success")
        (is (= 5 (:stopped drain-result))
            "should stop all 5 connections"))

      ;; Post-drain status
      (let [ps (:pool (bridge/bridge-status pb))]
        (is (true? (:drained? ps))
            "pool should be marked drained")
        (is (= 0 (:available ps))
            "no connections available")
        (is (= 0 (:total ps))
            "no connections tracked"))

      ;; Post-drain call should fail
      (is (thrown? clojure.lang.ExceptionInfo
                   (bridge/call-tool pb "echo" {"message" "post-drain"}))
          "call-tool after drain should throw"))))

(comment
  ;; Run all pool E2E tests
  (clojure.test/run-tests 'hive-mcp.addons.pool-e2e-test))
