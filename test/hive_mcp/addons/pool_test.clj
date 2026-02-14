(ns hive-mcp.addons.pool-test
  "Tests for connection pool manager.

   Covers: pool creation, borrow/return, with-connection macro,
   eviction with replacement, drain, PooledBridge integration,
   concurrent access, timeout behavior, and predicates."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.pool :as pool]
            [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.addons.protocol :as proto]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn- noop-factory
  "Factory that creates noop bridges with unique IDs."
  []
  (bridge/->noop-bridge (keyword (gensym "noop-"))))

(defn- connected-factory
  "Factory that creates a mock bridge reporting connected?=true."
  []
  (let [state (atom {:connected true})]
    (reify
      proto/IAddon
      (addon-id [_] (keyword (gensym "mock-")))
      (addon-type [_] :mcp-bridge)
      (capabilities [_] #{:mcp-bridge})
      (initialize! [_ _] {:success? true :errors []})
      (shutdown! [_] (reset! state nil) {:success? true :errors []})
      (tools [_] [])
      (schema-extensions [_] {})
      (health [_] {:status (if (:connected @state) :ok :down)
                   :details {}})

      bridge/IMcpBridge
      (transport-type [_] :stdio)
      (start-bridge! [_ _] {:success? true :errors []})
      (stop-bridge! [_] (reset! state nil) {:success? true :errors []})
      (bridge-status [_]
        {:connected? (boolean (:connected @state))
         :transport :stdio
         :uptime-ms 0
         :remote-tool-count 0})
      (call-tool [_ tool-name params]
        {:content [{:type "text" :text (str "mock:" tool-name ":" params)}]})
      (list-remote-tools [_]
        [{:name "mock_tool"
          :description "A mock tool"
          :inputSchema {:type "object" :properties {}}}]))))

(defn- killable-factory
  "Factory that creates bridges that can be 'killed' (set disconnected)."
  []
  (let [alive (atom true)]
    (reify
      proto/IAddon
      (addon-id [_] (keyword (gensym "killable-")))
      (addon-type [_] :mcp-bridge)
      (capabilities [_] #{:mcp-bridge})
      (initialize! [_ _] {:success? true :errors []})
      (shutdown! [_] (reset! alive false) {:success? true :errors []})
      (tools [_] [])
      (schema-extensions [_] {})
      (health [_] {:status (if @alive :ok :down) :details {}})

      bridge/IMcpBridge
      (transport-type [_] :stdio)
      (start-bridge! [_ _] {:success? true :errors []})
      (stop-bridge! [_] (reset! alive false) {:success? true :errors []})
      (bridge-status [_]
        {:connected? @alive
         :transport :stdio
         :uptime-ms 0
         :remote-tool-count 0})
      (call-tool [_ tool-name _params]
        (if @alive
          {:content [{:type "text" :text (str "alive:" tool-name)}]}
          {:isError true :content [{:type "text" :text "dead"}]}))
      (list-remote-tools [_] []))))

;; =============================================================================
;; Pool Creation
;; =============================================================================

(deftest pool-creation-test
  (testing "Creates pool with N connections"
    (let [pool (pool/create-pool! noop-factory 3
                                  {:health-check-on-borrow false})]
      (is (pool/pool? pool))
      (let [status (pool/pool-status pool)]
        (is (= 3 (:pool-size status)))
        (is (= 3 (:available status)))
        (is (= 3 (:total status)))
        (is (= 0 (:borrowed status)))
        (is (false? (:drained? status))))
      (pool/drain! pool)))

  (testing "Creates pool with connected factory"
    (let [pool (pool/create-pool! connected-factory 2 {})]
      (let [status (pool/pool-status pool)]
        (is (= 2 (:pool-size status)))
        (is (= 2 (:available status))))
      (pool/drain! pool)))

  (testing "Throws when factory fails completely"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"no connections could be created"
         (pool/create-pool! (fn [] (throw (Exception. "fail"))) 2 {})))))

;; =============================================================================
;; Borrow / Return
;; =============================================================================

(deftest borrow-return-test
  (let [pool (pool/create-pool! noop-factory 3
                                {:health-check-on-borrow false})]
    (testing "Borrow returns a PooledConnection"
      (let [conn (pool/borrow pool)]
        (is (some? conn))
        (is (instance? hive_mcp.addons.pool.PooledConnection conn))
        (is (bridge/bridge? (:bridge conn)))
        (pool/return! pool conn)))

    (testing "Borrow decrements available, return restores"
      (let [conn (pool/borrow pool)]
        (is (= 2 (:available (pool/pool-status pool))))
        (pool/return! pool conn)
        (is (= 3 (:available (pool/pool-status pool))))))

    (testing "Stats track borrows and returns"
      (let [stats (:stats (pool/pool-status pool))]
        (is (pos? (:borrows stats)))
        (is (pos? (:returns stats)))))

    (pool/drain! pool)))

(deftest borrow-timeout-test
  (let [pool (pool/create-pool! noop-factory 1
                                {:health-check-on-borrow false
                                 :borrow-timeout-ms 100})]
    (testing "Times out when no connections available"
      (let [c1 (pool/borrow pool)]
        ;; Pool exhausted, next borrow should timeout
        (let [c2 (pool/borrow pool 100)]
          (is (nil? c2)))
        (pool/return! pool c1)))

    (testing "Stats track timeouts"
      (is (pos? (get-in (pool/pool-status pool) [:stats :timeouts]))))

    (pool/drain! pool)))

(deftest borrow-from-drained-pool-test
  (let [pool (pool/create-pool! noop-factory 2
                                {:health-check-on-borrow false})]
    (pool/drain! pool)
    (testing "Throws on borrow from drained pool"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"drained"
           (pool/borrow pool))))))

;; =============================================================================
;; with-connection Macro
;; =============================================================================

(deftest with-connection-test
  (let [pool (pool/create-pool! connected-factory 2 {})]
    (testing "Borrows, executes body, returns connection"
      (let [result (pool/with-connection [conn pool]
                     (bridge/call-tool conn "test" {"k" "v"}))]
        (is (map? result))
        (is (= 2 (:available (pool/pool-status pool))))))

    (testing "Returns connection even on exception"
      (try
        (pool/with-connection [_conn pool]
          (throw (Exception. "boom")))
        (catch Exception _))
      (is (= 2 (:available (pool/pool-status pool)))))

    (pool/drain! pool)))

(deftest with-connection-timeout-test
  (let [pool (pool/create-pool! noop-factory 1
                                {:health-check-on-borrow false
                                 :borrow-timeout-ms 50})]
    (testing "Throws when borrow times out inside with-connection"
      ;; Exhaust pool first
      (let [c1 (pool/borrow pool)]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"timed out"
             (pool/with-connection [_conn pool]
               :should-not-reach)))
        (pool/return! pool c1)))
    (pool/drain! pool)))

;; =============================================================================
;; Eviction
;; =============================================================================

(deftest evict-test
  (let [pool (pool/create-pool! noop-factory 3
                                {:health-check-on-borrow false
                                 :replace-on-evict true})]
    (testing "Evict removes connection and replaces"
      (let [conn (pool/borrow pool)
            result (pool/evict! pool conn)]
        (is (:evicted result))
        (is (:replaced result))
        ;; Pool should still have 3 total (2 original + 1 replacement)
        (let [status (pool/pool-status pool)]
          (is (= 3 (:total status))))))

    (testing "Stats track evictions"
      (is (pos? (get-in (pool/pool-status pool) [:stats :evictions]))))

    (pool/drain! pool)))

(deftest evict-without-replace-test
  (let [pool (pool/create-pool! noop-factory 2
                                {:health-check-on-borrow false
                                 :replace-on-evict false})]
    (testing "Evict without replacement shrinks pool"
      (let [conn (pool/borrow pool)
            result (pool/evict! pool conn)]
        (is (:evicted result))
        (is (false? (:replaced result)))
        (is (= 1 (:total (pool/pool-status pool))))))

    (pool/drain! pool)))

;; =============================================================================
;; Health Check on Borrow
;; =============================================================================

(deftest health-check-on-borrow-test
  (let [bridges (atom [])
        factory (fn []
                  (let [b (killable-factory)]
                    (swap! bridges conj b)
                    b))
        pool (pool/create-pool! factory 2
                                {:health-check-on-borrow true
                                 :replace-on-evict true})]
    (testing "Healthy connections returned normally"
      (let [conn (pool/borrow pool)]
        (is (some? conn))
        (pool/return! pool conn)))

    (testing "Dead connections evicted on borrow, replacement served"
      ;; Kill ALL existing bridges so any borrow hits a dead one.
      (doseq [b @bridges]
        (bridge/stop-bridge! b))
      ;; Borrow should evict dead + spawn replacement via factory
      (let [conn (pool/borrow pool)]
        (is (some? conn) "Replacement connection served after eviction")
        (is (pos? (get-in (pool/pool-status pool) [:stats :evictions]))
            "At least one eviction recorded")
        (pool/return! pool conn)))

    (pool/drain! pool)))

;; =============================================================================
;; Drain
;; =============================================================================

(deftest drain-test
  (let [pool (pool/create-pool! noop-factory 3
                                {:health-check-on-borrow false})]
    (testing "Drain stops all connections"
      (let [result (pool/drain! pool)]
        (is (:drained result))
        (is (= 3 (:stopped result)))
        (is (empty? (:errors result)))))

    (testing "After drain: pool empty and marked"
      (let [status (pool/pool-status pool)]
        (is (true? (:drained? status)))
        (is (= 0 (:available status)))
        (is (= 0 (:total status)))))

    (testing "Drain is idempotent"
      (let [result (pool/drain! pool)]
        (is (:drained result))
        (is (= 0 (:stopped result)))))))

;; =============================================================================
;; Concurrent Access
;; =============================================================================

(deftest concurrent-borrow-test
  (let [pool (pool/create-pool! connected-factory 3
                                {:borrow-timeout-ms 5000})]
    (testing "N concurrent tasks through M-slot pool"
      (let [counter (atom 0)
            futures (doall
                     (for [_ (range 10)]
                       (future
                         (pool/with-connection [conn pool]
                           (swap! counter inc)
                           (Thread/sleep 20)
                           (bridge/call-tool conn "test" {})))))]
        (doseq [f futures] @f)
        (is (= 10 @counter))
        (is (= 3 (:available (pool/pool-status pool))))))

    (pool/drain! pool)))

(deftest concurrent-no-double-checkout-test
  (let [pool      (pool/create-pool! connected-factory 2
                                     {:borrow-timeout-ms 5000})
        max-concurrent (atom 0)
        current        (atom 0)]
    (testing "Never exceeds pool-size concurrent checkouts"
      (let [futures (doall
                     (for [_ (range 20)]
                       (future
                         (pool/with-connection [_conn pool]
                           (let [c (swap! current inc)]
                             (swap! max-concurrent max c))
                           (Thread/sleep 10)
                           (swap! current dec)))))]
        (doseq [f futures] @f)
        (is (<= @max-concurrent 2)
            (str "Max concurrent was " @max-concurrent " but pool-size is 2"))))

    (pool/drain! pool)))

;; =============================================================================
;; PooledBridge
;; =============================================================================

(deftest pooled-bridge-protocols-test
  (let [pb (pool/->pooled-bridge
            :bridge/test
            connected-factory
            {:pool-size 2 :transport :stdio})]
    (testing "Satisfies IAddon"
      (is (satisfies? proto/IAddon pb))
      (is (= :bridge/test (proto/addon-id pb)))
      (is (= :mcp-bridge (proto/addon-type pb))))

    (testing "Satisfies IMcpBridge"
      (is (satisfies? bridge/IMcpBridge pb))
      (is (bridge/bridge? pb))
      (is (bridge/bridge-addon? pb))
      (is (= :stdio (bridge/transport-type pb))))

    (testing "Predicates"
      (is (pool/pooled-bridge? pb))
      (is (not (pool/pool? pb))))

    (proto/shutdown! pb)))

(deftest pooled-bridge-call-tool-test
  (let [pb (pool/->pooled-bridge
            :bridge/test
            connected-factory
            {:pool-size 2 :transport :stdio})]
    (testing "call-tool routes through pool"
      (let [result (bridge/call-tool pb "test_tool" {"x" "1"})]
        (is (map? result))
        (is (not (:isError result)))))

    (testing "bridge-status includes pool metrics"
      (let [status (bridge/bridge-status pb)]
        (is (true? (:connected? status)))
        (is (= :stdio (:transport status)))
        (is (map? (:pool status)))
        (is (= 2 (get-in status [:pool :pool-size])))))

    (testing "list-remote-tools routes through pool"
      (let [tools (bridge/list-remote-tools pb)]
        (is (sequential? tools))
        (is (= "mock_tool" (:name (first tools))))))

    (proto/shutdown! pb)))

(deftest pooled-bridge-shutdown-test
  (let [pb (pool/->pooled-bridge
            :bridge/test
            connected-factory
            {:pool-size 2 :transport :stdio})]
    (testing "Shutdown drains pool"
      (proto/shutdown! pb)
      (let [status (bridge/bridge-status pb)]
        (is (false? (:connected? status)))
        (is (true? (get-in status [:pool :drained?])))))

    (testing "Call after shutdown throws"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"drained"
           (bridge/call-tool pb "test" {}))))))

;; =============================================================================
;; Pool Status (Detailed)
;; =============================================================================

(deftest pool-status-detail-test
  (let [pool (pool/create-pool! connected-factory 2 {})]
    (testing "Detailed status includes per-connection info"
      (let [status (pool/pool-status pool true)]
        (is (vector? (:connections status)))
        (is (= 2 (count (:connections status))))
        (doseq [c (:connections status)]
          (is (number? (:created-at c)))
          (is (number? (:last-used-at c)))
          (is (number? (:idle-ms c)))
          (is (boolean? (:healthy? c))))))
    (pool/drain! pool)))

;; =============================================================================
;; Predicates
;; =============================================================================

(deftest predicates-test
  (let [pool (pool/create-pool! noop-factory 1
                                {:health-check-on-borrow false})]
    (testing "pool? predicate"
      (is (pool/pool? pool))
      (is (not (pool/pool? {})))
      (is (not (pool/pool? nil))))

    (testing "pooled-bridge? predicate"
      (is (not (pool/pooled-bridge? pool)))
      (is (not (pool/pooled-bridge? {}))))

    (pool/drain! pool)))
