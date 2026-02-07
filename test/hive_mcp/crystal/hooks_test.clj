(ns hive-mcp.crystal.hooks-test
  "Tests for crystal/hooks.clj crystallize-session cross-pollination wiring (P2.7).

   Verifies:
   - xpoll-stats appears in return map for both no-content and content paths
   - Cross-pollination failure does NOT block crystallization
   - Correct args passed to lifecycle/run-cross-pollination-cycle!"
  (:require [clojure.test :refer [deftest testing is are]]
            [hive-mcp.crystal.hooks :as hooks]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.memory.lifecycle :as lifecycle]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.agent.context :as ctx]))

;; =============================================================================
;; Test helpers
;; =============================================================================

(def ^:private base-harvested
  "Minimal harvested data for testing crystallize-session."
  {:progress-notes []
   :completed-tasks []
   :git-commits []
   :directory "/tmp/test-project"
   :recalls {}
   :summary {:progress-count 0
             :task-count 0
             :commit-count 0
             :recall-count 0}})

(defmacro with-crystallize-mocks
  "Bind mocks for all crystallize-session dependencies.

   opts keys:
     :summary       - return value for crystal/summarize-session-progress (nil = no-content path)
     :promotion     - return value for kg-edges/promote-co-access-edges!
     :decay         - return value for kg-edges/decay-unverified-edges!
     :xpoll         - return value for lifecycle/run-cross-pollination-cycle!
     :xpoll-fn      - custom fn for xpoll (overrides :xpoll)
     :memory-decay   - return value for lifecycle/run-decay-cycle!
     :memory-decay-fn - custom fn for memory decay (overrides :memory-decay)
     :entry-id      - return value for chroma/index-memory-entry!
     :project-id    - return value for scope/get-current-project-id"
  [opts & body]
  `(let [opts# ~opts
         xpoll-calls# (atom [])
         memory-decay-calls# (atom [])]
     (with-redefs [crystal/summarize-session-progress
                   (fn [& _#] (:summary opts#))

                   crystal/session-id
                   (fn [] "test-session-123")

                   scope/get-current-project-id
                   (fn [_#] (get opts# :project-id "test-project"))

                   scope/inject-project-scope
                   (fn [tags# _pid#] tags#)

                   dur/calculate-expires
                   (fn [_#] "2026-02-13T00:00:00Z")

                   ctx/current-directory
                   (fn [] "/tmp/test-project")

                   chroma/index-memory-entry!
                   (fn [_#] (get opts# :entry-id "entry-test-001"))

                   chroma/content-hash
                   (fn [c#] (str (hash c#)))

                   kg-edges/promote-co-access-edges!
                   (fn [_#] (get opts# :promotion
                                 {:promoted 0 :skipped 0 :below 0 :evaluated 0}))

                   kg-edges/decay-unverified-edges!
                   (fn [_#] (get opts# :decay
                                 {:decayed 0 :pruned 0 :fresh 0 :evaluated 0}))

                   lifecycle/run-cross-pollination-cycle!
                   (if-let [custom-fn# (:xpoll-fn opts#)]
                     (fn [args#]
                       (swap! xpoll-calls# conj args#)
                       (custom-fn# args#))
                     (fn [args#]
                       (swap! xpoll-calls# conj args#)
                       (get opts# :xpoll
                            {:promoted 0 :candidates 0 :total-scanned 0})))

                   lifecycle/run-decay-cycle!
                   (if-let [custom-fn# (:memory-decay-fn opts#)]
                     (fn [args#]
                       (swap! memory-decay-calls# conj args#)
                       (custom-fn# args#))
                     (fn [args#]
                       (swap! memory-decay-calls# conj args#)
                       (get opts# :memory-decay
                            {:decayed 0 :expired 0 :total-scanned 0})))]
       (let [result# (do ~@body)]
         {:result result#
          :xpoll-calls @xpoll-calls#
          :memory-decay-calls @memory-decay-calls#}))))

;; =============================================================================
;; No-content path tests
;; =============================================================================

(deftest crystallize-session-no-content-includes-xpoll-stats
  (testing "No-content path includes :xpoll-stats in return map"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary nil
             :xpoll {:promoted 0 :candidates 0 :total-scanned 50}}
            (hooks/crystallize-session base-harvested))]
      (is (:skipped result) "Should be skipped (no-content path)")
      (is (contains? result :xpoll-stats) "Return map must include :xpoll-stats")
      (is (= 0 (:promoted (:xpoll-stats result))))
      (is (= 0 (:candidates (:xpoll-stats result))))
      (is (= 50 (:total-scanned (:xpoll-stats result)))))))

(deftest crystallize-session-no-content-xpoll-with-promotions
  (testing "No-content path reports xpoll promotions correctly"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary nil
             :xpoll {:promoted 3 :candidates 5 :total-scanned 100}}
            (hooks/crystallize-session base-harvested))]
      (is (:skipped result))
      (is (= 3 (:promoted (:xpoll-stats result))))
      (is (= 5 (:candidates (:xpoll-stats result))))
      (is (= 100 (:total-scanned (:xpoll-stats result)))))))

(deftest crystallize-session-no-content-xpoll-failure-non-blocking
  (testing "No-content path: xpoll failure does NOT block crystallization"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary nil
             :xpoll-fn (fn [_] (throw (Exception. "Chroma connection refused")))}
            (hooks/crystallize-session base-harvested))]
      (is (:skipped result) "Should still return skipped result")
      (is (contains? result :xpoll-stats) "Must still have :xpoll-stats key")
      (is (string? (:error (:xpoll-stats result))) "Should contain error message")
      (is (= 0 (:promoted (:xpoll-stats result))) "Promoted should be 0 on error"))))

(deftest crystallize-session-no-content-xpoll-receives-directory
  (testing "No-content path: xpoll receives correct directory arg"
    (let [{:keys [xpoll-calls]}
          (with-crystallize-mocks
            {:summary nil}
            (hooks/crystallize-session
             (assoc base-harvested :directory "/home/test/my-project")))]
      (is (= 1 (count xpoll-calls)) "xpoll should be called exactly once")
      (is (= "/home/test/my-project" (:directory (first xpoll-calls)))
          "Should pass directory from harvested data")
      (is (= 100 (:limit (first xpoll-calls)))
          "Should pass limit 100"))))

;; =============================================================================
;; Content path tests
;; =============================================================================

(deftest crystallize-session-content-includes-xpoll-stats
  (testing "Content path includes :xpoll-stats in return map"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary {:content "Session summary" :tags ["wrap"]}
             :entry-id "entry-abc-123"
             :xpoll {:promoted 2 :candidates 4 :total-scanned 80}}
            (hooks/crystallize-session
             (assoc base-harvested
                    :progress-notes [{:content "did stuff"}]
                    :git-commits ["abc1234 feat: something"])))]
      (is (not (:skipped result)) "Should NOT be skipped (content path)")
      (is (= "entry-abc-123" (:summary-id result)))
      (is (contains? result :xpoll-stats) "Return map must include :xpoll-stats")
      (is (= 2 (:promoted (:xpoll-stats result))))
      (is (= 4 (:candidates (:xpoll-stats result))))
      (is (= 80 (:total-scanned (:xpoll-stats result)))))))

(deftest crystallize-session-content-xpoll-failure-non-blocking
  (testing "Content path: xpoll failure does NOT block crystallization"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary {:content "Session summary" :tags ["wrap"]}
             :entry-id "entry-def-456"
             :xpoll-fn (fn [_] (throw (Exception. "Embedding model unavailable")))}
            (hooks/crystallize-session
             (assoc base-harvested
                    :progress-notes [{:content "work done"}])))]
      (is (= "entry-def-456" (:summary-id result))
          "Summary should still be created despite xpoll failure")
      (is (contains? result :xpoll-stats) "Must still have :xpoll-stats key")
      (is (string? (:error (:xpoll-stats result))))
      (is (= 0 (:promoted (:xpoll-stats result)))))))

(deftest crystallize-session-content-xpoll-receives-directory
  (testing "Content path: xpoll receives correct directory arg"
    (let [{:keys [xpoll-calls]}
          (with-crystallize-mocks
            {:summary {:content "Summary content" :tags ["wrap"]}
             :entry-id "entry-ghi-789"}
            (hooks/crystallize-session
             (assoc base-harvested
                    :directory "/home/user/other-project"
                    :progress-notes [{:content "progress"}])))]
      (is (= 1 (count xpoll-calls)) "xpoll should be called exactly once")
      (is (= "/home/user/other-project" (:directory (first xpoll-calls))))
      (is (= 100 (:limit (first xpoll-calls)))))))

;; =============================================================================
;; Cross-path consistency tests
;; =============================================================================

(deftest crystallize-session-xpoll-stats-keys-consistent
  (testing "Both paths return same xpoll-stats keys via select-keys"
    (let [{:keys [result] :as no-content}
          (with-crystallize-mocks
            {:summary nil
             :xpoll {:promoted 1 :candidates 2 :total-scanned 10
                     :entries [{:id "x" :promoted true}]}}
            (hooks/crystallize-session base-harvested))
          no-content-keys (set (keys (:xpoll-stats result)))

          {content-result :result}
          (with-crystallize-mocks
            {:summary {:content "Some content" :tags []}
             :entry-id "eid-1"
             :xpoll {:promoted 1 :candidates 2 :total-scanned 10
                     :entries [{:id "x" :promoted true}]}}
            (hooks/crystallize-session
             (assoc base-harvested :progress-notes [{:content "x"}])))
          content-keys (set (keys (:xpoll-stats content-result)))]
      (is (= no-content-keys content-keys)
          "Both paths should select-keys the same fields")
      (is (= #{:promoted :candidates :total-scanned} no-content-keys)
          "Should only include :promoted :candidates :total-scanned (no :entries)")
      ;; :entries should be filtered out by select-keys
      (is (not (contains? no-content-keys :entries))
          ":entries should NOT leak through select-keys"))))

(deftest crystallize-session-xpoll-error-in-stats-keys
  (testing "When xpoll has :error, it appears in xpoll-stats"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary nil
             :xpoll {:promoted 0 :candidates 0 :total-scanned 0
                     :error "chroma-not-configured"}}
            (hooks/crystallize-session base-harvested))]
      (is (contains? (:xpoll-stats result) :error))
      (is (= "chroma-not-configured" (:error (:xpoll-stats result)))))))

;; =============================================================================
;; Ordering: xpoll runs AFTER co-access promotion and edge decay
;; =============================================================================

(deftest crystallize-session-xpoll-runs-after-promotion-and-decay
  (testing "Cross-pollination runs after co-access promotion and edge decay, memory decay runs last"
    (let [call-order (atom [])
          {_result :result}
          (with-redefs [crystal/summarize-session-progress (fn [& _] nil)
                        crystal/session-id (fn [] "test-session")
                        scope/get-current-project-id (fn [_] "test-proj")
                        ctx/current-directory (fn [] "/tmp")
                        kg-edges/promote-co-access-edges!
                        (fn [_] (swap! call-order conj :promotion)
                          {:promoted 0 :skipped 0 :below 0 :evaluated 0})
                        kg-edges/decay-unverified-edges!
                        (fn [_] (swap! call-order conj :edge-decay)
                          {:decayed 0 :pruned 0 :fresh 0 :evaluated 0})
                        lifecycle/run-cross-pollination-cycle!
                        (fn [_] (swap! call-order conj :xpoll)
                          {:promoted 0 :candidates 0 :total-scanned 0})
                        lifecycle/run-decay-cycle!
                        (fn [_] (swap! call-order conj :memory-decay)
                          {:decayed 0 :expired 0 :total-scanned 0})]
            (hooks/crystallize-session base-harvested))]
      (is (= [:promotion :edge-decay :xpoll :memory-decay] @call-order)
          "Execution order: promotion → edge-decay → cross-pollination → memory-decay"))))

;; =============================================================================
;; P0.3: Memory decay (L2 time-decay) tests
;; =============================================================================

(deftest crystallize-session-no-content-includes-memory-decay-stats
  (testing "No-content path includes :memory-decay-stats in return map"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary nil
             :memory-decay {:decayed 5 :expired 2 :total-scanned 50}}
            (hooks/crystallize-session base-harvested))]
      (is (:skipped result) "Should be skipped (no-content path)")
      (is (contains? result :memory-decay-stats) "Return map must include :memory-decay-stats")
      (is (= 5 (:decayed (:memory-decay-stats result))))
      (is (= 2 (:expired (:memory-decay-stats result))))
      (is (= 50 (:total-scanned (:memory-decay-stats result)))))))

(deftest crystallize-session-content-includes-memory-decay-stats
  (testing "Content path includes :memory-decay-stats in return map"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary {:content "Session summary" :tags ["wrap"]}
             :entry-id "entry-decay-001"
             :memory-decay {:decayed 3 :expired 1 :total-scanned 30}}
            (hooks/crystallize-session
             (assoc base-harvested
                    :progress-notes [{:content "did stuff"}])))]
      (is (not (:skipped result)) "Should NOT be skipped (content path)")
      (is (contains? result :memory-decay-stats) "Return map must include :memory-decay-stats")
      (is (= 3 (:decayed (:memory-decay-stats result))))
      (is (= 1 (:expired (:memory-decay-stats result)))))))

(deftest crystallize-session-memory-decay-failure-non-blocking
  (testing "Memory decay failure does NOT block crystallization"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary nil
             :memory-decay-fn (fn [_] (throw (Exception. "Chroma timeout")))}
            (hooks/crystallize-session base-harvested))]
      (is (:skipped result) "Should still return skipped result")
      (is (contains? result :memory-decay-stats) "Must still have :memory-decay-stats key")
      (is (string? (:error (:memory-decay-stats result))) "Should contain error message")
      (is (= 0 (:decayed (:memory-decay-stats result))) "Decayed should be 0 on error"))))

(deftest crystallize-session-memory-decay-receives-directory
  (testing "Memory decay receives correct directory arg"
    (let [{:keys [memory-decay-calls]}
          (with-crystallize-mocks
            {:summary nil}
            (hooks/crystallize-session
             (assoc base-harvested :directory "/home/test/my-project")))]
      (is (= 1 (count memory-decay-calls)) "memory-decay should be called exactly once")
      (is (= "/home/test/my-project" (:directory (first memory-decay-calls)))
          "Should pass directory from harvested data")
      (is (= 50 (:limit (first memory-decay-calls)))
          "Should pass limit 50"))))

(deftest crystallize-session-memory-decay-stats-keys-consistent
  (testing "Both paths return same memory-decay-stats keys via select-keys"
    (let [{:keys [result]}
          (with-crystallize-mocks
            {:summary nil
             :memory-decay {:decayed 1 :expired 2 :total-scanned 10
                            :extra-field "should-not-leak"}}
            (hooks/crystallize-session base-harvested))
          no-content-keys (set (keys (:memory-decay-stats result)))

          {content-result :result}
          (with-crystallize-mocks
            {:summary {:content "Content" :tags []}
             :entry-id "eid-2"
             :memory-decay {:decayed 1 :expired 2 :total-scanned 10
                            :extra-field "should-not-leak"}}
            (hooks/crystallize-session
             (assoc base-harvested :progress-notes [{:content "x"}])))
          content-keys (set (keys (:memory-decay-stats content-result)))]
      (is (= no-content-keys content-keys)
          "Both paths should select-keys the same fields")
      (is (= #{:decayed :expired :total-scanned} no-content-keys)
          "Should only include :decayed :expired :total-scanned (no extra fields)")
      (is (not (contains? no-content-keys :extra-field))
          ":extra-field should NOT leak through select-keys"))))
