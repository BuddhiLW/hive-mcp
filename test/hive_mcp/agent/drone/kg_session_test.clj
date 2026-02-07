(ns hive-mcp.agent.drone.kg-session-test
  "Tests for KG-compressed context reconstruction stubs.

   Tests the noop fallback behavior when hive-knowledge is not on classpath.
   When hive-knowledge IS available, the real implementation is tested
   in hive-knowledge's own test suite."
  (:require [clojure.test :refer [deftest testing is are]]
            [hive-mcp.agent.drone.kg-session :as kg-session]))

;; =============================================================================
;; Schema Tests (Open contract — always testable)
;; =============================================================================

(deftest session-schema-test
  (testing "session-schema is a valid DataScript schema map"
    (is (map? kg-session/session-schema))
    (is (contains? kg-session/session-schema :node/id))
    (is (contains? kg-session/session-schema :node/type))
    (is (contains? kg-session/session-schema :node/content))
    (is (contains? kg-session/session-schema :node/turn))
    (is (contains? kg-session/session-schema :node/importance))
    (is (contains? kg-session/session-schema :node/superseded))
    (is (contains? kg-session/session-schema :node/files))
    (is (contains? kg-session/session-schema :node/tags)))

  (testing "node/id has unique identity constraint"
    (is (= :db.unique/identity
           (get-in kg-session/session-schema [:node/id :db/unique]))))

  (testing "node/files has cardinality many"
    (is (= :db.cardinality/many
           (get-in kg-session/session-schema [:node/files :db/cardinality]))))

  (testing "node/tags has cardinality many"
    (is (= :db.cardinality/many
           (get-in kg-session/session-schema [:node/tags :db/cardinality])))))

(deftest node-types-test
  (testing "node-types contains all expected types"
    (is (set? kg-session/node-types))
    (is (contains? kg-session/node-types :observation))
    (is (contains? kg-session/node-types :action))
    (is (contains? kg-session/node-types :discovery))
    (is (contains? kg-session/node-types :decision))
    (is (contains? kg-session/node-types :goal-state))
    (is (= 5 (count kg-session/node-types)))))

;; =============================================================================
;; Noop Fallback Tests (when hive-knowledge is NOT on classpath)
;; =============================================================================

(deftest compression-available-test
  (testing "compression-available? returns false without hive-knowledge"
    (is (false? (kg-session/compression-available?)))))

(deftest create-session-kg-noop-test
  (testing "create-session-kg! returns nil when hive-knowledge unavailable"
    (is (nil? (kg-session/create-session-kg! "drone-123" "Fix the bug")))))

(deftest compress-turn-noop-test
  (testing "compress-turn! returns 0 for nil session"
    (is (zero? (kg-session/compress-turn! nil [{:role "tool" :content "result"}]))))

  (testing "compress-turn! returns 0 for nil session with tool calls"
    (is (zero? (kg-session/compress-turn! nil
                [{:role "assistant"
                  :tool_calls [{:id "c1" :function {:name "read_file" :arguments "{}"}}]}
                 {:role "tool" :tool_call_id "c1" :name "read_file" :content "file content"}])))))

(deftest reconstruct-context-noop-test
  (testing "reconstruct-context returns empty string for nil session"
    (is (= "" (kg-session/reconstruct-context nil))))

  (testing "reconstruct-context accepts opts for nil session"
    (is (= "" (kg-session/reconstruct-context nil {:max-tokens 200})))))

(deftest build-compressed-messages-noop-test
  (testing "build-compressed-messages returns original messages for nil session"
    (let [original [{:role "system" :content "You are helpful."}
                    {:role "user" :content "Fix the bug."}
                    {:role "assistant" :content "I'll look at the code."}
                    {:role "user" :content "Great."}]]
      (is (= original
             (kg-session/build-compressed-messages nil "system" original [])))))

  (testing "all-messages pass through unchanged (no compression)"
    (let [msgs [{:role "user" :content "task"}]]
      (is (identical? msgs
                      (kg-session/build-compressed-messages nil "sys" msgs []))))))

(deftest promote-to-global-noop-test
  (testing "promote-to-global! returns {:promoted 0} for nil session"
    (is (= {:promoted 0 :edges-created 0}
           (kg-session/promote-to-global! nil nil))))

  (testing "promote-to-global! accepts opts for nil session"
    (is (= {:promoted 0 :edges-created 0}
           (kg-session/promote-to-global! nil nil {:threshold 0.5 :scope "project"})))))

(deftest promotable-nodes-noop-test
  (testing "promotable-nodes returns [] for nil session"
    (is (= [] (kg-session/promotable-nodes nil))))

  (testing "promotable-nodes accepts opts for nil session"
    (is (= [] (kg-session/promotable-nodes nil {:threshold 0.5})))))

(deftest session-stats-noop-test
  (testing "session-stats returns noop stats for nil session"
    (let [stats (kg-session/session-stats nil)]
      (is (map? stats))
      (is (= 0 (:turns-compressed stats)))
      (is (= 0 (:tokens-saved stats)))
      (is (= 0 (:nodes-created stats)))
      (is (= 0 (:nodes-active stats)))
      (is (= 0 (:nodes-superseded stats)))
      (is (string? (:compression-ratio stats))))))

(deftest close-session-noop-test
  (testing "close-session! returns noop stats for nil session"
    (let [stats (kg-session/close-session! nil)]
      (is (map? stats))
      (is (= 0 (:turns-compressed stats))))))

;; =============================================================================
;; Integration Contract Tests
;; =============================================================================

(deftest noop-integration-flow-test
  (testing "Full noop flow: create -> compress -> reconstruct -> close"
    (let [session (kg-session/create-session-kg! "drone-test" "Implement feature X")]
      ;; Session is nil (noop)
      (is (nil? session))

      ;; Compress returns 0
      (is (zero? (kg-session/compress-turn! session
                   [{:role "assistant" :tool_calls [{:id "c1" :function {:name "read_file" :arguments "{}"}}]}
                    {:role "tool" :tool_call_id "c1" :name "read_file" :content "file content"}])))

      ;; Reconstruct returns empty
      (is (= "" (kg-session/reconstruct-context session)))

      ;; Build returns original messages
      (let [original [{:role "system" :content "sys"} {:role "user" :content "task"}]]
        (is (= original (kg-session/build-compressed-messages session "sys" original []))))

      ;; Promote returns zero
      (is (= {:promoted 0 :edges-created 0} (kg-session/promote-to-global! session nil)))

      ;; Close returns noop stats
      (let [final (kg-session/close-session! session)]
        (is (= 0 (:turns-compressed final)))))))
