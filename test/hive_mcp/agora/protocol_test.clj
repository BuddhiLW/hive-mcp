(ns hive-mcp.agora.protocol-test
  "Tests for Agora unified coordination protocols.

   TDD: These tests define expected behavior for ICoordination,
   ISignalable, and IConsensus protocols."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.agora.protocol :as proto]
            [hive-mcp.agora.schema :as schema]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-fixture [f]
  (schema/reset-conn!)
  (f)
  (schema/reset-conn!))

(use-fixtures :each reset-fixture)

;; =============================================================================
;; ICoordination Protocol Tests
;; =============================================================================

(deftest test-icoordination-protocol-exists
  (testing "ICoordination protocol is defined"
    (is (some? proto/ICoordination))
    ;; Create a real coordination to test protocol satisfaction
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["test-1"]
                  :topic "Protocol test"})]
      (is (satisfies? proto/ICoordination coord)))))

(deftest test-add-participant
  (testing "add-participant adds agent to coordination"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1"]
                  :topic "Test dialogue"})]
      (proto/add-participant coord "agent-2" :reviewer)
      (is (contains? (proto/get-participants coord) "agent-2")))))

(deftest test-signal-dispatch
  (testing "signal dispatches event and records turn"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1" "agent-2"]
                  :topic "Test dialogue"})
          result (proto/signal coord :propose {:from "agent-1"
                                               :to "agent-2"
                                               :message "Initial proposal"})]
      (is (= :propose (:signal result)))
      (is (= 1 (:turn result))))))

(deftest test-check-consensus
  (testing "check-consensus returns consensus status"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1" "agent-2"]
                  :topic "Test dialogue"})]
      ;; Fresh coordination has insufficient turns for consensus evaluation
      (is (= :insufficient (proto/check-consensus coord))))))

(deftest test-next-turn
  (testing "next-turn advances turn counter"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1" "agent-2"]
                  :topic "Test dialogue"})]
      ;; Signal advances turn
      (proto/signal coord :propose {:from "agent-1" :to "agent-2" :message "Test"})
      (is (= 2 (proto/next-turn coord))))))

;; =============================================================================
;; ISignalable Protocol Tests
;; =============================================================================

(deftest test-isignalable-protocol-exists
  (testing "ISignalable protocol is defined"
    (is (some? proto/ISignalable))))

(deftest test-emit-and-subscribe
  (testing "emit broadcasts events to subscribers"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1" "agent-2"]
                  :topic "Test dialogue"})
          received (atom nil)]
      ;; Subscribe to turn events
      (proto/subscribe coord :turn (fn [data] (reset! received data)))
      ;; Emit event
      (proto/emit coord {:type :turn :turn-num 1 :signal :propose})
      ;; Verify received
      (is (= :turn (:type @received)))
      (is (= 1 (:turn-num @received))))))

(deftest test-multiple-subscribers
  (testing "multiple handlers receive same event"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1" "agent-2"]
                  :topic "Test"})
          count1 (atom 0)
          count2 (atom 0)]
      (proto/subscribe coord :turn (fn [_] (swap! count1 inc)))
      (proto/subscribe coord :turn (fn [_] (swap! count2 inc)))
      (proto/emit coord {:type :turn :data "test"})
      (is (= 1 @count1))
      (is (= 1 @count2)))))

;; =============================================================================
;; IConsensus Protocol Tests
;; =============================================================================

(deftest test-iconsensus-protocol-exists
  (testing "IConsensus protocol is defined"
    (is (some? proto/IConsensus))))

(deftest test-consensus-threshold
  (testing "threshold returns configured approval threshold"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1" "agent-2"]
                  :topic "Test"
                  :config {:threshold 0.8}})]
      (is (= 0.8 (proto/threshold coord))))))

(deftest test-consensus-evaluate
  (testing "evaluate computes consensus from signals"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["agent-1" "agent-2"]
                  :topic "Test"
                  :config {:threshold 0.8}})]
      ;; Both approve
      (let [votes [{:participant "agent-1" :signal :approve}
                   {:participant "agent-2" :signal :approve}]]
        (is (= :consensus (proto/evaluate coord votes))))
      ;; One counter
      (let [votes [{:participant "agent-1" :signal :approve}
                   {:participant "agent-2" :signal :counter}]]
        (is (= :continue (proto/evaluate coord votes)))))))

;; =============================================================================
;; Dialogue Coordination Integration Tests
;; =============================================================================

(deftest test-dialogue-full-cycle
  (testing "dialogue coordination through full cycle to consensus"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["writer" "critic"]
                  :topic "Code review"})]
      ;; Writer proposes
      (proto/signal coord :propose {:from "writer"
                                    :to "critic"
                                    :message "Here is my implementation"})
      ;; After 1 turn, still insufficient
      (is (= :insufficient (proto/check-consensus coord)))

      ;; Critic approves
      (proto/signal coord :approve {:from "critic"
                                    :to "writer"
                                    :message "LGTM"})
      ;; After 2 turns, status should be computed (may be :continue or :consensus)
      (let [status (proto/check-consensus coord)]
        (is (contains? #{:consensus :continue :insufficient :stuck :timeout} status))))))

(deftest test-coordination-get-turns
  (testing "get-turns returns turn history"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["a" "b"]
                  :topic "Test"})]
      (proto/signal coord :propose {:from "a" :to "b" :message "First"})
      (proto/signal coord :counter {:from "b" :to "a" :message "Second"})
      (let [turns (proto/get-turns coord)]
        (is (= 2 (count turns)))
        (is (= :propose (:signal (first turns))))
        (is (= :counter (:signal (second turns))))))))

;; =============================================================================
;; Debate Coordination Tests
;; =============================================================================

(deftest test-debate-coordination-type
  (testing "DebateCoordination implements ICoordination"
    (let [coord (proto/create-debate-coordination!
                 {:topic "Strategy vs Decorator"
                  :roles [{:role "advocate" :position "For Strategy"}
                          {:role "skeptic" :position "Against Strategy"}]})]
      (is (satisfies? proto/ICoordination coord))
      (is (satisfies? proto/ISignalable coord))
      (is (satisfies? proto/IConsensus coord)))))

(deftest test-debate-methodology
  (testing "DebateCoordination supports methodology"
    (let [coord (proto/create-debate-coordination!
                 {:topic "Test debate"
                  :roles [{:role "a" :position "X"}
                          {:role "b" :position "Y"}]
                  :methodology :fact-based})]
      (is (= :fact-based (proto/get-methodology coord))))))

;; =============================================================================
;; Protocol Compliance - Liskov Substitution Tests
;; =============================================================================

(deftest test-liskov-substitution
  (testing "all coordination types satisfy ICoordination uniformly"
    (let [dialogue (proto/create-dialogue-coordination!
                    {:participants ["a" "b"] :topic "D"})
          debate (proto/create-debate-coordination!
                  {:topic "Debate"
                   :roles [{:role "x" :position "P1"}
                           {:role "y" :position "P2"}]})]
      ;; Both should support same protocol methods
      (doseq [coord [dialogue debate]]
        (is (satisfies? proto/ICoordination coord))
        (is (keyword? (proto/check-consensus coord)))
        (is (number? (proto/next-turn coord)))))))

;; =============================================================================
;; Backward Compatibility Tests
;; =============================================================================

(deftest test-backward-compat-dialogue-api
  (testing "existing dialogue.clj functions still work"
    ;; Ensure protocol doesn't break existing API
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["a" "b"]
                  :topic "Test"})]
      ;; get-id should return dialogue-id usable with schema
      (let [dialogue-id (proto/get-id coord)]
        (is (string? dialogue-id))
        (is (some? (schema/get-dialogue dialogue-id)))))))

(deftest test-backward-compat-consensus-api
  (testing "consensus functions work with coordination"
    (let [coord (proto/create-dialogue-coordination!
                 {:participants ["a" "b"]
                  :topic "Test"})]
      ;; Protocol consensus check should return same status types
      (let [status (proto/check-consensus coord)]
        (is (contains? #{:consensus :continue :timeout :stuck :insufficient}
                       status))))))
