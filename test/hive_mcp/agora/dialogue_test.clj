(ns hive-mcp.agora.dialogue-test
  "Tests for Agora dialogue dispatch wrapper.

   Covers:
   - Signal parsing from [SIGNAL: X] prefix
   - Dialogue lifecycle (create, join, leave)
   - Turn recording and querying
   - Nash equilibrium detection
   - Dialogue dispatch wrapper"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.agora.dialogue :as dialogue]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn reset-dialogues-fixture
  "Reset dialogue state before each test."
  [f]
  (dialogue/reset-dialogues!)
  (f))

(use-fixtures :each reset-dialogues-fixture)

;;; =============================================================================
;;; Signal Parsing Tests
;;; =============================================================================

(deftest parse-signal-with-prefix-test
  (testing "parse-signal extracts signal from [SIGNAL: X] prefix"
    (is (= [:propose "Here's my change" :prefix]
           (dialogue/parse-signal "[SIGNAL: propose] Here's my change")))
    (is (= [:approve "LGTM" :prefix]
           (dialogue/parse-signal "[SIGNAL: approve] LGTM")))
    (is (= [:no-change "Nothing to add" :prefix]
           (dialogue/parse-signal "[SIGNAL: no-change] Nothing to add")))
    (is (= [:counter "Alternative approach" :prefix]
           (dialogue/parse-signal "[SIGNAL: counter] Alternative approach")))
    (is (= [:defer "I'll let others decide" :prefix]
           (dialogue/parse-signal "[SIGNAL: defer] I'll let others decide")))))

(deftest parse-signal-case-insensitive-test
  (testing "parse-signal is case-insensitive"
    (is (= [:propose "test" :prefix]
           (dialogue/parse-signal "[SIGNAL: PROPOSE] test")))
    (is (= [:approve "test" :prefix]
           (dialogue/parse-signal "[SIGNAL: Approve] test")))
    (is (= [:no-change "test" :prefix]
           (dialogue/parse-signal "[SIGNAL: NO-CHANGE] test")))))

(deftest parse-signal-no-prefix-test
  (testing "parse-signal defaults to :propose when no prefix and no natural language match"
    (is (= [:propose "Just a regular message" :default]
           (dialogue/parse-signal "Just a regular message")))
    (is (= [:propose "" :default]
           (dialogue/parse-signal "")))))

(deftest parse-signal-invalid-signal-test
  (testing "parse-signal defaults to :propose for unknown signals"
    ;; Unknown signal should fall through with warning, keep original message
    (let [[signal message detection] (dialogue/parse-signal "[SIGNAL: unknown] text")]
      (is (= :propose signal))
      (is (= "[SIGNAL: unknown] text" message))
      (is (= :default detection)))))

(deftest format-signal-test
  (testing "format-signal creates [SIGNAL: X] prefix"
    (is (= "[SIGNAL: propose] My proposal"
           (dialogue/format-signal :propose "My proposal")))
    (is (= "[SIGNAL: approve] LGTM"
           (dialogue/format-signal :approve "LGTM")))))

;;; =============================================================================
;;; Dialogue Lifecycle Tests
;;; =============================================================================

(deftest create-dialogue-basic-test
  (testing "create-dialogue creates dialogue with participants"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["writer-123" "critic-456"]
                        :topic "Code review"})]
      (is (string? dialogue-id))
      (is (clojure.string/starts-with? dialogue-id "dialogue-"))
      (let [d (dialogue/get-dialogue dialogue-id)]
        (is (= #{"writer-123" "critic-456"} (:participants d)))
        (is (= "Code review" (:topic d)))
        (is (= :active (:status d)))
        (is (empty? (:turns d)))))))

(deftest create-dialogue-minimum-participants-test
  (testing "create-dialogue requires at least 2 participants"
    (is (thrown? AssertionError
                 (dialogue/create-dialogue {:participants ["only-one"]})))))

(deftest join-dialogue-test
  (testing "join-dialogue adds participant"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["writer" "critic"]})]
      (is (true? (dialogue/join-dialogue dialogue-id "mediator")))
      (is (= #{"writer" "critic" "mediator"}
             (dialogue/get-participants dialogue-id))))))

(deftest join-dialogue-not-found-test
  (testing "join-dialogue returns false for non-existent dialogue"
    (is (false? (dialogue/join-dialogue "non-existent" "someone")))))

(deftest leave-dialogue-test
  (testing "leave-dialogue removes participant"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["a" "b" "c"]})]
      (is (true? (dialogue/leave-dialogue dialogue-id "c")))
      (is (= #{"a" "b"} (dialogue/get-participants dialogue-id))))))

(deftest leave-dialogue-ends-when-insufficient-test
  (testing "leave-dialogue ends dialogue when < 2 participants"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["a" "b"]})]
      (dialogue/leave-dialogue dialogue-id "b")
      ;; Use public API instead of accessing private state
      (is (= :ended (:status (dialogue/get-dialogue dialogue-id)))))))

;;; =============================================================================
;;; Turn Recording Tests
;;; =============================================================================

(deftest record-turn-via-internal-test
  (testing "Turns are recorded with correct structure"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["sender" "receiver"]})]
      ;; Use internal function for isolated testing
      (#'dialogue/record-turn! dialogue-id
                               {:sender "sender"
                                :receiver "receiver"
                                :message "Test message"
                                :signal :propose})
      (let [turns (dialogue/get-dialogue-turns dialogue-id)]
        (is (= 1 (count turns)))
        (let [turn (first turns)]
          (is (= 1 (:turn-num turn)))
          (is (= "sender" (:sender turn)))
          (is (= "receiver" (:receiver turn)))
          (is (= "Test message" (:message turn)))
          (is (= :propose (:signal turn)))
          (is (some? (:timestamp turn)))
          (is (string? (:id turn))))))))

(deftest get-last-turn-for-test
  (testing "get-last-turn-for returns most recent turn from participant"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["a" "b"]})]
      (#'dialogue/record-turn! dialogue-id
                               {:sender "a" :receiver "b" :message "First" :signal :propose})
      (#'dialogue/record-turn! dialogue-id
                               {:sender "b" :receiver "a" :message "Response" :signal :counter})
      (#'dialogue/record-turn! dialogue-id
                               {:sender "a" :receiver "b" :message "Second" :signal :approve})
      (let [last-a (dialogue/get-last-turn-for dialogue-id "a")
            last-b (dialogue/get-last-turn-for dialogue-id "b")]
        (is (= "Second" (:message last-a)))
        (is (= :approve (:signal last-a)))
        (is (= "Response" (:message last-b)))
        (is (= :counter (:signal last-b)))))))

;;; =============================================================================
;;; Nash Equilibrium Tests
;;; =============================================================================

(deftest nash-equilibrium-not-reached-test
  (testing "nash-equilibrium? returns false when not all have signaled"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["a" "b"]})]
      ;; Only one participant has signaled
      (#'dialogue/record-turn! dialogue-id
                               {:sender "a" :receiver "b" :message "x" :signal :approve})
      (is (false? (dialogue/nash-equilibrium? dialogue-id))))))

(deftest nash-equilibrium-disrupted-test
  (testing "nash-equilibrium? returns false when any signal is disruptive"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["a" "b"]})]
      (#'dialogue/record-turn! dialogue-id
                               {:sender "a" :receiver "b" :message "x" :signal :approve})
      (#'dialogue/record-turn! dialogue-id
                               {:sender "b" :receiver "a" :message "x" :signal :propose})
      (is (false? (dialogue/nash-equilibrium? dialogue-id))))))

(deftest nash-equilibrium-reached-test
  (testing "nash-equilibrium? returns true when all signal equilibrium"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["a" "b"]})]
      (#'dialogue/record-turn! dialogue-id
                               {:sender "a" :receiver "b" :message "x" :signal :approve})
      (#'dialogue/record-turn! dialogue-id
                               {:sender "b" :receiver "a" :message "x" :signal :no-change})
      (is (true? (dialogue/nash-equilibrium? dialogue-id))))))

(deftest nash-equilibrium-both-approve-test
  (testing "nash-equilibrium? with both participants approving"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["writer" "critic"]})]
      (#'dialogue/record-turn! dialogue-id
                               {:sender "writer" :receiver "critic" :message "Final version" :signal :approve})
      (#'dialogue/record-turn! dialogue-id
                               {:sender "critic" :receiver "writer" :message "LGTM" :signal :approve})
      (is (true? (dialogue/nash-equilibrium? dialogue-id))))))

;;; =============================================================================
;;; List/Summary Tests
;;; =============================================================================

(deftest list-dialogues-test
  (testing "list-dialogues returns all dialogues"
    (dialogue/create-dialogue {:participants ["a" "b"] :topic "First"})
    (dialogue/create-dialogue {:participants ["c" "d"] :topic "Second"})
    (let [dialogues (dialogue/list-dialogues)]
      (is (= 2 (count dialogues)))
      (is (= #{"First" "Second"} (set (map :topic dialogues)))))))

(deftest dialogue-summary-test
  (testing "dialogue-summary returns human-readable summary"
    (let [dialogue-id (dialogue/create-dialogue
                       {:participants ["writer" "critic"]
                        :topic "Auth review"})]
      (#'dialogue/record-turn! dialogue-id
                               {:sender "writer" :receiver "critic" :message "x" :signal :propose})
      (#'dialogue/record-turn! dialogue-id
                               {:sender "critic" :receiver "writer" :message "x" :signal :counter})
      (let [summary (dialogue/dialogue-summary dialogue-id)]
        (is (= dialogue-id (:id summary)))
        (is (= "Auth review" (:topic summary)))
        (is (= :active (:status summary)))
        (is (= 2 (:turn-count summary)))
        (is (= :counter (:last-signal summary)))
        (is (false? (:consensus? summary)))))))

;;; =============================================================================
;;; Signal Set Tests
;;; =============================================================================

;;; =============================================================================
;;; Natural Language Signal Detection Tests
;;; =============================================================================

(deftest detect-natural-signal-test
  (testing "detects approval patterns"
    (is (= :approve (dialogue/detect-natural-signal "I accept this approach")))
    (is (= :approve (dialogue/detect-natural-signal "I approve of this change")))
    (is (= :approve (dialogue/detect-natural-signal "I agree with the proposal")))
    (is (= :approve (dialogue/detect-natural-signal "LGTM!")))
    (is (= :approve (dialogue/detect-natural-signal "looks good to me")))
    (is (= :approve (dialogue/detect-natural-signal "This looks good")))
    (is (= :approve (dialogue/detect-natural-signal "No objections here")))
    (is (= :approve (dialogue/detect-natural-signal "ship it!")))
    (is (= :approve (dialogue/detect-natural-signal "Sounds good to me"))))

  (testing "detects counter patterns"
    (is (= :counter (dialogue/detect-natural-signal "I disagree with this")))
    (is (= :counter (dialogue/detect-natural-signal "I object to the proposal")))
    (is (= :counter (dialogue/detect-natural-signal "I reject this approach")))
    (is (= :counter (dialogue/detect-natural-signal "I have concerns about this")))
    (is (= :counter (dialogue/detect-natural-signal "I have issues with the design"))))

  (testing "returns nil for neutral/ambiguous messages"
    (is (nil? (dialogue/detect-natural-signal "Here is my analysis")))
    (is (nil? (dialogue/detect-natural-signal "Let me review this")))
    (is (nil? (dialogue/detect-natural-signal "What do you think?")))))

(deftest parse-signal-natural-language-test
  (testing "parse-signal detects natural language approval"
    (is (= [:approve "LGTM, ship it!" :natural]
           (dialogue/parse-signal "LGTM, ship it!")))
    (is (= [:approve "I accept this approach" :natural]
           (dialogue/parse-signal "I accept this approach"))))

  (testing "parse-signal detects natural language counter"
    (is (= [:counter "I disagree, we need a different approach" :natural]
           (dialogue/parse-signal "I disagree, we need a different approach"))))

  (testing "prefix takes priority over natural language"
    ;; Even if the message contains natural approval language, prefix wins
    (is (= [:propose "I accept this, here's my proposal" :prefix]
           (dialogue/parse-signal "[SIGNAL: propose] I accept this, here's my proposal")))))

;;; =============================================================================
;;; Signal Set Tests
;;; =============================================================================

(deftest signal-sets-test
  (testing "Signal sets are correctly defined"
    (is (= #{:propose :counter :no-change :approve :defer}
           dialogue/signals))
    (is (= #{:no-change :approve}
           dialogue/equilibrium-signals))
    (is (= #{:propose :counter}
           dialogue/disruption-signals))))
