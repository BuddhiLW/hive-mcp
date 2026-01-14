(ns hive-mcp.prompts.infra-test
  "Tests for prompts/infra notification adapters.
   
   Tests verify:
   - Specialized notification functions delegate correctly to hive-mcp.notify
   - Correct notification types, urgency levels, and content formatting
   - Channel emission functions work correctly
   
   Following ISP (Interface Segregation Principle):
   Tests verify PUBLIC API behavior, not implementation details."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.prompts.infra :as infra]
            [hive-mcp.notify :as notify]
            [hive-mcp.channel :as channel]))

;; =============================================================================
;; Test Fixtures & Helpers
;; =============================================================================

(def ^:dynamic *notify-calls* nil)
(def ^:dynamic *channel-calls* nil)

(defn capture-calls-fixture
  "Fixture to capture notify and channel calls for verification."
  [f]
  (binding [*notify-calls* (atom [])
            *channel-calls* (atom [])]
    (f)))

(use-fixtures :each capture-calls-fixture)

(defn mock-notify!
  "Mock notify! that captures calls and returns success."
  [opts]
  (when *notify-calls*
    (swap! *notify-calls* conj opts))
  true)

(defn mock-channel-emit!
  "Mock channel emit that captures calls."
  [event-type data]
  (when *channel-calls*
    (swap! *channel-calls* conj {:event-type event-type :data data}))
  true)

;; =============================================================================
;; Test: notify-permission-request!
;; =============================================================================

(deftest test-notify-permission-request!
  (testing "notify-permission-request! sends critical notification with correct format"
    (with-redefs [notify/notify! mock-notify!]
      (infra/notify-permission-request! "agent-123" "file_edit" "Edit src/core.clj")

      (is (= 1 (count @*notify-calls*)))
      (let [opts (first @*notify-calls*)]
        ;; Should have lock emoji and tool name in summary
        (is (clojure.string/includes? (:summary opts) "🔐"))
        (is (clojure.string/includes? (:summary opts) "file_edit"))
        ;; Body should include agent and summary
        (is (clojure.string/includes? (:body opts) "agent-123"))
        (is (clojure.string/includes? (:body opts) "Edit src/core.clj"))
        ;; Critical type (error)
        (is (= "error" (:type opts)))
        ;; Never auto-dismiss (timeout 0)
        (is (= 0 (:timeout opts)))))))

;; =============================================================================
;; Test: notify-agent-blocked!
;; =============================================================================

(deftest test-notify-agent-blocked!
  (testing "notify-agent-blocked! sends warning notification"
    (with-redefs [notify/notify! mock-notify!]
      (infra/notify-agent-blocked! "agent-456" "Waiting for user input")

      (is (= 1 (count @*notify-calls*)))
      (let [opts (first @*notify-calls*)]
        ;; Should have pause emoji in summary
        (is (clojure.string/includes? (:summary opts) "⏸️"))
        (is (clojure.string/includes? (:summary opts) "agent-456"))
        ;; Body should be the reason
        (is (= "Waiting for user input" (:body opts)))
        ;; Warning type
        (is (= "warning" (:type opts)))))))

;; =============================================================================
;; Test: notify-agent-completed!
;; =============================================================================

(deftest test-notify-agent-completed!
  (testing "notify-agent-completed! sends info notification"
    (with-redefs [notify/notify! mock-notify!]
      (infra/notify-agent-completed! "agent-789" "Tests passed: 42/42")

      (is (= 1 (count @*notify-calls*)))
      (let [opts (first @*notify-calls*)]
        ;; Should have checkmark emoji in summary
        (is (clojure.string/includes? (:summary opts) "✅"))
        (is (clojure.string/includes? (:summary opts) "agent-789"))
        ;; Body should be the task summary
        (is (= "Tests passed: 42/42" (:body opts)))
        ;; Info type
        (is (= "info" (:type opts)))))))

;; =============================================================================
;; Test: Channel Emission
;; =============================================================================

(deftest test-emit-prompt-pending!
  (testing "emit-prompt-pending! emits correct event structure"
    (with-redefs [channel/emit-event! mock-channel-emit!]
      (infra/emit-prompt-pending! {:id "prompt-1"
                                   :agent-id "agent-123"
                                   :question "Delete file?"
                                   :options ["yes" "no"]
                                   :created-at 1234567890})

      (is (= 1 (count @*channel-calls*)))
      (let [{:keys [event-type data]} (first @*channel-calls*)]
        (is (= :prompt-pending event-type))
        (is (= "prompt-1" (:prompt-id data)))
        (is (= "agent-123" (:agent-id data)))
        (is (= "Delete file?" (:question data)))
        (is (= ["yes" "no"] (:options data)))
        (is (= 1234567890 (:created-at data)))))))

(deftest test-emit-prompt-resolved!
  (testing "emit-prompt-resolved! emits correct event structure"
    (with-redefs [channel/emit-event! mock-channel-emit!]
      (infra/emit-prompt-resolved! {:id "prompt-2"
                                    :agent-id "agent-456"
                                    :response "yes"
                                    :status :approved
                                    :resolved-at 1234567899})

      (is (= 1 (count @*channel-calls*)))
      (let [{:keys [event-type data]} (first @*channel-calls*)]
        (is (= :prompt-resolved event-type))
        (is (= "prompt-2" (:prompt-id data)))
        (is (= "agent-456" (:agent-id data)))
        (is (= "yes" (:response data)))
        (is (= :approved (:status data)))
        (is (= 1234567899 (:resolved-at data)))))))

(deftest test-emit-prompt-expired!
  (testing "emit-prompt-expired! emits correct event structure"
    (with-redefs [channel/emit-event! mock-channel-emit!]
      (infra/emit-prompt-expired! {:id "prompt-3"
                                   :agent-id "agent-789"
                                   :resolved-at 1234567900})

      (is (= 1 (count @*channel-calls*)))
      (let [{:keys [event-type data]} (first @*channel-calls*)]
        (is (= :prompt-expired event-type))
        (is (= "prompt-3" (:prompt-id data)))
        (is (= "agent-789" (:agent-id data)))
        (is (= 1234567900 (:resolved-at data)))))))

;; =============================================================================
;; Test: Edge Cases
;; =============================================================================

(deftest test-notify-with-special-characters
  (testing "notification functions handle special characters"
    (with-redefs [notify/notify! mock-notify!]
      (infra/notify-permission-request!
       "agent<>123"
       "file_edit"
       "Edit \"core.clj\" & <test>.clj")

      (is (= 1 (count @*notify-calls*)))
      (let [opts (first @*notify-calls*)]
        (is (clojure.string/includes? (:body opts) "agent<>123"))
        (is (clojure.string/includes? (:body opts) "Edit \"core.clj\" & <test>.clj"))))))

(deftest test-notify-with-multiline-content
  (testing "notification functions handle multiline content"
    (with-redefs [notify/notify! mock-notify!]
      (infra/notify-agent-blocked! "agent-1" "Line 1\nLine 2\nLine 3")

      (is (= 1 (count @*notify-calls*)))
      (let [opts (first @*notify-calls*)]
        (is (= "Line 1\nLine 2\nLine 3" (:body opts)))))))
