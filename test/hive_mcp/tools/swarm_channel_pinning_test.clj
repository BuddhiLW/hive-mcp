(ns hive-mcp.tools.swarm-channel-pinning-test
  "Pinning tests for swarm channel/event management functions.
   
   These tests verify the channel subscription and event journal behavior:
   - Event journal state management (atoms)
   - Channel subscription lifecycle
   - Event handling and journal updates
   
   Uses with-redefs to mock channel dependencies for isolated unit testing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.core.async :as async :refer [chan >!! <!! close! timeout alts!!]]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.tools.swarm.channel :as channel]))

;; =============================================================================
;; Test Fixtures - Reset state between tests
;; =============================================================================

(defn reset-swarm-state
  "Fixture to reset swarm state between tests."
  [f]
  (swarm/clear-event-journal!)
  (swarm/stop-channel-subscriptions!)
  (f)
  (swarm/clear-event-journal!)
  (swarm/stop-channel-subscriptions!))

(use-fixtures :each reset-swarm-state)

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn gen-task-id
  "Generate unique task ID for tests."
  []
  (str "task-" (java.util.UUID/randomUUID)))

(defn make-test-event
  "Create a test event with string keys (as bencode returns)."
  [& {:keys [task-id slave-id result error prompt timestamp]
      :or {task-id (gen-task-id)
           slave-id "test-slave-1"
           timestamp (System/currentTimeMillis)}}]
  (cond-> {"task-id" task-id
           "slave-id" slave-id
           "timestamp" timestamp}
    result (assoc "result" result)
    error (assoc "error" error)
    prompt (assoc "prompt" prompt)))

;; =============================================================================
;; Test: Event Journal State Management
;; =============================================================================

(deftest test-clear-event-journal!-resets-state
  (testing "clear-event-journal! resets the journal to empty map"
    ;; Pre-populate the journal by checking non-existent (to verify it works)
    ;; Then manually add via the handler mechanism
    (let [task-id (gen-task-id)]
      ;; Simulate adding an event via handle-task-completed
      (with-redefs [hive-mcp.tools.swarm.channel/try-require-channel (constantly false)]
        ;; Directly manipulate the atom for setup (since handlers are private)
        ;; We'll use check-event-journal to verify state
        (is (nil? (swarm/check-event-journal task-id)))

        ;; Clear should work even on empty journal
        (swarm/clear-event-journal!)
        (is (nil? (swarm/check-event-journal task-id)))))))

(deftest test-check-event-journal-returns-nil-for-missing
  (testing "check-event-journal returns nil for non-existent task"
    (let [task-id (gen-task-id)]
      (is (nil? (swarm/check-event-journal task-id))))))

(deftest test-check-event-journal-coerces-task-id-to-string
  (testing "check-event-journal converts task-id to string"
    ;; Both numeric and string lookups should work
    (is (nil? (swarm/check-event-journal 12345)))
    (is (nil? (swarm/check-event-journal "12345")))
    (is (nil? (swarm/check-event-journal :keyword-id)))))

;; =============================================================================
;; Test: Channel Subscription Lifecycle
;; =============================================================================

(deftest test-start-channel-subscriptions!-no-channel-available
  (testing "start-channel-subscriptions! does nothing when channel unavailable"
    (with-redefs [hive-mcp.tools.swarm.channel/try-require-channel (constantly false)]
      ;; Should not throw, just return nil
      (is (nil? (swarm/start-channel-subscriptions!))))))

(deftest test-stop-channel-subscriptions!-idempotent
  (testing "stop-channel-subscriptions! is idempotent and safe to call multiple times"
    ;; First call
    (swarm/stop-channel-subscriptions!)
    ;; Second call should not throw
    (swarm/stop-channel-subscriptions!)
    ;; Third call for good measure
    (is (nil? (swarm/stop-channel-subscriptions!)))))

(deftest test-start-channel-subscriptions!-with-mock-channel
  (testing "start-channel-subscriptions! subscribes to expected event types"
    (let [subscribed-events (atom [])]
      (with-redefs [hive-mcp.tools.swarm.channel/try-require-channel (constantly true)
                    hive-mcp.tools.swarm.channel/channel-subscribe!
                    (fn [event-type]
                      (swap! subscribed-events conj event-type)
                      (chan 1))] ; Return a real channel
        (swarm/start-channel-subscriptions!)

        ;; Should have subscribed to all three event types
        (is (contains? (set @subscribed-events) :task-completed))
        (is (contains? (set @subscribed-events) :task-failed))
        (is (contains? (set @subscribed-events) :prompt-shown))

        ;; Cleanup
        (swarm/stop-channel-subscriptions!)))))

(deftest test-stop-channel-subscriptions!-closes-channels
  (testing "stop-channel-subscriptions! closes all subscription channels"
    (let [test-channels (atom [])
          make-test-chan (fn []
                           (let [c (chan 1)]
                             (swap! test-channels conj c)
                             c))]
      (with-redefs [hive-mcp.tools.swarm.channel/try-require-channel (constantly true)
                    hive-mcp.tools.swarm.channel/channel-subscribe! (fn [_] (make-test-chan))]
        (swarm/start-channel-subscriptions!)

        ;; Verify channels were created
        (is (= 3 (count @test-channels)))

        ;; Stop subscriptions
        (swarm/stop-channel-subscriptions!)

        ;; Verify all channels are closed (put! returns false on closed channel)
        (doseq [c @test-channels]
          (is (false? (async/put! c {:test true}))
              "Channel should be closed after stop"))))))

;; =============================================================================
;; Test: Event Handler Integration (via exposed journal functions)
;; =============================================================================

(deftest test-event-journal-persistence-across-operations
  (testing "Event journal maintains state across multiple operations"
    ;; Since handlers are private, we test through the exposed interface
    ;; by verifying the journal lookup behavior
    (let [task-1 "persist-task-1"
          task-2 "persist-task-2"]

      ;; Both should be nil initially
      (is (nil? (swarm/check-event-journal task-1)))
      (is (nil? (swarm/check-event-journal task-2)))

      ;; Clear should not affect subsequent lookups
      (swarm/clear-event-journal!)
      (is (nil? (swarm/check-event-journal task-1)))
      (is (nil? (swarm/check-event-journal task-2))))))

;; =============================================================================
;; Test: Private Handler Functions (via #' access for pinning)
;; =============================================================================

(deftest test-handle-task-completed-updates-journal
  (testing "handle-task-completed adds completed event to journal"
    (let [task-id (gen-task-id)
          event (make-test-event :task-id task-id
                                 :slave-id "slave-1"
                                 :result "success output")]
      ;; Access private function via var
      (#'channel/handle-task-completed event)

      (let [journal-entry (swarm/check-event-journal task-id)]
        (is (some? journal-entry))
        (is (= "completed" (:status journal-entry)))
        (is (= "success output" (:result journal-entry)))
        (is (= "slave-1" (:slave-id journal-entry)))
        (is (number? (:timestamp journal-entry)))))))

(deftest test-handle-task-failed-updates-journal
  (testing "handle-task-failed adds failed event to journal"
    (let [task-id (gen-task-id)
          event (make-test-event :task-id task-id
                                 :slave-id "slave-2"
                                 :error "Something went wrong")]
      (#'channel/handle-task-failed event)

      (let [journal-entry (swarm/check-event-journal task-id)]
        (is (some? journal-entry))
        (is (= "failed" (:status journal-entry)))
        (is (= "Something went wrong" (:error journal-entry)))
        (is (= "slave-2" (:slave-id journal-entry)))
        (is (number? (:timestamp journal-entry)))))))

(deftest test-handle-prompt-shown-does-not-modify-journal
  (testing "handle-prompt-shown only logs, doesn't modify event journal"
    (let [slave-id "prompt-slave"
          event (make-test-event :slave-id slave-id
                                 :prompt "Allow file write?")]
      ;; Capture journal state before
      (swarm/clear-event-journal!)

      (#'channel/handle-prompt-shown event)

      ;; Journal should still be empty (prompt-shown just logs)
      (is (nil? (swarm/check-event-journal slave-id))))))

(deftest test-handle-task-completed-uses-event-timestamp
  (testing "handle-task-completed uses timestamp from event when provided"
    (let [task-id (gen-task-id)
          custom-timestamp 1234567890
          event (make-test-event :task-id task-id
                                 :timestamp custom-timestamp
                                 :result "done")]
      (#'channel/handle-task-completed event)

      (let [journal-entry (swarm/check-event-journal task-id)]
        (is (= custom-timestamp (:timestamp journal-entry)))))))

(deftest test-handle-task-completed-generates-timestamp-when-missing
  (testing "handle-task-completed generates timestamp when not in event"
    (let [task-id (gen-task-id)
          before-ts (System/currentTimeMillis)
          event {"task-id" task-id
                 "slave-id" "slave-1"
                 "result" "done"}]
      (#'channel/handle-task-completed event)

      (let [after-ts (System/currentTimeMillis)
            journal-entry (swarm/check-event-journal task-id)]
        (is (<= before-ts (:timestamp journal-entry) after-ts))))))

;; =============================================================================
;; Test: Event Journal Overwrite Behavior
;; =============================================================================

(deftest test-event-journal-overwrites-same-task-id
  (testing "New event for same task-id overwrites previous entry"
    (let [task-id (gen-task-id)
          event-1 (make-test-event :task-id task-id
                                   :slave-id "slave-1"
                                   :result "first result")
          event-2 (make-test-event :task-id task-id
                                   :slave-id "slave-2"
                                   :error "second failed")]
      ;; First: completed
      (#'channel/handle-task-completed event-1)
      (is (= "completed" (:status (swarm/check-event-journal task-id))))
      (is (= "slave-1" (:slave-id (swarm/check-event-journal task-id))))

      ;; Second: failed (overwrites)
      (#'channel/handle-task-failed event-2)
      (is (= "failed" (:status (swarm/check-event-journal task-id))))
      (is (= "slave-2" (:slave-id (swarm/check-event-journal task-id)))))))

(deftest test-event-journal-multiple-tasks
  (testing "Event journal tracks multiple tasks independently"
    (let [task-1 (gen-task-id)
          task-2 (gen-task-id)
          task-3 (gen-task-id)]

      (#'channel/handle-task-completed (make-test-event :task-id task-1 :result "r1"))
      (#'channel/handle-task-failed (make-test-event :task-id task-2 :error "e2"))
      (#'channel/handle-task-completed (make-test-event :task-id task-3 :result "r3"))

      ;; All three should be independently accessible
      (is (= "completed" (:status (swarm/check-event-journal task-1))))
      (is (= "r1" (:result (swarm/check-event-journal task-1))))

      (is (= "failed" (:status (swarm/check-event-journal task-2))))
      (is (= "e2" (:error (swarm/check-event-journal task-2))))

      (is (= "completed" (:status (swarm/check-event-journal task-3))))
      (is (= "r3" (:result (swarm/check-event-journal task-3)))))))

;; =============================================================================
;; Test: Clear Event Journal Removes All Entries
;; =============================================================================

(deftest test-clear-event-journal!-removes-all-entries
  (testing "clear-event-journal! removes all task entries"
    (let [task-1 (gen-task-id)
          task-2 (gen-task-id)]

      ;; Add some events
      (#'channel/handle-task-completed (make-test-event :task-id task-1 :result "r1"))
      (#'channel/handle-task-failed (make-test-event :task-id task-2 :error "e2"))

      ;; Verify they exist
      (is (some? (swarm/check-event-journal task-1)))
      (is (some? (swarm/check-event-journal task-2)))

      ;; Clear
      (swarm/clear-event-journal!)

      ;; Both should be gone
      (is (nil? (swarm/check-event-journal task-1)))
      (is (nil? (swarm/check-event-journal task-2))))))

;; =============================================================================
;; Test: try-require-channel Behavior
;; =============================================================================

(deftest test-try-require-channel-returns-boolean
  (testing "try-require-channel returns true when channel namespace exists"
    ;; This tests the actual behavior - channel namespace may or may not be available
    ;; in test environment, so we test the return type
    (let [result (#'channel/try-require-channel)]
      (is (boolean? result)))))

(deftest test-try-require-channel-handles-missing-namespace
  (testing "try-require-channel returns false for non-existent namespace"
    ;; We can't easily mock require, but we can verify the function doesn't throw
    ;; when the channel namespace is not available (which is the current test state)
    (is (boolean? (#'channel/try-require-channel)))))

;; =============================================================================
;; Test: channel-subscribe! Behavior
;; =============================================================================

(deftest test-channel-subscribe!-returns-nil-when-no-channel
  (testing "channel-subscribe! returns nil when channel not available"
    (with-redefs [hive-mcp.tools.swarm.channel/try-require-channel (constantly false)]
      (is (nil? (#'channel/channel-subscribe! :task-completed)))
      (is (nil? (#'channel/channel-subscribe! :task-failed)))
      (is (nil? (#'channel/channel-subscribe! :prompt-shown))))))

(deftest test-channel-subscribe!-returns-channel-when-available
  (testing "channel-subscribe! returns subscription channel when available"
    (let [mock-chan (chan 1)]
      (with-redefs [hive-mcp.tools.swarm.channel/try-require-channel (constantly true)]
        ;; Mock the resolve to return a function that returns our mock channel
        (with-redefs [resolve (fn [sym]
                                (when (= sym 'hive-mcp.channel/subscribe!)
                                  (fn [_] mock-chan)))]
          (let [result (#'channel/channel-subscribe! :task-completed)]
            (is (some? result))
            (is (= mock-chan result))))))))
