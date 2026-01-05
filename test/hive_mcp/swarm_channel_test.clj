(ns hive-mcp.swarm-channel-test
  "Tests for swarm push-based event integration with channel."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.channel :as ch]
            [clojure.core.async :as async :refer [<!! timeout alts!!]]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn with-clean-state [f]
  "Ensure clean state before and after each test."
  (swarm/clear-event-journal!)
  (swarm/stop-channel-subscriptions!)
  (ch/stop-server!)
  (Thread/sleep 100)
  (f)
  (swarm/stop-channel-subscriptions!)
  (ch/stop-server!)
  (swarm/clear-event-journal!)
  (Thread/sleep 100))

(use-fixtures :each with-clean-state)

;; =============================================================================
;; Event Journal Tests
;; =============================================================================

(deftest event-journal-empty-test
  (testing "Event journal starts empty"
    (is (nil? (swarm/check-event-journal "task-123")))))

(deftest event-journal-clear-test
  (testing "Event journal can be cleared"
    ;; Manually add an entry via the atom (double deref: var -> atom)
    (swap! @#'swarm/event-journal assoc "test-task" {:status "completed"})
    (is (some? (swarm/check-event-journal "test-task")))
    (swarm/clear-event-journal!)
    (is (nil? (swarm/check-event-journal "test-task")))))

;; =============================================================================
;; Channel Subscription Tests
;; =============================================================================

(deftest channel-subscriptions-start-stop-test
  (testing "Channel subscriptions can start and stop"
    ;; Start channel server first
    (ch/start-server! {:type :unix :path "/tmp/hive-mcp-swarm-test.sock"})
    (Thread/sleep 100)

    ;; Start subscriptions
    (swarm/start-channel-subscriptions!)
    (Thread/sleep 100)

    ;; Verify subscriptions are active (double deref: var -> atom -> value)
    (is (seq @@#'swarm/channel-subscriptions))

    ;; Stop subscriptions
    (swarm/stop-channel-subscriptions!)
    (is (empty? @@#'swarm/channel-subscriptions))))

;; =============================================================================
;; Push Event Integration Tests
;; =============================================================================

(deftest task-completed-event-updates-journal-test
  (testing "task-completed event updates event journal"
    ;; Start channel infrastructure
    (ch/start-server! {:type :unix :path "/tmp/hive-mcp-swarm-test2.sock"})
    (Thread/sleep 100)
    (swarm/start-channel-subscriptions!)
    (Thread/sleep 100)

    ;; Emit a task-completed event via the channel
    (ch/emit-event! :task-completed
                    {:task-id "test-task-001"
                     :slave-id "test-slave"
                     :result "success!"})

    ;; Wait for event to be processed
    (Thread/sleep 200)

    ;; Check the journal was updated
    (let [entry (swarm/check-event-journal "test-task-001")]
      (is (some? entry))
      (is (= "completed" (:status entry)))
      (is (= "success!" (:result entry)))
      (is (= "test-slave" (:slave-id entry))))))

(deftest task-failed-event-updates-journal-test
  (testing "task-failed event updates event journal"
    ;; Start channel infrastructure
    (ch/start-server! {:type :unix :path "/tmp/hive-mcp-swarm-test3.sock"})
    (Thread/sleep 100)
    (swarm/start-channel-subscriptions!)
    (Thread/sleep 100)

    ;; Emit a task-failed event
    (ch/emit-event! :task-failed
                    {:task-id "test-task-002"
                     :slave-id "test-slave"
                     :error "Something went wrong"})

    ;; Wait for event to be processed
    (Thread/sleep 200)

    ;; Check the journal was updated
    (let [entry (swarm/check-event-journal "test-task-002")]
      (is (some? entry))
      (is (= "failed" (:status entry)))
      (is (= "Something went wrong" (:error entry))))))

;; =============================================================================
;; Collect Push-First Fallback Tests
;; =============================================================================

(deftest collect-finds-journal-entry-immediately-test
  (testing "handle-swarm-collect finds journal entry without polling"
    ;; Pre-populate the journal (simulating event arrival)
    (swap! @#'swarm/event-journal assoc "instant-task"
           {:status "completed"
            :result "instant result"
            :slave-id "fast-slave"
            :timestamp (System/currentTimeMillis)})

    ;; Note: We can't fully test handle-swarm-collect without emacs running,
    ;; but we can verify the journal lookup path works
    (let [entry (swarm/check-event-journal "instant-task")]
      (is (= "completed" (:status entry)))
      (is (= "instant result" (:result entry))))))

(comment
  ;; Run tests
  (clojure.test/run-tests 'hive-mcp.swarm-channel-test)

  ;; Run single test
  (clojure.test/test-vars [#'task-completed-event-updates-journal-test]))
