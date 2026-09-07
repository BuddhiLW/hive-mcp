(ns hive-mcp.swarm-channel-test
  "Push-based swarm event integration: an event emitted on the channel bus
   lands in the event journal through the live core.async subscriptions.

   Runs against the in-process bus only — no socket server — and waits on a
   deadline instead of fixed sleeps, so it holds regardless of suite order."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.tools.swarm.channel :as swarm-channel]
            [hive-mcp.channel.core :as ch]
            [hive-dsl.bounded-atom :refer [bput!]]))

;; =============================================================================
;; Fixtures + helpers
;; =============================================================================

(defn with-clean-state
  "Fresh journal and no live subscriptions before and after each test."
  [f]
  (swarm/stop-channel-subscriptions!)
  (swarm/clear-event-journal!)
  (try (f)
       (finally
         (swarm/stop-channel-subscriptions!)
         (swarm/clear-event-journal!))))

(use-fixtures :each with-clean-state)

(def ^:private await-ms 2000)

(defn- await-journal
  "Poll the journal for task-id until an entry lands or the deadline passes.
   Returns the entry, or nil on timeout."
  [task-id]
  (let [deadline (+ (System/currentTimeMillis) await-ms)]
    (loop []
      (or (swarm/check-event-journal task-id)
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 10)
            (recur))))))

(defn- fresh-task-id
  "A task id no other test namespace can collide with in the shared journal."
  []
  (str "swarm-channel-test-" (random-uuid)))

;; =============================================================================
;; Event journal
;; =============================================================================

(deftest event-journal-empty-test
  (testing "Event journal starts empty"
    (is (nil? (swarm/check-event-journal (fresh-task-id))))))

(deftest event-journal-clear-test
  (testing "Event journal can be cleared"
    (let [task-id (fresh-task-id)]
      ;; Manually add an entry via the private atom in swarm.channel
      (bput! @#'swarm-channel/event-journal task-id {:status "completed"})
      (is (some? (swarm/check-event-journal task-id)))
      (swarm/clear-event-journal!)
      (is (nil? (swarm/check-event-journal task-id))))))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(deftest channel-subscriptions-start-stop-test
  (testing "Channel subscriptions can start and stop"
    (swarm/start-channel-subscriptions!)
    ;; Subscriptions are registered synchronously in the bounded-atom
    (is (seq @(:atom @#'swarm-channel/channel-subscriptions)))

    (swarm/stop-channel-subscriptions!)
    (is (empty? @(:atom @#'swarm-channel/channel-subscriptions)))))

;; =============================================================================
;; Events → journal (through the real pub/sub)
;; =============================================================================

(deftest task-completed-event-updates-journal-test
  (testing "task-completed event updates event journal"
    (let [task-id (fresh-task-id)]
      (swarm/start-channel-subscriptions!)

      ;; Emit a task-completed event on the in-process bus
      (ch/emit-event! :task-completed
                      {:task-id task-id
                       :slave-id "test-slave"
                       :result "success!"})

      (let [entry (await-journal task-id)]
        (is (some? entry) "event never reached the journal")
        (is (= "completed" (:status entry)))
        (is (= "success!" (:result entry)))
        (is (= "test-slave" (:slave-id entry)))))))

(deftest task-failed-event-updates-journal-test
  (testing "task-failed event updates event journal"
    (let [task-id (fresh-task-id)]
      (swarm/start-channel-subscriptions!)

      ;; Emit a task-failed event on the in-process bus
      (ch/emit-event! :task-failed
                      {:task-id task-id
                       :slave-id "test-slave"
                       :error "Something went wrong"})

      (let [entry (await-journal task-id)]
        (is (some? entry) "event never reached the journal")
        (is (= "failed" (:status entry)))
        (is (= "Something went wrong" (:error entry)))))))

;; =============================================================================
;; Collect path
;; =============================================================================

(deftest collect-finds-journal-entry-immediately-test
  (testing "handle-swarm-collect finds journal entry without polling"
    (let [task-id (fresh-task-id)]
      ;; Pre-populate the journal (simulating event arrival)
      (bput! @#'swarm-channel/event-journal task-id
             {:status "completed"
              :result "instant result"
              :slave-id "fast-slave"
              :timestamp (System/currentTimeMillis)})

      ;; Note: We can't fully test handle-swarm-collect without emacs running,
      ;; but we can verify the journal lookup path works
      (let [entry (swarm/check-event-journal task-id)]
        (is (= "completed" (:status entry)))
        (is (= "instant result" (:result entry)))))))
