(ns hive-mcp.hooks.events-test
  "Tests for hooks event constructors.

   TDD: These tests define the expected behavior for event constructors
   before implementation."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [hive-mcp.hooks.events :as events]
            [hive-mcp.specs.hooks :as specs]))

;; =============================================================================
;; Event Constructor Tests
;; =============================================================================

(deftest task-start-event-test
  (testing "creates valid task-start event"
    (let [event (events/task-start-event
                 {:task "Implement feature X"
                  :slave-id "agent-1"})]
      (is (= :task-start (:event event)))
      (is (= "Implement feature X" (:task event)))
      (is (= "agent-1" (:slave-id event)))
      (is (inst? (:timestamp event)))))

  (testing "validates required fields"
    (is (thrown? clojure.lang.ExceptionInfo
                 (events/task-start-event {})))))

(deftest task-complete-event-test
  (testing "creates valid task-complete event"
    (let [event (events/task-complete-event
                 {:task "Implement feature X"
                  :slave-id "agent-1"
                  :files ["src/foo.clj" "test/foo_test.clj"]})]
      (is (= :task-complete (:event event)))
      (is (= "Implement feature X" (:task event)))
      (is (= "agent-1" (:slave-id event)))
      (is (= ["src/foo.clj" "test/foo_test.clj"] (:files event)))
      (is (inst? (:timestamp event)))))

  (testing "allows optional message"
    (let [event (events/task-complete-event
                 {:task "Task"
                  :slave-id "agent-1"
                  :message "All tests passing"})]
      (is (= "All tests passing" (:message event))))))

(deftest session-start-event-test
  (testing "creates valid session-start event"
    (let [event (events/session-start-event
                 {:slave-id "session-main"})]
      (is (= :session-start (:event event)))
      (is (= "session-main" (:slave-id event)))
      (is (inst? (:timestamp event)))))

  (testing "allows project context"
    (let [event (events/session-start-event
                 {:slave-id "session-main"
                  :data {:project "hive-mcp"
                         :branch "feat/hooks"}})]
      (is (= {:project "hive-mcp" :branch "feat/hooks"} (:data event))))))

(deftest session-end-event-test
  (testing "creates valid session-end event"
    (let [event (events/session-end-event
                 {:slave-id "session-main"})]
      (is (= :session-end (:event event)))
      (is (= "session-main" (:slave-id event)))
      (is (inst? (:timestamp event)))))

  (testing "allows wrap-completed flag"
    (let [event (events/session-end-event
                 {:slave-id "session-main"
                  :data {:wrap-completed true
                         :summary-id "mem-123"}})]
      (is (true? (get-in event [:data :wrap-completed]))))))

(deftest file-modified-event-test
  (testing "creates valid file-modified event"
    (let [event (events/file-modified-event
                 {:files ["src/core.clj"]
                  :slave-id "agent-1"})]
      (is (= :file-modified (:event event)))
      (is (= ["src/core.clj"] (:files event)))
      (is (= "agent-1" (:slave-id event)))))

  (testing "validates files is non-empty"
    (is (thrown? clojure.lang.ExceptionInfo
                 (events/file-modified-event {:files []})))))

(deftest error-event-test
  (testing "creates valid error event"
    (let [event (events/error-event
                 {:error "Connection timeout"
                  :slave-id "agent-1"
                  :task "Fetch data"})]
      (is (= :error (:event event)))
      (is (= "Connection timeout" (:error event)))
      (is (= "agent-1" (:slave-id event)))
      (is (= "Fetch data" (:task event)))))

  (testing "accepts exception as error"
    (let [ex (ex-info "Test error" {:type :test})
          event (events/error-event
                 {:error ex
                  :slave-id "agent-1"})]
      (is (= ex (:error event)))))

  (testing "accepts map as error"
    (let [err-map {:code 500 :message "Internal error"}
          event (events/error-event
                 {:error err-map
                  :slave-id "agent-1"})]
      (is (= err-map (:error event))))))

(deftest ling-spawn-event-test
  (testing "creates valid ling-spawn event"
    (let [event (events/ling-spawn-event
                 {:slave-id "ling-tdd-1"
                  :data {:presets ["tdd"]
                         :cwd "/home/user/project"}})]
      (is (= :ling-spawn (:event event)))
      (is (= "ling-tdd-1" (:slave-id event)))
      (is (= ["tdd"] (get-in event [:data :presets]))))))

(deftest ling-terminate-event-test
  (testing "creates valid ling-terminate event"
    (let [event (events/ling-terminate-event
                 {:slave-id "ling-tdd-1"})]
      (is (= :ling-terminate (:event event)))
      (is (= "ling-tdd-1" (:slave-id event)))))

  (testing "allows reason"
    (let [event (events/ling-terminate-event
                 {:slave-id "ling-tdd-1"
                  :message "Task completed"})]
      (is (= "Task completed" (:message event))))))

;; =============================================================================
;; Spec Validation Tests
;; =============================================================================

(deftest events-conform-to-specs
  (testing "all event constructors produce spec-valid contexts"
    (let [events [(events/task-start-event {:task "T" :slave-id "s1"})
                  (events/task-complete-event {:task "T" :slave-id "s1"})
                  (events/session-start-event {:slave-id "s1"})
                  (events/session-end-event {:slave-id "s1"})
                  (events/file-modified-event {:files ["f.clj"] :slave-id "s1"})
                  (events/error-event {:error "err" :slave-id "s1"})
                  (events/ling-spawn-event {:slave-id "s1"})
                  (events/ling-terminate-event {:slave-id "s1"})]]
      (doseq [event events]
        (is (s/valid? ::specs/hook-context event)
            (str "Event should be valid: " (:event event)
                 "\n" (s/explain-str ::specs/hook-context event)))))))

;; =============================================================================
;; Event->Shout Payload Conversion Tests
;; =============================================================================

(deftest event->shout-payload-test
  (testing "converts task-complete to shout payload"
    (let [event (events/task-complete-event
                 {:task "Feature done"
                  :slave-id "agent-1"
                  :files ["src/a.clj"]})
          payload (events/event->shout-payload event)]
      (is (= :task-complete (:hook-type payload)))
      (is (= ["src/a.clj"] (:files payload)))
      (is (string? (:message payload)))))

  (testing "converts session-end to shout payload with wrap data"
    (let [event (events/session-end-event
                 {:slave-id "session-main"
                  :data {:wrap-completed true}})
          payload (events/event->shout-payload event)]
      (is (= :session-end (:hook-type payload)))
      (is (true? (get-in payload [:data :wrap-completed])))))

  (testing "converts error to shout payload"
    (let [event (events/error-event
                 {:error "Timeout"
                  :slave-id "agent-1"
                  :task "Fetch"})
          payload (events/event->shout-payload event)]
      (is (= :error (:hook-type payload)))
      (is (string? (:message payload)))
      (is (str/includes? (:message payload) "Timeout")))))

(deftest shout-payload-conforms-to-spec
  (testing "converted payloads are spec-valid"
    (let [events [(events/task-complete-event {:task "T" :slave-id "s"})
                  (events/session-end-event {:slave-id "s"})
                  (events/error-event {:error "e" :slave-id "s"})]]
      (doseq [event events]
        (let [payload (events/event->shout-payload event)]
          (is (s/valid? ::specs/hook-payload payload)
              (str "Payload should be valid for: " (:event event))))))))

;; =============================================================================
;; Generic Event Constructor Test
;; =============================================================================

(deftest create-event-test
  (testing "generic constructor creates any event type"
    (let [event (events/create-event :task-start
                                     {:task "Generic task"
                                      :slave-id "agent-x"})]
      (is (= :task-start (:event event)))
      (is (inst? (:timestamp event)))))

  (testing "rejects invalid event types"
    (is (thrown? clojure.lang.ExceptionInfo
                 (events/create-event :invalid-event {})))))
