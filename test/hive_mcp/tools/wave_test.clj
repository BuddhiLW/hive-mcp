(ns hive-mcp.tools.wave-test
  "Unit tests for dispatch_drone_wave functionality.

   Tests:
   1. Plan creation with items
   2. Item status updates
   3. Event handler dispatch
   4. MCP tool schema validation

   TDD approach: Tests verify core functionality without external deps."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.swarm.wave :as wave]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers :as handlers]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-db-fixture
  "Reset DataScript and event system before each test."
  [f]
  (ds/reset-conn!)
  (handlers/reset-registration!)
  (ev/reset-all!)
  (ev/init!)
  (handlers/register-handlers!)
  (f)
  (ds/reset-conn!))

(use-fixtures :each reset-db-fixture)

;; =============================================================================
;; Plan Creation Tests
;; =============================================================================

(deftest create-plan-test
  (testing "creates plan with items"
    (let [tasks [{:file "a.clj" :task "task a"}
                 {:file "b.clj" :task "task b"}]
          plan-id (wave/create-plan! tasks "drone-worker")]
      (is (string? plan-id))
      (is (.startsWith plan-id "plan-"))
      (is (= 2 (count (wave/get-pending-items plan-id)))))))

(deftest create-plan-with-default-preset-test
  (testing "creates plan with default preset when not specified"
    (let [tasks [{:file "test.clj" :task "test task"}]
          plan-id (wave/create-plan! tasks)]
      (is (string? plan-id))
      (let [plan (ds/get-plan plan-id)]
        (is (= "drone-worker" (:change-plan/preset plan)))))))

(deftest create-plan-with-custom-preset-test
  (testing "creates plan with custom preset"
    (let [tasks [{:file "test.clj" :task "test task"}]
          plan-id (wave/create-plan! tasks "tdd")]
      (let [plan (ds/get-plan plan-id)]
        (is (= "tdd" (:change-plan/preset plan)))))))

;; =============================================================================
;; Item Status Tests
;; =============================================================================

(deftest get-pending-items-test
  (testing "returns only pending items"
    (let [tasks [{:file "a.clj" :task "task a"}
                 {:file "b.clj" :task "task b"}]
          plan-id (wave/create-plan! tasks)
          items (wave/get-pending-items plan-id)]
      (is (= 2 (count items)))
      (is (every? #(= :pending (:change-item/status %)) items)))))

(deftest update-item-status-test
  (testing "updates item status correctly"
    (let [tasks [{:file "a.clj" :task "task a"}]
          plan-id (wave/create-plan! tasks)
          item (first (wave/get-pending-items plan-id))
          item-id (:change-item/id item)]
      ;; Update to dispatched
      (ds/update-item-status! item-id :dispatched)
      (is (= 0 (count (wave/get-pending-items plan-id))))

      ;; Verify all items shows updated status
      (let [all-items (wave/get-plan-items plan-id)]
        (is (= 1 (count all-items)))
        (is (= :dispatched (:change-item/status (first all-items))))))))

(deftest item-completion-test
  (testing "marks item as completed with result"
    (let [tasks [{:file "a.clj" :task "task a"}]
          plan-id (wave/create-plan! tasks)
          item (first (wave/get-pending-items plan-id))
          item-id (:change-item/id item)]
      (ds/update-item-status! item-id :completed {:result "success"})
      (let [items (wave/get-plan-items plan-id)
            updated-item (first items)]
        (is (= :completed (:change-item/status updated-item)))
        (is (= "success" (:change-item/result updated-item)))))))

;; =============================================================================
;; Wave Creation Tests
;; =============================================================================

(deftest create-wave-test
  (testing "creates wave for plan"
    (let [tasks [{:file "a.clj" :task "task a"}]
          plan-id (wave/create-plan! tasks)
          wave-id (ds/create-wave! plan-id)]
      (is (string? wave-id))
      (is (.startsWith wave-id "wave-"))
      (let [wave (ds/get-wave wave-id)]
        (is (= :running (:wave/status wave)))
        (is (= 0 (:wave/completed-count wave)))
        (is (= 0 (:wave/failed-count wave)))))))

(deftest wave-concurrency-test
  (testing "creates wave with custom concurrency"
    (let [tasks [{:file "a.clj" :task "task a"}]
          plan-id (wave/create-plan! tasks)
          wave-id (ds/create-wave! plan-id {:concurrency 5})]
      (let [wave (ds/get-wave wave-id)]
        (is (= 5 (:wave/concurrency wave)))))))

(deftest update-wave-counts-test
  (testing "updates wave counts correctly"
    (let [tasks [{:file "a.clj" :task "task a"}
                 {:file "b.clj" :task "task b"}]
          plan-id (wave/create-plan! tasks)
          wave-id (ds/create-wave! plan-id)]
      ;; Simulate item completion
      (ds/update-wave-counts! wave-id {:completed 1})
      (let [wave (ds/get-wave wave-id)]
        (is (= 1 (:wave/completed-count wave))))

      ;; Simulate another completion
      (ds/update-wave-counts! wave-id {:completed 1})
      (let [wave (ds/get-wave wave-id)]
        (is (= 2 (:wave/completed-count wave)))))))

(deftest complete-wave-test
  (testing "marks wave as completed"
    (let [tasks [{:file "a.clj" :task "task a"}]
          plan-id (wave/create-plan! tasks)
          wave-id (ds/create-wave! plan-id)]
      (ds/complete-wave! wave-id :completed)
      (let [wave (ds/get-wave wave-id)]
        (is (= :completed (:wave/status wave)))
        (is (some? (:wave/completed-at wave)))))))

(deftest complete-wave-partial-failure-test
  (testing "marks wave as partial-failure"
    (let [tasks [{:file "a.clj" :task "task a"}]
          plan-id (wave/create-plan! tasks)
          wave-id (ds/create-wave! plan-id)]
      (ds/complete-wave! wave-id :partial-failure)
      (let [wave (ds/get-wave wave-id)]
        (is (= :partial-failure (:wave/status wave)))))))

;; =============================================================================
;; Event Handler Tests
;; =============================================================================

(deftest wave-start-event-handler-test
  (testing ":wave/start event produces correct effects"
    (let [captured (atom nil)]
      ;; Register test effect to capture channel-publish
      (ev/reg-fx :channel-publish (fn [data] (reset! captured data)))

      ;; Dispatch event
      (ev/dispatch [:wave/start {:plan-id "plan-123"
                                 :wave-id "wave-456"
                                 :item-count 3}])

      ;; Verify effects
      (is (= :wave-started (:event @captured)))
      (is (= "plan-123" (get-in @captured [:data :plan-id])))
      (is (= "wave-456" (get-in @captured [:data :wave-id])))
      (is (= 3 (get-in @captured [:data :item-count]))))))

(deftest wave-item-done-event-handler-test
  (testing ":wave/item-done event produces correct effects"
    (let [captured (atom nil)]
      (ev/reg-fx :channel-publish (fn [data] (reset! captured data)))

      (ev/dispatch [:wave/item-done {:item-id "item-789"
                                     :status :completed
                                     :wave-id "wave-456"}])

      (is (= :wave-item-done (:event @captured)))
      (is (= "item-789" (get-in @captured [:data :item-id])))
      (is (= :completed (get-in @captured [:data :status]))))))

(deftest wave-complete-event-handler-test
  (testing ":wave/complete event produces correct effects"
    (let [captured (atom nil)]
      (ev/reg-fx :channel-publish (fn [data] (reset! captured data)))

      (ev/dispatch [:wave/complete {:plan-id "plan-123"
                                    :wave-id "wave-456"
                                    :results {:completed 5 :failed 1}}])

      (is (= :wave-complete (:event @captured)))
      (is (= 5 (get-in @captured [:data :results :completed])))
      (is (= 1 (get-in @captured [:data :results :failed]))))))

;; =============================================================================
;; MCP Handler Tests
;; =============================================================================

(deftest handle-dispatch-drone-wave-empty-tasks-test
  (testing "returns error for empty tasks"
    (let [result (wave/handle-dispatch-drone-wave {:tasks []})]
      (is (= "text" (:type result)))
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (contains? parsed :error))))))

(deftest handle-dispatch-drone-wave-creates-plan-test
  (testing "creates plan and returns wave-id"
    ;; Mock execute-wave! to avoid async execution in tests
    (with-redefs [wave/execute-wave! (fn [plan-id _opts]
                                       (ds/create-wave! plan-id))]
      (let [result (wave/handle-dispatch-drone-wave
                    {:tasks [{"file" "a.clj" "task" "fix bug"}
                             {"file" "b.clj" "task" "add test"}]})]
        (is (= "text" (:type result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "wave_started" (:status parsed)))
          (is (string? (:plan_id parsed)))
          (is (string? (:wave_id parsed)))
          (is (= 2 (:item_count parsed))))))))

(deftest handle-dispatch-drone-wave-custom-preset-test
  (testing "passes custom preset"
    (with-redefs [wave/execute-wave! (fn [plan-id _opts]
                                       (ds/create-wave! plan-id))]
      (let [result (wave/handle-dispatch-drone-wave
                    {:tasks [{"file" "a.clj" "task" "task"}]
                     :preset "tdd"})]
        (let [parsed (json/read-str (:text result) :key-fn keyword)
              plan (ds/get-plan (:plan_id parsed))]
          (is (= "tdd" (:change-plan/preset plan))))))))

;; =============================================================================
;; Status Query Tests
;; =============================================================================

(deftest get-wave-status-test
  (testing "returns wave status map"
    (let [tasks [{:file "a.clj" :task "task a"}]
          plan-id (wave/create-plan! tasks)
          wave-id (ds/create-wave! plan-id)]
      (let [status (wave/get-wave-status wave-id)]
        (is (= wave-id (:wave-id status)))
        (is (= plan-id (:plan-id status)))
        (is (= :running (:status status)))
        (is (= 0 (:completed-count status)))
        (is (= 0 (:failed-count status)))))))

(deftest get-plan-status-test
  (testing "returns plan status with items"
    (let [tasks [{:file "a.clj" :task "task a"}
                 {:file "b.clj" :task "task b"}]
          plan-id (wave/create-plan! tasks)]
      (let [status (wave/get-plan-status plan-id)]
        (is (= plan-id (:plan-id status)))
        (is (= :pending (:status status)))
        (is (= 2 (count (:items status))))))))
