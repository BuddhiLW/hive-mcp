(ns hive-mcp.workflows.complete-session-test
  "Tests for the complete session FSM workflow.

   Tests the FSM state machine behavior with mock resources,
   verifying state transitions, handler logic, and error paths."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.workflows.complete-session :as complete]))

;; =============================================================================
;; Test Resources (mock side-effect functions)
;; =============================================================================

(defn make-test-resources
  "Create mock resources for testing.
   Resource keys match the handler docstrings:
     :validate-fn       -- (fn [data] -> nil|{:error str})
     :merge-task-ids-fn -- (fn [task-ids agent-id] -> [string])
     :git-commit-fn     -- (fn [message cwd] -> {:success bool})
     :kanban-done-fn    -- (fn [task-ids directory] -> {:moved N})
     :crystallize-fn    -- (fn [agent-id directory] -> {:summary-id str, ...})
     :shout-fn          -- (fn [agent-id event-type message] -> nil)
     :plan-check-fn     -- (fn [agent-id directory] -> {:triggered? bool}|nil)
     :evict-fn          -- (fn [agent-id] -> {:evicted N})"
  ([] (make-test-resources {}))
  ([overrides]
   (merge
    {:git-commit-fn     (fn [_msg _cwd] {:success true})
     :kanban-done-fn    (fn [task-ids _dir] {:moved (count task-ids)})
     :crystallize-fn    (fn [_aid _dir] {:summary-id "sum-456" :stats {:promoted 1}})
     :shout-fn          (fn [& _args] nil)
     :plan-check-fn     (fn [_aid _dir] nil)
     :evict-fn          (fn [_aid] {:evicted 2})
     :directory         "/default/project"}
    overrides)))

;; =============================================================================
;; Happy Path Tests
;; =============================================================================

(deftest test-complete-session-happy-path
  (testing "Full complete session with commit + kanban + crystallize"
    (let [resources (make-test-resources)
          result (complete/run-session-complete
                  resources
                  {:commit-msg "feat: implement feature X"
                   :task-ids ["kb-1" "kb-2"]
                   :agent-id "ling-test-456"
                   :directory "/test/project"})]
      (is (= :ok (:status result)))
      (is (= "ling-test-456" (:agent-id result)))
      (is (= "feat: implement feature X" (:commit-msg result)))
      (is (= 2 (:tasks-completed result)))
      (is (= true (get-in result [:commit-result :success])))
      (is (= 2 (get-in result [:kanban-result :moved])))
      (is (= "sum-456" (get-in result [:crystal-result :summary-id])))
      (is (true? (:shout-sent? result)))
      (is (= 2 (get-in result [:eviction :evicted]))))))

(deftest test-complete-session-no-tasks
  (testing "Complete session without kanban tasks skips kanban phase entirely"
    (let [kanban-called (atom false)
          resources (make-test-resources
                     {:kanban-done-fn (fn [_ids _dir]
                                        (reset! kanban-called true)
                                        {:moved 0})})
          result (complete/run-session-complete
                  resources
                  {:commit-msg "feat: simple change"
                   :agent-id "ling-test"
                   :directory "/test"})]
      (is (= :ok (:status result)))
      ;; FSM dispatches from ::commit directly to ::crystallize via no-tasks? predicate
      ;; so the ::kanban handler is never invoked
      (is (false? @kanban-called))
      ;; kanban-result is nil because the kanban state was never entered
      (is (nil? (:kanban-result result))))))

(deftest test-complete-session-auto-merges-task-ids
  (testing "merge-task-ids-fn is called to auto-merge ling's linked task"
    (let [resources (make-test-resources
                     {:merge-task-ids-fn (fn [task-ids _agent-id]
                                           (conj (vec task-ids) "kb-auto-1"))})
          result (complete/run-session-complete
                  resources
                  {:commit-msg "feat: with auto-task"
                   :task-ids ["kb-explicit-1"]
                   :agent-id "ling-test"
                   :directory "/test"})]
      (is (= :ok (:status result)))
      (is (= 2 (:tasks-completed result)))
      (is (= ["kb-explicit-1" "kb-auto-1"] (:task-ids result))))))

(deftest test-complete-session-no-duplicate-task-ids
  (testing "merge-task-ids-fn can deduplicate task IDs"
    (let [resources (make-test-resources
                     {:merge-task-ids-fn (fn [task-ids _agent-id]
                                           ;; Simulate merging "kb-1" but it's already there
                                           (vec (distinct (conj (vec task-ids) "kb-1"))))})
          result (complete/run-session-complete
                  resources
                  {:commit-msg "feat: already has it"
                   :task-ids ["kb-1" "kb-2"]
                   :agent-id "ling-test"
                   :directory "/test"})]
      (is (= :ok (:status result)))
      (is (= 2 (:tasks-completed result)))
      ;; kb-1 should not be duplicated
      (is (= ["kb-1" "kb-2"] (:task-ids result))))))

;; =============================================================================
;; Validation Error Tests
;; =============================================================================

(deftest test-complete-session-missing-commit-msg
  (testing "Missing commit_msg causes validation error (throws)"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Session complete workflow error"
                          (complete/run-session-complete
                           (make-test-resources)
                           {:agent-id "ling-test"
                            :directory "/test"})))))

(deftest test-complete-session-empty-commit-msg
  (testing "Empty commit_msg causes validation error (throws)"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Session complete workflow error"
                          (complete/run-session-complete
                           (make-test-resources)
                           {:commit-msg "   "
                            :agent-id "ling-test"
                            :directory "/test"})))))

;; =============================================================================
;; Plan Check Tests
;; =============================================================================

(deftest test-complete-session-plan-check-triggered
  (testing "Plan-to-kanban triggered for explorer lings"
    (let [resources (make-test-resources
                     {:plan-check-fn (fn [_aid _dir]
                                       {:triggered? true :plan-id "plan-789"})})
          result (complete/run-session-complete
                  resources
                  {:commit-msg "feat: explorer done"
                   :agent-id "explorer-ling"
                   :directory "/test"})]
      (is (= :ok (:status result)))
      (is (= true (get-in result [:plan-result :triggered?])))
      (is (= "plan-789" (get-in result [:plan-result :plan-id]))))))

(deftest test-complete-session-plan-check-not-triggered
  (testing "Plan-to-kanban not triggered for non-explorer lings"
    (let [resources (make-test-resources
                     {:plan-check-fn (fn [_aid _dir] nil)})
          result (complete/run-session-complete
                  resources
                  {:commit-msg "feat: regular ling"
                   :agent-id "regular-ling"
                   :directory "/test"})]
      (is (= :ok (:status result)))
      (is (nil? (:plan-result result))))))

;; =============================================================================
;; Compilation Tests
;; =============================================================================

(deftest test-complete-session-compile-idempotent
  (testing "compile-complete produces a reusable compiled FSM"
    (let [compiled (complete/compile-complete)
          resources (make-test-resources)]
      (let [r1 (complete/run-complete compiled resources
                                      {:commit-msg "feat: first"
                                       :agent-id "ling-1"
                                       :directory "/test"})
            r2 (complete/run-complete compiled resources
                                      {:commit-msg "feat: second"
                                       :agent-id "ling-2"
                                       :directory "/test"})]
        (is (= "ling-1" (:agent-id r1)))
        (is (= "ling-2" (:agent-id r2)))
        (is (= :ok (:status r1)))
        (is (= :ok (:status r2)))))))

;; =============================================================================
;; Handler Unit Tests
;; =============================================================================

(deftest test-handle-start-validation
  (testing "handle-start validates commit-msg"
    (let [result (complete/handle-start {} {:agent-id "test"})]
      (is (some? (:error result)))
      (is (re-find #"commit_msg" (str (:error result)))))))

(deftest test-handle-start-merges-task-ids
  (testing "handle-start calls merge-task-ids-fn"
    (let [resources {:merge-task-ids-fn (fn [ids _] (conj (vec ids) "kb-linked"))}
          result (complete/handle-start resources
                                        {:commit-msg "feat: test"
                                         :task-ids ["kb-1"]
                                         :agent-id "ling-x"})]
      (is (= ["kb-1" "kb-linked"] (:task-ids result))))))

(deftest test-handle-commit-success
  (testing "handle-commit calls git-commit-fn"
    (let [committed-msg (atom nil)
          resources {:git-commit-fn (fn [msg _cwd]
                                      (reset! committed-msg msg)
                                      {:success true})}
          result (complete/handle-commit resources
                                         {:commit-msg "feat: test"
                                          :directory "/test"})]
      (is (= "feat: test" @committed-msg))
      (is (= true (get-in result [:commit-result :success]))))))

(deftest test-handle-commit-no-fn
  (testing "handle-commit skips gracefully when no git-commit-fn"
    (let [result (complete/handle-commit {} {:commit-msg "test"})]
      (is (true? (get-in result [:commit-result :skipped]))))))

(deftest test-handle-kanban-with-tasks
  (testing "handle-kanban calls kanban-done-fn with task-ids"
    (let [resources {:kanban-done-fn (fn [ids _dir] {:moved (count ids)})}
          result (complete/handle-kanban resources
                                         {:task-ids ["t1" "t2"]
                                          :directory "/test"})]
      (is (= 2 (get-in result [:kanban-result :moved]))))))

(deftest test-handle-kanban-no-tasks
  (testing "handle-kanban skips when no task-ids"
    (let [result (complete/handle-kanban {} {:task-ids [] :directory "/test"})]
      (is (= 0 (get-in result [:kanban-result :moved])))
      (is (true? (get-in result [:kanban-result :skipped]))))))

(deftest test-handle-crystallize-success
  (testing "handle-crystallize calls crystallize-fn with agent-id and directory"
    (let [called-with (atom nil)
          resources {:crystallize-fn (fn [aid dir]
                                       (reset! called-with [aid dir])
                                       {:summary-id "sum-1"})}
          result (complete/handle-crystallize resources
                                              {:agent-id "ling-x"
                                               :directory "/test"})]
      (is (= ["ling-x" "/test"] @called-with))
      (is (= "sum-1" (get-in result [:crystal-result :summary-id]))))))

(deftest test-handle-shout-sends
  (testing "handle-shout calls shout-fn and sets shout-sent?"
    (let [shouted (atom nil)
          resources {:shout-fn (fn [aid event-type msg]
                                  (reset! shouted {:aid aid :type event-type :msg msg}))}
          result (complete/handle-shout resources
                                        {:agent-id "ling-x"
                                         :commit-msg "feat: stuff"})]
      (is (true? (:shout-sent? result)))
      (is (= "ling-x" (:aid @shouted)))
      (is (= :completed (:type @shouted))))))

(deftest test-dispatch-predicates
  (testing "valid? requires commit-msg and agent-id and no error"
    (is (complete/valid? {:commit-msg "x" :agent-id "y"}))
    (is (not (complete/valid? {:agent-id "y"})))
    (is (not (complete/valid? {:commit-msg "x"})))
    (is (not (complete/valid? {:commit-msg "x" :agent-id "y" :error "bad"}))))
  (testing "invalid? checks for error"
    (is (complete/invalid? {:error "bad"}))
    (is (not (complete/invalid? {}))))
  (testing "has-tasks? and no-tasks?"
    (is (complete/has-tasks? {:task-ids ["a"]}))
    (is (not (complete/has-tasks? {:task-ids []})))
    (is (complete/no-tasks? {:task-ids []}))
    (is (not (complete/no-tasks? {:task-ids ["a"]})))))
