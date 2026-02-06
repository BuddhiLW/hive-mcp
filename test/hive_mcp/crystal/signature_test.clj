(ns hive-mcp.crystal.signature-test
  "TDD tests for crystal/signature.clj - behavioral signature tracking.

   Tests both levels:
   1. Outcome Signatures (recall-level) - existing functionality
   2. Behavioral Signatures (agent-level) - new W1 functionality

   NOTE: Tests only cover L1/L2 schema + CRUD. L3+ analysis is in hive-knowledge."
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.crystal.signature :as sig]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn clean-stores-fixture
  "Clean all signature stores before each test to prevent cross-contamination."
  [f]
  ;; Clear outcome signature stores
  (sig/flush-completed-signatures!)
  ;; Clear all pending signatures by abandoning them
  (doseq [recall-id (keys (sig/get-pending-signatures))]
    (sig/abandon-signature recall-id "test-cleanup"))
  ;; Clear behavioral stores
  (sig/clear-all-behaviors!)
  (f))

(use-fixtures :each clean-stores-fixture)

;; =============================================================================
;; Outcome Signature Tests (recall-level - existing)
;; =============================================================================

(deftest start-signature-test
  (testing "start-signature returns a recall-id string"
    (let [recall-id (sig/start-signature ["entry-1" "entry-2"]
                                         {:session-id "2026-02-05"
                                          :project-id "hive-mcp"
                                          :source "agent"})]
      (is (string? recall-id))
      (is (.startsWith recall-id "rcl-"))))

  (testing "start-signature adds to pending buffer"
    ;; Note: previous testing block already added one signature in this deftest
    (let [count-before (count (sig/get-pending-signatures))
          _recall-id (sig/start-signature ["entry-1"]
                                          {:session-id "s1"
                                           :project-id "p1"
                                           :source "test"})
          count-after (count (sig/get-pending-signatures))]
      (is (= (inc count-before) count-after)))))

(deftest complete-signature-test
  (testing "complete-signature returns completed map with correct schema"
    (let [recall-id (sig/start-signature ["entry-1" "entry-2"]
                                         {:session-id "2026-02-05"
                                          :project-id "hive-mcp"
                                          :source "agent"})
          completed (sig/complete-signature recall-id
                                            {:task-completed? true
                                             :tools-used ["read_file" "file_write"]
                                             :user-correction? false})]
      (is (map? completed))
      (is (string? (:signature/id completed)))
      (is (.startsWith (:signature/id completed) "sig-"))
      (is (= recall-id (:signature/recall-id completed)))
      (is (= ["entry-1" "entry-2"] (:signature/entries-recalled completed)))
      (is (true? (get-in completed [:signature/outcome :task-completed?])))
      (is (false? (get-in completed [:signature/outcome :user-correction?])))
      (is (= ["read_file" "file_write"] (get-in completed [:signature/outcome :tools-used])))))

  (testing "complete-signature auto-computes elapsed time"
    (let [recall-id (sig/start-signature ["e1"] {:session-id "s" :project-id "p" :source "t"})
          _ (Thread/sleep 10) ;; small delay
          completed (sig/complete-signature recall-id {:task-completed? true})]
      (is (some? (get-in completed [:signature/outcome :time-to-completion-ms])))
      (is (pos? (get-in completed [:signature/outcome :time-to-completion-ms])))))

  (testing "complete-signature moves from pending to completed"
    (let [completed-before (count (sig/get-completed-signatures))
          recall-id (sig/start-signature ["e1"] {:session-id "s" :project-id "p" :source "t"})
          _ (sig/complete-signature recall-id {:task-completed? true})]
      (is (empty? (sig/get-pending-signatures)))
      (is (= (inc completed-before) (count (sig/get-completed-signatures))))))

  (testing "complete-signature returns nil for unknown recall-id"
    (is (nil? (sig/complete-signature "unknown-recall-id" {:task-completed? false})))))

(deftest abandon-signature-test
  (testing "abandon removes from pending buffer"
    (let [recall-id (sig/start-signature ["e1"] {:session-id "s" :project-id "p" :source "t"})]
      (is (true? (sig/abandon-signature recall-id "test")))
      (is (empty? (sig/get-pending-signatures)))))

  (testing "abandon returns false for unknown recall-id"
    (is (false? (sig/abandon-signature "unknown" "test")))))

(deftest flush-completed-signatures-test
  (testing "flush returns and clears completed signatures"
    (let [recall-id (sig/start-signature ["e1"] {:session-id "s" :project-id "p" :source "t"})
          _ (sig/complete-signature recall-id {:task-completed? true})
          flushed (sig/flush-completed-signatures!)]
      (is (= 1 (count flushed)))
      (is (empty? (sig/get-completed-signatures))))))

;; =============================================================================
;; Outcome Weight & Promotion Signal Tests
;; =============================================================================

(deftest calculate-outcome-weight-test
  (testing "user correction yields negative weight"
    (is (= -1.0 (sig/calculate-outcome-weight {:task-completed? true
                                               :user-correction? true}))))

  (testing "failed task yields zero weight"
    (is (= 0.0 (sig/calculate-outcome-weight {:task-completed? false
                                              :user-correction? false}))))

  (testing "successful task yields positive weight"
    (is (= 2.0 (sig/calculate-outcome-weight {:task-completed? true
                                              :user-correction? false
                                              :time-to-completion-ms 60000}))))

  (testing "fast completion gets speed bonus"
    (is (= 3.0 (sig/calculate-outcome-weight {:task-completed? true
                                              :user-correction? false
                                              :time-to-completion-ms 5000})))))

(deftest outcome->promotion-signal-test
  (testing "correction maps to behavioral-correction context"
    (let [signal (sig/outcome->promotion-signal {:task-completed? true
                                                 :user-correction? true})]
      (is (= :behavioral-correction (:context signal)))
      (is (= -2.0 (:weight signal)))))

  (testing "success maps to behavioral-success context"
    (let [signal (sig/outcome->promotion-signal {:task-completed? true
                                                 :user-correction? false
                                                 :time-to-completion-ms 60000})]
      (is (= :behavioral-success (:context signal)))
      (is (pos? (:weight signal)))))

  (testing "failure maps to behavioral-failure context"
    (let [signal (sig/outcome->promotion-signal {:task-completed? false
                                                 :user-correction? false})]
      (is (= :behavioral-failure (:context signal)))
      (is (= 0.0 (:weight signal))))))

;; =============================================================================
;; Aggregate & Promotion Adjustment Tests
;; =============================================================================

(deftest aggregate-entry-outcomes-test
  (testing "aggregates correctly for an entry across multiple signatures"
    (let [sigs [{:signature/entries-recalled ["e1" "e2"]
                 :signature/outcome {:task-completed? true :user-correction? false
                                     :time-to-completion-ms 1000}}
                {:signature/entries-recalled ["e1" "e3"]
                 :signature/outcome {:task-completed? false :user-correction? false
                                     :time-to-completion-ms 2000}}
                {:signature/entries-recalled ["e1"]
                 :signature/outcome {:task-completed? true :user-correction? true
                                     :time-to-completion-ms 3000}}]
          result (sig/aggregate-entry-outcomes "e1" sigs)]
      (is (= 3 (:recalls result)))
      (is (= 2 (:successes result)))
      (is (= 1 (:failures result)))
      (is (= 1 (:corrections result)))
      (is (= 2000 (:avg-completion-ms result)))))

  (testing "returns zero counts for unknown entry"
    (let [result (sig/aggregate-entry-outcomes "unknown" [])]
      (is (= 0 (:recalls result)))
      (is (= 0 (:successes result))))))

(deftest compute-promotion-adjustment-test
  (testing "not enough data returns 0.0"
    (is (= 0.0 (sig/compute-promotion-adjustment "e1" [])))
    (let [one-sig [{:signature/entries-recalled ["e1"]
                    :signature/outcome {:task-completed? true :user-correction? false}}]]
      (is (= 0.0 (sig/compute-promotion-adjustment "e1" one-sig)))))

  (testing "high correction rate returns negative"
    (let [sigs [{:signature/entries-recalled ["e1"]
                 :signature/outcome {:task-completed? true :user-correction? true}}
                {:signature/entries-recalled ["e1"]
                 :signature/outcome {:task-completed? true :user-correction? true}}
                {:signature/entries-recalled ["e1"]
                 :signature/outcome {:task-completed? true :user-correction? false}}]]
      (is (neg? (sig/compute-promotion-adjustment "e1" sigs)))))

  (testing "high effectiveness returns positive"
    (let [sigs [{:signature/entries-recalled ["e1"]
                 :signature/outcome {:task-completed? true :user-correction? false}}
                {:signature/entries-recalled ["e1"]
                 :signature/outcome {:task-completed? true :user-correction? false}}
                {:signature/entries-recalled ["e1"]
                 :signature/outcome {:task-completed? true :user-correction? false}}]]
      (is (pos? (sig/compute-promotion-adjustment "e1" sigs))))))

;; =============================================================================
;; Integration Hooks Tests
;; =============================================================================

(deftest on-memory-query-complete-test
  (testing "starts tracking when entry-ids are present"
    (let [recall-id (sig/on-memory-query-complete ["e1" "e2"]
                                                  {:session-id "s" :project-id "p"})]
      (is (string? recall-id))
      (is (= 1 (count (sig/get-pending-signatures))))))

  (testing "returns nil for empty entry-ids"
    (is (nil? (sig/on-memory-query-complete [] {})))))

(deftest on-task-complete-test
  (testing "completes signature by recall-id"
    (let [recall-id (sig/start-signature ["e1"] {:session-id "s" :project-id "p" :source "t"})
          result (sig/on-task-complete recall-id {:task-completed? true})]
      (is (map? result))
      (is (= 1 (count (sig/get-completed-signatures))))))

  (testing "completes most recent when recall-id is nil"
    (let [_r1 (sig/start-signature ["e1"] {:session-id "s" :project-id "p" :source "t"})
          result (sig/on-task-complete nil {:task-completed? false})]
      (is (map? result)))))

(deftest on-user-correction-test
  (testing "marks all pending as corrected"
    (let [_r1 (sig/start-signature ["e1"] {:session-id "s" :project-id "p" :source "t"})
          _r2 (sig/start-signature ["e2"] {:session-id "s" :project-id "p" :source "t"})
          count-marked (sig/on-user-correction)]
      (is (= 2 count-marked))
      (is (empty? (sig/get-pending-signatures)))
      (is (= 2 (count (sig/get-completed-signatures))))
      ;; All should be marked as user-correction
      (is (every? #(get-in % [:signature/outcome :user-correction?])
                  (sig/get-completed-signatures))))))

;; =============================================================================
;; Behavioral Signature Tests (Agent-Level - New W1)
;; =============================================================================

(deftest record-behavior-test
  (testing "records a tool-use event"
    (let [event (sig/record-behavior! "agent-1" :tool-use
                                      {:tool-name "read_file"
                                       :duration-ms 150
                                       :success? true}
                                      :project-id "hive-mcp"
                                      :session-id "2026-02-05")]
      (is (map? event))
      (is (string? (:event/id event)))
      (is (.startsWith (:event/id event) "bev-"))
      (is (= :tool-use (:event/type event)))
      (is (= "agent-1" (:event/agent-id event)))
      (is (= "hive-mcp" (:event/project-id event)))
      (is (= "2026-02-05" (:event/session-id event)))
      (is (inst? (:event/timestamp event)))
      (is (= "read_file" (get-in event [:event/data :tool-name])))))

  (testing "records a task-complete event"
    (let [event (sig/record-behavior! "agent-1" :task-complete
                                      {:task-id "task-42"
                                       :task-type "implementation"
                                       :tools-used ["read_file" "file_write"]
                                       :duration-ms 300000
                                       :error-count 0}
                                      :project-id "hive-mcp")]
      (is (= :task-complete (:event/type event)))
      (is (= "task-42" (get-in event [:event/data :task-id])))))

  (testing "records a task-fail event"
    (let [event (sig/record-behavior! "agent-1" :task-fail
                                      {:task-id "task-43"
                                       :task-type "review"
                                       :error-count 3})]
      (is (= :task-fail (:event/type event)))))

  (testing "records an error event"
    (let [event (sig/record-behavior! "agent-1" :error
                                      {:error-type :nrepl-connection
                                       :error-message "Connection refused"
                                       :tool-name "cider_eval"
                                       :recoverable? true})]
      (is (= :error (:event/type event)))
      (is (= :nrepl-connection (get-in event [:event/data :error-type])))))

  (testing "records session-start and session-end events"
    (let [start (sig/record-behavior! "agent-1" :session-start
                                      {:cwd "/home/user/project"}
                                      :session-id "2026-02-05")
          end (sig/record-behavior! "agent-1" :session-end
                                    {:duration-ms 1800000
                                     :tasks-completed 3}
                                    :session-id "2026-02-05")]
      (is (= :session-start (:event/type start)))
      (is (= :session-end (:event/type end)))))

  (testing "preconditions: requires string agent-id, keyword type, map data"
    (is (thrown? AssertionError
                 (sig/record-behavior! nil :tool-use {:tool-name "x"})))
    (is (thrown? AssertionError
                 (sig/record-behavior! "agent" "not-keyword" {:tool-name "x"})))
    (is (thrown? AssertionError
                 (sig/record-behavior! "agent" :tool-use "not-a-map")))))

(deftest record-behavior-fifo-eviction-test
  (testing "FIFO eviction keeps store bounded"
    ;; Record more than max-events-per-agent (500)
    (dotimes [i 510]
      (sig/record-behavior! "overflow-agent" :tool-use
                            {:tool-name (str "tool-" i)
                             :success? true}))
    (let [count (sig/get-behavior-event-count "overflow-agent")]
      ;; Should be capped at max-events-per-agent (500)
      (is (<= count 500))
      ;; Oldest events should be evicted
      (let [events (sig/query-behaviors "overflow-agent")
            tool-names (set (map #(get-in % [:event/data :tool-name]) events))]
        ;; tool-0 through tool-9 should be evicted (oldest)
        (is (not (contains? tool-names "tool-0")))
        ;; Latest should be present
        (is (contains? tool-names "tool-509"))))))

;; =============================================================================
;; Behavioral Query Tests
;; =============================================================================

(deftest query-behaviors-test
  (testing "returns all events for an agent"
    (sig/record-behavior! "query-agent" :tool-use {:tool-name "a" :success? true})
    (sig/record-behavior! "query-agent" :tool-use {:tool-name "b" :success? true})
    (sig/record-behavior! "query-agent" :error {:error-type :timeout})
    (let [events (sig/query-behaviors "query-agent")]
      (is (= 3 (count events)))))

  (testing "returns newest first"
    (sig/record-behavior! "order-agent" :tool-use {:tool-name "first" :success? true})
    (Thread/sleep 5)
    (sig/record-behavior! "order-agent" :tool-use {:tool-name "second" :success? true})
    (let [events (sig/query-behaviors "order-agent")]
      (is (= "second" (get-in (first events) [:event/data :tool-name])))
      (is (= "first" (get-in (second events) [:event/data :tool-name])))))

  (testing "filters by event-type"
    (sig/record-behavior! "filter-agent" :tool-use {:tool-name "a" :success? true})
    (sig/record-behavior! "filter-agent" :error {:error-type :timeout})
    (sig/record-behavior! "filter-agent" :tool-use {:tool-name "b" :success? true})
    (let [tool-events (sig/query-behaviors "filter-agent" :event-type :tool-use)
          error-events (sig/query-behaviors "filter-agent" :event-type :error)]
      (is (= 2 (count tool-events)))
      (is (= 1 (count error-events)))))

  (testing "filters by project-id"
    (sig/record-behavior! "proj-agent" :tool-use {:tool-name "a" :success? true}
                          :project-id "proj-1")
    (sig/record-behavior! "proj-agent" :tool-use {:tool-name "b" :success? true}
                          :project-id "proj-2")
    (let [p1-events (sig/query-behaviors "proj-agent" :project-id "proj-1")]
      (is (= 1 (count p1-events)))
      (is (= "a" (get-in (first p1-events) [:event/data :tool-name])))))

  (testing "filters by session-id"
    (sig/record-behavior! "sess-agent" :tool-use {:tool-name "a" :success? true}
                          :session-id "session-1")
    (sig/record-behavior! "sess-agent" :tool-use {:tool-name "b" :success? true}
                          :session-id "session-2")
    (let [s1-events (sig/query-behaviors "sess-agent" :session-id "session-1")]
      (is (= 1 (count s1-events)))))

  (testing "respects limit"
    (dotimes [i 10]
      (sig/record-behavior! "limit-agent" :tool-use
                            {:tool-name (str "tool-" i) :success? true}))
    (let [limited (sig/query-behaviors "limit-agent" :limit 3)]
      (is (= 3 (count limited)))))

  (testing "returns empty vector for unknown agent"
    (is (empty? (sig/query-behaviors "nonexistent-agent")))))

(deftest query-all-behaviors-test
  (testing "returns events across all agents"
    (sig/record-behavior! "all-a" :tool-use {:tool-name "x" :success? true}
                          :project-id "p1")
    (sig/record-behavior! "all-b" :error {:error-type :timeout}
                          :project-id "p1")
    (sig/record-behavior! "all-c" :tool-use {:tool-name "y" :success? true}
                          :project-id "p2")
    (let [all (sig/query-all-behaviors)
          p1-only (sig/query-all-behaviors :project-id "p1")]
      (is (= 3 (count all)))
      (is (= 2 (count p1-only)))))

  (testing "filters by event-type across agents"
    ;; Events from previous test still present
    (let [tool-events (sig/query-all-behaviors :event-type :tool-use)]
      (is (= 2 (count tool-events))))))

;; =============================================================================
;; Agent Profile Tests
;; =============================================================================

(deftest agent-profile-test
  (testing "computes a comprehensive profile from events"
    ;; Record a mix of events
    (sig/record-behavior! "profile-agent" :session-start {:cwd "/project"} :session-id "s1")
    (sig/record-behavior! "profile-agent" :tool-use {:tool-name "read_file" :success? true} :session-id "s1")
    (sig/record-behavior! "profile-agent" :tool-use {:tool-name "read_file" :success? true} :session-id "s1")
    (sig/record-behavior! "profile-agent" :tool-use {:tool-name "grep" :success? true} :session-id "s1")
    (sig/record-behavior! "profile-agent" :tool-use {:tool-name "file_write" :success? true} :session-id "s1")
    (sig/record-behavior! "profile-agent" :task-complete {:task-id "t1" :task-type "impl"
                                                          :duration-ms 60000 :error-count 0}
                          :session-id "s1")
    (sig/record-behavior! "profile-agent" :task-complete {:task-id "t2" :task-type "review"
                                                          :duration-ms 30000 :error-count 1}
                          :session-id "s1")
    (sig/record-behavior! "profile-agent" :task-fail {:task-id "t3" :error-count 5} :session-id "s1")
    (sig/record-behavior! "profile-agent" :error {:error-type :nrepl-connection :recoverable? true} :session-id "s1")
    (sig/record-behavior! "profile-agent" :error {:error-type :timeout :recoverable? false} :session-id "s1")

    (let [profile (sig/agent-profile "profile-agent")]
      (is (map? profile))
      ;; Identity
      (is (= "profile-agent" (:profile/agent-id profile)))
      ;; Task counts
      (is (= 3 (:profile/total-tasks profile)))
      (is (= 2 (:profile/completed-tasks profile)))
      (is (= 1 (:profile/failed-tasks profile)))
      ;; Tool usage
      (is (= 4 (:profile/total-tool-calls profile)))
      (is (= {"read_file" 2 "grep" 1 "file_write" 1}
             (:profile/tool-frequency profile)))
      ;; Error tracking
      (is (= 2 (:profile/total-errors profile)))
      (is (= {:nrepl-connection 1 :timeout 1}
             (:profile/error-frequency profile)))
      ;; Average task duration (from completed tasks only)
      (is (= 45000.0 (:profile/avg-task-duration-ms profile)))
      ;; Sessions
      (is (= 1 (:profile/sessions profile)))
      ;; Timestamps
      (is (inst? (:profile/last-active profile)))
      (is (inst? (:profile/computed-at profile)))))

  (testing "returns nil for agent with no data"
    (is (nil? (sig/agent-profile "nonexistent-agent"))))

  (testing "handles agent with only tool-use events (no tasks)"
    (sig/record-behavior! "tools-only" :tool-use {:tool-name "grep" :success? true})
    (sig/record-behavior! "tools-only" :tool-use {:tool-name "grep" :success? true})
    (let [profile (sig/agent-profile "tools-only")]
      (is (= 0 (:profile/total-tasks profile)))
      (is (= 2 (:profile/total-tool-calls profile)))
      (is (nil? (:profile/avg-task-duration-ms profile))))))

;; =============================================================================
;; Behavioral Similarity Tests
;; =============================================================================

(deftest behavior-similarity-test
  (testing "identical tool usage yields jaccard 1.0"
    (sig/record-behavior! "sim-a" :tool-use {:tool-name "read_file" :success? true})
    (sig/record-behavior! "sim-a" :tool-use {:tool-name "grep" :success? true})
    (sig/record-behavior! "sim-b" :tool-use {:tool-name "read_file" :success? true})
    (sig/record-behavior! "sim-b" :tool-use {:tool-name "grep" :success? true})
    (let [sim (sig/behavior-similarity "sim-a" "sim-b")]
      (is (= 1.0 (:jaccard sim)))
      (is (= #{"read_file" "grep"} (:shared-tools sim)))
      (is (empty? (:unique-a sim)))
      (is (empty? (:unique-b sim)))))

  (testing "completely different tools yields jaccard 0.0"
    (sig/record-behavior! "diff-a" :tool-use {:tool-name "tool-x" :success? true})
    (sig/record-behavior! "diff-b" :tool-use {:tool-name "tool-y" :success? true})
    (let [sim (sig/behavior-similarity "diff-a" "diff-b")]
      (is (= 0.0 (:jaccard sim)))
      (is (empty? (:shared-tools sim)))
      (is (= #{"tool-x"} (:unique-a sim)))
      (is (= #{"tool-y"} (:unique-b sim)))))

  (testing "partial overlap yields jaccard between 0 and 1"
    (sig/record-behavior! "partial-a" :tool-use {:tool-name "read_file" :success? true})
    (sig/record-behavior! "partial-a" :tool-use {:tool-name "grep" :success? true})
    (sig/record-behavior! "partial-b" :tool-use {:tool-name "read_file" :success? true})
    (sig/record-behavior! "partial-b" :tool-use {:tool-name "file_write" :success? true})
    (let [sim (sig/behavior-similarity "partial-a" "partial-b")]
      ;; Jaccard: |{read_file}| / |{read_file, grep, file_write}| = 1/3
      (is (< 0.3 (:jaccard sim) 0.4))
      (is (= #{"read_file"} (:shared-tools sim)))))

  (testing "returns nil when either agent has no data"
    (is (nil? (sig/behavior-similarity "has-no-data-a" "has-no-data-b")))
    (sig/record-behavior! "has-data" :tool-use {:tool-name "x" :success? true})
    (is (nil? (sig/behavior-similarity "has-data" "has-no-data-a")))))

;; =============================================================================
;; Behavioral Store Management Tests
;; =============================================================================

(deftest get-tracked-agents-test
  (testing "returns empty set when no agents tracked"
    (is (empty? (sig/get-tracked-agents))))

  (testing "returns set of tracked agent IDs"
    (sig/record-behavior! "agent-x" :tool-use {:tool-name "a" :success? true})
    (sig/record-behavior! "agent-y" :tool-use {:tool-name "b" :success? true})
    (let [agents (sig/get-tracked-agents)]
      (is (set? agents))
      (is (contains? agents "agent-x"))
      (is (contains? agents "agent-y")))))

(deftest get-behavior-event-count-test
  (testing "returns 0 for unknown agent"
    (is (= 0 (sig/get-behavior-event-count "unknown"))))

  (testing "returns correct count"
    (sig/record-behavior! "count-agent" :tool-use {:tool-name "a" :success? true})
    (sig/record-behavior! "count-agent" :error {:error-type :timeout})
    (is (= 2 (sig/get-behavior-event-count "count-agent")))))

(deftest clear-agent-behaviors-test
  (testing "clears events for specific agent"
    (sig/record-behavior! "clear-me" :tool-use {:tool-name "a" :success? true})
    (sig/record-behavior! "keep-me" :tool-use {:tool-name "b" :success? true})
    (let [cleared (sig/clear-agent-behaviors! "clear-me")]
      (is (= 1 cleared))
      (is (= 0 (sig/get-behavior-event-count "clear-me")))
      (is (= 1 (sig/get-behavior-event-count "keep-me")))))

  (testing "returns 0 for unknown agent"
    (is (= 0 (sig/clear-agent-behaviors! "unknown")))))

(deftest clear-all-behaviors-test
  (testing "clears all agents"
    (sig/record-behavior! "all-1" :tool-use {:tool-name "a" :success? true})
    (sig/record-behavior! "all-2" :tool-use {:tool-name "b" :success? true})
    (let [cleared (sig/clear-all-behaviors!)]
      (is (= 2 cleared))
      (is (empty? (sig/get-tracked-agents))))))

(deftest evict-old-behaviors-test
  (testing "evicts events older than max-age"
    ;; Record events, then evict with 0ms max-age (everything is old)
    (sig/record-behavior! "evict-agent" :tool-use {:tool-name "a" :success? true})
    (sig/record-behavior! "evict-agent" :tool-use {:tool-name "b" :success? true})
    (Thread/sleep 10)
    (let [result (sig/evict-old-behaviors! :max-age-ms 1)]
      (is (pos? (:events-evicted result)))
      (is (= 0 (sig/get-behavior-event-count "evict-agent")))))

  (testing "keeps recent events"
    (sig/record-behavior! "fresh-agent" :tool-use {:tool-name "a" :success? true})
    (let [result (sig/evict-old-behaviors! :max-age-ms 60000)]
      ;; Should keep the event (it's less than 60s old)
      (is (= 1 (sig/get-behavior-event-count "fresh-agent"))))))

;; =============================================================================
;; Schema Validation Tests
;; =============================================================================

(deftest schema-definitions-test
  (testing "outcome-schema has expected keys"
    (is (contains? sig/outcome-schema :task-completed?))
    (is (contains? sig/outcome-schema :tools-used))
    (is (contains? sig/outcome-schema :user-correction?))
    (is (contains? sig/outcome-schema :time-to-completion-ms)))

  (testing "signature-schema has expected keys"
    (is (contains? sig/signature-schema :signature/id))
    (is (contains? sig/signature-schema :signature/recall-id))
    (is (contains? sig/signature-schema :signature/entries-recalled))
    (is (contains? sig/signature-schema :signature/outcome)))

  (testing "behavior-event-schema has expected keys"
    (is (contains? sig/behavior-event-schema :event/type))
    (is (contains? sig/behavior-event-schema :event/agent-id))
    (is (contains? sig/behavior-event-schema :event/timestamp))
    (is (contains? sig/behavior-event-schema :event/data)))

  (testing "tool-use-data-schema has expected keys"
    (is (contains? sig/tool-use-data-schema :tool-name))
    (is (contains? sig/tool-use-data-schema :duration-ms))
    (is (contains? sig/tool-use-data-schema :success?)))

  (testing "task-outcome-data-schema has expected keys"
    (is (contains? sig/task-outcome-data-schema :task-id))
    (is (contains? sig/task-outcome-data-schema :task-type))
    (is (contains? sig/task-outcome-data-schema :tools-used))
    (is (contains? sig/task-outcome-data-schema :duration-ms))
    (is (contains? sig/task-outcome-data-schema :error-count)))

  (testing "error-data-schema has expected keys"
    (is (contains? sig/error-data-schema :error-type))
    (is (contains? sig/error-data-schema :error-message))
    (is (contains? sig/error-data-schema :tool-name))
    (is (contains? sig/error-data-schema :recoverable?)))

  (testing "agent-profile-schema has expected keys"
    (is (contains? sig/agent-profile-schema :profile/agent-id))
    (is (contains? sig/agent-profile-schema :profile/total-tasks))
    (is (contains? sig/agent-profile-schema :profile/completed-tasks))
    (is (contains? sig/agent-profile-schema :profile/failed-tasks))
    (is (contains? sig/agent-profile-schema :profile/total-tool-calls))
    (is (contains? sig/agent-profile-schema :profile/tool-frequency))
    (is (contains? sig/agent-profile-schema :profile/error-frequency))
    (is (contains? sig/agent-profile-schema :profile/total-errors))
    (is (contains? sig/agent-profile-schema :profile/avg-task-duration-ms))
    (is (contains? sig/agent-profile-schema :profile/sessions))
    (is (contains? sig/agent-profile-schema :profile/last-active))
    (is (contains? sig/agent-profile-schema :profile/computed-at))))
