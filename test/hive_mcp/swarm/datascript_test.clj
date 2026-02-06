(ns hive-mcp.swarm.datascript-test
  "Tests for unified DataScript swarm state.
   
   Coverage target: 60% of core functions (slave/task/claim CRUD + queries).
   Tests follow TDD pattern with fixture-based isolation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.datascript :as ds]))

;; Fixture - reset global conn before each test
(defn with-fresh-conn [f]
  (ds/reset-conn!)
  (f))

(use-fixtures :each with-fresh-conn)

(defn gen-slave-id []
  (str "test-slave-" (java.util.UUID/randomUUID)))

(defn gen-task-id []
  (str "test-task-" (java.util.UUID/randomUUID)))

;; =============================================================================
;; Slave CRUD Tests
;; =============================================================================

(deftest slave-add-test
  (testing "Adding a slave creates entity"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (let [slave (ds/get-slave slave-id)]
        (is (= slave-id (:slave/id slave)))
        (is (= :idle (:slave/status slave)))))))

(deftest slave-add-with-metadata-test
  (testing "Adding slave with name and presets"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle :name "worker" :presets ["tdd"]})
      (let [slave (ds/get-slave slave-id)]
        (is (= "worker" (:slave/name slave)))
        ;; presets are stored as a set due to :db.cardinality/many
        (is (contains? (:slave/presets slave) "tdd"))))))

(deftest slave-add-with-cwd-test
  (testing "Adding slave with working directory"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle :cwd "/home/user/project"})
      (let [slave (ds/get-slave slave-id)]
        (is (= "/home/user/project" (:slave/cwd slave)))))))

(deftest slave-update-test
  (testing "Updating slave status"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/update-slave! slave-id {:slave/status :working})
      (let [slave (ds/get-slave slave-id)]
        (is (= :working (:slave/status slave)))))))

(deftest slave-remove-test
  (testing "Removing a slave"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/remove-slave! slave-id)
      (is (nil? (ds/get-slave slave-id))))))

(deftest slave-remove-nonexistent-test
  (testing "Removing non-existent slave returns nil"
    (is (nil? (ds/remove-slave! "nonexistent")))))

(deftest slave-get-nonexistent-test
  (testing "Getting non-existent slave returns nil"
    (is (nil? (ds/get-slave "nonexistent")))))

;; =============================================================================
;; Task CRUD Tests
;; =============================================================================

(deftest task-add-test
  (testing "Adding a task"
    (let [slave-id (gen-slave-id)
          task-id (gen-task-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/add-task! task-id slave-id {:status :dispatched})
      (let [task (ds/get-task task-id)]
        (is (= task-id (:task/id task)))
        (is (= :dispatched (:task/status task)))))))

(deftest task-complete-test
  (testing "Completing a task"
    (let [slave-id (gen-slave-id)
          task-id (gen-task-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/add-task! task-id slave-id {:status :dispatched})
      (ds/complete-task! task-id)
      (let [task (ds/get-task task-id)]
        (is (= :completed (:task/status task)))))))

(deftest task-fail-error-test
  (testing "Failing a task with error status"
    (let [slave-id (gen-slave-id)
          task-id (gen-task-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/add-task! task-id slave-id {:status :dispatched})
      (ds/fail-task! task-id :error)
      (let [task (ds/get-task task-id)]
        (is (= :error (:task/status task)))))))

(deftest task-fail-timeout-test
  (testing "Failing a task with timeout status"
    (let [slave-id (gen-slave-id)
          task-id (gen-task-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/add-task! task-id slave-id {:status :dispatched})
      (ds/fail-task! task-id :timeout)
      (let [task (ds/get-task task-id)]
        (is (= :timeout (:task/status task)))))))

(deftest task-get-nonexistent-test
  (testing "Getting non-existent task returns nil"
    (is (nil? (ds/get-task "nonexistent")))))

;; =============================================================================
;; Claims Tests
;; =============================================================================

(deftest claim-file-test
  (testing "Claiming a file"
    (let [slave-id (gen-slave-id)
          file "/src/core.clj"]
      (ds/add-slave! slave-id {:status :idle})
      (ds/claim-file! file slave-id)
      (let [claim (ds/get-claims-for-file file)]
        (is (some? claim))
        (is (= file (:file claim)))
        (is (= slave-id (:slave-id claim)))))))

(deftest release-claim-test
  (testing "Releasing a file claim"
    (let [slave-id (gen-slave-id)
          file "/src/core.clj"]
      (ds/add-slave! slave-id {:status :idle})
      (ds/claim-file! file slave-id)
      (ds/release-claim! file)
      (is (nil? (ds/get-claims-for-file file))))))

(deftest has-conflict-detects-conflict-test
  (testing "Conflict detection when different slave holds claim"
    (let [slave1-id (gen-slave-id)
          slave2-id (gen-slave-id)
          file "/src/core.clj"]
      (ds/add-slave! slave1-id {:status :idle})
      (ds/add-slave! slave2-id {:status :idle})
      (ds/claim-file! file slave1-id)
      (is (:conflict? (ds/has-conflict? file slave2-id))))))

(deftest has-conflict-no-conflict-same-slave-test
  (testing "No conflict when same slave holds claim"
    (let [slave-id (gen-slave-id)
          file "/src/core.clj"]
      (ds/add-slave! slave-id {:status :idle})
      (ds/claim-file! file slave-id)
      (is (not (:conflict? (ds/has-conflict? file slave-id)))))))

(deftest has-conflict-shows-holder-test
  (testing "Conflict shows holder ID"
    (let [slave1-id (gen-slave-id)
          slave2-id (gen-slave-id)
          file "/src/core.clj"]
      (ds/add-slave! slave1-id {:status :idle})
      (ds/add-slave! slave2-id {:status :idle})
      (ds/claim-file! file slave1-id)
      (let [result (ds/has-conflict? file slave2-id)]
        (is (:conflict? result))
        (is (= slave1-id (:held-by result)))))))

(deftest has-conflict-unclaimed-file-test
  (testing "No conflict for unclaimed file"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (is (not (:conflict? (ds/has-conflict? "/unclaimed.clj" slave-id)))))))

;; =============================================================================
;; Query Tests
;; =============================================================================

(deftest get-all-slaves-test
  (testing "Get all slaves"
    (let [slave1-id (gen-slave-id)
          slave2-id (gen-slave-id)]
      (ds/add-slave! slave1-id {:status :idle})
      (ds/add-slave! slave2-id {:status :working})
      (let [slaves (ds/get-all-slaves)]
        (is (= 2 (count slaves)))))))

(deftest get-slaves-by-status-test
  (testing "Get slaves by status"
    (let [slave1-id (gen-slave-id)
          slave2-id (gen-slave-id)]
      (ds/add-slave! slave1-id {:status :idle})
      (ds/add-slave! slave2-id {:status :working})
      (let [idle-slaves (ds/get-slaves-by-status :idle)]
        (is (= 1 (count idle-slaves)))))))

(deftest get-slaves-by-status-working-test
  (testing "Get working slaves"
    (let [slave1-id (gen-slave-id)
          slave2-id (gen-slave-id)
          slave3-id (gen-slave-id)]
      (ds/add-slave! slave1-id {:status :idle})
      (ds/add-slave! slave2-id {:status :working})
      (ds/add-slave! slave3-id {:status :working})
      (let [working-slaves (ds/get-slaves-by-status :working)]
        (is (= 2 (count working-slaves)))))))

(deftest get-all-claims-test
  (testing "Get all claims"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/claim-file! "/src/a.clj" slave-id)
      (ds/claim-file! "/src/b.clj" slave-id)
      (let [claims (ds/get-all-claims)]
        (is (= 2 (count claims)))))))

(deftest db-stats-test
  (testing "DB statistics"
    (let [slave-id (gen-slave-id)
          task-id (gen-task-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/add-task! task-id slave-id {:status :dispatched})
      (ds/claim-file! "/src/core.clj" slave-id)
      (let [stats (ds/db-stats)]
        (is (= 1 (:slaves stats)))
        (is (= 1 (:tasks stats)))
        (is (= 1 (:claims stats)))))))

(deftest get-tasks-for-slave-test
  (testing "Get tasks for specific slave"
    (let [slave-id (gen-slave-id)
          task1-id (gen-task-id)
          task2-id (gen-task-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/add-task! task1-id slave-id {:status :dispatched})
      (ds/add-task! task2-id slave-id {:status :dispatched})
      (let [tasks (ds/get-tasks-for-slave slave-id)]
        (is (= 2 (count tasks)))))))

(deftest get-completed-tasks-test
  (testing "Get completed tasks"
    (let [slave-id (gen-slave-id)
          task1-id (gen-task-id)
          task2-id (gen-task-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/add-task! task1-id slave-id {:status :dispatched})
      (ds/add-task! task2-id slave-id {:status :dispatched})
      (ds/complete-task! task1-id)
      (let [completed (ds/get-completed-tasks)]
        (is (= 1 (count completed)))
        (is (= task1-id (:task/id (first completed))))))))

;; =============================================================================
;; Kill Guard / Critical Operations Tests (ADR-003)
;; =============================================================================

(deftest enter-critical-op-test
  (testing "Entering a critical operation adds to set"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/enter-critical-op! slave-id :wrap)
      (let [ops (ds/get-critical-ops slave-id)]
        (is (contains? ops :wrap))))))

(deftest exit-critical-op-test
  (testing "Exiting a critical operation removes from set"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/enter-critical-op! slave-id :wrap)
      (ds/exit-critical-op! slave-id :wrap)
      (let [ops (ds/get-critical-ops slave-id)]
        (is (not (contains? ops :wrap)))))))

(deftest multiple-critical-ops-test
  (testing "Multiple critical operations can be active"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/enter-critical-op! slave-id :wrap)
      (ds/enter-critical-op! slave-id :commit)
      (let [ops (ds/get-critical-ops slave-id)]
        (is (= #{:wrap :commit} ops))))))

(deftest get-critical-ops-nonexistent-test
  (testing "Getting critical ops for non-existent slave returns empty set"
    (is (= #{} (ds/get-critical-ops "nonexistent-slave")))))

(deftest can-kill-when-no-ops-test
  (testing "can-kill? returns true when no critical ops"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
        (is can-kill?)
        (is (empty? blocking-ops))))))

(deftest can-kill-blocked-by-wrap-test
  (testing "can-kill? returns false when wrap in progress"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/enter-critical-op! slave-id :wrap)
      (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
        (is (not can-kill?))
        (is (contains? blocking-ops :wrap))))))

(deftest can-kill-blocked-by-commit-test
  (testing "can-kill? returns false when commit in progress"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/enter-critical-op! slave-id :commit)
      (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
        (is (not can-kill?))
        (is (contains? blocking-ops :commit))))))

(deftest can-kill-blocked-by-dispatch-test
  (testing "can-kill? returns false when dispatch in progress"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/enter-critical-op! slave-id :dispatch)
      (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
        (is (not can-kill?))
        (is (contains? blocking-ops :dispatch))))))

(deftest can-kill-multiple-blocking-ops-test
  (testing "can-kill? returns all blocking ops"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/enter-critical-op! slave-id :wrap)
      (ds/enter-critical-op! slave-id :commit)
      (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
        (is (not can-kill?))
        (is (= #{:wrap :commit} blocking-ops))))))

(deftest can-kill-nonexistent-slave-test
  (testing "can-kill? returns true for non-existent slave (no ops to block)"
    (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? "nonexistent")]
      (is can-kill?)
      (is (empty? blocking-ops)))))

(deftest with-critical-op-macro-test
  (testing "with-critical-op macro manages enter/exit correctly"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      ;; Before: no ops
      (is (empty? (ds/get-critical-ops slave-id)))
      ;; During: op is active
      (ds/with-critical-op slave-id :wrap
        (is (contains? (ds/get-critical-ops slave-id) :wrap)))
      ;; After: op is cleared
      (is (empty? (ds/get-critical-ops slave-id))))))

(deftest with-critical-op-exception-cleanup-test
  (testing "with-critical-op cleans up even on exception"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (try
        (ds/with-critical-op slave-id :commit
          (throw (ex-info "Simulated error" {})))
        (catch Exception _))
      ;; Op should be cleaned up despite exception
      (is (empty? (ds/get-critical-ops slave-id))))))

;; =============================================================================
;; Agent-ID Resolution Tests (BUG FIX: hivemind status sync)
;; =============================================================================

(deftest get-slave-by-name-exact-id-match-test
  (testing "get-slave-by-name-or-id returns slave on exact ID match"
    (let [slave-id "swarm-github-connector-1770230187"]
      (ds/add-slave! slave-id {:status :idle :name "github-connector"})
      (let [slave (ds/get-slave-by-name-or-id slave-id)]
        (is (some? slave))
        (is (= slave-id (:slave/id slave)))))))

(deftest get-slave-by-name-fallback-test
  (testing "get-slave-by-name-or-id finds slave by name when ID doesn't match"
    (let [spawn-id "swarm-github-connector-1770230187"]
      (ds/add-slave! spawn-id {:status :working :name "github-connector"})
      ;; Query using short ling-style ID
      (let [slave (ds/get-slave-by-name-or-id "ling-github-connector")]
        (is (some? slave))
        (is (= spawn-id (:slave/id slave)))
        (is (= "github-connector" (:slave/name slave)))))))

(deftest get-slave-by-name-strips-ling-prefix-test
  (testing "get-slave-by-name-or-id strips 'ling-' prefix for name lookup"
    (let [spawn-id "swarm-worker-123456"]
      (ds/add-slave! spawn-id {:status :idle :name "worker"})
      (let [slave (ds/get-slave-by-name-or-id "ling-worker")]
        (is (some? slave))
        (is (= spawn-id (:slave/id slave)))))))

(deftest get-slave-by-name-extracts-from-swarm-id-test
  (testing "get-slave-by-name-or-id extracts name from swarm-NAME-TIMESTAMP format"
    (let [spawn-id "swarm-reviewer-987654"]
      (ds/add-slave! spawn-id {:status :idle :name "reviewer"})
      ;; Query using a different swarm-style ID (different timestamp)
      ;; Should still find by extracted name
      (let [slave (ds/get-slave-by-name-or-id "swarm-reviewer-111111")]
        (is (some? slave))
        (is (= spawn-id (:slave/id slave)))))))

(deftest get-slave-by-name-not-found-test
  (testing "get-slave-by-name-or-id returns nil when slave doesn't exist"
    (is (nil? (ds/get-slave-by-name-or-id "ling-nonexistent")))
    (is (nil? (ds/get-slave-by-name-or-id "swarm-nonexistent-999")))))

(deftest get-slave-by-name-most-recent-test
  (testing "get-slave-by-name-or-id returns most recent slave when multiple share name"
    ;; This can happen if a ling is killed and respawned with same name
    (let [old-id "swarm-tdd-100"
          new-id "swarm-tdd-200"]
      ;; Add older slave first
      (ds/add-slave! old-id {:status :terminated :name "tdd"
                             :created-at (java.util.Date. 1000)})
      ;; Add newer slave
      (ds/add-slave! new-id {:status :working :name "tdd"
                             :created-at (java.util.Date. 2000)})
      (let [slave (ds/get-slave-by-name-or-id "ling-tdd")]
        (is (some? slave))
        ;; Should return the more recent one (new-id)
        (is (= new-id (:slave/id slave)))))))

;; =============================================================================
;; Fuzzy Keyword Matching Tests (BUG FIX: ling status stuck on working)
;; =============================================================================
;; Tests for keyword-based fuzzy matching when exact ID/name match fails.
;; This handles the case where lings shout with IDs like "ling-wave-fix"
;; but their spawn IDs are "swarm-fix-wave-race-condition-1770255229".

(deftest fuzzy-match-keyword-overlap-test
  (testing "get-slave-by-name-or-id finds slave via keyword overlap"
    (let [spawn-id "swarm-fix-wave-race-condition-1770255229"]
      (ds/add-slave! spawn-id {:status :working})
      ;; Query with ling-style ID that shares keywords "fix" and "wave"
      (let [slave (ds/get-slave-by-name-or-id "ling-wave-fix")]
        (is (some? slave) "Should find slave via keyword overlap")
        (is (= spawn-id (:slave/id slave)))))))

(deftest fuzzy-match-multiple-keywords-test
  (testing "get-slave-by-name-or-id matches on multiple keyword overlap"
    (let [spawn-id "swarm-explore-status-bug-1770256094"]
      (ds/add-slave! spawn-id {:status :idle})
      ;; Query with ling-style ID sharing "explore" and "status"
      (let [slave (ds/get-slave-by-name-or-id "ling-explore-status")]
        (is (some? slave) "Should find slave with multiple keyword matches")
        (is (= spawn-id (:slave/id slave)))))))

(deftest fuzzy-match-ignores-timestamp-test
  (testing "get-slave-by-name-or-id ignores numeric timestamp in matching"
    (let [spawn-id "swarm-github-connector-1770230187"]
      (ds/add-slave! spawn-id {:status :working})
      ;; Query shouldn't be confused by timestamps in the spawn ID
      (let [slave (ds/get-slave-by-name-or-id "ling-github-connector")]
        (is (some? slave))
        (is (= spawn-id (:slave/id slave)))))))

(deftest fuzzy-match-requires-minimum-overlap-test
  (testing "get-slave-by-name-or-id requires >=50% keyword overlap"
    (let [spawn-id "swarm-fix-wave-race-condition-1770255229"]
      (ds/add-slave! spawn-id {:status :working})
      ;; Query with only 1 of 4 keywords (25% overlap) - should NOT match
      (let [slave (ds/get-slave-by-name-or-id "ling-fix-unrelated-task-here")]
        ;; "fix" matches, but that's only 1/4 = 25% < 50% threshold
        ;; Actually let's recalculate: "fix", "unrelated", "task", "here" = 4 keywords
        ;; Only "fix" matches, so 1/4 = 25% which is below 50%
        (is (nil? slave) "Should not match with <50% keyword overlap")))))

(deftest fuzzy-match-prefers-exact-match-test
  (testing "get-slave-by-name-or-id prefers exact match over fuzzy"
    (let [exact-id "ling-wave-fix"
          fuzzy-id "swarm-fix-wave-race-1234"]
      ;; Add both - one with exact ID, one that would fuzzy match
      (ds/add-slave! exact-id {:status :idle})
      (ds/add-slave! fuzzy-id {:status :working})
      ;; Query should find exact match first
      (let [slave (ds/get-slave-by-name-or-id "ling-wave-fix")]
        (is (some? slave))
        (is (= exact-id (:slave/id slave)) "Should prefer exact match")))))

(deftest fuzzy-match-best-overlap-wins-test
  (testing "get-slave-by-name-or-id picks slave with best keyword overlap"
    (let [partial-id "swarm-fix-something-else-1111"
          better-id "swarm-fix-wave-race-2222"]
      (ds/add-slave! partial-id {:status :idle})
      (ds/add-slave! better-id {:status :working})
      ;; Query "ling-wave-fix" has keywords: "wave", "fix"
      ;; partial-id has: "fix", "something", "else" -> 1 match
      ;; better-id has: "fix", "wave", "race" -> 2 matches
      (let [slave (ds/get-slave-by-name-or-id "ling-wave-fix")]
        (is (some? slave))
        (is (= better-id (:slave/id slave)) "Should pick slave with best overlap")))))

(deftest fuzzy-match-no-false-positives-test
  (testing "get-slave-by-name-or-id doesn't match unrelated slaves"
    (ds/add-slave! "swarm-github-connector-1234" {:status :idle})
    (ds/add-slave! "swarm-tdd-runner-5678" {:status :working})
    ;; Query with completely unrelated keywords
    (let [slave (ds/get-slave-by-name-or-id "ling-kanban-cleanup")]
      (is (nil? slave) "Should not match unrelated slaves"))))

;; =============================================================================
;; Headless Ling Schema Tests
;; =============================================================================

(deftest spawn-mode-vterm-test
  (testing "Adding slave with spawn-mode :vterm"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/update-slave! slave-id {:ling/spawn-mode :vterm})
      (let [slave (ds/get-slave slave-id)]
        (is (= :vterm (:ling/spawn-mode slave)))))))

(deftest spawn-mode-headless-test
  (testing "Adding slave with headless spawn-mode and process attrs"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/update-slave! slave-id {:ling/spawn-mode :headless
                                  :ling/process-pid 12345
                                  :ling/process-alive? true})
      (let [slave (ds/get-slave slave-id)]
        (is (= :headless (:ling/spawn-mode slave)))
        (is (= 12345 (:ling/process-pid slave)))
        (is (true? (:ling/process-alive? slave)))))))

(deftest process-alive-update-test
  (testing "Updating process-alive? on headless ling"
    (let [slave-id (gen-slave-id)]
      (ds/add-slave! slave-id {:status :idle})
      (ds/update-slave! slave-id {:ling/spawn-mode :headless
                                  :ling/process-pid 99999
                                  :ling/process-alive? true})
      ;; Process dies
      (ds/update-slave! slave-id {:ling/process-alive? false})
      (let [slave (ds/get-slave slave-id)]
        (is (false? (:ling/process-alive? slave)))
        (is (= 99999 (:ling/process-pid slave)))))))

;; =============================================================================
;; Stdout Ring Buffer Tests
;; =============================================================================

;; Use a separate fixture for buffer tests to reset buffers
(defn with-fresh-buffers [f]
  (ds/reset-stdout-buffers!)
  (f)
  (ds/reset-stdout-buffers!))

(use-fixtures :each with-fresh-conn with-fresh-buffers)

(deftest stdout-init-buffer-test
  (testing "Initializing stdout buffer creates empty buffer"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (let [info (ds/get-stdout-buffer-info slave-id)]
        (is (= 0 (:line-count info)))
        (is (= 0 (:next-idx info)))
        (is (nil? (:oldest-idx info)))
        (is (nil? (:newest-idx info)))))))

(deftest stdout-append-single-string-test
  (testing "Appending a single string line"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id "hello world")
      (let [lines (ds/get-stdout slave-id 10)]
        (is (= 1 (count lines)))
        (is (= "hello world" (:text (first lines))))
        (is (= 0 (:idx (first lines))))))))

(deftest stdout-append-multiple-lines-test
  (testing "Appending multiple lines at once"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id ["line1" "line2" "line3"])
      (let [lines (ds/get-stdout slave-id 10)]
        (is (= 3 (count lines)))
        (is (= "line1" (:text (first lines))))
        (is (= "line3" (:text (last lines))))
        (is (= [0 1 2] (mapv :idx lines)))))))

(deftest stdout-append-sequential-test
  (testing "Sequential appends maintain monotonic indices"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id ["a" "b"])
      (ds/append-stdout! slave-id ["c" "d"])
      (let [lines (ds/get-stdout slave-id 10)]
        (is (= 4 (count lines)))
        (is (= [0 1 2 3] (mapv :idx lines)))
        (is (= ["a" "b" "c" "d"] (mapv :text lines)))))))

(deftest stdout-get-last-n-test
  (testing "get-stdout returns last N lines"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id (mapv str (range 50)))
      (let [last-5 (ds/get-stdout slave-id 5)]
        (is (= 5 (count last-5)))
        (is (= "45" (:text (first last-5))))
        (is (= "49" (:text (last last-5))))))))

(deftest stdout-get-defaults-to-100-test
  (testing "get-stdout defaults to 100 lines"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id (mapv str (range 200)))
      (let [lines (ds/get-stdout slave-id)]
        (is (= 100 (count lines)))
        (is (= "100" (:text (first lines))))))))

(deftest stdout-get-nonexistent-test
  (testing "get-stdout returns empty vector for unknown slave"
    (is (= [] (ds/get-stdout "nonexistent-slave" 10)))))

(deftest stdout-get-since-test
  (testing "get-stdout-since returns lines after given index"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id ["a" "b" "c" "d" "e"])
      ;; Get lines since index 2 (should return idx 3, 4)
      (let [lines (ds/get-stdout-since slave-id 2)]
        (is (= 2 (count lines)))
        (is (= "d" (:text (first lines))))
        (is (= "e" (:text (last lines))))))))

(deftest stdout-get-since-beginning-test
  (testing "get-stdout-since with -1 returns all lines"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id ["a" "b" "c"])
      (let [lines (ds/get-stdout-since slave-id -1)]
        (is (= 3 (count lines)))))))

(deftest stdout-get-since-future-test
  (testing "get-stdout-since with index beyond newest returns empty"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id ["a" "b" "c"])
      (let [lines (ds/get-stdout-since slave-id 999)]
        (is (= 0 (count lines)))))))

(deftest stdout-get-since-nonexistent-test
  (testing "get-stdout-since returns empty for unknown slave"
    (is (= [] (ds/get-stdout-since "nonexistent" 0)))))

(deftest stdout-fifo-eviction-test
  (testing "Buffer evicts oldest lines at capacity"
    (let [slave-id (gen-slave-id)
          cap 5000 ;; ds/stdout-buffer-max-lines
          ;; Add cap + 500 lines to trigger eviction
          total (+ cap 500)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id (mapv str (range total)))
      (let [info (ds/get-stdout-buffer-info slave-id)]
        ;; Buffer should be capped at max
        (is (= cap (:line-count info)))
        ;; Oldest line should be index 500 (first 500 evicted)
        (is (= 500 (:oldest-idx info)))
        ;; Newest should be total - 1
        (is (= (dec total) (:newest-idx info)))
        ;; next-idx should be total
        (is (= total (:next-idx info)))))))

(deftest stdout-fifo-preserves-recent-test
  (testing "FIFO eviction preserves most recent lines"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      ;; Add 5100 lines (100 over cap)
      (ds/append-stdout! slave-id (mapv str (range 5100)))
      ;; First 100 should be evicted, get last 5
      (let [lines (ds/get-stdout slave-id 5)]
        (is (= 5 (count lines)))
        (is (= "5095" (:text (first lines))))
        (is (= "5099" (:text (last lines))))))))

(deftest stdout-cleanup-test
  (testing "Cleaning up buffer removes it entirely"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id "test line")
      (is (true? (ds/cleanup-stdout-buffer! slave-id)))
      (is (nil? (ds/get-stdout-buffer-info slave-id)))
      (is (= [] (ds/get-stdout slave-id 10))))))

(deftest stdout-cleanup-nonexistent-test
  (testing "Cleaning up nonexistent buffer returns false"
    (is (false? (ds/cleanup-stdout-buffer! "nonexistent")))))

(deftest stdout-auto-create-on-append-test
  (testing "append-stdout! auto-creates buffer if missing"
    (let [slave-id (gen-slave-id)]
      ;; Don't call init, just append directly
      (ds/append-stdout! slave-id "auto-created")
      (let [lines (ds/get-stdout slave-id 10)]
        (is (= 1 (count lines)))
        (is (= "auto-created" (:text (first lines))))))))

(deftest stdout-buffer-info-test
  (testing "get-stdout-buffer-info returns correct metadata"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id ["a" "b" "c" "d" "e"])
      (let [info (ds/get-stdout-buffer-info slave-id)]
        (is (= 5 (:line-count info)))
        (is (= 5 (:next-idx info)))
        (is (= 0 (:oldest-idx info)))
        (is (= 4 (:newest-idx info)))
        (is (= 5000 (:max-lines info)))))))

(deftest stdout-buffer-info-nonexistent-test
  (testing "get-stdout-buffer-info returns nil for unknown slave"
    (is (nil? (ds/get-stdout-buffer-info "nonexistent")))))

(deftest stdout-timestamps-present-test
  (testing "Each stdout line has a timestamp"
    (let [slave-id (gen-slave-id)]
      (ds/init-stdout-buffer! slave-id)
      (ds/append-stdout! slave-id "timed line")
      (let [line (first (ds/get-stdout slave-id 1))]
        (is (inst? (:ts line)))))))
