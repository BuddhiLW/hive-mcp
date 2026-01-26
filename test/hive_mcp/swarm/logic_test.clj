(ns hive-mcp.swarm.logic-test
  "Unit tests for swarm logic predicates."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.swarm.logic :as logic]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-db-fixture
  "Reset the logic database before each test."
  [f]
  (logic/reset-db!)
  (f))

(use-fixtures :each reset-db-fixture)

;; =============================================================================
;; Database Mutation Tests
;; =============================================================================

(deftest add-slave-test
  (testing "Adding a slave registers it in the database"
    (logic/add-slave! "slave-1" :idle)
    (is (logic/slave-exists? "slave-1"))
    (let [slaves (logic/get-all-slaves)]
      (is (= 1 (count slaves)))
      (is (= "slave-1" (:slave-id (first slaves))))
      (is (= :idle (:status (first slaves)))))))

(deftest update-slave-status-test
  (testing "Updating slave status changes the database"
    (logic/add-slave! "slave-1" :idle)
    (logic/update-slave-status! "slave-1" :working)
    (let [slaves (logic/get-all-slaves)]
      (is (= 1 (count slaves)))
      (is (= :working (:status (first slaves)))))))

(deftest remove-slave-test
  (testing "Removing a slave removes it from the database"
    (logic/add-slave! "slave-1" :idle)
    (is (logic/slave-exists? "slave-1"))
    (logic/remove-slave! "slave-1")
    (is (not (logic/slave-exists? "slave-1")))))

(deftest add-task-test
  (testing "Adding a task registers it in the database"
    (logic/add-task! "task-1" "slave-1" :dispatched)
    (let [tasks (logic/get-all-tasks)]
      (is (= 1 (count tasks)))
      (is (= "task-1" (:task-id (first tasks))))
      (is (= "slave-1" (:slave-id (first tasks))))
      (is (= :dispatched (:status (first tasks)))))))

(deftest add-claim-test
  (testing "Adding a file claim registers it"
    (logic/add-claim! "/src/core.clj" "slave-1")
    (let [claims (logic/get-all-claims)]
      (is (= 1 (count claims)))
      (is (= "/src/core.clj" (:file (first claims))))
      (is (= "slave-1" (:slave-id (first claims)))))))

;; =============================================================================
;; File Conflict Detection Tests
;; =============================================================================

(deftest file-conflict-detection-test
  (testing "Detects conflict when two slaves claim same file"
    (logic/add-slave! "slave-1" :working)
    (logic/add-slave! "slave-2" :working)
    (logic/add-claim! "/src/core.clj" "slave-1")

    (let [conflicts (logic/check-file-conflicts "slave-2" ["/src/core.clj"])]
      (is (= 1 (count conflicts)))
      (is (= "/src/core.clj" (:file (first conflicts))))
      (is (= "slave-1" (:held-by (first conflicts)))))))

(deftest no-conflict-for-same-slave-test
  (testing "No conflict when slave re-claims its own file"
    (logic/add-slave! "slave-1" :working)
    (logic/add-claim! "/src/core.clj" "slave-1")

    (let [conflicts (logic/check-file-conflicts "slave-1" ["/src/core.clj"])]
      (is (empty? conflicts)))))

(deftest no-conflict-for-unclaimed-file-test
  (testing "No conflict for unclaimed files"
    (logic/add-slave! "slave-1" :working)

    (let [conflicts (logic/check-file-conflicts "slave-1" ["/src/new-file.clj"])]
      (is (empty? conflicts)))))

(deftest multiple-file-conflicts-test
  (testing "Detects multiple file conflicts"
    (logic/add-slave! "slave-1" :working)
    (logic/add-slave! "slave-2" :working)
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-claim! "/src/b.clj" "slave-1")

    (let [conflicts (logic/check-file-conflicts "slave-2" ["/src/a.clj" "/src/b.clj" "/src/c.clj"])]
      (is (= 2 (count conflicts)))
      (is (every? #(= "slave-1" (:held-by %)) conflicts)))))

;; =============================================================================
;; Circular Dependency Detection Tests
;; =============================================================================

(deftest no-circular-dependency-test
  (testing "No cycle when dependencies are linear"
    (logic/add-task! "task-a" "slave-1" :dispatched)
    (logic/add-task! "task-b" "slave-2" :dispatched)
    (logic/add-task! "task-c" "slave-3" :dispatched)
    (logic/add-dependency! "task-a" "task-b")
    (logic/add-dependency! "task-b" "task-c")

    ;; Adding task-a -> task-c would not create cycle
    (is (not (logic/check-would-deadlock "task-a" "task-c")))))

(deftest direct-circular-dependency-test
  (testing "Detects direct circular dependency"
    (logic/add-task! "task-a" "slave-1" :dispatched)
    (logic/add-task! "task-b" "slave-2" :dispatched)
    (logic/add-dependency! "task-a" "task-b")

    ;; Adding task-b -> task-a would create cycle
    (is (logic/check-would-deadlock "task-b" "task-a"))))

(deftest transitive-circular-dependency-test
  (testing "Detects transitive circular dependency"
    (logic/add-task! "task-a" "slave-1" :dispatched)
    (logic/add-task! "task-b" "slave-2" :dispatched)
    (logic/add-task! "task-c" "slave-3" :dispatched)
    (logic/add-dependency! "task-a" "task-b")
    (logic/add-dependency! "task-b" "task-c")

    ;; Adding task-c -> task-a would create cycle: a->b->c->a
    (is (logic/check-would-deadlock "task-c" "task-a"))))

;; =============================================================================
;; Dependency Readiness Tests
;; =============================================================================

(deftest dependency-ready-when-complete-test
  (testing "Dependency is ready when all deps are completed"
    (logic/add-task! "task-a" "slave-1" :completed)
    (logic/add-task! "task-b" "slave-2" :dispatched)
    (logic/add-dependency! "task-b" "task-a")

    (let [result (logic/check-dependencies-ready "task-b")]
      (is (:ready? result))
      (is (empty? (:pending result))))))

(deftest dependency-not-ready-when-pending-test
  (testing "Dependency is not ready when deps are pending"
    (logic/add-task! "task-a" "slave-1" :dispatched)
    (logic/add-task! "task-b" "slave-2" :dispatched)
    (logic/add-dependency! "task-b" "task-a")

    (let [result (logic/check-dependencies-ready "task-b")]
      (is (not (:ready? result)))
      (is (= ["task-a"] (:pending result))))))

(deftest partial-dependency-readiness-test
  (testing "Reports pending dependencies correctly"
    (logic/add-task! "task-a" "slave-1" :completed)
    (logic/add-task! "task-b" "slave-2" :dispatched)
    (logic/add-task! "task-c" "slave-3" :dispatched)
    (logic/add-dependency! "task-c" "task-a")
    (logic/add-dependency! "task-c" "task-b")

    (let [result (logic/check-dependencies-ready "task-c")]
      (is (not (:ready? result)))
      (is (= ["task-b"] (:pending result))))))

;; =============================================================================
;; Claim Release Tests
;; =============================================================================

(deftest release-claims-for-slave-test
  (testing "Releases all claims for a slave"
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-claim! "/src/b.clj" "slave-1")
    (logic/add-claim! "/src/c.clj" "slave-2")

    (logic/release-claims-for-slave! "slave-1")

    (let [claims (logic/get-all-claims)]
      (is (= 1 (count claims)))
      (is (= "slave-2" (:slave-id (first claims)))))))

(deftest release-claims-for-task-test
  (testing "Releases claims associated with a task"
    (logic/add-task! "task-1" "slave-1" :dispatched)
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-task-file! "task-1" "/src/a.clj")

    (logic/release-claims-for-task! "task-1")

    (let [claims (logic/get-all-claims)]
      (is (empty? claims)))))

(deftest release-claims-for-task-without-task-relation-test
  (testing "Releases claims when task relation is NOT populated (atomic-claim-files! case)"
    ;; This tests the bug fix: atomic-claim-files! only populates claims and
    ;; task-files, NOT the task relation. release-claims-for-task! must still
    ;; work by getting slave-id from claims directly.
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-claim! "/src/b.clj" "slave-1")
    (logic/add-task-file! "task-1" "/src/a.clj")
    (logic/add-task-file! "task-1" "/src/b.clj")
    ;; Note: NO add-task! call - mimics atomic-claim-files! behavior

    (logic/release-claims-for-task! "task-1")

    (let [claims (logic/get-all-claims)]
      (is (empty? claims) "All claims should be released even without task relation"))))

(deftest release-claims-for-task-leaves-other-claims-test
  (testing "Only releases claims for the specified task, not other tasks"
    ;; Task 1 claims
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-task-file! "task-1" "/src/a.clj")
    ;; Task 2 claims (different slave)
    (logic/add-claim! "/src/b.clj" "slave-2")
    (logic/add-task-file! "task-2" "/src/b.clj")

    (logic/release-claims-for-task! "task-1")

    (let [claims (logic/get-all-claims)]
      (is (= 1 (count claims)) "Only task-2 claim should remain")
      (is (= "slave-2" (:slave-id (first claims)))))))

;; =============================================================================
;; Database Stats Tests
;; =============================================================================

(deftest db-stats-test
  (testing "Returns correct database statistics"
    (logic/add-slave! "slave-1" :idle)
    (logic/add-slave! "slave-2" :working)
    (logic/add-task! "task-1" "slave-1" :dispatched)
    (logic/add-claim! "/src/a.clj" "slave-1")

    (let [stats (logic/db-stats)]
      (is (= 2 (:slaves stats)))
      (is (= 1 (:tasks stats)))
      (is (= 1 (:claims stats))))))

;; =============================================================================
;; Dispatch Race Condition Tests (TDD)
;; =============================================================================
;; These tests verify that dispatch waits for preset completion.
;; A slave is only ready for dispatch when status is :idle (preset loaded).

(deftest slave-ready-for-dispatch-test
  (testing "Slave with :idle status is ready for dispatch"
    (logic/add-slave! "slave-1" :idle)
    (is (logic/slave-ready-for-dispatch? "slave-1"))))

(deftest slave-spawning-not-ready-test
  (testing "Slave with :spawning status is NOT ready for dispatch"
    (logic/add-slave! "slave-1" :spawning)
    (is (not (logic/slave-ready-for-dispatch? "slave-1")))))

(deftest slave-starting-not-ready-test
  (testing "Slave with :starting status is NOT ready for dispatch"
    (logic/add-slave! "slave-1" :starting)
    (is (not (logic/slave-ready-for-dispatch? "slave-1")))))

(deftest slave-working-is-busy-not-ready-test
  (testing "Slave with :working status is busy, not ready for new dispatch"
    (logic/add-slave! "slave-1" :working)
    (is (not (logic/slave-ready-for-dispatch? "slave-1")))))

(deftest nonexistent-slave-not-ready-test
  (testing "Non-existent slave is not ready for dispatch"
    (is (not (logic/slave-ready-for-dispatch? "ghost-slave")))))

(deftest check-dispatch-readiness-test
  (testing "check-dispatch-readiness returns status for pre-flight check"
    (logic/add-slave! "slave-1" :idle)
    (logic/add-slave! "slave-2" :spawning)

    (let [result-ready (logic/check-dispatch-readiness "slave-1")
          result-spawning (logic/check-dispatch-readiness "slave-2")
          result-missing (logic/check-dispatch-readiness "missing")]

      ;; Ready slave
      (is (:ready? result-ready))
      (is (= :idle (:status result-ready)))
      (is (nil? (:reason result-ready)))

      ;; Spawning slave
      (is (not (:ready? result-spawning)))
      (is (= :spawning (:status result-spawning)))
      (is (= :preset-loading (:reason result-spawning)))

      ;; Missing slave
      (is (not (:ready? result-missing)))
      (is (nil? (:status result-missing)))
      (is (= :slave-not-found (:reason result-missing))))))
