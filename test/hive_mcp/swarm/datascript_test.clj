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
