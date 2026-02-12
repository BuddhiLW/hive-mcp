(ns hive-mcp.swarm.logic-test
  "Unit tests for swarm logic - claims, conflicts, and batch computation."
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
;; Claim Tests
;; =============================================================================

(deftest add-claim-test
  (testing "Adding a file claim registers it"
    (logic/add-claim! "/src/core.clj" "slave-1")
    (let [claims (logic/get-all-claims)]
      (is (= 1 (count claims)))
      (is (= "/src/core.clj" (:file (first claims))))
      (is (= "slave-1" (:slave-id (first claims)))))))

(deftest get-claim-for-file-test
  (testing "Returns claim info for a specific file"
    (logic/add-claim! "/src/core.clj" "slave-1")
    (let [claim (logic/get-claim-for-file "/src/core.clj")]
      (is (= "/src/core.clj" (:file claim)))
      (is (= "slave-1" (:slave-id claim)))))
  (testing "Returns nil for unclaimed file"
    (is (nil? (logic/get-claim-for-file "/src/unclaimed.clj")))))

(deftest release-claim-for-file-test
  (testing "Releases claim regardless of owner"
    (logic/add-claim! "/src/core.clj" "slave-1")
    (is (true? (logic/release-claim-for-file! "/src/core.clj")))
    (is (empty? (logic/get-all-claims))))
  (testing "Returns false for unclaimed file"
    (is (false? (logic/release-claim-for-file! "/src/unclaimed.clj")))))

;; =============================================================================
;; Slave Tests
;; =============================================================================

(deftest add-slave-test
  (testing "Adding a slave registers it in the database"
    (logic/add-slave! "slave-1" :idle)
    (is (logic/slave-exists? "slave-1"))))

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
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-task-file! "task-1" "/src/a.clj")

    (logic/release-claims-for-task! "task-1")

    (let [claims (logic/get-all-claims)]
      (is (empty? claims)))))

(deftest release-claims-for-task-without-task-relation-test
  (testing "Releases claims when task relation is NOT populated (atomic-claim-files! case)"
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-claim! "/src/b.clj" "slave-1")
    (logic/add-task-file! "task-1" "/src/a.clj")
    (logic/add-task-file! "task-1" "/src/b.clj")

    (logic/release-claims-for-task! "task-1")

    (let [claims (logic/get-all-claims)]
      (is (empty? claims) "All claims should be released even without task relation"))))

(deftest release-claims-for-task-leaves-other-claims-test
  (testing "Only releases claims for the specified task, not other tasks"
    (logic/add-claim! "/src/a.clj" "slave-1")
    (logic/add-task-file! "task-1" "/src/a.clj")
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
    (logic/add-claim! "/src/a.clj" "slave-1")

    (let [stats (logic/db-stats)]
      (is (= 2 (:slaves stats)))
      (is (= 1 (:claims stats))))))
