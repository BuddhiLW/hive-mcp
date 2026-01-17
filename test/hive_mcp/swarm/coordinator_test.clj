(ns hive-mcp.swarm.coordinator-test
  "Tests for coordinator lifecycle management in DataScript.

   Covers:
   - Coordinator registration and retrieval
   - Heartbeat updates
   - Status transitions (active → stale → terminated)
   - Stale coordinator cleanup
   - Multi-project coordinator queries"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.swarm.datascript :as ds]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn reset-db-fixture
  "Reset DataScript database before each test."
  [f]
  (ds/reset-conn!)
  (f))

(use-fixtures :each reset-db-fixture)

;;; =============================================================================
;;; Registration Tests
;;; =============================================================================

(deftest register-coordinator-basic-test
  (testing "register-coordinator! creates coordinator with required fields"
    (ds/register-coordinator! "coord-1" {:project "test-project"})
    (let [coord (ds/get-coordinator "coord-1")]
      (is (some? coord) "Coordinator should exist")
      (is (= "coord-1" (:coordinator/id coord)))
      (is (= "test-project" (:coordinator/project coord)))
      (is (= :active (:coordinator/status coord)) "Initial status should be :active")
      (is (some? (:coordinator/pid coord)) "PID should be auto-populated")
      (is (some? (:coordinator/session-id coord)) "Session ID should be auto-generated")
      (is (some? (:coordinator/started-at coord)) "Started timestamp should be set")
      (is (some? (:coordinator/heartbeat-at coord)) "Heartbeat timestamp should be set"))))

(deftest register-coordinator-custom-opts-test
  (testing "register-coordinator! respects custom options"
    (ds/register-coordinator! "coord-2"
                              {:project "custom-project"
                               :pid 12345
                               :session-id "custom-session-uuid"})
    (let [coord (ds/get-coordinator "coord-2")]
      (is (= 12345 (:coordinator/pid coord)))
      (is (= "custom-session-uuid" (:coordinator/session-id coord))))))

(deftest register-coordinator-upsert-test
  (testing "register-coordinator! upserts on same ID (unique identity)"
    (ds/register-coordinator! "coord-3" {:project "project-v1"})
    (let [coord-v1 (ds/get-coordinator "coord-3")]
      (ds/register-coordinator! "coord-3" {:project "project-v2"})
      (let [coord-v2 (ds/get-coordinator "coord-3")]
        (is (= "project-v2" (:coordinator/project coord-v2)))
        (is (= :active (:coordinator/status coord-v2)))))))

;;; =============================================================================
;;; Query Tests
;;; =============================================================================

(deftest get-coordinator-not-found-test
  (testing "get-coordinator returns nil for non-existent ID"
    (is (nil? (ds/get-coordinator "non-existent")))))

(deftest get-all-coordinators-test
  (testing "get-all-coordinators returns all registered coordinators"
    (ds/register-coordinator! "coord-a" {:project "proj-a"})
    (ds/register-coordinator! "coord-b" {:project "proj-b"})
    (ds/register-coordinator! "coord-c" {:project "proj-c"})
    (let [all (ds/get-all-coordinators)]
      (is (= 3 (count all)))
      (is (= #{"coord-a" "coord-b" "coord-c"}
             (set (map :coordinator/id all)))))))

(deftest get-coordinators-by-status-test
  (testing "get-coordinators-by-status filters correctly"
    (ds/register-coordinator! "active-1" {:project "p1"})
    (ds/register-coordinator! "active-2" {:project "p2"})
    (ds/register-coordinator! "stale-1" {:project "p3"})
    (ds/mark-coordinator-stale! "stale-1")

    (let [active (ds/get-coordinators-by-status :active)
          stale (ds/get-coordinators-by-status :stale)]
      (is (= 2 (count active)))
      (is (= #{"active-1" "active-2"} (set (map :coordinator/id active))))
      (is (= 1 (count stale)))
      (is (= "stale-1" (:coordinator/id (first stale)))))))

(deftest get-coordinators-for-project-test
  (testing "get-coordinators-for-project filters by project"
    (ds/register-coordinator! "hive-1" {:project "hive-mcp"})
    (ds/register-coordinator! "hive-2" {:project "hive-mcp"})
    (ds/register-coordinator! "other-1" {:project "other-project"})

    (let [hive-coords (ds/get-coordinators-for-project "hive-mcp")]
      (is (= 2 (count hive-coords)))
      (is (every? #(= "hive-mcp" (:coordinator/project %)) hive-coords)))))

;;; =============================================================================
;;; Heartbeat Tests
;;; =============================================================================

(deftest update-heartbeat-basic-test
  (testing "update-heartbeat! updates timestamp"
    (ds/register-coordinator! "heartbeat-test" {:project "test"})
    (let [before (ds/get-coordinator "heartbeat-test")
          _ (Thread/sleep 10)
          _ (ds/update-heartbeat! "heartbeat-test")
          after (ds/get-coordinator "heartbeat-test")]
      (is (.after (:coordinator/heartbeat-at after)
                  (:coordinator/heartbeat-at before))
          "Heartbeat timestamp should be updated"))))

(deftest update-heartbeat-reactivates-stale-test
  (testing "update-heartbeat! reactivates stale coordinator"
    (ds/register-coordinator! "stale-reactivate" {:project "test"})
    (ds/mark-coordinator-stale! "stale-reactivate")
    (is (= :stale (:coordinator/status (ds/get-coordinator "stale-reactivate"))))

    (ds/update-heartbeat! "stale-reactivate")
    (is (= :active (:coordinator/status (ds/get-coordinator "stale-reactivate")))
        "Heartbeat should reactivate stale coordinator")))

(deftest update-heartbeat-nonexistent-test
  (testing "update-heartbeat! returns nil for non-existent coordinator"
    (is (nil? (ds/update-heartbeat! "non-existent")))))

;;; =============================================================================
;;; Status Transition Tests
;;; =============================================================================

(deftest mark-coordinator-terminated-test
  (testing "mark-coordinator-terminated! sets status to :terminated"
    (ds/register-coordinator! "to-terminate" {:project "test"})
    (ds/mark-coordinator-terminated! "to-terminate")
    (is (= :terminated (:coordinator/status (ds/get-coordinator "to-terminate"))))))

(deftest mark-coordinator-stale-test
  (testing "mark-coordinator-stale! sets status to :stale"
    (ds/register-coordinator! "to-stale" {:project "test"})
    (ds/mark-coordinator-stale! "to-stale")
    (is (= :stale (:coordinator/status (ds/get-coordinator "to-stale"))))))

;;; =============================================================================
;;; Cleanup Tests
;;; =============================================================================

(deftest cleanup-stale-coordinators-finds-stale-test
  (testing "cleanup-stale-coordinators! marks old heartbeats as stale"
    ;; Register coordinator with old heartbeat (simulated by direct transact)
    (ds/register-coordinator! "old-coord" {:project "test"})
    ;; Manually set heartbeat to 5 minutes ago
    (let [c (ds/get-conn)
          db @c
          eid (:db/id (datascript.core/entity db [:coordinator/id "old-coord"]))
          old-time (java.util.Date. (- (System/currentTimeMillis) (* 5 60 1000)))]
      (datascript.core/transact! c [{:db/id eid :coordinator/heartbeat-at old-time}]))

    ;; Register fresh coordinator
    (ds/register-coordinator! "fresh-coord" {:project "test"})

    ;; Cleanup with 2-minute threshold (default)
    (let [stale-ids (ds/cleanup-stale-coordinators!)]
      (is (= ["old-coord"] (vec stale-ids))
          "Should mark old-coord as stale")
      (is (= :stale (:coordinator/status (ds/get-coordinator "old-coord"))))
      (is (= :active (:coordinator/status (ds/get-coordinator "fresh-coord")))
          "Fresh coordinator should remain active"))))

(deftest cleanup-stale-coordinators-custom-threshold-test
  (testing "cleanup-stale-coordinators! respects custom threshold"
    (ds/register-coordinator! "threshold-test" {:project "test"})
    ;; Set heartbeat to 1 second ago
    (let [c (ds/get-conn)
          db @c
          eid (:db/id (datascript.core/entity db [:coordinator/id "threshold-test"]))
          old-time (java.util.Date. (- (System/currentTimeMillis) 1000))]
      (datascript.core/transact! c [{:db/id eid :coordinator/heartbeat-at old-time}]))

    ;; Cleanup with 500ms threshold
    (let [stale-ids (ds/cleanup-stale-coordinators! {:threshold-ms 500})]
      (is (= ["threshold-test"] (vec stale-ids))))))

(deftest cleanup-stale-coordinators-skips-already-stale-test
  (testing "cleanup-stale-coordinators! skips already stale/terminated coordinators"
    (ds/register-coordinator! "already-stale" {:project "test"})
    (ds/mark-coordinator-stale! "already-stale")

    (ds/register-coordinator! "already-terminated" {:project "test"})
    (ds/mark-coordinator-terminated! "already-terminated")

    ;; Set old heartbeats
    (let [c (ds/get-conn)
          db @c
          old-time (java.util.Date. (- (System/currentTimeMillis) (* 5 60 1000)))]
      (doseq [id ["already-stale" "already-terminated"]]
        (when-let [eid (:db/id (datascript.core/entity db [:coordinator/id id]))]
          (datascript.core/transact! c [{:db/id eid :coordinator/heartbeat-at old-time}]))))

    ;; Cleanup should find nothing (both are already non-active)
    (let [stale-ids (ds/cleanup-stale-coordinators!)]
      (is (nil? stale-ids) "Should not mark already non-active coordinators"))))

;;; =============================================================================
;;; Remove Tests
;;; =============================================================================

(deftest remove-coordinator-test
  (testing "remove-coordinator! deletes the entity"
    (ds/register-coordinator! "to-remove" {:project "test"})
    (is (some? (ds/get-coordinator "to-remove")))

    (ds/remove-coordinator! "to-remove")
    (is (nil? (ds/get-coordinator "to-remove")))))

(deftest remove-coordinator-nonexistent-test
  (testing "remove-coordinator! returns nil for non-existent coordinator"
    (is (nil? (ds/remove-coordinator! "non-existent")))))

;;; =============================================================================
;;; Integration Tests
;;; =============================================================================

(deftest coordinator-lifecycle-integration-test
  (testing "Full coordinator lifecycle: register → heartbeat → terminate → remove"
    ;; 1. Register
    (ds/register-coordinator! "lifecycle-test" {:project "integration"})
    (is (= :active (:coordinator/status (ds/get-coordinator "lifecycle-test"))))

    ;; 2. Heartbeat
    (Thread/sleep 10)
    (ds/update-heartbeat! "lifecycle-test")
    (let [coord (ds/get-coordinator "lifecycle-test")]
      (is (= :active (:coordinator/status coord))))

    ;; 3. Mark stale (simulating crash detection)
    (ds/mark-coordinator-stale! "lifecycle-test")
    (is (= :stale (:coordinator/status (ds/get-coordinator "lifecycle-test"))))

    ;; 4. Reactivate via heartbeat (simulating recovery)
    (ds/update-heartbeat! "lifecycle-test")
    (is (= :active (:coordinator/status (ds/get-coordinator "lifecycle-test"))))

    ;; 5. Graceful termination
    (ds/mark-coordinator-terminated! "lifecycle-test")
    (is (= :terminated (:coordinator/status (ds/get-coordinator "lifecycle-test"))))

    ;; 6. Cleanup
    (ds/remove-coordinator! "lifecycle-test")
    (is (nil? (ds/get-coordinator "lifecycle-test")))))

(deftest multi-project-coordinator-test
  (testing "Multiple coordinators across different projects"
    (ds/register-coordinator! "proj1-coord1" {:project "project-1"})
    (ds/register-coordinator! "proj1-coord2" {:project "project-1"})
    (ds/register-coordinator! "proj2-coord1" {:project "project-2"})

    ;; Mark one as stale
    (ds/mark-coordinator-stale! "proj1-coord2")

    ;; Query by project
    (let [proj1 (ds/get-coordinators-for-project "project-1")
          proj2 (ds/get-coordinators-for-project "project-2")]
      (is (= 2 (count proj1)))
      (is (= 1 (count proj2))))

    ;; Query active in project-1
    (let [active (ds/get-coordinators-by-status :active)]
      (is (= 1 (count (filter #(= "project-1" (:coordinator/project %)) active)))
          "project-1 should have 1 active coordinator (the other is stale)"))))
