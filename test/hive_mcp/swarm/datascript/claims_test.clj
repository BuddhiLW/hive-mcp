;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.datascript.claims-test
  "Tests for wait-queue and file claim management.

   Tests the file-claim event cascade Part 2 implementation:
   - Wait-queue CRUD operations
   - Wait-queue queries for coeffects
   - Integration with claim release events"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.datascript.claims :as claims]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.datascript.queries :as queries]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-db-fixture
  "Reset DataScript connection before each test."
  [f]
  (conn/reset-conn!)
  (f))

(use-fixtures :each reset-db-fixture)

;; =============================================================================
;; Wait-Queue CRUD Tests
;; =============================================================================

(deftest add-to-wait-queue-test
  (testing "Can add a ling to wait queue for a file"
    (let [result (claims/add-to-wait-queue! "ling-1" "/src/core.clj")]
      (is (some? result) "Should return transaction report")
      (is (map? result))))

  (testing "Can add multiple lings waiting for same file"
    (claims/add-to-wait-queue! "ling-1" "/src/core.clj")
    (claims/add-to-wait-queue! "ling-2" "/src/core.clj")
    (let [waiting (claims/get-waiting-lings "/src/core.clj")]
      (is (= 2 (count waiting)))
      (is (contains? (set (map :ling-id waiting)) "ling-1"))
      (is (contains? (set (map :ling-id waiting)) "ling-2"))))

  (testing "Can add same ling waiting for multiple files"
    (claims/add-to-wait-queue! "ling-multi" "/src/a.clj")
    (claims/add-to-wait-queue! "ling-multi" "/src/b.clj")
    (let [files (claims/get-ling-wait-queue "ling-multi")]
      (is (= 2 (count files)))
      (is (contains? (set files) "/src/a.clj"))
      (is (contains? (set files) "/src/b.clj")))))

(deftest remove-from-wait-queue-test
  (testing "Can remove ling from wait queue for specific file"
    (claims/add-to-wait-queue! "ling-1" "/src/core.clj")
    (claims/add-to-wait-queue! "ling-1" "/src/other.clj")
    (claims/remove-from-wait-queue! "ling-1" "/src/core.clj")
    (let [remaining (claims/get-ling-wait-queue "ling-1")]
      (is (= 1 (count remaining)))
      (is (= "/src/other.clj" (first remaining)))))

  (testing "Removing non-existent entry is safe (no-op)"
    (is (nil? (claims/remove-from-wait-queue! "ghost" "/nonexistent.clj")))))

(deftest remove-all-from-wait-queue-for-file-test
  (testing "Can remove all lings waiting for a specific file"
    (claims/add-to-wait-queue! "ling-1" "/src/contested.clj")
    (claims/add-to-wait-queue! "ling-2" "/src/contested.clj")
    (claims/add-to-wait-queue! "ling-3" "/src/contested.clj")
    (let [removed-count (claims/remove-all-from-wait-queue-for-file! "/src/contested.clj")]
      (is (= 3 removed-count))
      (is (empty? (claims/get-waiting-lings "/src/contested.clj"))))))

;; =============================================================================
;; Wait-Queue Query Tests (for coeffects)
;; =============================================================================

(deftest get-waiting-lings-test
  (testing "Returns empty vector when no lings waiting"
    (let [waiting (claims/get-waiting-lings "/src/nobody-wants-this.clj")]
      (is (vector? waiting))
      (is (empty? waiting))))

  (testing "Returns ling info with queued-at timestamp"
    (claims/add-to-wait-queue! "ling-1" "/src/popular.clj")
    (let [waiting (claims/get-waiting-lings "/src/popular.clj")
          entry (first waiting)]
      (is (= 1 (count waiting)))
      (is (= "ling-1" (:ling-id entry)))
      (is (inst? (:queued-at entry)))))

  (testing "Returns lings in FIFO order (earliest first)"
    (claims/add-to-wait-queue! "ling-first" "/src/fifo.clj")
    (Thread/sleep 10) ;; Ensure different timestamps
    (claims/add-to-wait-queue! "ling-second" "/src/fifo.clj")
    (let [waiting (claims/get-waiting-lings "/src/fifo.clj")]
      (is (= "ling-first" (:ling-id (first waiting))))
      (is (= "ling-second" (:ling-id (second waiting)))))))

(deftest get-ling-wait-queue-test
  (testing "Returns empty vector when ling not waiting"
    (let [files (claims/get-ling-wait-queue "idle-ling")]
      (is (vector? files))
      (is (empty? files))))

  (testing "Returns all files ling is waiting for"
    (claims/add-to-wait-queue! "busy-ling" "/src/a.clj")
    (claims/add-to-wait-queue! "busy-ling" "/src/b.clj")
    (claims/add-to-wait-queue! "busy-ling" "/src/c.clj")
    (let [files (claims/get-ling-wait-queue "busy-ling")]
      (is (= 3 (count files)))
      (is (= #{"/src/a.clj" "/src/b.clj" "/src/c.clj"} (set files))))))

;; =============================================================================
;; Integration: Claim Release Event Dispatch
;; =============================================================================

(deftest release-claim-dispatches-event-test
  (testing "release-claim! dispatches :claim/file-released event when handler registered"
    ;; This test verifies that the modified release-claim! function
    ;; dispatches an event when a claim is released.
    ;;
    ;; Setup: We need a slave and a claim
    (lings/add-slave! "claiming-ling" {:name "Claiming Ling"})
    (lings/claim-file! "/src/claimed.clj" "claiming-ling")

    ;; Verify claim exists
    (is (queries/has-conflict? "other-ling" ["/src/claimed.clj"]))

    ;; Add a ling to wait queue (this will be notified on release)
    (claims/add-to-wait-queue! "waiting-ling" "/src/claimed.clj")

    ;; Force load events.core so vars exist for with-redefs
    (require 'hive-mcp.events.core)

    ;; Track if event was dispatched (we'll use an atom as spy)
    (let [events-captured (atom [])
          handler-registered-var (resolve 'hive-mcp.events.core/handler-registered?)
          dispatch-var (resolve 'hive-mcp.events.core/dispatch)
          orig-handler-registered @handler-registered-var
          orig-dispatch @dispatch-var]
      ;; Use alter-var-root to temporarily replace the functions
      (try
        (alter-var-root handler-registered-var (constantly (constantly true)))
        (alter-var-root dispatch-var (constantly (fn [event]
                                                   (swap! events-captured conj event))))
        ;; Release the claim
        (lings/release-claim! "/src/claimed.clj")

        ;; Verify event was dispatched
        (is (= 1 (count @events-captured)))
        (let [[event-id data] (first @events-captured)]
          (is (= :claim/file-released event-id))
          (is (= "/src/claimed.clj" (:file data))))
        (finally
          (alter-var-root handler-registered-var (constantly orig-handler-registered))
          (alter-var-root dispatch-var (constantly orig-dispatch))))))

  (testing "release-claim! does not dispatch when no handler registered"
    ;; Setup
    (lings/add-slave! "claiming-ling-2" {:name "Claiming Ling 2"})
    (lings/claim-file! "/src/claimed2.clj" "claiming-ling-2")

    (let [events-captured (atom [])
          handler-registered-var (resolve 'hive-mcp.events.core/handler-registered?)
          dispatch-var (resolve 'hive-mcp.events.core/dispatch)
          orig-handler-registered @handler-registered-var
          orig-dispatch @dispatch-var]
      (try
        (alter-var-root handler-registered-var (constantly (constantly false)))
        (alter-var-root dispatch-var (constantly (fn [event]
                                                   (swap! events-captured conj event))))
        ;; Release the claim
        (lings/release-claim! "/src/claimed2.clj")

        ;; Verify event was NOT dispatched
        (is (= 0 (count @events-captured)))
        (finally
          (alter-var-root handler-registered-var (constantly orig-handler-registered))
          (alter-var-root dispatch-var (constantly orig-dispatch)))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest duplicate-wait-queue-entry-test
  (testing "Adding same ling/file pair twice creates only one entry"
    (claims/add-to-wait-queue! "ling-dup" "/src/same.clj")
    (claims/add-to-wait-queue! "ling-dup" "/src/same.clj")
    (let [waiting (claims/get-waiting-lings "/src/same.clj")]
      ;; Should have only one entry due to upsert behavior
      (is (= 1 (count waiting))))))
