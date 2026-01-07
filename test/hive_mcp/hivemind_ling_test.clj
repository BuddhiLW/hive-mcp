(ns hive-mcp.hivemind-ling-test
  "Pinning tests for hivemind ling (delegated agent) result management.
   
   Tests verify:
   - `record-ling-result!` stores results with metadata
   - `get-pending-ling-results` returns only unreviewed entries
   - `mark-ling-reviewed!` changes status correctly
   - `clear-ling-results!` empties the atom
   
   Each test is independent with state reset between tests."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.hivemind :as hivemind]))

;;; Test fixtures

(defn reset-ling-state
  "Fixture to ensure clean ling-results state between tests."
  [f]
  (hivemind/clear-ling-results!)
  (f)
  (hivemind/clear-ling-results!))

(use-fixtures :each reset-ling-state)

;;; record-ling-result! tests

(deftest record-ling-result!-stores-result-test
  (testing "record-ling-result! stores result with required fields"
    (hivemind/record-ling-result! "agent-1" {:output "task completed"})

    (let [results (hivemind/get-pending-ling-results)]
      (is (= 1 (count results)) "Should have one result")
      (is (contains? results "agent-1") "Should be keyed by agent-id")
      (let [entry (get results "agent-1")]
        (is (= {:output "task completed"} (:result entry)))
        (is (number? (:timestamp entry)) "Should have timestamp")
        (is (false? (:reviewed? entry)) "Should be unreviewed initially")))))

(deftest record-ling-result!-overwrites-previous-test
  (testing "record-ling-result! overwrites previous result for same agent"
    (hivemind/record-ling-result! "agent-1" {:output "first"})
    (hivemind/record-ling-result! "agent-1" {:output "second"})

    (let [results (hivemind/get-pending-ling-results)]
      (is (= 1 (count results)) "Should have one result (overwritten)")
      (is (= {:output "second"} (:result (get results "agent-1")))))))

(deftest record-ling-result!-multiple-agents-test
  (testing "record-ling-result! tracks multiple agents independently"
    (hivemind/record-ling-result! "agent-1" {:output "from-1"})
    (hivemind/record-ling-result! "agent-2" {:output "from-2"})
    (hivemind/record-ling-result! "agent-3" {:output "from-3"})

    (let [results (hivemind/get-pending-ling-results)]
      (is (= 3 (count results)))
      (is (= {:output "from-1"} (:result (get results "agent-1"))))
      (is (= {:output "from-2"} (:result (get results "agent-2"))))
      (is (= {:output "from-3"} (:result (get results "agent-3")))))))

(deftest record-ling-result!-timestamp-test
  (testing "record-ling-result! sets timestamp close to current time"
    (let [before (System/currentTimeMillis)]
      (hivemind/record-ling-result! "agent-1" {:output "test"})
      (let [after (System/currentTimeMillis)
            timestamp (:timestamp (get (hivemind/get-pending-ling-results) "agent-1"))]
        (is (>= timestamp before))
        (is (<= timestamp after))))))

;;; get-pending-ling-results tests

(deftest get-pending-ling-results-empty-test
  (testing "get-pending-ling-results returns empty map when no results"
    (is (= {} (hivemind/get-pending-ling-results)))))

(deftest get-pending-ling-results-filters-reviewed-test
  (testing "get-pending-ling-results excludes reviewed results"
    (hivemind/record-ling-result! "agent-1" {:output "pending"})
    (hivemind/record-ling-result! "agent-2" {:output "will-be-reviewed"})

    ;; Mark one as reviewed
    (hivemind/mark-ling-reviewed! "agent-2")

    (let [pending (hivemind/get-pending-ling-results)]
      (is (= 1 (count pending)) "Should only have unreviewed")
      (is (contains? pending "agent-1"))
      (is (not (contains? pending "agent-2"))))))

;;; mark-ling-reviewed! tests

(deftest mark-ling-reviewed!-sets-flag-test
  (testing "mark-ling-reviewed! sets reviewed? to true"
    (hivemind/record-ling-result! "agent-1" {:output "test"})

    ;; Before marking
    (is (false? (:reviewed? (get (hivemind/get-pending-ling-results) "agent-1"))))

    ;; Mark as reviewed
    (hivemind/mark-ling-reviewed! "agent-1")

    ;; After marking - should not appear in pending
    (is (= {} (hivemind/get-pending-ling-results)))

    ;; But the entry still exists in the atom (verify via direct access)
    (let [ling-results-var (resolve 'hive-mcp.hivemind/ling-results)
          all-results @@ling-results-var]
      (is (true? (:reviewed? (get all-results "agent-1")))))))

(deftest mark-ling-reviewed!-idempotent-test
  (testing "mark-ling-reviewed! is idempotent"
    (hivemind/record-ling-result! "agent-1" {:output "test"})
    (hivemind/mark-ling-reviewed! "agent-1")
    (hivemind/mark-ling-reviewed! "agent-1") ; Second call

    ;; Should still be reviewed, no error
    (let [ling-results-var (resolve 'hive-mcp.hivemind/ling-results)
          entry (get @@ling-results-var "agent-1")]
      (is (true? (:reviewed? entry))))))

(deftest mark-ling-reviewed!-nonexistent-agent-test
  (testing "mark-ling-reviewed! handles nonexistent agent gracefully"
    ;; Should not throw
    (hivemind/mark-ling-reviewed! "nonexistent-agent")

    ;; Creates entry with nil values but reviewed? true
    (let [ling-results-var (resolve 'hive-mcp.hivemind/ling-results)
          entry (get @@ling-results-var "nonexistent-agent")]
      (is (true? (:reviewed? entry))))))

;;; clear-ling-results! tests

(deftest clear-ling-results!-empties-atom-test
  (testing "clear-ling-results! removes all entries"
    (hivemind/record-ling-result! "agent-1" {:output "test1"})
    (hivemind/record-ling-result! "agent-2" {:output "test2"})
    (hivemind/mark-ling-reviewed! "agent-1")

    ;; Clear all
    (hivemind/clear-ling-results!)

    ;; Verify empty
    (is (= {} (hivemind/get-pending-ling-results)))
    (let [ling-results-var (resolve 'hive-mcp.hivemind/ling-results)]
      (is (= {} @@ling-results-var)))))

(deftest clear-ling-results!-idempotent-test
  (testing "clear-ling-results! is idempotent"
    (hivemind/clear-ling-results!)
    (hivemind/clear-ling-results!)
    (is (= {} (hivemind/get-pending-ling-results)))))

;;; Integration/workflow tests

(deftest ling-workflow-full-cycle-test
  (testing "Full ling workflow: record → get-pending → review → clear"
    ;; 1. Record multiple results
    (hivemind/record-ling-result! "ling-tdd" {:status "success" :tests-passed 10})
    (hivemind/record-ling-result! "ling-docs" {:status "success" :docs-updated 3})

    ;; 2. Verify pending
    (let [pending (hivemind/get-pending-ling-results)]
      (is (= 2 (count pending))))

    ;; 3. Review one
    (hivemind/mark-ling-reviewed! "ling-tdd")
    (let [pending (hivemind/get-pending-ling-results)]
      (is (= 1 (count pending)))
      (is (contains? pending "ling-docs")))

    ;; 4. Review remaining
    (hivemind/mark-ling-reviewed! "ling-docs")
    (is (= {} (hivemind/get-pending-ling-results)))

    ;; 5. Clear at session end
    (hivemind/clear-ling-results!)
    (let [ling-results-var (resolve 'hive-mcp.hivemind/ling-results)]
      (is (= {} @@ling-results-var)))))

(deftest ling-result-preserves-complex-data-test
  (testing "record-ling-result! preserves complex nested data"
    (let [complex-result {:status "completed"
                          :metrics {:time-ms 1234
                                    :memory-mb 256}
                          :files ["a.clj" "b.clj"]
                          :nested {:deep {:value 42}}}]
      (hivemind/record-ling-result! "agent-complex" complex-result)

      (let [stored (get (hivemind/get-pending-ling-results) "agent-complex")]
        (is (= complex-result (:result stored)))))))
