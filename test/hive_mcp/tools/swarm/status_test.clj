(ns hive-mcp.tools.swarm.status-test
  "Tests for swarm status handlers - broadcast fix verification.

   BUG FIX: hivemind_broadcast/swarm_broadcast was silently succeeding
   even when no slaves were available. Now returns error with clear message.

   Kanban: 20260130114548"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.status :as status]
            [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.emacsclient :as ec]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; handle-swarm-broadcast Tests
;; =============================================================================

(deftest handle-swarm-broadcast-no-targets-returns-error
  (testing "broadcast returns error when no slaves available (bug fix)"
    (with-redefs [core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    ;; Simulate elisp returning empty list (no slaves)
                    {:success true :result "[]" :timed-out false})]
      (let [result (status/handle-swarm-broadcast {:prompt "test prompt"})
            parsed (json/read-str (:text result) :key-fn keyword)]
        ;; Should be an error, not success
        (is (:isError result)
            "Should return isError when no targets")
        (is (= "no-targets" (:error parsed))
            "Error type should be 'no-targets'")
        (is (zero? (:delivered-count parsed))
            "Delivered count should be 0")
        (is (string? (:message parsed))
            "Should have helpful error message")))))

(deftest handle-swarm-broadcast-success-returns-count
  (testing "broadcast returns delivery count on success"
    (with-redefs [core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    ;; Simulate elisp returning list of task-ids
                    {:success true
                     :result "[\"task-1\", \"task-2\", \"task-3\"]"
                     :timed-out false})]
      (let [result (status/handle-swarm-broadcast {:prompt "test prompt"})
            parsed (json/read-str (:text result) :key-fn keyword)]
        ;; Should NOT be an error
        (is (not (:isError result))
            "Should not be an error when slaves received broadcast")
        (is (= 3 (:delivered-count parsed))
            "Should report correct delivery count")
        (is (= ["task-1" "task-2" "task-3"] (:task-ids parsed))
            "Should include task IDs")
        (is (string? (:message parsed))
            "Should have success message")))))

(deftest handle-swarm-broadcast-timeout
  (testing "broadcast returns timeout error when elisp times out"
    (with-redefs [core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    {:success false :result nil :timed-out true})]
      (let [result (status/handle-swarm-broadcast {:prompt "test"})
            parsed (json/read-str (:text result) :key-fn keyword)]
        (is (:isError result)
            "Should return error on timeout")
        (is (= "timeout" (:status parsed))
            "Status should be timeout")))))

(deftest handle-swarm-broadcast-addon-not-loaded
  (testing "broadcast returns error when swarm addon not available"
    (with-redefs [core/swarm-addon-available? (constantly false)]
      (let [result (status/handle-swarm-broadcast {:prompt "test"})]
        (is (:isError result)
            "Should return error when addon not loaded")
        (is (re-find #"not loaded" (:text result))
            "Should mention addon not loaded")))))
