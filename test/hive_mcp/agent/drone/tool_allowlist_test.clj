(ns hive-mcp.agent.drone.tool-allowlist-test
  "Tests for drone tool allowlist enforcement.

   CLARITY-I: Verifies that tool calls are properly filtered
   by the allowlist before reaching the executor."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.drone.tool-allowlist :as allowlist]))

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defn make-tool-call
  "Create a tool call map for testing."
  ([name] (make-tool-call name {}))
  ([name args]
   {:id (str "call-" name "-" (rand-int 10000))
    :name name
    :arguments args}))

;;; ============================================================
;;; resolve-allowlist tests
;;; ============================================================

(deftest resolve-allowlist-explicit-override
  (testing "Explicit :tool-allowlist takes highest priority"
    (let [custom #{"read_file" "grep"}
          result (allowlist/resolve-allowlist {:tool-allowlist custom
                                               :task-type :testing})]
      (is (= custom result))
      (is (contains? result "read_file"))
      (is (not (contains? result "bash"))))))

(deftest resolve-allowlist-task-type
  (testing "Task-type profile used when no explicit allowlist"
    (let [result (allowlist/resolve-allowlist {:task-type :documentation})]
      ;; :documentation profile is minimal: read_file + core-tools
      (is (contains? result "read_file"))
      (is (contains? result "propose_diff"))
      (is (contains? result "hivemind_shout"))
      ;; Should NOT include tools not in documentation profile
      (is (not (contains? result "grep"))))))

(deftest resolve-allowlist-default-fallback
  (testing "Default allowlist used when no options provided"
    (let [result (allowlist/resolve-allowlist {})]
      (is (= allowlist/default-allowlist result))
      (is (contains? result "read_file"))
      (is (contains? result "bash"))
      (is (contains? result "cider_eval_silent"))
      (is (contains? result "propose_diff")))))

(deftest resolve-allowlist-nil-opts
  (testing "nil task-type and nil allowlist returns default"
    (let [result (allowlist/resolve-allowlist {:tool-allowlist nil
                                               :task-type nil})]
      (is (= allowlist/default-allowlist result)))))

;;; ============================================================
;;; tool-allowed? tests
;;; ============================================================

(deftest tool-allowed-present
  (testing "Tool on allowlist returns true"
    (is (true? (allowlist/tool-allowed? "read_file" #{"read_file" "grep"})))))

(deftest tool-allowed-absent
  (testing "Tool not on allowlist returns false"
    (is (false? (allowlist/tool-allowed? "bash" #{"read_file" "grep"})))))

(deftest tool-allowed-empty-allowlist
  (testing "Empty allowlist rejects everything"
    (is (false? (allowlist/tool-allowed? "read_file" #{})))))

;;; ============================================================
;;; reject-tool-call tests
;;; ============================================================

(deftest reject-tool-call-format
  (testing "Rejection result has correct format"
    (let [result (allowlist/reject-tool-call "call-1" "bash" #{"read_file"})]
      (is (= "tool" (:role result)))
      (is (= "call-1" (:tool_call_id result)))
      (is (= "bash" (:name result)))
      (is (string? (:content result)))
      (is (.contains (:content result) "TOOL REJECTED"))
      (is (.contains (:content result) "bash"))
      (is (.contains (:content result) "read_file")))))

;;; ============================================================
;;; enforce-allowlist tests
;;; ============================================================

(deftest enforce-allowlist-all-allowed
  (testing "All tools on allowlist pass through"
    (let [calls [(make-tool-call "read_file" {:path "/src/foo.clj"})
                 (make-tool-call "grep" {:pattern "defn"})]
          result (allowlist/enforce-allowlist calls #{"read_file" "grep"})]
      (is (= 2 (count (:allowed result))))
      (is (empty? (:rejected result))))))

(deftest enforce-allowlist-all-rejected
  (testing "All tools not on allowlist are rejected"
    (let [calls [(make-tool-call "bash" {:command "rm -rf /"})
                 (make-tool-call "magit_push" {})]
          result (allowlist/enforce-allowlist calls #{"read_file"})]
      (is (empty? (:allowed result)))
      (is (= 2 (count (:rejected result))))
      ;; Verify rejection format
      (doseq [r (:rejected result)]
        (is (= "tool" (:role r)))
        (is (.contains (:content r) "TOOL REJECTED"))))))

(deftest enforce-allowlist-mixed
  (testing "Mixed batch: some allowed, some rejected"
    (let [calls [(make-tool-call "read_file" {:path "/src/foo.clj"})
                 (make-tool-call "bash" {:command "ls"})
                 (make-tool-call "grep" {:pattern "TODO"})
                 (make-tool-call "magit_push" {})]
          al #{"read_file" "grep"}
          result (allowlist/enforce-allowlist calls al)]
      (is (= 2 (count (:allowed result))))
      (is (= 2 (count (:rejected result))))
      ;; Allowed should be read_file and grep
      (is (= #{"read_file" "grep"}
             (set (map :name (:allowed result)))))
      ;; Rejected should be bash and magit_push
      (is (= #{"bash" "magit_push"}
             (set (map :name (:rejected result))))))))

(deftest enforce-allowlist-empty-calls
  (testing "Empty tool calls returns empty results"
    (let [result (allowlist/enforce-allowlist [] #{"read_file"})]
      (is (empty? (:allowed result)))
      (is (empty? (:rejected result))))))

(deftest enforce-allowlist-preserves-call-data
  (testing "Allowed calls preserve original data"
    (let [call (make-tool-call "read_file" {:path "/src/foo.clj"})
          result (allowlist/enforce-allowlist [call] #{"read_file"})
          allowed-call (first (:allowed result))]
      (is (= (:id call) (:id allowed-call)))
      (is (= (:name call) (:name allowed-call)))
      (is (= (:arguments call) (:arguments allowed-call))))))

;;; ============================================================
;;; Default allowlist coverage
;;; ============================================================

(deftest default-allowlist-contains-required-tools
  (testing "Default allowlist includes all documented tools from task spec"
    (let [al allowlist/default-allowlist]
      ;; Core drone tools
      (is (contains? al "read_file"))
      (is (contains? al "file_write"))
      (is (contains? al "grep"))
      (is (contains? al "glob_files"))
      (is (contains? al "bash"))
      (is (contains? al "clojure_eval"))
      (is (contains? al "cider_eval_silent"))
      ;; Drone-specific
      (is (contains? al "propose_diff"))
      (is (contains? al "hivemind_shout")))))

(deftest default-allowlist-excludes-dangerous-tools
  (testing "Default allowlist does NOT include dangerous tools"
    (let [al allowlist/default-allowlist]
      ;; Agent spawning — forbidden for drones
      (is (not (contains? al "swarm_spawn")))
      (is (not (contains? al "swarm_dispatch")))
      (is (not (contains? al "delegate_drone")))
      ;; Memory writes — drones should be read-only for memory
      (is (not (contains? al "mcp_memory_add")))
      ;; Destructive git
      (is (not (contains? al "magit_push")))
      (is (not (contains? al "magit_commit"))))))
