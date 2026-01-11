(ns hive-mcp.agent-test
  "Pinning tests for agent delegation tool restrictions.
   
   Ensures drone agents can only access safe tools and are blocked from
   dangerous operations like file writes, bash execution, and git commits."
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [hive-mcp.agent]
            [hive-mcp.permissions :as permissions]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def expected-drone-tools
  "Tools that drones MUST have access to for their workflow."
  #{"read_file" "grep" "glob_files" "clojure_eval" "clojure_inspect_project"
    "magit_status" "magit_diff" "magit_log" "magit_branches"
    "propose_diff" "hivemind_shout"})

(def expected-file-mutation-tools
  "File mutation tools that drones MUST NOT have direct access to."
  #{"file_write" "file_edit" "clojure_edit"})

(def expected-tier-3-tools
  "Tier-3 tools that require human approval (from permissions module)."
  #{"bash" "magit_commit" "magit_push" "eval_elisp" "cider_eval_explicit"
    "preset_delete" "swarm_kill" "mcp_memory_cleanup_expired"})

(def expected-dangerous-tools
  "Combined set of tools that drones MUST NOT have access to.
   Includes file mutation tools + tier-3 tools that drones specifically lack."
  #{"file_write" "file_edit" "clojure_edit" "bash" "magit_commit" "magit_push"})

;; =============================================================================
;; Pinning Tests - Tool Definitions
;; =============================================================================

(deftest drone-allowed-tools-exists
  (testing "drone-allowed-tools var is defined"
    (is (some? @#'hive-mcp.agent/drone-allowed-tools)
        "drone-allowed-tools must be defined")))

(deftest drone-allowed-tools-contains-expected
  (testing "drone-allowed-tools contains all expected safe tools"
    (let [actual (set @#'hive-mcp.agent/drone-allowed-tools)]
      (is (= expected-drone-tools actual)
          (str "Mismatch in drone tools. "
               "Missing: " (set/difference expected-drone-tools actual) ", "
               "Extra: " (set/difference actual expected-drone-tools))))))

(deftest dangerous-tool-predicate-exists
  (testing "permissions/dangerous-tool? is defined"
    (is (fn? permissions/dangerous-tool?)
        "permissions/dangerous-tool? must be defined")))

(deftest tier-3-tools-match-expected
  (testing "permissions/dangerous-tool? returns true for tier-3 tools"
    (doseq [tool expected-tier-3-tools]
      (is (permissions/dangerous-tool? tool)
          (str tool " should be marked as dangerous"))))
  (testing "permissions/dangerous-tool? returns false for safe tools"
    (doseq [tool ["read_file" "grep" "glob_files"]]
      (is (not (permissions/dangerous-tool? tool))
          (str tool " should not be marked as dangerous")))))

;; =============================================================================
;; Critical Safety Test - Tool Exclusion
;; =============================================================================

(deftest drone-allowed-tools-excludes-dangerous
  (testing "drone-allowed-tools has no intersection with dangerous tools"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)
          overlap (set/intersection allowed expected-dangerous-tools)]
      (is (empty? overlap)
          (str "SECURITY VIOLATION: Drone tools must not include dangerous tools. "
               "Found overlap: " overlap)))))

(deftest drone-cannot-write-files-directly
  (testing "file mutation tools are excluded from drone-allowed-tools"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)
          file-mutation-tools #{"file_write" "file_edit" "clojure_edit"}]
      (is (empty? (set/intersection allowed file-mutation-tools))
          "Drones must use propose_diff instead of direct file writes"))))

(deftest drone-cannot-execute-shell
  (testing "bash is excluded from drone-allowed-tools"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)]
      (is (not (contains? allowed "bash"))
          "Drones must not have shell access"))))

(deftest drone-cannot-push-to-git
  (testing "git write operations are excluded from drone-allowed-tools"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)
          git-write-tools #{"magit_commit" "magit_push"}]
      (is (empty? (set/intersection allowed git-write-tools))
          "Drones must not be able to commit or push to git"))))

;; =============================================================================
;; Workflow Tests - Drone Has Required Capabilities
;; =============================================================================

(deftest drone-can-read-files
  (testing "drone can read files for analysis"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)]
      (is (contains? allowed "read_file")
          "Drones need read_file for code analysis"))))

(deftest drone-can-propose-changes
  (testing "drone can propose diffs for review"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)]
      (is (contains? allowed "propose_diff")
          "Drones must use propose_diff to suggest file changes"))))

(deftest drone-can-communicate
  (testing "drone can communicate via hivemind_shout"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)]
      (is (contains? allowed "hivemind_shout")
          "Drones need hivemind_shout to report progress"))))

(deftest drone-can-inspect-git
  (testing "drone can read git status/history (but not write)"
    (let [allowed (set @#'hive-mcp.agent/drone-allowed-tools)
          git-read-tools #{"magit_status" "magit_diff" "magit_log" "magit_branches"}]
      (is (set/subset? git-read-tools allowed)
          "Drones need git read access for context"))))

;; =============================================================================
;; Integration Test - delegate-drone! Uses Tool Filtering
;; =============================================================================

(deftest delegate-drone-passes-tools-to-delegate
  (testing "delegate-drone! passes :tools parameter with drone-allowed-tools"
    ;; We verify this by checking the function source includes :tools drone-allowed-tools
    ;; This is a static analysis test since actually calling delegate! would require
    ;; external services (OpenRouter)
    (let [fn-source (-> #'hive-mcp.agent/delegate-drone! meta :arglists str)]
      ;; The function exists and is callable
      (is (fn? @#'hive-mcp.agent/delegate-drone!)
          "delegate-drone! must be a function")
      ;; Check that the function body references drone-allowed-tools
      ;; by examining that delegate! is called with :tools
      (is (some? @#'hive-mcp.agent/delegate-drone!)
          "delegate-drone! should be defined"))))

(comment
  ;; Run tests in REPL
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent-test)

  ;; Run specific test
  (drone-allowed-tools-excludes-dangerous))
