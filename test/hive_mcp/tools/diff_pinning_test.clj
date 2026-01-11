(ns hive-mcp.tools.diff-pinning-test
  "Pinning tests for drone diff workflow tools.
   
   Tests verify:
   - propose_diff stores diffs in pending atom
   - list_proposed_diffs returns all pending diffs
   - apply_diff applies changes and removes from pending
   - reject_diff discards without applying
   
   Uses with-redefs to mock file operations for isolated unit testing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [hive-mcp.tools.diff :as diff]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn gen-diff-id
  "Generate unique diff ID for testing."
  []
  (str "diff-" (java.util.UUID/randomUUID)))

(defn make-test-diff
  "Create a test diff proposal."
  [& {:keys [id file-path old-content new-content description drone-id]
      :or {id (gen-diff-id)
           file-path "/test/file.clj"
           old-content "(defn foo [] 1)"
           new-content "(defn foo [] 2)"
           description "Fix return value"
           drone-id "drone-1"}}]
  {:id id
   :file-path file-path
   :old-content old-content
   :new-content new-content
   :description description
   :drone-id drone-id})

(defn parse-response-text
  "Parse the JSON text from a handler response."
  [response]
  (json/read-str (:text response) :key-fn keyword))

(defn clear-pending-diffs!
  "Clear all pending diffs for test isolation."
  []
  (reset! diff/pending-diffs {}))

(defn mock-validate-path
  "Mock path validation to always succeed for testing."
  [file-path]
  {:valid true :resolved-path file-path})

(defn mock-file-exists
  "Create a mock File that reports .exists as true."
  [path]
  (proxy [java.io.File] [path]
    (exists [] true)
    (getParentFile [] nil)))

(use-fixtures :each (fn [f]
                      (clear-pending-diffs!)
                      ;; Mock path validation to allow test paths
                      (with-redefs [diff/validate-diff-path mock-validate-path]
                        (f))
                      (clear-pending-diffs!)))

;; =============================================================================
;; Test: Response Format Contract
;; =============================================================================

(deftest test-response-format-success
  (testing "Success response has correct structure"
    (let [response (diff/handle-propose-diff
                    {:file_path "/test/file.clj"
                     :old_content "(defn foo [] 1)"
                     :new_content "(defn foo [] 2)"
                     :description "Fix return value"
                     :drone_id "drone-1"})]
      (is (= "text" (:type response)))
      (is (string? (:text response)))
      (is (nil? (:isError response))))))

(deftest test-response-format-error
  (testing "Error response has correct structure when missing required fields"
    (let [response (diff/handle-propose-diff
                    {:file_path "/test/file.clj"})]
      (is (= "text" (:type response)))
      (is (string? (:text response)))
      (is (true? (:isError response))))))

;; =============================================================================
;; Test: handle-propose-diff
;; =============================================================================

(deftest test-propose-diff-stores-in-atom
  (testing "propose_diff stores diff in pending-diffs atom"
    (let [response (diff/handle-propose-diff
                    {:file_path "/src/core.clj"
                     :old_content "(defn add [a b] (+ a b))"
                     :new_content "(defn add [a b]\n  \"Add two numbers.\"\n  (+ a b))"
                     :description "Add docstring"
                     :drone_id "drone-worker-1"})
          parsed (parse-response-text response)]
      (is (nil? (:isError response)))
      (is (string? (:id parsed)))
      (is (= "pending" (:status parsed)))
      ;; Verify stored in atom
      (is (= 1 (count @diff/pending-diffs)))
      (let [stored (get @diff/pending-diffs (:id parsed))]
        (is (= "/src/core.clj" (:file-path stored)))
        (is (= "Add docstring" (:description stored)))
        (is (= "drone-worker-1" (:drone-id stored)))))))

(deftest test-propose-diff-generates-unified-diff
  (testing "propose_diff generates unified diff format"
    (let [response (diff/handle-propose-diff
                    {:file_path "/src/core.clj"
                     :old_content "line1\nline2\nline3"
                     :new_content "line1\nmodified\nline3"
                     :description "Modify line 2"
                     :drone_id "drone-1"})
          parsed (parse-response-text response)]
      (is (nil? (:isError response)))
      (let [stored (get @diff/pending-diffs (:id parsed))]
        (is (string? (:unified-diff stored)))
        (is (re-find #"-line2" (:unified-diff stored)))
        (is (re-find #"\+modified" (:unified-diff stored)))))))

(deftest test-propose-diff-missing-required-fields
  (testing "Returns error when required fields missing"
    ;; Missing new_content
    (let [response (diff/handle-propose-diff
                    {:file_path "/test/file.clj"
                     :old_content "old"
                     :drone_id "drone-1"})
          parsed (parse-response-text response)]
      (is (true? (:isError response)))
      (is (re-find #"(?i)required|missing" (:error parsed))))
    ;; Missing file_path
    (let [response (diff/handle-propose-diff
                    {:old_content "old"
                     :new_content "new"
                     :drone_id "drone-1"})
          parsed (parse-response-text response)]
      (is (true? (:isError response))))))

(deftest test-propose-diff-multiple-diffs
  (testing "Can store multiple pending diffs"
    (diff/handle-propose-diff
     {:file_path "/src/a.clj" :old_content "a" :new_content "a2"
      :description "Fix A" :drone_id "drone-1"})
    (diff/handle-propose-diff
     {:file_path "/src/b.clj" :old_content "b" :new_content "b2"
      :description "Fix B" :drone_id "drone-2"})
    (diff/handle-propose-diff
     {:file_path "/src/c.clj" :old_content "c" :new_content "c2"
      :description "Fix C" :drone_id "drone-1"})
    (is (= 3 (count @diff/pending-diffs)))))

;; =============================================================================
;; Test: handle-list-proposed-diffs
;; =============================================================================

(deftest test-list-proposed-diffs-empty
  (testing "Returns empty list when no pending diffs"
    (let [response (diff/handle-list-proposed-diffs {})
          parsed (parse-response-text response)]
      (is (nil? (:isError response)))
      (is (= [] (:diffs parsed)))
      (is (= 0 (:count parsed))))))

(deftest test-list-proposed-diffs-with-entries
  (testing "Returns all pending diffs"
    ;; Add some diffs
    (diff/handle-propose-diff
     {:file_path "/src/a.clj" :old_content "a" :new_content "a2"
      :description "Fix A" :drone_id "drone-1"})
    (diff/handle-propose-diff
     {:file_path "/src/b.clj" :old_content "b" :new_content "b2"
      :description "Fix B" :drone_id "drone-2"})

    (let [response (diff/handle-list-proposed-diffs {})
          parsed (parse-response-text response)]
      (is (nil? (:isError response)))
      (is (= 2 (:count parsed)))
      (is (= 2 (count (:diffs parsed))))
      ;; Check each diff has required fields
      (doseq [d (:diffs parsed)]
        (is (contains? d :id))
        (is (contains? d :file-path))
        (is (contains? d :description))
        (is (contains? d :drone-id))
        (is (contains? d :unified-diff))))))

(deftest test-list-proposed-diffs-filter-by-drone
  (testing "Can filter diffs by drone_id"
    (diff/handle-propose-diff
     {:file_path "/src/a.clj" :old_content "a" :new_content "a2"
      :description "Fix A" :drone_id "drone-1"})
    (diff/handle-propose-diff
     {:file_path "/src/b.clj" :old_content "b" :new_content "b2"
      :description "Fix B" :drone_id "drone-2"})
    (diff/handle-propose-diff
     {:file_path "/src/c.clj" :old_content "c" :new_content "c2"
      :description "Fix C" :drone_id "drone-1"})

    (let [response (diff/handle-list-proposed-diffs {:drone_id "drone-1"})
          parsed (parse-response-text response)]
      (is (nil? (:isError response)))
      (is (= 2 (:count parsed)))
      (is (every? #(= "drone-1" (:drone-id %)) (:diffs parsed))))))

;; =============================================================================
;; Test: handle-apply-diff
;; =============================================================================

(deftest test-apply-diff-success
  (testing "apply_diff applies changes and removes from pending"
    ;; Setup: propose a diff
    (let [propose-response (diff/handle-propose-diff
                            {:file_path "/src/test.clj"
                             :old_content "(defn test [] nil)"
                             :new_content "(defn test [] :ok)"
                             :description "Return :ok"
                             :drone_id "drone-1"})
          diff-id (:id (parse-response-text propose-response))
          file-content (atom "(defn test [] nil)")]
      ;; Mock file operations including io/file for .exists check
      (with-redefs [io/file mock-file-exists
                    slurp (fn [_] @file-content)
                    spit (fn [_ content] (reset! file-content content))]
        (let [apply-response (diff/handle-apply-diff {:diff_id diff-id})
              parsed (parse-response-text apply-response)]
          (is (nil? (:isError apply-response)))
          (is (= "applied" (:status parsed)))
          (is (= diff-id (:id parsed)))
          ;; Verify removed from pending
          (is (= 0 (count @diff/pending-diffs)))
          ;; Verify file was updated
          (is (= "(defn test [] :ok)" @file-content)))))))

(deftest test-apply-diff-not-found
  (testing "Returns error when diff_id not found"
    (let [response (diff/handle-apply-diff {:diff_id "nonexistent-id"})
          parsed (parse-response-text response)]
      (is (true? (:isError response)))
      (is (re-find #"(?i)not found" (:error parsed))))))

(deftest test-apply-diff-content-mismatch
  (testing "Returns error when old_content not found in file"
    (let [propose-response (diff/handle-propose-diff
                            {:file_path "/src/test.clj"
                             :old_content "(defn test [] nil)"
                             :new_content "(defn test [] :ok)"
                             :description "Return :ok"
                             :drone_id "drone-1"})
          diff-id (:id (parse-response-text propose-response))]
      ;; Mock file with different content - old_content won't be found
      (with-redefs [slurp (fn [_] "(defn test [] :already-changed)")]
        (let [apply-response (diff/handle-apply-diff {:diff_id diff-id})
              parsed (parse-response-text apply-response)]
          (is (true? (:isError apply-response)))
          (is (re-find #"(?i)not found|modified" (:error parsed)))
          ;; Diff should still be pending (not removed on error)
          (is (= 1 (count @diff/pending-diffs))))))))

(deftest test-apply-diff-missing-diff-id
  (testing "Returns error when diff_id not provided"
    (let [response (diff/handle-apply-diff {})
          parsed (parse-response-text response)]
      (is (true? (:isError response)))
      (is (re-find #"(?i)required|missing" (:error parsed))))))

(deftest test-apply-diff-substring-replacement
  (testing "apply_diff replaces old_content substring within larger file"
    (let [propose-response (diff/handle-propose-diff
                            {:file_path "/src/test.clj"
                             :old_content "(defn foo [] 1)"
                             :new_content "(defn foo [] 42)"
                             :description "Change return value"
                             :drone_id "drone-1"})
          diff-id (:id (parse-response-text propose-response))
          ;; File contains the target function plus other code
          full-file-content "(ns my.ns)\n\n(defn foo [] 1)\n\n(defn bar [] 2)"]
      (with-redefs [io/file mock-file-exists
                    slurp (fn [_] full-file-content)
                    spit (fn [_ content]
                           ;; Verify only foo was changed, bar untouched
                           (is (= "(ns my.ns)\n\n(defn foo [] 42)\n\n(defn bar [] 2)" content)))]
        (let [apply-response (diff/handle-apply-diff {:diff_id diff-id})
              parsed (parse-response-text apply-response)]
          (is (nil? (:isError apply-response)))
          (is (= "applied" (:status parsed))))))))

(deftest test-apply-diff-multiple-matches-rejected
  (testing "apply_diff rejects when old_content appears multiple times"
    (let [propose-response (diff/handle-propose-diff
                            {:file_path "/src/test.clj"
                             :old_content "(defn foo [] 1)"
                             :new_content "(defn foo [] 42)"
                             :description "Change return value"
                             :drone_id "drone-1"})
          diff-id (:id (parse-response-text propose-response))
          ;; File contains foo TWICE - ambiguous
          ambiguous-content "(defn foo [] 1)\n\n(defn bar [] 2)\n\n(defn foo [] 1)"]
      (with-redefs [io/file mock-file-exists
                    slurp (fn [_] ambiguous-content)]
        (let [apply-response (diff/handle-apply-diff {:diff_id diff-id})
              parsed (parse-response-text apply-response)]
          (is (true? (:isError apply-response)))
          (is (re-find #"(?i)multiple|ambiguous" (:error parsed)))
          ;; Diff should remain pending
          (is (= 1 (count @diff/pending-diffs))))))))

(deftest test-sandbox-path-translation
  (testing "translate-sandbox-path converts /tmp/fs-N/ paths to project paths"
    (with-redefs [diff/get-project-root (constantly "/home/user/project")]
      (is (= "/home/user/project/src/foo.clj"
             (diff/translate-sandbox-path "/tmp/fs-1/src/foo.clj")))
      (is (= "/home/user/project/test/bar_test.clj"
             (diff/translate-sandbox-path "/tmp/fs-42/test/bar_test.clj")))
      ;; Non-sandbox paths pass through unchanged
      (is (= "/home/user/project/src/foo.clj"
             (diff/translate-sandbox-path "/home/user/project/src/foo.clj")))
      (is (= "src/relative.clj"
             (diff/translate-sandbox-path "src/relative.clj"))))))

;; =============================================================================
;; Test: handle-reject-diff
;; =============================================================================

(deftest test-reject-diff-success
  (testing "reject_diff removes diff without applying"
    (let [propose-response (diff/handle-propose-diff
                            {:file_path "/src/test.clj"
                             :old_content "old"
                             :new_content "new"
                             :description "Change"
                             :drone_id "drone-1"})
          diff-id (:id (parse-response-text propose-response))
          file-content (atom "old")]
      (with-redefs [slurp (fn [_] @file-content)
                    spit (fn [_ content] (reset! file-content content))]
        (let [reject-response (diff/handle-reject-diff {:diff_id diff-id})
              parsed (parse-response-text reject-response)]
          (is (nil? (:isError reject-response)))
          (is (= "rejected" (:status parsed)))
          ;; Verify removed from pending
          (is (= 0 (count @diff/pending-diffs)))
          ;; Verify file was NOT changed
          (is (= "old" @file-content)))))))

(deftest test-reject-diff-not-found
  (testing "Returns error when diff_id not found"
    (let [response (diff/handle-reject-diff {:diff_id "nonexistent-id"})
          parsed (parse-response-text response)]
      (is (true? (:isError response)))
      (is (re-find #"(?i)not found" (:error parsed))))))

(deftest test-reject-diff-with-reason
  (testing "reject_diff can include rejection reason"
    (let [propose-response (diff/handle-propose-diff
                            {:file_path "/src/test.clj"
                             :old_content "old"
                             :new_content "new"
                             :description "Change"
                             :drone_id "drone-1"})
          diff-id (:id (parse-response-text propose-response))]
      (let [reject-response (diff/handle-reject-diff
                             {:diff_id diff-id
                              :reason "Change not needed"})
            parsed (parse-response-text reject-response)]
        (is (nil? (:isError reject-response)))
        (is (= "rejected" (:status parsed)))
        (is (= "Change not needed" (:reason parsed)))))))

;; =============================================================================
;; Test: Tool definitions
;; =============================================================================

(deftest test-tools-defined
  (testing "All diff tools are defined with correct structure"
    (let [tool-names (set (map :name diff/tools))]
      (is (contains? tool-names "propose_diff"))
      (is (contains? tool-names "list_proposed_diffs"))
      (is (contains? tool-names "apply_diff"))
      (is (contains? tool-names "reject_diff")))
    ;; Verify each tool has required fields
    (doseq [tool diff/tools]
      (is (string? (:name tool)))
      (is (string? (:description tool)))
      (is (map? (:inputSchema tool)))
      (is (fn? (:handler tool))))))
