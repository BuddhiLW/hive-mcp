(ns hive-mcp.tools.memory-pinning-test
  "Pinning tests for MCP memory tool handlers.
   
   These tests verify the handler contract:
   - Success: {:type \"text\" :text \"...\"}
   - Error: {:type \"text\" :text \"...\" :isError true}
   
   Uses with-redefs to mock Chroma and elisp calls for isolated unit testing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.memory :as memory]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.emacsclient :as ec]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn gen-test-id
  "Generate unique test ID."
  []
  (str "test-" (java.util.UUID/randomUUID)))

(defn make-test-entry
  "Create a test memory entry with defaults."
  [& {:keys [id type content tags duration project-id created]
      :or {id (gen-test-id)
           type "note"
           content "Test memory content"
           tags ["scope:project:test-project"]
           duration "long"
           project-id "test-project"
           created "2024-01-01T00:00:00Z"}}]
  {:id id
   :type type
   :content content
   :tags tags
   :duration duration
   :project-id project-id
   :created created})

(defn parse-response-text
  "Parse the JSON text from a handler response."
  [response]
  (json/read-str (:text response) :key-fn keyword))

;; =============================================================================
;; Test: Response Format Contract
;; =============================================================================

(deftest test-response-format-success
  (testing "Success response has correct structure"
    (let [test-entry (make-test-entry :id "test-123")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/content-hash (constantly "hash123")
                    chroma/find-duplicate (constantly nil)
                    chroma/index-memory-entry! (constantly "test-123")
                    chroma/get-entry-by-id (constantly test-entry)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-add
                        {:type "note" :content "Test content"})]
          (is (= "text" (:type response)))
          (is (string? (:text response)))
          (is (nil? (:isError response))))))))

(deftest test-response-format-error
  (testing "Error response has correct structure"
    (with-redefs [chroma/embedding-configured? (constantly false)]
      (let [response (memory/handle-mcp-memory-add
                      {:type "note" :content "Test content"})]
        (is (= "text" (:type response)))
        (is (string? (:text response)))
        (is (true? (:isError response)))))))

;; =============================================================================
;; Test: handle-mcp-memory-add
;; =============================================================================

(deftest test-handle-mcp-memory-add-success
  (testing "Successfully adds a memory entry"
    (let [test-id "mem-add-123"
          test-entry (make-test-entry :id test-id
                                      :type "note"
                                      :content "Important note")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/content-hash (constantly "hash-abc")
                    chroma/find-duplicate (constantly nil)
                    chroma/index-memory-entry! (constantly test-id)
                    chroma/get-entry-by-id (constantly test-entry)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-add
                        {:type "note"
                         :content "Important note"
                         :tags ["work"]
                         :duration "long"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (= test-id (:id parsed)))
          (is (= "note" (:type parsed))))))))

(deftest test-handle-mcp-memory-add-duplicate-merge
  (testing "Merges tags when duplicate found"
    (let [existing-entry (make-test-entry :id "existing-123"
                                          :tags ["scope:project:test" "old-tag"])]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/content-hash (constantly "hash-dup")
                    chroma/find-duplicate (constantly existing-entry)
                    chroma/update-entry! (constantly (assoc existing-entry
                                                            :tags ["scope:project:test" "old-tag" "new-tag"]))
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-add
                        {:type "note"
                         :content "Duplicate content"
                         :tags ["new-tag"]})]
          (is (= "text" (:type response)))
          (is (nil? (:isError response))))))))

(deftest test-handle-mcp-memory-add-chroma-not-configured
  (testing "Returns error when Chroma not configured"
    (with-redefs [chroma/embedding-configured? (constantly false)]
      (let [response (memory/handle-mcp-memory-add
                      {:type "note" :content "Test"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (true? (:isError response)))
        (is (= "Chroma not configured" (:error parsed)))))))

(deftest test-handle-mcp-memory-add-exception-handling
  (testing "Handles exceptions gracefully"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/content-hash (fn [_] (throw (ex-info "Hash failed" {})))
                  ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
      (let [response (memory/handle-mcp-memory-add
                      {:type "note" :content "Test"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (true? (:isError response)))
        (is (contains? parsed :error))))))

;; =============================================================================
;; Test: handle-mcp-memory-query
;; =============================================================================

(deftest test-handle-mcp-memory-query-success
  (testing "Successfully queries memory entries"
    (let [entries [(make-test-entry :id "q1" :type "note" :content "First note")
                   (make-test-entry :id "q2" :type "note" :content "Second note")]]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/query-entries (constantly entries)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-query
                        {:type "note" :limit 10})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (vector? parsed))
          (is (= 2 (count parsed))))))))

(deftest test-handle-mcp-memory-query-with-scope-filter
  (testing "Filters by scope when specified"
    (let [global-entry (make-test-entry :id "g1" :tags ["scope:global"])
          project-entry (make-test-entry :id "p1" :tags ["scope:project:test"])]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/query-entries (constantly [global-entry project-entry])
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-query
                        {:type "note" :scope "global"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          ;; Only global entry should match
          (is (= 1 (count parsed)))
          (is (= "g1" (:id (first parsed)))))))))

(deftest test-handle-mcp-memory-query-with-tag-filter
  (testing "Filters by tags when specified"
    (let [tagged (make-test-entry :id "t1" :tags ["scope:global" "important"])
          untagged (make-test-entry :id "t2" :tags ["scope:global"])]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/query-entries (constantly [tagged untagged])
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-query
                        {:type "note" :scope "all" :tags ["important"]})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (= 1 (count parsed)))
          (is (= "t1" (:id (first parsed)))))))))

(deftest test-handle-mcp-memory-query-empty-results
  (testing "Returns empty array when no matches"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/query-entries (constantly [])
                  ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
      (let [response (memory/handle-mcp-memory-query {:type "note"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (nil? (:isError response)))
        (is (= [] parsed))))))

(deftest test-handle-mcp-memory-query-chroma-not-configured
  (testing "Returns error when Chroma not configured"
    (with-redefs [chroma/embedding-configured? (constantly false)]
      (let [response (memory/handle-mcp-memory-query {:type "note"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (true? (:isError response)))
        (is (contains? parsed :error))))))

;; =============================================================================
;; Test: handle-mcp-memory-get-full
;; =============================================================================

(deftest test-handle-mcp-memory-get-full-success
  (testing "Successfully retrieves full entry by ID"
    (let [test-entry (make-test-entry :id "full-123"
                                      :content "Full content here"
                                      :tags ["tag1" "tag2"])]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/get-entry-by-id (constantly test-entry)]
        (let [response (memory/handle-mcp-memory-get-full {:id "full-123"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (= "full-123" (:id parsed)))
          (is (= "Full content here" (:content parsed)))
          (is (= ["tag1" "tag2"] (:tags parsed))))))))

(deftest test-handle-mcp-memory-get-full-not-found
  (testing "Returns error when entry not found"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/get-entry-by-id (constantly nil)]
      (let [response (memory/handle-mcp-memory-get-full {:id "nonexistent"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (true? (:isError response)))
        (is (= "Entry not found" (:error parsed)))
        (is (= "nonexistent" (:id parsed)))))))

(deftest test-handle-mcp-memory-get-full-chroma-not-configured
  (testing "Returns error when Chroma not configured"
    (with-redefs [chroma/embedding-configured? (constantly false)]
      (let [response (memory/handle-mcp-memory-get-full {:id "test-id"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (true? (:isError response)))
        (is (contains? parsed :error))))))

(deftest test-handle-mcp-memory-get-full-exception-handling
  (testing "Handles exceptions gracefully"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/get-entry-by-id (fn [_] (throw (ex-info "DB error" {})))]
      (let [response (memory/handle-mcp-memory-get-full {:id "test-id"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (true? (:isError response)))
        (is (contains? parsed :error))))))

;; =============================================================================
;; Test: handle-mcp-memory-query-metadata
;; =============================================================================

(deftest test-handle-mcp-memory-query-metadata-success
  (testing "Returns metadata-only format with preview"
    (let [entries [(make-test-entry :id "meta-1"
                                    :content "This is a longer content that should be truncated in preview"
                                    :tags ["scope:global" "work"])]]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/query-entries (constantly entries)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-query-metadata
                        {:type "note" :scope "all"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (vector? parsed))
          (is (= 1 (count parsed)))
          ;; Metadata format should have these fields
          (let [meta-entry (first parsed)]
            (is (contains? meta-entry :id))
            (is (contains? meta-entry :type))
            (is (contains? meta-entry :preview))
            (is (contains? meta-entry :tags))
            (is (contains? meta-entry :created))
            ;; Should NOT have full content
            (is (not (contains? meta-entry :content)))))))))

(deftest test-handle-mcp-memory-query-metadata-preview-truncation
  (testing "Truncates long content in preview"
    (let [long-content (apply str (repeat 200 "x"))
          entries [(make-test-entry :id "long-1" :content long-content)]]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/query-entries (constantly entries)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-query-metadata
                        {:type "note" :scope "all"})
              parsed (parse-response-text response)
              preview (:preview (first parsed))]
          (is (<= (count preview) 103))))))) ; 100 chars + "..."

;; =============================================================================
;; Test: Duration and Expiration Handlers
;; =============================================================================

(deftest test-handle-mcp-memory-set-duration
  (testing "Sets duration on existing entry"
    (let [updated-entry (make-test-entry :id "dur-1" :duration "permanent")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/update-entry! (constantly updated-entry)]
        (let [response (memory/handle-mcp-memory-set-duration
                        {:id "dur-1" :duration "permanent"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (= "permanent" (:duration parsed))))))))

(deftest test-handle-mcp-memory-promote
  (testing "Promotes entry to longer duration"
    (let [original (make-test-entry :id "promo-1" :duration "medium")
          promoted (make-test-entry :id "promo-1" :duration "long")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/get-entry-by-id (constantly original)
                    chroma/update-entry! (constantly promoted)]
        (let [response (memory/handle-mcp-memory-promote {:id "promo-1"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response))))))))

(deftest test-handle-mcp-memory-demote
  (testing "Demotes entry to shorter duration"
    (let [original (make-test-entry :id "demo-1" :duration "long")
          demoted (make-test-entry :id "demo-1" :duration "medium")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/get-entry-by-id (constantly original)
                    chroma/update-entry! (constantly demoted)]
        (let [response (memory/handle-mcp-memory-demote {:id "demo-1"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response))))))))

;; =============================================================================
;; Test: Feedback and Access Tracking
;; =============================================================================

(deftest test-handle-mcp-memory-feedback-helpful
  (testing "Records helpful feedback"
    (let [entry (make-test-entry :id "fb-1")
          updated (assoc entry :helpful-count 1)]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/get-entry-by-id (constantly entry)
                    chroma/update-entry! (constantly updated)]
        (let [response (memory/handle-mcp-memory-feedback
                        {:id "fb-1" :feedback "helpful"})]
          (is (= "text" (:type response)))
          (is (nil? (:isError response))))))))

(deftest test-handle-mcp-memory-feedback-unhelpful
  (testing "Records unhelpful feedback"
    (let [entry (make-test-entry :id "fb-2")
          updated (assoc entry :unhelpful-count 1)]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/get-entry-by-id (constantly entry)
                    chroma/update-entry! (constantly updated)]
        (let [response (memory/handle-mcp-memory-feedback
                        {:id "fb-2" :feedback "unhelpful"})]
          (is (= "text" (:type response)))
          (is (nil? (:isError response))))))))

(deftest test-handle-mcp-memory-helpfulness-ratio
  (testing "Calculates helpfulness ratio"
    (let [entry (assoc (make-test-entry :id "ratio-1")
                       :helpful-count 3
                       :unhelpful-count 1)]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/get-entry-by-id (constantly entry)]
        (let [response (memory/handle-mcp-memory-helpfulness-ratio {:id "ratio-1"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (= 0.75 (:ratio parsed)))
          (is (= 3 (:helpful parsed)))
          (is (= 1 (:unhelpful parsed))))))))

(deftest test-handle-mcp-memory-log-access
  (testing "Increments access count"
    (let [entry (assoc (make-test-entry :id "access-1") :access-count 5)
          updated (assoc entry :access-count 6)]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/get-entry-by-id (constantly entry)
                    chroma/update-entry! (constantly updated)]
        (let [response (memory/handle-mcp-memory-log-access {:id "access-1"})]
          (is (= "text" (:type response)))
          (is (nil? (:isError response))))))))

;; =============================================================================
;; Test: Cleanup and Expiration
;; =============================================================================

(deftest test-handle-mcp-memory-cleanup-expired
  (testing "Cleans up expired entries"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/cleanup-expired! (constantly 5)]
      (let [response (memory/handle-mcp-memory-cleanup-expired {})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (nil? (:isError response)))
        (is (= 5 (:deleted parsed)))))))

(deftest test-handle-mcp-memory-expiring-soon
  (testing "Lists entries expiring soon"
    (let [expiring [(make-test-entry :id "exp-1")
                    (make-test-entry :id "exp-2")]]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/entries-expiring-soon (constantly expiring)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-expiring-soon {:days 7})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (= 2 (count parsed))))))))

;; =============================================================================
;; Test: Duplicate Detection
;; =============================================================================

(deftest test-handle-mcp-memory-check-duplicate-found
  (testing "Detects existing duplicate"
    (let [existing (make-test-entry :id "dup-1")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/content-hash (constantly "hash-xyz")
                    chroma/find-duplicate (constantly existing)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [response (memory/handle-mcp-memory-check-duplicate
                        {:type "note" :content "Some content"})
              parsed (parse-response-text response)]
          (is (= "text" (:type response)))
          (is (nil? (:isError response)))
          (is (true? (:exists parsed)))
          (is (some? (:entry parsed))))))))

(deftest test-handle-mcp-memory-check-duplicate-not-found
  (testing "Reports no duplicate when content is new"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/content-hash (constantly "hash-new")
                  chroma/find-duplicate (constantly nil)
                  ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
      (let [response (memory/handle-mcp-memory-check-duplicate
                      {:type "note" :content "New unique content"})
            parsed (parse-response-text response)]
        (is (= "text" (:type response)))
        (is (nil? (:isError response)))
        (is (false? (:exists parsed)))
        (is (nil? (:entry parsed)))
        (is (= "hash-new" (:content_hash parsed)))))))
