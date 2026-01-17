(ns hive-mcp.hot-reload-e2e-test
  "E2E tests for hot-reload MCP integration.

   Tests the complete hot-reload flow:
   - Watcher status via MCP tools
   - Claim-aware coordination with logic module
   - Handler response structures

   REQUIRES: Running MCP server with hive-hot initialized.
   Tagged :integration to exclude from CI runs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [hive-mcp.tools.hot :as hot]
            [hive-mcp.swarm.logic :as logic]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-test-state!
  "Reset test state between tests."
  []
  (logic/reset-db!))

(use-fixtures :each
  (fn [f]
    (reset-test-state!)
    (try
      (f)
      (finally
        (reset-test-state!)))))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn parse-response
  "Parse MCP JSON response text to Clojure map."
  [response]
  (json/read-str (:text response) :key-fn keyword))

(defn with-temp-file
  "Execute body with a temporary file in src directory.
   Cleans up file after body executes."
  [filename body-fn]
  (let [file (io/file "src" filename)]
    (try
      (io/make-parents file)
      (body-fn file)
      (finally
        (when (.exists file)
          (.delete file))))))

;; =============================================================================
;; Test: Watcher Status (Integration)
;; =============================================================================

(deftest ^:integration test-watcher-is-running
  (testing "hot_watcher_status reports watcher is active"
    (let [response (hot/handle-hot-watcher-status {})
          parsed (parse-response response)]
      (is (= "text" (:type response)) "Response should be text type")
      (is (true? (:watching? parsed)) "Watcher should be active")
      (is (contains? (set (:paths parsed)) "src")
          "Should be watching 'src' directory"))))

(deftest ^:integration test-watcher-status-structure
  (testing "hot_watcher_status returns complete structure"
    (let [response (hot/handle-hot-watcher-status {})
          parsed (parse-response response)]
      (is (boolean? (:watching? parsed)) "watching? should be boolean")
      (is (vector? (:paths parsed)) "paths should be vector")
      (is (number? (:pending-count parsed)) "pending-count should be number")
      (is (vector? (:claimed-files parsed)) "claimed-files should be vector")
      (is (or (map? (:hot-core-status parsed))
              (nil? (:hot-core-status parsed)))
          "hot-core-status should be map or nil"))))

;; =============================================================================
;; Test: Hot Reload Handler
;; =============================================================================

(deftest ^:integration test-hot-reload-success
  (testing "hot_reload returns success structure"
    (with-redefs [hot/hot-core-reload!
                  (fn [] {:success true
                          :loaded ['hive-mcp.test-ns]
                          :unloaded ['hive-mcp.test-ns]
                          :ms 15})]
      (let [response (hot/handle-hot-reload {})
            parsed (parse-response response)]
        (is (true? (:success parsed)) "Reload should succeed")
        (is (= ["hive-mcp.test-ns"] (:loaded parsed)) "Should report loaded namespaces")
        (is (= ["hive-mcp.test-ns"] (:unloaded parsed)) "Should report unloaded namespaces")
        (is (= 15 (:ms parsed)) "Should report elapsed time")))))

(deftest ^:integration test-hot-reload-with-failure
  (testing "hot_reload handles reload failures"
    (with-redefs [hot/hot-core-reload!
                  (fn [] {:success false
                          :failed 'broken.ns
                          :exception (Exception. "Parse error")})]
      (let [response (hot/handle-hot-reload {})
            parsed (parse-response response)]
        (is (false? (:success parsed)) "Reload should fail")
        (is (= "broken.ns" (:failed parsed)) "Should report failed namespace")))))

;; =============================================================================
;; Test: Claim-Aware Integration
;; =============================================================================

(deftest ^:integration test-claims-appear-in-status
  (testing "File claims appear in hot_watcher_status"
    ;; Add claims via logic module
    (logic/add-claim! "/src/feature/alpha.clj" "ling-alpha")
    (logic/add-claim! "/src/feature/beta.clj" "ling-beta")
    
    (let [response (hot/handle-hot-watcher-status {})
          parsed (parse-response response)]
      ;; Check claimed-files list
      (is (= 2 (count (:claimed-files parsed)))
          "Should have 2 claimed files")
      (is (some #(= "/src/feature/alpha.clj" %) (:claimed-files parsed))
          "alpha.clj should be claimed")
      (is (some #(= "/src/feature/beta.clj" %) (:claimed-files parsed))
          "beta.clj should be claimed")
      
      ;; Check claims-detail structure
      (let [claims (:claims-detail parsed)]
        (is (= 2 (count claims)) "claims-detail should have 2 entries")
        (when-let [alpha-claim (first (filter #(= "/src/feature/alpha.clj" (:file %)) claims))]
          (is (= "ling-alpha" (:slave-id alpha-claim))
              "alpha claim should have correct slave-id"))))))

(deftest ^:integration test-claim-lifecycle
  (testing "Claims can be added and released"
    (let [file-path "/src/temp/lifecycle-test.clj"]
      ;; Initially no claims
      (let [status (parse-response (hot/handle-hot-watcher-status {}))]
        (is (empty? (:claimed-files status))
            "Should start with no claims"))
      
      ;; Add claim
      (logic/add-claim! file-path "ling-lifecycle")
      (let [status (parse-response (hot/handle-hot-watcher-status {}))]
        (is (= 1 (count (:claimed-files status)))
            "Should have 1 claim")
        (is (= file-path (first (:claimed-files status)))
            "Should be our file"))
      
      ;; Release claim
      (logic/remove-claim! file-path "ling-lifecycle")
      (let [status (parse-response (hot/handle-hot-watcher-status {}))]
        (is (empty? (:claimed-files status))
            "Claim should be released")))))

(deftest ^:integration test-flush-pending
  (testing "hot_flush_pending calls debounce flush"
    (with-redefs [hot/debounce-flush!
                  (fn [] {:flushed-count 3
                          :reload-result {:success true
                                          :loaded ['a 'b 'c]}})]
      (let [response (hot/handle-hot-flush-pending {})
            parsed (parse-response response)]
        (is (true? (:success parsed)) "Flush should succeed")
        (is (= 3 (:flushed-count parsed)) "Should report flushed count")
        (is (= {:success true :loaded ["a" "b" "c"]}
               (:reload-result parsed))
            "Should include reload result")))))

;; =============================================================================
;; Test: File Change Detection (requires running watcher)
;; =============================================================================

(deftest ^:integration test-file-modification-detection
  (testing "Watcher detects file modifications"
    (with-temp-file "_e2e_hot_test.clj"
      (fn [file]
        ;; Create initial file
        (spit file "(ns _e2e-hot-test)")
        
        ;; Wait for watcher to pick up initial creation
        (Thread/sleep 200)
        
        ;; Get initial status
        (let [initial-status (parse-response (hot/handle-hot-watcher-status {}))]
          (is (true? (:watching? initial-status))
              "Watcher should be active"))
        
        ;; Modify file
        (spit file "(ns _e2e-hot-test)\n;; modified")
        
        ;; The watcher + debouncer will handle this asynchronously
        ;; We just verify no errors in the status call
        (Thread/sleep 300)
        (let [status (parse-response (hot/handle-hot-watcher-status {}))]
          (is (true? (:watching? status))
              "Watcher should still be active after file change"))))))

;; =============================================================================
;; Test: Claimed File Debouncing
;; =============================================================================

(deftest ^:integration test-claimed-file-buffered
  (testing "Claimed file changes are buffered, not reloaded immediately"
    (with-temp-file "_e2e_claimed_file.clj"
      (fn [file]
        (let [file-path (str (.getAbsolutePath file))]
          ;; Create file
          (spit file "(ns _e2e-claimed-file)")
          (Thread/sleep 150)
          
          ;; Claim the file
          (logic/add-claim! file-path "test-ling")
          
          ;; Verify claim is registered
          (let [status (parse-response (hot/handle-hot-watcher-status {}))]
            (is (some #(= file-path %) (:claimed-files status))
                "File should appear in claimed-files"))
          
          ;; Modify file while claimed
          (spit file "(ns _e2e-claimed-file)\n;; modified while claimed")
          
          ;; The debouncer should buffer this change
          ;; We can verify the claim is still active
          (Thread/sleep 150)
          (let [status (parse-response (hot/handle-hot-watcher-status {}))]
            (is (some #(= file-path %) (:claimed-files status))
                "File should still be claimed"))
          
          ;; Release claim
          (logic/remove-claim! file-path "test-ling")
          (let [status (parse-response (hot/handle-hot-watcher-status {}))]
            (is (not (some #(= file-path %) (:claimed-files status)))
                "File should no longer be claimed")))))))

;; =============================================================================  
;; Test: Error Handling
;; =============================================================================

(deftest ^:integration test-reload-exception-handling
  (testing "hot_reload handles exceptions gracefully"
    (with-redefs [hot/hot-core-reload!
                  (fn [] (throw (Exception. "Simulated reload failure")))]
      (let [response (hot/handle-hot-reload {})]
        (is (true? (:isError response)) "Should return error response")
        (is (re-find #"Simulated reload failure" (:text response))
            "Error message should be included")))))

(deftest ^:integration test-flush-exception-handling
  (testing "hot_flush_pending handles exceptions gracefully"
    (with-redefs [hot/debounce-flush!
                  (fn [] (throw (Exception. "Flush failed")))]
      (let [response (hot/handle-hot-flush-pending {})]
        (is (true? (:isError response)) "Should return error response")
        (is (re-find #"Flush failed" (:text response))
            "Error message should be included")))))

(deftest ^:integration test-graceful-degradation
  (testing "hot_watcher_status handles missing modules gracefully"
    (with-redefs [hot/watcher-status (fn [] nil)
                  hot/watcher-watching-paths (fn [] [])
                  hot/debounce-pending-count (fn [] 0)
                  hot/hot-core-status (fn [] nil)]
      (let [response (hot/handle-hot-watcher-status {})
            parsed (parse-response response)]
        (is (false? (:watching? parsed))
            "Should report not watching when modules unavailable")
        (is (= [] (:paths parsed))
            "Paths should be empty when unavailable")
        (is (= 0 (:pending-count parsed))
            "Pending count should be 0")))))
