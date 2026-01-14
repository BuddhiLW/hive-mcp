(ns hive-mcp.tools-duration-test
  "Unit tests for MCP duration management tool handlers.

   Tests cover the following handlers:
   - handle-mcp-memory-set-duration: Set duration category for entry
   - handle-mcp-memory-promote: Promote entry to longer duration
   - handle-mcp-memory-demote: Demote entry to shorter duration
   - handle-mcp-memory-cleanup-expired: Remove expired entries
   - handle-mcp-memory-expiring-soon: List entries expiring within N days
   - handle-mcp-memory-add: Updated with duration param
   - handle-mcp-memory-query: Updated with duration filter

   All tests verify proper MCP response format {:type \"text\" :text ...}
   and Chroma-based error handling.
   
   UPDATED: Tests now mock Chroma functions instead of emacsclient,
   reflecting the Chroma-only storage architecture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.memory :as memory]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.elisp :as el]))

;; =============================================================================
;; Test Fixtures and Helpers
;; =============================================================================

(defn mock-emacsclient-success
  "Creates a mock eval-elisp that returns success with given result."
  [result]
  (fn [_elisp]
    {:success true :result result :duration-ms 10}))

(defn mock-emacsclient-failure
  "Creates a mock eval-elisp that returns failure with given error."
  [error]
  (fn [_elisp]
    {:success false :error error :duration-ms 10}))

(defn mock-emacsclient-not-loaded
  "Mock for when hive-mcp.el is not loaded (featurep returns nil)."
  []
  (fn [elisp]
    (if (str/includes? elisp "featurep")
      {:success true :result "nil" :duration-ms 5}
      {:success false :error "Function not available" :duration-ms 10})))

(defn mock-emacsclient-loaded
  "Mock for when hive-mcp.el is loaded."
  [api-result]
  (fn [elisp]
    (if (str/includes? elisp "featurep")
      {:success true :result "t" :duration-ms 5}
      {:success true :result api-result :duration-ms 10})))

(defmacro with-mock-emacsclient
  "Execute body with mocked emacsclient/eval-elisp."
  [mock-fn & body]
  `(with-redefs [ec/eval-elisp ~mock-fn]
     ~@body))

;; =============================================================================
;; Chroma Mock Helpers (for Chroma-only architecture)
;; =============================================================================

(defn make-test-entry
  "Create a test memory entry with given overrides."
  [& {:keys [id type content duration tags expires]
      :or {id "test-id"
           type "note"
           content "Test content"
           duration "long"
           tags []
           expires nil}}]
  {:id id
   :type type
   :content content
   :duration duration
   :tags tags
   :expires expires
   :created (str (java.time.ZonedDateTime/now))})

(defmacro with-mock-chroma
  "Execute body with mocked Chroma functions.
   
   Options:
   - :configured? - whether Chroma is configured (default: true)
   - :entry - entry to return from get-entry-by-id (default: make-test-entry)
   - :entries - list of entries for query functions"
  [{:keys [configured? entry entries]
    :or {configured? true}} & body]
  `(with-redefs [chroma/embedding-configured? (constantly ~configured?)
                 chroma/get-entry-by-id (fn [id#]
                                          (if ~entry
                                            (assoc ~entry :id id#)
                                            (make-test-entry :id id#)))
                 chroma/update-entry! (fn [id# updates#]
                                        (merge (or ~entry (make-test-entry :id id#)) updates#))
                 chroma/query-entries (fn [& _#] (or ~entries []))
                 chroma/delete-entry! (fn [_#] true)
                 chroma/cleanup-expired! (fn [] {:deleted 0})
                 chroma/entries-expiring-soon (fn [_# & _opts#] (or ~entries []))]
     ~@body))

(defmacro with-chroma-not-configured
  "Execute body with Chroma not configured."
  [& body]
  `(with-mock-chroma {:configured? false}
     ~@body))

;; =============================================================================
;; MCP Response Format Tests
;; =============================================================================

(deftest mcp-response-format-test
  (testing "mcp-success creates proper response format"
    (let [result (core/mcp-success "test result")]
      (is (= "text" (:type result)))
      (is (= "test result" (:text result)))
      (is (nil? (:isError result)))))

  (testing "mcp-error creates proper error response format"
    (let [result (core/mcp-error "test error")]
      (is (= "text" (:type result)))
      (is (= "test error" (:text result)))
      (is (true? (:isError result)))))

  (testing "mcp-json creates proper JSON response format"
    (let [result (core/mcp-json {:id "123" :status "ok"})]
      (is (= "text" (:type result)))
      (is (string? (:text result)))
      (let [parsed (json/read-str (:text result) :key-fn keyword)]
        (is (= "123" (:id parsed)))
        (is (= "ok" (:status parsed)))))))

;; =============================================================================
;; handle-mcp-memory-set-duration Tests
;; =============================================================================

(deftest handle-mcp-memory-set-duration-test
  (testing "Returns proper MCP response on success"
    (with-mock-chroma {:entry (make-test-entry :id "abc123" :duration "long")}
      (let [result (memory/handle-mcp-memory-set-duration {:id "abc123" :duration "permanent"})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "abc123" (:id parsed)))
          (is (= "permanent" (:duration parsed)))))))

  (testing "Returns error when Chroma not configured"
    (with-chroma-not-configured
      (let [result (memory/handle-mcp-memory-set-duration {:id "abc123" :duration "session"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Chroma not configured")))))

  (testing "Returns error when entry not found"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/update-entry! (fn [_ _] nil)]
      (let [result (memory/handle-mcp-memory-set-duration {:id "nonexistent" :duration "permanent"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "not found"))))))

;; =============================================================================
;; handle-mcp-memory-promote Tests
;; =============================================================================

(deftest handle-mcp-memory-promote-test
  (testing "Returns proper MCP response on successful promotion"
    (with-mock-chroma {:entry (make-test-entry :id "abc123" :duration "short")}
      (let [result (memory/handle-mcp-memory-promote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "abc123" (:id parsed)))
          ;; short -> medium
          (is (= "medium" (:duration parsed)))))))

  (testing "Returns error when Chroma not configured"
    (with-chroma-not-configured
      (let [result (memory/handle-mcp-memory-promote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Chroma not configured")))))

  (testing "Returns message when already at maximum duration"
    (with-mock-chroma {:entry (make-test-entry :id "abc123" :duration "permanent")}
      (let [result (memory/handle-mcp-memory-promote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (str/includes? (:message parsed) "maximum"))
          (is (= "permanent" (:duration parsed))))))))

;; =============================================================================
;; handle-mcp-memory-demote Tests
;; =============================================================================

(deftest handle-mcp-memory-demote-test
  (testing "Returns proper MCP response on successful demotion"
    (with-mock-chroma {:entry (make-test-entry :id "abc123" :duration "medium")}
      (let [result (memory/handle-mcp-memory-demote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= "abc123" (:id parsed)))
          ;; medium -> short
          (is (= "short" (:duration parsed)))))))

  (testing "Returns error when Chroma not configured"
    (with-chroma-not-configured
      (let [result (memory/handle-mcp-memory-demote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Chroma not configured")))))

  (testing "Returns message when already at minimum duration"
    (with-mock-chroma {:entry (make-test-entry :id "abc123" :duration "ephemeral")}
      (let [result (memory/handle-mcp-memory-demote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (str/includes? (:message parsed) "minimum"))
          (is (= "ephemeral" (:duration parsed))))))))

;; =============================================================================
;; handle-mcp-memory-cleanup-expired Tests
;; =============================================================================

(deftest handle-mcp-memory-cleanup-expired-test
  (testing "Returns proper MCP response on successful cleanup"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/cleanup-expired! (fn [] 5)] ; Returns count, not map
      (let [result (memory/handle-mcp-memory-cleanup-expired {})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= 5 (:deleted parsed)))))))

  (testing "Returns error when Chroma not configured"
    (with-chroma-not-configured
      (let [result (memory/handle-mcp-memory-cleanup-expired {})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Chroma not configured")))))

  (testing "Returns zero deleted when nothing expired"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/cleanup-expired! (fn [] 0)] ; Returns count, not map
      (let [result (memory/handle-mcp-memory-cleanup-expired {})]
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= 0 (:deleted parsed))))))))

;; =============================================================================
;; handle-mcp-memory-expiring-soon Tests
;; =============================================================================

(deftest handle-mcp-memory-expiring-soon-test
  (testing "Returns proper MCP response with expiring entries"
    (let [test-entries [(make-test-entry :id "exp1" :duration "short")
                        (make-test-entry :id "exp2" :duration "ephemeral")]]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/entries-expiring-soon (fn [_ & _] test-entries)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [result (memory/handle-mcp-memory-expiring-soon {:days 7})]
          (is (= "text" (:type result)))
          (is (string? (:text result)))
          (is (nil? (:isError result)))
          ;; Handler returns array directly, not wrapped in {:entries ...}
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= 2 (count parsed))))))))

  (testing "Returns error when Chroma not configured"
    (with-chroma-not-configured
      (let [result (memory/handle-mcp-memory-expiring-soon {:days 7})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Chroma not configured")))))

  (testing "Defaults to 7 days when days not specified"
    (let [captured-days (atom nil)]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/entries-expiring-soon (fn [days & _]
                                                   (reset! captured-days days)
                                                   [])
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (memory/handle-mcp-memory-expiring-soon {})
        (is (= 7 @captured-days))))))

;; =============================================================================
;; handle-mcp-memory-add with duration Tests
;; =============================================================================

(deftest handle-mcp-memory-add-with-duration-test
  (testing "Adds memory entry with duration parameter"
    (let [captured-entry (atom nil)
          created-entry (make-test-entry :id "new-id" :duration "short")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/content-hash (fn [_] "hash123")
                    chroma/find-duplicate (fn [& _] nil) ; No duplicate
                    chroma/index-memory-entry! (fn [entry]
                                                 (reset! captured-entry entry)
                                                 "new-id") ; Returns just ID
                    chroma/get-entry-by-id (fn [_] created-entry)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [result (memory/handle-mcp-memory-add {:type "note"
                                                    :content "Test note"
                                                    :tags ["test"]
                                                    :duration "short"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          ;; Verify duration was passed to Chroma
          (is (= "short" (:duration @captured-entry)))))))

  (testing "Adds memory entry with default duration"
    (let [captured-entry (atom nil)
          created-entry (make-test-entry :id "new-id" :duration "long")]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/content-hash (fn [_] "hash456")
                    chroma/find-duplicate (fn [& _] nil)
                    chroma/index-memory-entry! (fn [entry]
                                                 (reset! captured-entry entry)
                                                 "new-id")
                    chroma/get-entry-by-id (fn [_] created-entry)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [result (memory/handle-mcp-memory-add {:type "note"
                                                    :content "Test note"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          ;; Verify default duration is "long"
          (is (= "long" (:duration @captured-entry)))))))

  (testing "Returns error when Chroma not configured"
    (with-chroma-not-configured
      (let [result (memory/handle-mcp-memory-add {:type "note"
                                                  :content "Test"
                                                  :duration "short"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Chroma not configured"))))))

;; =============================================================================
;; handle-mcp-memory-query with duration filter Tests
;; =============================================================================

(deftest handle-mcp-memory-query-with-duration-test
  (testing "Queries memory entries filtered by duration"
    (let [test-entries [(make-test-entry :id "id1" :duration "permanent" :tags ["scope:project:test-project"])
                        (make-test-entry :id "id2" :duration "permanent" :tags ["scope:project:test-project"])]]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/query-entries (fn [& _] test-entries)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [result (memory/handle-mcp-memory-query {:type "note"
                                                      :duration "permanent"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          ;; Handler returns array directly
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (= 2 (count parsed))))))))

  (testing "Queries without duration filter returns all"
    (let [test-entries [(make-test-entry :id "id1" :duration "short" :tags ["scope:project:test-project"])
                        (make-test-entry :id "id2" :duration "long" :tags ["scope:project:test-project"])]]
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/query-entries (fn [& _] test-entries)
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]
        (let [result (memory/handle-mcp-memory-query {:type "note"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))))))

  (testing "Returns error when Chroma not configured"
    (with-chroma-not-configured
      (let [result (memory/handle-mcp-memory-query {:type "note" :duration "short"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Chroma not configured"))))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest duration-lifecycle-integration-test
  (testing "Full lifecycle: create -> promote -> verify -> demote -> verify"
    (let [entry-id (atom "test-entry-123")
          memory-store (atom {})]
      ;; Mock Chroma to use our in-memory store
      (with-redefs [chroma/embedding-configured? (constantly true)
                    chroma/content-hash (fn [_] "hash789")
                    chroma/find-duplicate (fn [& _] nil)
                    chroma/index-memory-entry! (fn [entry]
                                                 (let [id (str (random-uuid))]
                                                   (reset! entry-id id)
                                                   (swap! memory-store assoc id (assoc entry :id id))
                                                   id))
                    chroma/get-entry-by-id (fn [id] (get @memory-store id))
                    chroma/update-entry! (fn [id updates]
                                           (when (get @memory-store id)
                                             (swap! memory-store update id merge updates)
                                             (get @memory-store id)))
                    ec/eval-elisp (constantly {:success true :result "\"test-project\""})]

        ;; Step 1: Create entry with ephemeral duration
        (let [create-result (memory/handle-mcp-memory-add {:type "note"
                                                           :content "Integration test"
                                                           :duration "ephemeral"})]
          (is (nil? (:isError create-result)) (str "Create failed: " (:text create-result)))
          (let [parsed (json/read-str (:text create-result) :key-fn keyword)]
            (is (= "ephemeral" (:duration parsed)))))

        ;; Step 2: Promote to short
        (let [promote-result (memory/handle-mcp-memory-promote {:id @entry-id})]
          (is (nil? (:isError promote-result)))
          (let [parsed (json/read-str (:text promote-result) :key-fn keyword)]
            (is (= "short" (:duration parsed)))))

        ;; Step 3: Verify state in memory store
        (is (= "short" (:duration (get @memory-store @entry-id))))

        ;; Step 4: Promote again to medium
        (let [promote-result (memory/handle-mcp-memory-promote {:id @entry-id})]
          (is (nil? (:isError promote-result)))
          (let [parsed (json/read-str (:text promote-result) :key-fn keyword)]
            (is (= "medium" (:duration parsed)))))

        ;; Step 5: Demote back to short
        (let [demote-result (memory/handle-mcp-memory-demote {:id @entry-id})]
          (is (nil? (:isError demote-result)))
          (let [parsed (json/read-str (:text demote-result) :key-fn keyword)]
            (is (= "short" (:duration parsed)))))

        ;; Step 6: Verify final state
        (is (= "short" (:duration (get @memory-store @entry-id))))))))

;; =============================================================================
;; Error Handling Edge Cases
;; =============================================================================

(deftest error-handling-edge-cases-test
  (testing "Handles entry not found error"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/get-entry-by-id (constantly nil)]
      (let [result (memory/handle-mcp-memory-promote {:id "non-existent-id"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "not found")))))

  (testing "Handles Chroma exception"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/get-entry-by-id (fn [_] (throw (Exception. "Connection failed")))]
      (let [result (memory/handle-mcp-memory-promote {:id "test-id"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Connection failed")))))

  (testing "Handles cleanup with Chroma exception"
    (with-redefs [chroma/embedding-configured? (constantly true)
                  chroma/cleanup-expired! (fn [] (throw (Exception. "Cleanup failed")))]
      (let [result (memory/handle-mcp-memory-cleanup-expired {})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Cleanup failed"))))))

;; =============================================================================
;; Elisp Generation Verification Tests
;; =============================================================================

(deftest elisp-generation-test
  (testing "set-duration uses require-and-call-json pattern"
    (let [elisp (el/require-and-call-json "hive-mcp-api"
                                          "hive-mcp-api-memory-set-duration"
                                          "test-id" "permanent")]
      (is (str/includes? elisp "progn"))
      (is (str/includes? elisp "require"))
      (is (str/includes? elisp "fboundp"))
      (is (str/includes? elisp "json-encode"))
      (is (str/includes? elisp "hive-mcp-api-memory-set-duration"))
      (is (str/includes? elisp "test-id"))
      (is (str/includes? elisp "permanent"))))

  (testing "promote uses require-and-call-json pattern"
    (let [elisp (el/require-and-call-json "hive-mcp-api"
                                          "hive-mcp-api-memory-promote"
                                          "test-id")]
      (is (str/includes? elisp "hive-mcp-api-memory-promote"))
      (is (str/includes? elisp "test-id"))))

  (testing "demote uses require-and-call-json pattern"
    (let [elisp (el/require-and-call-json "hive-mcp-api"
                                          "hive-mcp-api-memory-demote"
                                          "test-id")]
      (is (str/includes? elisp "hive-mcp-api-memory-demote"))
      (is (str/includes? elisp "test-id"))))

  (testing "cleanup-expired uses require-and-call-json pattern"
    (let [elisp (el/require-and-call-json "hive-mcp-api"
                                          "hive-mcp-api-memory-cleanup-expired")]
      (is (str/includes? elisp "hive-mcp-api-memory-cleanup-expired"))))

  (testing "expiring-soon uses require-and-call-json pattern with days param"
    (let [elisp (el/require-and-call-json "hive-mcp-api"
                                          "hive-mcp-api-memory-expiring-soon"
                                          14)]
      (is (str/includes? elisp "hive-mcp-api-memory-expiring-soon"))
      (is (str/includes? elisp "14")))))
