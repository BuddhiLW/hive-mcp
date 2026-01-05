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
   and error handling when hive-mcp.el is not available."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [hive-mcp.tools :as tools]
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
;; MCP Response Format Tests
;; =============================================================================

(deftest mcp-response-format-test
  (testing "mcp-success creates proper response format"
    (let [result (tools/mcp-success "test result")]
      (is (= "text" (:type result)))
      (is (= "test result" (:text result)))
      (is (nil? (:isError result)))))

  (testing "mcp-error creates proper error response format"
    (let [result (tools/mcp-error "test error")]
      (is (= "text" (:type result)))
      (is (= "test error" (:text result)))
      (is (true? (:isError result)))))

  (testing "mcp-json creates proper JSON response format"
    (let [result (tools/mcp-json {:id "123" :status "ok"})]
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
    (with-mock-emacsclient
      (mock-emacsclient-loaded "{\"success\":true,\"id\":\"abc123\",\"duration\":\"permanent\"}")
      (let [result (tools/handle-mcp-memory-set-duration {:id "abc123" :duration "permanent"})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (true? (:success parsed)))
          (is (= "abc123" (:id parsed)))
          (is (= "permanent" (:duration parsed)))))))

  (testing "Returns error when hive-mcp.el not loaded"
    (with-mock-emacsclient
      (mock-emacsclient-not-loaded)
      (let [result (tools/handle-mcp-memory-set-duration {:id "abc123" :duration "session"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "hive-mcp.el is not loaded")))))

  (testing "Returns error on elisp evaluation failure"
    (with-mock-emacsclient
      (fn [elisp]
        (if (str/includes? elisp "featurep")
          {:success true :result "t" :duration-ms 5}
          {:success false :error "Invalid duration value" :duration-ms 10}))
      (let [result (tools/handle-mcp-memory-set-duration {:id "abc123" :duration "invalid"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Error:")))))

  (testing "Generates correct elisp with require-and-call-json"
    (let [captured-elisp (atom nil)]
      (with-mock-emacsclient
        (fn [elisp]
          (reset! captured-elisp elisp)
          (if (str/includes? elisp "featurep")
            {:success true :result "t" :duration-ms 5}
            {:success true :result "{}" :duration-ms 10}))
        (tools/handle-mcp-memory-set-duration {:id "test-id" :duration "project"})
        ;; Verify the elisp contains expected components
        (is (str/includes? @captured-elisp "hive-mcp-api"))
        (is (str/includes? @captured-elisp "hive-mcp-api-memory-set-duration"))))))

;; =============================================================================
;; handle-mcp-memory-promote Tests
;; =============================================================================

(deftest handle-mcp-memory-promote-test
  (testing "Returns proper MCP response on successful promotion"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "{\"success\":true,\"id\":\"abc123\",\"old_duration\":\"session\",\"new_duration\":\"project\"}")
      (let [result (tools/handle-mcp-memory-promote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (true? (:success parsed)))
          (is (= "session" (:old_duration parsed)))
          (is (= "project" (:new_duration parsed)))))))

  (testing "Returns error when hive-mcp.el not loaded"
    (with-mock-emacsclient
      (mock-emacsclient-not-loaded)
      (let [result (tools/handle-mcp-memory-promote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "hive-mcp.el is not loaded")))))

  (testing "Returns error when already at maximum duration"
    (with-mock-emacsclient
      (fn [elisp]
        (if (str/includes? elisp "featurep")
          {:success true :result "t" :duration-ms 5}
          {:success false :error "Already at maximum duration (permanent)" :duration-ms 10}))
      (let [result (tools/handle-mcp-memory-promote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Error:"))))))

;; =============================================================================
;; handle-mcp-memory-demote Tests
;; =============================================================================

(deftest handle-mcp-memory-demote-test
  (testing "Returns proper MCP response on successful demotion"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "{\"success\":true,\"id\":\"abc123\",\"old_duration\":\"permanent\",\"new_duration\":\"project\"}")
      (let [result (tools/handle-mcp-memory-demote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (true? (:success parsed)))
          (is (= "permanent" (:old_duration parsed)))
          (is (= "project" (:new_duration parsed)))))))

  (testing "Returns error when hive-mcp.el not loaded"
    (with-mock-emacsclient
      (mock-emacsclient-not-loaded)
      (let [result (tools/handle-mcp-memory-demote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "hive-mcp.el is not loaded")))))

  (testing "Returns error when already at minimum duration"
    (with-mock-emacsclient
      (fn [elisp]
        (if (str/includes? elisp "featurep")
          {:success true :result "t" :duration-ms 5}
          {:success false :error "Already at minimum duration (session)" :duration-ms 10}))
      (let [result (tools/handle-mcp-memory-demote {:id "abc123"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Error:"))))))

;; =============================================================================
;; handle-mcp-memory-cleanup-expired Tests
;; =============================================================================

(deftest handle-mcp-memory-cleanup-expired-test
  (testing "Returns proper MCP response with cleanup count"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "{\"success\":true,\"removed_count\":5,\"removed_ids\":[\"id1\",\"id2\",\"id3\",\"id4\",\"id5\"]}")
      (let [result (tools/handle-mcp-memory-cleanup-expired {})]
        (is (= "text" (:type result)))
        (is (string? (:text result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (true? (:success parsed)))
          (is (= 5 (:removed_count parsed)))
          (is (= 5 (count (:removed_ids parsed))))))))

  (testing "Returns zero count when no expired entries"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "{\"success\":true,\"removed_count\":0,\"removed_ids\":[]}")
      (let [result (tools/handle-mcp-memory-cleanup-expired {})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= 0 (:removed_count parsed)))))))

  (testing "Returns error when hive-mcp.el not loaded"
    (with-mock-emacsclient
      (mock-emacsclient-not-loaded)
      (let [result (tools/handle-mcp-memory-cleanup-expired {})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "hive-mcp.el is not loaded")))))

  (testing "Accepts nil params (no params needed)"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "{\"success\":true,\"removed_count\":0,\"removed_ids\":[]}")
      (let [result (tools/handle-mcp-memory-cleanup-expired nil)]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))))))

;; =============================================================================
;; handle-mcp-memory-expiring-soon Tests
;; =============================================================================

(deftest handle-mcp-memory-expiring-soon-test
  (testing "Returns entries expiring within default 7 days"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "[{\"id\":\"id1\",\"expires_in_days\":3},{\"id\":\"id2\",\"expires_in_days\":5}]")
      (let [result (tools/handle-mcp-memory-expiring-soon {})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (is (= 2 (count parsed)))
          (is (= "id1" (:id (first parsed))))))))

  (testing "Returns entries expiring within custom days"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "[{\"id\":\"id1\",\"expires_in_days\":1}]")
      (let [result (tools/handle-mcp-memory-expiring-soon {:days 3})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result))))))

  (testing "Returns empty array when no entries expiring soon"
    (with-mock-emacsclient
      (mock-emacsclient-loaded "[]")
      (let [result (tools/handle-mcp-memory-expiring-soon {:days 7})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (is (= "[]" (:text result))))))

  (testing "Returns error when hive-mcp.el not loaded"
    (with-mock-emacsclient
      (mock-emacsclient-not-loaded)
      (let [result (tools/handle-mcp-memory-expiring-soon {:days 7})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "hive-mcp.el is not loaded")))))

  (testing "Uses default of 7 days when days param is nil"
    (let [captured-elisp (atom nil)]
      (with-mock-emacsclient
        (fn [elisp]
          (when-not (str/includes? elisp "featurep")
            (reset! captured-elisp elisp))
          (if (str/includes? elisp "featurep")
            {:success true :result "t" :duration-ms 5}
            {:success true :result "[]" :duration-ms 10}))
        (tools/handle-mcp-memory-expiring-soon {})
        ;; Verify 7 is passed as default
        (is (str/includes? @captured-elisp "7"))))))

;; =============================================================================
;; handle-mcp-memory-add with duration Tests
;; =============================================================================

(deftest handle-mcp-memory-add-with-duration-test
  (testing "Adds memory entry with duration parameter"
    (let [captured-elisp (atom nil)]
      (with-mock-emacsclient
        (fn [elisp]
          (when-not (str/includes? elisp "featurep")
            (reset! captured-elisp elisp))
          (if (str/includes? elisp "featurep")
            {:success true :result "t" :duration-ms 5}
            {:success true :result "{\"id\":\"new-id\",\"duration\":\"project\"}" :duration-ms 10}))
        (let [result (tools/handle-mcp-memory-add {:type "note"
                                                   :content "Test note"
                                                   :tags ["test"]
                                                   :duration "project"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          ;; Verify duration is in the elisp call
          (is (str/includes? @captured-elisp "project"))))))

  (testing "Adds memory entry without duration (nil)"
    (let [captured-elisp (atom nil)]
      (with-mock-emacsclient
        (fn [elisp]
          (when-not (str/includes? elisp "featurep")
            (reset! captured-elisp elisp))
          (if (str/includes? elisp "featurep")
            {:success true :result "t" :duration-ms 5}
            {:success true :result "{\"id\":\"new-id\"}" :duration-ms 10}))
        (let [result (tools/handle-mcp-memory-add {:type "note"
                                                   :content "Test note"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          ;; Verify nil is passed for duration
          (is (str/includes? @captured-elisp "nil"))))))

  (testing "Returns error when hive-mcp.el not loaded"
    (with-mock-emacsclient
      (mock-emacsclient-not-loaded)
      (let [result (tools/handle-mcp-memory-add {:type "note"
                                                 :content "Test"
                                                 :duration "session"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "hive-mcp.el is not loaded"))))))

;; =============================================================================
;; handle-mcp-memory-query with duration filter Tests
;; =============================================================================

(deftest handle-mcp-memory-query-with-duration-test
  (testing "Queries memory entries filtered by duration"
    (let [captured-elisp (atom nil)]
      (with-mock-emacsclient
        (fn [elisp]
          (when-not (str/includes? elisp "featurep")
            (reset! captured-elisp elisp))
          (if (str/includes? elisp "featurep")
            {:success true :result "t" :duration-ms 5}
            {:success true :result "[{\"id\":\"id1\",\"duration\":\"permanent\"}]" :duration-ms 10}))
        (let [result (tools/handle-mcp-memory-query {:type "note"
                                                     :duration "permanent"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))
          ;; Verify duration filter is in the elisp call
          (is (str/includes? @captured-elisp "permanent"))))))

  (testing "Queries without duration filter"
    (let [captured-elisp (atom nil)]
      (with-mock-emacsclient
        (fn [elisp]
          (when-not (str/includes? elisp "featurep")
            (reset! captured-elisp elisp))
          (if (str/includes? elisp "featurep")
            {:success true :result "t" :duration-ms 5}
            {:success true :result "[{\"id\":\"id1\"},{\"id\":\"id2\"}]" :duration-ms 10}))
        (let [result (tools/handle-mcp-memory-query {:type "note"})]
          (is (= "text" (:type result)))
          (is (nil? (:isError result)))))))

  (testing "Returns error when hive-mcp.el not loaded"
    (with-mock-emacsclient
      (mock-emacsclient-not-loaded)
      (let [result (tools/handle-mcp-memory-query {:type "note" :duration "session"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "hive-mcp.el is not loaded"))))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest duration-lifecycle-integration-test
  (testing "Full lifecycle: create -> promote -> verify -> demote -> verify"
    (let [entry-id (atom nil)
          memory-store (atom {})]
      ;; Mock that simulates actual state changes
      (with-mock-emacsclient
        (fn [elisp]
          (cond
            ;; Feature check
            (str/includes? elisp "featurep")
            {:success true :result "t" :duration-ms 5}

            ;; Memory add
            (str/includes? elisp "memory-add")
            (let [id "test-entry-123"]
              (reset! entry-id id)
              (swap! memory-store assoc id {:id id :duration "session"})
              {:success true
               :result (json/write-str {:id id :duration "session"})
               :duration-ms 10})

            ;; Memory promote
            (str/includes? elisp "memory-promote")
            (let [entry (get @memory-store @entry-id)
                  old-duration (:duration entry)
                  new-duration (case old-duration
                                 "session" "project"
                                 "project" "permanent"
                                 "permanent" "permanent")]
              (swap! memory-store assoc-in [@entry-id :duration] new-duration)
              {:success true
               :result (json/write-str {:success true
                                        :id @entry-id
                                        :old_duration old-duration
                                        :new_duration new-duration})
               :duration-ms 10})

            ;; Memory demote
            (str/includes? elisp "memory-demote")
            (let [entry (get @memory-store @entry-id)
                  old-duration (:duration entry)
                  new-duration (case old-duration
                                 "permanent" "project"
                                 "project" "session"
                                 "session" "session")]
              (swap! memory-store assoc-in [@entry-id :duration] new-duration)
              {:success true
               :result (json/write-str {:success true
                                        :id @entry-id
                                        :old_duration old-duration
                                        :new_duration new-duration})
               :duration-ms 10})

            ;; Default
            :else
            {:success true :result "{}" :duration-ms 10}))

        ;; Step 1: Create entry with session duration
        (let [create-result (tools/handle-mcp-memory-add {:type "note"
                                                          :content "Integration test"
                                                          :duration "session"})]
          (is (nil? (:isError create-result)))
          (let [parsed (json/read-str (:text create-result) :key-fn keyword)]
            (is (= "session" (:duration parsed)))))

        ;; Step 2: Promote to project
        (let [promote-result (tools/handle-mcp-memory-promote {:id @entry-id})]
          (is (nil? (:isError promote-result)))
          (let [parsed (json/read-str (:text promote-result) :key-fn keyword)]
            (is (= "session" (:old_duration parsed)))
            (is (= "project" (:new_duration parsed)))))

        ;; Step 3: Verify state in memory store
        (is (= "project" (:duration (get @memory-store @entry-id))))

        ;; Step 4: Promote again to permanent
        (let [promote-result (tools/handle-mcp-memory-promote {:id @entry-id})]
          (is (nil? (:isError promote-result)))
          (let [parsed (json/read-str (:text promote-result) :key-fn keyword)]
            (is (= "project" (:old_duration parsed)))
            (is (= "permanent" (:new_duration parsed)))))

        ;; Step 5: Demote back to project
        (let [demote-result (tools/handle-mcp-memory-demote {:id @entry-id})]
          (is (nil? (:isError demote-result)))
          (let [parsed (json/read-str (:text demote-result) :key-fn keyword)]
            (is (= "permanent" (:old_duration parsed)))
            (is (= "project" (:new_duration parsed)))))

        ;; Step 6: Verify final state
        (is (= "project" (:duration (get @memory-store @entry-id))))))))

;; =============================================================================
;; Error Handling Edge Cases
;; =============================================================================

(deftest error-handling-edge-cases-test
  (testing "Handles missing id parameter gracefully"
    (with-mock-emacsclient
      (fn [elisp]
        (if (str/includes? elisp "featurep")
          {:success true :result "t" :duration-ms 5}
          {:success false :error "Missing required parameter: id" :duration-ms 10}))
      (let [result (tools/handle-mcp-memory-set-duration {:duration "session"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result))))))

  (testing "Handles entry not found error"
    (with-mock-emacsclient
      (fn [elisp]
        (if (str/includes? elisp "featurep")
          {:success true :result "t" :duration-ms 5}
          {:success false :error "Entry not found: non-existent-id" :duration-ms 10}))
      (let [result (tools/handle-mcp-memory-promote {:id "non-existent-id"})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "Entry not found")))))

  (testing "Handles elisp evaluation timeout"
    (with-mock-emacsclient
      (fn [elisp]
        (if (str/includes? elisp "featurep")
          {:success true :result "t" :duration-ms 5}
          {:success false :error "Evaluation timed out" :duration-ms 30000}))
      (let [result (tools/handle-mcp-memory-cleanup-expired {})]
        (is (= "text" (:type result)))
        (is (true? (:isError result)))
        (is (str/includes? (:text result) "timed out")))))

  (testing "Handles malformed JSON response from elisp"
    (with-mock-emacsclient
      (fn [elisp]
        (if (str/includes? elisp "featurep")
          {:success true :result "t" :duration-ms 5}
          {:success true :result "not-valid-json{" :duration-ms 10}))
      ;; The handler returns the raw text, parsing happens at caller
      (let [result (tools/handle-mcp-memory-expiring-soon {:days 7})]
        (is (= "text" (:type result)))
        ;; Raw text is returned, error handling is at caller level
        (is (= "not-valid-json{" (:text result)))))))

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
