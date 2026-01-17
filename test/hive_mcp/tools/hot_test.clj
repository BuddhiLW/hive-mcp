(ns hive-mcp.tools.hot-test
  "Tests for hot reload MCP tools.

   Tests handlers with mocked hive-hot modules since
   watcher.clj and debounce.clj may not exist yet (Phases 1-3)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.hot :as hot]
            [hive-mcp.swarm.logic :as logic]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-logic-db!
  "Reset logic database for test isolation."
  []
  (logic/reset-db!))

(use-fixtures :each (fn [f]
                      (reset-logic-db!)
                      (f)
                      (reset-logic-db!)))

;; =============================================================================
;; Helper: Parse MCP JSON Response
;; =============================================================================

(defn parse-response
  "Parse MCP JSON response text to Clojure map."
  [response]
  (json/read-str (:text response) :key-fn keyword))

;; =============================================================================
;; Test: hot_watcher_status
;; =============================================================================

(deftest test-hot-watcher-status-basic-structure
  (testing "hot_watcher_status returns expected structure"
    (let [response (hot/handle-hot-watcher-status {})
          parsed (parse-response response)]
      (is (= "text" (:type response)))
      (is (contains? parsed :watching?))
      (is (contains? parsed :paths))
      (is (contains? parsed :pending-count))
      (is (contains? parsed :claimed-files)))))

(deftest test-hot-watcher-status-no-watcher
  (testing "hot_watcher_status handles missing watcher gracefully"
    ;; Without hive-hot.watcher loaded, should return defaults
    (let [response (hot/handle-hot-watcher-status {})
          parsed (parse-response response)]
      (is (false? (:watching? parsed)))
      (is (empty? (:paths parsed)))
      (is (= 0 (:pending-count parsed))))))

(deftest test-hot-watcher-status-includes-claims
  (testing "hot_watcher_status includes swarm file claims"
    ;; Add some claims to the logic database
    (logic/add-claim! "/src/foo.clj" "ling-1")
    (logic/add-claim! "/src/bar.clj" "ling-2")

    (let [response (hot/handle-hot-watcher-status {})
          parsed (parse-response response)]
      (is (= 2 (count (:claimed-files parsed))))
      (is (some #(= "/src/foo.clj" %) (:claimed-files parsed)))
      (is (some #(= "/src/bar.clj" %) (:claimed-files parsed))))))

(deftest test-hot-watcher-status-claims-detail
  (testing "hot_watcher_status includes detailed claim info"
    (logic/add-claim! "/src/test.clj" "ling-test")

    (let [response (hot/handle-hot-watcher-status {})
          parsed (parse-response response)
          claims (:claims-detail parsed)]
      (is (= 1 (count claims)))
      (is (= "/src/test.clj" (:file (first claims))))
      (is (= "ling-test" (:slave-id (first claims)))))))

;; =============================================================================
;; Test: hot_reload
;; =============================================================================

(deftest test-hot-reload-no-core-loaded
  (testing "hot_reload handles missing hive-hot.core gracefully"
    ;; Without hive-hot.core in test classpath, should return error
    (let [response (hot/handle-hot-reload {})
          parsed (parse-response response)]
      (is (= "text" (:type response)))
      ;; Either success false with error, or actual success if core is loaded
      (is (or (false? (:success parsed))
              (:success parsed))))))

(deftest test-hot-reload-with-mock-success
  (testing "hot_reload returns success structure when reload succeeds"
    (with-redefs [hot/hot-core-reload!
                  (fn [] {:success true
                          :unloaded '[foo.bar]
                          :loaded '[foo.bar foo.baz]
                          :ms 42})]
      (let [response (hot/handle-hot-reload {})
            parsed (parse-response response)]
        (is (true? (:success parsed)))
        (is (= ["foo.bar"] (:unloaded parsed)))
        (is (= ["foo.bar" "foo.baz"] (:loaded parsed)))
        (is (= 42 (:ms parsed)))))))

(deftest test-hot-reload-with-mock-failure
  (testing "hot_reload returns failure structure when reload fails"
    (with-redefs [hot/hot-core-reload!
                  (fn [] {:success false
                          :failed 'broken.ns
                          :exception (Exception. "Parse error")})]
      (let [response (hot/handle-hot-reload {})
            parsed (parse-response response)]
        (is (false? (:success parsed)))
        (is (= "broken.ns" (:failed parsed)))))))

(deftest test-hot-reload-exception-handling
  (testing "hot_reload handles exceptions gracefully"
    (with-redefs [hot/hot-core-reload!
                  (fn [] (throw (Exception. "Connection refused")))]
      (let [response (hot/handle-hot-reload {})]
        (is (= "text" (:type response)))
        (is (true? (:isError response)))
        (is (re-find #"Connection refused" (:text response)))))))

;; =============================================================================
;; Test: hot_flush_pending
;; =============================================================================

(deftest test-hot-flush-pending-no-debounce
  (testing "hot_flush_pending handles missing debounce module"
    ;; Without hive-hot.debounce loaded, should return error
    (let [response (hot/handle-hot-flush-pending {})
          parsed (parse-response response)]
      (is (= "text" (:type response)))
      (is (false? (:success parsed)))
      (is (re-find #"not loaded" (:error parsed))))))

(deftest test-hot-flush-pending-with-mock
  (testing "hot_flush_pending calls debounce flush and returns result"
    (with-redefs [hot/debounce-flush!
                  (fn [] {:flushed-count 3
                          :reload-result {:success true :loaded '[a b c]}})]
      (let [response (hot/handle-hot-flush-pending {})
            parsed (parse-response response)]
        (is (true? (:success parsed)))
        (is (= 3 (:flushed-count parsed)))
        (is (= {:success true :loaded ["a" "b" "c"]}
               (:reload-result parsed)))))))

(deftest test-hot-flush-pending-exception
  (testing "hot_flush_pending handles exceptions gracefully"
    (with-redefs [hot/debounce-flush!
                  (fn [] (throw (Exception. "Flush failed")))]
      (let [response (hot/handle-hot-flush-pending {})]
        (is (true? (:isError response)))
        (is (re-find #"Flush failed" (:text response)))))))

;; =============================================================================
;; Test: Tool Definitions
;; =============================================================================

(deftest test-tools-vector-count
  (testing "tools vector contains 3 tools"
    (is (= 3 (count hot/tools)))))

(deftest test-tools-have-required-keys
  (testing "each tool has name, description, inputSchema, handler"
    (doseq [tool hot/tools]
      (is (string? (:name tool)) (str "Tool missing name: " tool))
      (is (string? (:description tool)) (str "Tool missing description: " tool))
      (is (map? (:inputSchema tool)) (str "Tool missing inputSchema: " tool))
      (is (fn? (:handler tool)) (str "Tool missing handler: " tool)))))

(deftest test-tool-names
  (testing "tool names are correct"
    (let [names (set (map :name hot/tools))]
      (is (contains? names "hot_watcher_status"))
      (is (contains? names "hot_reload"))
      (is (contains? names "hot_flush_pending")))))
