(ns hive-mcp.make-tool-test
  "TDD tests to PIN make-tool behavior before refactoring.

   CLARITY: Inputs are guarded - test content normalization and agent-id extraction.
   DDD: Testing the domain contract of tool response wrapping.

   These tests capture current behavior as executable specification."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.server :as server]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel.piggyback :as piggyback]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-state-fixture [f]
  (reset! hivemind/agent-registry {})
  (piggyback/reset-all-cursors!)
  (f))

(use-fixtures :each reset-state-fixture)

;; =============================================================================
;; Content Normalization Tests (SRP: Single transformation rules)
;; =============================================================================

(deftest test-content-normalization-map-to-vector
  (testing "Map result is normalized to vector of one item"
    (let [test-tool {:name "map-tool"
                     :description "Returns a map"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "single-map"})}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      (is (vector? (:content result)) "Content should be a vector")
      (is (= 1 (count (:content result))) "Vector should have one item")
      (is (= {:type "text" :text "single-map"} (first (:content result)))
          "Item should be the original map"))))

(deftest test-content-normalization-sequential-to-vec
  (testing "Sequential result is converted to vec"
    (let [test-tool {:name "seq-tool"
                     :description "Returns a list (sequential)"
                     :inputSchema {}
                     ;; Return a list (sequential but not vector)
                     :handler (fn [_] '({:type "text" :text "item1"}
                                        {:type "text" :text "item2"}))}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      (is (vector? (:content result)) "Content should be a vector, not list")
      (is (= 2 (count (:content result))) "Should have 2 items")
      (is (= "item1" (get-in (:content result) [0 :text])) "First item preserved")
      (is (= "item2" (get-in (:content result) [1 :text])) "Second item preserved"))))

(deftest test-content-normalization-vector-passthrough
  (testing "Vector result is preserved as-is"
    (let [test-tool {:name "vec-tool"
                     :description "Returns a vector"
                     :inputSchema {}
                     :handler (fn [_] [{:type "text" :text "a"}
                                       {:type "text" :text "b"}])}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      (is (vector? (:content result)) "Content should be a vector")
      (is (= 2 (count (:content result))) "Should have 2 items"))))

(deftest test-content-normalization-non-map-non-seq
  (testing "Non-map/non-seq result is wrapped in text item"
    (let [test-tool {:name "string-tool"
                     :description "Returns a plain string"
                     :inputSchema {}
                     :handler (fn [_] "plain-string")}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      (is (vector? (:content result)) "Content should be a vector")
      (is (= 1 (count (:content result))) "Should have one item")
      (is (= "text" (get-in (:content result) [0 :type])) "Type should be text")
      (is (= "plain-string" (get-in (:content result) [0 :text])) "Text should be stringified"))))

;; =============================================================================
;; Piggyback Embedding Tests (CLARITY: Telemetry first)
;; =============================================================================

(deftest test-piggyback-with-hivemind-markers
  (testing "Piggyback messages are embedded with HIVEMIND delimiters"
    ;; Add a message to piggyback
    (hivemind/shout! "worker" :progress {:task "test" :message "progress-msg"})

    (let [test-tool {:name "status-tool"
                     :description "Get status"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "OK"})}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      ;; Content should have the piggyback appended
      (is (vector? (:content result)) "Content should be vector")
      (let [text (get-in (:content result) [0 :text])]
        (is (clojure.string/includes? text "---HIVEMIND---")
            "Should contain opening marker")
        (is (clojure.string/includes? text "---/HIVEMIND---")
            "Should contain closing marker")
        (is (clojure.string/includes? text ":a \"worker\"")
            "Should contain agent-id")
        (is (clojure.string/includes? text ":e \"progress\"")
            "Should contain event type")
        (is (clojure.string/includes? text ":m \"progress-msg\"")
            "Should contain message")))))

(deftest test-empty-piggyback-no-markers
  (testing "No HIVEMIND markers when piggyback is empty"
    ;; No shouts - piggyback should be empty
    (let [test-tool {:name "clean-tool"
                     :description "No piggyback"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "result"})}
          wrapped (server/make-tool test-tool)
          ;; First call might consume any existing piggyback
          _ ((:handler wrapped) {})
          ;; Second call should definitely have no piggyback
          result ((:handler wrapped) {})]
      (let [text (get-in (:content result) [0 :text])]
        (is (not (clojure.string/includes? text "---HIVEMIND---"))
            "Should NOT contain HIVEMIND markers")
        (is (= "result" text) "Should just be the original result")))))

(deftest test-piggyback-consumed-after-read
  (testing "Piggyback is consumed after being read"
    (hivemind/shout! "agent" :started {:message "started"})

    (let [test-tool {:name "consume-tool"
                     :description "test"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "done"})}
          wrapped (server/make-tool test-tool)]
      ;; First call gets the piggyback
      (let [result1 ((:handler wrapped) {})]
        (is (clojure.string/includes? (get-in (:content result1) [0 :text])
                                      "---HIVEMIND---")
            "First call should have piggyback"))
      ;; Second call should not have it
      (let [result2 ((:handler wrapped) {})]
        (is (not (clojure.string/includes? (get-in (:content result2) [0 :text])
                                           "---HIVEMIND---"))
            "Second call should NOT have piggyback")))))

;; =============================================================================
;; Agent-ID Extraction Tests (ISP: Interface Segregation)
;; =============================================================================

(deftest test-agent-id-keyword-extraction
  (testing "Agent-id is extracted from :agent-id keyword in args"
    ;; Add message for specific agent
    (hivemind/shout! "sender" :progress {:message "for-agent-a"})

    (let [test-tool {:name "id-tool"
                     :description "test"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "x"})}
          wrapped (server/make-tool test-tool)
          ;; Call with keyword :agent-id
          result ((:handler wrapped) {:agent-id "agent-a"})]
      ;; Should have gotten the piggyback (agent-a hadn't read yet)
      (is (clojure.string/includes? (get-in (:content result) [0 :text])
                                    "---HIVEMIND---")
          "Agent-a should receive piggyback"))))

(deftest test-agent-id-string-extraction
  (testing "Agent-id is extracted from string \"agent-id\" key in args"
    (hivemind/shout! "sender" :progress {:message "for-agent-b"})

    (let [test-tool {:name "id-tool"
                     :description "test"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "x"})}
          wrapped (server/make-tool test-tool)
          ;; Call with string key "agent-id"
          result ((:handler wrapped) {"agent-id" "agent-b"})]
      (is (clojure.string/includes? (get-in (:content result) [0 :text])
                                    "---HIVEMIND---")
          "Agent-b should receive piggyback via string key"))))

(deftest test-agent-id-defaults-to-coordinator
  (testing "Default agent-id is 'coordinator' when not provided"
    (hivemind/shout! "worker" :completed {:message "done"})

    (let [test-tool {:name "default-tool"
                     :description "test"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "x"})}
          wrapped (server/make-tool test-tool)
          ;; Call without agent-id - should default to "coordinator"
          _ ((:handler wrapped) {})
          ;; Now call as "coordinator" - should have no new messages
          result2 ((:handler wrapped) {})]
      ;; The first call consumed as "coordinator", so second call has nothing
      (is (not (clojure.string/includes? (get-in (:content result2) [0 :text])
                                         "---HIVEMIND---"))
          "Second coordinator call should have no piggyback (already consumed)"))))

;; =============================================================================
;; Multiple Content Items Tests (OCP: Open for extension)
;; =============================================================================

(deftest test-piggyback-appended-to-last-text-item
  (testing "Piggyback is appended to the LAST text item in content"
    (hivemind/shout! "worker" :progress {:message "append-test"})

    (let [test-tool {:name "multi-tool"
                     :description "Returns multiple items"
                     :inputSchema {}
                     ;; Return multiple content items
                     :handler (fn [_] [{:type "text" :text "first"}
                                       {:type "image" :data "base64..."}
                                       {:type "text" :text "last"}])}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      (is (= 3 (count (:content result))) "Should have 3 items")
      ;; First text item should be unchanged
      (is (= "first" (get-in (:content result) [0 :text]))
          "First item unchanged")
      ;; Image item unchanged
      (is (= "image" (get-in (:content result) [1 :type]))
          "Image item unchanged")
      ;; Last text item should have piggyback appended
      (let [last-text (get-in (:content result) [2 :text])]
        (is (clojure.string/starts-with? last-text "last")
            "Last item starts with original text")
        (is (clojure.string/includes? last-text "---HIVEMIND---")
            "Last item has piggyback appended")))))

(deftest test-piggyback-creates-text-item-when-no-text
  (testing "Piggyback creates new text item when content has no text items"
    (hivemind/shout! "worker" :error {:message "error-msg"})

    (let [test-tool {:name "image-only-tool"
                     :description "Returns only images"
                     :inputSchema {}
                     ;; Return only non-text items
                     :handler (fn [_] [{:type "image" :data "img1"}
                                       {:type "image" :data "img2"}])}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      ;; Should have 3 items now (2 images + 1 text for piggyback)
      (is (= 3 (count (:content result))) "Should have 3 items (2 images + 1 text)")
      ;; Images unchanged
      (is (= "image" (get-in (:content result) [0 :type])) "First image preserved")
      (is (= "image" (get-in (:content result) [1 :type])) "Second image preserved")
      ;; New text item added with piggyback
      (let [new-item (get (:content result) 2)]
        (is (= "text" (:type new-item)) "New item is text type")
        (is (clojure.string/includes? (:text new-item) "---HIVEMIND---")
            "New text item contains piggyback")))))

(deftest test-multiple-piggyback-messages
  (testing "Multiple hivemind messages are all included in piggyback"
    ;; Send multiple messages
    (hivemind/shout! "agent-1" :started {:message "msg1"})
    (Thread/sleep 10)
    (hivemind/shout! "agent-2" :progress {:message "msg2"})
    (Thread/sleep 10)
    (hivemind/shout! "agent-3" :completed {:message "msg3"})

    (let [test-tool {:name "multi-msg-tool"
                     :description "test"
                     :inputSchema {}
                     :handler (fn [_] {:type "text" :text "result"})}
          wrapped (server/make-tool test-tool)
          result ((:handler wrapped) {})]
      (let [text (get-in (:content result) [0 :text])]
        (is (clojure.string/includes? text "agent-1") "Contains agent-1")
        (is (clojure.string/includes? text "agent-2") "Contains agent-2")
        (is (clojure.string/includes? text "agent-3") "Contains agent-3")
        (is (clojure.string/includes? text "msg1") "Contains msg1")
        (is (clojure.string/includes? text "msg2") "Contains msg2")
        (is (clojure.string/includes? text "msg3") "Contains msg3")))))

;; =============================================================================
;; Edge Cases (DIP: Defensive programming)
;; =============================================================================

(deftest test-nil-result-from-handler
  (testing "nil result from handler is stringified"
    (let [test-tool {:name "nil-tool"
                     :description "Returns nil"
                     :inputSchema {}
                     :handler (fn [_] nil)}
          wrapped (server/make-tool test-tool)]
      ;; Should not throw
      (let [result ((:handler wrapped) {})]
        (is (vector? (:content result)) "Content should be vector")
        ;; nil will be stringified since it's not map/sequential
        (is (= "text" (get-in (:content result) [0 :type])) "Type is text")))))

(deftest test-empty-vector-from-handler
  (testing "Empty vector from handler is preserved"
    (let [test-tool {:name "empty-tool"
                     :description "Returns empty vector"
                     :inputSchema {}
                     :handler (fn [_] [])}
          wrapped (server/make-tool test-tool)]
      (let [result ((:handler wrapped) {})]
        (is (vector? (:content result)) "Content should be vector")
        (is (empty? (:content result)) "Content should be empty")))))
