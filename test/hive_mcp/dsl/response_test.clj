(ns hive-mcp.dsl.response-test
  "Tests for MCP response processing layer.
   Verifies noop pass-through when no extensions registered,
   and parameter resolution logic."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.dsl.response :as r]))

;; =============================================================================
;; Test Fixtures: Realistic Response Shapes
;; =============================================================================

(def sample-memory-entry
  "Realistic memory entry as returned by entry->json-alist."
  {:id "20260210123456-abcdef01"
   :type "convention"
   :content "When using requiring-resolve, always provide fallback noop."
   :tags ["clojure" "patterns" "conventions"]
   :project-id "hive-mcp"
   :content-hash "sha256-abc123def456"
   :created "2026-02-10T12:34:56Z"
   :updated "2026-02-10T12:34:56Z"
   :duration "long"
   :expires nil
   :access-count 5
   :helpful-count 3
   :unhelpful-count 0
   :abstraction-level 2
   :grounded-at "2026-02-10T12:00:00Z"
   :grounded-from "sha256-000111"
   :knowledge-gaps []
   :source-hash "sha256-999aaa"
   :source-file "src/hive_mcp/agent/config.clj"
   :staleness-alpha 1.0
   :staleness-beta 0.95
   :staleness-source "direct"
   :staleness-depth 0
   :document "When using requiring-resolve, always provide fallback noop."
   :kg_outgoing_ids ["edge-1" "edge-2"]
   :kg_incoming_ids ["edge-3"]})

(def sample-kg-edge
  "Realistic KG edge as returned by edge->json-map."
  {:id "edge-abc123"
   :from "20260210-aaa"
   :to "20260210-bbb"
   :relation "implements"
   :confidence 0.9
   :scope "hive-mcp"
   :created_by "ling-worker-1"
   :created_at "2026-02-10T12:00:00Z"
   :last_verified "2026-02-10T13:00:00Z"
   :source_type "manual"})

(def sample-query-result
  "Realistic memory query response (JSON-parsed)."
  [{:id "20260210-001"
    :type "note"
    :content "Important finding about auth flow."
    :tags ["auth" "security"]
    :project-id "hive-mcp"
    :content-hash "sha256-111"
    :created "2026-02-10T10:00:00Z"}
   {:id "20260210-002"
    :type "decision"
    :content "Use DataScript for unified state."
    :tags ["architecture" "datascript"]
    :project-id "hive-mcp"
    :content-hash "sha256-222"
    :created "2026-02-10T11:00:00Z"}])

(def sample-batch-op-success
  "A successful batch operation result."
  {:id "op-1" :success true :result "{\"id\":\"mem-123\"}"})

(def sample-batch-op-failure
  "A failed batch operation result."
  {:id "op-2" :success false :error "Tool not found: bogus"})

(def sample-batch-envelope
  "Realistic multi-op batch envelope."
  {:success true
   :summary {:total 3 :success 2 :failed 1 :waves 2}
   :waves {"wave_1" [sample-batch-op-success
                     {:id "op-2" :success true :result "{\"ok\":true}"}]
           "wave_2" [sample-batch-op-failure]}})

;; =============================================================================
;; Tests: Noop Pass-Through — omit-defaults
;; =============================================================================

(deftest omit-defaults-noop-returns-input
  (testing "returns input unchanged when no extension registered"
    (let [m {:a 1 :b nil :c ""}]
      (is (= m (r/omit-defaults m))))))

(deftest omit-defaults-noop-nil-input
  (is (nil? (r/omit-defaults nil))))

(deftest omit-defaults-noop-preserves-all-fields
  (is (= sample-memory-entry (r/omit-defaults sample-memory-entry))))

;; =============================================================================
;; Tests: Noop Pass-Through — strip-verbose
;; =============================================================================

(deftest strip-verbose-noop-returns-input
  (testing "returns input unchanged when no extension registered"
    (is (= sample-memory-entry (r/strip-verbose sample-memory-entry)))))

(deftest strip-verbose-noop-nil-input
  (is (nil? (r/strip-verbose nil))))

(deftest strip-verbose-noop-kg-edge
  (is (= sample-kg-edge (r/strip-verbose sample-kg-edge))))

;; =============================================================================
;; Tests: Noop Pass-Through — abbreviate-keys
;; =============================================================================

(deftest abbreviate-keys-noop-returns-input
  (testing "returns input unchanged when no extension registered"
    (let [m {:id "x" :type "note" :content "hello" :tags ["a"]}]
      (is (= m (r/abbreviate-keys m))))))

(deftest abbreviate-keys-noop-nil-input
  (is (nil? (r/abbreviate-keys nil))))

;; =============================================================================
;; Tests: Noop Pass-Through — minimal-entry
;; =============================================================================

(deftest minimal-entry-noop-returns-input
  (testing "returns input unchanged when no extension registered"
    (is (= sample-memory-entry (r/minimal-entry sample-memory-entry)))))

;; =============================================================================
;; Tests: Noop Pass-Through — compact-response
;; =============================================================================

(deftest compact-response-noop-single-entry
  (is (= sample-memory-entry (r/compact-response sample-memory-entry))))

(deftest compact-response-noop-query-list
  (is (= sample-query-result (r/compact-response sample-query-result))))

(deftest compact-response-noop-passthrough-primitives
  (is (= "hello" (r/compact-response "hello")))
  (is (= 42 (r/compact-response 42)))
  (is (nil? (r/compact-response nil))))

;; =============================================================================
;; Tests: Noop Pass-Through — minimal-response
;; =============================================================================

(deftest minimal-response-noop-single-entry
  (is (= sample-memory-entry (r/minimal-response sample-memory-entry))))

(deftest minimal-response-noop-passthrough-primitives
  (is (= "hello" (r/minimal-response "hello")))
  (is (= 42 (r/minimal-response 42)))
  (is (nil? (r/minimal-response nil))))

;; =============================================================================
;; Tests: Noop Pass-Through — compress (mode dispatcher)
;; =============================================================================

(deftest compress-full-mode-noop
  (is (= sample-memory-entry (r/compress sample-memory-entry :full)))
  (is (= sample-memory-entry (r/compress sample-memory-entry nil))))

(deftest compress-compact-mode-noop
  (testing "noop returns input unchanged regardless of mode"
    (is (= sample-memory-entry (r/compress sample-memory-entry :compact)))))

(deftest compress-minimal-mode-noop
  (testing "noop returns input unchanged regardless of mode"
    (is (= sample-memory-entry (r/compress sample-memory-entry :minimal)))))

(deftest compress-string-mode-noop
  (is (= sample-memory-entry (r/compress sample-memory-entry "compact"))))

(deftest compress-unknown-mode-noop
  (is (= sample-memory-entry (r/compress sample-memory-entry :turbo))))

;; =============================================================================
;; Tests: Noop Pass-Through — compress-mcp-text
;; =============================================================================

(deftest compress-mcp-text-noop-compact
  (let [item {:type "text" :text "{\"id\":\"x\"}"}]
    (is (= item (r/compress-mcp-text item :compact)))))

(deftest compress-mcp-text-noop-full-passthrough
  (let [item {:type "text" :text "original"}]
    (is (= item (r/compress-mcp-text item :full)))
    (is (= item (r/compress-mcp-text item nil)))))

(deftest compress-mcp-text-noop-error-passthrough
  (let [item {:type "text" :text "some json" :isError true}]
    (is (= item (r/compress-mcp-text item :compact)))))

(deftest compress-mcp-text-noop-non-text-type
  (let [item {:type "image" :data "base64..."}]
    (is (= item (r/compress-mcp-text item :compact)))))

;; =============================================================================
;; Tests: Noop Pass-Through — compress-content
;; =============================================================================

(deftest compress-content-noop-vector
  (let [content [{:type "text" :text "{\"id\":\"a\"}"}]]
    (is (= content (r/compress-content content :compact)))))

(deftest compress-content-noop-full-passthrough
  (let [content [{:type "text" :text "hello"}]]
    (is (= content (r/compress-content content :full)))
    (is (= content (r/compress-content content nil)))))

;; =============================================================================
;; Tests: Noop Pass-Through — compress-batch-op
;; =============================================================================

(deftest compress-batch-op-noop-success
  (is (= sample-batch-op-success (r/compress-batch-op sample-batch-op-success))))

(deftest compress-batch-op-noop-failure
  (is (= sample-batch-op-failure (r/compress-batch-op sample-batch-op-failure))))

;; =============================================================================
;; Tests: Noop Pass-Through — compress-batch-envelope
;; =============================================================================

(deftest compress-batch-envelope-noop-multi-op
  (is (= sample-batch-envelope (r/compress-batch-envelope sample-batch-envelope))))

(deftest compress-batch-envelope-noop-preserves-structure
  (let [result (r/compress-batch-envelope sample-batch-envelope)]
    (testing "preserves all top-level keys"
      (is (= true (:success result)))
      (is (= {:total 3 :success 2 :failed 1 :waves 2} (:summary result)))
      (is (= 2 (count (:waves result)))))))

;; =============================================================================
;; Tests: ling-caller? (auto-detection predicate — unchanged)
;; =============================================================================

(deftest ling-caller-swarm-prefix
  (is (r/ling-caller? "swarm-worker-1770255229"))
  (is (r/ling-caller? "swarm-impl-autocompact-1770860775")))

(deftest ling-caller-ling-prefix
  (is (r/ling-caller? "ling-worker-1")))

(deftest ling-caller-coordinator
  (is (nil? (r/ling-caller? "coordinator"))))

(deftest ling-caller-nil
  (is (nil? (r/ling-caller? nil))))

(deftest ling-caller-empty-string
  (is (nil? (r/ling-caller? ""))))

;; =============================================================================
;; Tests: resolve-compress-mode (param resolution — unchanged)
;; =============================================================================

(deftest resolve-mode-boolean-true
  (is (= :compact (r/resolve-compress-mode {:compact true}))))

(deftest resolve-mode-boolean-false
  (is (nil? (r/resolve-compress-mode {:compact false}))))

(deftest resolve-mode-nil
  (is (nil? (r/resolve-compress-mode {})))
  (is (nil? (r/resolve-compress-mode {:compact nil}))))

(deftest resolve-mode-string-compact
  (is (= :compact (r/resolve-compress-mode {:compact "compact"}))))

(deftest resolve-mode-string-minimal
  (is (= :minimal (r/resolve-compress-mode {:compact "minimal"}))))

(deftest resolve-mode-string-full
  (is (= :full (r/resolve-compress-mode {:compact "full"}))))

(deftest resolve-mode-keyword
  (is (= :minimal (r/resolve-compress-mode {:compact :minimal}))))

(deftest resolve-mode-invalid-string
  (is (nil? (r/resolve-compress-mode {:compact "turbo"}))))

;; =============================================================================
;; Tests: resolve-compress-mode with auto-detection (unchanged)
;; =============================================================================

(deftest resolve-mode-auto-detect-swarm-caller
  (is (= :compact (r/resolve-compress-mode {:_caller_id "swarm-worker-123"}))))

(deftest resolve-mode-auto-detect-ling-caller
  (is (= :compact (r/resolve-compress-mode {:_caller_id "ling-xyz-456"}))))

(deftest resolve-mode-no-auto-detect-for-coordinator
  (is (nil? (r/resolve-compress-mode {:_caller_id "coordinator"}))))

(deftest resolve-mode-no-auto-detect-without-caller-id
  (is (nil? (r/resolve-compress-mode {}))))

(deftest resolve-mode-explicit-false-overrides-auto-detect
  (testing "compact:false overrides ling auto-detection"
    (is (nil? (r/resolve-compress-mode {:compact false :_caller_id "swarm-worker-123"})))))

(deftest resolve-mode-explicit-true-with-ling-caller
  (testing "compact:true with ling caller still gives :compact"
    (is (= :compact (r/resolve-compress-mode {:compact true :_caller_id "swarm-worker-123"})))))

(deftest resolve-mode-explicit-minimal-overrides-auto-detect
  (testing "compact:minimal overrides auto-detect :compact"
    (is (= :minimal (r/resolve-compress-mode {:compact "minimal" :_caller_id "swarm-worker-123"})))))

(deftest resolve-mode-explicit-full-with-ling-caller
  (testing "compact:full with ling caller gives :full (explicit override)"
    (is (= :full (r/resolve-compress-mode {:compact "full" :_caller_id "swarm-worker-123"})))))

;; =============================================================================
;; Tests: format-results-compact — batch envelope compression
;; =============================================================================

(deftest format-results-compact-basic-execution
  (testing "flattens waves into :r, abbreviates envelope and summary keys"
    (let [result (r/format-results-compact sample-batch-envelope)]
      (is (true? (:ok result)))
      (is (= {:t 3 :ok 2 :f 1 :w 2} (:s result)))
      (is (= 3 (count (:r result))))
      (is (nil? (:success result)) "original :success key should not appear")
      (is (nil? (:summary result)) "original :summary key should not appear")
      (is (nil? (:waves result)) "original :waves key should not appear"))))

(deftest format-results-compact-op-result-shape
  (testing "each compacted op has :id :ok and optional :d/:e"
    (let [ops (:r (r/format-results-compact sample-batch-envelope))
          op-1 (first ops)
          op-fail (last ops)]
      (is (= "op-1" (:id op-1)))
      (is (true? (:ok op-1)))
      (is (= "{\"id\":\"mem-123\"}" (:d op-1)))
      (is (nil? (:e op-1)))
      (is (false? (:ok op-fail)))
      (is (= "Tool not found: bogus" (:e op-fail))))))

(deftest format-results-compact-wave-ordering
  (testing "results from wave_1 appear before wave_2"
    (let [ops (:r (r/format-results-compact sample-batch-envelope))]
      ;; wave_1 has op-1 and op-2(success), wave_2 has op-2(failure)
      (is (= ["op-1" "op-2"] (mapv :id (take 2 ops))))
      (is (= "op-1" (:id (first ops))))
      (is (true? (:ok (second ops))))
      (is (false? (:ok (nth ops 2)))))))

(deftest format-results-compact-dry-run
  (testing "dry-run includes :dr and :p, no :r"
    (let [input {:success true
                 :summary {:total 2 :success 0 :failed 0 :waves 1}
                 :dry_run true
                 :plan {"wave_1" [{:id "a" :tool "memory" :command "add"}]}}
          result (r/format-results-compact input)]
      (is (true? (:ok result)))
      (is (true? (:dr result)))
      (is (some? (:p result)))
      (is (nil? (:r result))))))

(deftest format-results-compact-errors
  (testing "validation errors appear under :E"
    (let [input {:success false
                 :summary {:total 1 :success 0 :failed 0 :waves 0}
                 :errors ["Op missing :id" "Op missing :tool"]}
          result (r/format-results-compact input)]
      (is (false? (:ok result)))
      (is (= ["Op missing :id" "Op missing :tool"] (:E result)))
      (is (nil? (:r result))))))

(deftest format-results-compact-empty-waves
  (testing "empty waves produces empty :r"
    (let [input {:success true
                 :summary {:total 0 :success 0 :failed 0 :waves 0}
                 :waves {}}
          result (r/format-results-compact input)]
      (is (= [] (:r result)))
      (is (= {:t 0 :ok 0 :f 0 :w 0} (:s result))))))

(deftest format-results-compact-nil-summary
  (testing "nil summary becomes empty abbreviated map"
    (let [input {:success true :summary nil :waves {}}
          result (r/format-results-compact input)]
      (is (= {} (:s result))))))

(deftest format-results-compact-preserves-unknown-summary-keys
  (testing "unknown summary keys pass through unabbreviated"
    (let [input {:success true
                 :summary {:total 1 :success 1 :failed 0 :waves 1 :custom "extra"}
                 :waves {}}
          result (r/format-results-compact input)]
      (is (= "extra" (:custom (:s result))))
      (is (= 1 (:t (:s result)))))))
