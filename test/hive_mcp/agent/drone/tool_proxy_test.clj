(ns hive-mcp.agent.drone.tool-proxy-test
  "TDD tests for Tool Proxy architecture.

   Two-tier model architecture:
   - Tier 1: Free reasoning models output [TOOL:name param=value] markers
   - Tier 2: Tool proxy (gpt-oss-120b) executes actual tool calls

   This test suite covers:
   - Intent parsing from model text output
   - Proxy dispatch to actual tool execution
   - Result formatting for Tier 1 consumption"
  (:require [clojure.test :refer :all]
            [hive-mcp.agent.drone.tool-proxy :as proxy]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Data - Model Output Samples
;; =============================================================================

(def simple-tool-intent
  "Basic tool intent with simple parameter."
  "Let me read that file.
[TOOL:read_file path=/src/core.clj]
Then I'll analyze it.")

(def multiple-params-intent
  "Tool intent with multiple parameters."
  "I need to search for patterns.
[TOOL:grep pattern=\"TODO\" path=/src recursive=true]
This will find all TODO comments.")

(def quoted-value-intent
  "Tool intent with quoted values containing spaces."
  "Let me write the file.
[TOOL:file_write path=/src/hello.clj content=\"(ns hello.core)\"]
Done.")

(def complex-quoted-intent
  "Tool intent with complex quoted content including special chars."
  "Writing code:
[TOOL:file_write path=/test.clj content=\"(defn foo [x]\\n  (+ x 1))\"]
File written.")

(def multiple-tools-intent
  "Model output with multiple tool intents."
  "First, read the file:
[TOOL:read_file path=/src/a.clj]
Then check another:
[TOOL:read_file path=/src/b.clj]
Now I can compare them.")

(def no-tools-intent
  "Model output with no tool intents."
  "This is just a normal response.
No tools needed here.")

(def nested-brackets-intent
  "Model output with nested brackets (code examples)."
  "Here's a code example:
```clojure
(defn foo [{:keys [a b]}]
  {:result (+ a b)})
```
Now let me read the actual file:
[TOOL:read_file path=/src/foo.clj]
Done.")

(def malformed-tool-intent
  "Malformed tool marker (missing closing bracket)."
  "Let me try:
[TOOL:read_file path=/src/core.clj
This is broken.")

(def empty-params-intent
  "Tool with no parameters."
  "Check status:
[TOOL:cider_status]
Done.")

(def equals-in-value-intent
  "Parameter value containing equals sign."
  "Set config:
[TOOL:bash command=\"export FOO=bar && echo $FOO\"]
Done.")

;; =============================================================================
;; Intent Parser Tests
;; =============================================================================

(deftest parse-simple-tool-intent
  (testing "Parse basic tool intent with single parameter"
    (let [intents (proxy/parse-tool-intents simple-tool-intent)]
      (is (= 1 (count intents))
          "Should find exactly one tool intent")
      (is (= "read_file" (:tool (first intents)))
          "Should extract tool name")
      (is (= "/src/core.clj" (get-in intents [0 :params :path]))
          "Should extract path parameter"))))

(deftest parse-multiple-params
  (testing "Parse tool intent with multiple parameters"
    (let [intents (proxy/parse-tool-intents multiple-params-intent)]
      (is (= 1 (count intents)))
      (is (= "grep" (:tool (first intents))))
      (is (= "TODO" (get-in intents [0 :params :pattern]))
          "Should extract quoted pattern")
      (is (= "/src" (get-in intents [0 :params :path]))
          "Should extract path")
      (is (= "true" (get-in intents [0 :params :recursive]))
          "Should extract boolean-like param as string"))))

(deftest parse-quoted-values
  (testing "Parse quoted parameter values"
    (let [intents (proxy/parse-tool-intents quoted-value-intent)]
      (is (= 1 (count intents)))
      (is (= "file_write" (:tool (first intents))))
      (is (= "(ns hello.core)" (get-in intents [0 :params :content]))
          "Should extract quoted content without quotes"))))

(deftest parse-complex-quoted-content
  (testing "Parse complex quoted content with escapes"
    (let [intents (proxy/parse-tool-intents complex-quoted-intent)]
      (is (= 1 (count intents)))
      (is (= "file_write" (:tool (first intents))))
      (is (string? (get-in intents [0 :params :content]))
          "Should have content parameter"))))

(deftest parse-multiple-tools
  (testing "Parse multiple tool intents in single output"
    (let [intents (proxy/parse-tool-intents multiple-tools-intent)]
      (is (= 2 (count intents))
          "Should find both tool intents")
      (is (= "/src/a.clj" (get-in intents [0 :params :path]))
          "First tool should have path a.clj")
      (is (= "/src/b.clj" (get-in intents [1 :params :path]))
          "Second tool should have path b.clj"))))

(deftest parse-no-tools
  (testing "Handle output with no tool intents"
    (let [intents (proxy/parse-tool-intents no-tools-intent)]
      (is (empty? intents)
          "Should return empty collection when no tools"))))

(deftest parse-with-nested-brackets
  (testing "Should not confuse code brackets with tool markers"
    (let [intents (proxy/parse-tool-intents nested-brackets-intent)]
      (is (= 1 (count intents))
          "Should only find the actual tool marker")
      (is (= "read_file" (:tool (first intents)))))))

(deftest parse-malformed-tool
  (testing "Handle malformed tool marker gracefully"
    (let [intents (proxy/parse-tool-intents malformed-tool-intent)]
      (is (empty? intents)
          "Should not match malformed markers"))))

(deftest parse-empty-params
  (testing "Parse tool with no parameters"
    (let [intents (proxy/parse-tool-intents empty-params-intent)]
      (is (= 1 (count intents)))
      (is (= "cider_status" (:tool (first intents))))
      (is (empty? (:params (first intents)))
          "Params should be empty map"))))

(deftest parse-equals-in-value
  (testing "Handle equals sign within quoted value"
    (let [intents (proxy/parse-tool-intents equals-in-value-intent)]
      (is (= 1 (count intents)))
      (is (= "bash" (:tool (first intents))))
      (is (= "export FOO=bar && echo $FOO" (get-in intents [0 :params :command]))
          "Should correctly parse value with embedded equals"))))

(deftest parse-nil-input
  (testing "Handle nil input gracefully"
    (is (empty? (proxy/parse-tool-intents nil)))))

(deftest parse-empty-string
  (testing "Handle empty string input"
    (is (empty? (proxy/parse-tool-intents "")))))

;; =============================================================================
;; Intent Extraction with Position Tests
;; =============================================================================

(deftest extract-intents-with-positions
  (testing "Extract intents with their text positions"
    (let [result (proxy/extract-intents-with-positions simple-tool-intent)]
      (is (= 1 (count (:intents result))))
      (is (integer? (get-in result [:intents 0 :start]))
          "Should include start position")
      (is (integer? (get-in result [:intents 0 :end]))
          "Should include end position")
      (is (string? (:text-before result))
          "Should include text before first intent")
      (is (string? (:text-after result))
          "Should include text after last intent"))))

;; =============================================================================
;; Has Tool Intent Tests (fast check)
;; =============================================================================

(deftest has-tool-intent-check
  (testing "Quick check for tool intent presence"
    (is (true? (proxy/has-tool-intent? simple-tool-intent)))
    (is (true? (proxy/has-tool-intent? multiple-tools-intent)))
    (is (false? (proxy/has-tool-intent? no-tools-intent)))
    (is (false? (proxy/has-tool-intent? nil)))
    (is (false? (proxy/has-tool-intent? "")))))

;; =============================================================================
;; Result Formatting Tests
;; =============================================================================

(def sample-tool-result
  {:tool "read_file"
   :success true
   :content "(ns foo.core)\n(defn bar [x] (+ x 1))"})

(def sample-error-result
  {:tool "read_file"
   :success false
   :error "File not found: /src/missing.clj"})

(def sample-grep-result
  {:tool "grep"
   :success true
   :matches [{:file "/src/a.clj" :line 10 :content "TODO: fix this"}
             {:file "/src/b.clj" :line 25 :content "TODO: refactor"}]})

(deftest format-tool-result-success
  (testing "Format successful tool result for Tier 1 consumption"
    (let [formatted (proxy/format-result-for-tier1 sample-tool-result)]
      (is (string? formatted)
          "Result should be formatted as string")
      (is (re-find #"RESULT" formatted)
          "Should include RESULT marker")
      (is (re-find #"read_file" formatted)
          "Should include tool name")
      (is (re-find #"ns foo.core" formatted)
          "Should include actual content"))))

(deftest format-tool-result-error
  (testing "Format error result for Tier 1 consumption"
    (let [formatted (proxy/format-result-for-tier1 sample-error-result)]
      (is (string? formatted))
      (is (re-find #"ERROR" formatted)
          "Should indicate error")
      (is (re-find #"File not found" formatted)
          "Should include error message"))))

(deftest format-multiple-results
  (testing "Format multiple tool results"
    (let [results [sample-tool-result sample-grep-result]
          formatted (proxy/format-results-for-tier1 results)]
      (is (string? formatted))
      (is (re-find #"read_file" formatted))
      (is (re-find #"grep" formatted)))))

;; =============================================================================
;; Proxy Dispatch Tests (integration)
;; =============================================================================

(deftest dispatch-intent-structure
  (testing "Dispatch returns expected structure"
    (let [intent {:tool "read_file" :params {:path "/test.clj"}}
          ;; Mock dispatch that doesn't actually call OpenRouter
          result (proxy/mock-dispatch intent)]
      (is (map? result)
          "Should return a map")
      (is (contains? result :tool)
          "Should include tool name")
      (is (contains? result :success)
          "Should include success flag"))))

;; =============================================================================
;; Full Pipeline Tests
;; =============================================================================

(deftest process-tier1-output-with-tools
  (testing "Full pipeline: parse, dispatch (mock), format"
    (let [tier1-output simple-tool-intent
          result (proxy/process-tier1-output-mock tier1-output)]
      (is (map? result))
      (is (string? (:continuation-prompt result))
          "Should produce continuation prompt for Tier 1"))))

(deftest process-tier1-output-without-tools
  (testing "Output without tools passes through unchanged"
    (let [result (proxy/process-tier1-output-mock no-tools-intent)]
      (is (nil? (:continuation-prompt result))
          "No continuation needed when no tools")
      (is (= no-tools-intent (:original-output result))))))

(comment
  ;; Run tests in REPL
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.drone.tool-proxy-test)

  ;; Run specific test
  (parse-simple-tool-intent))
