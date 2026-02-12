(ns hive-mcp.tools.swarm.lazy-preset-integration-test
  "Integration tests for lazy preset loading via consolidated preset tool.
   
   Tests that the preset(command: 'header') works correctly with lazy=true vs lazy=false.
   
   Wave 3 - Step 8: E2E test for preset on-demand loading."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.consolidated.preset :as preset]
            [hive-mcp.tools.presets :as preset-handlers]
            [hive-mcp.presets.core :as presets-core]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- parse-result
  "Parse the JSON text from a tool result."
  [result]
  (when-let [text (:text result)]
    (try
      (json/read-str text :key-fn keyword)
      (catch Exception _ nil))))

(defn- estimate-tokens
  "Estimate token count (~4 chars per token)."
  [s]
  (when s (/ (count s) 4)))

(defn- presets-dir-exists?
  "Check if presets directory exists."
  []
  (let [dir (or (System/getenv "HIVE_MCP_PRESETS_DIR")
                (str (System/getProperty "user.dir") "/presets"))]
    (.isDirectory (io/file dir))))

;; =============================================================================
;; Preset Header Command Tests
;; =============================================================================

(deftest preset-header-lazy-mode-test
  (testing "Header with lazy=true returns compact instructions"
    (let [result (preset/handle-preset {:command "header"
                                        :presets ["ling" "mcp-first"]
                                        :lazy true})
          parsed (parse-result result)
          header (:header parsed)]
      ;; Should succeed
      (is (not (:isError result))
          "Lazy header should succeed")
      ;; Should return lazy mode indicator
      (is (= "lazy" (:mode parsed))
          "Should indicate lazy mode")
      ;; Should be compact (< 2000 chars for any number of presets)
      (is (< (count header) 2000)
          (format "Lazy header should be compact, got %d chars" (count header)))
      ;; Should have approximate token count
      (is (number? (:approx-tokens parsed))
          "Should include approx-tokens estimate")
      (is (< (:approx-tokens parsed) 500)
          "Lazy mode should estimate <500 tokens")))

  (testing "Header with lazy=false returns full content"
    (when (presets-dir-exists?)
      (let [result (preset/handle-preset {:command "header"
                                          :presets ["ling"]
                                          :lazy false})
            parsed (parse-result result)
            header (:header parsed)]
        ;; Should succeed
        (is (not (:isError result))
            "Full header should succeed")
        ;; Should return full mode indicator
        (is (= "full" (:mode parsed))
            "Should indicate full mode")
        ;; Full ling preset is ~6000+ chars (~1500 tokens)
        (when (and header (pos? (count header)))
          (is (> (count header) 1000)
              (format "Full header should include preset content, got %d chars" (count header))))
        ;; Should have higher token estimate
        (is (number? (:approx-tokens parsed))
            "Should include approx-tokens estimate")))))

(deftest preset-header-default-lazy-test
  (testing "Header defaults to lazy=true when not specified"
    (let [result (preset/handle-preset {:command "header"
                                        :presets ["ling"]})
          parsed (parse-result result)]
      (is (= "lazy" (:mode parsed))
          "Default mode should be lazy"))))

(deftest preset-header-token-comparison-test
  (testing "Lazy mode saves significant tokens compared to full mode"
    (when (presets-dir-exists?)
      (let [lazy-result (preset/handle-preset {:command "header"
                                               :presets ["ling" "mcp-first" "hivemind"]
                                               :lazy true})
            full-result (preset/handle-preset {:command "header"
                                               :presets ["ling" "mcp-first" "hivemind"]
                                               :lazy false})
            lazy-parsed (parse-result lazy-result)
            full-parsed (parse-result full-result)
            lazy-header (:header lazy-parsed)
            full-header (:header full-parsed)]
        (when (and lazy-header full-header 
                   (pos? (count lazy-header)) 
                   (pos? (count full-header)))
          ;; Lazy should be dramatically smaller
          (is (< (count lazy-header) (* 0.3 (count full-header)))
              (format "Lazy (%d chars) should be <30%% of full (%d chars)"
                      (count lazy-header) (count full-header)))
          ;; Token estimates should reflect this
          (is (< (:approx-tokens lazy-parsed) (* 0.3 (:approx-tokens full-parsed)))
              (format "Lazy tokens (%d) should be <30%% of full tokens (%d)"
                      (:approx-tokens lazy-parsed) (:approx-tokens full-parsed))))))))

;; =============================================================================
;; Preset Core Command Tests (Related to lazy loading workflow)
;; =============================================================================

(deftest preset-core-returns-summary-test
  (testing "Core command returns summary without full content"
    (when (presets-dir-exists?)
      (let [core-result (preset/handle-preset {:command "core"
                                               :name "ling"})
            get-result (preset/handle-preset {:command "get"
                                              :name "ling"})
            core-parsed (parse-result core-result)
            get-parsed (parse-result get-result)]
        ;; Core should succeed
        (is (not (:isError core-result))
            "Core command should succeed")
        ;; Core should be smaller than full get (when both work)
        (when (and core-parsed get-parsed
                   (not (:error core-parsed))
                   (not (:error get-parsed)))
          (let [core-size (count (str core-parsed))
                get-size (count (str get-parsed))]
            (is (< core-size get-size)
                (format "Core (%d) should be smaller than full get (%d)"
                        core-size get-size))))))))

;; =============================================================================
;; Preset List Slim Command Tests (Related to lazy loading workflow)
;; =============================================================================

(deftest preset-list-slim-returns-names-only-test
  (testing "List slim returns names and categories without full content"
    (let [slim-result (preset/handle-preset {:command "list_slim"})
          full-result (preset/handle-preset {:command "list"})
          slim-parsed (parse-result slim-result)
          full-parsed (parse-result full-result)]
      ;; Slim should succeed
      (is (not (:isError slim-result))
          "List slim command should succeed")
      ;; Both should return arrays
      (when (and (sequential? slim-parsed) (sequential? full-parsed))
        ;; Slim should be notably smaller (no content field)
        (let [slim-size (count (str slim-parsed))
              full-size (count (str full-parsed))]
          ;; If full has content, slim should be much smaller
          (when (and (> full-size 1000) (> slim-size 0))
            (is (< slim-size full-size)
                (format "Slim list (%d) should be smaller than full list (%d)"
                        slim-size full-size))))))))

;; =============================================================================
;; End-to-End Workflow Test
;; =============================================================================

(deftest lazy-loading-workflow-test
  (testing "Complete lazy loading workflow: header -> core -> get"
    (when (presets-dir-exists?)
      ;; Step 1: Get lazy header (what ling receives at spawn)
      (let [header-result (preset/handle-preset {:command "header"
                                                 :presets ["tdd"]
                                                 :lazy true})
            header-parsed (parse-result header-result)]
        (is (= "lazy" (:mode header-parsed))
            "Step 1: Should get lazy header")
        
        ;; Step 2: Ling wants quick orientation - use core
        (let [core-result (preset/handle-preset {:command "core"
                                                 :name "tdd"})
              core-parsed (parse-result core-result)]
          (is (not (:isError core-result))
              "Step 2: Core lookup should succeed")
          
          ;; Step 3: Ling needs full guidance - use get
          (let [get-result (preset/handle-preset {:command "get"
                                                  :name "tdd"})
                get-parsed (parse-result get-result)]
            (is (not (:isError get-result))
                "Step 3: Full get should succeed")
            
            ;; Verify the workflow saved tokens
            (when (and (:header header-parsed) 
                       (not (:error get-parsed))
                       (:content get-parsed))
              (let [header-tokens (estimate-tokens (:header header-parsed))
                    full-tokens (estimate-tokens (:content get-parsed))]
                (is (< header-tokens (* 0.5 full-tokens))
                    (format "Header (%.0f tokens) should be <50%% of full content (%.0f tokens)"
                            (float header-tokens) (float full-tokens)))))))))))

;; =============================================================================
;; Error Handling Tests
;; =============================================================================

(deftest preset-header-error-handling-test
  (testing "Header with missing presets parameter returns error"
    (let [result (preset/handle-preset {:command "header"
                                        :lazy true})
          parsed (parse-result result)]
      ;; Should handle gracefully (empty or error)
      (is (or (:error parsed) 
              (nil? (:header parsed))
              (= "" (:header parsed)))
          "Missing presets should be handled")))

  (testing "Header with empty presets array returns empty/nil header"
    (let [result (preset/handle-preset {:command "header"
                                        :presets []
                                        :lazy true})
          parsed (parse-result result)]
      (is (or (nil? (:header parsed))
              (= "" (:header parsed)))
          "Empty presets should return empty header"))))
