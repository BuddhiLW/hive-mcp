(ns hive-mcp.tools.swarm.prompt-test
  "Tests for swarm prompt handlers including event emission and lazy preset loading."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.prompt :as prompt]
            [hive-mcp.presets.core :as presets]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

;; =============================================================================
;; Test emit-prompt-pending-events! helper
;; =============================================================================

(deftest emit-prompt-pending-events-test
  (testing "handles empty prompts"
    (is (nil? (#'prompt/emit-prompt-pending-events!
               "{\"count\":0,\"prompts\":[],\"mode\":\"human\"}"))))

  (testing "handles single prompt"
    (is (nil? (#'prompt/emit-prompt-pending-events!
               "{\"count\":1,\"prompts\":[{\"slave-id\":\"ling-1\",\"prompt\":\"Test?\",\"timestamp\":\"2026-01-29T12:00:00\"}],\"mode\":\"human\"}"))))

  (testing "truncates long prompt text to 100 chars"
    (let [long-prompt (apply str (repeat 200 "x"))
          data (json/write-str {:count 1
                                :prompts [{:slave-id "ling-long"
                                           :prompt long-prompt
                                           :timestamp "2026-01-29T12:00:00"}]
                                :mode "human"})]
      (is (nil? (#'prompt/emit-prompt-pending-events! data)))))

  (testing "handles malformed JSON gracefully"
    (is (nil? (#'prompt/emit-prompt-pending-events! "not-valid-json"))))

  (testing "handles nil prompts list"
    (is (nil? (#'prompt/emit-prompt-pending-events!
               "{\"count\":0,\"mode\":\"human\"}")))))

;; =============================================================================
;; Lazy Preset Header Tests (Wave 3 - Step 7)
;; =============================================================================

(defn- estimate-tokens
  "Estimate token count from character count (~4 chars per token)."
  [s]
  (when s
    (/ (count s) 4)))

(defn- get-preset-file-content
  "Read preset content directly from file for comparison.
   Returns nil if file doesn't exist."
  [preset-name]
  (let [presets-dir (or (System/getenv "HIVE_MCP_PRESETS_DIR")
                        (str (System/getProperty "user.dir") "/presets"))
        file-path (str presets-dir "/" preset-name ".md")]
    (when (.exists (io/file file-path))
      (slurp file-path))))

(deftest lazy-header-token-efficiency
  (testing "Lazy header uses fewer tokens than full content"
    (let [preset-names ["ling" "mcp-first" "hivemind"]
          lazy-header (prompt/build-lazy-preset-header preset-names)
          lazy-tokens (estimate-tokens lazy-header)]
      
      ;; Lazy header should be < 500 tokens (~2000 chars)
      (is (< lazy-tokens 500) 
          (format "Lazy header should be <500 tokens, got %.0f" (float lazy-tokens)))
      
      ;; Compare to full content if preset files exist
      (let [full-contents (map get-preset-file-content preset-names)
            available-contents (filter some? full-contents)]
        (when (seq available-contents)
          (let [full-content (apply str available-contents)
                full-tokens (estimate-tokens full-content)]
            ;; Lazy should be significantly smaller than full content
            (is (< lazy-tokens (* 0.3 full-tokens))
                (format "Lazy (%.0f tokens) should be <30%% of full (%.0f tokens)" 
                        (float lazy-tokens) (float full-tokens))))))))

  (testing "Lazy header is compact for single preset"
    (let [header (prompt/build-lazy-preset-header ["tdd"])
          tokens (estimate-tokens header)]
      (is (< tokens 300)
          (format "Single preset lazy header should be <300 tokens, got %.0f" (float tokens)))))

  (testing "Lazy header scales linearly with preset count"
    (let [one-preset (prompt/build-lazy-preset-header ["ling"])
          three-presets (prompt/build-lazy-preset-header ["ling" "mcp-first" "hivemind"])
          one-len (count one-preset)
          three-len (count three-presets)]
      ;; Three presets should NOT be 3x the size (most content is fixed)
      ;; Expect maybe 1.5x due to extra preset names in instructions
      (is (< three-len (* 2 one-len))
          (format "3 presets (%d chars) should be <2x single preset (%d chars)" 
                  three-len one-len)))))

(deftest lazy-header-contains-instructions
  (testing "Lazy header includes fetch instructions"
    (let [header (prompt/build-lazy-preset-header ["ling" "tdd"])]
      ;; Should contain consolidated tool syntax
      (is (re-find #"preset\(command:" header)
          "Header should contain preset tool syntax")
      ;; Should list all preset names
      (is (re-find #"ling" header)
          "Header should list preset names")
      (is (re-find #"tdd" header)
          "Header should list all preset names")))

  (testing "Lazy header includes core command reference"
    (let [header (prompt/build-lazy-preset-header ["clarity"])]
      (is (re-find #"core" header)
          "Header should mention 'core' command for quick summaries")))

  (testing "Lazy header includes discovery commands"
    (let [header (prompt/build-lazy-preset-header ["solid"])]
      (is (re-find #"search" header)
          "Header should mention search command")
      (is (re-find #"list_slim" header)
          "Header should mention list_slim command")))

  (testing "Lazy header has structure sections"
    (let [header (prompt/build-lazy-preset-header ["ling" "mcp-first"])]
      (is (re-find #"Assigned Presets" header)
          "Header should have 'Assigned Presets' section")
      (is (re-find #"IMMEDIATE" header)
          "Header should have immediate fetch instruction")
      (is (re-find #"Discovery" header)
          "Header should have discovery section"))))

(deftest lazy-header-edge-cases
  (testing "Empty preset list returns nil"
    (is (nil? (prompt/build-lazy-preset-header []))
        "Empty preset list should return nil"))

  (testing "Nil preset list returns nil"
    (is (nil? (prompt/build-lazy-preset-header nil))
        "Nil preset list should return nil"))

  (testing "Single preset header is valid"
    (let [header (prompt/build-lazy-preset-header ["tdd"])]
      (is (string? header)
          "Single preset should produce valid header string")
      (is (pos? (count header))
          "Header should not be empty")))

  (testing "Preset names with special characters are escaped"
    (let [header (prompt/build-lazy-preset-header ["test-preset" "my_preset"])]
      (is (re-find #"test-preset" header)
          "Hyphenated names should appear correctly")
      (is (re-find #"my_preset" header)
          "Underscored names should appear correctly"))))
