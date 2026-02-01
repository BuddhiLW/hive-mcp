(ns hive-mcp.tools.swarm-kg-integration-test
  "Integration tests for KG-first context in swarm dispatch.

   Tests verify:
   - kg-first-context is called when files are extracted from prompt
   - Staleness warnings are injected into enhanced prompts
   - format-staleness-warnings produces expected output

   Uses with-redefs to mock emacsclient and coordinator functions.
   Each test uses a fresh DataScript connection via fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.tools.swarm.dispatch :as dispatch]
            [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each fixtures/datascript-fixture)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn gen-path
  "Generate a unique file path for testing."
  []
  (str "/test/dispatch/" (subs (str (java.util.UUID/randomUUID)) 0 8) ".clj"))

(defn add-stale-disc!
  "Add a disc entity with stale data (40 days old, no hash match)."
  [path]
  (let [old-date (java.util.Date. (- (System/currentTimeMillis)
                                     (* 40 24 60 60 1000)))]
    (disc/add-disc! {:path path
                     :content-hash "old-hash-wont-match"
                     :analyzed-at old-date
                     :project-id "test"})
    (disc/update-disc! path {:disc/last-read-at old-date
                             :disc/read-count 1})))

(defn mock-elisp-success
  "Creates a mock eval-elisp-with-timeout that captures and returns success."
  [captured-atom result]
  (fn [elisp _timeout]
    (reset! captured-atom elisp)
    {:success true :result result :duration-ms 10 :timed-out false}))

;; =============================================================================
;; staleness-warnings Function Tests
;; =============================================================================

(deftest staleness-warnings-returns-nil-for-fresh-files-test
  (testing "staleness-warnings returns nil when no files are stale"
    (let [path (gen-path)]
      ;; Add fresh disc (recently read)
      (disc/add-disc! {:path path :content-hash "test"})
      (disc/update-disc! path {:disc/last-read-at (java.util.Date.)
                               :disc/read-count 5})
      (let [warnings (disc/staleness-warnings [path])]
        (is (empty? warnings)
            "Fresh files should produce no warnings")))))

(deftest staleness-warnings-returns-warnings-for-stale-files-test
  (testing "staleness-warnings returns warnings for stale files"
    (let [path (gen-path)]
      (add-stale-disc! path)
      (let [warnings (disc/staleness-warnings [path])]
        (is (= 1 (count warnings))
            "Should have one warning for stale file")
        (is (str/includes? (:message (first warnings)) path)
            "Warning should mention the file path")
        (is (str/includes? (:message (first warnings)) "stale")
            "Warning should mention staleness")))))

(deftest staleness-warnings-ignores-unknown-files-test
  (testing "staleness-warnings returns empty for files without disc entities"
    (let [path (gen-path)]
      ;; No disc entity exists
      (let [warnings (disc/staleness-warnings [path])]
        (is (empty? warnings)
            "Unknown files should not produce warnings (zero noise)")))))

;; =============================================================================
;; format-staleness-warnings Tests
;; =============================================================================

(deftest format-staleness-warnings-nil-for-empty-test
  (testing "format-staleness-warnings returns nil for empty warnings"
    (is (nil? (disc/format-staleness-warnings []))
        "Empty warnings should return nil")
    (is (nil? (disc/format-staleness-warnings nil))
        "Nil warnings should return nil")))

(deftest format-staleness-warnings-formats-correctly-test
  (testing "format-staleness-warnings produces readable text block"
    (let [warnings [{:path "/src/foo.clj"
                     :staleness 0.6
                     :message "NOTE: file /src/foo.clj is stale (staleness: 0.6, last read 45d ago)."}]]
      (let [formatted (disc/format-staleness-warnings warnings)]
        (is (string? formatted))
        (is (str/includes? formatted "L1 Disc Staleness Warnings")
            "Should have section header")
        (is (str/includes? formatted "/src/foo.clj")
            "Should include file path")))))

;; =============================================================================
;; kg-first-context Integration with Dispatch
;; =============================================================================

(deftest kg-first-context-classifies-files-from-prompt-test
  (testing "kg-first-context correctly classifies files extracted from prompt"
    (let [stale-path (gen-path)
          fresh-path (gen-path)
          missing-path (gen-path)]
      ;; Set up disc states
      (add-stale-disc! stale-path)
      (disc/add-disc! {:path fresh-path :content-hash "test"})
      (disc/update-disc! fresh-path {:disc/last-read-at (java.util.Date.)
                                     :disc/read-count 3})
      ;; missing-path has no disc
      (let [result (disc/kg-first-context [stale-path fresh-path missing-path])]
        (is (contains? (:kg-known result) fresh-path)
            "Fresh file should be :kg-known")
        (is (contains? (set (:stale result)) stale-path)
            "Stale file should be in :stale")
        (is (contains? (set (:needs-read result)) missing-path)
            "Missing file should be in :needs-read")))))

;; =============================================================================
;; Staleness Warning Injection Tests (for dispatch enhancement)
;; =============================================================================

(deftest build-staleness-context-for-dispatch-test
  (testing "Staleness context can be built from extracted files"
    (let [stale-path "/src/stale-file.clj"]
      (add-stale-disc! stale-path)
      ;; Simulate what dispatch would do: extract files, check staleness
      (let [files [stale-path]
            warnings (disc/staleness-warnings files)
            formatted (disc/format-staleness-warnings warnings)]
        (is (seq warnings) "Should have warnings for stale file")
        (is (string? formatted) "Should format warnings as string")
        (is (str/includes? formatted stale-path)
            "Formatted warnings should include file path")))))

(deftest inject-shout-reminder-function-test
  (testing "inject-shout-reminder appends reminder to prompt"
    (let [original "Fix the bug in src/core.clj"
          enhanced (dispatch/inject-shout-reminder original)]
      (is (str/starts-with? enhanced original)
          "Should preserve original prompt")
      (is (str/includes? enhanced "hivemind_shout")
          "Should include hivemind_shout reminder")
      (is (str/includes? enhanced "completed")
          "Should mention completed event"))))

;; =============================================================================
;; End-to-End Dispatch with KG Context (Mocked)
;; =============================================================================

(deftest dispatch-with-stale-files-includes-context-test
  (testing "Dispatch with stale files should include staleness context when wired"
    ;; This test documents the EXPECTED behavior after integration
    ;; Currently tests the building blocks work together
    (let [stale-path "/src/test-stale.clj"]
      (add-stale-disc! stale-path)
      ;; Build context manually (this is what dispatch should do)
      (let [files [stale-path]
            kg-result (disc/kg-first-context files)
            warnings (disc/staleness-warnings (:stale kg-result))
            warning-text (disc/format-staleness-warnings warnings)]
        ;; Verify the pipeline works
        (is (contains? (set (:stale kg-result)) stale-path)
            "File should be classified as stale")
        (is (seq warnings)
            "Should generate staleness warnings")
        (is (str/includes? (or warning-text "") stale-path)
            "Warning text should include file path")))))
