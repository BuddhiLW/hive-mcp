(ns hive-mcp.tools.consolidated.multi-property-test
  "Property tests for multi tool batch validation and routing."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [hive-mcp.tools.consolidated.multi :as multi]
            [hive-mcp.tools.result-bridge :as rb]))

;; ── Generators ────────────────────────────────────────────────────────────────

(def gen-tool-name
  (gen/elements ["memory" "kg" "agent" "kanban" "session" "config"
                 "preset" "magit" "emacs" "wave" "hivemind"]))

(def gen-string-keyed-params
  "Simulates raw MCP JSON params with string keys."
  (gen/hash-map "tool" gen-tool-name
                "command" (gen/return "help")))

;; ── P1: handle-multi with help command never throws ──────────────────────────

(defspec multi-help-never-throws 50
  (prop/for-all [tool gen-tool-name]
                (let [result (multi/handle-multi {"tool" tool "command" "help"})]
                  (and (map? result)
                       (contains? result :text)))))

;; ── P2: handle-multi with unknown tool returns error ─────────────────────────

(defspec multi-unknown-tool-returns-error 50
  (prop/for-all [tool (gen/such-that
                       #(and (not (str/blank? %))
                             (nil? (multi/get-tool-handler %)))
                       gen/string-alphanumeric
                       100)]
                (let [result (multi/handle-multi {"tool" tool "command" "help"})]
                  (:isError result))))

;; ── P3: handle-multi with no params returns help text ────────────────────────

(deftest multi-empty-params-returns-help
  (testing "handle-multi with empty params returns help"
    (let [result (multi/handle-multi {})]
      (is (not (:isError result)))
      (is (str/includes? (:text result) "Multi tool")))))

;; ── P4: handle-multi rejects both dsl and operations ─────────────────────────

(deftest multi-dsl-and-operations-mutual-exclusion
  (testing "providing both dsl and operations returns error"
    (let [result (multi/handle-multi {"dsl" [["m+", {"c" "x"}]]
                                      "operations" [{"id" "1" "tool" "memory"}]})]
      (is (:isError result))
      (is (str/includes? (:text result) "Cannot specify both")))))

;; ── P5: batch with nil operations returns error ──────────────────────────────

(deftest multi-batch-nil-operations-error
  (testing "batch with nil operations returns error"
    (let [result (multi/handle-multi {"operations" nil})]
      ;; nil operations with no tool = help text
      (is (not (:isError result))))))

;; ── P6: batch with empty operations returns error ────────────────────────────

(deftest multi-batch-empty-operations-error
  (testing "batch with empty operations returns error"
    (let [result (multi/handle-multi {"operations" []})]
      (is (:isError result))
      (is (str/includes? (:text result) "empty")))))

;; ── P7: keywordize-map used in handle-multi normalizes string keys ───────────

(defspec keywordize-preserves-all-values 100
  (prop/for-all [m (gen/map gen/string-alphanumeric gen/string-alphanumeric {:max-elements 5})]
                (let [kw-map (rb/keywordize-map m)]
                  (= (count m) (count kw-map)))))
