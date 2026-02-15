(ns hive-mcp.tools.result-bridge-property-test
  "Property tests for shared result-bridge helpers."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dns.result :as result]))

;; ── Generators ────────────────────────────────────────────────────────────────

(def gen-json-safe
  "Generate values that are safe for JSON serialization (no Characters, etc.)."
  (gen/one-of [gen/string-alphanumeric
               gen/small-integer
               gen/boolean
               (gen/return nil)
               (gen/vector gen/string-alphanumeric 0 3)
               (gen/map gen/string-alphanumeric gen/string-alphanumeric {:max-elements 3})]))

(def gen-ok-result
  (gen/fmap result/ok gen-json-safe))

(def gen-err-result
  (gen/let [cat (gen/fmap keyword gen/string-alphanumeric)
            msg gen/string-alphanumeric]
    (result/err cat {:message msg})))

(def gen-any-result
  (gen/one-of [gen-ok-result gen-err-result]))

(def gen-string-map
  "Map with string keys (simulates MCP JSON params)."
  (gen/map gen/string-alphanumeric gen/any-printable-equatable {:max-elements 10}))

(def gen-keyword-map
  "Map with keyword keys."
  (gen/map gen/keyword gen/any-printable-equatable {:max-elements 10}))

(def gen-mixed-map
  (gen/one-of [gen-string-map gen-keyword-map]))

;; ── P1: result->mcp totality ─────────────────────────────────────────────────

(defspec result->mcp-never-throws 200
  (prop/for-all [r gen-any-result]
                (let [mcp (rb/result->mcp r)]
                  (map? mcp))))

;; ── P2: ok Results -> {:type "text"} response (no :isError) ──────────────────

(defspec ok-result-produces-text-response 200
  (prop/for-all [v gen-json-safe]
                (let [mcp (rb/result->mcp (result/ok v))]
                  (and (= "text" (:type mcp))
                       (not (:isError mcp))))))

;; ── P3: err Results -> {:isError true} response ──────────────────────────────

(defspec err-result-produces-error-response 200
  (prop/for-all [msg gen/string-alphanumeric]
                (let [mcp (rb/result->mcp (result/err :test/error {:message msg}))]
                  (and (:isError mcp)
                       (string? (:text mcp))))))

;; ── P4: try-result totality ──────────────────────────────────────────────────

(defspec try-result-always-returns-result 200
  (prop/for-all [throw? gen/boolean
                 msg gen/string-alphanumeric]
                (let [r (rb/try-result :test/cat
                                       (if throw?
                                         #(throw (ex-info msg {}))
                                         #(result/ok msg)))]
                  (or (result/ok? r) (result/err? r)))))

;; ── P5: keywordize-map idempotent ────────────────────────────────────────────

(defspec keywordize-map-idempotent 200
  (prop/for-all [m gen-mixed-map]
                (let [once (rb/keywordize-map m)
                      twice (rb/keywordize-map once)]
                  (= once twice))))

;; ── P6: keywordize-map preserves map size ────────────────────────────────────

(defspec keywordize-map-preserves-size 200
  (prop/for-all [m gen-string-map]
                (= (count m) (count (rb/keywordize-map m)))))

;; ── P7: result->mcp-text totality ───────────────────────────────────────────

(defspec result->mcp-text-never-throws 200
  (prop/for-all [r gen-any-result]
                (let [mcp (rb/result->mcp-text r)]
                  (and (map? mcp)
                       (string? (:text mcp))))))

;; ── Unit tests ───────────────────────────────────────────────────────────────

(deftest test-try-result-catches-ex-info
  (testing "try-result catches ExceptionInfo and returns err with data"
    (let [r (rb/try-result :test/ex-info
                           #(throw (ex-info "boom" {:detail 42})))]
      (is (result/err? r))
      (is (= :test/ex-info (:error r)))
      (is (= "boom" (:message r)))
      (is (= {:detail 42} (:data r))))))

(deftest test-try-result-catches-generic-exception
  (testing "try-result catches generic Exception"
    (let [r (rb/try-result :test/generic
                           #(throw (NullPointerException. "npe")))]
      (is (result/err? r))
      (is (= "npe" (:message r)))
      (is (string? (:class r))))))

(deftest test-keywordize-map-empty
  (testing "keywordize-map on empty map returns empty map"
    (is (= {} (rb/keywordize-map {})))))

(deftest test-keywordize-map-mixed-keys
  (testing "keywordize-map handles already-keyword keys"
    (is (= {:a 1 :b 2} (rb/keywordize-map {:a 1 "b" 2})))))
