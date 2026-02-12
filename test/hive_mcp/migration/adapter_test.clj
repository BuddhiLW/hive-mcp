(ns hive-mcp.migration.adapter-test
  "Unit tests for migration adapter protocol and implementations.

   Tests cover (isolated, no real backends):
   - IAdapter protocol compliance
   - IdentityAdapter (EDN passthrough)
   - JsonAdapter (keyword conversion)
   - Adapter registry operations
   - Transform helpers

   CRITICAL: Pure unit tests - no I/O, no backends."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.migration.adapter :as adapter]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-registry-fixture
  "Ensure adapters are registered for each test."
  [f]
  ;; Re-register built-in adapters (they're registered on ns load, but be safe)
  (adapter/register-adapter! (adapter/->IdentityAdapter))
  (adapter/register-adapter! (adapter/->JsonAdapter))
  (f))

(use-fixtures :each reset-registry-fixture)

;; =============================================================================
;; Identity Adapter Tests
;; =============================================================================

(deftest identity-adapter-id-test
  (testing "IdentityAdapter has correct id"
    (let [adpt (adapter/->IdentityAdapter)]
      (is (= :edn (adapter/adapter-id adpt))))))

(deftest identity-adapter-info-test
  (testing "IdentityAdapter returns correct info"
    (let [adpt (adapter/->IdentityAdapter)
          info (adapter/adapter-info adpt)]
      (is (= :edn (:id info)))
      (is (string? (:name info)))
      (is (string? (:description info)))
      (is (string? (:version info)))
      (is (contains? (:formats info) :edn)))))

(deftest identity-adapter-export-passthrough-test
  (testing "IdentityAdapter export returns data unchanged"
    (let [adpt (adapter/->IdentityAdapter)
          data {:edges [{:kg-edge/id "e1"
                         :kg-edge/from "a"
                         :kg-edge/to "b"
                         :kg-edge/relation :implements}]
                :counts {:edges 1}}
          result (adapter/export-transform adpt data {})]
      (is (= data result)))))

(deftest identity-adapter-import-passthrough-test
  (testing "IdentityAdapter import returns data unchanged"
    (let [adpt (adapter/->IdentityAdapter)
          data {:edges [{:id "e1"}] :disc [] :synthetic []}
          result (adapter/import-transform adpt data {})]
      (is (= data result)))))

(deftest identity-adapter-validate-map-test
  (testing "IdentityAdapter validates maps as valid"
    (let [adpt (adapter/->IdentityAdapter)
          result (adapter/validate-external adpt {:data "test"})]
      (is (:valid? result))
      (is (empty? (:errors result))))))

(deftest identity-adapter-validate-non-map-test
  (testing "IdentityAdapter rejects non-maps"
    (let [adpt (adapter/->IdentityAdapter)
          result (adapter/validate-external adpt [1 2 3])]
      (is (not (:valid? result)))
      (is (seq (:errors result))))))

;; =============================================================================
;; JSON Adapter Tests
;; =============================================================================

(deftest json-adapter-id-test
  (testing "JsonAdapter has correct id"
    (let [adpt (adapter/->JsonAdapter)]
      (is (= :json (adapter/adapter-id adpt))))))

(deftest json-adapter-info-test
  (testing "JsonAdapter returns correct info"
    (let [adpt (adapter/->JsonAdapter)
          info (adapter/adapter-info adpt)]
      (is (= :json (:id info)))
      (is (string? (:name info)))
      (is (contains? (:formats info) :json)))))

(deftest json-adapter-export-transforms-keywords-test
  (testing "JsonAdapter export transforms all keywords to strings"
    (let [adpt (adapter/->JsonAdapter)
          data {:relation :implements}
          result (adapter/export-transform adpt data {})]
      ;; postwalk converts both keys and values
      ;; Result should be a map (structure preserved)
      (is (map? result))
      ;; All keywords should be gone
      (is (not-any? keyword? (keys result)))
      (is (not-any? keyword? (vals result))))))

(deftest json-adapter-import-transforms-namespaced-strings-test
  (testing "JsonAdapter import converts namespaced strings to keywords"
    (let [adpt (adapter/->JsonAdapter)
          ;; Strings matching "\w+/\w+" pattern become keywords
          ;; Note: hyphens are NOT matched by \w, so use simple words
          data {"value" "backup/version"}
          result (adapter/import-transform adpt data {})]
      ;; "backup/version" matches pattern and becomes keyword
      (is (some keyword? (vals result)))
      (is (= :backup/version (get result "value"))))))

(deftest json-adapter-roundtrip-preserves-map-structure-test
  (testing "JsonAdapter export->import preserves map structure"
    (let [adpt (adapter/->JsonAdapter)
          original {:backup/version 1}
          exported (adapter/export-transform adpt original {})
          imported (adapter/import-transform adpt exported {})]
      ;; Structure preserved
      (is (map? imported))
      ;; Has one entry
      (is (= 1 (count imported)))
      ;; Value preserved
      (is (= 1 (first (vals imported)))))))

(deftest json-adapter-validate-map-test
  (testing "JsonAdapter validates maps as valid"
    (let [adpt (adapter/->JsonAdapter)
          result (adapter/validate-external adpt {"key" "value"})]
      (is (:valid? result)))))

(deftest json-adapter-validate-collection-test
  (testing "JsonAdapter validates collections as valid"
    (let [adpt (adapter/->JsonAdapter)
          result (adapter/validate-external adpt [{"a" 1} {"b" 2}])]
      (is (:valid? result)))))

(deftest json-adapter-validate-primitive-test
  (testing "JsonAdapter rejects primitives"
    (let [adpt (adapter/->JsonAdapter)
          result (adapter/validate-external adpt "just a string")]
      (is (not (:valid? result)))
      (is (seq (:errors result))))))

;; =============================================================================
;; Adapter Registry Tests
;; =============================================================================

(deftest get-adapter-returns-registered-test
  (testing "get-adapter returns registered adapters"
    (is (some? (adapter/get-adapter :edn)))
    (is (some? (adapter/get-adapter :json)))))

(deftest get-adapter-returns-nil-unknown-test
  (testing "get-adapter returns nil for unknown adapters"
    (is (nil? (adapter/get-adapter :nonexistent)))
    (is (nil? (adapter/get-adapter :logseq)))))

(deftest list-adapters-includes-builtin-test
  (testing "list-adapters includes built-in adapters"
    (let [adapters (adapter/list-adapters)
          ids (set (map :id adapters))]
      (is (contains? ids :edn))
      (is (contains? ids :json)))))

(deftest register-adapter-adds-to-registry-test
  (testing "register-adapter! adds adapter to registry"
    ;; Create a custom test adapter
    (let [test-adapter (reify adapter/IAdapter
                         (adapter-id [_] :test-custom)
                         (adapter-info [_] {:id :test-custom
                                            :name "Test"
                                            :description "Test adapter"
                                            :version "1.0.0"
                                            :formats #{:test}})
                         (export-transform [_ data _] data)
                         (import-transform [_ data _] data)
                         (validate-external [_ _data]
                           {:valid? true :errors []}))]
      (adapter/register-adapter! test-adapter)
      (is (some? (adapter/get-adapter :test-custom)))
      (is (= :test-custom (:id (adapter/adapter-info
                                (adapter/get-adapter :test-custom))))))))

;; =============================================================================
;; Transform Helper Tests
;; =============================================================================

(deftest transform-export-uses-adapter-test
  (testing "transform-export applies adapter transformation"
    (let [data {:key :value}
          result (adapter/transform-export :edn data)]
      (is (= data result)))))

(deftest transform-export-throws-unknown-adapter-test
  (testing "transform-export throws for unknown adapter"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unknown adapter"
                          (adapter/transform-export :nonexistent {})))))

(deftest transform-import-uses-adapter-test
  (testing "transform-import applies adapter transformation"
    (let [data {:key :value}
          result (adapter/transform-import :edn data)]
      (is (= data result)))))

(deftest transform-import-throws-unknown-adapter-test
  (testing "transform-import throws for unknown adapter"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unknown adapter"
                          (adapter/transform-import :nonexistent {})))))

;; =============================================================================
;; Edge Cases and Complex Data Tests
;; =============================================================================

(deftest json-adapter-handles-primitives-in-values-test
  (testing "JsonAdapter preserves primitive values"
    (let [adpt (adapter/->JsonAdapter)
          data {:number 42 :float 3.14 :bool true :nil-val nil}
          exported (adapter/export-transform adpt data {})]
      ;; Values are preserved (only keywords converted)
      (is (some #(= 42 %) (vals exported)))
      (is (some #(= 3.14 %) (vals exported)))
      (is (some true? (vals exported))))))

(deftest json-adapter-handles-vectors-test
  (testing "JsonAdapter preserves vector structure"
    (let [adpt (adapter/->JsonAdapter)
          data {:items [1 2 3]}
          exported (adapter/export-transform adpt data {})]
      ;; Should have a vector somewhere in values
      (is (some sequential? (vals exported)))
      (is (= [1 2 3] (first (filter sequential? (vals exported))))))))

(deftest json-adapter-handles-nested-maps-test
  (testing "JsonAdapter handles nested map structures"
    (let [adpt (adapter/->JsonAdapter)
          data {:outer {:inner 42}}
          exported (adapter/export-transform adpt data {})]
      ;; Nested map preserved
      (is (some map? (vals exported)))
      (is (= 42 (first (vals (first (filter map? (vals exported))))))))))

(deftest identity-adapter-preserves-all-types-test
  (testing "IdentityAdapter preserves all Clojure types"
    (let [adpt (adapter/->IdentityAdapter)
          data {:keyword :value
                :string "text"
                :number 42
                :float 3.14
                :boolean true
                :nil-val nil
                :vector [1 2 3]
                :set #{:a :b}
                :list '(1 2 3)
                :date (java.util.Date.)
                :uuid (random-uuid)}
          result (adapter/export-transform adpt data {})]
      (is (= data result)))))

;; =============================================================================
;; Adapter Protocol Compliance Tests
;; =============================================================================

(deftest identity-adapter-implements-all-protocol-methods-test
  (testing "IdentityAdapter implements all IAdapter methods"
    (let [adpt (adapter/->IdentityAdapter)]
      (is (keyword? (adapter/adapter-id adpt)))
      (is (map? (adapter/adapter-info adpt)))
      (is (some? (adapter/export-transform adpt {} {})))
      (is (some? (adapter/import-transform adpt {} {})))
      (is (map? (adapter/validate-external adpt {}))))))

(deftest json-adapter-implements-all-protocol-methods-test
  (testing "JsonAdapter implements all IAdapter methods"
    (let [adpt (adapter/->JsonAdapter)]
      (is (keyword? (adapter/adapter-id adpt)))
      (is (map? (adapter/adapter-info adpt)))
      (is (some? (adapter/export-transform adpt {} {})))
      (is (some? (adapter/import-transform adpt {} {})))
      (is (map? (adapter/validate-external adpt {}))))))
