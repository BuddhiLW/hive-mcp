(ns hive-mcp.schema.elisp-test
  "Tests for hive-mcp.schema.elisp namespace.

   Tests cover:
   - emit-struct: Basic struct generation from Malli :map schemas
   - emit-struct-with-defaults: Struct generation with default values
   - emit-structs: Batch struct generation
   - Error handling for invalid inputs"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.schema.elisp :as se]))

;; =============================================================================
;; Test emit-struct - Basic Usage
;; =============================================================================

(deftest test-emit-struct-basic
  (testing "Basic struct with simple fields"
    (let [schema [:map [:id :string] [:name :string]]
          result (se/emit-struct schema {:name "hive-item"})]
      ;; Should have cl-defstruct wrapper
      (is (str/starts-with? result "(cl-defstruct"))
      ;; Should have struct name
      (is (str/includes? result "hive-item"))
      ;; Should have default constructor
      (is (str/includes? result "(:constructor hive-item-create)"))
      ;; Should have fields
      (is (str/includes? result "id"))
      (is (str/includes? result "name")))))

(deftest test-emit-struct-memory-entry-example
  (testing "MemoryEntry schema from task specification"
    (let [schema [:map
                  [:id :string]
                  [:type [:enum :note :snippet :decision]]
                  [:content :string]
                  [:tags [:vector :string]]]
          result (se/emit-struct schema {:name "hive-memory-entry"})]
      ;; Should produce expected output format
      (is (str/starts-with? result "(cl-defstruct (hive-memory-entry"))
      (is (str/includes? result "(:constructor hive-memory-entry-create)"))
      (is (str/includes? result "id type content tags")))))

(deftest test-emit-struct-custom-constructor
  (testing "Custom constructor name"
    (let [schema [:map [:id :string]]
          result (se/emit-struct schema {:name "hive-node"
                                         :constructor "make-hive-node"})]
      (is (str/includes? result "(:constructor make-hive-node)")))))

(deftest test-emit-struct-with-include
  (testing "Struct inheritance with :include"
    (let [schema [:map [:extra :string]]
          result (se/emit-struct schema {:name "hive-extended"
                                         :include "hive-base"})]
      (is (str/includes? result "(:include hive-base)")))))

(deftest test-emit-struct-kebab-case-fields
  (testing "Kebab-case field names are preserved"
    (let [schema [:map [:first-name :string] [:last-name :string]]
          result (se/emit-struct schema {:name "hive-person"})]
      (is (str/includes? result "first-name"))
      (is (str/includes? result "last-name")))))

(deftest test-emit-struct-optional-fields
  (testing "Optional fields are still included (validator handles optionality)"
    (let [schema [:map
                  [:id :string]
                  [:metadata {:optional true} :map]]
          result (se/emit-struct schema {:name "hive-record"})]
      ;; Both fields should be present
      (is (str/includes? result "id"))
      (is (str/includes? result "metadata")))))

;; =============================================================================
;; Test emit-struct - Error Handling
;; =============================================================================

(deftest test-emit-struct-missing-name
  (testing "Throws when :name option is missing"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"emit-struct requires :name option"
         (se/emit-struct [:map [:id :string]] {})))))

(deftest test-emit-struct-non-map-schema
  (testing "Throws when schema is not a :map"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"emit-struct requires a :map schema"
         (se/emit-struct [:vector :string] {:name "bad"})))))

(deftest test-emit-struct-enum-schema
  (testing "Throws for :enum schema (not :map)"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"emit-struct requires a :map schema"
         (se/emit-struct [:enum :a :b :c] {:name "bad"})))))

;; =============================================================================
;; Test emit-struct-with-defaults
;; =============================================================================

(deftest test-emit-struct-with-defaults-basic
  (testing "Struct with default values"
    (let [schema [:map [:id :string] [:enabled :boolean]]
          result (se/emit-struct-with-defaults schema {:name "hive-config"
                                                       :defaults {:enabled true}})]
      ;; Should have default value syntax
      (is (str/includes? result "(enabled t)"))
      ;; id should have no default
      (is (str/includes? result "id ")))))

(deftest test-emit-struct-with-defaults-nil
  (testing "nil default value"
    (let [schema [:map [:data :any]]
          result (se/emit-struct-with-defaults schema {:name "hive-wrapper"
                                                       :defaults {:data nil}})]
      (is (str/includes? result "(data nil)")))))

(deftest test-emit-struct-with-defaults-string
  (testing "String default value"
    (let [schema [:map [:status :string]]
          result (se/emit-struct-with-defaults schema {:name "hive-task"
                                                       :defaults {:status "pending"}})]
      (is (str/includes? result "(status \"pending\")")))))

(deftest test-emit-struct-with-defaults-number
  (testing "Number default value"
    (let [schema [:map [:count :int]]
          result (se/emit-struct-with-defaults schema {:name "hive-counter"
                                                       :defaults {:count 0}})]
      (is (str/includes? result "(count 0)")))))

(deftest test-emit-struct-with-defaults-false
  (testing "false default (becomes nil in elisp)"
    (let [schema [:map [:active :boolean]]
          result (se/emit-struct-with-defaults schema {:name "hive-flag"
                                                       :defaults {:active false}})]
      (is (str/includes? result "(active nil)")))))

(deftest test-emit-struct-with-defaults-no-defaults
  (testing "Works without any defaults specified"
    (let [schema [:map [:id :string]]
          result (se/emit-struct-with-defaults schema {:name "hive-simple"})]
      ;; Should just have field name without parens
      (is (str/includes? result "\n  id)")))))

;; =============================================================================
;; Test emit-structs - Batch Generation
;; =============================================================================

(deftest test-emit-structs-multiple
  (testing "Multiple struct generation"
    (let [pairs [[[:map [:id :string]] {:name "hive-item"}]
                 [[:map [:name :string] [:value :int]] {:name "hive-pair"}]]
          result (se/emit-structs pairs)]
      ;; Should contain both structs
      (is (str/includes? result "hive-item"))
      (is (str/includes? result "hive-pair"))
      ;; Should be separated by blank lines
      (is (str/includes? result "\n\n")))))

(deftest test-emit-structs-empty
  (testing "Empty input returns empty string"
    (let [result (se/emit-structs [])]
      (is (= "" result)))))

(deftest test-emit-structs-single
  (testing "Single struct in batch"
    (let [result (se/emit-structs [[[:map [:x :int]] {:name "hive-point"}]])]
      (is (str/starts-with? result "(cl-defstruct"))
      (is (str/includes? result "hive-point")))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest test-complex-schema
  (testing "Complex nested schema types (types ignored, just field names)"
    (let [schema [:map
                  [:id :string]
                  [:created-at :int]  ;; timestamp as int
                  [:tags [:vector :string]]
                  [:metadata [:map
                              [:source :string]
                              [:version :int]]]
                  [:type [:enum :a :b :c]]]
          result (se/emit-struct schema {:name "hive-document"})]
      ;; All field names should be present
      (is (str/includes? result "id"))
      (is (str/includes? result "created-at"))
      (is (str/includes? result "tags"))
      (is (str/includes? result "metadata"))
      (is (str/includes? result "type")))))

(deftest test-real-world-memory-entry
  (testing "Real-world memory entry schema"
    (let [schema [:map
                  [:id :string]
                  [:type [:enum :note :snippet :decision :convention]]
                  [:content :string]
                  [:tags [:vector :string]]
                  [:project {:optional true} :string]
                  [:duration {:optional true} [:enum :short :medium :long :permanent]]
                  [:created-at :int]  ;; timestamp as int
                  [:updated-at {:optional true} :int]]  ;; timestamp as int
          result (se/emit-struct schema {:name "hive-memory-entry"})]
      ;; Should generate valid elisp structure
      (is (str/starts-with? result "(cl-defstruct"))
      (is (str/ends-with? (str/trim result) ")"))
      ;; All 8 fields present
      (is (str/includes? result "id type content tags project duration created-at updated-at")))))

;; =============================================================================
;; Test emit-validator - W1.2
;; =============================================================================

(deftest test-emit-validator-basic
  (testing "Basic validator with string fields"
    (let [schema [:map [:id :string] [:name :string]]
          result (se/emit-validator schema {:name "hive-item"})]
      ;; Should have defun wrapper
      (is (str/starts-with? result "(defun"))
      ;; Should have correct function name
      (is (str/includes? result "hive-item-valid-p"))
      ;; Should check struct type first
      (is (str/includes? result "(hive-item-p entry)"))
      ;; Should have stringp checks
      (is (str/includes? result "(stringp (hive-item-id entry))"))
      (is (str/includes? result "(stringp (hive-item-name entry))")))))

(deftest test-emit-validator-enum
  (testing "Validator with enum field"
    (let [schema [:map
                  [:id :string]
                  [:type [:enum :note :snippet :decision]]]
          result (se/emit-validator schema {:name "hive-entry"})]
      ;; Should have memq for enum
      (is (str/includes? result "(memq (hive-entry-type entry) '(note snippet decision))")))))

(deftest test-emit-validator-vector
  (testing "Validator with vector field"
    (let [schema [:map [:tags [:vector :string]]]
          result (se/emit-validator schema {:name "hive-tagged"})]
      ;; Should have listp for vector
      (is (str/includes? result "(listp (hive-tagged-tags entry))")))))

(deftest test-emit-validator-maybe
  (testing "Validator with maybe (optional value) field"
    (let [schema [:map [:metadata [:maybe :string]]]
          result (se/emit-validator schema {:name "hive-optional"})]
      ;; Should have null-or check
      (is (str/includes? result "(or (null (hive-optional-metadata entry))")))))

(deftest test-emit-validator-integer
  (testing "Validator with integer field"
    (let [schema [:map [:count :int]]
          result (se/emit-validator schema {:name "hive-counter"})]
      (is (str/includes? result "(integerp (hive-counter-count entry))")))))

(deftest test-emit-validator-boolean
  (testing "Validator with boolean field"
    (let [schema [:map [:active :boolean]]
          result (se/emit-validator schema {:name "hive-flag"})]
      (is (str/includes? result "(booleanp (hive-flag-active entry))")))))

(deftest test-emit-validator-custom-var
  (testing "Custom variable name"
    (let [schema [:map [:id :string]]
          result (se/emit-validator schema {:name "hive-item" :var "obj"})]
      ;; Should use custom var name
      (is (str/includes? result "(defun hive-item-valid-p (OBJ)"))
      (is (str/includes? result "(hive-item-p obj)")))))

(deftest test-emit-validator-memory-entry
  (testing "Full MemoryEntry example from task spec"
    (let [schema [:map
                  [:id :string]
                  [:type [:enum :note :snippet :decision]]
                  [:content :string]]
          result (se/emit-validator schema {:name "hive-memory-entry"})]
      ;; Matches expected pattern from task spec
      (is (str/includes? result "(defun hive-memory-entry-valid-p (ENTRY)"))
      (is (str/includes? result "(and (hive-memory-entry-p entry)"))
      (is (str/includes? result "(stringp (hive-memory-entry-id entry))"))
      (is (str/includes? result "(memq (hive-memory-entry-type entry) '(note snippet decision))"))
      (is (str/includes? result "(stringp (hive-memory-entry-content entry))")))))

(deftest test-emit-validator-missing-name
  (testing "Throws when :name option is missing"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"emit-validator requires :name option"
         (se/emit-validator [:map [:id :string]] {})))))

(deftest test-emit-validator-non-map-schema
  (testing "Throws when schema is not a :map"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"emit-validator requires a :map schema"
         (se/emit-validator [:vector :string] {:name "bad"})))))

(deftest test-emit-validators-batch
  (testing "Multiple validator generation"
    (let [pairs [[[:map [:id :string]] {:name "hive-a"}]
                 [[:map [:name :string]] {:name "hive-b"}]]
          result (se/emit-validators pairs)]
      (is (str/includes? result "hive-a-valid-p"))
      (is (str/includes? result "hive-b-valid-p")))))

;; =============================================================================
;; Test emit-from-plist - W1.3
;; =============================================================================

(deftest test-emit-from-plist-basic
  (testing "Basic plist converter with string fields"
    (let [schema [:map [:id :string] [:name :string]]
          result (se/emit-from-plist schema {:name "hive-item"})]
      ;; Should have defun wrapper
      (is (str/starts-with? result "(defun"))
      ;; Should have correct function name
      (is (str/includes? result "hive-item-from-plist"))
      ;; Should call create function
      (is (str/includes? result "(hive-item-create"))
      ;; Should have plist-get for each field
      (is (str/includes? result ":id (plist-get plist :id)"))
      (is (str/includes? result ":name (plist-get plist :name)")))))

(deftest test-emit-from-plist-enum
  (testing "Plist converter with enum field (needs intern)"
    (let [schema [:map [:type [:enum :note :snippet :decision]]]
          result (se/emit-from-plist schema {:name "hive-entry"})]
      ;; Enum should be wrapped with intern
      (is (str/includes? result "(intern (plist-get plist :type))")))))

(deftest test-emit-from-plist-optional
  (testing "Plist converter with optional field"
    (let [schema [:map
                  [:id :string]
                  [:metadata {:optional true} :string]]
          result (se/emit-from-plist schema {:name "hive-record"})]
      ;; Optional field should have (or ... nil) wrapper
      (is (str/includes? result "(or (plist-get plist :metadata) nil)")))))

(deftest test-emit-from-plist-memory-entry
  (testing "Full MemoryEntry example from task spec"
    (let [schema [:map
                  [:id :string]
                  [:type [:enum :note :snippet :decision]]
                  [:content :string]
                  [:tags [:vector :string]]]
          result (se/emit-from-plist schema {:name "hive-memory-entry"})]
      ;; Matches expected pattern from task spec
      (is (str/includes? result "(defun hive-memory-entry-from-plist (plist)"))
      (is (str/includes? result "\"Create hive-memory-entry from PLIST.\""))
      (is (str/includes? result "(hive-memory-entry-create"))
      (is (str/includes? result ":id (plist-get plist :id)"))
      (is (str/includes? result ":type (intern (plist-get plist :type))"))
      (is (str/includes? result ":content (plist-get plist :content)"))
      (is (str/includes? result ":tags (plist-get plist :tags)")))))

(deftest test-emit-from-plist-missing-name
  (testing "Throws when :name option is missing"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"emit-from-plist requires :name option"
         (se/emit-from-plist [:map [:id :string]] {})))))

(deftest test-emit-from-plist-non-map-schema
  (testing "Throws when schema is not a :map"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"emit-from-plist requires a :map schema"
         (se/emit-from-plist [:vector :string] {:name "bad"})))))

(deftest test-emit-from-plist-all-batch
  (testing "Multiple from-plist function generation"
    (let [pairs [[[:map [:id :string]] {:name "hive-a"}]
                 [[:map [:name :string]] {:name "hive-b"}]]
          result (se/emit-from-plist-all pairs)]
      (is (str/includes? result "hive-a-from-plist"))
      (is (str/includes? result "hive-b-from-plist")))))

;; =============================================================================
;; Integration Test - Full Schema Pipeline
;; =============================================================================

(deftest test-full-pipeline-memory-entry
  (testing "Complete code generation for MemoryEntry schema"
    (let [schema [:map
                  [:id :string]
                  [:type [:enum :note :snippet :decision :axiom]]
                  [:content :string]
                  [:tags [:vector :string]]
                  [:created-at :int]]
          opts {:name "hive-memory-entry"}
          struct-code (se/emit-struct schema opts)
          validator-code (se/emit-validator schema opts)
          from-plist-code (se/emit-from-plist schema opts)]
      ;; All three outputs should be valid
      (is (str/starts-with? struct-code "(cl-defstruct"))
      (is (str/starts-with? validator-code "(defun"))
      (is (str/starts-with? from-plist-code "(defun"))
      ;; Each should reference the same struct name
      (is (str/includes? struct-code "hive-memory-entry"))
      (is (str/includes? validator-code "hive-memory-entry"))
      (is (str/includes? from-plist-code "hive-memory-entry")))))
