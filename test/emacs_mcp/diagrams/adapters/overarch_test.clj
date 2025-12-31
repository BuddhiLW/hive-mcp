(ns emacs-mcp.diagrams.adapters.overarch-test
  "Tests for Overarch adapter.
   
   Bug Investigation (RESOLVED): View rendering fails with 'Cannot invoke 
   Object.toString() because s is null'
   
   Root Cause Analysis:
   ====================
   
   Issue Category: ADAPTER FORMAT MISMATCH (our bug, not upstream)
   
   Problems identified and fixed:
   1. Model used vector [] - now uses set #{}
   2. Views used vector [] - now uses set #{}
   3. Relations missing :id - now auto-generated from from/to
   4. Keywords not namespaced - now all IDs use 'diagram/' namespace
   
   Overarch requires namespaced keywords for all element and relation IDs,
   e.g. :diagram/user not just :user. This is because it organizes output
   by namespace."
  (:require [clojure.test :refer [deftest testing is are]]
            [emacs-mcp.diagrams.adapters.overarch :as overarch]
            [emacs-mcp.diagrams.core :as diagrams]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;;; Test Data

(def sample-c4-spec
  "A minimal C4 context diagram spec."
  {:type :c4-context
   :title "Test System Context"
   :elements [{:id :user :name "User" :el :person :desc "A user"}
              {:id :system :name "System" :el :system :desc "The system"}]
   :relations [{:from :user :to :system :name "uses"}]})

;;; Bug Demonstration Tests (RED - these should fail with current implementation)

(deftest test-model-format-uses-set
  (testing "BUG: Model should be a set, not a vector"
    (let [model (#'overarch/spec->overarch-model sample-c4-spec)]
      ;; Current behavior: returns a map with :elements as vector
      ;; Expected: should return EDN that serializes to a set
      (is (set? (:elements model))
          "Model elements should be a set #{} not a vector []"))))

(deftest test-view-format-uses-ct-refs
  (testing "View should use :ct with refs to all elements and relations"
    (let [views (#'overarch/spec->overarch-views sample-c4-spec)
          view (first views)]
      (is (contains? view :ct)
          "View should have :ct key")
      (is (vector? (:ct view))
          "View :ct should be a vector of refs")
      (is (every? #(contains? % :ref) (:ct view))
          "Each :ct entry should have :ref"))))

(deftest test-relation-has-id
  (testing "Relations should have :id for view refs to work"
    (let [model (#'overarch/spec->overarch-model sample-c4-spec)
          relations (filter #(= :rel (:el %)) (:elements model))]
      (is (every? :id relations)
          "Every relation should have :id"))))

;;; Expected Format Tests (what the fix should produce)

(deftest test-overarch-model-edn-format
  (testing "Model EDN should match Overarch's expected format"
    (let [model (#'overarch/spec->overarch-model sample-c4-spec)
          ;; The elements (what gets written to model.edn) should be a set
          elements-edn (pr-str (:elements model))]
      (is (clojure.string/starts-with? elements-edn "#{")
          "Model elements should serialize as set #{}"))))

(deftest test-overarch-views-edn-format
  (testing "Views EDN should match Overarch's expected format"
    (let [views (#'overarch/spec->overarch-views sample-c4-spec)
          views-edn (pr-str views)]
      (is (clojure.string/includes? views-edn ":ct")
          "Views EDN should contain :ct key")
      (is (clojure.string/includes? views-edn ":ref")
          "Views EDN should contain :ref entries"))))

;;; Integration Test (demonstrates the actual failure)

(deftest test-overarch-render-does-not-throw
  (testing "Rendering should not throw null pointer exception"
    (let [adapter (overarch/create-adapter)]
      (diagrams/register-adapter! adapter)
      (let [result (diagrams/render adapter sample-c4-spec :plantuml)]
        ;; Currently fails with "Cannot invoke Object.toString() because s is null"
        (is (:success? result)
            "Render should succeed")
        (is (some? (:output result))
            "Should produce output files")
        (is (empty? (:errors result))
            (str "Should have no errors, got: " (:errors result)))))))

;;; Reference: Correct Overarch Format Examples

(def expected-model-format
  "What the model.edn should look like (from Overarch docs)."
  '#{{:el :person
      :id :user
      :name "User"
      :desc "A user"}
     {:el :system
      :id :system
      :name "System"
      :desc "The system"}
     {:el :rel
      :from :user
      :to :system
      :name "uses"}})

(def expected-view-format
  "What the views.edn should look like (from Overarch docs)."
  '#{{:el :context-view
      :id :test-context-view
      :title "Test System Context"
      :ct [{:ref :user}
           {:ref :system}
           {:ref :user-to-system}]}})

(deftest test-format-documentation
  (testing "Document the expected formats for reference"
    (is (set? expected-model-format)
        "Model should be a set")
    (is (set? expected-view-format)
        "Views should be a set")
    (is (every? #(contains? % :el) expected-model-format)
        "Every model element has :el")
    (let [view (first expected-view-format)]
      (is (contains? view :ct)
          "View has :ct")
      (is (every? :ref (:ct view))
          "Every :ct entry has :ref"))))
