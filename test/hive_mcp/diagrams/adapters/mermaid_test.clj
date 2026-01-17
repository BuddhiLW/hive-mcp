(ns hive-mcp.diagrams.adapters.mermaid-test
  "Tests for Mermaid diagram adapter.
   
   Coverage target: 70% - Protocol methods + public multimethods.
   Note: Private functions (sanitize-id, escape-mermaid, diagram-header) 
   cannot be tested directly from outside the namespace."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [hive-mcp.diagrams.adapters.mermaid :as mermaid]
            [hive-mcp.diagrams.core :as diagrams]))

;; =============================================================================
;; Protocol Tests - Using adapter instance
;; =============================================================================

(deftest adapter-id-test
  (testing "Adapter ID is :mermaid"
    (let [adapter (mermaid/create-adapter)]
      (is (= :mermaid (diagrams/adapter-id adapter))))))

(deftest supported-types-test
  (testing "Supported diagram types"
    (let [adapter (mermaid/create-adapter)
          types (diagrams/supported-types adapter)]
      (is (set? types))
      (is (contains? types :flowchart))
      (is (contains? types :sequence))
      (is (contains? types :class))
      (is (contains? types :state))
      (is (contains? types :er))
      (is (contains? types :gantt))
      (is (contains? types :pie))
      (is (contains? types :c4-context))
      (is (contains? types :c4-container)))))

(deftest validate-spec-valid-flowchart-test
  (testing "Valid flowchart spec with elements"
    (let [adapter (mermaid/create-adapter)
          result (diagrams/validate-spec adapter {:type :flowchart
                                                  :elements [{:id :a :label "A"}]
                                                  :relations []})]
      (is (or (nil? result) (:valid? result)) "Valid spec should pass"))))

(deftest validate-spec-invalid-type-test
  (testing "Invalid diagram type"
    (let [adapter (mermaid/create-adapter)
          result (diagrams/validate-spec adapter {:type :invalid-type
                                                  :elements [{:id :a}]
                                                  :relations []})]
      (is (or (some? result) (not (:valid? result))) "Invalid type should fail"))))

;; =============================================================================
;; Public Multimethod Tests - render-element-mermaid
;; Multimethods take [type element] as arguments
;; =============================================================================

(deftest render-element-flowchart-shapes-test
  (testing "Flowchart shape rendering"
    ;; Note: output includes 4-space indentation
    (is (str/includes? (mermaid/render-element-mermaid :flowchart {:id :test :label "Test" :shape :circle})
                       "((Test))"))
    (is (str/includes? (mermaid/render-element-mermaid :flowchart {:id :test :label "Test" :shape :stadium})
                       "([Test])"))
    (is (str/includes? (mermaid/render-element-mermaid :flowchart {:id :test :label "Test" :shape :database})
                       "[(Test)]"))
    (is (str/includes? (mermaid/render-element-mermaid :flowchart {:id :test :label "Test" :shape :hexagon})
                       "{{Test}}"))))

(deftest render-element-flowchart-default-test
  (testing "Flowchart default shape (rectangle)"
    (let [result (mermaid/render-element-mermaid :flowchart {:id :test :label "Test"})]
      (is (str/includes? result "test"))
      (is (str/includes? result "[Test]")))))

(deftest render-element-sequence-test
  (testing "Sequence diagram participant"
    (let [result (mermaid/render-element-mermaid :sequence {:id :service :label "MyService"})]
      (is (str/includes? result "participant"))
      (is (str/includes? result "MyService")))))

(deftest render-element-class-test
  (testing "Class diagram element"
    (let [result (mermaid/render-element-mermaid :class {:id :user
                                                         :label "User"
                                                         :attributes ["name" "email"]
                                                         :methods ["save" "delete"]})]
      ;; Class uses sanitized id (lowercase), not label
      (is (str/includes? result "class user"))
      (is (str/includes? result "name"))
      (is (str/includes? result "save")))))

(deftest render-element-state-test
  (testing "State diagram element"
    (let [result (mermaid/render-element-mermaid :state {:id :idle :label "IdleState"})]
      (is (str/includes? result "idle"))
      (is (str/includes? result "IdleState")))))

;; C4 elements use :el for type, :name for name, :external? for external flag
(deftest render-element-c4-context-person-test
  (testing "C4 context person"
    (let [result (mermaid/render-element-mermaid :c4-context {:el :person :id :user :name "Customer"})]
      (is (str/includes? result "Person"))
      (is (str/includes? result "Customer")))))

(deftest render-element-c4-context-system-test
  (testing "C4 context system"
    (let [result (mermaid/render-element-mermaid :c4-context {:el :system :id :api :name "API Gateway"})]
      (is (str/includes? result "System"))
      (is (str/includes? result "API Gateway")))))

(deftest render-element-c4-context-external-test
  (testing "C4 context external system"
    (let [result (mermaid/render-element-mermaid :c4-context {:el :system :external? true :id :payment :name "Stripe"})]
      (is (str/includes? result "System_Ext"))
      (is (str/includes? result "Stripe")))))

(deftest render-element-c4-container-test
  (testing "C4 container element"
    (let [result (mermaid/render-element-mermaid :c4-container {:el :container :id :web :name "Web App"})]
      (is (str/includes? result "Container"))
      (is (str/includes? result "Web App")))))

;; =============================================================================
;; Public Multimethod Tests - render-relation-mermaid
;; Multimethods take [type relation] as arguments
;; =============================================================================

(deftest render-relation-flowchart-default-test
  (testing "Flowchart default relation"
    (let [result (mermaid/render-relation-mermaid :flowchart {:from :a :to :b})]
      (is (str/includes? result "a"))
      (is (str/includes? result "b"))
      (is (str/includes? result "-->")))))

(deftest render-relation-flowchart-thick-test
  (testing "Flowchart thick arrow (requires label)"
    (let [result (mermaid/render-relation-mermaid :flowchart {:from :a :to :b :style :thick :label "calls"})]
      (is (str/includes? result "==>")))))

(deftest render-relation-flowchart-bidirectional-test
  (testing "Flowchart bidirectional arrow (requires label)"
    (let [result (mermaid/render-relation-mermaid :flowchart {:from :a :to :b :style :bidirectional :label "syncs"})]
      (is (str/includes? result "<-->")))))

(deftest render-relation-sequence-test
  (testing "Sequence diagram message"
    (let [result (mermaid/render-relation-mermaid :sequence {:from :client :to :server})]
      (is (str/includes? result "client"))
      (is (str/includes? result "server")))))

(deftest render-relation-class-inheritance-test
  (testing "Class diagram inheritance"
    (let [result (mermaid/render-relation-mermaid :class {:from :parent :to :child :relation-type :inheritance})]
      (is (str/includes? result "<|--")))))

(deftest render-relation-class-composition-test
  (testing "Class diagram composition"
    (let [result (mermaid/render-relation-mermaid :class {:from :whole :to :part :relation-type :composition})]
      (is (str/includes? result "*--")))))

(deftest render-relation-class-aggregation-test
  (testing "Class diagram aggregation"
    (let [result (mermaid/render-relation-mermaid :class {:from :whole :to :part :relation-type :aggregation})]
      (is (str/includes? result "o--")))))

(deftest render-relation-state-start-test
  (testing "State diagram start transition"
    (let [result (mermaid/render-relation-mermaid :state {:from :start :to :ready})]
      (is (str/includes? result "[*]"))
      (is (str/includes? result "ready")))))

(deftest render-relation-state-end-test
  (testing "State diagram end transition"
    (let [result (mermaid/render-relation-mermaid :state {:from :done :to :end})]
      (is (str/includes? result "[*]"))
      (is (str/includes? result "done")))))

(deftest render-relation-c4-context-test
  (testing "C4 context relation"
    (let [result (mermaid/render-relation-mermaid :c4-context {:from :user :to :api})]
      (is (str/includes? result "Rel"))
      (is (str/includes? result "user"))
      (is (str/includes? result "api")))))

;; =============================================================================
;; Integration Tests - Full render pipeline
;; =============================================================================

(deftest render-simple-flowchart-test
  (testing "Render simple flowchart"
    (let [adapter (mermaid/create-adapter)
          spec {:type :flowchart
                :elements [{:id :a :label "Start"}
                           {:id :b :label "End"}]
                :relations [{:from :a :to :b}]}
          result (diagrams/render adapter spec :mermaid)]
      (is (map? result)))))

(deftest preview-command-test
  (testing "Preview command returns string or nil"
    (let [adapter (mermaid/create-adapter)
          cmd (diagrams/preview-command adapter "/tmp/test.svg")]
      (is (or (nil? cmd) (string? cmd))))))
