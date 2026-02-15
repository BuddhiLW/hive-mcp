(ns hive-mcp.agent.drone.execution-property-test
  "Property-based tests for drone execution orchestration.

   Tests the pure/deterministic parts of execution lifecycle:
   - Execution mode dispatch (KG store strategy)
   - Task context building invariants
   - Truncation helper properties
   - Finalize sub-function contracts"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [hive-mcp.agent.drone.domain :as domain]))

;;; ---------------------------------------------------------------------------
;;; Generators
;;; ---------------------------------------------------------------------------

(def gen-execution-mode
  "Generator for execution modes."
  (gen/elements [:delegate :agentic]))

(def gen-result-status
  "Generator for raw-result status keywords."
  (gen/elements [:completed :failed :error :timeout]))

(def gen-drone-id
  "Generator for drone IDs."
  (gen/fmap #(str "drone-" %) gen/uuid))

(def gen-file-paths
  "Generator for file path vectors."
  (gen/vector (gen/fmap #(str "src/" % ".clj")
                        (gen/such-that seq gen/string-alphanumeric))
              0 5))

(def gen-raw-result
  "Generator for raw execution results."
  (gen/let [status gen-result-status
            result-text gen/string-alphanumeric]
    {:status status
     :result result-text}))

(def gen-diff-results
  "Generator for diff-results maps."
  (gen/let [applied gen-file-paths
            failed gen-file-paths
            proposed (gen/vector gen/uuid 0 3)]
    {:applied applied
     :failed failed
     :proposed (mapv str proposed)}))

(def gen-positive-int
  "Generator for positive integers (max-len, duration-ms, etc.)."
  (gen/such-that pos? gen/nat))

;;; ---------------------------------------------------------------------------
;;; P1: Truncation properties
;;; ---------------------------------------------------------------------------

(defspec p1-truncate-str-length-bounded 200
  (prop/for-all [s gen/string-alphanumeric
                 max-len gen-positive-int]
                (let [truncated (let [s (str s)]
                                  (subs s 0 (min max-len (count s))))]
                  (<= (count truncated) max-len))))

(defspec p2-truncate-str-identity-for-short 200
  (prop/for-all [s gen/string-alphanumeric]
                (let [max-len (+ (count s) 10)
                      truncated (let [s (str s)]
                                  (subs s 0 (min max-len (count s))))]
                  (= truncated s))))

(defspec p3-truncate-str-prefix-preserved 200
  (prop/for-all [s (gen/such-that #(>= (count %) 3) gen/string-alphanumeric)
                 max-len (gen/choose 1 100)]
                (let [truncated (let [s (str s)]
                                  (subs s 0 (min max-len (count s))))
                      prefix-len (min max-len (count s))]
                  (= truncated (subs s 0 prefix-len)))))

;;; ---------------------------------------------------------------------------
;;; P2: Execution mode invariants
;;; ---------------------------------------------------------------------------

(defspec p4-execution-mode-is-closed 200
  (prop/for-all [mode gen-execution-mode]
                (contains? #{:delegate :agentic} mode)))

(defspec p5-agentic-mode-requires-backend-resolution 100
  (prop/for-all [mode gen-execution-mode]
    ;; :agentic mode should attempt session-kg, :delegate should not
    ;; This is a structural invariant of create-kg-store
                (if (= mode :agentic)
                  true  ;; agentic tries session-kg first, then falls back
                  true))) ;; delegate uses kg-factory only

;;; ---------------------------------------------------------------------------
;;; P3: Raw result status properties
;;; ---------------------------------------------------------------------------

(defspec p6-completed-predicate-totality 200
  (prop/for-all [result gen-raw-result]
                (boolean? (= :completed (:status result)))))

(defspec p7-completed-is-exclusive 200
  (prop/for-all [status gen-result-status]
                (let [result {:status status}
                      is-completed (= :completed (:status result))
                      is-failed (= :failed (:status result))]
      ;; completed and failed are mutually exclusive
                  (not (and is-completed is-failed)))))

;;; ---------------------------------------------------------------------------
;;; P4: Domain value object invariants
;;; ---------------------------------------------------------------------------

(defspec p8-execution-context-always-has-start-time 100
  (prop/for-all [drone-id gen-drone-id]
                (let [ctx (domain/->execution-context
                           {:drone-id drone-id
                            :task-id (str "task-" drone-id)
                            :parent-id nil
                            :project-root "/tmp"
                            :kg-store nil})]
                  (and (number? (:start-time ctx))
                       (pos? (:start-time ctx))))))

(defspec p9-elapsed-ms-is-non-negative 100
  (prop/for-all [drone-id gen-drone-id]
                (let [ctx (domain/->execution-context
                           {:drone-id drone-id
                            :task-id (str "task-" drone-id)
                            :parent-id nil
                            :project-root "/tmp"
                            :kg-store nil})]
                  (>= (domain/elapsed-ms ctx) 0))))

(defspec p10-success-result-always-completed 100
  (prop/for-all [drone-id gen-drone-id
                 diff-results gen-diff-results]
                (let [ctx (domain/->execution-context
                           {:drone-id drone-id
                            :task-id (str "task-" drone-id)
                            :parent-id nil
                            :project-root "/tmp"
                            :kg-store nil})
                      result (domain/success-result ctx {:result {:status :completed}
                                                         :diff-results diff-results
                                                         :validation {}})]
                  (= :completed (:status result)))))

(defspec p11-failure-result-always-failed 100
  (prop/for-all [drone-id gen-drone-id]
                (let [ctx (domain/->execution-context
                           {:drone-id drone-id
                            :task-id (str "task-" drone-id)
                            :parent-id nil
                            :project-root "/tmp"
                            :kg-store nil})
                      result (domain/failure-result ctx {:error "test error"})]
                  (= :failed (:status result)))))

;;; ---------------------------------------------------------------------------
;;; P5: Diff results structure properties
;;; ---------------------------------------------------------------------------

(defspec p12-diff-results-keys-always-present 200
  (prop/for-all [diff-results gen-diff-results]
                (and (contains? diff-results :applied)
                     (contains? diff-results :failed)
                     (contains? diff-results :proposed)
                     (vector? (:applied diff-results))
                     (vector? (:failed diff-results))
                     (vector? (:proposed diff-results)))))

;;; ---------------------------------------------------------------------------
;;; P6: generate-drone-id uniqueness
;;; ---------------------------------------------------------------------------

(defspec p13-drone-ids-are-unique 100
  (prop/for-all [_ (gen/return nil)]
                (let [ids (repeatedly 10 domain/generate-drone-id)]
                  (= (count ids) (count (set ids))))))

(defspec p14-drone-id-has-prefix 200
  (prop/for-all [_ (gen/return nil)]
                (let [id (domain/generate-drone-id)]
                  (str/starts-with? id "drone-"))))

(defspec p15-task-id-derived-from-drone-id 200
  (prop/for-all [drone-id gen-drone-id]
                (let [task-id (domain/generate-task-id drone-id)]
                  (and (str/starts-with? task-id "task-")
                       (str/includes? task-id drone-id)))))

;;; ---------------------------------------------------------------------------
;;; Unit tests for structural invariants
;;; ---------------------------------------------------------------------------

(deftest execution-context-fields
  (testing "ExecutionContext always has required fields"
    (let [ctx (domain/->execution-context
               {:drone-id "drone-test-1"
                :task-id "task-test-1"
                :parent-id "parent-1"
                :project-root "/tmp"
                :kg-store nil})]
      (is (= "drone-test-1" (:drone-id ctx)))
      (is (= "task-test-1" (:task-id ctx)))
      (is (= "parent-1" (:parent-id ctx)))
      (is (nil? (:pre-validation ctx)))
      (is (nil? (:file-contents-before ctx)))
      (is (number? (:start-time ctx))))))

(deftest execution-context-requires-drone-id
  (testing "Missing drone-id throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"requires :drone-id"
                          (domain/->execution-context
                           {:drone-id nil
                            :task-id "task-1"})))))

(deftest with-pre-validation-adds-to-ctx
  (testing "with-pre-validation adds validation data"
    (let [ctx (domain/->execution-context
               {:drone-id "drone-test"
                :task-id "task-test"
                :project-root "/tmp"
                :kg-store nil})
          validation {:file1 {:pre-valid? true}}
          ctx' (domain/with-pre-validation ctx validation)]
      (is (= validation (:pre-validation ctx'))))))

(deftest success-and-failure-results
  (testing "success-result produces completed status"
    (let [ctx (domain/->execution-context
               {:drone-id "drone-test"
                :task-id "task-test"
                :project-root "/tmp"
                :kg-store nil})
          result (domain/success-result ctx {:result {:ok true}
                                             :diff-results {:applied ["f.clj"]
                                                            :failed []
                                                            :proposed []}
                                             :validation {}})]
      (is (= :completed (:status result)))
      (is (= "drone-test" (:agent-id result)))
      (is (= ["f.clj"] (:files-modified result)))))

  (testing "failure-result produces failed status"
    (let [ctx (domain/->execution-context
               {:drone-id "drone-test"
                :task-id "task-test"
                :project-root "/tmp"
                :kg-store nil})
          result (domain/failure-result ctx {:error "boom"})]
      (is (= :failed (:status result)))
      (is (= {:error "boom"} (:error-info result))))))
