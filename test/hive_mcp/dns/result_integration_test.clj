(ns hive-mcp.dns.result-integration-test
  "End-to-end integration test for the Result DSL pipeline.

   Tests the COMPLETE flow from Result creation through composition
   to MCP boundary conversion, mirroring real tool handler patterns:

   Input → Validation (require-*) → Computation (let-ok) →
     try-result boundary → result->mcp → MCP response

   Coverage sections:
   1. Happy path E2E (full pipeline success)
   2. Validation failure (short-circuit on bad input)
   3. Domain errors (business logic failures)
   4. IO/exception handling (try-effect boundaries)
   5. Composition & chaining (bind, let-ok, map-ok, map-err)
   6. Taxonomy integration (known error categories)
   7. Spec validation (conformance through pipeline)
   8. Instrumented pipeline (spec contracts enabled)
   9. Error recovery patterns (fallback, enrichment, accumulation)"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [hive-mcp.dns.result :as r]
            [hive-mcp.dns.result.spec :as rspec]
            [hive-mcp.dns.result.taxonomy :as tax]))

;; =============================================================================
;; Simulated Tool Infrastructure
;;
;; Mirrors the boundary pattern used in tools/kg.clj, tools/crystal.clj,
;; tools/cider.clj, tools/session_complete.clj. These are NOT stubs — they
;; exercise the exact same patterns and code paths as production handlers.
;; =============================================================================

(defn- require-param
  "Validate a required string parameter. Returns Result.
   Pattern from tools/kg.clj require-non-empty."
  [value param-name]
  (if (or (nil? value) (and (string? value) (str/blank? value)))
    (r/err :sdk/missing-param {:message (str param-name " is required")
                               :param   param-name})
    (r/ok value)))

(defn- require-positive-int
  "Validate an integer parameter is positive. Returns Result."
  [value param-name]
  (cond
    (nil? value)           (r/err :sdk/missing-param
                                  {:message (str param-name " is required")})
    (not (integer? value)) (r/err :parse/invalid-json
                                  {:message (str param-name " must be integer")})
    (not (pos? value))     (r/err :sdk/invalid-request
                                  {:message (str param-name " must be positive")})
    :else                  (r/ok value)))

(defn- try-result
  "Execute thunk f returning Result; catch unexpected exceptions.
   Pattern from tools/kg.clj, tools/crystal.clj, tools/cider.clj."
  [category f]
  (try
    (f)
    (catch Exception e
      (r/err category {:message (.getMessage e)}))))

(defn- result->mcp
  "Convert Result to MCP response map.
   Pattern from tools/kg.clj result->mcp."
  [r]
  (if (r/ok? r)
    {:type "text" :text (pr-str (:ok r))}
    {:type "text" :text (or (:message r) (str (:error r))) :isError true}))

;; =============================================================================
;; Simulated Domain Operations (pure calculations returning Results)
;; =============================================================================

(defn- lookup-entity
  "Simulate a KG node lookup. Returns Result."
  [id]
  (if (= id "missing")
    (r/err :kg/node-not-found {:message (str "Node " id " not found")
                               :node-id id})
    (r/ok {:id id :type :entity :data {:name (str "Entity-" id)}})))

(defn- transform-entity
  "Simulate entity transformation. Returns Result."
  [entity format]
  (case format
    "json"    (r/ok (assoc entity :format :json))
    "edn"     (r/ok (assoc entity :format :edn))
    "invalid" (r/err :parse/invalid-json {:message "Unsupported format"})
    (r/err :sdk/invalid-request {:message (str "Unknown format: " format)})))

(defn- persist-entity!
  "Simulate persistence with try-effect at IO boundary."
  [entity]
  (r/try-effect* :io/write-failure
    (assoc entity :persisted true :timestamp (System/currentTimeMillis))))

(defn- enrich-with-edges
  "Simulate KG edge enrichment. Pure calculation."
  [entity edge-count]
  (r/ok (assoc entity :edges (vec (range edge-count)))))

;; =============================================================================
;; Simulated Tool Handlers (mirror real handler patterns exactly)
;; =============================================================================

(defn- handle-get-entity*
  "Internal handler logic using let-ok pipeline.
   Pattern: validate → lookup → transform → return."
  [{:keys [id format]}]
  (r/let-ok [validated-id (require-param id "id")
             entity       (lookup-entity validated-id)
             formatted    (transform-entity entity (or format "edn"))]
    (r/ok {:success true :entity formatted})))

(defn handle-get-entity
  "Public handler with try-result boundary.
   Pattern from tools/kg.clj."
  [params]
  (result->mcp (try-result :sdk/handler-error #(handle-get-entity* params))))

(defn- handle-persist-entity*
  "Internal handler: validate → lookup → persist."
  [{:keys [id]}]
  (r/let-ok [validated-id (require-param id "id")
             entity       (lookup-entity validated-id)
             persisted    (persist-entity! entity)]
    (r/ok {:success true :persisted persisted})))

(defn handle-persist-entity
  "Public handler with try-result boundary."
  [params]
  (result->mcp (try-result :io/write-failure #(handle-persist-entity* params))))

(defn- handle-complex-pipeline*
  "Internal handler: multi-step pipeline with validation + enrichment."
  [{:keys [id format edge-count]}]
  (r/let-ok [vid      (require-param id "id")
             ecount   (require-positive-int edge-count "edge-count")
             entity   (lookup-entity vid)
             enriched (enrich-with-edges entity ecount)
             formatted (transform-entity enriched (or format "edn"))]
    (r/ok {:success true
            :entity  formatted
            :summary {:id vid :edges ecount :format (or format "edn")}})))

(defn handle-complex-pipeline
  "Public handler for multi-step pipeline."
  [params]
  (result->mcp (try-result :sdk/handler-error #(handle-complex-pipeline* params))))

;; =============================================================================
;; Section 1: Happy Path E2E Tests
;; =============================================================================

(deftest e2e-happy-path-simple
  (testing "Full pipeline: valid input → lookup → transform → MCP success"
    (let [response (handle-get-entity {:id "node-1" :format "json"})]
      (is (map? response))
      (is (= "text" (:type response)))
      (is (nil? (:isError response)))
      (is (string? (:text response)))
      (let [payload (read-string (:text response))]
        (is (:success payload))
        (is (= :json (get-in payload [:entity :format])))
        (is (= "node-1" (get-in payload [:entity :id])))))))

(deftest e2e-happy-path-default-format
  (testing "Pipeline with default format (nil → 'edn' fallback)"
    (let [response (handle-get-entity {:id "node-2"})]
      (is (nil? (:isError response)))
      (let [payload (read-string (:text response))]
        (is (= :edn (get-in payload [:entity :format])))))))

(deftest e2e-happy-path-persist
  (testing "Full pipeline with IO effect: validate → lookup → persist → success"
    (let [response (handle-persist-entity {:id "node-3"})]
      (is (nil? (:isError response)))
      (let [payload (read-string (:text response))]
        (is (:success payload))
        (is (true? (get-in payload [:persisted :persisted])))
        (is (number? (get-in payload [:persisted :timestamp])))))))

(deftest e2e-happy-path-complex
  (testing "Multi-step pipeline: validate → lookup → enrich → transform → success"
    (let [response (handle-complex-pipeline {:id "node-4" :format "json" :edge-count 5})]
      (is (nil? (:isError response)))
      (let [payload (read-string (:text response))]
        (is (:success payload))
        (is (= 5 (count (get-in payload [:entity :edges]))))
        (is (= :json (get-in payload [:entity :format])))
        (is (= "node-4" (get-in payload [:summary :id])))
        (is (= 5 (get-in payload [:summary :edges])))))))

;; =============================================================================
;; Section 2: Validation Failure E2E Tests
;; =============================================================================

(deftest e2e-validation-missing-param
  (testing "Missing required param → short-circuits → MCP error"
    (let [response (handle-get-entity {})]
      (is (true? (:isError response)))
      (is (re-find #"id.*required" (:text response))))))

(deftest e2e-validation-empty-string
  (testing "Empty string param → validation error → MCP error"
    (let [response (handle-get-entity {:id ""})]
      (is (true? (:isError response)))
      (is (re-find #"id.*required" (:text response))))))

(deftest e2e-validation-nil-param
  (testing "Nil param → validation error → MCP error"
    (let [response (handle-get-entity {:id nil})]
      (is (true? (:isError response)))
      (is (re-find #"required" (:text response))))))

(deftest e2e-validation-complex-first-step-fails
  (testing "Complex pipeline: first validation fails → no subsequent steps run"
    (let [response (handle-complex-pipeline {:id nil :format "json" :edge-count 5})]
      (is (true? (:isError response)))
      (is (re-find #"id.*required" (:text response))))))

(deftest e2e-validation-complex-second-step-fails
  (testing "Complex pipeline: second validation fails → short-circuits"
    (let [response (handle-complex-pipeline {:id "node-1" :edge-count -1})]
      (is (true? (:isError response)))
      (is (re-find #"positive" (:text response))))))

(deftest e2e-validation-complex-non-integer
  (testing "Complex pipeline: non-integer edge-count → parse error"
    (let [response (handle-complex-pipeline {:id "node-1" :edge-count "five"})]
      (is (true? (:isError response)))
      (is (re-find #"integer" (:text response))))))

;; =============================================================================
;; Section 3: Domain Error E2E Tests
;; =============================================================================

(deftest e2e-domain-entity-not-found
  (testing "Valid param but entity doesn't exist → domain error → MCP error"
    (let [response (handle-get-entity {:id "missing"})]
      (is (true? (:isError response)))
      (is (re-find #"not found" (:text response))))))

(deftest e2e-domain-invalid-format
  (testing "Valid entity but invalid format → domain error → MCP error"
    (let [response (handle-get-entity {:id "node-1" :format "invalid"})]
      (is (true? (:isError response)))
      (is (re-find #"[Uu]nsupported" (:text response))))))

(deftest e2e-domain-unknown-format
  (testing "Valid entity but unknown format → domain error with details"
    (let [response (handle-get-entity {:id "node-1" :format "xml"})]
      (is (true? (:isError response)))
      (is (re-find #"Unknown format" (:text response))))))

;; =============================================================================
;; Section 4: IO/Exception E2E Tests
;; =============================================================================

(deftest e2e-exception-in-try-result
  (testing "Unexpected exception caught by try-result boundary → MCP error"
    (let [response (result->mcp
                     (try-result :sdk/handler-error
                       (fn [] (throw (ex-info "Unexpected NPE" {:cause :null})))))]
      (is (true? (:isError response)))
      (is (re-find #"Unexpected NPE" (:text response))))))

(deftest e2e-try-effect-success
  (testing "try-effect wraps successful IO → ok Result"
    (let [result (r/try-effect (+ 1 2))]
      (is (r/ok? result))
      (is (= 3 (:ok result))))))

(deftest e2e-try-effect-failure
  (testing "try-effect catches exception → err Result with :effect/exception"
    (let [result (r/try-effect (/ 1 0))]
      (is (r/err? result))
      (is (= :effect/exception (:error result)))
      (is (string? (:message result))))))

(deftest e2e-try-effect*-custom-category
  (testing "try-effect* uses custom error category from taxonomy"
    (let [result (r/try-effect* :io/read-failure
                   (throw (java.io.FileNotFoundException. "/tmp/missing")))]
      (is (r/err? result))
      (is (= :io/read-failure (:error result)))
      (is (tax/known-error? (:error result)))
      (is (string? (:message result))))))

(deftest e2e-try-effect*-io-boundary
  (testing "try-effect* at IO boundary catches and categorizes diverse exceptions"
    ;; IOException
    (let [r1 (r/try-effect* :io/write-failure
               (throw (java.io.IOException. "Permission denied")))]
      (is (r/err? r1))
      (is (= :io/write-failure (:error r1)))
      (is (re-find #"Permission denied" (:message r1))))
    ;; NullPointerException
    (let [r2 (r/try-effect* :sdk/handler-error
               (throw (NullPointerException. "null ref")))]
      (is (r/err? r2))
      (is (= :sdk/handler-error (:error r2))))
    ;; NumberFormatException
    (let [r3 (r/try-effect* :parse/invalid-json
               (Integer/parseInt "not-a-number"))]
      (is (r/err? r3))
      (is (= :parse/invalid-json (:error r3))))))

(deftest e2e-nested-try-effect-isolation
  (testing "try-effect in nested calls: inner catches don't leak to outer"
    (let [inner-result (r/try-effect (/ 1 0))
          outer-result (r/try-effect
                         (if (r/err? inner-result)
                           {:recovered true :from (:error inner-result)}
                           (:ok inner-result)))]
      (is (r/err? inner-result) "Inner catches the divide-by-zero")
      (is (r/ok? outer-result) "Outer sees the recovery, not the exception")
      (is (true? (get-in outer-result [:ok :recovered]))))))

;; =============================================================================
;; Section 5: Composition & Chaining E2E Tests
;; =============================================================================

(deftest e2e-bind-chain-happy
  (testing "Bind chain: ok → ok → ok produces final value"
    (let [result (-> (r/ok 1)
                     (r/bind #(r/ok (+ % 10)))
                     (r/bind #(r/ok (* % 2)))
                     (r/bind #(r/ok (str "result: " %))))]
      (is (r/ok? result))
      (is (= "result: 22" (:ok result))))))

(deftest e2e-bind-chain-short-circuit
  (testing "Bind chain: ok → err → skips rest → preserves first error"
    (let [sentinel (atom false)
          result (-> (r/ok 1)
                     (r/bind #(r/err :test/fail {:value %}))
                     (r/bind (fn [_] (reset! sentinel true) (r/ok :never))))]
      (is (r/err? result))
      (is (= :test/fail (:error result)))
      (is (false? @sentinel) "Subsequent steps should not execute"))))

(deftest e2e-let-ok-pipeline-with-side-effects
  (testing "let-ok pipeline mimicking real tool handler with effects"
    (let [call-log (atom [])
          result (r/let-ok [a (do (swap! call-log conj :step-a) (r/ok 10))
                            b (do (swap! call-log conj :step-b) (r/ok 20))
                            c (do (swap! call-log conj :step-c) (r/ok (+ a b)))]
                   (r/ok {:sum c :steps @call-log}))]
      (is (r/ok? result))
      (is (= 30 (get-in result [:ok :sum])))
      (is (= [:step-a :step-b :step-c] (get-in result [:ok :steps]))))))

(deftest e2e-let-ok-short-circuit-skips-effects
  (testing "let-ok short-circuit prevents side effects in later steps"
    (let [call-log (atom [])
          result   (r/let-ok [a (do (swap! call-log conj :step-a) (r/ok 10))
                              _b (do (swap! call-log conj :step-b) (r/err :test/fail))
                              _c (do (swap! call-log conj :step-c) (r/ok 99))]
                     (r/ok :unreachable))]
      (is (r/err? result))
      (is (= :test/fail (:error result)))
      (is (= [:step-a :step-b] @call-log)
          "step-c should not execute after step-b fails"))))

(deftest e2e-map-ok-transform-pipeline
  (testing "map-ok chain transforms values through pipeline"
    (let [result (-> (r/ok {:name "test" :count 0})
                     (r/map-ok #(update % :count inc))
                     (r/map-ok #(update % :count inc))
                     (r/map-ok #(assoc % :processed true)))]
      (is (r/ok? result))
      (is (= {:name "test" :count 2 :processed true} (:ok result))))))

(deftest e2e-map-err-enrichment
  (testing "map-err enriches error data without changing category"
    (let [result (-> (r/err :io/timeout {:path "/tmp"})
                     (r/map-err #(assoc % :retries 3))
                     (r/map-err #(assoc % :severity :high)))]
      (is (r/err? result))
      (is (= :io/timeout (:error result)))
      (is (= 3 (:retries result)))
      (is (= :high (:severity result))))))

(deftest e2e-mixed-bind-and-map-ok
  (testing "Mixed bind + map-ok pipeline (real-world pattern)"
    (let [result (-> (r/ok {:items [1 2 3]})
                     (r/map-ok #(update % :items count))   ;; {:items 3}
                     (r/bind (fn [{:keys [items]}]
                               (if (pos? items)
                                 (r/ok {:count items :status :ok})
                                 (r/err :domain/empty {:message "No items"}))))
                     (r/map-ok #(assoc % :processed true)))]
      (is (r/ok? result))
      (is (= {:count 3 :status :ok :processed true} (:ok result))))))

;; =============================================================================
;; Section 6: Taxonomy Integration Tests
;; =============================================================================

(deftest e2e-taxonomy-known-errors-in-pipeline
  (testing "All domain errors in pipeline use known taxonomy categories"
    (let [results [(handle-get-entity* {:id nil})                   ;; :sdk/missing-param
                   (handle-get-entity* {:id "missing"})             ;; :kg/node-not-found
                   (handle-get-entity* {:id "x" :format "invalid"}) ;; :parse/invalid-json
                   (handle-complex-pipeline* {:id "x" :edge-count "nope"})]] ;; :parse/invalid-json
      (doseq [r results]
        (when (r/err? r)
          (is (tax/known-error? (:error r))
              (str "Error category " (:error r) " should be in taxonomy")))))))

(deftest e2e-taxonomy-coverage
  (testing "Taxonomy covers all subsystems used in the codebase"
    ;; IO subsystem
    (is (tax/known-error? :io/read-failure))
    (is (tax/known-error? :io/write-failure))
    (is (tax/known-error? :io/timeout))
    ;; SDK subsystem
    (is (tax/known-error? :sdk/invalid-request))
    (is (tax/known-error? :sdk/missing-param))
    (is (tax/known-error? :sdk/handler-error))
    ;; KG subsystem
    (is (tax/known-error? :kg/node-not-found))
    (is (tax/known-error? :kg/edge-invalid))
    ;; Parse subsystem
    (is (tax/known-error? :parse/invalid-json))
    (is (tax/known-error? :parse/invalid-edn))
    ;; Effect boundary
    (is (tax/known-error? :effect/exception))
    ;; Chroma
    (is (tax/known-error? :chroma/connection-failed))
    ;; Transport
    (is (tax/known-error? :transport/timeout))
    ;; Drone
    (is (tax/known-error? :drone/spawn-failed))))

(deftest e2e-taxonomy-unknown-not-matched
  (testing "Unknown categories are correctly rejected"
    (is (false? (tax/known-error? :foo/bar)))
    (is (false? (tax/known-error? :made-up/category)))
    (is (false? (tax/known-error? :unknown)))))

;; =============================================================================
;; Section 7: Spec Validation E2E Tests
;; =============================================================================

(deftest e2e-spec-pipeline-results-conform
  (testing "All Results flowing through the pipeline conform to ::result spec"
    ;; Validation helpers
    (is (s/valid? ::rspec/result (require-param "hello" "test")))
    (is (s/valid? ::rspec/result (require-param nil "test")))
    (is (s/valid? ::rspec/result (require-positive-int 5 "count")))
    (is (s/valid? ::rspec/result (require-positive-int -1 "count")))
    (is (s/valid? ::rspec/result (require-positive-int nil "count")))
    ;; Domain operations
    (is (s/valid? ::rspec/result (lookup-entity "node-1")))
    (is (s/valid? ::rspec/result (lookup-entity "missing")))
    (is (s/valid? ::rspec/result (transform-entity {:id "x"} "json")))
    (is (s/valid? ::rspec/result (transform-entity {:id "x"} "invalid")))
    ;; try-effect wrapping
    (is (s/valid? ::rspec/result (r/try-effect (+ 1 2))))
    (is (s/valid? ::rspec/result (r/try-effect (/ 1 0))))
    ;; Full handler pipeline
    (is (s/valid? ::rspec/result (handle-get-entity* {:id "node-1" :format "json"})))
    (is (s/valid? ::rspec/result (handle-get-entity* {:id nil})))
    (is (s/valid? ::rspec/result (handle-get-entity* {:id "missing"})))))

(deftest e2e-spec-discriminated-union
  (testing "No Result is both ok and err (sum type invariant through pipeline)"
    (let [results [(r/ok 42)
                   (r/err :test/fail)
                   (require-param "hello" "p")
                   (require-param nil "p")
                   (lookup-entity "node-1")
                   (lookup-entity "missing")
                   (handle-get-entity* {:id "node-1"})
                   (handle-get-entity* {:id nil})]]
      (doseq [r results]
        (is (not= (r/ok? r) (r/err? r))
            (str "Result should be exactly one of ok/err: " (pr-str r)))
        (is (not (and (contains? r :ok) (contains? r :error)))
            "Result must never contain both :ok and :error keys")))))

(deftest e2e-spec-ok-err-partition
  (testing "ok? and err? form a complete partition on valid Results"
    (let [results [(r/ok nil) (r/ok 0) (r/ok false) (r/ok "") (r/ok [])
                   (r/err :x) (r/err :x {:data 1}) (r/err :y/z)]]
      (doseq [r results]
        (is (or (r/ok? r) (r/err? r))
            (str "Every valid Result must be ok? or err?: " (pr-str r)))
        (is (not (and (r/ok? r) (r/err? r)))
            (str "No Result should be both ok? and err?: " (pr-str r)))))))

;; =============================================================================
;; Section 8: Instrumented Pipeline E2E Test
;; =============================================================================

(deftest e2e-instrumented-pipeline
  (testing "Full pipeline works under spec instrumentation"
    (rspec/instrument!)
    (try
      ;; Happy path
      (let [response (handle-get-entity {:id "node-1" :format "json"})]
        (is (nil? (:isError response))))
      ;; Validation error path
      (let [response (handle-get-entity {:id nil})]
        (is (true? (:isError response))))
      ;; Domain error path
      (let [response (handle-get-entity {:id "missing"})]
        (is (true? (:isError response))))
      ;; Complex pipeline happy path
      (let [response (handle-complex-pipeline {:id "n1" :format "edn" :edge-count 3})]
        (is (nil? (:isError response))))
      ;; Complex pipeline error path
      (let [response (handle-complex-pipeline {:id "n1" :edge-count -1})]
        (is (true? (:isError response))))
      (finally
        (rspec/unstrument!)))))

;; =============================================================================
;; Section 9: Error Recovery & Composition Patterns
;; =============================================================================

(deftest e2e-error-recovery-with-map-err
  (testing "Error recovery: map-err adds context, then result->mcp formats"
    (let [raw-err  (r/err :io/timeout {:path "/slow/endpoint"})
          enriched (-> raw-err
                       (r/map-err #(assoc % :retry-after 5))
                       (r/map-err #(assoc % :message
                                     (str "Timeout accessing " (:path %)
                                          ". Retry after " (:retry-after %) "s"))))
          mcp      (result->mcp enriched)]
      (is (true? (:isError mcp)))
      (is (re-find #"Timeout accessing /slow/endpoint" (:text mcp)))
      (is (re-find #"Retry after 5s" (:text mcp))))))

(deftest e2e-fallback-pattern
  (testing "Fallback: try primary, on error try secondary"
    (let [primary   (fn [id] (if (= id "cached")
                               (r/ok {:source :cache :id id})
                               (r/err :cache/miss {:id id})))
          secondary (fn [id] (r/ok {:source :db :id id}))
          with-fallback (fn [id]
                          (let [r (primary id)]
                            (if (r/ok? r) r (secondary id))))]
      ;; Cache hit → returns cache result
      (let [r (with-fallback "cached")]
        (is (r/ok? r))
        (is (= :cache (:source (:ok r)))))
      ;; Cache miss → falls back to DB
      (let [r (with-fallback "not-cached")]
        (is (r/ok? r))
        (is (= :db (:source (:ok r))))))))

(deftest e2e-accumulate-errors-pattern
  (testing "Accumulate multiple independent validations before failing"
    (let [validate-all (fn [{:keys [name age email]}]
                         (let [errs (cond-> []
                                      (nil? name)  (conj "name required")
                                      (nil? age)   (conj "age required")
                                      (nil? email) (conj "email required"))]
                           (if (seq errs)
                             (r/err :sdk/invalid-request
                                    {:message (str/join ", " errs)
                                     :errors  errs})
                             (r/ok {:name name :age age :email email}))))
          valid   (validate-all {:name "Alice" :age 30 :email "a@b.com"})
          invalid (validate-all {:name nil :age nil :email nil})]
      (is (r/ok? valid))
      (is (r/err? invalid))
      (is (= 3 (count (:errors invalid)))))))

(deftest e2e-retry-with-result
  (testing "Retry pattern: attempt up to N times, accumulate errors"
    (let [attempt-count (atom 0)
          flaky-op (fn []
                     (swap! attempt-count inc)
                     (if (<= @attempt-count 2)
                       (r/err :io/timeout {:attempt @attempt-count})
                       (r/ok {:result "success" :attempts @attempt-count})))
          retry (fn [f max-retries]
                  (loop [n 0]
                    (let [r (f)]
                      (if (or (r/ok? r) (>= n max-retries))
                        r
                        (recur (inc n))))))]
      ;; Retry succeeds on 3rd attempt
      (let [result (retry flaky-op 5)]
        (is (r/ok? result))
        (is (= 3 (get-in result [:ok :attempts])))))))

(deftest e2e-pipeline-with-logging-side-effects
  (testing "Pipeline with side-effects tracked via atom (mimics logging)"
    (let [log (atom [])
          pipeline (fn [input]
                     (swap! log conj {:step :start :input input})
                     (r/let-ok [v (do (swap! log conj {:step :validate})
                                      (require-param (:id input) "id"))
                                e (do (swap! log conj {:step :lookup})
                                      (lookup-entity v))
                                t (do (swap! log conj {:step :transform})
                                      (transform-entity e "json"))]
                       (do (swap! log conj {:step :complete})
                           (r/ok t))))]
      ;; Happy path — all steps logged
      (let [result (pipeline {:id "node-1"})]
        (is (r/ok? result))
        (is (= [:start :validate :lookup :transform :complete]
               (mapv :step @log))))
      ;; Error path — stops at lookup
      (reset! log [])
      (let [result (pipeline {:id "missing"})]
        (is (r/err? result))
        (is (= [:start :validate :lookup]
               (mapv :step @log))
            "Transform and complete should not execute after lookup fails")))))

;; =============================================================================
;; Section 10: Cross-Module Boundary Integration
;; =============================================================================

(deftest e2e-result-through-full-boundary
  (testing "Result flows: internal* → try-result → result->mcp → MCP shape"
    ;; Verify the exact MCP response shape for each path
    (let [success-mcp (handle-get-entity {:id "node-1" :format "edn"})
          error-mcp   (handle-get-entity {:id nil})]
      ;; Success: {:type "text" :text "..."} — NO :isError key
      (is (= "text" (:type success-mcp)))
      (is (string? (:text success-mcp)))
      (is (not (contains? success-mcp :isError)))
      ;; Error: {:type "text" :text "..." :isError true}
      (is (= "text" (:type error-mcp)))
      (is (string? (:text error-mcp)))
      (is (true? (:isError error-mcp))))))

(deftest e2e-try-result-does-not-double-wrap
  (testing "try-result returns Result directly (no double-wrapping in {:ok {:ok ...}})"
    (let [result (try-result :test/category
                   #(r/ok {:data "hello"}))]
      (is (r/ok? result))
      (is (= {:data "hello"} (:ok result)))
      (is (not (contains? (:ok result) :ok))
          "Should not double-wrap"))))

(deftest e2e-try-result-catches-only-exceptions
  (testing "try-result catches exceptions but passes through normal Results"
    ;; Normal ok → passes through
    (let [r1 (try-result :x #(r/ok 42))]
      (is (r/ok? r1))
      (is (= 42 (:ok r1))))
    ;; Normal err → passes through
    (let [r2 (try-result :x #(r/err :y {:msg "expected"}))]
      (is (r/err? r2))
      (is (= :y (:error r2))))
    ;; Exception → caught and wrapped
    (let [r3 (try-result :x #(throw (ex-info "boom" {})))]
      (is (r/err? r3))
      (is (= :x (:error r3))))))
