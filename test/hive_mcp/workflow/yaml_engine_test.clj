(ns hive-mcp.workflow.yaml-engine-test
  "Tests for YAMLWorkflowEngine (IWorkflowEngine implementation).

   Validates:
   - YAML parsing and normalization
   - Variable substitution ({{var}} syntax)
   - Sequential step execution
   - Parallel branch execution
   - Conditional step execution (:when)
   - Step dependency validation and topological sort
   - Workflow lifecycle (load, validate, execute, status, cancel)
   - Error handling (CLARITY-Y: never throws)
   - Built-in actions (echo, noop, shell, transform, mcp-call)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.workflow :as wf]
            [hive-mcp.workflow.yaml-engine :as engine]
            [hive-mcp.tools :as tools]))

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defn clean-state [f]
  (engine/clear-executions!)
  (wf/clear-workflow-engine!)
  (f)
  (engine/clear-executions!)
  (wf/clear-workflow-engine!))

(use-fixtures :each clean-state)

;;; ============================================================================
;;; Sample YAML Definitions
;;; ============================================================================

(def simple-workflow-yaml
  "name: simple-test
version: '1.0'
description: A simple test workflow
params:
  greeting: hello
steps:
  - id: step-1
    title: Echo Greeting
    action: echo
    args:
      message: '{{greeting}} world'
  - id: step-2
    title: Echo Result
    action: echo
    depends-on:
      - step-1
    args:
      message: 'after step-1: {{step-1.result.message}}'")

(def parallel-workflow-yaml
  "name: parallel-test
version: '1.0'
description: Workflow with parallel branches
steps:
  - id: setup
    title: Setup
    action: echo
    args:
      value: initialized
  - id: parallel-group
    title: Parallel Processing
    parallel:
      - id: branch-a
        title: Branch A
        action: echo
        args:
          branch: A
          msg: 'processing A'
      - id: branch-b
        title: Branch B
        action: echo
        args:
          branch: B
          msg: 'processing B'
  - id: aggregate
    title: Aggregate Results
    action: echo
    args:
      summary: done")

(def conditional-workflow-yaml
  "name: conditional-test
version: '1.0'
steps:
  - id: check
    title: Check Condition
    action: echo
    args:
      status: ok
  - id: on-success
    title: On Success
    action: echo
    when: '{{check.success}}'
    depends-on:
      - check
    args:
      message: 'check passed'
  - id: on-failure
    title: On Failure
    action: echo
    when: 'false'
    args:
      message: 'this should be skipped'")

(def bad-deps-yaml
  "name: bad-deps
version: '1.0'
steps:
  - id: step-1
    title: Step 1
    action: echo
    depends-on:
      - nonexistent-step
    args:
      message: hello")

(def empty-workflow-yaml
  "name: empty-workflow
version: '1.0'
description: Workflow with no steps
steps: []")

(def multi-action-yaml
  "name: multi-action
version: '1.0'
params:
  name: World
steps:
  - id: greet
    title: Greeting
    action: echo
    args:
      message: 'Hello {{name}}'
  - id: noop-step
    title: Do Nothing
    action: noop
  - id: unknown-action
    title: Unknown
    action: nonexistent
    args:
      x: 1")

;;; ============================================================================
;;; Helper
;;; ============================================================================

(defn- make-engine []
  (engine/create-yaml-engine))

(defn- load-from-yaml [engine yaml-str wf-name]
  (wf/load-workflow engine wf-name {:yaml yaml-str}))

;;; ============================================================================
;;; YAML Parsing Tests
;;; ============================================================================

(deftest parse-yaml-valid-test
  (testing "Valid YAML parses successfully"
    (let [result (engine/parse-yaml simple-workflow-yaml)]
      (is (true? (:parsed? result)))
      (is (map? (:data result)))
      (is (= "simple-test" (:name (:data result))))
      (is (= 2 (count (:steps (:data result))))))))

(deftest parse-yaml-invalid-test
  (testing "Invalid YAML returns error"
    (let [result (engine/parse-yaml "{{invalid: yaml: [[[")]
      (is (false? (:parsed? result)))
      (is (seq (:errors result))))))

(deftest parse-yaml-non-map-test
  (testing "Non-map YAML returns error"
    (let [result (engine/parse-yaml "- just\n- a\n- list")]
      (is (false? (:parsed? result)))
      (is (some #(re-find #"not parse to a map" %) (:errors result))))))

;;; ============================================================================
;;; Variable Substitution Tests
;;; ============================================================================

(deftest substitute-simple-vars-test
  (testing "Simple variable substitution"
    (let [result (#'engine/substitute-vars "Hello {{name}}" {:name "World"})]
      (is (= "Hello World" result)))))

(deftest substitute-nested-vars-test
  (testing "Nested path variable substitution"
    (let [ctx {:step-1 {:result {:message "from step 1"}}}
          result (#'engine/substitute-vars "Got: {{step-1.result.message}}" ctx)]
      (is (= "Got: from step 1" result)))))

(deftest substitute-missing-vars-test
  (testing "Missing variables are left as-is"
    (let [result (#'engine/substitute-vars "{{unknown}}" {})]
      (is (= "{{unknown}}" result)))))

(deftest substitute-multiple-vars-test
  (testing "Multiple variables in one string"
    (let [result (#'engine/substitute-vars "{{a}} and {{b}}" {:a "X" :b "Y"})]
      (is (= "X and Y" result)))))

(deftest substitute-non-string-test
  (testing "Non-string values pass through"
    (is (= 42 (#'engine/substitute-vars 42 {:x 1})))))

;;; ============================================================================
;;; Protocol Satisfaction Tests
;;; ============================================================================

(deftest engine-satisfies-protocol-test
  (testing "YAMLWorkflowEngine satisfies IWorkflowEngine"
    (let [engine (make-engine)]
      (is (satisfies? wf/IWorkflowEngine engine))
      (is (wf/workflow-engine? engine)))))

(deftest engine-can-be-set-as-active-test
  (testing "Engine can be registered as the active workflow engine"
    (let [engine (make-engine)]
      (wf/set-workflow-engine! engine)
      (is (wf/workflow-engine-set?))
      (is (wf/enhanced?))
      (is (= engine (wf/get-workflow-engine))))))

;;; ============================================================================
;;; Load Workflow Tests
;;; ============================================================================

(deftest load-simple-workflow-test
  (testing "Load a simple workflow from YAML string"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")]
      (is (true? (:loaded? wf)))
      (is (= "simple-test" (:name wf)))
      (is (string? (:workflow-id wf)))
      (is (= 2 (count (:steps wf))))
      (is (empty? (:errors wf)))
      (is (= :yaml (get-in wf [:metadata :engine]))))))

(deftest load-workflow-preserves-params-test
  (testing "Params from YAML are preserved"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")]
      (is (= "hello" (get-in wf [:params :greeting]))))))

(deftest load-workflow-opts-params-merge-test
  (testing "Opts params merge with YAML params (opts win)"
    (let [engine (make-engine)
          wf     (wf/load-workflow engine "simple"
                                   {:yaml simple-workflow-yaml
                                    :params {:greeting "overridden"}})]
      (is (= "overridden" (get-in wf [:params :greeting]))))))

(deftest load-workflow-no-source-test
  (testing "Load without YAML source returns error"
    (let [engine (make-engine)
          wf     (wf/load-workflow engine "missing" {})]
      (is (false? (:loaded? wf)))
      (is (seq (:errors wf))))))

(deftest load-workflow-invalid-yaml-test
  (testing "Invalid YAML returns loaded?=false"
    (let [engine (make-engine)
          wf     (wf/load-workflow engine "bad" {:yaml "{{bad yaml"})]
      (is (false? (:loaded? wf)))
      (is (seq (:errors wf))))))

;;; ============================================================================
;;; Validate Workflow Tests
;;; ============================================================================

(deftest validate-simple-workflow-test
  (testing "Simple workflow validates successfully"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          result (wf/validate-workflow engine wf)]
      (is (true? (:valid? result)))
      (is (empty? (:errors result)))
      (is (vector? (:dependency-order result)))
      (is (seq (:dependency-order result))))))

(deftest validate-bad-deps-test
  (testing "Unknown dependencies produce validation error"
    (let [engine (make-engine)
          wf     (load-from-yaml engine bad-deps-yaml "bad")
          result (wf/validate-workflow engine wf)]
      (is (false? (:valid? result)))
      (is (some #(re-find #"nonexistent-step" %) (:errors result))))))

(deftest validate-empty-workflow-test
  (testing "Empty workflow is valid but warns"
    (let [engine (make-engine)
          wf     (load-from-yaml engine empty-workflow-yaml "empty")
          result (wf/validate-workflow engine wf)]
      (is (true? (:valid? result)))
      (is (some #(re-find #"no steps" %) (:warnings result))))))

(deftest validate-not-loaded-workflow-test
  (testing "Not-loaded workflow fails validation"
    (let [engine (make-engine)
          wf     (wf/load-workflow engine "missing" {})
          result (wf/validate-workflow engine wf)]
      (is (false? (:valid? result))))))

;;; ============================================================================
;;; Execute Step Tests
;;; ============================================================================

(deftest execute-echo-step-test
  (testing "Execute a single echo step"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          result (wf/execute-step engine wf "step-1" {})]
      (is (true? (:success? result)))
      (is (= "step-1" (:step-id result)))
      (is (= "hello world" (get-in result [:result :message])))
      (is (nat-int? (:duration-ms result)))
      (is (map? (:context result))))))

(deftest execute-step-with-context-test
  (testing "Step uses provided context for substitution"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          ctx    {:step-1 {:success true :result {:message "step-1-output"}}}
          result (wf/execute-step engine wf "step-2" {:context ctx})]
      (is (true? (:success? result)))
      (is (= "after step-1: step-1-output" (get-in result [:result :message]))))))

(deftest execute-missing-step-test
  (testing "Missing step returns error"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          result (wf/execute-step engine wf "nonexistent" {})]
      (is (false? (:success? result)))
      (is (= "nonexistent" (:step-id result)))
      (is (seq (:errors result))))))

(deftest execute-noop-step-test
  (testing "Noop action returns success with empty result"
    (let [engine (make-engine)
          wf     (load-from-yaml engine multi-action-yaml "multi")
          result (wf/execute-step engine wf "noop-step" {})]
      (is (true? (:success? result)))
      (is (= {} (:result result))))))

(deftest execute-unknown-action-step-test
  (testing "Unknown action returns failure"
    (let [engine (make-engine)
          wf     (load-from-yaml engine multi-action-yaml "multi")
          result (wf/execute-step engine wf "unknown-action" {})]
      (is (false? (:success? result)))
      (is (some #(re-find #"Unknown action" %) (:errors result))))))

;;; ============================================================================
;;; Execute Workflow Tests
;;; ============================================================================

(deftest execute-simple-workflow-test
  (testing "Execute a complete simple workflow"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          result (wf/execute-workflow engine wf {})]
      (is (true? (:success? result)))
      (is (= (:workflow-id wf) (:workflow-id result)))
      (is (= 2 (:steps-completed result)))
      (is (= 2 (:steps-total result)))
      (is (nat-int? (:duration-ms result)))
      (is (empty? (:errors result)))
      (is (map? (:final-context result)))
      ;; Step 2 should have step 1's output in context
      (is (get-in result [:final-context :step-1 :result :message])))))

(deftest execute-workflow-step-outputs-propagate-test
  (testing "Step outputs propagate through context to later steps"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          result (wf/execute-workflow engine wf {})]
      (is (true? (:success? result)))
      ;; step-2 result should contain interpolated step-1 result
      (let [step-2-result (get-in result [:results "step-2" :result :message])]
        (is (string? step-2-result))
        (is (re-find #"hello world" step-2-result))))))

(deftest execute-parallel-workflow-test
  (testing "Parallel branches execute concurrently"
    (let [engine (make-engine)
          wf     (load-from-yaml engine parallel-workflow-yaml "parallel")
          result (wf/execute-workflow engine wf {})]
      (is (true? (:success? result)))
      ;; 1 setup + 2 parallel + 1 aggregate = 4 steps
      (is (= 4 (:steps-total result)))
      (is (= 4 (:steps-completed result)))
      ;; Both branches should be in results
      (is (get-in result [:results "branch-a"]))
      (is (get-in result [:results "branch-b"])))))

(deftest execute-conditional-workflow-test
  (testing "Conditional steps are skipped when condition is false"
    (let [engine (make-engine)
          wf     (load-from-yaml engine conditional-workflow-yaml "cond")
          result (wf/execute-workflow engine wf {})]
      (is (true? (:success? result)))
      ;; on-success should have executed (check.success = true)
      (let [success-step (get-in result [:results "on-success"])]
        (is (true? (:success? success-step)))
        (is (= "check passed" (get-in success-step [:result :message]))))
      ;; on-failure should have been skipped
      (let [failure-step (get-in result [:results "on-failure"])]
        (is (true? (:success? failure-step))) ;; skipped = success
        (is (true? (get-in failure-step [:result :skipped])))))))

(deftest execute-workflow-fail-fast-test
  (testing "Fail-fast stops on first failure"
    (let [engine (make-engine)
          wf     (load-from-yaml engine multi-action-yaml "multi")
          result (wf/execute-workflow engine wf {:fail-fast? true})]
      ;; unknown-action step should cause failure
      (is (false? (:success? result)))
      (is (seq (:errors result)))
      ;; Should have executed greet + noop + failed on unknown
      (is (= 3 (:steps-completed result))))))

(deftest execute-workflow-no-fail-fast-test
  (testing "Without fail-fast, all steps run even after failure"
    (let [engine (make-engine)
          wf     (load-from-yaml engine multi-action-yaml "multi")
          result (wf/execute-workflow engine wf {:fail-fast? false})]
      ;; Errors present but all steps ran
      (is (= 3 (:steps-completed result)))
      (is (seq (:errors result))))))

(deftest execute-workflow-on-step-complete-callback-test
  (testing "on-step-complete callback fires for each step"
    (let [engine     (make-engine)
          wf         (load-from-yaml engine simple-workflow-yaml "simple")
          completed  (atom [])
          result     (wf/execute-workflow engine wf
                                          {:on-step-complete
                                           (fn [step-result]
                                             (swap! completed conj (:step-id step-result)))})]
      (is (true? (:success? result)))
      (is (= 2 (count @completed)))
      (is (= "step-1" (first @completed))))))

(deftest execute-workflow-invalid-workflow-test
  (testing "Executing invalid workflow returns failure without throwing"
    (let [engine (make-engine)
          wf     (load-from-yaml engine bad-deps-yaml "bad")
          result (wf/execute-workflow engine wf {})]
      (is (false? (:success? result)))
      (is (seq (:errors result))))))

(deftest execute-empty-workflow-test
  (testing "Empty workflow succeeds with 0 steps"
    (let [engine (make-engine)
          wf     (load-from-yaml engine empty-workflow-yaml "empty")
          result (wf/execute-workflow engine wf {})]
      (is (true? (:success? result)))
      (is (zero? (:steps-completed result))))))

;;; ============================================================================
;;; Get Status Tests
;;; ============================================================================

(deftest get-status-after-execute-test
  (testing "Status is :completed after successful execution"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          _      (wf/execute-workflow engine wf {})
          status (wf/get-status engine (:workflow-id wf))]
      (is (some? status))
      (is (= :completed (:status status)))
      (is (= 1.0 (:progress status)))
      (is (some? (:started-at status)))
      (is (some? (:completed-at status))))))

(deftest get-status-unknown-workflow-test
  (testing "Unknown workflow-id returns nil"
    (let [engine (make-engine)
          status (wf/get-status engine "nonexistent-workflow-id")]
      (is (nil? status)))))

(deftest get-status-failed-workflow-test
  (testing "Status is :failed after failure with fail-fast"
    (let [engine (make-engine)
          wf     (load-from-yaml engine multi-action-yaml "multi")
          _      (wf/execute-workflow engine wf {:fail-fast? true})
          status (wf/get-status engine (:workflow-id wf))]
      (is (= :failed (:status status))))))

;;; ============================================================================
;;; Cancel Workflow Tests
;;; ============================================================================

(deftest cancel-completed-workflow-test
  (testing "Cancelling a completed workflow is idempotent"
    (let [engine (make-engine)
          wf     (load-from-yaml engine simple-workflow-yaml "simple")
          _      (wf/execute-workflow engine wf {})
          result (wf/cancel-workflow engine (:workflow-id wf)
                                     {:reason "testing"})]
      (is (true? (:success? result)))
      (is (= :completed (:status result))))))

(deftest cancel-unknown-workflow-test
  (testing "Cancelling unknown workflow returns error"
    (let [engine (make-engine)
          result (wf/cancel-workflow engine "nonexistent" {:reason "test"})]
      (is (false? (:success? result)))
      (is (= :unknown (:status result)))
      (is (seq (:errors result))))))

;;; ============================================================================
;;; Constructor Tests
;;; ============================================================================

(deftest create-engine-defaults-test
  (testing "Default engine creation"
    (let [engine (engine/create-yaml-engine)]
      (is (some? engine))
      (is (satisfies? wf/IWorkflowEngine engine)))))

(deftest create-engine-with-opts-test
  (testing "Engine creation with options"
    (let [engine (engine/create-yaml-engine {:workflow-dir "/tmp/workflows"})]
      (is (some? engine))
      (is (= "/tmp/workflows" (:workflow-dir engine))))))

;;; ============================================================================
;;; Shell Action Tests (Dry Run)
;;; ============================================================================

(deftest shell-action-dry-run-test
  (testing "Shell action in dry-run mode"
    (let [yaml-str "name: shell-test
version: '1.0'
params:
  target: world
steps:
  - id: run-cmd
    title: Run Command
    action: shell
    args:
      command: 'echo Hello {{target}}'"
          engine (make-engine)
          wf     (load-from-yaml engine yaml-str "shell")
          result (wf/execute-step engine wf "run-cmd" {:dry-run? true})]
      (is (true? (:success? result)))
      (is (true? (get-in result [:result :dry-run])))
      (is (= "echo Hello world" (get-in result [:result :command]))))))

;;; ============================================================================
;;; Variable Substitution Integration Tests
;;; ============================================================================

(deftest full-pipeline-substitution-test
  (testing "Variables propagate through a multi-step pipeline"
    (let [yaml-str "name: pipeline
version: '1.0'
params:
  prefix: hello
steps:
  - id: producer
    title: Produce
    action: echo
    args:
      data: '{{prefix}}-data'
  - id: consumer
    title: Consume
    action: echo
    depends-on:
      - producer
    args:
      received: '{{producer.result.data}}'"
          engine (make-engine)
          wf     (load-from-yaml engine yaml-str "pipeline")
          result (wf/execute-workflow engine wf {})]
      (is (true? (:success? result)))
      (is (= "hello-data"
             (get-in result [:results "consumer" :result :received]))))))

;;; ============================================================================
;;; MCP-Call Action Tests
;;; ============================================================================

(deftest mcp-call-basic-test
  (testing "mcp-call invokes a mock tool handler and returns result"
    ;; Register a mock tool in hive-mcp.tools/tools for get-tool-by-name
    (let [call-log (atom [])
          mock-handler (fn [args]
                         (swap! call-log conj args)
                         {:type "text" :text (str "called with command=" (:command args))})
          ;; Temporarily override get-tool-by-name via with-redefs
          result (with-redefs [tools/get-tool-by-name
                               (fn [name]
                                 (when (= name "mock-tool")
                                   {:name "mock-tool"
                                    :handler mock-handler}))]
                   (engine/execute-action :mcp-call
                                          {:tool "mock-tool" :command "status"}
                                          {}
                                          {}))]
      (is (true? (:success? result)))
      (is (some? (:result result)))
      ;; Handler was called exactly once
      (is (= 1 (count @call-log)))
      ;; Command was passed through
      (is (= "status" (:command (first @call-log)))))))

(deftest mcp-call-with-params-test
  (testing "mcp-call passes extra params to tool handler"
    (let [captured (atom nil)
          mock-handler (fn [args]
                         (reset! captured args)
                         {:type "text" :text "ok"})
          result (with-redefs [tools/get-tool-by-name
                               (fn [name]
                                 (when (= name "memory")
                                   {:name "memory" :handler mock-handler}))]
                   (engine/execute-action :mcp-call
                                          {:tool "memory"
                                           :command "query"
                                           :params {:type "axiom" :limit 5}}
                                          {}
                                          {}))]
      (is (true? (:success? result)))
      ;; Params were merged into handler args
      (is (= "query" (:command @captured)))
      (is (= "axiom" (:type @captured)))
      (is (= 5 (:limit @captured))))))

(deftest mcp-call-variable-substitution-test
  (testing "mcp-call substitutes {{vars}} in tool name, command, and params"
    (let [captured (atom nil)
          mock-handler (fn [args]
                         (reset! captured args)
                         {:type "text" :text "ok"})
          ctx {:tool-name "session"
               :cmd "whoami"
               :my-dir "/home/test"}
          result (with-redefs [tools/get-tool-by-name
                               (fn [name]
                                 (when (= name "session")
                                   {:name "session" :handler mock-handler}))]
                   (engine/execute-action :mcp-call
                                          {:tool "{{tool-name}}"
                                           :command "{{cmd}}"
                                           :params {:directory "{{my-dir}}"}}
                                          ctx
                                          {}))]
      (is (true? (:success? result)))
      (is (= "whoami" (:command @captured)))
      (is (= "/home/test" (:directory @captured))))))

(deftest mcp-call-unknown-tool-test
  (testing "mcp-call returns error for unknown tool (CLARITY-Y)"
    (let [result (with-redefs [tools/get-tool-by-name
                               (fn [_] nil)]
                   (engine/execute-action :mcp-call
                                          {:tool "nonexistent-tool"
                                           :command "status"}
                                          {}
                                          {}))]
      (is (false? (:success? result)))
      (is (nil? (:result result)))
      (is (some #(re-find #"Tool not found" %) (:errors result))))))

(deftest mcp-call-missing-tool-arg-test
  (testing "mcp-call returns error when :tool is missing"
    (let [result (engine/execute-action :mcp-call
                                        {:command "status"}
                                        {}
                                        {})]
      (is (false? (:success? result)))
      (is (some #(re-find #"requires :tool" %) (:errors result))))))

(deftest mcp-call-handler-exception-test
  (testing "mcp-call catches handler exceptions (CLARITY-Y: never throws)"
    (let [result (with-redefs [tools/get-tool-by-name
                               (fn [name]
                                 (when (= name "broken")
                                   {:name "broken"
                                    :handler (fn [_] (throw (ex-info "boom" {})))}))]
                   (engine/execute-action :mcp-call
                                          {:tool "broken" :command "explode"}
                                          {}
                                          {}))]
      (is (false? (:success? result)))
      (is (some #(re-find #"mcp-call error.*boom" %) (:errors result))))))

(deftest mcp-call-context-passthrough-test
  (testing "mcp-call passes directory and agent_id from context"
    (let [captured (atom nil)
          mock-handler (fn [args]
                         (reset! captured args)
                         {:type "text" :text "ok"})
          ctx {:directory "/home/user/project"
               :agent_id "swarm-worker-123"}
          result (with-redefs [tools/get-tool-by-name
                               (fn [name]
                                 (when (= name "session")
                                   {:name "session" :handler mock-handler}))]
                   (engine/execute-action :mcp-call
                                          {:tool "session" :command "whoami"}
                                          ctx
                                          {}))]
      (is (true? (:success? result)))
      (is (= "/home/user/project" (:directory @captured)))
      (is (= "swarm-worker-123" (:agent_id @captured))))))

(deftest mcp-call-in-workflow-test
  (testing "mcp-call works as a step within a full YAML workflow"
    (let [captured (atom nil)
          mock-handler (fn [args]
                         (reset! captured args)
                         {:type "text" :text "catchup-data"})
          yaml-str "name: mcp-call-test
version: '1.0'
params:
  project_dir: /home/test
steps:
  - id: setup
    title: Setup
    action: echo
    args:
      info: starting
  - id: call-tool
    title: Call MCP Tool
    action: mcp-call
    depends-on:
      - setup
    args:
      tool: session
      command: whoami
      params:
        directory: '{{project_dir}}'"
          engine (make-engine)
          wf     (load-from-yaml engine yaml-str "mcp-call-test")
          result (with-redefs [tools/get-tool-by-name
                               (fn [name]
                                 (when (= name "session")
                                   {:name "session" :handler mock-handler}))]
                   (wf/execute-workflow engine wf {}))]
      (is (true? (:success? result)))
      (is (= 2 (:steps-completed result)))
      ;; Tool was called
      (is (some? @captured))
      (is (= "whoami" (:command @captured)))
      (is (= "/home/test" (:directory @captured))))))

(deftest mcp-call-no-command-test
  (testing "mcp-call works without command (some tools don't need it)"
    (let [captured (atom nil)
          mock-handler (fn [args]
                         (reset! captured args)
                         {:type "text" :text "ok"})
          result (with-redefs [tools/get-tool-by-name
                               (fn [name]
                                 (when (= name "simple-tool")
                                   {:name "simple-tool" :handler mock-handler}))]
                   (engine/execute-action :mcp-call
                                          {:tool "simple-tool"
                                           :params {:key "value"}}
                                          {}
                                          {}))]
      (is (true? (:success? result)))
      ;; No :command key in captured args
      (is (nil? (:command @captured)))
      (is (= "value" (:key @captured))))))
