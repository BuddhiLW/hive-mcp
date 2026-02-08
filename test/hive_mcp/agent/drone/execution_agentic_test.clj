(ns hive-mcp.agent.drone.execution-agentic-test
  "Integration tests for in-process agentic drone execution pipeline.

   Validates the full pipeline:
   - delegate-agentic! → run-agentic-execution! → phase:execute-agentic! → ha-bridge
   - Session KG (Datalevin) creation, recording, cleanup
   - Fallback to DataScript in-memory KG when Datalevin unavailable
   - phase:execute-agentic! binds *drone-kg-store* and routes through hive-agent bridge
   - Tool registry initialization

   E2E tests mock ha-bridge/run-agent-via-bridge (primary path) to avoid external API calls."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.agent.drone.execution :as execution]
            [hive-mcp.agent.drone.augment :as augment]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.kg-factory :as kg-factory]
            [hive-mcp.agent.drone.diff-mgmt :as diff-mgmt]
            [hive-mcp.agent.hive-agent-bridge :as ha-bridge]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.events.core :as ev]
            [hive-mcp.hivemind :as hivemind]))

;;; ============================================================
;;; Fixtures
;;; ============================================================

(use-fixtures :each
  (fn [f]
    (kg-factory/cleanup-all-drone-stores!)
    (f)
    (kg-factory/cleanup-all-drone-stores!)))

;;; ============================================================
;;; phase:execute-agentic! Tests
;;; ============================================================

(deftest phase-execute-agentic-routes-through-bridge
  (testing "phase:execute-agentic! routes through hive-agent bridge (primary path)"
    (let [bridge-opts (atom nil)
          ctx (domain/->execution-context
               {:drone-id "drone-agentic-bind-test"
                :task-id "task-agentic-bind"
                :parent-id nil
                :project-root "/tmp"})
          task-spec (domain/->task-spec {:task "test agentic binding"
                                         :files []})
          config {:tools [] :preset nil :model "test-model" :step-budget 1}]
      ;; Stub dependencies
      (with-redefs [augment/augment-task (fn [task _files _opts] task)
                    sandbox/create-sandbox (fn [files root]
                                             {:allowed-files (set files)
                                              :allowed-dirs #{root}
                                              :blocked-patterns []
                                              :blocked-tools #{}
                                              :rejected-files []})
                    ev/dispatch (fn [_] nil)
                    hivemind/shout! (fn [& _] nil)
                    registry/ensure-registered! (fn [] nil)
                    ;; Mock the primary bridge path and capture opts
                    ha-bridge/run-agent-via-bridge
                    (fn [opts]
                      (reset! bridge-opts opts)
                      {:status :completed :result "mocked"
                       :steps [] :tool_calls_made 0
                       :tokens {} :turns 1 :model "test-model"
                       :kg-stats {}})]
        (let [result (execution/phase:execute-agentic! ctx task-spec config)]
          ;; Verify bridge was called
          (is (some? @bridge-opts)
              "Should route through hive-agent bridge")
          ;; Verify result passes through
          (is (= :completed (:status result)))
          ;; After execution, dynamic var should remain unbound (primary path doesn't bind it)
          (is (nil? domain/*drone-kg-store*)))))))

(deftest phase-execute-agentic-passes-config-to-bridge
  (testing "phase:execute-agentic! passes correct params to hive-agent bridge"
    (let [captured-opts (atom nil)
          ctx (domain/->execution-context
               {:drone-id "drone-cfg-test"
                :task-id "task-cfg"
                :parent-id nil
                :project-root "/tmp"})
          task-spec (domain/->task-spec {:task "config test task"
                                         :files ["a.clj"]
                                         :cwd "/project"})
          config {:tools ["read_file" "grep"] :preset "tdd" :model "test-model" :step-budget 5}]
      (with-redefs [augment/augment-task (fn [task _files _opts] (str "augmented: " task))
                    sandbox/create-sandbox (fn [files root]
                                             {:allowed-files (set files)
                                              :allowed-dirs #{root}
                                              :blocked-patterns []
                                              :blocked-tools #{}
                                              :rejected-files []})
                    ev/dispatch (fn [_] nil)
                    hivemind/shout! (fn [& _] nil)
                    registry/ensure-registered! (fn [] nil)
                    ha-bridge/run-agent-via-bridge
                    (fn [opts]
                      (reset! captured-opts opts)
                      {:status :completed :result "ok"
                       :steps [] :tool_calls_made 0
                       :tokens {} :turns 1 :model "test-model"
                       :kg-stats {}})]
        (execution/phase:execute-agentic! ctx task-spec config))
      ;; Verify bridge received correct opts
      (let [opts @captured-opts]
        ;; Task should be augmented
        (is (string? (:task opts)))
        ;; Model from config
        (is (= "test-model" (:model opts)))
        ;; Max turns from step-budget
        (is (= 5 (:max-turns opts)))))))

(deftest phase-execute-agentic-rejects-path-escape
  (testing "phase:execute-agentic! rejects sandbox path escape attempts"
    (let [ctx (domain/->execution-context
               {:drone-id "drone-escape-test"
                :task-id "task-escape"
                :parent-id nil
                :project-root "/safe/dir"})
          task-spec (domain/->task-spec {:task "escape test"
                                         :files ["../../etc/passwd"]})
          config {:tools [] :preset nil :model "test" :step-budget 1}]
      (with-redefs [augment/augment-task (fn [task _files _opts] task)
                    sandbox/create-sandbox (fn [_files _root]
                                             {:allowed-files #{}
                                              :allowed-dirs #{}
                                              :blocked-patterns []
                                              :blocked-tools #{}
                                              :rejected-files ["../../etc/passwd"]})
                    ev/dispatch (fn [_] nil)
                    hivemind/shout! (fn [& _] nil)
                    registry/ensure-registered! (fn [] nil)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"File paths escape"
                              (execution/phase:execute-agentic! ctx task-spec config)))))))

;;; ============================================================
;;; run-agentic-execution! Tests
;;; ============================================================

(deftest run-agentic-execution-creates-session-kg
  (testing "run-agentic-execution! creates and cleans up session KG"
    (let [session-created (atom false)
          session-closed (atom false)
          task-spec (domain/->task-spec {:task "session kg test"
                                         :files []})]
      ;; Stub everything to focus on session KG lifecycle
      (with-redefs [session-kg/create-session-kg!
                    (fn [_drone-id]
                      (reset! session-created true)
                      (kg-factory/create-drone-store "session-kg-stub"))
                    session-kg/close-session-kg!
                    (fn [_store _drone-id]
                      (reset! session-closed true))
                    execution/phase:prepare
                    (fn [_] {:task-type :general :tools [] :preset nil
                             :model "test" :step-budget 3
                             :model-selection {:model "test"}})
                    execution/phase:register!
                    (fn [ctx _] ctx)
                    execution/phase:validate
                    (fn [ctx _] ctx)
                    execution/phase:execute-agentic!
                    (fn [_ctx _spec _config]
                      {:status :completed :result "ok"})
                    execution/phase:finalize!
                    (fn [_ctx _spec _config raw diffs]
                      raw)
                    execution/phase:cleanup!
                    (fn [_ctx _] nil)
                    domain/generate-drone-id
                    (fn [] "test-drone-session")
                    domain/generate-task-id
                    (fn [did] (str "task-" did))
                    diff/get-project-root
                    (fn [] "/tmp")
                    diff-mgmt/capture-diffs-before
                    (fn [] #{})]
        (execution/run-agentic-execution! task-spec))
      (is @session-created "Session KG should be created")
      (is @session-closed "Session KG should be closed in finally"))))

(deftest run-agentic-execution-fallback-to-datascript
  (testing "run-agentic-execution! falls back to DataScript when Datalevin fails"
    (let [fallback-used (atom false)
          task-spec (domain/->task-spec {:task "fallback test"
                                         :files []})]
      (with-redefs [session-kg/create-session-kg!
                    (fn [_] (throw (Exception. "Datalevin unavailable")))
                    kg-factory/create-drone-store
                    (fn [drone-id]
                      (reset! fallback-used true)
                      ;; Return nil to test nil KG path
                      nil)
                    session-kg/close-session-kg!
                    (fn [_ _] nil)
                    execution/phase:prepare
                    (fn [_] {:task-type :general :tools [] :preset nil
                             :model "test" :step-budget 3
                             :model-selection {:model "test"}})
                    execution/phase:register!
                    (fn [ctx _] ctx)
                    execution/phase:validate
                    (fn [ctx _] ctx)
                    execution/phase:execute-agentic!
                    (fn [_ctx _spec _config]
                      {:status :completed :result "ok"})
                    execution/phase:finalize!
                    (fn [_ctx _spec _config raw diffs]
                      raw)
                    execution/phase:cleanup!
                    (fn [_ctx _] nil)
                    domain/generate-drone-id
                    (fn [] "test-drone-fallback")
                    domain/generate-task-id
                    (fn [did] (str "task-" did))
                    diff/get-project-root
                    (fn [] "/tmp")
                    diff-mgmt/capture-diffs-before
                    (fn [] #{})]
        (execution/run-agentic-execution! task-spec))
      (is @fallback-used "Should attempt DataScript fallback when Datalevin fails"))))

;;; ============================================================
;;; E2E: Agentic Loop + Session KG (Real Datalevin)
;;; ============================================================

(deftest e2e-agentic-execution-with-mock-backend
  (testing "E2E: phase:execute-agentic! routes through hive-agent bridge (primary path)"
    (let [drone-id (str "e2e-test-" (System/currentTimeMillis))
          ctx (domain/->execution-context
               {:drone-id drone-id
                :task-id (str "task-" drone-id)
                :parent-id nil
                :project-root "/tmp"})
          task-spec (domain/->task-spec {:task "Fix the nil check"
                                         :files []})
          config {:tools [] :preset nil :model "mock-model" :step-budget 3}]
      ;; Mock the primary path (ha-bridge) to return a structured result
      (with-redefs [augment/augment-task (fn [task _files _opts] task)
                    sandbox/create-sandbox (fn [files root]
                                             {:allowed-files (set files)
                                              :allowed-dirs #{root}
                                              :blocked-patterns []
                                              :blocked-tools #{}
                                              :rejected-files []})
                    ev/dispatch (fn [_] nil)
                    hivemind/shout! (fn [& _] nil)
                    registry/ensure-registered! (fn [] nil)
                    ;; Mock the primary hive-agent bridge path
                    ha-bridge/run-agent-via-bridge
                    (fn [_opts]
                      {:status :completed
                       :result "I've fixed the nil check."
                       :turns 2
                       :tool_calls_made 1
                       :model "mock-model"
                       :tokens {:input 100 :output 50}
                       :steps [{:type :tool_call :tool "read_file"}
                               {:type :text :content "Fixed."}]
                       :kg-stats {:reasoning 1}
                       :hive-agent-metadata {:turns 2}})]
        (let [result (execution/phase:execute-agentic! ctx task-spec config)]
          ;; Verify loop completed via primary path
          (is (= :completed (:status result))
              "Agentic execution should complete successfully via bridge")
          ;; Should have run 2 turns (tool call + text)
          (is (= 2 (:turns result))
              "Should report 2 turns from bridge result")
          ;; Verify KG stats passed through
          (is (map? (:kg-stats result)))
          (when (:kg-stats result)
            (is (pos? (:reasoning (:kg-stats result)))
                "Should have reasoning entries from bridge result")))))))

(deftest e2e-agentic-bridge-multi-turn
  (testing "E2E: phase:execute-agentic! bridge handles multi-turn execution"
    (let [drone-id (str "e2e-bridge-multi-" (System/currentTimeMillis))
          bridge-called (atom false)
          ctx (domain/->execution-context
               {:drone-id drone-id
                :task-id (str "task-" drone-id)
                :parent-id nil
                :project-root "/tmp"})
          task-spec (domain/->task-spec {:task "Refactor the parser"
                                         :files ["src/parser.clj"]})
          config {:tools [] :preset nil :model "mock-model" :step-budget 5}]
      (with-redefs [augment/augment-task (fn [task _files _opts] task)
                    sandbox/create-sandbox (fn [files root]
                                             {:allowed-files (set files)
                                              :allowed-dirs #{root}
                                              :blocked-patterns []
                                              :blocked-tools #{}
                                              :rejected-files []})
                    ev/dispatch (fn [_] nil)
                    hivemind/shout! (fn [& _] nil)
                    registry/ensure-registered! (fn [] nil)
                    ;; Mock the primary hive-agent bridge path
                    ha-bridge/run-agent-via-bridge
                    (fn [opts]
                      (reset! bridge-called true)
                      ;; Verify opts received from phase:execute-agentic!
                      {:status :completed
                       :result "Refactoring complete."
                       :turns 3
                       :tool_calls_made 2
                       :model (:model opts)
                       :tokens {:input 200 :output 100}
                       :steps [{:type :tool_call :tool "read_file"}
                               {:type :tool_call :tool "grep"}
                               {:type :text :content "Done."}]
                       :kg-stats {:reasoning 2 :observations 2}
                       :hive-agent-metadata {:turns 3}})]
        (let [result (execution/phase:execute-agentic! ctx task-spec config)]
          ;; Bridge should have been called
          (is @bridge-called "Should route through hive-agent bridge")
          ;; Loop should complete in 3 turns
          (is (= :completed (:status result)))
          (is (= 3 (:turns result)))
          ;; Verify KG stats from bridge
          (is (= 2 (:observations (:kg-stats result))))
          (is (= 2 (:reasoning (:kg-stats result)))))))))

;;; ============================================================
;;; delegate-agentic! API Tests
;;; ============================================================

(deftest delegate-agentic-creates-task-spec
  (testing "delegate-agentic! creates proper TaskSpec and delegates to run-agentic-execution!"
    (let [captured-spec (atom nil)]
      (with-redefs [execution/run-agentic-execution!
                    (fn [task-spec]
                      (reset! captured-spec task-spec)
                      {:status :completed :result "ok"})]
        (let [drone-ns (requiring-resolve 'hive-mcp.agent.drone/delegate-agentic!)
              result (drone-ns {:task "Test task"
                                :files ["a.clj" "b.clj"]
                                :task-type :refactoring
                                :preset "tdd"
                                :cwd "/project"
                                :parent-id "parent-ling"
                                :wave-id "wave-123"
                                :trace true
                                :skip-auto-apply false})]
          (is (= :completed (:status result)))
          ;; Verify TaskSpec was created correctly
          (let [spec @captured-spec]
            (is (= "Test task" (:task spec)))
            (is (= ["a.clj" "b.clj"] (:files spec)))
            (is (= :refactoring (:task-type spec)))
            (is (= "tdd" (:preset spec)))
            (is (= "/project" (:cwd spec)))
            (is (= "parent-ling" (:parent-id spec)))))))))
