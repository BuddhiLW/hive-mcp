(ns hive-mcp.agent.drone.execution-agentic-test
  "Integration tests for in-process agentic drone execution pipeline.

   Validates the full pipeline:
   - delegate-agentic! → run-agentic-execution! → phase:execute-agentic! → agentic-loop
   - Session KG (Datalevin) creation, recording, cleanup
   - Fallback to DataScript in-memory KG when Datalevin unavailable
   - phase:execute-agentic! binds *drone-kg-store* and runs agentic loop
   - Tool registry initialization
   - LLM backend creation

   All tests use mock LLMBackend to avoid external API calls."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.agent.drone.execution :as execution]
            [hive-mcp.agent.drone.augment :as augment]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.kg-factory :as kg-factory]
            [hive-mcp.agent.drone.loop :as agentic-loop]
            [hive-mcp.agent.drone.tool-allowlist :as allowlist]
            [hive-mcp.agent.drone.diff-mgmt :as diff-mgmt]
            [hive-mcp.agent.config :as agent-config]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.events.core :as ev]
            [hive-mcp.hivemind :as hivemind]))

;;; ============================================================
;;; Mock Backends
;;; ============================================================

(defn mock-backend
  "Create a mock LLMBackend that returns predefined responses in sequence."
  [model responses]
  (let [call-counter (atom 0)]
    (reify proto/LLMBackend
      (chat [_ _messages _tools]
        (let [idx @call-counter
              response (get responses idx (last responses))]
          (swap! call-counter inc)
          response))
      (model-name [_] model))))

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

(deftest phase-execute-agentic-binds-kg-store
  (testing "phase:execute-agentic! binds *drone-kg-store* during agentic loop"
    (let [drone-store (kg-factory/create-drone-store "drone-agentic-bind-test")
          captured-store (atom nil)
          ctx (domain/->execution-context
               {:drone-id "drone-agentic-bind-test"
                :task-id "task-agentic-bind"
                :parent-id nil
                :project-root "/tmp"
                :kg-store drone-store})
          task-spec (domain/->task-spec {:task "test agentic binding"
                                         :files []})
          config {:tools [] :preset nil :model "test-model" :step-budget 1}
          ;; Create a mock backend that captures the dynamic var
          mock-be (mock-backend "test-model" [{:type :text :content "Done"}])]
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
                    ;; Intercept the backend creation to use our mock
                    ;; and capture the kg-store binding
                    agentic-loop/run-agentic-loop
                    (fn [_task-spec _ctx _opts]
                      (reset! captured-store domain/*drone-kg-store*)
                      {:status :completed :result "mocked"
                       :steps [] :tool_calls_made 0
                       :tokens {} :turns 1 :model "test-model"
                       :kg-stats {}})]
        (execution/phase:execute-agentic! ctx task-spec config))
      ;; Verify the store was bound
      (is (identical? drone-store @captured-store)
          "*drone-kg-store* should be bound to ctx's :kg-store during agentic loop")
      ;; After execution, binding should be unwound
      (is (nil? domain/*drone-kg-store*)))))

(deftest phase-execute-agentic-passes-config-to-loop
  (testing "phase:execute-agentic! passes correct params to run-agentic-loop"
    (let [captured-args (atom nil)
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
                    agentic-loop/run-agentic-loop
                    (fn [task-map ctx-map opts-map]
                      (reset! captured-args {:task-map task-map
                                             :ctx-map ctx-map
                                             :opts-map opts-map})
                      {:status :completed :result "ok"
                       :steps [] :tool_calls_made 0
                       :tokens {} :turns 1 :model "test-model"
                       :kg-stats {}})]
        (execution/phase:execute-agentic! ctx task-spec config))
      ;; Verify args
      (let [{:keys [task-map ctx-map opts-map]} @captured-args]
        ;; Task map
        (is (string? (:task task-map)))
        (is (= ["a.clj"] (:files task-map)))
        (is (= "/project" (:cwd task-map)))
        ;; Context map
        (is (= "drone-cfg-test" (:drone-id ctx-map)))
        ;; Options map
        (is (= 5 (:max-turns opts-map)))
        (is (= "drone-cfg-test" (:agent-id opts-map)))))))

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
  (testing "E2E: phase:execute-agentic! with mock backend and real session KG"
    (let [drone-id (str "e2e-test-" (System/currentTimeMillis))
          session-store (try
                          (session-kg/create-session-kg! drone-id)
                          (catch Exception _
                            ;; Fall back to DataScript if Datalevin unavailable
                            (kg-factory/create-drone-store drone-id)))]
      (if (nil? session-store)
        (println "SKIP: Neither Datalevin nor DataScript available for E2E test")
        (try
          (let [ctx (domain/->execution-context
                     {:drone-id drone-id
                      :task-id (str "task-" drone-id)
                      :parent-id nil
                      :project-root "/tmp"
                      :kg-store session-store})
                task-spec (domain/->task-spec {:task "Fix the nil check"
                                               :files []})
                config {:tools [] :preset nil :model "mock-model" :step-budget 3}
                ;; Mock backend: tool call then completion
                mock-be (mock-backend "mock-model"
                                      [{:type :tool_calls
                                        :calls [{:id "c1" :name "read_file"
                                                 :arguments {:path "src/core.clj"}}]}
                                       {:type :text :content "I've fixed the nil check."}])]
            ;; Stub only external I/O, let agentic loop + session KG run for real
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
                          ;; Override backend creation to use our mock
                          ;; We use the resolve pattern from the production code
                          agent-config/openrouter-backend
                          (fn [_opts] mock-be)
                          ;; Resolve allowlist to empty (no tools needed for mock)
                          allowlist/resolve-allowlist
                          (fn [_opts] #{"read_file"})]
              (let [result (execution/phase:execute-agentic! ctx task-spec config)]
                ;; Verify loop completed
                (is (= :completed (:status result))
                    "Agentic loop should complete successfully")
                ;; Should have run 2 turns (tool call + text)
                (is (= 2 (:turns result))
                    "Should have 2 turns: tool call then text completion")
                ;; Verify KG stats
                (is (map? (:kg-stats result)))
                (when (:kg-stats result)
                  (is (pos? (:reasoning (:kg-stats result)))
                      "Should have reasoning entries in KG")))))
          (finally
            (try
              (session-kg/close-session-kg! session-store drone-id)
              (catch Exception _
                (try (kg-factory/close-drone-store! drone-id) (catch Exception _ nil))))))))))

(deftest e2e-agentic-loop-records-in-session-kg
  (testing "E2E: Agentic loop records observations and reasoning in Datalevin session KG"
    (let [drone-id (str "e2e-kg-record-" (System/currentTimeMillis))
          session-store (try
                          (session-kg/create-session-kg! drone-id)
                          (catch Exception _ nil))]
      (if (nil? session-store)
        (println "SKIP: Datalevin not available for session KG recording test")
        (try
          (let [ctx (domain/->execution-context
                     {:drone-id drone-id
                      :task-id (str "task-" drone-id)
                      :parent-id nil
                      :project-root "/tmp"
                      :kg-store session-store})
                task-spec (domain/->task-spec {:task "Refactor the parser"
                                               :files ["src/parser.clj"]})
                config {:tools [] :preset nil :model "mock-model" :step-budget 5}
                mock-be (mock-backend "mock-model"
                                      [{:type :tool_calls
                                        :calls [{:id "c1" :name "read_file"
                                                 :arguments {:path "src/parser.clj"}}]}
                                       {:type :tool_calls
                                        :calls [{:id "c2" :name "grep"
                                                 :arguments {:pattern "defn parse"}}]}
                                       {:type :text :content "Refactoring complete."}])]
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
                          agent-config/openrouter-backend
                          (fn [_opts] mock-be)
                          allowlist/resolve-allowlist
                          (fn [_opts] #{"read_file" "grep"})]
              (let [result (execution/phase:execute-agentic! ctx task-spec config)]
                ;; Loop should complete in 3 turns
                (is (= :completed (:status result)))
                (is (= 3 (:turns result)))
                ;; Query observations from session KG
                (let [obs (try
                            (kg/query session-store
                                      '[:find ?tool
                                        :where
                                        [?e :obs/tool ?tool]])
                            (catch Exception _ #{}))]
                  (is (pos? (count obs))
                      "Should have observation nodes in session KG"))
                ;; Query reasoning entries
                (let [reasons (try
                                (kg/query session-store
                                          '[:find ?intent
                                            :where
                                            [?e :reason/intent ?intent]])
                                (catch Exception _ #{}))]
                  (is (pos? (count reasons))
                      "Should have reasoning nodes in session KG")))))
          (finally
            (session-kg/close-session-kg! session-store drone-id)))))))

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
