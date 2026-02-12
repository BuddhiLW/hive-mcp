(ns hive-mcp.agent.ling.headless-e2e-test
  "End-to-end integration tests for headless ling lifecycle with OpenRouter support.

   Tests the full ILingStrategy pattern from the Ling facade down:
   1. Strategy resolution — correct strategy for spawn-mode/model combos
   2. HeadlessStrategy E2E — real subprocess spawn with `echo`, ring buffer capture
   3. OpenRouterStrategy — HTTP request formatting (mocked HTTP)
   4. Stdin dispatch — send input to headless lings via stdin pipe
   5. Ring buffer — output capture and retrieval

   Uses with-redefs to mock external calls (ProcessBuilder for claude CLI, HTTP for OpenRouter).
   Uses real `echo` command for subprocess tests (safe, no side effects)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.headless-strategy :as headless-strat]
            [hive-mcp.agent.ling.openrouter-strategy :as or-strat]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.headless-sdk :as sdk]
            [hive-mcp.agent.context-envelope :as envelope]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.swarm.datascript.schema :as schema]
            [clojure.core.async :as async]
            [clojure.string :as str]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-all-state [f]
  "Reset DataScript, context-store, kill headless processes, SDK sessions, and OpenRouter sessions between tests."
  (ds/reset-conn!)
  (context-store/reset-all!)
  (headless/kill-all-headless!)
  (sdk/kill-all-sdk!)
  (or-strat/kill-all-openrouter!)
  (f)
  (headless/kill-all-headless!)
  (sdk/kill-all-sdk!)
  (or-strat/kill-all-openrouter!)
  (context-store/reset-all!)
  (ds/reset-conn!))

(use-fixtures :each reset-all-state)

(defn gen-id
  "Generate unique test ling ID."
  [prefix]
  (str prefix "-" (System/nanoTime)))

;; =============================================================================
;; Section 1: Strategy Resolution via ->ling Factory
;;
;; resolve-strategy is defn- (private), so we test it indirectly through
;; the ->ling factory which sets spawn-mode, and through the Ling facade
;; which calls resolve-strategy internally.
;; =============================================================================

(deftest ling-factory-headless-mode
  (testing "->ling with :headless spawn-mode maps to :agent-sdk (since 0.12.0)"
    (let [ling (ling/->ling "test-hl-001"
                            {:cwd "/tmp"
                             :project-id "test"
                             :spawn-mode :headless})]
      ;; Since 0.12.0, :headless maps to :agent-sdk (default headless mechanism)
      (is (= :agent-sdk (:spawn-mode ling))
          ":headless should map to :agent-sdk")
      (is (nil? (:model ling))
          "model should be nil when not specified"))))

(deftest ling-factory-vterm-mode-default
  (testing "->ling defaults to :vterm spawn-mode"
    (let [ling (ling/->ling "test-vt-001" {:cwd "/tmp" :project-id "test"})]
      (is (= :vterm (:spawn-mode ling))
          "Default spawn-mode should be :vterm"))))

(deftest ling-factory-openrouter-for-deepseek
  (testing "->ling with deepseek model returns :openrouter spawn-mode"
    (let [ling (ling/->ling "test-ds-001"
                            {:cwd "/tmp"
                             :project-id "test"
                             :model "deepseek/deepseek-v3.2"})]
      (is (= :openrouter (:spawn-mode ling))
          "Non-claude model should force :openrouter")
      (is (= "deepseek/deepseek-v3.2" (:model ling))
          "Model should be preserved"))))

(deftest ling-factory-openrouter-for-various-models
  (testing "->ling correctly routes various non-Claude model IDs to :openrouter"
    (doseq [model ["deepseek/deepseek-v3.2"
                   "meta-llama/llama-3.3-70b"
                   "mistralai/mistral-large"
                   "google/gemini-pro"
                   "openai/gpt-4-turbo"]]
      (let [ling (ling/->ling "test-multi"
                              {:cwd "/tmp" :model model})]
        (is (= :openrouter (:spawn-mode ling))
            (str model " should route to :openrouter"))))))

(deftest ling-factory-claude-model-stays-vterm
  (testing "->ling with 'claude' model stays :vterm"
    (let [ling (ling/->ling "test-cl-001"
                            {:cwd "/tmp"
                             :project-id "test"
                             :model "claude"})]
      (is (= :vterm (:spawn-mode ling))
          "'claude' model should use :vterm")
      (is (= "claude" (:model ling))))))

(deftest ling-factory-nil-model-stays-vterm
  (testing "->ling with nil model stays :vterm"
    (let [ling (ling/->ling "test-nil-001"
                            {:cwd "/tmp" :project-id "test"})]
      (is (= :vterm (:spawn-mode ling)))
      (is (nil? (:model ling))))))

(deftest ling-factory-anthropic-model-stays-vterm
  (testing "->ling with 'anthropic/claude-*' model stays :vterm (uses Claude Code CLI)"
    (doseq [model ["anthropic/claude-sonnet-4"
                   "anthropic/claude-3-5-sonnet"
                   "anthropic/claude-3-opus"]]
      (let [ling (ling/->ling (str "test-ant-" (hash model))
                              {:cwd "/tmp" :project-id "test" :model model})]
        (is (= :vterm (:spawn-mode ling))
            (str model " should route to :vterm (it's a claude model)"))
        (is (= model (:model ling)))))))

(deftest claude-model-predicate
  (testing "schema/claude-model? correctly identifies Claude vs non-Claude"
    (is (true? (schema/claude-model? nil))
        "nil should be claude model")
    (is (true? (schema/claude-model? "claude"))
        "'claude' should be claude model")
    (is (true? (schema/claude-model? "anthropic/claude-sonnet-4"))
        "'anthropic/claude-sonnet-4' should be claude model (prefix match)")
    (is (true? (schema/claude-model? "anthropic/claude-3-5-sonnet"))
        "'anthropic/claude-3-5-sonnet' should be claude model (prefix match)")
    (is (false? (schema/claude-model? "deepseek/deepseek-v3.2"))
        "deepseek should not be claude model")
    (is (false? (schema/claude-model? "meta-llama/llama-3.3-70b"))
        "llama should not be claude model")
    (is (false? (schema/claude-model? ""))
        "empty string should not be claude model")))

;; =============================================================================
;; Section 2: HeadlessStrategy Spawn — Real Subprocess with `echo`
;;
;; This tests the ACTUAL ProcessBuilder path. We use `echo hello` as a safe
;; command instead of `claude`. The headless module supports a :claude-cmd
;; override specifically for testing.
;; =============================================================================

(deftest headless-spawn-real-subprocess-echo
  (testing "spawn-headless! creates a real subprocess using echo command"
    (let [ling-id (gen-id "echo-ling")
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "echo"
                                            :task "hello world"
                                            :buffer-capacity 100})]
      (is (map? result) "Should return a result map")
      (is (= ling-id (:ling-id result)) "Should return the ling-id")
      (is (pos? (:pid result)) "Should have a positive PID")
      (is (some? (:process result)) "Should have a Process handle")
      (is (some? (:stdout-buf result)) "Should have stdout buffer")
      (is (some? (:stderr-buf result)) "Should have stderr buffer")

      ;; Wait for process to complete (echo is instant)
      (let [^java.lang.Process proc (:process result)]
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))

      ;; Verify process is tracked in registry
      (is (headless/headless? ling-id)
          "Should be registered in process registry")

      ;; Check headless status
      (let [status (headless/headless-status ling-id)]
        (is (some? status) "Status should be available")
        (is (= ling-id (:ling-id status)))
        (is (= (:pid result) (:pid status))))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest headless-spawn-stdout-ring-buffer-captures-output
  (testing "Ring buffer captures stdout from a real subprocess"
    (let [ling-id (gen-id "buf-ling")
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "echo"
                                            :task "hello from ring buffer test"
                                            :buffer-capacity 100})]
      ;; Wait for echo to finish and reader thread to capture output
      (let [^java.lang.Process proc (:process result)]
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
      ;; Give the reader thread time to process
      (Thread/sleep 200)

      ;; Check stdout buffer
      (let [stdout (headless/get-stdout ling-id)]
        (is (vector? stdout) "stdout should be a vector")
        (is (pos? (count stdout)) "stdout should have captured lines")
        ;; echo outputs the task as positional args
        ;; build-command-parts for non-claude: ["echo" "hello from ring buffer test"]
        (is (some #(str/includes? % "hello") stdout)
            (str "stdout should contain 'hello', got: " (pr-str stdout))))

      ;; Check buffer stats
      (let [stats (headless/ring-buffer-stats (headless/get-stdout-buffer ling-id))]
        (is (pos? (:total-lines-seen stats))
            "Should have seen at least one line")
        (is (= 100 (:capacity stats))
            "Capacity should match what we set"))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest headless-spawn-ring-buffer-timestamps
  (testing "Ring buffer entries have timestamps for time-based retrieval"
    (let [ling-id (gen-id "ts-ling")
          before-spawn (System/currentTimeMillis)
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "echo"
                                            :task "timestamp test"
                                            :buffer-capacity 100})]
      (let [^java.lang.Process proc (:process result)]
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
      (Thread/sleep 200)

      ;; Test time-based retrieval
      (let [entries (headless/get-stdout-since ling-id 0)]
        (is (vector? entries) "Should return a vector of entries")
        (when (seq entries)
          (is (every? #(and (:text %) (:ts %)) entries)
              "Each entry should have :text and :ts")
          (is (every? #(>= (:ts %) before-spawn) entries)
              "All timestamps should be >= spawn time")))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest headless-kill-cleans-up-process
  (testing "kill-headless! terminates process and removes from registry"
    (let [ling-id (gen-id "kill-ling")
          ;; Use 'sleep' for a long-running process we can kill
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "sleep"
                                       :task "300"
                                       :buffer-capacity 100})]
      (is (headless/headless? ling-id) "Should be in registry")

      (let [result (headless/kill-headless! ling-id {:force? true})]
        (is (true? (:killed? result)) "Should report killed")
        (is (= ling-id (:ling-id result))))

      (is (not (headless/headless? ling-id))
          "Should be removed from registry after kill"))))

;; =============================================================================
;; Section 3: HeadlessStrategy via ILingStrategy Protocol
;;
;; Tests that HeadlessStrategy correctly implements ILingStrategy,
;; delegating to the headless module with proper argument passing.
;; =============================================================================

(deftest headless-strategy-satisfies-protocol
  (testing "HeadlessStrategy satisfies ILingStrategy"
    (let [strat (headless-strat/->headless-strategy)]
      (is (satisfies? strategy/ILingStrategy strat)))))

(deftest headless-strategy-spawn-delegates-correctly
  (testing "HeadlessStrategy.strategy-spawn! passes correct args to headless module"
    (let [strat (headless-strat/->headless-strategy)
          captured-args (atom nil)
          ctx {:id "hs-spawn-001" :cwd "/tmp/proj" :presets ["tdd"] :model nil}]
      ;; Mock build-spawn-envelope to nil so L2 enrichment doesn't interfere
      ;; with argument delegation testing (L2 enrichment tested separately in Section 3b)
      (with-redefs [headless/spawn-headless!
                    (fn [ling-id opts]
                      (reset! captured-args {:ling-id ling-id :opts opts})
                      {:ling-id ling-id :pid 12345})
                    envelope/build-spawn-envelope
                    (fn [_ _] nil)]
        (let [result (strategy/strategy-spawn! strat ctx {:task "Run tests"
                                                          :buffer-capacity 2000})]
          (is (= "hs-spawn-001" result) "Should return ling-id")
          (is (= "hs-spawn-001" (:ling-id @captured-args)))
          (is (= "/tmp/proj" (:cwd (:opts @captured-args))))
          (is (= "Run tests" (:task (:opts @captured-args)))
              "Task should pass through unchanged when no L2 context available")
          (is (= ["tdd"] (:presets (:opts @captured-args))))
          (is (= 2000 (:buffer-capacity (:opts @captured-args))))
          (is (nil? (:system-prompt (:opts @captured-args)))
              "No :system-prompt when L2 context unavailable"))))))

(deftest headless-strategy-spawn-routes-l2-to-system-prompt
  (testing "P3-T4: L2 context routes to :system-prompt, not task prefix"
    (let [strat (headless-strat/->headless-strategy)
          captured-args (atom nil)
          ctx {:id "hs-l2-spawn-001" :cwd "/tmp/proj" :presets ["tdd"] :model nil}
          ;; Store test data in context-store for L2 envelope
          ax-id (context-store/context-put!
                 [{:id "ax-1" :content "Test axiom for L2 routing"}]
                 :tags #{"axioms" "test"})]
      (with-redefs [headless/spawn-headless!
                    (fn [ling-id opts]
                      (reset! captured-args {:ling-id ling-id :opts opts})
                      {:ling-id ling-id :pid 12345})]
        (let [result (strategy/strategy-spawn! strat ctx
                                               {:task "Run tests"
                                                :ctx-refs {:axioms ax-id}
                                                :kg-node-ids []
                                                :scope "test-project"})]
          (is (= "hs-l2-spawn-001" result) "Should return ling-id")
          ;; KEY ASSERTION: Task stays clean — L2 context goes to system-prompt
          (is (= "Run tests" (:task (:opts @captured-args)))
              "Task must NOT be prefixed with L2 envelope (P3-T4)")
          ;; KEY ASSERTION: L2 context routed to :system-prompt
          (is (some? (:system-prompt (:opts @captured-args)))
              ":system-prompt should contain L2 envelope")
          (is (str/includes? (:system-prompt (:opts @captured-args)) "L2-CONTEXT")
              "system-prompt should contain L2 context marker")
          ;; Verify task is NOT contaminated with L2 markers
          (is (not (str/includes? (or (:task (:opts @captured-args)) "") "L2-CONTEXT"))
              "Task string must not contain L2 markers — they belong in system-prompt"))))))

(deftest headless-strategy-spawn-no-system-prompt-without-refs
  (testing "P3-T4: No :system-prompt when no L2 context available"
    (let [strat (headless-strat/->headless-strategy)
          captured-args (atom nil)
          ctx {:id "hs-no-l2-001" :cwd "/tmp/proj" :presets ["ling"] :model nil}]
      (with-redefs [headless/spawn-headless!
                    (fn [ling-id opts]
                      (reset! captured-args {:ling-id ling-id :opts opts})
                      {:ling-id ling-id :pid 12345})
                    ;; Mock spawn-envelope to nil (no catchup context available)
                    envelope/build-spawn-envelope
                    (fn [_ _] nil)]
        (strategy/strategy-spawn! strat ctx {:task "Plain task"})
        (is (= "Plain task" (:task (:opts @captured-args)))
            "Task should be unchanged")
        (is (nil? (:system-prompt (:opts @captured-args)))
            ":system-prompt should be nil without L2 context")))))

(deftest headless-strategy-dispatch-via-stdin
  (testing "HeadlessStrategy.strategy-dispatch! writes to stdin"
    (let [strat (headless-strat/->headless-strategy)
          captured (atom nil)
          ctx {:id "hs-dispatch-001"}]
      (with-redefs [headless/dispatch-via-stdin!
                    (fn [ling-id message]
                      (reset! captured {:ling-id ling-id :message message})
                      true)]
        (let [result (strategy/strategy-dispatch! strat ctx {:task "Analyze code"})]
          (is (true? result))
          (is (= "hs-dispatch-001" (:ling-id @captured)))
          (is (= "Analyze code" (:message @captured))))))))

;; =============================================================================
;; Section 3b: L2 Context Protocol — HeadlessStrategy Enrichment
;;
;; Tests that HeadlessStrategy correctly enriches tasks with L2 context
;; envelopes when dispatch-context is a RefContext. This is the Phase 3
;; integration: coordinator → RefContext → HeadlessStrategy → L2 envelope → stdin.
;; =============================================================================

(deftest headless-strategy-dispatch-l2-enrichment
  (testing "HeadlessStrategy enriches task with L2 envelope when RefContext provided"
    (let [strat (headless-strat/->headless-strategy)
          captured (atom nil)
          ctx {:id "hs-l2-001"}
          ;; Store test data in context-store
          ax-id (context-store/context-put!
                 [{:id "ax-1" :content "Never spawn drones from lings"}]
                 :tags #{"axioms" "test"})
          ;; Create RefContext with context-store refs
          ref-ctx (dispatch-ctx/->ref-context
                   "Fix the auth bug"
                   {:ctx-refs {:axioms ax-id}
                    :kg-node-ids []
                    :scope "test-project"})]
      (with-redefs [headless/dispatch-via-stdin!
                    (fn [ling-id message]
                      (reset! captured {:ling-id ling-id :message message})
                      true)]
        (strategy/strategy-dispatch! strat ctx {:task "Fix the auth bug"
                                                :dispatch-context ref-ctx})
        ;; The dispatched message should contain L2 envelope markers
        (is (some? @captured) "Should have dispatched to stdin")
        (is (str/includes? (:message @captured) "L2-CONTEXT")
            "Enriched task should contain L2 context envelope marker")
        (is (str/includes? (:message @captured) "Fix the auth bug")
            "Original task should still be present in enriched message")))))

(deftest headless-strategy-dispatch-no-enrichment-for-text-context
  (testing "HeadlessStrategy does not enrich when TextContext provided"
    (let [strat (headless-strat/->headless-strategy)
          captured (atom nil)
          ctx {:id "hs-text-001"}
          text-ctx (dispatch-ctx/->text-context "Fix the text bug")]
      (with-redefs [headless/dispatch-via-stdin!
                    (fn [ling-id message]
                      (reset! captured {:ling-id ling-id :message message})
                      true)]
        (strategy/strategy-dispatch! strat ctx {:task "Fix the text bug"
                                                :dispatch-context text-ctx})
        (is (some? @captured))
        ;; TextContext should NOT produce L2 envelope
        (is (not (str/includes? (:message @captured) "L2-CONTEXT"))
            "TextContext dispatch should not add L2 envelope")
        (is (= "Fix the text bug" (:message @captured))
            "Plain task should pass through unchanged")))))

(deftest headless-strategy-dispatch-nil-context-passthrough
  (testing "HeadlessStrategy passes task through when no dispatch-context"
    (let [strat (headless-strat/->headless-strategy)
          captured (atom nil)
          ctx {:id "hs-nil-001"}]
      (with-redefs [headless/dispatch-via-stdin!
                    (fn [ling-id message]
                      (reset! captured {:ling-id ling-id :message message})
                      true)]
        (strategy/strategy-dispatch! strat ctx {:task "Plain task, no context"})
        (is (= "Plain task, no context" (:message @captured))
            "Task without dispatch-context should pass through unchanged")))))

;; =============================================================================
;; Section 4: Ling Facade — Headless E2E (spawn → dispatch → status → kill)
;;
;; Tests the full lifecycle through the Ling record, which delegates to
;; HeadlessStrategy under the hood. All external calls are mocked.
;; =============================================================================

(deftest facade-headless-full-lifecycle
  (testing "Full lifecycle: spawn → status → dispatch → kill via Ling facade (headless→agent-sdk)"
    (let [ling-id "lifecycle-hl-001"
          ;; Since 0.12.0, :headless maps to :agent-sdk in ->ling
          ling-inst (ling/->ling ling-id {:cwd "/tmp/project"
                                          :project-id "test-project"
                                          :spawn-mode :headless})
          dispatched-tasks (atom [])]

      ;; Since :headless maps to :agent-sdk at the facade level, we mock SDK functions
      (with-redefs [;; SDK mocks (facade routes :headless → :agent-sdk)
                    sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [id opts]
                      (is (= ling-id id))
                      (is (= "/tmp/project" (:cwd opts)))
                      {:ling-id id :status :spawned :backend :agent-sdk :phase :idle})
                    sdk/sdk-status-for
                    (fn [id]
                      (when (= ling-id id)
                        {:ling-id id :phase :act :session-id "sess-mock"
                         :observations-count 0 :started-at 1000 :uptime-ms 1000
                         :backend :agent-sdk}))
                    sdk/get-session
                    (fn [id] (when (= ling-id id) {:ling-id id :phase :idle}))
                    sdk/dispatch-headless-sdk!
                    (fn [id task & _]
                      (swap! dispatched-tasks conj {:id id :task task})
                      (clojure.core.async/chan 1))
                    sdk/kill-headless-sdk!
                    (fn [id]
                      {:killed? true :ling-id id})]

        ;; 1. SPAWN
        (let [slave-id (proto/spawn! ling-inst {:depth 1})]
          (is (= ling-id slave-id) "spawn should return ling-id")

          ;; Verify DataScript registration
          (let [slave (ds-queries/get-slave ling-id)]
            (is (some? slave) "Should be registered in DataScript")
            ;; :headless maps to :agent-sdk since 0.12.0
            (is (= :agent-sdk (:ling/spawn-mode slave)))
            (is (= "claude" (:ling/model slave)))))

        ;; 2. STATUS
        (let [status (proto/status ling-inst)]
          (is (map? status))
          ;; SDK status fields (not headless fields)
          (is (true? (:sdk-alive? status)))
          (is (= :act (:sdk-phase status))))

        ;; 3. DISPATCH
        (let [ling-for-dispatch (ling/->ling ling-id {:cwd "/tmp/project"
                                                      :spawn-mode :headless})
              task-id (proto/dispatch! ling-for-dispatch {:task "Fix the bug"})]
          (is (string? task-id) "dispatch should return task-id string")
          (is (= 1 (count @dispatched-tasks)))
          (is (= "Fix the bug" (:task (first @dispatched-tasks)))))

        ;; 4. KILL
        (let [result (proto/kill! ling-inst)]
          (is (true? (:killed? result)))
          ;; DataScript should be cleaned up
          (is (nil? (ds-queries/get-slave ling-id))
              "Should be removed from DataScript after kill"))))))

;; =============================================================================
;; Section 5: OpenRouterStrategy HTTP Request Formatting (Mocked)
;;
;; Tests that the OpenRouter strategy builds the correct HTTP request
;; structure without making real API calls.
;; =============================================================================

(deftest openrouter-strategy-satisfies-protocol
  (testing "OpenRouterStrategy satisfies ILingStrategy"
    (let [strat (or-strat/->openrouter-strategy)]
      (is (satisfies? strategy/ILingStrategy strat)))))

(deftest openrouter-system-prompt-format
  (testing "build-system-prompt includes ling context fields"
    (let [prompt (or-strat/build-system-prompt
                  {:id "or-ling-42"
                   :cwd "/home/user/project"
                   :project-id "my-project"
                   :presets ["tdd" "clarity"]})]
      (is (str/includes? prompt "or-ling-42") "Should contain ling ID")
      (is (str/includes? prompt "/home/user/project") "Should contain cwd")
      (is (str/includes? prompt "my-project") "Should contain project-id")
      (is (str/includes? prompt "tdd") "Should contain preset names")
      (is (str/includes? prompt "clarity") "Should contain preset names"))))

(deftest openrouter-spawn-session-registry
  (testing "OpenRouter spawn registers session with correct metadata"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-id "or-spawn")
          ctx {:id ling-id :cwd "/tmp/project" :model "deepseek/deepseek-v3.2"
               :project-id "test-proj" :presets ["worker"]}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-api-key-xyz")]
        (let [result (strategy/strategy-spawn! strat ctx {})]
          (is (= ling-id result) "Should return ling-id")
          ;; Verify session registry
          (let [session (or-strat/get-session ling-id)]
            (is (some? session) "Session should exist in registry")
            (is (= "deepseek/deepseek-v3.2" (:model session)))
            (is (true? (:alive? session)))
            (is (= ling-id (:ling-id session)))
            (is (= "/tmp/project" (:cwd session)))
            ;; Initial conversation: just system prompt
            (is (= 1 (:message-count session))))
          ;; Verify conversation history has system prompt
          (let [conv (or-strat/get-conversation ling-id)]
            (is (= 1 (count conv)))
            (is (= "system" (:role (first conv))))
            (is (str/includes? (:content (first conv)) ling-id))))))))

(deftest openrouter-spawn-with-task-dispatches
  (testing "OpenRouter spawn with initial task triggers dispatch"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-id "or-task")
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-v3.2"
               :project-id "test" :presets []}
          dispatch-called? (atom false)]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key")
                    or-strat/dispatch-async! (fn [id msg]
                                               (is (= ling-id id))
                                               (is (= "Hello, identify yourself" msg))
                                               (reset! dispatch-called? true)
                                               nil)]
        (strategy/strategy-spawn! strat ctx {:task "Hello, identify yourself"})
        (is @dispatch-called? "Initial task should trigger dispatch-async!")))))

(deftest openrouter-sse-parsing-valid-chunk
  (testing "SSE parsing extracts content delta from streaming chunks"
    (let [line "data: {\"choices\":[{\"delta\":{\"content\":\"Hello world\"}}]}"
          chunk (or-strat/parse-sse-line line)]
      (is (some? chunk) "Should parse valid SSE line")
      (is (= "Hello world" (or-strat/extract-delta-content chunk))
          "Should extract content from delta"))))

(deftest openrouter-sse-parsing-done-signal
  (testing "SSE parsing handles [DONE] signal"
    (is (nil? (or-strat/parse-sse-line "data: [DONE]"))
        "[DONE] should return nil")))

(deftest openrouter-sse-parsing-edge-cases
  (testing "SSE parsing handles edge cases"
    (is (nil? (or-strat/parse-sse-line nil)) "nil input")
    (is (nil? (or-strat/parse-sse-line "")) "empty string")
    (is (nil? (or-strat/parse-sse-line "event: ping")) "non-data line")
    (is (nil? (or-strat/parse-sse-line ": keep-alive")) "comment line")
    (is (nil? (or-strat/parse-sse-line "data: {invalid json}")) "invalid JSON")))

(deftest openrouter-ring-buffer-after-spawn
  (testing "OpenRouter spawn creates ring buffer with system message"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-id "or-buf")
          ctx {:id ling-id :cwd "/tmp" :model "test-model"
               :project-id "test" :presets []}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key")]
        (strategy/strategy-spawn! strat ctx {})
        (let [stdout (or-strat/get-stdout ling-id)]
          (is (vector? stdout))
          (is (>= (count stdout) 1))
          (is (some #(str/includes? % "OpenRouter ling spawned") stdout)
              "Ring buffer should contain spawn message"))))))

;; =============================================================================
;; Section 6: Dispatch to Headless Ling via Stdin
;;
;; Tests that dispatch-via-stdin! correctly writes to the process stdin
;; using a real subprocess (cat command which echoes stdin to stdout).
;; =============================================================================

(deftest dispatch-stdin-real-subprocess
  (testing "dispatch-via-stdin! sends data through stdin pipe to a real process"
    (let [ling-id (gen-id "stdin-ling")
          ;; Use `cat` which reads stdin and writes to stdout
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "cat"
                                            :buffer-capacity 100})]
      ;; cat is running, waiting for stdin
      (is (headless/headless? ling-id))

      ;; Dispatch a message via stdin
      (let [sent? (headless/dispatch-via-stdin! ling-id "hello from stdin test")]
        (is (true? sent?) "Should return true on successful write"))

      ;; Give the reader thread time to capture
      (Thread/sleep 300)

      ;; cat should echo it back to stdout
      (let [stdout (headless/get-stdout ling-id)]
        (is (some #(str/includes? % "hello from stdin test") stdout)
            (str "stdout should contain dispatched message, got: " (pr-str stdout))))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest dispatch-stdin-nonexistent-ling-throws
  (testing "dispatch-via-stdin! throws for nonexistent ling"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not found"
                          (headless/dispatch-via-stdin! "nonexistent-ling" "hello")))))

(deftest dispatch-stdin-dead-process-throws
  (testing "dispatch-via-stdin! throws when process is dead"
    (let [ling-id (gen-id "dead-ling")
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "echo"
                                            :task "goodbye"
                                            :buffer-capacity 100})]
      ;; Wait for echo to finish (it exits immediately)
      (let [^java.lang.Process proc (:process result)]
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
      (Thread/sleep 200)

      ;; Now dispatch should fail because process is dead
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"not alive"
                            (headless/dispatch-via-stdin! ling-id "you're dead"))
          "Should throw when process is not alive")

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

;; =============================================================================
;; Section 7: Facade Integration — Headless Spawn with Model Override
;;
;; Tests the Ling facade's spawn-mode routing when model is passed
;; as a spawn option (overriding the record's model).
;; =============================================================================

(deftest facade-spawn-model-override-to-openrouter
  (testing "Ling.spawn! with non-claude model in opts routes to OpenRouter"
    (let [ling-id "model-override-001"
          ling-inst (ling/->ling ling-id {:cwd "/tmp"
                                          :project-id "test"
                                          :spawn-mode :headless})
          spawn-mode-used (atom nil)]
      ;; The ling was created with :headless, but passing a non-claude
      ;; model in spawn opts should override to :openrouter
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key")
                    ;; Track which strategy's spawn! gets called
                    headless/spawn-headless!
                    (fn [id _opts]
                      (reset! spawn-mode-used :headless)
                      {:ling-id id :pid 11111})
                    headless/headless-status
                    (fn [_] {:alive? true :pid 11111})
                    or-strat/dispatch-async!
                    (fn [_ _] nil)]
        ;; Spawn with model override
        (let [slave-id (proto/spawn! ling-inst {:model "deepseek/deepseek-v3.2"
                                                :depth 1})]
          (is (= ling-id slave-id))
          ;; Should NOT have used headless strategy
          (is (nil? @spawn-mode-used)
              "Non-claude model should bypass headless, use openrouter")
          ;; Should have registered in DS with openrouter mode
          (let [slave (ds-queries/get-slave ling-id)]
            (is (some? slave))
            (is (= :openrouter (:ling/spawn-mode slave))
                "DataScript should record :openrouter mode")
            (is (= "deepseek/deepseek-v3.2" (:ling/model slave))
                "DataScript should record the model")))))))

;; =============================================================================
;; Section 8: Ring Buffer Unit Tests (Detailed)
;; =============================================================================

(deftest ring-buffer-basic-operations
  (testing "Ring buffer append and retrieve"
    (let [buf (headless/create-ring-buffer 5)]
      (headless/ring-buffer-append! buf "line 1")
      (headless/ring-buffer-append! buf "line 2")
      (headless/ring-buffer-append! buf "line 3")

      (let [contents (headless/ring-buffer-contents buf)]
        (is (= 3 (count contents)))
        (is (= ["line 1" "line 2" "line 3"] contents))))))

(deftest ring-buffer-capacity-enforcement
  (testing "Ring buffer drops oldest lines when capacity exceeded"
    (let [buf (headless/create-ring-buffer 3)]
      (doseq [i (range 5)]
        (headless/ring-buffer-append! buf (str "line " i)))

      (let [contents (headless/ring-buffer-contents buf)]
        (is (= 3 (count contents)) "Should only hold capacity")
        (is (= ["line 2" "line 3" "line 4"] contents)
            "Should keep newest lines"))

      (let [stats (headless/ring-buffer-stats buf)]
        (is (= 3 (:current-lines stats)))
        (is (= 3 (:capacity stats)))
        (is (= 5 (:total-lines-seen stats)))
        (is (= 2 (:dropped stats)))))))

(deftest ring-buffer-last-n
  (testing "Ring buffer get-stdout with :last-n"
    (let [buf (headless/create-ring-buffer 100)]
      (doseq [i (range 10)]
        (headless/ring-buffer-append! buf (str "line " i)))

      (let [last-3 (headless/ring-buffer-contents buf {:last-n 3})]
        (is (= 3 (count last-3)))
        (is (= ["line 7" "line 8" "line 9"] last-3))))))

(deftest ring-buffer-since-timestamp
  (testing "Ring buffer time-based retrieval"
    (let [buf (headless/create-ring-buffer 100)
          before (System/currentTimeMillis)]
      (Thread/sleep 10)
      (headless/ring-buffer-append! buf "after-timestamp")
      (Thread/sleep 10)
      (headless/ring-buffer-append! buf "also-after")

      (let [entries (headless/ring-buffer-contents-since buf before)]
        (is (= 2 (count entries)))
        (is (= "after-timestamp" (:text (first entries))))
        (is (every? #(> (:ts %) before) entries)
            "All entries should be after the timestamp")))))

;; =============================================================================
;; Section 9: Build Command Parts (Headless Module)
;; =============================================================================

(deftest build-command-parts-default-claude
  (testing "Default command parts for claude CLI"
    ;; build-command-parts is private, test through spawn behavior
    ;; by checking what command headless/spawn-headless! would run.
    ;; We can test it indirectly by mocking ProcessBuilder
    ;; or by testing the public API behavior.
    ;; For now, test indirectly: spawn with claude-cmd override
    (let [ling-id (gen-id "cmd-test")
          ;; Use echo as a safe replacement
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "echo"
                                            :task "test task"
                                            :buffer-capacity 10})]
      ;; Process should have started
      (is (pos? (:pid result)))
      ;; Cleanup
      (let [^java.lang.Process proc (:process result)]
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest duplicate-ling-id-throws
  (testing "spawn-headless! with duplicate ID throws"
    (let [ling-id (gen-id "dup-test")
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "sleep"
                                       :task "300"
                                       :buffer-capacity 10})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"already exists"
                            (headless/spawn-headless! ling-id
                                                      {:cwd "/tmp"
                                                       :claude-cmd "echo"
                                                       :task "dup"
                                                       :buffer-capacity 10})))
      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

;; =============================================================================
;; Section 10: Full E2E — Spawn Headless → Dispatch → Collect → Verify
;;
;; This is the true end-to-end integration test for headless lings.
;; Uses a REAL subprocess (`cat`) which echoes stdin to stdout.
;; No mocks — tests the actual ProcessBuilder → ring buffer pipeline.
;;
;; Flow: spawn-headless!(cat) → dispatch-via-stdin!(msg) → get-stdout → assert
;; =============================================================================

(deftest ^:e2e headless-full-e2e-spawn-dispatch-collect-verify
  (testing "Full E2E: spawn headless → dispatch task → collect stdout → verify output"
    (let [ling-id (gen-id "e2e-full")
          test-message "E2E-MARKER-hello-from-headless-test"
          ;; 1. SPAWN: Use `cat` as the subprocess (echoes stdin → stdout)
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "cat"
                                            :buffer-capacity 500})]
      ;; Verify spawn succeeded
      (is (map? result) "spawn-headless! should return a result map")
      (is (= ling-id (:ling-id result)))
      (is (pos? (:pid result)) "Should have a positive PID")
      (is (headless/headless? ling-id) "Should be registered in process registry")

      ;; Verify process is alive
      (let [status (headless/headless-status ling-id)]
        (is (true? (:alive? status)) "Process should be alive after spawn"))

      ;; 2. DISPATCH: Send a task via stdin
      (let [sent? (headless/dispatch-via-stdin! ling-id test-message)]
        (is (true? sent?) "dispatch-via-stdin! should return true"))

      ;; 3. COLLECT: Poll stdout ring buffer until output appears
      ;; cat echoes stdin to stdout, so our message should appear
      (let [max-wait-ms 3000
            poll-interval 100
            deadline (+ (System/currentTimeMillis) max-wait-ms)
            collected (loop []
                        (let [stdout (headless/get-stdout ling-id)]
                          (if (some #(str/includes? % test-message) stdout)
                            stdout
                            (if (< (System/currentTimeMillis) deadline)
                              (do (Thread/sleep poll-interval) (recur))
                              stdout))))]

        ;; 4. VERIFY: stdout contains our dispatched message
        (is (some #(str/includes? % test-message) collected)
            (str "stdout should contain dispatched message '" test-message
                 "', got: " (pr-str (vec (take-last 5 collected))))))

      ;; 5. VERIFY: Ring buffer stats are consistent
      (let [stats (headless/ring-buffer-stats (headless/get-stdout-buffer ling-id))]
        (is (pos? (:total-lines-seen stats))
            "Should have seen at least one line")
        (is (= 500 (:capacity stats))
            "Capacity should match what we set"))

      ;; 6. CLEANUP: Kill process
      (let [kill-result (headless/kill-headless! ling-id {:force? true})]
        (is (true? (:killed? kill-result)) "Should report killed"))
      (is (not (headless/headless? ling-id))
          "Should be removed from registry after kill"))))

(deftest ^:e2e headless-full-e2e-multi-dispatch-collect
  (testing "Full E2E: spawn → multiple dispatches → collect all → verify ordering"
    (let [ling-id (gen-id "e2e-multi")
          messages ["MSG-FIRST-alpha" "MSG-SECOND-beta" "MSG-THIRD-gamma"]
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "cat"
                                            :buffer-capacity 500})]
      (is (true? (:alive? (headless/headless-status ling-id))))

      ;; Dispatch multiple messages sequentially
      (doseq [msg messages]
        (headless/dispatch-via-stdin! ling-id msg)
        ;; Small delay so cat processes each line
        (Thread/sleep 50))

      ;; Collect with polling
      (let [max-wait-ms 3000
            deadline (+ (System/currentTimeMillis) max-wait-ms)
            all-found? (loop []
                         (let [stdout (headless/get-stdout ling-id)
                               found (every? (fn [msg]
                                               (some #(str/includes? % msg) stdout))
                                             messages)]
                           (if found
                             true
                             (if (< (System/currentTimeMillis) deadline)
                               (do (Thread/sleep 100) (recur))
                               false))))]
        (is (true? all-found?)
            (str "All messages should appear in stdout. Got: "
                 (pr-str (headless/get-stdout ling-id)))))

      ;; Verify ordering via timestamps (since-based retrieval)
      (let [entries (headless/get-stdout-since ling-id 0)]
        (is (vector? entries))
        (when (>= (count entries) 3)
          ;; Find indices of each message in the entries
          (let [find-idx (fn [msg]
                           (first (keep-indexed
                                   (fn [i entry]
                                     (when (str/includes? (:text entry) msg) i))
                                   entries)))
                idx-first (find-idx "MSG-FIRST")
                idx-second (find-idx "MSG-SECOND")
                idx-third (find-idx "MSG-THIRD")]
            (when (and idx-first idx-second idx-third)
              (is (< idx-first idx-second idx-third)
                  "Messages should appear in dispatch order")))))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest ^:e2e headless-full-e2e-incremental-collect-since
  (testing "Full E2E: spawn → dispatch → incremental collect via get-stdout-since"
    (let [ling-id (gen-id "e2e-since")
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "cat"
                                       :buffer-capacity 500})]

      ;; Dispatch first batch
      (headless/dispatch-via-stdin! ling-id "BATCH-ONE-alpha")
      (Thread/sleep 300)

      ;; Record cursor timestamp
      (let [cursor-ts (System/currentTimeMillis)]
        (Thread/sleep 50)  ;; Ensure next batch gets later timestamps

        ;; Dispatch second batch
        (headless/dispatch-via-stdin! ling-id "BATCH-TWO-beta")
        (Thread/sleep 300)

        ;; Collect only entries AFTER cursor (should only get BATCH-TWO)
        (let [new-entries (headless/get-stdout-since ling-id cursor-ts)]
          (is (vector? new-entries) "Should return a vector")
          (when (seq new-entries)
            (is (some #(str/includes? (:text %) "BATCH-TWO") new-entries)
                "New entries should contain BATCH-TWO")
            (is (not (some #(str/includes? (:text %) "BATCH-ONE") new-entries))
                "New entries should NOT contain BATCH-ONE (it was before cursor)"))))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest ^:e2e headless-full-e2e-facade-lifecycle
  (testing "Full E2E via HeadlessStrategy directly: spawn → dispatch → status → kill"
    ;; Since 0.12.0, :headless maps to :agent-sdk in the Ling facade.
    ;; This test exercises the HeadlessStrategy (ProcessBuilder) path directly
    ;; to verify the real subprocess pipeline works end-to-end.
    (let [ling-id (gen-id "e2e-facade")
          strat (headless-strat/->headless-strategy)
          test-msg "FACADE-E2E-hello-world"]

      ;; 1. SPAWN: Use cat directly via HeadlessStrategy
      (let [original-spawn headless/spawn-headless!]
        (with-redefs [headless/spawn-headless!
                      (fn [id opts]
                        (original-spawn id (assoc opts :claude-cmd "cat")))
                      envelope/build-spawn-envelope (fn [_ _] nil)]
          (let [result (strategy/strategy-spawn! strat
                                                 {:id ling-id :cwd "/tmp" :presets []}
                                                 {:buffer-capacity 500})]
            (is (= ling-id result) "spawn should return ling-id"))))

      ;; Register in DataScript (manually, as strategy doesn't do this)
      (ds-lings/add-slave!
       ling-id {:status :idle
                :depth 1
                :cwd "/tmp"
                :project-id "test-e2e"})
      (ds-lings/update-slave!
       ling-id {:ling/spawn-mode :headless
                :ling/model "claude"
                :ling/process-pid (:pid (headless/headless-status ling-id))
                :ling/process-alive? true})

      ;; 2. STATUS via HeadlessStrategy
      (let [status (strategy/strategy-status strat {:id ling-id} (ds-queries/get-slave ling-id))]
        (is (map? status))
        (is (true? (:headless-alive? status))
            "Headless should be alive"))

      ;; 3. DISPATCH via HeadlessStrategy (stdin dispatch)
      (let [result (strategy/strategy-dispatch! strat {:id ling-id} {:task test-msg})]
        (is (true? result) "dispatch should return true"))

      ;; 4. COLLECT stdout
      (Thread/sleep 500)
      (let [stdout (headless/get-stdout ling-id)]
        (is (some #(str/includes? % test-msg) stdout)
            (str "stdout should contain '" test-msg "', got: "
                 (pr-str (vec (take-last 5 stdout))))))

      ;; 5. KILL via HeadlessStrategy
      (let [result (strategy/strategy-kill! strat {:id ling-id})]
        (is (true? (:killed? result))))

      ;; 6. VERIFY cleanup
      (is (not (headless/headless? ling-id))
          "Should be removed from process registry")
      ;; Note: DataScript cleanup is the facade's responsibility (Ling.kill!),
      ;; not the strategy's. We manually clean up since we're testing strategy directly.
      (ds-lings/remove-slave! ling-id)
      (is (nil? (ds-queries/get-slave ling-id))
          "Should be removed from DataScript after manual cleanup"))))

(comment
  ;; Run all E2E tests
  (clojure.test/run-tests 'hive-mcp.agent.ling.headless-e2e-test)

  ;; Run only the new E2E tests
  (clojure.test/test-vars [#'headless-full-e2e-spawn-dispatch-collect-verify
                           #'headless-full-e2e-multi-dispatch-collect
                           #'headless-full-e2e-incremental-collect-since
                           #'headless-full-e2e-facade-lifecycle]))
