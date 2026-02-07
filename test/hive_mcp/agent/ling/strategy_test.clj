(ns hive-mcp.agent.ling.strategy-test
  "Tests for the Ling Strategy Protocol decomposition.

   Tests cover:
   - ILingStrategy protocol contract
   - VtermStrategy (mocked elisp)
   - HeadlessStrategy (mocked headless)
   - Strategy resolution (resolve-strategy)
   - Ling facade delegation to strategies

   All external dependencies (emacsclient, headless) are mocked."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.vterm :as vterm]
            [hive-mcp.agent.ling.headless-strategy :as headless-strat]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.headless-sdk :as sdk]
            [clojure.core.async :as async]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.emacsclient :as ec]
            [clojure.string :as str]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-datascript-fixture
  "Reset DataScript and SDK state before and after each test."
  [f]
  (ds/reset-conn!)
  (try (sdk/kill-all-sdk!) (catch Exception _))
  (f)
  (try (sdk/kill-all-sdk!) (catch Exception _))
  (ds/reset-conn!))

(use-fixtures :each reset-datascript-fixture)

;; =============================================================================
;; Section 1: ILingStrategy Protocol Tests
;; =============================================================================

(deftest strategy-protocol-exists
  (testing "ILingStrategy protocol is defined with correct methods"
    (is (some? strategy/ILingStrategy)
        "ILingStrategy protocol should exist")
    ;; Verify the protocol has the expected method signatures
    (is (ifn? strategy/strategy-spawn!)
        "strategy-spawn! should be a function")
    (is (ifn? strategy/strategy-dispatch!)
        "strategy-dispatch! should be a function")
    (is (ifn? strategy/strategy-status)
        "strategy-status should be a function")
    (is (ifn? strategy/strategy-kill!)
        "strategy-kill! should be a function")))

;; =============================================================================
;; Section 2: VtermStrategy Tests
;; =============================================================================

(deftest vterm-strategy-spawn-success
  (testing "VtermStrategy spawn delegates to emacsclient"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "vterm-test-001" :cwd "/tmp/project" :presets ["tdd"]
               :project-id "test" :model nil}]
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [code _timeout]
                      ;; Verify elisp code contains the ling ID
                      (is (re-find #"vterm-test-001" code)
                          "Elisp code should contain ling ID")
                      {:success true :result "vterm-test-001"})]
        (let [slave-id (strategy/strategy-spawn! strat ctx {})]
          (is (= "vterm-test-001" slave-id)
              "Should return elisp result as slave-id"))))))

(deftest vterm-strategy-spawn-failure-throws
  (testing "VtermStrategy spawn throws on elisp failure"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "fail-ling" :cwd "/tmp" :presets [] :model nil}]
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [_code _timeout]
                      {:success false :error "Emacs not running"})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Failed to spawn ling"
                              (strategy/strategy-spawn! strat ctx {})))))))

(deftest vterm-strategy-dispatch-success
  (testing "VtermStrategy dispatch sends via elisp"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "dispatch-vterm" :cwd "/tmp"}]
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [code timeout]
                      (is (re-find #"dispatch-vterm" code)
                          "Elisp dispatch should target the ling ID")
                      (is (re-find #"Fix the bug" code)
                          "Elisp should contain the task")
                      {:success true :result "ok"})]
        (let [result (strategy/strategy-dispatch! strat ctx {:task "Fix the bug"
                                                             :timeout-ms 30000})]
          (is (true? result)))))))

(deftest vterm-strategy-dispatch-failure-throws
  (testing "VtermStrategy dispatch throws on elisp failure"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "fail-dispatch" :cwd "/tmp"}]
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [_code _timeout]
                      {:success false :error "Timeout"})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Vterm dispatch failed"
                              (strategy/strategy-dispatch! strat ctx {:task "Fix it"})))))))

(deftest vterm-strategy-status-with-ds
  (testing "VtermStrategy status enriches DataScript data"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "status-vterm"}
          ds-data {:slave/id "status-vterm"
                   :slave/status :working}]
      ;; When DataScript has data and status is known, no elisp call needed
      (let [result (strategy/strategy-status strat ctx ds-data)]
        (is (= :working (:slave/status result))
            "Should return DataScript status directly")))))

(deftest vterm-strategy-status-unknown-queries-elisp
  (testing "VtermStrategy queries elisp when status unknown"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "unknown-vterm"}
          ds-data {:slave/id "unknown-vterm"
                   :slave/status :unknown}]
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [_code _timeout]
                      {:success true :result "working"})]
        (let [result (strategy/strategy-status strat ctx ds-data)]
          (is (true? (:elisp-alive? result))
              "Should indicate elisp alive"))))))

(deftest vterm-strategy-kill-success
  (testing "VtermStrategy kill delegates to elisp"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "kill-vterm"}]
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [_code _timeout]
                      {:success true :result "killed"})]
        (let [result (strategy/strategy-kill! strat ctx)]
          (is (true? (:killed? result)))
          (is (= "kill-vterm" (:id result))))))))

(deftest vterm-strategy-kill-failure
  (testing "VtermStrategy returns killed?=false on elisp failure"
    (let [strat (vterm/->vterm-strategy)
          ctx {:id "kill-fail-vterm"}]
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [_code _timeout]
                      {:success false :error "process not found"})]
        (let [result (strategy/strategy-kill! strat ctx)]
          (is (false? (:killed? result)))
          (is (= :elisp-kill-failed (:reason result))))))))

;; =============================================================================
;; Section 3: HeadlessStrategy Tests
;; =============================================================================

(deftest headless-strategy-spawn-success
  (testing "HeadlessStrategy spawn delegates to headless module"
    (let [strat (headless-strat/->headless-strategy)
          ctx {:id "headless-001" :cwd "/tmp/project"
               :presets ["worker"] :model nil}]
      (with-redefs [headless/spawn-headless!
                    (fn [ling-id opts]
                      (is (= "headless-001" ling-id))
                      (is (= "/tmp/project" (:cwd opts)))
                      {:ling-id ling-id :pid 12345})]
        (let [slave-id (strategy/strategy-spawn! strat ctx {})]
          (is (= "headless-001" slave-id)
              "Headless always returns requested id"))))))

(deftest headless-strategy-dispatch-success
  (testing "HeadlessStrategy dispatch writes to stdin"
    (let [strat (headless-strat/->headless-strategy)
          ctx {:id "dispatch-headless"}]
      (with-redefs [headless/dispatch-via-stdin!
                    (fn [ling-id message]
                      (is (= "dispatch-headless" ling-id))
                      (is (= "Run tests" message))
                      true)]
        (let [result (strategy/strategy-dispatch! strat ctx {:task "Run tests"})]
          (is (true? result)))))))

(deftest headless-strategy-status-with-ds
  (testing "HeadlessStrategy enriches DataScript with process info"
    (let [strat (headless-strat/->headless-strategy)
          ctx {:id "status-headless"}
          ds-data {:slave/id "status-headless" :slave/status :idle}]
      (with-redefs [headless/headless-status
                    (fn [ling-id]
                      {:ling-id ling-id :alive? true :pid 9999
                       :uptime-ms 5000
                       :stdout {:current-lines 100}
                       :stderr {:current-lines 0}})]
        (let [result (strategy/strategy-status strat ctx ds-data)]
          (is (true? (:headless-alive? result)))
          (is (= 9999 (:headless-pid result)))
          (is (= 5000 (:headless-uptime-ms result))))))))

(deftest headless-strategy-status-no-ds
  (testing "HeadlessStrategy returns headless-only status when no DataScript"
    (let [strat (headless-strat/->headless-strategy)
          ctx {:id "orphan-headless"}]
      (with-redefs [headless/headless-status
                    (fn [_] {:alive? true :pid 8888})]
        (let [result (strategy/strategy-status strat ctx nil)]
          (is (= :idle (:slave/status result))
              "Alive headless with no DS should report :idle")
          (is (true? (:headless-alive? result))))))))

(deftest headless-strategy-kill-success
  (testing "HeadlessStrategy kill delegates to headless module"
    (let [strat (headless-strat/->headless-strategy)
          ctx {:id "kill-headless"}]
      (with-redefs [headless/kill-headless!
                    (fn [ling-id]
                      {:killed? true :ling-id ling-id :pid 7777 :exit-code 0})]
        (let [result (strategy/strategy-kill! strat ctx)]
          (is (true? (:killed? result)))
          (is (= 7777 (:pid result))))))))

(deftest headless-strategy-kill-already-dead
  (testing "HeadlessStrategy returns killed?=true when process already dead"
    (let [strat (headless-strat/->headless-strategy)
          ctx {:id "dead-headless"}]
      (with-redefs [headless/kill-headless!
                    (fn [_] (throw (ex-info "Not found" {})))]
        (let [result (strategy/strategy-kill! strat ctx)]
          (is (true? (:killed? result)))
          (is (= :process-already-dead (:reason result))))))))

;; =============================================================================
;; Section 4: Facade Integration Tests
;; =============================================================================

(deftest facade-spawn-uses-vterm-strategy-by-default
  (testing "Ling facade defaults to vterm strategy"
    (let [ling (ling/->ling "facade-vterm" {:cwd "/tmp" :project-id "test"})]
      (is (= :vterm (:spawn-mode ling))
          "Default spawn mode should be :vterm")
      (with-redefs [ec/eval-elisp-with-timeout
                    (fn [_code _timeout]
                      {:success true :result "facade-vterm"})]
        (let [slave-id (proto/spawn! ling {:depth 1})]
          (is (= "facade-vterm" slave-id))
          ;; Verify DataScript registration (common concern)
          (let [slave (ds-queries/get-slave "facade-vterm")]
            (is (some? slave))
            (is (= :vterm (:ling/spawn-mode slave)))))))))

(deftest facade-spawn-uses-headless-strategy
  (testing "Ling facade maps :headless to :agent-sdk since 0.12.0"
    (let [ling (ling/->ling "facade-headless"
                            {:cwd "/tmp" :project-id "test"
                             :spawn-mode :headless})]
      ;; ->ling maps :headless → :agent-sdk at factory level
      (is (= :agent-sdk (:spawn-mode ling)))
      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [ling-id _opts]
                      {:ling-id ling-id :status :spawned
                       :backend :agent-sdk :phase :idle})
                    sdk/get-session
                    (fn [_] {:session-id "test-sess" :status :idle})]
        (let [slave-id (proto/spawn! ling {:depth 1})]
          (is (= "facade-headless" slave-id))
          (let [slave (ds-queries/get-slave "facade-headless")]
            (is (some? slave))
            (is (= :agent-sdk (:ling/spawn-mode slave)))))))))

(deftest facade-auto-openrouter-for-non-claude
  (testing "Non-claude models automatically use openrouter strategy"
    (let [ling (ling/->ling "deepseek-ling"
                            {:cwd "/tmp" :project-id "test"
                             :model "deepseek/deepseek-v3.2"})]
      (is (= :openrouter (:spawn-mode ling))
          "Non-claude model should force openrouter")
      (is (= "deepseek/deepseek-v3.2" (:model ling))
          "Model should be preserved"))))

(deftest facade-dispatch-delegates-to-strategy
  (testing "Dispatch delegates to correct strategy based on mode"
    ;; Register ling in DataScript — facade maps :headless → :agent-sdk
    (ds-lings/add-slave! "dispatch-delegate" {:status :idle :cwd "/tmp"})
    (ds-lings/update-slave! "dispatch-delegate" {:ling/spawn-mode :agent-sdk})

    (let [ling (ling/->ling "dispatch-delegate" {:cwd "/tmp" :spawn-mode :headless})]
      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/dispatch-headless-sdk!
                    (fn [ling-id message & _]
                      (is (= "dispatch-delegate" ling-id))
                      (is (= "Analyze code" message))
                      (let [ch (async/chan 1)]
                        (async/put! ch {:task-id "task-1" :result "ok"})
                        (async/close! ch)
                        ch))
                    sdk/get-session
                    (fn [_] {:session-id "test-sess" :status :idle})]
        (let [task-id (proto/dispatch! ling {:task "Analyze code"})]
          (is (string? task-id))
          ;; Verify common DataScript updates
          (let [slave (ds-queries/get-slave "dispatch-delegate")]
            (is (= :working (:slave/status slave)))))))))

(deftest facade-kill-delegates-to-strategy
  (testing "Kill delegates to correct strategy and cleans up DataScript"
    (ds-lings/add-slave! "kill-delegate" {:status :idle})

    ;; facade maps :headless → :agent-sdk
    (let [ling (ling/->ling "kill-delegate" {:spawn-mode :headless})]
      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/kill-headless-sdk!
                    (fn [ling-id]
                      {:killed? true :ling-id ling-id})
                    sdk/get-session
                    (fn [_] nil)]
        (let [result (proto/kill! ling)]
          (is (true? (:killed? result)))
          ;; Common: should be removed from DataScript
          (is (nil? (ds-queries/get-slave "kill-delegate"))
              "Should be removed from DataScript after kill"))))))

(deftest facade-status-delegates-to-strategy
  (testing "Status delegates to correct strategy (agent-sdk since :headless maps to it)"
    (ds-lings/add-slave! "status-delegate" {:status :idle})
    (ds-lings/update-slave! "status-delegate" {:ling/spawn-mode :agent-sdk})

    ;; facade maps :headless → :agent-sdk
    (let [ling (ling/->ling "status-delegate" {:spawn-mode :headless})]
      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/sdk-status-for
                    (fn [ling-id]
                      {:session-id "sess-444" :status :idle
                       :model "claude" :phase :act})
                    sdk/get-session
                    (fn [_] {:session-id "sess-444" :status :idle})]
        (let [status (proto/status ling)]
          ;; Agent-SDK status has different fields than headless
          (is (some? status) "Status should be returned")
          (is (= :idle (:slave/status status))))))))

;; =============================================================================
;; Section 5: Query Functions (mode-independent, unaffected by refactoring)
;; =============================================================================

(deftest query-fns-preserve-spawn-mode
  (testing "get-ling maps :headless to :agent-sdk for claude models (0.12.0+)"
    (ds-lings/add-slave! "mode-preserved" {:status :idle :depth 1})
    (ds-lings/update-slave! "mode-preserved" {:ling/spawn-mode :headless
                                              :ling/model "claude"})

    (let [ling (ling/get-ling "mode-preserved")]
      (is (some? ling))
      ;; ->ling factory maps :headless → :agent-sdk for claude models
      (is (= :agent-sdk (:spawn-mode ling))
          "spawn-mode should be mapped from :headless to :agent-sdk")
      (is (= "claude" (:model ling))
          "model should be preserved from DataScript")))

  (testing "get-ling overrides spawn-mode to :openrouter for non-claude models"
    (ds-lings/add-slave! "mode-override" {:status :idle :depth 1})
    (ds-lings/update-slave! "mode-override" {:ling/spawn-mode :headless
                                             :ling/model "deepseek/deepseek-v3.2"})

    (let [ling (ling/get-ling "mode-override")]
      (is (some? ling))
      (is (= :openrouter (:spawn-mode ling))
          "Non-claude model should override to :openrouter regardless of DataScript")
      (is (= "deepseek/deepseek-v3.2" (:model ling))
          "model should be preserved from DataScript"))))

;; =============================================================================
;; Section 6: Hint Injection into Headless Spawn (Tier 1 Activation E2E)
;;
;; Tests that when a headless ling is spawned with a kanban-task-id,
;; generate-task-hints is called and the hint block is prepended to the
;; task string passed to strategy-spawn! (embedded in CLI arg).
;; =============================================================================

(deftest headless-spawn-with-hints-injects-into-task
  (testing "Headless spawn with kanban-task-id prepends hints to task (via agent-sdk)"
    (let [captured-task (atom nil)
          ling-inst (ling/->ling "hints-headless-001"
                                 {:cwd "/tmp/project"
                                  :project-id "test-project"
                                  :spawn-mode :headless})]
      (with-redefs [;; Mock hints generation
                    hints/generate-task-hints
                    (fn [{:keys [task-id depth]}]
                      (is (= "kanban-task-42" task-id) "Should pass kanban-task-id")
                      (is (= 2 depth) "Should use depth 2")
                      {:l1-ids ["mem-1" "mem-2"]
                       :l2-queries ["conventions for: testing"]
                       :l3-seeds [{:id "seed-1" :depth 2}]})
                    hints/generate-hints
                    (fn [project-id opts]
                      (is (= "test-project" project-id))
                      (is (some #{"mem-1"} (:extra-ids opts))
                          "Should pass l1-ids as extra-ids")
                      {:memory-hints {:axiom-ids ["ax-1"]
                                      :read-ids ["mem-1" "mem-2"]
                                      :queries ["conventions for: testing"]}})
                    hints/serialize-hints
                    (fn [hint-data & {:keys [project-name]}]
                      (is (= "test-project" project-name))
                      "## Memory Hints (Auto-Injected)\n\nMocked hint block")
                    ;; Mock SDK spawn — returns plain map (not channel)
                    sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [ling-id _opts]
                      {:ling-id ling-id :status :spawned :backend :agent-sdk :phase :idle})
                    ;; Capture task from dispatch (SDK strategy dispatches after spawn)
                    sdk/dispatch-headless-sdk!
                    (fn [_ling-id task & _]
                      (reset! captured-task task)
                      nil)
                    sdk/get-session
                    (fn [_] {:session-id "test-sess" :status :idle})]
        (let [slave-id (proto/spawn! ling-inst {:task "Fix the authentication bug"
                                                :kanban-task-id "kanban-task-42"
                                                :depth 1})]
          (is (= "hints-headless-001" slave-id))
          ;; The task dispatched via sdk/dispatch-headless-sdk! should contain hints
          (is (some? @captured-task) "Task should be passed to dispatch")
          (is (str/includes? @captured-task "Memory Hints")
              "Task should contain hint block")
          (is (str/includes? @captured-task "Mocked hint block")
              "Task should contain serialized hints")
          (is (str/includes? @captured-task "Fix the authentication bug")
              "Task should contain original task")
          (is (str/starts-with? @captured-task "## Memory Hints")
              "Hints should be prepended (before task)")
          ;; Verify separator between hints and task
          (is (str/includes? @captured-task "---")
              "Should have separator between hints and task"))))))

(deftest headless-spawn-without-kanban-task-id-no-hints
  (testing "Headless spawn without kanban-task-id passes task as-is (via agent-sdk)"
    (let [captured-task (atom nil)
          ling-inst (ling/->ling "no-hints-headless"
                                 {:cwd "/tmp/project"
                                  :project-id "test-project"
                                  :spawn-mode :headless})]
      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [ling-id _opts]
                      {:ling-id ling-id :status :spawned :backend :agent-sdk :phase :idle})
                    sdk/dispatch-headless-sdk!
                    (fn [_ling-id task & _]
                      (reset! captured-task task)
                      nil)
                    sdk/get-session
                    (fn [_] {:session-id "test-sess" :status :idle})]
        (let [slave-id (proto/spawn! ling-inst {:task "Simple task"
                                                :depth 1})]
          (is (= "no-hints-headless" slave-id))
          (is (= "Simple task" @captured-task)
              "Without kanban-task-id, task should be passed verbatim")
          (is (not (str/includes? (or @captured-task "") "Memory Hints"))
              "No hints should be injected"))))))

(deftest headless-spawn-hints-failure-is-non-fatal
  (testing "Hint generation failure doesn't prevent spawn (via agent-sdk)"
    (let [captured-task (atom nil)
          ling-inst (ling/->ling "hints-fail-headless"
                                 {:cwd "/tmp/project"
                                  :project-id "test-project"
                                  :spawn-mode :headless})]
      (with-redefs [hints/generate-task-hints
                    (fn [_] (throw (Exception. "Chroma down")))
                    sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [ling-id _opts]
                      {:ling-id ling-id :status :spawned :backend :agent-sdk :phase :idle})
                    sdk/dispatch-headless-sdk!
                    (fn [_ling-id task & _]
                      (reset! captured-task task)
                      nil)
                    sdk/get-session
                    (fn [_] {:session-id "test-sess" :status :idle})]
        (let [slave-id (proto/spawn! ling-inst {:task "Important task"
                                                :kanban-task-id "failing-task"
                                                :depth 1})]
          (is (= "hints-fail-headless" slave-id)
              "Spawn should succeed despite hint failure")
          (is (= "Important task" @captured-task)
              "Task should be passed without hints on failure"))))))

(deftest headless-spawn-with-hints-no-task-no-crash
  (testing "Headless spawn with kanban-task-id but no task doesn't crash (via agent-sdk)"
    (let [dispatch-called? (atom false)
          ling-inst (ling/->ling "no-task-headless"
                                 {:cwd "/tmp/project"
                                  :project-id "test-project"
                                  :spawn-mode :headless})]
      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [ling-id _opts]
                      {:ling-id ling-id :status :spawned :backend :agent-sdk :phase :idle})
                    sdk/dispatch-headless-sdk!
                    (fn [_ling-id _task & _]
                      (reset! dispatch-called? true)
                      nil)
                    sdk/get-session
                    (fn [_] {:session-id "test-sess" :status :idle})]
        (let [slave-id (proto/spawn! ling-inst {:kanban-task-id "task-no-prompt"
                                                :depth 1})]
          (is (= "no-task-headless" slave-id)
              "Should spawn successfully without task")
          (is (not @dispatch-called?)
              "No dispatch should happen when no task provided"))))))

(deftest vterm-spawn-with-hints-dispatches-enriched-task
  (testing "Vterm spawn with kanban-task-id dispatches hints-enriched task via elisp"
    (let [captured-dispatch-code (atom nil)
          ling-inst (ling/->ling "hints-vterm-001"
                                 {:cwd "/tmp/project"
                                  :project-id "test-project"
                                  :spawn-mode :vterm})]
      (with-redefs [hints/generate-task-hints
                    (fn [{:keys [task-id]}]
                      {:l1-ids ["vterm-mem-1"]
                       :l2-queries []
                       :l3-seeds []})
                    hints/generate-hints
                    (fn [_ _]
                      {:memory-hints {:axiom-ids ["vax-1"]
                                      :read-ids ["vterm-mem-1"]}})
                    hints/serialize-hints
                    (fn [_ & _] "## Memory Hints\n\nVterm hints")
                    ec/eval-elisp-with-timeout
                    (fn [code _timeout]
                      ;; Capture the dispatch call (contains the task), not the spawn call
                      (when (str/includes? code "hive-mcp-swarm-api-dispatch")
                        (reset! captured-dispatch-code code))
                      {:success true :result "hints-vterm-001"})]
        (let [slave-id (proto/spawn! ling-inst {:task "Vterm task"
                                                :kanban-task-id "kanban-vterm-1"
                                                :depth 1})]
          (is (= "hints-vterm-001" slave-id))
          ;; The dispatch elisp code should contain hints in the task
          (is (some? @captured-dispatch-code)
              "A dispatch call should have been made")
          (is (str/includes? @captured-dispatch-code "Memory Hints")
              "Dispatch elisp should contain hint block")
          (is (str/includes? @captured-dispatch-code "Vterm task")
              "Dispatch elisp should contain original task"))))))

(comment
  ;; Run all strategy tests
  (clojure.test/run-tests 'hive-mcp.agent.ling.strategy-test))
