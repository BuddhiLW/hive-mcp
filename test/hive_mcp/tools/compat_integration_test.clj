(ns hive-mcp.tools.compat-integration-test
  "Integration tests for backward-compatibility shims.

   These tests verify that deprecated tools properly delegate to
   consolidated handlers with parameter transformation.

   Test Coverage:
   1. swarm_spawn shim delegates to agent:spawn
   2. swarm_status shim returns all agents via agent:status
   3. swarm_kill shim kills correct target via agent:kill
   4. Parameter renames work (directory->cwd, slave_id->agent_id)
   5. Static params merged correctly (type='ling' for swarm_spawn)

   Unlike compat_test.clj which uses mocks, these tests exercise
   real consolidated handlers with DataScript state."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.compat :as compat]
            [hive-mcp.tools.consolidated.agent :as agent]
            [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.emacsclient :as ec]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-state-fixture
  "Reset DataScript and logic state for clean tests."
  [f]
  ;; Reset DataScript connection
  (conn/reset-conn!)
  ;; Reset logic database
  (logic/reset-db!)
  ;; Clear consolidated handler cache to ensure fresh state
  (reset! @#'compat/consolidated-handlers nil)
  (f)
  ;; Cleanup after test
  (conn/reset-conn!)
  (logic/reset-db!))

(use-fixtures :each reset-state-fixture)

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn parse-response
  "Parse JSON response from handler."
  [result]
  (when-not (:isError result)
    (json/read-str (:text result) :key-fn keyword)))

(defn add-test-slave!
  "Add a test slave to DataScript for testing."
  [slave-id {:keys [depth status cwd project-id presets parent]
             :or {depth 1 status :idle cwd "/tmp/test" project-id "test-project"}}]
  (ds-lings/add-slave! slave-id {:depth depth
                                 :status status
                                 :cwd cwd
                                 :project-id project-id
                                 :presets (or presets [])
                                 :parent parent}))

(defn capture-logs
  "Execute body and capture all log messages via timbre appender.
   Returns [result log-messages]."
  [f]
  (let [logs (atom [])]
    (log/with-merged-config
      {:appenders {:test-capture {:enabled? true
                                  :fn (fn [data]
                                        (swap! logs conj
                                               {:level (:level data)
                                                :msg (force (:msg_ data))}))}}}
      [(f) @logs])))

;; =============================================================================
;; swarm_spawn Integration Tests
;; =============================================================================

(deftest swarm-spawn-delegates-to-agent-spawn
  (testing "swarm_spawn shim delegates to agent:spawn with correct params"
    ;; Mock elisp at emacsclient level for ling spawn
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "spawned"})]
      (let [result (compat/swarm-spawn-shim {:directory "/project/path"
                                             :presets ["coordinator"]
                                             :name "test-ling-spawn"})]
        ;; Should succeed
        (is (not (:isError result)) (str "Unexpected error: " (:text result)))
        (let [parsed (parse-response result)]
          (is (:success parsed))
          (is (= "test-ling-spawn" (:agent-id parsed)))
          ;; JSON returns strings, not keywords
          (is (= "ling" (:type parsed)))
          ;; directory should have been renamed to cwd
          (is (= "/project/path" (:cwd parsed)))
          (is (= ["coordinator"] (:presets parsed))))))))

(deftest swarm-spawn-renames-directory-to-cwd
  (testing "swarm_spawn renames 'directory' param to 'cwd' for agent:spawn"
    ;; We verify param transformation by checking the spawned agent's cwd
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "spawned"})]
      (let [;; Use OLD param name 'directory'
            result (compat/swarm-spawn-shim {"directory" "/home/user/myproject"
                                             "name" "ling-dir-rename"})]
        (is (not (:isError result)))
        (let [parsed (parse-response result)]
          ;; Verify cwd was set from directory
          (is (= "/home/user/myproject" (:cwd parsed))))))))

(deftest swarm-spawn-adds-type-ling-static-param
  (testing "swarm_spawn adds type='ling' static param"
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "spawned"})]
      (let [result (compat/swarm-spawn-shim {:directory "/tmp"})]
        (is (not (:isError result)))
        (let [parsed (parse-response result)]
          ;; Static param type='ling' should be applied
          (is (= "ling" (:type parsed))))))))

(deftest swarm-spawn-emits-deprecation-warning
  (testing "swarm_spawn emits DEPRECATED log warning"
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "spawned"})]
      (let [[_result logs] (capture-logs
                            #(compat/swarm-spawn-shim {:directory "/tmp"}))]
        ;; Should have logged a deprecation warning
        (is (some #(re-find #"DEPRECATED" (str (:msg %))) logs)
            "Should emit DEPRECATED warning")
        (is (some #(re-find #"swarm_spawn" (str (:msg %))) logs)
            "Should mention old tool name")
        (is (some #(re-find #"agent" (str (:msg %))) logs)
            "Should mention new tool")))))

;; =============================================================================
;; swarm_status Integration Tests
;; =============================================================================

(deftest swarm-status-returns-all-agents
  (testing "swarm_status shim returns all agents via agent:status"
    ;; Add test agents
    (add-test-slave! "ling-1" {:depth 1 :status :idle :cwd "/project/a"})
    (add-test-slave! "ling-2" {:depth 1 :status :working :cwd "/project/b"})
    (add-test-slave! "drone-1" {:depth 2 :status :idle :cwd "/project/c"})

    (let [result (compat/swarm-status-shim {})
          parsed (parse-response result)]
      (is (not (:isError result)))
      (is (= 3 (:count parsed)))
      (is (vector? (:agents parsed)))
      ;; Check agent types
      (let [types (map :type (:agents parsed))]
        (is (= 2 (count (filter #(= "ling" %) types))))
        (is (= 1 (count (filter #(= "drone" %) types))))))))

(deftest swarm-status-includes-status-grouping
  (testing "swarm_status returns by-status grouping"
    (add-test-slave! "ling-idle" {:depth 1 :status :idle})
    (add-test-slave! "ling-working" {:depth 1 :status :working})
    (add-test-slave! "ling-working-2" {:depth 1 :status :working})

    (let [result (compat/swarm-status-shim {})
          parsed (parse-response result)]
      (is (not (:isError result)))
      (is (map? (:by-status parsed)))
      ;; Note: JSON returns string keys, timbre formats status as string
      (let [by-status (:by-status parsed)]
        (is (= 1 (or (get by-status "idle") (get by-status :idle))))
        (is (= 2 (or (get by-status "working") (get by-status :working))))))))

(deftest swarm-status-empty-when-no-agents
  (testing "swarm_status returns empty when no agents exist"
    (let [result (compat/swarm-status-shim {})
          parsed (parse-response result)]
      (is (not (:isError result)))
      (is (= 0 (:count parsed)))
      (is (empty? (:agents parsed))))))

;; =============================================================================
;; swarm_kill Integration Tests
;; =============================================================================

(deftest swarm-kill-kills-correct-agent
  (testing "swarm_kill shim kills the specified agent via agent:kill"
    (add-test-slave! "ling-to-kill" {:depth 1 :status :idle})
    (add-test-slave! "ling-to-keep" {:depth 1 :status :working})

    ;; Mock elisp kill response
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "killed"})]
      (let [result (compat/swarm-kill-shim {:slave_id "ling-to-kill"})
            parsed (parse-response result)]
        (is (not (:isError result)))
        (is (:killed? parsed))
        (is (= "ling-to-kill" (:id parsed)))))))

(deftest swarm-kill-renames-slave-id-to-agent-id
  (testing "swarm_kill renames 'slave_id' param to 'agent_id'"
    (add-test-slave! "ling-slave-rename" {:depth 1 :status :idle})

    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "killed"})]
      ;; Use OLD param name 'slave_id' (string key like MCP JSON)
      (let [result (compat/swarm-kill-shim {"slave_id" "ling-slave-rename"})
            parsed (parse-response result)]
        ;; Should have killed the agent despite using old param name
        (is (not (:isError result)))
        (is (:killed? parsed))
        (is (= "ling-slave-rename" (:id parsed)))))))

(deftest swarm-kill-agent-not-found
  (testing "swarm_kill returns error for non-existent agent"
    (let [result (compat/swarm-kill-shim {:slave_id "non-existent"})]
      (is (:isError result))
      (is (re-find #"not found" (:text result))))))

(deftest swarm-kill-missing-slave-id
  (testing "swarm_kill returns error when slave_id not provided"
    (let [result (compat/swarm-kill-shim {})]
      (is (:isError result))
      (is (re-find #"agent_id is required" (:text result))))))

;; =============================================================================
;; swarm_dispatch Integration Tests
;; =============================================================================
;;
;; NOTE: swarm_dispatch shim routes to ORIGINAL handler (not consolidated)
;; because the original has critical coordinator preflight and context injection.
;; These tests mock emacsclient since original handler uses elisp dispatch.

(deftest swarm-dispatch-calls-original-handler-with-message-rename
  (testing "swarm_dispatch renames message->prompt and calls original handler"
    (add-test-slave! "ling-dispatch-test" {:depth 1 :status :idle})

    ;; Must mock swarm-addon-available? AND emacsclient for original handler
    (with-redefs [swarm-core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    {:success true
                     :result "{\"status\":\"dispatched\",\"task-id\":\"task-123\"}"})]
      ;; Use OLD 'message' param - should be renamed to 'prompt' for original handler
      (let [result (compat/swarm-dispatch-shim {"slave_id" "ling-dispatch-test"
                                                "message" "Test task prompt"})
            parsed (parse-response result)]
        (is (not (:isError result)) (str "Unexpected error: " (:text result)))
        (is (= "dispatched" (:status parsed)))
        (is (= "task-123" (:task-id parsed)))))))

(deftest swarm-dispatch-works-with-keyword-params
  (testing "swarm_dispatch works with keyword params (Clojure callers)"
    (add-test-slave! "ling-kw-dispatch" {:depth 1 :status :idle})

    (with-redefs [swarm-core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    {:success true
                     :result "{\"status\":\"dispatched\",\"task-id\":\"task-456\"}"})]
      (let [result (compat/swarm-dispatch-shim {:slave_id "ling-kw-dispatch"
                                                :message "Another task"})
            parsed (parse-response result)]
        (is (not (:isError result)))
        (is (= "dispatched" (:status parsed)))))))

;; =============================================================================
;; lings_available Integration Tests
;; =============================================================================

(deftest lings-available-returns-only-lings
  (testing "lings_available shim returns only ling-type agents"
    (add-test-slave! "ling-a" {:depth 1 :status :idle})
    (add-test-slave! "ling-b" {:depth 1 :status :working})
    (add-test-slave! "drone-x" {:depth 2 :status :idle})

    (let [result (compat/lings-available-shim {})
          parsed (parse-response result)]
      (is (not (:isError result)))
      ;; Should only return lings (depth 1), not drones (depth 2)
      (is (= 2 (:count parsed)))
      (is (every? #(= "ling" (:type %)) (:agents parsed))))))

(deftest lings-available-adds-type-ling-filter
  (testing "lings_available adds type='ling' static param for filtering"
    ;; Empty state
    (let [result (compat/lings-available-shim {})
          parsed (parse-response result)]
      (is (not (:isError result)))
      (is (= 0 (:count parsed)))
      ;; by-type should only show lings (or be empty)
      (is (or (empty? (:by-type parsed))
              (not (contains? (:by-type parsed) "drone")))))))

;; =============================================================================
;; Parameter Rename Integration Tests
;; =============================================================================

(deftest param-rename-directory-to-cwd-integration
  (testing "directory param is renamed to cwd across shim boundary"
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "spawned"})]
      ;; Verify with string key (MCP JSON format)
      (let [result (compat/swarm-spawn-shim {"directory" "/test/path/one"
                                             "name" "dir-test-1"})]
        (is (not (:isError result)))
        (is (= "/test/path/one" (:cwd (parse-response result)))))

      ;; Verify with keyword key (Clojure format)
      (let [result (compat/swarm-spawn-shim {:directory "/test/path/two"
                                             :name "dir-test-2"})]
        (is (not (:isError result)))
        (is (= "/test/path/two" (:cwd (parse-response result))))))))

(deftest param-rename-slave-id-to-agent-id-integration
  (testing "slave_id param is renamed to agent_id across shim boundary"
    (add-test-slave! "test-slave" {:depth 1 :status :idle})

    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "killed"})]
      ;; Verify with string key (MCP JSON format)
      (let [result (compat/swarm-kill-shim {"slave_id" "test-slave"})]
        (is (not (:isError result)))
        (is (= "test-slave" (:id (parse-response result))))))))

(deftest param-rename-message-to-prompt-integration
  (testing "message param is renamed to prompt for dispatch"
    (add-test-slave! "msg-test-ling" {:depth 1 :status :idle})

    ;; Mock swarm-addon AND emacsclient for original handler
    (with-redefs [swarm-core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    {:success true
                     :result "{\"status\":\"dispatched\",\"task-id\":\"msg-task-789\"}"})]
      ;; Use old 'message' param
      (let [result (compat/swarm-dispatch-shim {"slave_id" "msg-test-ling"
                                                "message" "Do this task"})
            parsed (parse-response result)]
        (is (not (:isError result)))
        (is (= "dispatched" (:status parsed)))
        ;; Dispatch succeeded, meaning 'message' was translated to 'prompt'
        (is (= "msg-task-789" (:task-id parsed)))))))

;; =============================================================================
;; Mixed Key Type Tests (MCP Compatibility)
;; =============================================================================

(deftest shims-handle-string-keys-from-mcp
  (testing "shims normalize string keys from MCP JSON to keywords"
    (add-test-slave! "string-key-test" {:depth 1 :status :idle})

    ;; Mock swarm-addon AND emacsclient for original handler
    (with-redefs [swarm-core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    {:success true
                     :result "{\"status\":\"dispatched\",\"task-id\":\"string-key-task\"}"})]
      ;; Test swarm_dispatch with all string keys
      (let [result (compat/swarm-dispatch-shim {"slave_id" "string-key-test"
                                                "message" "Task from MCP"
                                                "files" ["a.clj" "b.clj"]})
            parsed (parse-response result)]
        (is (not (:isError result)))
        (is (= "dispatched" (:status parsed)))))))

(deftest shims-handle-mixed-string-keyword-keys
  (testing "shims handle mix of string and keyword keys"
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "spawned"})]
      ;; Mix of string and keyword keys
      (let [result (compat/swarm-spawn-shim {"directory" "/mixed/path"
                                             :presets ["test"]
                                             "name" "mixed-key-ling"})
            parsed (parse-response result)]
        (is (not (:isError result)))
        (is (= "/mixed/path" (:cwd parsed)))
        (is (= "mixed-key-ling" (:agent-id parsed)))))))

;; =============================================================================
;; Deprecation Warning Integration Tests
;; =============================================================================

(deftest all-agent-shims-emit-deprecation-warnings
  (testing "all agent shims emit deprecation warnings on call"
    (add-test-slave! "warn-test" {:depth 1 :status :idle})

    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "ok"})]
      ;; Test a few shims
      (doseq [[shim-name shim-fn] [["swarm_spawn" compat/swarm-spawn-shim]
                                   ["swarm_status" compat/swarm-status-shim]
                                   ["lings_available" compat/lings-available-shim]]]
        (let [[_result logs] (capture-logs #(shim-fn {:directory "/tmp"
                                                      :slave_id "warn-test"}))]
          (is (some #(re-find #"DEPRECATED" (str (:msg %))) logs)
              (str shim-name " should emit DEPRECATED warning"))
          (is (some #(re-find #"agent" (str (:msg %))) logs)
              (str shim-name " should mention replacement tool")))))))

;; =============================================================================
;; Error Propagation Tests
;; =============================================================================

(deftest shims-propagate-errors-from-handlers
  (testing "shims properly propagate errors from consolidated handlers"
    ;; Test kill with non-existent agent
    (let [result (compat/swarm-kill-shim {:slave_id "does-not-exist"})]
      (is (:isError result))
      (is (string? (:text result))))

    ;; Test dispatch with missing agent (requires swarm-addon AND emacsclient mock)
    ;; Original handler checks swarm addon availability first
    (with-redefs [swarm-core/swarm-addon-available? (constantly true)
                  ec/eval-elisp-with-timeout
                  (fn [_elisp _timeout]
                    {:success true
                     :result "{\"status\":\"error\",\"error\":\"agent-not-found\"}"})]
      (let [result (compat/swarm-dispatch-shim {:slave_id "not-here"
                                                :message "task"})]
        ;; Original handler returns MCP error for error status
        (is (:isError result))))))

;; =============================================================================
;; End-to-End Workflow Tests
;; =============================================================================

(deftest spawn-status-kill-workflow
  (testing "complete workflow: spawn -> status -> kill via shims"
    (with-redefs [ec/eval-elisp-with-timeout (fn [_elisp _timeout]
                                               {:success true :result "ok"})]
      ;; 1. Spawn via shim
      (let [spawn-result (compat/swarm-spawn-shim {:directory "/workflow/test"
                                                   :name "workflow-ling"
                                                   :presets ["worker"]})
            spawn-parsed (parse-response spawn-result)]
        (is (not (:isError spawn-result)))
        (is (:success spawn-parsed))

        ;; 2. Add to DataScript (ling spawn via elisp doesn't auto-add to DS)
        (add-test-slave! "workflow-ling" {:depth 1 :status :idle :cwd "/workflow/test"})

        ;; 3. Check status via shim
        (let [status-result (compat/swarm-status-shim {})
              status-parsed (parse-response status-result)]
          (is (not (:isError status-result)))
          (is (>= (:count status-parsed) 1))
          (is (some #(= "workflow-ling" (:id %)) (:agents status-parsed))))

        ;; 4. Kill via shim
        (let [kill-result (compat/swarm-kill-shim {:slave_id "workflow-ling"})
              kill-parsed (parse-response kill-result)]
          (is (not (:isError kill-result)))
          (is (:killed? kill-parsed))
          (is (= "workflow-ling" (:id kill-parsed))))))))

;; =============================================================================
;; Consolidated Handler Resolution Tests
;; =============================================================================

(deftest shims-resolve-agent-handler
  (testing "shims correctly resolve :agent consolidated handler"
    ;; Verify handler is resolvable
    (let [handler (compat/get-consolidated-handler :agent)]
      (is (fn? handler) "Should resolve to a function")
      (is (= agent/handle-agent handler)))))

(deftest shims-handle-missing-handler-gracefully
  (testing "shims return error for missing handler"
    ;; Create a shim pointing to non-existent handler
    (let [bad-shim (compat/make-shim "test_bad" :nonexistent "cmd")]
      (let [result (bad-shim {:foo "bar"})]
        (is (:isError result))
        (is (re-find #"not found" (:text result)))))))
