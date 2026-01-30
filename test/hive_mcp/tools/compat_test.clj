(ns hive-mcp.tools.compat-test
  "Tests for backward-compatibility deprecation shims.

   Test Coverage:
   1. make-shim - Factory function creates working shims
   2. Deprecation warnings - Shims emit DEPRECATED log messages
   3. Parameter renaming - Old param names translated to new ones
   4. Static params - Shims merge static params into calls
   5. Metadata - Shims carry deprecation metadata
   6. Utility functions - deprecated?, sunset-date, migration-info
   7. Shim registry - All shims defined and registered
   8. Tool definitions - MCP tool defs have correct structure"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.compat :as compat]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Helpers
;; =============================================================================

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

(defn mock-handler
  "Create a mock handler that records calls."
  [received-atom return-val]
  (fn [params]
    (reset! received-atom params)
    return-val))

;; =============================================================================
;; make-shim Factory Tests
;; =============================================================================

(deftest make-shim-emits-deprecation-warning
  (testing "make-shim creates shim that emits DEPRECATED warning on call"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old_tool" :test "new_cmd")]
      ;; Inject mock handlers
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (let [[result logs] (capture-logs #(shim {:foo "bar"}))
              log-msgs (map :msg logs)]
          ;; Should have logged a deprecation warning
          (is (some #(re-find #"DEPRECATED" (str %)) log-msgs)
              "Should emit DEPRECATED in warning")
          (is (some #(re-find #"old_tool" (str %)) log-msgs)
              "Should mention old tool name")
          (is (some #(re-find #"test" (str %)) log-msgs)
              "Should mention new tool name")
          (is (some #(re-find #"new_cmd" (str %)) log-msgs)
              "Should mention new command")
          (is (some #(re-find #"2026" (str %)) log-msgs)
              "Should mention sunset date"))))))

(deftest make-shim-translates-params
  (testing "make-shim translates old param names to new ones via :param-rename"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old_tool" :test "cmd"
                                 :param-rename {:old_key :new_key
                                                :slave_id :agent_id})]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (shim {:old_key "value1" :slave_id "ling-1" :unchanged "kept"})

        ;; Check param translations
        (is (= "value1" (:new_key @received))
            "old_key should be renamed to new_key")
        (is (= "ling-1" (:agent_id @received))
            "slave_id should be renamed to agent_id")
        (is (= "kept" (:unchanged @received))
            "Untranslated params should pass through")
        (is (not (contains? @received :old_key))
            "Old param name should not be present")
        (is (not (contains? @received :slave_id))
            "Old param name should not be present")))))

(deftest make-shim-merges-static-params
  (testing "make-shim merges :static-params into every call"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old_tool" :test "cmd"
                                 :static-params {:type "ling"
                                                 :source "shim"})]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (shim {:name "foo" :cwd "/tmp"})

        ;; Check static params merged
        (is (= "ling" (:type @received))
            "Static type param should be merged")
        (is (= "shim" (:source @received))
            "Static source param should be merged")
        (is (= "foo" (:name @received))
            "User params should be preserved")
        (is (= "/tmp" (:cwd @received))
            "User params should be preserved")))))

(deftest make-shim-adds-command-param
  (testing "make-shim adds :command param for consolidated handler routing"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old_tool" :test "traverse")]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (shim {:node_id "test-node"})

        (is (= "traverse" (:command @received))
            "Shim should add command param for handler routing")))))

(deftest make-shim-combines-all-transformations
  (testing "make-shim correctly combines param-rename, static-params, and command"
    (let [received (atom nil)
          mock-handlers {:agent (mock-handler received {:success true})}
          ;; This mimics the swarm_spawn shim configuration
          shim (compat/make-shim "swarm_spawn" :agent "spawn"
                                 :param-rename {:slave_id :agent_id}
                                 :static-params {:type "ling"})]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (shim {:slave_id "ling-1" :cwd "/project" :presets ["coordinator"]})

        ;; All transformations should be applied
        (is (= "spawn" (:command @received)) "Command should be set")
        (is (= "ling" (:type @received)) "Static param should be merged")
        (is (= "ling-1" (:agent_id @received)) "slave_id should be renamed to agent_id")
        (is (= "/project" (:cwd @received)) "Other params preserved")
        (is (= ["coordinator"] (:presets @received)) "Other params preserved")
        (is (not (contains? @received :slave_id)) "Old param name gone")))))

(deftest make-shim-delegates-to-consolidated-handler
  (testing "make-shim delegates to correct consolidated handler and returns result"
    (let [received (atom nil)
          expected-result {:success true :data "test-result"}
          mock-handlers {:kg (mock-handler received expected-result)}
          shim (compat/make-shim "kg_traverse" :kg "traverse")]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (let [result (shim {:node_id "test-node"})]
          (is (= expected-result result)
              "Shim should return result from consolidated handler")
          (is (= "test-node" (:node_id @received))
              "Shim should pass params to handler"))))))

(deftest make-shim-handles-missing-handler
  (testing "make-shim returns error when consolidated handler not found"
    (with-redefs [compat/get-consolidated-handler (constantly nil)]
      (let [shim (compat/make-shim "old_tool" :nonexistent "cmd")
            result (shim {:foo "bar"})]
        (is (:isError result)
            "Should return error when handler not found")
        (is (re-find #"not found" (:text result))
            "Error should mention handler not found")
        (is (re-find #"nonexistent" (:text result))
            "Error should mention tool name")))))

;; =============================================================================
;; Shim Metadata Tests
;; =============================================================================

(deftest make-shim-attaches-metadata
  (testing "make-shim attaches deprecation metadata to shim function"
    (let [shim (compat/make-shim "old_tool" :new-tool "new-cmd"
                                 :param-rename {:old :new})]
      (is (true? (:deprecated (meta shim)))
          "Should have :deprecated metadata")
      (is (= "2026-04-01" (:sunset-date (meta shim)))
          "Should have :sunset-date metadata")
      (is (= "old_tool" (:old-name (meta shim)))
          "Should have :old-name metadata")
      (is (= :new-tool (:new-tool (meta shim)))
          "Should have :new-tool metadata")
      (is (= "new-cmd" (:new-cmd (meta shim)))
          "Should have :new-cmd metadata")
      (is (= {:old :new} (:param-rename (meta shim)))
          "Should have :param-rename metadata"))))

;; =============================================================================
;; Utility Function Tests
;; =============================================================================

(deftest deprecated?-test
  (testing "deprecated? returns true for shims"
    (let [shim (compat/make-shim "old" :new "cmd")]
      (is (true? (compat/deprecated? shim))
          "Shims should be marked deprecated")))

  (testing "deprecated? returns false for regular functions"
    (is (false? (compat/deprecated? identity))
        "Regular functions are not deprecated")
    (is (false? (compat/deprecated? (fn [x] x)))
        "Anonymous functions are not deprecated")))

(deftest sunset-date-test
  (testing "sunset-date returns date for shims"
    (let [shim (compat/make-shim "old" :new "cmd")]
      (is (= "2026-04-01" (compat/sunset-date shim))
          "Should return sunset date from metadata")))

  (testing "sunset-date returns nil for regular functions"
    (is (nil? (compat/sunset-date identity))
        "Regular functions have no sunset date")))

(deftest migration-info-test
  (testing "migration-info returns full info for shims"
    (let [shim (compat/make-shim "kg_traverse" :kg "traverse"
                                 :param-rename {:start :node_id})
          info (compat/migration-info shim)]
      (is (= "kg_traverse" (:old-name info)))
      (is (= :kg (:new-tool info)))
      (is (= "traverse" (:new-cmd info)))
      (is (= {:start :node_id} (:param-rename info)))))

  (testing "migration-info returns nil for regular functions"
    (is (nil? (compat/migration-info identity)))))

;; =============================================================================
;; Shim Registry Tests
;; =============================================================================

(deftest shims-registry-contains-expected-tools
  (testing "shims registry contains all expected deprecated tools"
    ;; Agent/Swarm shims
    (is (contains? compat/shims "swarm_spawn"))
    (is (contains? compat/shims "swarm_status"))
    (is (contains? compat/shims "swarm_kill"))
    (is (contains? compat/shims "swarm_dispatch"))
    (is (contains? compat/shims "lings_available"))
    (is (contains? compat/shims "swarm_list_presets"))
    (is (contains? compat/shims "swarm_collect"))
    (is (contains? compat/shims "swarm_broadcast"))

    ;; Memory shims
    (is (contains? compat/shims "mcp_memory_add"))
    (is (contains? compat/shims "mcp_memory_query"))
    (is (contains? compat/shims "mcp_memory_get_full"))
    (is (contains? compat/shims "mcp_memory_search_semantic"))
    (is (contains? compat/shims "mcp_memory_promote"))
    (is (contains? compat/shims "mcp_memory_demote"))
    (is (contains? compat/shims "mcp_memory_feedback"))

    ;; Knowledge Graph shims
    (is (contains? compat/shims "kg_traverse"))
    (is (contains? compat/shims "kg_add_edge"))
    (is (contains? compat/shims "kg_impact_analysis"))
    (is (contains? compat/shims "kg_stats"))

    ;; Hivemind shims
    (is (contains? compat/shims "hivemind_shout"))
    (is (contains? compat/shims "hivemind_ask"))
    (is (contains? compat/shims "hivemind_status"))

    ;; Magit shims
    (is (contains? compat/shims "magit_status"))
    (is (contains? compat/shims "magit_commit"))
    (is (contains? compat/shims "magit_push"))))

(deftest shims-registry-all-are-deprecated
  (testing "all shims in registry are marked deprecated"
    (doseq [[name handler] compat/shims]
      (is (compat/deprecated? handler)
          (str "Shim " name " should be marked deprecated")))))

(deftest shim-count-test
  (testing "shim-count returns correct count"
    (is (= (count compat/shims) (compat/shim-count))
        "shim-count should match actual count")))

(deftest list-deprecated-tools-test
  (testing "list-deprecated-tools returns migration info for all shims"
    (let [deprecated-list (compat/list-deprecated-tools)]
      (is (= (count compat/shims) (count deprecated-list))
          "Should list all shims")

      ;; Check structure of entries
      (doseq [entry deprecated-list]
        (is (string? (:old-name entry))
            "Each entry should have old-name")
        (is (string? (:new-tool entry))
            "Each entry should have new-tool")
        (is (string? (:new-cmd entry))
            "Each entry should have new-cmd")
        (is (= "2026-04-01" (:sunset entry))
            "Each entry should have sunset date")))))

;; =============================================================================
;; Pre-defined Shim Tests
;; =============================================================================

(deftest swarm-spawn-shim-configuration
  (testing "swarm-spawn-shim has correct configuration"
    (let [info (compat/migration-info compat/swarm-spawn-shim)]
      (is (= "swarm_spawn" (:old-name info)))
      (is (= :agent (:new-tool info)))
      (is (= "spawn" (:new-cmd info)))))

  (testing "swarm-spawn-shim renames directory to cwd"
    (let [received (atom nil)
          mock-handlers {:agent (mock-handler received {:ok true})}]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (compat/swarm-spawn-shim {:directory "/project/path" :presets ["coordinator"]})

        (is (= "/project/path" (:cwd @received))
            "directory should be renamed to cwd")
        (is (not (contains? @received :directory))
            "directory should not be present")
        (is (= "ling" (:type @received))
            "static type param should be set")
        (is (= ["coordinator"] (:presets @received))
            "other params should pass through")))))

(deftest swarm-dispatch-shim-renames-slave-id
  (testing "swarm-dispatch-shim renames slave_id to agent_id"
    (let [received (atom nil)
          mock-handlers {:agent (mock-handler received {:ok true})}]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (compat/swarm-dispatch-shim {:slave_id "ling-1" :prompt "test"})

        (is (= "ling-1" (:agent_id @received))
            "slave_id should be renamed to agent_id")
        (is (not (contains? @received :slave_id))
            "slave_id should not be present")))))

(deftest lings-available-shim-adds-type
  (testing "lings-available-shim adds type=ling static param"
    (let [received (atom nil)
          mock-handlers {:agent (mock-handler received {:ok true})}]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (compat/lings-available-shim {})

        (is (= "ling" (:type @received))
            "Should add type=ling static param")
        (is (= "list" (:command @received))
            "Should route to list command")))))

;; =============================================================================
;; MCP Tool Definition Tests
;; =============================================================================

(deftest tools-vector-has-correct-count
  (testing "tools vector has same count as shims map"
    (is (= (count compat/shims) (count compat/tools))
        "Each shim should have a tool definition")))

(deftest tool-definitions-have-required-fields
  (testing "each tool definition has required MCP fields"
    (doseq [tool-def compat/tools]
      (is (string? (:name tool-def))
          (str "Tool should have name: " tool-def))
      (is (true? (:deprecated tool-def))
          (str "Tool should be marked deprecated: " (:name tool-def)))
      (is (= "2026-04-01" (:sunset-date tool-def))
          (str "Tool should have sunset-date: " (:name tool-def)))
      (is (map? (:replacement tool-def))
          (str "Tool should have replacement info: " (:name tool-def)))
      (is (string? (:description tool-def))
          (str "Tool should have description: " (:name tool-def)))
      (is (map? (:inputSchema tool-def))
          (str "Tool should have inputSchema: " (:name tool-def)))
      (is (fn? (:handler tool-def))
          (str "Tool should have handler function: " (:name tool-def))))))

(deftest tool-definitions-descriptions-mention-deprecated
  (testing "tool descriptions mention DEPRECATED"
    (doseq [tool-def compat/tools]
      (is (re-find #"DEPRECATED" (:description tool-def))
          (str "Description should mention DEPRECATED: " (:name tool-def))))))

(deftest tool-definitions-replacement-info
  (testing "tool definitions have correct replacement info"
    (let [tool-by-name (into {} (map (juxt :name identity) compat/tools))]
      ;; Check a few specific tools
      (let [kg-traverse (get tool-by-name "kg_traverse")]
        (is (= "kg" (get-in kg-traverse [:replacement :tool])))
        (is (= "traverse" (get-in kg-traverse [:replacement :command]))))

      (let [swarm-spawn (get tool-by-name "swarm_spawn")]
        (is (= "agent" (get-in swarm-spawn [:replacement :tool])))
        (is (= "spawn" (get-in swarm-spawn [:replacement :command]))))

      (let [memory-add (get tool-by-name "mcp_memory_add")]
        (is (= "memory" (get-in memory-add [:replacement :tool])))
        (is (= "add" (get-in memory-add [:replacement :command])))))))

(deftest tool-definitions-input-schema-permissive
  (testing "tool inputSchemas allow additional properties for passthrough"
    (doseq [tool-def compat/tools]
      (let [schema (:inputSchema tool-def)]
        (is (= "object" (:type schema))
            (str "Schema should be object type: " (:name tool-def)))
        (is (true? (:additionalProperties schema))
            (str "Schema should allow additionalProperties: " (:name tool-def)))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest make-shim-handles-empty-params
  (testing "make-shim handles empty params map"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old" :test "cmd")]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (shim {})
        (is (= {:command "cmd"} @received)
            "Empty params should just have command")))))

(deftest make-shim-handles-nil-values
  (testing "make-shim preserves nil values in params"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old" :test "cmd")]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        (shim {:key nil})
        (is (contains? @received :key)
            "Should preserve nil-valued keys")
        (is (nil? (:key @received))
            "Nil value should be preserved")))))

(deftest make-shim-user-params-override-static
  (testing "user params should override static params"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old" :test "cmd"
                                 :static-params {:type "default"})]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        ;; User provides their own type
        (shim {:type "custom"})
        (is (= "custom" (:type @received))
            "User params should override static params")))))

(deftest param-rename-with-missing-key
  (testing "param-rename gracefully handles missing keys"
    (let [received (atom nil)
          mock-handlers {:test (mock-handler received {:ok true})}
          shim (compat/make-shim "old" :test "cmd"
                                 :param-rename {:missing :renamed})]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        ;; Call without the key that would be renamed
        (shim {:other "value"})
        (is (= "value" (:other @received))
            "Other params should pass through")
        (is (not (contains? @received :missing))
            "Missing source key should not create entry")
        (is (not (contains? @received :renamed))
            "Renamed key should not exist without source")))))

;; =============================================================================
;; String Key Handling Tests (MCP JSON compatibility)
;; =============================================================================

(deftest make-shim-handles-string-keys
  (testing "param-rename works with string keys from MCP JSON"
    (let [received (atom nil)
          mock-handlers {:agent (mock-handler received {:ok true})}
          shim (compat/make-shim "swarm_kill" :agent "kill"
                                 :param-rename {:slave_id :agent_id})]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        ;; MCP sends string keys from JSON parsing
        (shim {"slave_id" "ling-123" "force" true})

        (is (= "ling-123" (:agent_id @received))
            "String slave_id should be renamed to keyword :agent_id")
        (is (not (contains? @received :slave_id))
            "Old key should not be present")
        (is (not (contains? @received "slave_id"))
            "String key should not be present")
        (is (true? (:force @received))
            "Other string keys should become keywords")))))

(deftest make-shim-handles-mixed-keys
  (testing "param-rename handles mixed string and keyword keys"
    (let [received (atom nil)
          mock-handlers {:agent (mock-handler received {:ok true})}
          shim (compat/make-shim "swarm_dispatch" :agent "dispatch"
                                 :param-rename {:slave_id :agent_id
                                                :message :prompt})]
      (with-redefs [compat/get-consolidated-handler (fn [tool-name]
                                                      (get mock-handlers tool-name))]
        ;; Mixed: some string, some keyword (edge case)
        (shim {"slave_id" "ling-1" :message "test prompt"})

        (is (= "ling-1" (:agent_id @received))
            "String key should be normalized and renamed")
        (is (= "test prompt" (:prompt @received))
            "Keyword key should be renamed")
        (is (= "dispatch" (:command @received))
            "Command should be set")))))
