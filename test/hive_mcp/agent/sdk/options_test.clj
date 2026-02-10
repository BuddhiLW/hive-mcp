(ns hive-mcp.agent.sdk.options-test
  "Tests for SDK options construction (build-options-obj, build-base-options-obj).

   Strategy: Mock all phase-hooks/* functions with with-redefs since they
   do Python interop. agent-options is mocked to return its input map,
   allowing us to inspect the exact options passed to the Python layer.

   Tests verify:
   - Correct SAA phase config lookup and prompt suffix appending
   - Conditional field assembly via cond-> (cwd, resume, agents, hooks)
   - Delegation to phase-hooks/agents-dict, hooks-for-phase, hooks-for-base
   - build-base-options-obj always uses :act phase config"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.sdk.options :as options]
            [hive-mcp.agent.sdk.phase-hooks :as phase-hooks]
            [hive-mcp.agent.sdk.saa :as saa]
            [clojure.string :as str]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(def mock-agents-dict {"researcher" :mock-agent-def})
(def mock-hooks {"PostToolUse" [:mock-matcher]})
(def mock-auto-obs-hooks {"PostToolUse" [:mock-auto-obs]})

(defmacro with-phase-hooks-mocks
  "Bind all phase-hooks fns to controllable stubs.
   agent-options returns its input map (identity), enabling assertion on
   the exact options map that would be passed to Python.
   agents-dict, hooks-for-phase, hooks-for-base return configurable values."
  [{:keys [agents-dict-ret hooks-for-phase-ret hooks-for-base-ret]} & body]
  `(with-redefs [phase-hooks/agent-options   identity
                 phase-hooks/agents-dict     (constantly ~agents-dict-ret)
                 phase-hooks/hooks-for-phase (constantly ~hooks-for-phase-ret)
                 phase-hooks/hooks-for-base  (constantly ~hooks-for-base-ret)]
     ~@body))

;; =============================================================================
;; build-options-obj Tests
;; =============================================================================

(deftest build-options-obj-silence-phase-full-opts
  (testing "silence phase with all opts produces correct options map"
    (with-phase-hooks-mocks {:agents-dict-ret    mock-agents-dict
                             :hooks-for-phase-ret mock-hooks}
      (let [opts   {:cwd "/tmp/project"
                    :system-prompt "You are a ling."
                    :session-id "sess-123"
                    :agents {"researcher" {:description "does research"}}
                    :agent-id "ling-1"
                    :compressed-context "prior context"}
            result (options/build-options-obj :silence opts)]
        (is (map? result) "agent-options mock returns the map")
        (is (= (:allowed_tools result)
               (get-in saa/saa-phases [:silence :allowed-tools]))
            "Should use silence phase allowed tools")
        (is (= (:permission_mode result)
               (get-in saa/saa-phases [:silence :permission-mode]))
            "Should use silence phase permission mode")
        (is (= "/tmp/project" (:cwd result))
            "Should include cwd when present")
        (is (= "sess-123" (:resume result))
            "Should include resume (session-id) when present")
        (is (= mock-agents-dict (:agents result))
            "Should include agents dict when non-nil")
        (is (= mock-hooks (:hooks result))
            "Should include hooks when non-nil")))))

(deftest build-options-obj-abstract-phase
  (testing "abstract phase uses correct config"
    (with-phase-hooks-mocks {:agents-dict-ret    nil
                             :hooks-for-phase-ret mock-hooks}
      (let [result (options/build-options-obj :abstract
                     {:system-prompt "Base prompt"
                      :cwd "/tmp/proj"})]
        (is (= (get-in saa/saa-phases [:abstract :allowed-tools])
               (:allowed_tools result))
            "Should use abstract phase allowed tools")
        (is (= "bypassPermissions" (:permission_mode result))
            "Abstract phase uses bypassPermissions")
        (is (nil? (:agents result))
            "Should not include agents key when agents-dict returns nil")))))

(deftest build-options-obj-act-phase
  (testing "act phase uses full tool access"
    (with-phase-hooks-mocks {:agents-dict-ret    mock-agents-dict
                             :hooks-for-phase-ret nil}
      (let [result (options/build-options-obj :act
                     {:cwd "/tmp/proj"
                      :system-prompt "Act now"
                      :agents {"worker" {:description "impl"}}})]
        (is (= (get-in saa/saa-phases [:act :allowed-tools])
               (:allowed_tools result))
            "Should use act phase allowed tools (includes Edit, Write, Bash)")
        (is (= "acceptEdits" (:permission_mode result))
            "Act phase uses acceptEdits")
        (is (nil? (:hooks result))
            "Should not include hooks key when hooks-for-phase returns nil")))))

(deftest build-options-obj-prompt-construction
  (testing "system prompt is concatenated with phase suffix"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-phase-ret nil}
      (let [result (options/build-options-obj :silence
                     {:system-prompt "Hello."})]
        (is (str/includes? (:system_prompt result) "Hello.")
            "Should contain the original system prompt")
        (is (str/includes? (:system_prompt result)
                           (get-in saa/saa-phases [:silence :system-prompt-suffix]))
            "Should contain the phase suffix")
        (is (str/starts-with? (:system_prompt result) "Hello.")
            "Original prompt should come first")))))

(deftest build-options-obj-nil-system-prompt
  (testing "nil system-prompt defaults to empty string before suffix"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-phase-ret nil}
      (let [result (options/build-options-obj :act {:system-prompt nil})]
        (is (string? (:system_prompt result))
            "System prompt should be a string")
        (is (str/includes? (:system_prompt result)
                           (get-in saa/saa-phases [:act :system-prompt-suffix]))
            "Should still contain the phase suffix")
        (is (str/starts-with? (:system_prompt result) "\n\n")
            "Should start with separator when prompt is nil (empty string + newlines)")))))

(deftest build-options-obj-no-cwd
  (testing "cwd key omitted when nil"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-phase-ret nil}
      (let [result (options/build-options-obj :silence
                     {:system-prompt "test"})]
        (is (not (contains? result :cwd))
            "Should not include :cwd key when cwd is nil")))))

(deftest build-options-obj-no-session-id
  (testing "resume key omitted when session-id is nil"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-phase-ret nil}
      (let [result (options/build-options-obj :silence
                     {:system-prompt "test"
                      :cwd "/tmp"})]
        (is (not (contains? result :resume))
            "Should not include :resume key when session-id is nil")))))

(deftest build-options-obj-empty-agents
  (testing "agents key omitted when agents map is empty"
    (with-phase-hooks-mocks {:agents-dict-ret nil  ;; agents-dict returns nil for empty
                             :hooks-for-phase-ret nil}
      (let [result (options/build-options-obj :act
                     {:system-prompt "test"
                      :agents {}})]
        (is (not (contains? result :agents))
            "Should not include :agents key when agents-dict returns nil")))))

(deftest build-options-obj-delegates-to-hooks-for-phase
  (testing "hooks-for-phase is called with correct arguments"
    (let [captured (atom nil)]
      (with-redefs [phase-hooks/agent-options   identity
                    phase-hooks/agents-dict     (constantly nil)
                    phase-hooks/hooks-for-phase (fn [phase opts phase-config]
                                                  (reset! captured {:phase phase
                                                                    :opts opts
                                                                    :phase-config phase-config})
                                                  mock-hooks)]
        (let [opts {:cwd "/proj" :system-prompt "sp" :agent-id "a1"
                    :compressed-context "ctx"}]
          (options/build-options-obj :silence opts)
          (is (= :silence (:phase @captured))
              "Should pass the phase keyword")
          (is (= opts (:opts @captured))
              "Should pass the full opts map")
          (is (= (get saa/saa-phases :silence) (:phase-config @captured))
              "Should pass the phase config from saa-phases"))))))

(deftest build-options-obj-delegates-to-agents-dict
  (testing "agents-dict is called with the agents from opts"
    (let [captured (atom nil)]
      (with-redefs [phase-hooks/agent-options   identity
                    phase-hooks/agents-dict     (fn [agents]
                                                  (reset! captured agents)
                                                  nil)
                    phase-hooks/hooks-for-phase (constantly nil)]
        (let [agents-map {"worker" {:description "impl"}}]
          (options/build-options-obj :act {:agents agents-map})
          (is (= agents-map @captured)
              "Should pass the agents map to agents-dict"))))))

;; =============================================================================
;; build-base-options-obj Tests
;; =============================================================================

(deftest build-base-options-obj-full-opts
  (testing "base options with all fields produces correct map"
    (with-phase-hooks-mocks {:agents-dict-ret    mock-agents-dict
                             :hooks-for-base-ret mock-auto-obs-hooks}
      (let [result (options/build-base-options-obj
                     {:cwd "/tmp/project"
                      :system-prompt "You are persistent."
                      :session-id "sess-456"
                      :agents {"worker" {:description "impl"}}})]
        (is (map? result))
        (is (= (get-in saa/saa-phases [:act :allowed-tools])
               (:allowed_tools result))
            "Should always use :act phase allowed tools")
        (is (= "acceptEdits" (:permission_mode result))
            "Should always use :act permission mode")
        (is (= "You are persistent." (:system_prompt result))
            "Should use system-prompt directly (no suffix for base)")
        (is (= "/tmp/project" (:cwd result)))
        (is (= "sess-456" (:resume result)))
        (is (= mock-agents-dict (:agents result)))
        (is (= mock-auto-obs-hooks (:hooks result)))))))

(deftest build-base-options-obj-always-uses-act-config
  (testing "base options always use :act phase config regardless of input"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-base-ret nil}
      (let [result (options/build-base-options-obj
                     {:system-prompt "test"})]
        (is (= (get-in saa/saa-phases [:act :allowed-tools])
               (:allowed_tools result))
            "Must use act tools (most permissive)")
        (is (= (get-in saa/saa-phases [:act :permission-mode])
               (:permission_mode result))
            "Must use act permission mode")))))

(deftest build-base-options-obj-no-prompt-suffix
  (testing "base options do NOT append SAA phase suffix to prompt"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-base-ret nil}
      (let [result (options/build-base-options-obj
                     {:system-prompt "Custom prompt only."})]
        (is (= "Custom prompt only." (:system_prompt result))
            "Should use exact system-prompt without phase suffix")
        (is (not (str/includes? (:system_prompt result) "ACT phase"))
            "Should NOT contain SAA phase text")))))

(deftest build-base-options-obj-nil-system-prompt
  (testing "nil system-prompt defaults to empty string"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-base-ret nil}
      (let [result (options/build-base-options-obj
                     {:system-prompt nil})]
        (is (= "" (:system_prompt result))
            "nil system-prompt should become empty string")))))

(deftest build-base-options-obj-no-cwd
  (testing "cwd omitted when nil"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-base-ret nil}
      (let [result (options/build-base-options-obj
                     {:system-prompt "test"})]
        (is (not (contains? result :cwd))
            "Should not include :cwd when nil")))))

(deftest build-base-options-obj-no-session-id
  (testing "resume omitted when session-id is nil"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-base-ret nil}
      (let [result (options/build-base-options-obj
                     {:system-prompt "test"
                      :cwd "/tmp"})]
        (is (not (contains? result :resume))
            "Should not include :resume when session-id is nil")))))

(deftest build-base-options-obj-no-agents
  (testing "agents omitted when agents-dict returns nil"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-base-ret nil}
      (let [result (options/build-base-options-obj
                     {:system-prompt "test"
                      :agents {}})]
        (is (not (contains? result :agents))
            "Should not include :agents when agents-dict returns nil")))))

(deftest build-base-options-obj-no-hooks
  (testing "hooks omitted when hooks-for-base returns nil"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-base-ret nil}
      (let [result (options/build-base-options-obj
                     {:cwd "/tmp"
                      :system-prompt "test"})]
        (is (not (contains? result :hooks))
            "Should not include :hooks when hooks-for-base returns nil")))))

(deftest build-base-options-obj-delegates-to-hooks-for-base
  (testing "hooks-for-base is called with cwd"
    (let [captured (atom nil)]
      (with-redefs [phase-hooks/agent-options  identity
                    phase-hooks/agents-dict    (constantly nil)
                    phase-hooks/hooks-for-base (fn [cwd]
                                                 (reset! captured cwd)
                                                 mock-auto-obs-hooks)]
        (options/build-base-options-obj {:cwd "/my/project"
                                         :system-prompt "test"})
        (is (= "/my/project" @captured)
            "Should pass cwd to hooks-for-base")))))

(deftest build-base-options-obj-delegates-to-agents-dict
  (testing "agents-dict receives agents from opts"
    (let [captured (atom nil)]
      (with-redefs [phase-hooks/agent-options  identity
                    phase-hooks/agents-dict    (fn [agents]
                                                 (reset! captured agents)
                                                 mock-agents-dict)
                    phase-hooks/hooks-for-base (constantly nil)]
        (let [agents-map {"analyzer" {:description "analyzes"}}]
          (options/build-base-options-obj {:agents agents-map
                                           :system-prompt "test"})
          (is (= agents-map @captured)
              "Should pass agents map to agents-dict"))))))

;; =============================================================================
;; Edge Cases & Comparison Tests
;; =============================================================================

(deftest build-options-vs-base-prompt-handling
  (testing "build-options-obj appends suffix, build-base-options-obj does not"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-phase-ret nil
                             :hooks-for-base-ret nil}
      (let [prompt "Shared prompt."
            phased (options/build-options-obj :act {:system-prompt prompt})
            base   (options/build-base-options-obj {:system-prompt prompt})]
        (is (> (count (:system_prompt phased))
               (count (:system_prompt base)))
            "Phased options should have longer prompt (includes suffix)")
        (is (str/includes? (:system_prompt phased)
                           (get-in saa/saa-phases [:act :system-prompt-suffix]))
            "Phased prompt should include act phase suffix")
        (is (= prompt (:system_prompt base))
            "Base prompt should be exact (no suffix)")))))

(deftest all-saa-phases-produce-valid-options
  (testing "every SAA phase in saa-phases can build options"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-phase-ret nil}
      (doseq [phase (keys saa/saa-phases)]
        (let [result (options/build-options-obj phase
                       {:system-prompt "test"
                        :cwd "/tmp"})]
          (is (map? result) (str "Phase " phase " should produce a map"))
          (is (contains? result :allowed_tools)
              (str "Phase " phase " should have allowed_tools"))
          (is (contains? result :permission_mode)
              (str "Phase " phase " should have permission_mode"))
          (is (contains? result :system_prompt)
              (str "Phase " phase " should have system_prompt")))))))

(deftest minimal-opts-still-produce-valid-options
  (testing "both builders work with minimal opts (only system-prompt)"
    (with-phase-hooks-mocks {:agents-dict-ret nil
                             :hooks-for-phase-ret nil
                             :hooks-for-base-ret nil}
      (let [phased (options/build-options-obj :silence {:system-prompt "min"})
            base   (options/build-base-options-obj {:system-prompt "min"})]
        (is (map? phased) "Phased with minimal opts should work")
        (is (map? base) "Base with minimal opts should work")
        (is (= 3 (count (keys phased)))
            "Minimal phased should have exactly 3 keys: allowed_tools, permission_mode, system_prompt")
        (is (= 3 (count (keys base)))
            "Minimal base should have exactly 3 keys: allowed_tools, permission_mode, system_prompt")))))
