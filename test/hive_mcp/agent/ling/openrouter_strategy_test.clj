(ns hive-mcp.agent.ling.openrouter-strategy-test
  "Tests for OpenRouter ling strategy.

   Tests cover:
   1. Session lifecycle (spawn, status, kill)
   2. Session registry management
   3. Ring buffer integration
   4. Conversation history tracking
   5. Dispatch mechanics (mocked HTTP)
   6. Error handling (missing API key, duplicate ID)
   7. Integration with Ling record (spawn-mode routing)

   Note: HTTP calls are mocked to avoid real API calls in tests.
   The strategy is tested at the session management + ring buffer level."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.openrouter-strategy :as or-strat]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.queries :as ds-queries]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn with-fresh-state [f]
  "Reset DataScript and kill all OpenRouter sessions between tests."
  (ds/reset-conn!)
  (or-strat/kill-all-openrouter!)
  (f)
  (or-strat/kill-all-openrouter!))

(use-fixtures :each with-fresh-state)

(defn gen-ling-id []
  (str "test-or-" (java.util.UUID/randomUUID)))

;; =============================================================================
;; Strategy Factory Tests
;; =============================================================================

(deftest factory-creates-strategy
  (testing "->openrouter-strategy creates an OpenRouterStrategy"
    (let [strat (or-strat/->openrouter-strategy)]
      (is (some? strat))
      (is (satisfies? strategy/ILingStrategy strat)))))

;; =============================================================================
;; Session Spawn Tests
;; =============================================================================

(deftest spawn-creates-session
  (testing "strategy-spawn! creates a session in registry"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets ["worker"]}]
      ;; Mock API key via env (or provide directly)
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (let [result (strategy/strategy-spawn! strat ctx {})]
          (is (= ling-id result) "Should return ling-id")
          ;; Session should be in registry
          (is (or-strat/openrouter-session? ling-id))
          (let [session (or-strat/get-session ling-id)]
            (is (some? session))
            (is (= "deepseek/deepseek-chat" (:model session)))
            (is (true? (:alive? session)))
            (is (= "/tmp" (:cwd session)))
            (is (= 1 (:message-count session)) "Should have system prompt")
            (is (zero? (:request-count session)))))))))

(deftest spawn-with-initial-task
  (testing "strategy-spawn! with task dispatches immediately"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "meta-llama/llama-3.3-70b"
               :project-id "test" :presets []}
          dispatched? (atom false)]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")
                    or-strat/dispatch-async! (fn [id _msg]
                                               (reset! dispatched? true)
                                               (is (= ling-id id))
                                               nil)]
        (strategy/strategy-spawn! strat ctx {:task "Hello, what model are you?"})
        (is @dispatched? "Should have dispatched initial task")
        ;; Session should exist
        (is (or-strat/openrouter-session? ling-id))))))

(deftest spawn-duplicate-id-throws
  (testing "strategy-spawn! with duplicate ID throws"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"already exists"
                              (strategy/strategy-spawn! strat ctx {})))))))

(deftest spawn-missing-api-key-throws
  (testing "strategy-spawn! without API key throws"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}]
      ;; Ensure env var is not set (mock resolve-api-key to throw)
      (with-redefs [or-strat/resolve-api-key (fn [_]
                                                (throw (ex-info "OpenRouter API key required"
                                                                {:env "OPENROUTER_API_KEY"})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"API key required"
                              (strategy/strategy-spawn! strat ctx {})))))))

;; =============================================================================
;; Session Status Tests
;; =============================================================================

(deftest status-returns-session-info
  (testing "strategy-status returns session metadata"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        (let [status (strategy/strategy-status strat ctx nil)]
          (is (map? status))
          (is (= ling-id (:slave/id status)))
          (is (= :openrouter (:ling/spawn-mode status)))
          (is (true? (:openrouter-alive? status)))
          (is (= "deepseek/deepseek-chat" (:openrouter-model status)))
          (is (zero? (:openrouter-request-count status)))
          (is (= 1 (:openrouter-message-count status)))
          (is (pos? (:openrouter-started-at status)))
          (is (>= (:openrouter-uptime-ms status) 0))
          (is (map? (:openrouter-stdout status))))))))

(deftest status-with-ds-status-merges
  (testing "strategy-status merges with existing DataScript status"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}
          ds-status {:slave/id ling-id :slave/status :working :slave/cwd "/tmp"}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        (let [status (strategy/strategy-status strat ctx ds-status)]
          ;; Should preserve DS status fields
          (is (= :working (:slave/status status)))
          (is (= "/tmp" (:slave/cwd status)))
          ;; Should add OpenRouter fields
          (is (true? (:openrouter-alive? status)))
          (is (= :openrouter (:ling/spawn-mode status))))))))

(deftest status-nonexistent-returns-nil
  (testing "strategy-status for nonexistent session returns nil or enriched ds-status"
    (let [strat (or-strat/->openrouter-strategy)
          ctx {:id "nonexistent" :cwd "/tmp" :model "test"}]
      ;; With nil ds-status, should return nil
      (is (nil? (strategy/strategy-status strat ctx nil)))
      ;; With ds-status, should add alive?=false
      (let [result (strategy/strategy-status strat ctx {:slave/id "nonexistent"})]
        (is (false? (:openrouter-alive? result)))))))

;; =============================================================================
;; Session Kill Tests
;; =============================================================================

(deftest kill-removes-session
  (testing "strategy-kill! removes session from registry"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        (is (or-strat/openrouter-session? ling-id))
        (let [result (strategy/strategy-kill! strat ctx)]
          (is (:killed? result))
          (is (= ling-id (:id result))))
        (is (not (or-strat/openrouter-session? ling-id)))))))

(deftest kill-nonexistent-returns-killed
  (testing "strategy-kill! for nonexistent session returns killed with reason"
    (let [strat (or-strat/->openrouter-strategy)
          ctx {:id "nonexistent" :cwd "/tmp" :model "test"}]
      (let [result (strategy/strategy-kill! strat ctx)]
        (is (:killed? result))
        (is (= :session-not-found (:reason result)))))))

;; =============================================================================
;; Dispatch Tests (Mocked HTTP)
;; =============================================================================

(deftest dispatch-to-alive-session
  (testing "strategy-dispatch! sends to alive session"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}
          dispatched-msg (atom nil)]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")
                    or-strat/dispatch-async! (fn [id msg]
                                               (reset! dispatched-msg msg)
                                               nil)]
        (strategy/strategy-spawn! strat ctx {})
        (let [result (strategy/strategy-dispatch! strat ctx {:task "Test task"})]
          (is (true? result))
          (is (= "Test task" @dispatched-msg)))))))

(deftest dispatch-to-dead-session-throws
  (testing "strategy-dispatch! to dead session throws"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        ;; Kill it first
        (strategy/strategy-kill! strat ctx)
        ;; Now dispatch should throw
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"not found"
                              (strategy/strategy-dispatch! strat ctx {:task "Test"})))))))

(deftest dispatch-to-nonexistent-throws
  (testing "strategy-dispatch! to nonexistent session throws"
    (let [strat (or-strat/->openrouter-strategy)
          ctx {:id "nonexistent" :cwd "/tmp" :model "test"}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"not found"
                            (strategy/strategy-dispatch! strat ctx {:task "Test"}))))))

;; =============================================================================
;; Ring Buffer Integration Tests
;; =============================================================================

(deftest spawn-creates-ring-buffer
  (testing "Spawned session has a ring buffer with system message"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets ["tdd"]}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        ;; Stdout should have the spawn message
        (let [stdout (or-strat/get-stdout ling-id)]
          (is (vector? stdout))
          (is (>= (count stdout) 1))
          (is (some #(re-find #"OpenRouter ling spawned" %) stdout)))))))

(deftest get-stdout-nonexistent-returns-nil
  (testing "get-stdout for nonexistent returns nil"
    (is (nil? (or-strat/get-stdout "nonexistent")))))

(deftest get-stdout-with-last-n
  (testing "get-stdout respects :last-n option"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "test" :presets []}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        ;; Get last 1 line
        (let [stdout (or-strat/get-stdout ling-id {:last-n 1})]
          (is (= 1 (count stdout))))))))

;; =============================================================================
;; Conversation History Tests
;; =============================================================================

(deftest conversation-starts-with-system-prompt
  (testing "New session has system prompt as first message"
    (let [strat (or-strat/->openrouter-strategy)
          ling-id (gen-ling-id)
          ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
               :project-id "my-project" :presets ["tdd" "clarity"]}]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat ctx {})
        (let [conv (or-strat/get-conversation ling-id)]
          (is (= 1 (count conv)))
          (is (= "system" (:role (first conv))))
          ;; System prompt should contain context
          (let [content (:content (first conv))]
            (is (re-find #"ling agent" content))
            (is (re-find (re-pattern ling-id) content))
            (is (re-find #"/tmp" content))
            (is (re-find #"my-project" content))
            (is (re-find #"tdd" content))))))))

(deftest conversation-nil-for-nonexistent
  (testing "get-conversation returns nil for nonexistent"
    (is (nil? (or-strat/get-conversation "nonexistent")))))

;; =============================================================================
;; Registry Query Tests
;; =============================================================================

(deftest list-sessions-empty
  (testing "list-openrouter-sessions returns empty when none"
    (is (empty? (or-strat/list-openrouter-sessions)))))

(deftest list-sessions-multiple
  (testing "list-openrouter-sessions returns all active sessions"
    (let [strat (or-strat/->openrouter-strategy)
          id1 (gen-ling-id)
          id2 (gen-ling-id)]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (strategy/strategy-spawn! strat
          {:id id1 :cwd "/tmp" :model "model-a" :project-id "test" :presets []} {})
        (strategy/strategy-spawn! strat
          {:id id2 :cwd "/tmp" :model "model-b" :project-id "test" :presets []} {})
        (let [sessions (or-strat/list-openrouter-sessions)]
          (is (= 2 (count sessions)))
          (is (= #{id1 id2} (set (map :ling-id sessions)))))))))

(deftest kill-all-cleans-registry
  (testing "kill-all-openrouter! removes all sessions"
    (let [strat (or-strat/->openrouter-strategy)]
      (with-redefs [or-strat/resolve-api-key (fn [_] "test-key-123")]
        (dotimes [i 3]
          (strategy/strategy-spawn! strat
            {:id (gen-ling-id) :cwd "/tmp" :model "test-model"
             :project-id "test" :presets []} {}))
        (is (= 3 (count (or-strat/list-openrouter-sessions))))
        (let [result (or-strat/kill-all-openrouter!)]
          (is (= 3 (:killed result)))
          (is (zero? (:errors result))))
        (is (empty? (or-strat/list-openrouter-sessions)))))))

;; =============================================================================
;; Ling Record Integration Tests
;; =============================================================================

(deftest ling-factory-openrouter-spawn-mode
  (testing "->ling with non-claude model sets :openrouter spawn-mode"
    (let [ling (ling/->ling "test-or-ling" {:cwd "/tmp"
                                             :model "deepseek/deepseek-chat"
                                             :project-id "test"})]
      (is (= :openrouter (:spawn-mode ling)))
      (is (= "deepseek/deepseek-chat" (:model ling))))))

(deftest ling-factory-claude-model-stays-vterm
  (testing "->ling with claude model keeps :vterm spawn-mode"
    (let [ling (ling/->ling "test-claude" {:cwd "/tmp"
                                            :model "claude"
                                            :project-id "test"})]
      (is (= :vterm (:spawn-mode ling))))))

(deftest ling-factory-nil-model-stays-vterm
  (testing "->ling with nil model keeps :vterm spawn-mode"
    (let [ling (ling/->ling "test-nil" {:cwd "/tmp" :project-id "test"})]
      (is (= :vterm (:spawn-mode ling)))
      (is (nil? (:model ling))))))

(deftest ling-factory-various-openrouter-models
  (testing "->ling correctly routes various OpenRouter model IDs"
    (doseq [model ["deepseek/deepseek-chat"
                    "meta-llama/llama-3.3-70b"
                    "mistralai/mistral-large"
                    "openai/gpt-4-turbo"
                    "google/gemini-pro"]]
      (let [ling (ling/->ling "test" {:cwd "/tmp" :model model})]
        (is (= :openrouter (:spawn-mode ling))
            (str "Model " model " should use :openrouter"))))))

;; =============================================================================
;; SSE Parsing Tests (Pure Functions)
;; =============================================================================

(deftest parse-sse-line-data
  (testing "parse-sse-line handles valid data lines"
    (let [chunk (or-strat/parse-sse-line
                 "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}")]
      (is (some? chunk))
      (is (= "hello" (get-in chunk [:choices 0 :delta :content]))))))

(deftest parse-sse-line-done
  (testing "parse-sse-line returns nil for [DONE]"
    (is (nil? (or-strat/parse-sse-line "data: [DONE]")))))

(deftest parse-sse-line-non-data
  (testing "parse-sse-line returns nil for non-data lines"
    (is (nil? (or-strat/parse-sse-line "")))
    (is (nil? (or-strat/parse-sse-line "event: completion")))
    (is (nil? (or-strat/parse-sse-line ": keep-alive")))))

(deftest parse-sse-line-nil
  (testing "parse-sse-line returns nil for nil"
    (is (nil? (or-strat/parse-sse-line nil)))))

(deftest parse-sse-line-invalid-json
  (testing "parse-sse-line returns nil for invalid JSON"
    (is (nil? (or-strat/parse-sse-line "data: {not valid json}")))))

(deftest extract-delta-content-present
  (testing "extract-delta-content extracts content from chunk"
    (is (= "world"
           (or-strat/extract-delta-content
            {:choices [{:delta {:content "world"}}]})))))

(deftest extract-delta-content-missing
  (testing "extract-delta-content returns nil when no content"
    (is (nil? (or-strat/extract-delta-content {:choices [{:delta {}}]})))
    (is (nil? (or-strat/extract-delta-content {:choices []})))))

;; =============================================================================
;; System Prompt Tests
;; =============================================================================

(deftest system-prompt-contains-context
  (testing "build-system-prompt includes ling context"
    (let [prompt (or-strat/build-system-prompt
                  {:id "test-ling" :cwd "/tmp/project"
                   :project-id "my-project" :presets ["tdd" "clarity"]})]
      (is (string? prompt))
      (is (re-find #"test-ling" prompt))
      (is (re-find #"/tmp/project" prompt))
      (is (re-find #"my-project" prompt))
      (is (re-find #"tdd" prompt))
      (is (re-find #"clarity" prompt)))))

(deftest system-prompt-handles-nil-fields
  (testing "build-system-prompt handles nil fields gracefully"
    (let [prompt (or-strat/build-system-prompt {:id "test" :cwd nil :project-id nil :presets nil})]
      (is (string? prompt))
      (is (re-find #"test" prompt)))))

(comment
  ;; Run tests
  ;; (clojure.test/run-tests 'hive-mcp.agent.ling.openrouter-strategy-test)
  )
