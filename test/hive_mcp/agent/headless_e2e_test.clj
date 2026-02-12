(ns hive-mcp.agent.headless-e2e-test
  "E2E tests for headless ling spawning with OpenRouter model override.

   Tests the full lifecycle:
   1. ->ling with non-claude model auto-routes to :openrouter spawn-mode
   2. spawn! -> session registry + DataScript registration
   3. dispatch -> async streaming -> ring buffer captures output
   4. status/stdout/kill work correctly through strategy pattern

   Mock: stream-completion! is replaced to avoid real OpenRouter API calls.
   The mock writes content to the ring buffer exactly as the real impl would.

   Note: Uses with-redefs-fn on the private stream-completion! var.
   This tests the full strategy -> session -> ring-buffer pipeline."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datascript.core :as d]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.ling.openrouter-strategy :as or-strat]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.connection :as ds-conn]
            [hive-mcp.swarm.datascript.schema :as ds-schema]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [clojure.string :as str]))

;; =============================================================================
;; Mock: stream-completion! replacement (avoids real OpenRouter API calls)
;; =============================================================================

(defn mock-stream-completion!
  "Simulates OpenRouter SSE streaming by writing directly to ring buffer.
   Echoes back the last user message content as mock response."
  [{:keys [stdout-buffer alive?]} messages]
  (when @alive?
    (let [last-user-msg (->> messages
                             (filter #(= "user" (:role %)))
                             last
                             :content)
          response-text (str "Mock response to: "
                             (or last-user-msg "system-init"))]
      ;; Write chunks to ring buffer (simulates SSE delta streaming)
      (doseq [word (str/split response-text #" ")]
        (when @alive?
          (headless/ring-buffer-append! stdout-buffer word)))
      ;; End-of-completion marker (matches real impl behavior)
      (headless/ring-buffer-append! stdout-buffer "\n---END-COMPLETION---")
      {:content response-text})))

(defn mock-stream-completion-slow!
  "Slow mock with per-chunk delay. For testing alive? cancellation."
  [{:keys [stdout-buffer alive?]} messages]
  (when @alive?
    (doseq [i (range 10)]
      (when @alive?
        (Thread/sleep 50)
        (headless/ring-buffer-append! stdout-buffer (str "chunk-" i))))
    (headless/ring-buffer-append! stdout-buffer "\n---END-COMPLETION---")
    {:content "slow-response"}))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn with-fresh-state [f]
  ;; Force-install fresh DataScript connection for test isolation.
  ;; ds/reset-conn! is guarded (no-op when coordinator running), so we
  ;; directly reset the conn atom. Save & restore original after test.
  (let [conn-atom @#'ds-conn/conn
        orig-conn @conn-atom
        fresh-conn (d/create-conn ds-schema/schema)]
    (reset! conn-atom fresh-conn)
    (or-strat/kill-all-openrouter!)
    (try
      (f)
      (finally
        (or-strat/kill-all-openrouter!)
        (reset! conn-atom orig-conn)))))

(use-fixtures :each with-fresh-state)

(defn gen-id []
  (str "test-or-e2e-" (java.util.UUID/randomUUID)))

;; Macro to wrap all E2E tests with stream-completion! mock
(defmacro with-mock-streaming [& body]
  `(with-redefs-fn
     {#'hive-mcp.agent.ling.openrouter-strategy/stream-completion!
      mock-stream-completion!}
     (fn [] ~@body)))

;; =============================================================================
;; Test: Auto-routing (non-claude model -> :openrouter spawn-mode)
;; =============================================================================

(deftest openrouter-auto-route-test
  (testing "Non-claude model auto-routes to :openrouter"
    (let [ling (ling/->ling "t1" {:cwd "/tmp" :model "deepseek/deepseek-chat"})]
      (is (= :openrouter (:spawn-mode ling)))
      (is (= "deepseek/deepseek-chat" (:model ling)))))

  (testing "Other non-claude models also route to :openrouter"
    (doseq [model ["meta-llama/llama-3-8b"
                   "mistralai/mistral-7b"
                   "google/gemma-7b"]]
      (let [ling (ling/->ling "t" {:cwd "/tmp" :model model})]
        (is (= :openrouter (:spawn-mode ling))
            (str model " should route to :openrouter")))))

  (testing "Claude model stays in :vterm"
    (let [ling (ling/->ling "t2" {:cwd "/tmp" :model "claude"})]
      (is (= :vterm (:spawn-mode ling)))))

  (testing "Nil model defaults to :vterm"
    (let [ling (ling/->ling "t3" {:cwd "/tmp"})]
      (is (= :vterm (:spawn-mode ling))))))

;; =============================================================================
;; Test: SSE Parsing (unit tests for parse-sse-line + extract-delta-content)
;; =============================================================================

(deftest parse-sse-line-test
  (testing "Valid SSE data line parses to map"
    (let [line "data: {\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}"
          result (or-strat/parse-sse-line line)]
      (is (some? result))
      (is (= "Hello" (or-strat/extract-delta-content result)))))

  (testing "[DONE] returns nil"
    (is (nil? (or-strat/parse-sse-line "data: [DONE]"))))

  (testing "Non-data lines return nil"
    (is (nil? (or-strat/parse-sse-line ": keepalive")))
    (is (nil? (or-strat/parse-sse-line "")))
    (is (nil? (or-strat/parse-sse-line nil))))

  (testing "Malformed JSON returns nil gracefully"
    (is (nil? (or-strat/parse-sse-line "data: {not valid json}")))))

;; =============================================================================
;; Test: Strategy-level spawn (session registry, no DataScript)
;; =============================================================================

(deftest openrouter-strategy-spawn-session-test
  (testing "Strategy spawn creates session in registry"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ling-id (gen-id)
            ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
                 :project-id "test" :presets []}]
        ;; Spawn without task (no streaming triggered)
        (strategy/strategy-spawn! strat ctx {:api-key "fake-key"})
        ;; Session should exist in registry
        (is (or-strat/openrouter-session? ling-id))
        (let [session (or-strat/get-session ling-id)]
          (is (true? (:alive? session)))
          (is (= "deepseek/deepseek-chat" (:model session)))
          (is (zero? (:request-count session)))
          (is (= 1 (:message-count session)))) ;; system prompt only
        ;; Stdout should have spawn message
        (let [stdout (or-strat/get-stdout ling-id)]
          (is (some #(str/includes? % "OpenRouter ling spawned") stdout)))))))

(deftest openrouter-strategy-spawn-duplicate-throws-test
  (testing "Duplicate spawn ID throws ExceptionInfo"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ling-id (gen-id)
            ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
                 :project-id "test" :presets []}]
        (strategy/strategy-spawn! strat ctx {:api-key "fake-key"})
        (is (thrown? clojure.lang.ExceptionInfo
                     (strategy/strategy-spawn! strat ctx {:api-key "fake-key"})))))))

;; =============================================================================
;; Test: Spawn with task -> ring buffer captures streamed output
;; =============================================================================

(deftest openrouter-spawn-with-task-captures-output-test
  (testing "Spawn with initial task -> async streaming -> ring buffer capture"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ling-id (gen-id)
            ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
                 :project-id "test" :presets []}]
        ;; Spawn WITH task (triggers dispatch-async!)
        (strategy/strategy-spawn! strat ctx
                                  {:api-key "fake-key"
                                   :task "Write hello world in Clojure"})
        ;; Wait for async dispatch thread to complete
        (Thread/sleep 500)
        ;; Ring buffer should have mock response
        (let [stdout (or-strat/get-stdout ling-id)]
          (is (some #(str/includes? % "Mock") stdout)
              "Ring buffer should contain mock response chunks")
          (is (some #(str/includes? % "END-COMPLETION") stdout)
              "Ring buffer should contain completion marker"))
        ;; Session should show 1 request, 3 messages (system + user + assistant)
        (let [session (or-strat/get-session ling-id)]
          (is (= 1 (:request-count session)))
          (is (= 3 (:message-count session))))))))

;; =============================================================================
;; Test: Dispatch to running ling -> more content in ring buffer
;; =============================================================================

(deftest openrouter-dispatch-adds-content-test
  (testing "Dispatch to running OpenRouter ling adds content to ring buffer"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ling-id (gen-id)
            ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
                 :project-id "test" :presets []}]
        ;; Spawn without task
        (strategy/strategy-spawn! strat ctx {:api-key "fake-key"})
        ;; Dispatch first task
        (or-strat/dispatch-async! ling-id "Explain Clojure macros")
        (Thread/sleep 500)
        ;; Verify first task echoed
        (let [stdout (or-strat/get-stdout ling-id)]
          (is (some #(str/includes? % "Clojure") stdout)))
        ;; Dispatch second task
        (or-strat/dispatch-async! ling-id "Now explain protocols")
        (Thread/sleep 500)
        ;; Verify cumulative state
        (let [session (or-strat/get-session ling-id)]
          (is (= 2 (:request-count session)))
          ;; system + user1 + assistant1 + user2 + assistant2 = 5
          (is (= 5 (:message-count session))))))))

(deftest openrouter-dispatch-to-nonexistent-throws-test
  (testing "Dispatch to nonexistent session returns nil (no-op)"
    ;; dispatch-async! returns nil when session not found (not throw)
    (is (nil? (or-strat/dispatch-async! "nonexistent-ling" "hello")))))

;; =============================================================================
;; Test: Status through strategy pattern
;; =============================================================================

(deftest openrouter-strategy-status-test
  (testing "Strategy status returns correct model/mode/liveness info"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ling-id (gen-id)
            ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
                 :project-id "test" :presets []}]
        (strategy/strategy-spawn! strat ctx {:api-key "fake-key"})
        (let [status (strategy/strategy-status strat ctx nil)]
          (is (= ling-id (:slave/id status)))
          (is (= :openrouter (:ling/spawn-mode status)))
          (is (true? (:openrouter-alive? status)))
          (is (= "deepseek/deepseek-chat" (:openrouter-model status)))
          (is (zero? (:openrouter-request-count status)))
          (is (pos? (:openrouter-started-at status)))
          (is (>= (:openrouter-uptime-ms status) 0))
          (is (map? (:openrouter-stdout status))))))))

(deftest openrouter-status-nonexistent-returns-nil-test
  (testing "Status for nonexistent session enriches ds-status or returns nil"
    (let [strat (or-strat/->openrouter-strategy)
          ctx {:id "nonexistent" :cwd "/tmp" :model "x" :presets []}]
      ;; With nil ds-status -> returns nil
      (is (nil? (strategy/strategy-status strat ctx nil))))))

;; =============================================================================
;; Test: Kill cleans up session registry
;; =============================================================================

(deftest openrouter-strategy-kill-test
  (testing "Strategy kill removes session from registry"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ling-id (gen-id)
            ctx {:id ling-id :cwd "/tmp" :model "deepseek/deepseek-chat"
                 :project-id "test" :presets []}]
        (strategy/strategy-spawn! strat ctx {:api-key "fake-key"})
        (is (or-strat/openrouter-session? ling-id))
        (let [result (strategy/strategy-kill! strat ctx)]
          (is (true? (:killed? result)))
          (is (= ling-id (:id result))))
        (is (not (or-strat/openrouter-session? ling-id)))))))

(deftest openrouter-kill-nonexistent-graceful-test
  (testing "Kill nonexistent session returns killed?=true with reason"
    (let [strat (or-strat/->openrouter-strategy)
          ctx {:id "nonexistent" :cwd "/tmp" :model "x" :presets []}
          result (strategy/strategy-kill! strat ctx)]
      (is (true? (:killed? result)))
      (is (= :session-not-found (:reason result))))))

;; =============================================================================
;; Test: Conversation history accumulates across dispatches
;; =============================================================================

(deftest openrouter-conversation-history-test
  (testing "Conversation history maintains state across dispatches"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ling-id (gen-id)
            ctx {:id ling-id :cwd "/tmp" :model "meta-llama/llama-3-8b"
                 :project-id "test" :presets []}]
        (strategy/strategy-spawn! strat ctx {:api-key "fake-key"})
        ;; Initial: just system prompt
        (let [conv (or-strat/get-conversation ling-id)]
          (is (= 1 (count conv)))
          (is (= "system" (:role (first conv)))))
        ;; After first dispatch
        (or-strat/dispatch-async! ling-id "First message")
        (Thread/sleep 500)
        (let [conv (or-strat/get-conversation ling-id)]
          (is (= 3 (count conv))) ;; system + user + assistant
          (is (= "user" (:role (second conv))))
          (is (= "First message" (:content (second conv))))
          (is (= "assistant" (:role (nth conv 2)))))
        ;; After second dispatch
        (or-strat/dispatch-async! ling-id "Second message")
        (Thread/sleep 500)
        (let [conv (or-strat/get-conversation ling-id)]
          (is (= 5 (count conv))) ;; + user2 + assistant2
          (is (= "Second message" (:content (nth conv 3)))))))))

;; =============================================================================
;; Test: kill-all-openrouter! cleans up all sessions
;; =============================================================================

(deftest kill-all-openrouter-test
  (testing "kill-all-openrouter! removes all sessions"
    (with-mock-streaming
      (let [strat (or-strat/->openrouter-strategy)
            ids (repeatedly 3 gen-id)]
        ;; Spawn 3 sessions
        (doseq [id ids]
          (strategy/strategy-spawn! strat
                                    {:id id :cwd "/tmp" :model "test/model"
                                     :project-id "test" :presets []}
                                    {:api-key "fake-key"}))
        (is (= 3 (count (or-strat/list-openrouter-sessions))))
        ;; Kill all
        (let [result (or-strat/kill-all-openrouter!)]
          (is (= 3 (:killed result)))
          (is (zero? (:errors result))))
        (is (empty? (or-strat/list-openrouter-sessions)))))))

;; =============================================================================
;; Test: Full E2E via Ling record (spawn -> dispatch -> stdout -> status -> kill)
;; =============================================================================

(deftest full-e2e-openrouter-ling-lifecycle-test
  (testing "Full lifecycle: Ling record with OpenRouter model override"
    (with-mock-streaming
      (let [ling-id (gen-id)
            ling (ling/->ling ling-id {:cwd "/tmp"
                                       :model "deepseek/deepseek-chat"
                                       :project-id "test-e2e-project"})]
        ;; 1. Verify auto-routing
        (is (= :openrouter (:spawn-mode ling))
            "Non-claude model should auto-route to :openrouter")

        ;; 2. Spawn with initial task
        (let [slave-id (.spawn! ling {:task "Hello from E2E test"
                                      :api-key "fake-key"})]
          (is (= ling-id slave-id)
              "Slave ID should match requested ID for openrouter mode")
          (Thread/sleep 500)

          ;; 3. Verify DataScript registration
          (let [ds-slave (ds-queries/get-slave ling-id)]
            (is (some? ds-slave) "Should be registered in DataScript")
            (is (= :openrouter (:ling/spawn-mode ds-slave)))
            (is (= "deepseek/deepseek-chat" (:ling/model ds-slave)))
            (is (true? (:ling/process-alive? ds-slave))))

          ;; 4. Verify ring buffer captured streamed output
          (let [stdout (or-strat/get-stdout ling-id)]
            (is (seq stdout) "Stdout should not be empty")
            (is (some #(str/includes? % "Mock") stdout)
                "Should contain mock response chunks")
            (is (some #(str/includes? % "END-COMPLETION") stdout)
                "Should contain completion marker"))

          ;; 5. Dispatch another task via Ling record
          (let [task-id (.dispatch! ling {:task "Second E2E task"})]
            (is (string? task-id) "dispatch! should return task-id string")
            (Thread/sleep 500)
            ;; Verify new content in ring buffer
            (let [stdout (or-strat/get-stdout ling-id)]
              (is (some #(str/includes? % "Second") stdout)
                  "Should contain second task response")))

          ;; 6. Status check via Ling record
          (let [status (.status ling)]
            (is (= :openrouter (:ling/spawn-mode status)))
            (is (true? (:openrouter-alive? status)))
            (is (= "deepseek/deepseek-chat" (:openrouter-model status)))
            (is (= 2 (:openrouter-request-count status))))

          ;; 7. Kill via Ling record
          (let [result (.kill! ling)]
            (is (true? (:killed? result))))

          ;; 8. Verify full cleanup
          (is (not (or-strat/openrouter-session? ling-id))
              "Session should be removed from OpenRouter registry")
          (is (nil? (ds-queries/get-slave ling-id))
              "Should be removed from DataScript"))))))

;; =============================================================================
;; Test: Reconstitution from DataScript (get-ling preserves openrouter mode)
;; =============================================================================

(deftest ling-reconstitution-preserves-openrouter-mode-test
  (testing "get-ling reconstitutes Ling with correct spawn-mode from DataScript"
    (with-mock-streaming
      (let [ling-id (gen-id)
            ling (ling/->ling ling-id {:cwd "/tmp"
                                       :model "deepseek/deepseek-chat"
                                       :project-id "test-recon"})]
        (.spawn! ling {:task "Initial task" :api-key "fake-key"})
        (Thread/sleep 300)
        ;; Reconstitute from DataScript
        (let [recon (ling/get-ling ling-id)]
          (is (some? recon) "Should reconstitute from DataScript")
          (is (= :openrouter (:spawn-mode recon))
              "Reconstituted ling should preserve :openrouter spawn-mode")
          (is (= "deepseek/deepseek-chat" (:model recon))
              "Reconstituted ling should preserve model"))
        ;; Cleanup
        (.kill! ling)))))

;; =============================================================================
;; Test: System prompt builder
;; =============================================================================

(deftest build-system-prompt-test
  (testing "System prompt includes ling context"
    (let [prompt (or-strat/build-system-prompt
                  {:id "test-ling" :cwd "/home/test/project"
                   :project-id "my-project" :presets ["worker" "tdd"]})]
      (is (str/includes? prompt "test-ling"))
      (is (str/includes? prompt "/home/test/project"))
      (is (str/includes? prompt "my-project"))
      (is (str/includes? prompt "worker, tdd")))))

(comment
  ;; Run all tests in this namespace
  (clojure.test/run-tests 'hive-mcp.agent.headless-e2e-test)

  ;; Run specific test
  (full-e2e-openrouter-ling-lifecycle-test)
  (openrouter-auto-route-test)
  (parse-sse-line-test))
