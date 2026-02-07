(ns hive-mcp.agent.ling.openrouter-streaming-e2e-test
  "REAL E2E test: spawn OpenRouter ling with DeepSeek model, verify streaming.

   Unlike other OpenRouter tests which mock stream-completion!, this test
   makes ACTUAL HTTP calls to the OpenRouter API. It verifies:
   1. Real SSE streaming from DeepSeek model into ring buffer
   2. Conversation history tracks user + assistant messages
   3. Multi-turn dispatch preserves conversation context
   4. Ring buffer captures incremental streaming deltas
   5. Session metrics (request-count, message-count) update correctly

   REQUIRES: OPENROUTER_API_KEY env var or config.edn secret.
   Tests are SKIPPED (not failed) when API key is unavailable.

   Run with:  (clojure.test/run-tests 'hive-mcp.agent.ling.openrouter-streaming-e2e-test)
   Or filter: lein test :only hive-mcp.agent.ling.openrouter-streaming-e2e-test"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.openrouter-strategy :as or-strat]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.config :as global-config]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; API Key Detection
;; =============================================================================

(def ^:private api-key
  "Resolved OpenRouter API key, or nil if unavailable."
  (global-config/get-secret :openrouter-api-key))

(def ^:private api-key-available?
  "True when OPENROUTER_API_KEY is set (config.edn or env var)."
  (some? api-key))

;; DeepSeek model for E2E testing — widely available on OpenRouter
(def ^:private test-model "deepseek/deepseek-chat")

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn with-fresh-state [f]
  "Reset DataScript + kill all OpenRouter sessions between tests."
  (ds/reset-conn!)
  (or-strat/kill-all-openrouter!)
  (f)
  (or-strat/kill-all-openrouter!)
  (ds/reset-conn!))

(use-fixtures :each with-fresh-state)

(defn gen-id
  "Generate unique ling ID for test isolation."
  [prefix]
  (str prefix "-" (System/nanoTime)))

;; =============================================================================
;; Helper: wait for dispatch thread to complete
;; =============================================================================

(defn- wait-for-thread!
  "Join on a dispatch thread with timeout. Returns true if thread completed."
  [^Thread thread timeout-ms]
  (when thread
    (.join thread timeout-ms)
    (not (.isAlive thread))))

;; =============================================================================
;; Conditional test macro — skip instead of fail when no API key
;; =============================================================================

(defmacro when-api-key
  "Run body only if OPENROUTER_API_KEY is available. Otherwise prints SKIP."
  [test-name & body]
  `(if-not api-key-available?
     (println "SKIP:" ~test-name "(no OPENROUTER_API_KEY)")
     (do ~@body)))

;; =============================================================================
;; Test 1: Basic spawn + single dispatch + verify streaming response
;; =============================================================================

(deftest ^:e2e openrouter-deepseek-single-dispatch-streaming
  (when-api-key "openrouter-deepseek-single-dispatch-streaming"
                (testing "Real OpenRouter streaming: spawn DeepSeek ling, dispatch, verify ring buffer"
                  (let [strat (or-strat/->openrouter-strategy)
                        ling-id (gen-id "e2e-single")
                        ctx {:id ling-id
                             :cwd "/tmp"
                             :model test-model
                             :project-id "e2e-streaming-test"
                             :presets []}]

        ;; 1. SPAWN — no initial task, verify session created
                    (let [result (strategy/strategy-spawn! strat ctx {})]
                      (is (= ling-id result) "spawn should return ling-id")
                      (is (or-strat/openrouter-session? ling-id) "session should exist in registry"))

        ;; 2. DISPATCH — send a simple prompt, get thread handle
                    (let [thread (or-strat/dispatch-async! ling-id
                                                           "Reply with exactly: HIVE_E2E_OK")]
                      (is (some? thread) "dispatch-async! should return a Thread")

          ;; 3. WAIT — for streaming completion (up to 60s for slow models)
                      (let [completed? (wait-for-thread! thread 60000)]
                        (is (true? completed?) "Streaming should complete within 60s"))

          ;; 4. VERIFY RING BUFFER — should have streamed content
                      (let [stdout (or-strat/get-stdout ling-id)]
                        (is (vector? stdout) "stdout should be a vector")
                        (is (> (count stdout) 1)
                            (str "Ring buffer should have more than spawn message, got "
                                 (count stdout) " entries"))
            ;; [USER] dispatch marker
                        (is (some #(str/includes? % "[USER]") stdout)
                            "Ring buffer should contain [USER] dispatch marker")
            ;; END-COMPLETION marker from streaming
                        (is (some #(str/includes? % "END-COMPLETION") stdout)
                            "Ring buffer should contain END-COMPLETION marker from finished stream"))

          ;; 5. VERIFY CONVERSATION HISTORY — system + user + assistant
                      (let [conv (or-strat/get-conversation ling-id)]
                        (is (= 3 (count conv))
                            (str "Should have 3 messages (system+user+assistant), got " (count conv)))
                        (is (= "system" (:role (first conv))) "First message should be system")
                        (is (= "user" (:role (second conv))) "Second message should be user")
                        (is (= "assistant" (:role (nth conv 2))) "Third message should be assistant")
            ;; Assistant response should have actual content
                        (let [assistant-content (:content (nth conv 2))]
                          (is (string? assistant-content) "Assistant content should be a string")
                          (is (pos? (count assistant-content))
                              "Assistant response should not be empty")))

          ;; 6. VERIFY SESSION METRICS
                      (let [session (or-strat/get-session ling-id)]
                        (is (= 1 (:request-count session)) "Should have exactly 1 request")
                        (is (= 3 (:message-count session)) "Should have 3 messages")
                        (is (true? (:alive? session)) "Session should still be alive")))

        ;; 7. CLEANUP
                    (strategy/strategy-kill! strat ctx)
                    (is (not (or-strat/openrouter-session? ling-id))
                        "Session should be removed after kill")))))

;; =============================================================================
;; Test 2: Multi-turn conversation — verify context preservation
;; =============================================================================

(deftest ^:e2e openrouter-deepseek-multi-turn-conversation
  (when-api-key "openrouter-deepseek-multi-turn-conversation"
                (testing "Multi-turn: DeepSeek remembers previous turns via conversation history"
                  (let [strat (or-strat/->openrouter-strategy)
                        ling-id (gen-id "e2e-multi")
                        ctx {:id ling-id
                             :cwd "/tmp"
                             :model test-model
                             :project-id "e2e-multi-turn"
                             :presets []}]

        ;; Spawn
                    (strategy/strategy-spawn! strat ctx {})

        ;; Turn 1: establish context
                    (let [t1 (or-strat/dispatch-async! ling-id
                                                       "Remember this secret code: ZEBRA42. Just confirm you remember it.")]
                      (wait-for-thread! t1 60000))

        ;; Turn 2: ask about the context (tests conversation history)
                    (let [t2 (or-strat/dispatch-async! ling-id
                                                       "What was the secret code I told you? Reply with just the code.")]
                      (wait-for-thread! t2 60000))

        ;; Verify conversation has 5 messages: system + user + assistant + user + assistant
                    (let [conv (or-strat/get-conversation ling-id)]
                      (is (= 5 (count conv))
                          (str "Should have 5 messages after 2 turns, got " (count conv)))
                      (is (= ["system" "user" "assistant" "user" "assistant"]
                             (mapv :role conv))
                          "Message roles should alternate correctly")

          ;; Second assistant response should reference ZEBRA42
          ;; (this verifies the model received conversation history)
                      (let [second-response (:content (nth conv 4))]
                        (is (string? second-response))
                        (is (pos? (count second-response))
                            "Second response should not be empty")
            ;; Note: we don't assert ZEBRA42 is in the response because
            ;; model behavior isn't deterministic, but conversation should exist
                        ))

        ;; Verify metrics
                    (let [session (or-strat/get-session ling-id)]
                      (is (= 2 (:request-count session)) "Should have 2 requests")
                      (is (= 5 (:message-count session)) "Should have 5 messages"))

        ;; Cleanup
                    (strategy/strategy-kill! strat ctx)))))

;; =============================================================================
;; Test 3: Spawn with initial task — dispatch happens on spawn
;; =============================================================================

(deftest ^:e2e openrouter-deepseek-spawn-with-task
  (when-api-key "openrouter-deepseek-spawn-with-task"
                (testing "Spawn with initial task dispatches immediately and streams response"
                  (let [strat (or-strat/->openrouter-strategy)
                        ling-id (gen-id "e2e-spawn-task")
                        ctx {:id ling-id
                             :cwd "/tmp"
                             :model test-model
                             :project-id "e2e-spawn-task"
                             :presets ["worker"]}]

        ;; Spawn WITH task — dispatch-async! fires internally
                    (strategy/strategy-spawn! strat ctx
                                              {:task "Say exactly: SPAWN_TASK_OK"})

        ;; Poll until response appears (spawn-initiated dispatch is async)
                    (loop [attempts 0]
                      (when (< attempts 120)  ; 120 * 500ms = 60s max
                        (let [session (or-strat/get-session ling-id)]
                          (when (and session (< (:message-count session) 3))
                            (Thread/sleep 500)
                            (recur (inc attempts))))))

        ;; Verify the conversation has a response
                    (let [conv (or-strat/get-conversation ling-id)]
                      (is (>= (count conv) 3)
                          (str "Should have at least 3 messages, got " (count conv)))
                      (when (>= (count conv) 3)
                        (is (= "assistant" (:role (nth conv 2))))
                        (is (seq (:content (nth conv 2)))
                            "Assistant should have responded")))

        ;; Ring buffer should have streaming output
                    (let [stdout (or-strat/get-stdout ling-id)]
                      (is (> (count stdout) 1)
                          "Ring buffer should have streaming content"))

        ;; System prompt should include preset name
                    (let [conv (or-strat/get-conversation ling-id)
                          sys-prompt (:content (first conv))]
                      (is (str/includes? sys-prompt "worker")
                          "System prompt should include preset name"))

        ;; Cleanup
                    (strategy/strategy-kill! strat ctx)))))

;; =============================================================================
;; Test 4: Ring buffer captures incremental streaming deltas
;; =============================================================================

(deftest ^:e2e openrouter-deepseek-incremental-streaming
  (when-api-key "openrouter-deepseek-incremental-streaming"
                (testing "Ring buffer captures multiple incremental deltas from SSE stream"
                  (let [strat (or-strat/->openrouter-strategy)
                        ling-id (gen-id "e2e-incremental")
                        ctx {:id ling-id
                             :cwd "/tmp"
                             :model test-model
                             :project-id "e2e-incremental"
                             :presets []}
                        before-dispatch (System/currentTimeMillis)]

        ;; Spawn
                    (strategy/strategy-spawn! strat ctx {})

        ;; Dispatch a prompt that should produce a multi-token response
                    (let [thread (or-strat/dispatch-async! ling-id
                                                           "Count from 1 to 5, each number on a separate line.")]
                      (wait-for-thread! thread 60000))

        ;; Verify ring buffer has MULTIPLE entries (not just one big blob)
        ;; The streaming impl writes each delta chunk as a separate ring buffer entry
                    (let [stdout (or-strat/get-stdout ling-id)]
                      (is (> (count stdout) 3)
                          (str "Streaming should produce multiple ring buffer entries, got "
                               (count stdout)))
          ;; Verify END-COMPLETION marker exists
                      (is (some #(str/includes? % "END-COMPLETION") stdout)
                          "Should have end-of-completion marker"))

        ;; Verify time-based retrieval works
                    (let [entries (or-strat/get-stdout-since ling-id before-dispatch)]
                      (is (vector? entries) "get-stdout-since should return vector")
                      (is (pos? (count entries))
                          "Should have entries after dispatch timestamp")
                      (when (seq entries)
                        (is (every? #(and (:text %) (:ts %)) entries)
                            "Each entry should have :text and :ts")
                        (is (every? #(>= (:ts %) before-dispatch) entries)
                            "All timestamps should be >= dispatch time")))

        ;; Cleanup
                    (strategy/strategy-kill! strat ctx)))))

;; =============================================================================
;; Test 5: Full Ling facade lifecycle with real OpenRouter
;; =============================================================================

(deftest ^:e2e openrouter-deepseek-ling-facade-lifecycle
  (when-api-key "openrouter-deepseek-ling-facade-lifecycle"
                (testing "Full Ling facade: ->ling → spawn! → dispatch! → status → kill!"
                  (let [ling-id (gen-id "e2e-facade")
                        ling-inst (ling/->ling ling-id {:cwd "/tmp"
                                                        :model test-model
                                                        :project-id "e2e-facade"})]
        ;; Verify auto-routing to :openrouter
                    (is (= :openrouter (:spawn-mode ling-inst))
                        "Non-claude model should auto-route to :openrouter")
                    (is (= test-model (:model ling-inst)))

        ;; 1. SPAWN via protocol
                    (let [slave-id (proto/spawn! ling-inst {:depth 1})]
                      (is (= ling-id slave-id))

          ;; Verify DataScript registration
                      (let [ds-slave (ds-queries/get-slave ling-id)]
                        (is (some? ds-slave) "Should be registered in DataScript")
                        (is (= :openrouter (:ling/spawn-mode ds-slave)))
                        (is (= test-model (:ling/model ds-slave)))))

        ;; 2. DISPATCH via protocol — returns task-id
                    (let [dispatch-ling (ling/->ling ling-id {:cwd "/tmp"
                                                              :spawn-mode :openrouter
                                                              :model test-model})
                          task-id (proto/dispatch! dispatch-ling {:task "Say: FACADE_OK"})]
                      (is (string? task-id) "dispatch! should return task-id"))

        ;; Wait for response via polling
                    (loop [attempts 0]
                      (when (< attempts 120)
                        (let [session (or-strat/get-session ling-id)]
                          (when (and session (< (:message-count session) 3))
                            (Thread/sleep 500)
                            (recur (inc attempts))))))

        ;; 3. STATUS via protocol
                    (let [status (proto/status ling-inst)]
                      (is (map? status))
                      (is (true? (:openrouter-alive? status)))
                      (is (= test-model (:openrouter-model status)))
                      (is (>= (:openrouter-request-count status) 1)))

        ;; 4. KILL via protocol
                    (let [result (proto/kill! ling-inst)]
                      (is (true? (:killed? result))))

        ;; Verify cleanup
                    (is (not (or-strat/openrouter-session? ling-id)))
                    (is (nil? (ds-queries/get-slave ling-id)))))))

;; =============================================================================
;; Test 6: Streaming interrupted by kill
;; =============================================================================

(deftest ^:e2e openrouter-deepseek-kill-during-streaming
  (when-api-key "openrouter-deepseek-kill-during-streaming"
                (testing "Kill during active streaming stops the response thread"
                  (let [strat (or-strat/->openrouter-strategy)
                        ling-id (gen-id "e2e-kill-stream")
                        ctx {:id ling-id
                             :cwd "/tmp"
                             :model test-model
                             :project-id "e2e-kill-stream"
                             :presets []}]

        ;; Spawn and dispatch a prompt that should produce a long response
                    (strategy/strategy-spawn! strat ctx {})
                    (let [thread (or-strat/dispatch-async! ling-id
                                                           "Write a 500-word essay about the history of computing.")]
          ;; Give streaming a moment to start
                      (Thread/sleep 2000)

          ;; Verify session is active
                      (let [session (or-strat/get-session ling-id)]
                        (is (true? (:alive? session)) "Session should be alive before kill"))

          ;; KILL while streaming
                      (let [result (strategy/strategy-kill! strat ctx)]
                        (is (true? (:killed? result)))
                        (is (not (or-strat/openrouter-session? ling-id))))

          ;; Thread should finish relatively quickly after kill
                      (wait-for-thread! thread 10000)
                      (is (not (.isAlive thread))
                          "Dispatch thread should stop after kill"))))))

(comment
  ;; Run all E2E streaming tests
  (clojure.test/run-tests 'hive-mcp.agent.ling.openrouter-streaming-e2e-test)

  ;; Run individual test
  (clojure.test/test-var #'openrouter-deepseek-single-dispatch-streaming)

  ;; Check if API key is available
  api-key-available?

  ;; Quick REPL smoke test
  (let [strat (or-strat/->openrouter-strategy)
        id (str "repl-test-" (System/nanoTime))]
    (strategy/strategy-spawn! strat
                              {:id id :cwd "/tmp" :model "deepseek/deepseek-chat"
                               :project-id "repl" :presets []}
                              {})
    (let [thread (or-strat/dispatch-async! id "Say hello")]
      (.join thread 30000)
      (println "Stdout:" (or-strat/get-stdout id))
      (println "Conv:" (count (or-strat/get-conversation id)))
      (strategy/strategy-kill! strat {:id id}))))
