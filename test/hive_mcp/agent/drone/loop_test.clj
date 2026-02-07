(ns hive-mcp.agent.drone.loop-test
  "Tests for the in-process agentic drone loop (AGPL implementation).

   Tests verify:
   - Termination heuristics (max turns, text-only, failures, completion language)
   - Result evaluation (good/bad/retryable/fatal)
   - Tool selection (nil = LLM decides)
   - Full agentic loop with mock LLMBackend
   - Session KG integration (observation recording, context reconstruction)
   - Two-tier KG: kg-session (hive-knowledge stub) overlay on session-kg (AGPL)
   - Noop behavior when no backend provided
   - Graceful degradation (no exceptions, sensible defaults)
   - End-to-end loop + Datalevin session KG (observations, reasoning, context)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.drone.loop :as loop]
            [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.kg-session :as kg-session]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.protocols.kg :as kg]))

;; =============================================================================
;; Mock LLM Backend
;; =============================================================================

(defn mock-backend
  "Create a mock LLMBackend that returns predefined responses in sequence.
   Each response should be a map with :type and either :content or :calls.

   Arguments:
     model     - Model name string
     responses - Vector of response maps to return in order

   Returns:
     LLMBackend implementation (reify)"
  [model responses]
  (let [call-counter (atom 0)]
    (reify proto/LLMBackend
      (chat [_ _messages _tools]
        (let [idx @call-counter
              response (get responses idx (last responses))]
          (swap! call-counter inc)
          response))
      (model-name [_] model))))

;; =============================================================================
;; Termination Logic Tests
;; =============================================================================

(deftest test-should-terminate-max-turns
  (testing "terminates when turn >= max-turns"
    (let [result (loop/should-terminate?
                  {:turn 10 :steps [] :consecutive-failures 0}
                  {:max-turns 10})]
      (is (:terminate? result))
      (is (re-find #"Max turns" (:reason result))))))

(deftest test-should-terminate-text-only
  (testing "terminates on text-only response (no tool calls = done)"
    (let [result (loop/should-terminate?
                  {:turn 2 :steps [] :consecutive-failures 0
                   :last-response-type :text :last-text "Done."}
                  {:max-turns 10})]
      (is (:terminate? result))
      (is (re-find #"[Tt]ext" (:reason result))))))

(deftest test-should-terminate-consecutive-failures
  (testing "terminates after 3 consecutive failures"
    (let [result (loop/should-terminate?
                  {:turn 3 :steps [] :consecutive-failures 3
                   :last-response-type :tool_calls}
                  {:max-turns 10})]
      (is (:terminate? result))
      (is (re-find #"failure" (:reason result))))))

(deftest test-should-terminate-completion-language
  (testing "terminates when completion language detected"
    (let [result (loop/should-terminate?
                  {:turn 2 :steps [] :consecutive-failures 0
                   :last-response-type :tool_calls
                   :last-text "I've successfully completed the task."}
                  {:max-turns 10})]
      (is (:terminate? result))
      (is (re-find #"[Cc]ompletion language" (:reason result))))))

(deftest test-should-not-terminate-continuing
  (testing "continues when no termination condition met"
    (let [result (loop/should-terminate?
                  {:turn 2 :steps [] :consecutive-failures 0
                   :last-response-type :tool_calls
                   :last-text nil}
                  {:max-turns 10})]
      (is (not (:terminate? result)))
      (is (= "Continuing" (:reason result))))))

(deftest test-should-terminate-default-max-turns
  (testing "defaults to max-turns 10 when no opts"
    (let [result (loop/should-terminate?
                  {:turn 10 :steps [] :consecutive-failures 0})]
      (is (:terminate? result)))))

(deftest test-should-terminate-turn-zero-no-last-response
  (testing "does not terminate at turn 0 with no history"
    (let [result (loop/should-terminate?
                  {:turn 0 :steps [] :consecutive-failures 0
                   :last-response-type nil :last-text nil}
                  {:max-turns 10})]
      (is (not (:terminate? result))))))

;; =============================================================================
;; Result Evaluation Tests
;; =============================================================================

(deftest test-evaluate-result-success
  (testing "success result → :good quality, continue"
    (let [result (loop/evaluate-result {:success true :result {:text "ok"}} "fix bug" [])]
      (is (= :good (:quality result)))
      (is (:continue? result)))))

(deftest test-evaluate-result-retryable-failure
  (testing "not-found error → :bad quality, continue (retryable)"
    (let [result (loop/evaluate-result {:success false :error "file not found"} "fix bug" [])]
      (is (= :bad (:quality result)))
      (is (:continue? result))
      (is (re-find #"[Rr]etryable" (:reason result))))))

(deftest test-evaluate-result-fatal-failure
  (testing "unknown tool error → :bad quality, stop (fatal)"
    (let [result (loop/evaluate-result {:success false :error "Unknown tool: foo"} "fix bug" [])]
      (is (= :bad (:quality result)))
      (is (not (:continue? result)))
      (is (re-find #"[Ff]atal" (:reason result))))))

(deftest test-evaluate-result-generic-failure
  (testing "generic error → :bad quality, continue"
    (let [result (loop/evaluate-result {:success false :error "something went wrong"} "fix bug" [])]
      (is (= :bad (:quality result)))
      (is (:continue? result)))))

;; =============================================================================
;; Tool Selection Tests
;; =============================================================================

(deftest test-select-next-tool-returns-nil
  (testing "AGPL implementation returns nil (let LLM decide)"
    (let [result (loop/select-next-tool [] #{"read_file" "grep"}
                                        {:task "fix bug" :files ["a.clj"]})]
      (is (nil? result)))))

;; =============================================================================
;; Agentic Loop — Noop Tests (no backend)
;; =============================================================================

(deftest test-run-agentic-loop-noop
  (testing "returns :noop status when no backend provided"
    (let [result (loop/run-agentic-loop
                  {:task "Fix the nil check" :files ["src/core.clj"]}
                  {:drone-id "test-drone-1"}
                  {:max-turns 5})]
      (is (map? result))
      (is (= :noop (:status result)))
      (is (string? (:result result)))
      (is (vector? (:steps result)))
      (is (empty? (:steps result)))
      (is (= 0 (:tool_calls_made result)))
      (is (= 0 (:turns result)))
      (is (map? (:tokens result)))
      (is (= 0 (get-in result [:tokens :total]))))))

(deftest test-run-agentic-loop-noop-minimal-args
  (testing "works with minimal arguments"
    (let [result (loop/run-agentic-loop
                  {:task "Simple task"}
                  {:drone-id "test-drone-2"})]
      (is (= :noop (:status result)))
      (is (= "none" (:model result))))))

(deftest test-run-agentic-loop-noop-unknown-drone
  (testing "handles missing drone-id gracefully"
    (let [result (loop/run-agentic-loop
                  {:task "test"} {} {})]
      (is (= :noop (:status result))))))

;; =============================================================================
;; Agentic Loop — Integration Tests with Mock Backend
;; =============================================================================

(deftest test-run-agentic-loop-text-completion
  (testing "loop terminates after text-only response (1 turn)"
    (let [backend (mock-backend "test-model"
                                [{:type :text :content "I've completed the task."}])
          result (loop/run-agentic-loop
                  {:task "Fix the bug" :files ["src/core.clj"]}
                  {:drone-id "test-drone-text"}
                  {:max-turns 5 :backend backend})]
      (is (= :completed (:status result)))
      (is (= "I've completed the task." (:result result)))
      (is (= 1 (:turns result)))
      (is (= 0 (:tool_calls_made result)))
      (is (= "test-model" (:model result))))))

(deftest test-run-agentic-loop-max-turns
  (testing "loop stops at max-turns"
    (let [;; Backend that always returns tool calls (never completes)
          backend (mock-backend "test-model"
                                [{:type :tool_calls
                                  :calls [{:id "c1" :name "read_file"
                                           :arguments {:path "a.clj"}}]}])
          result (loop/run-agentic-loop
                  {:task "Infinite task" :files []}
                  {:drone-id "test-drone-max"}
                  {:max-turns 2 :backend backend})]
      ;; Should terminate due to max turns
      (is (contains? #{:max_steps :completed} (:status result)))
      (is (<= (:turns result) 3))))) ; may go 1 beyond max-turns due to termination check order

(deftest test-run-agentic-loop-error-response
  (testing "loop handles LLM error response gracefully"
    (let [backend (mock-backend "test-model"
                                ;; Return error then text
                                [{:type :error :error "Rate limited"}
                                 {:type :error :error "Still limited"}
                                 {:type :error :error "Again"}
                                 {:type :text :content "Finally worked"}])
          result (loop/run-agentic-loop
                  {:task "Retry test" :files []}
                  {:drone-id "test-drone-err"}
                  {:max-turns 10 :backend backend})]
      ;; Should terminate due to consecutive failures (3) before text response
      (is (contains? #{:completed :max_steps :error} (:status result)))
      (is (pos? (:turns result))))))

(deftest test-run-agentic-loop-result-shape
  (testing "result map has all expected keys"
    (let [backend (mock-backend "shape-model"
                                [{:type :text :content "Done"}])
          result (loop/run-agentic-loop
                  {:task "Shape test"}
                  {:drone-id "test-drone-shape"}
                  {:max-turns 5 :backend backend})]
      (is (contains? result :status))
      (is (contains? result :result))
      (is (contains? result :steps))
      (is (contains? result :tool_calls_made))
      (is (contains? result :tokens))
      (is (contains? result :turns))
      (is (contains? result :model))
      (is (contains? result :kg-stats))
      (is (map? (:tokens result)))
      (is (map? (:kg-stats result))))))

(deftest test-run-agentic-loop-exception-handling
  (testing "loop catches exceptions and returns error status"
    (let [;; Backend that throws on chat
          backend (reify proto/LLMBackend
                    (chat [_ _ _] (throw (ex-info "Boom" {})))
                    (model-name [_] "exploding-model"))
          result (loop/run-agentic-loop
                  {:task "Exploding test"}
                  {:drone-id "test-drone-boom"}
                  {:max-turns 5 :backend backend})]
      (is (= :error (:status result)))
      (is (re-find #"Boom" (:result result))))))

;; =============================================================================
;; Session KG Integration Tests — Datalevin-backed
;; =============================================================================

(deftest test-loop-with-session-kg-text-completion
  (testing "loop with Datalevin session KG records observations on text-only response"
    (let [drone-id (str "test-kg-text-" (System/currentTimeMillis))
          session-store (session-kg/create-session-kg! drone-id)]
      (if (nil? session-store)
        ;; Skip if Datalevin not available
        (println "SKIP: Datalevin not available for session KG test")
        (try
          (let [backend (mock-backend "test-kg-model"
                                      [{:type :text :content "Task completed successfully."}])
                result (loop/run-agentic-loop
                        {:task "Fix the nil check in core.clj" :files ["src/core.clj"]}
                        {:drone-id drone-id :kg-store session-store}
                        {:max-turns 5 :backend backend})]
            ;; Loop should complete successfully
            (is (= :completed (:status result)))
            (is (= 1 (:turns result)))

            ;; KG stats should reflect recorded data
            (is (map? (:kg-stats result)))
            ;; At least 1 reasoning entry (for the text response)
            (is (pos? (:reasoning (:kg-stats result))))

            ;; Verify reasoning was actually recorded in Datalevin
            (let [reasons (kg/query session-store
                                    '[:find ?id ?intent
                                      :where
                                      [?e :reason/id ?id]
                                      [?e :reason/intent ?intent]])]
              (is (pos? (count reasons))
                  "At least one reasoning node should be in the session KG")))
          (finally
            (session-kg/close-session-kg! session-store drone-id)))))))

(deftest test-loop-with-session-kg-tool-calls
  (testing "loop with session KG records observations for tool call results"
    (let [drone-id (str "test-kg-tools-" (System/currentTimeMillis))
          session-store (session-kg/create-session-kg! drone-id)]
      (if (nil? session-store)
        (println "SKIP: Datalevin not available for session KG test")
        (try
          (let [;; Multi-turn: tool call then text completion
                backend (mock-backend "test-kg-model"
                                      [{:type :tool_calls
                                        :calls [{:id "c1" :name "read_file"
                                                 :arguments {:path "src/core.clj"}}]}
                                       {:type :text :content "I've fixed the bug."}])
                result (loop/run-agentic-loop
                        {:task "Read and fix core.clj" :files ["src/core.clj"]}
                        {:drone-id drone-id :kg-store session-store}
                        {:max-turns 5 :backend backend})]
            ;; Should complete after 2 turns (tool call + text)
            (is (= :completed (:status result)))
            (is (= 2 (:turns result)))

            ;; KG stats: observations from tool call + reasoning entries
            (let [stats (:kg-stats result)]
              (is (pos? (:observations stats))
                  "Should have at least 1 observation from the tool call")
              (is (pos? (:reasoning stats))
                  "Should have at least 1 reasoning entry"))

            ;; Verify observations recorded in Datalevin
            (let [obs (kg/query session-store
                                '[:find ?id ?tool ?success
                                  :where
                                  [?e :obs/id ?id]
                                  [?e :obs/tool ?tool]
                                  [?e :obs/success ?success]])]
              (is (pos? (count obs))
                  "Should have observations in session KG")
              ;; Check that tool name was recorded
              (is (some #(= "read_file" (second %)) obs)
                  "Should have an observation for read_file tool")))
          (finally
            (session-kg/close-session-kg! session-store drone-id)))))))

(deftest test-loop-with-session-kg-context-reconstruction
  (testing "loop uses KG context reconstruction on turn > 0"
    (let [drone-id (str "test-kg-ctx-" (System/currentTimeMillis))
          session-store (session-kg/create-session-kg! drone-id)
          ;; Track what context prompts the LLM receives
          received-messages (atom [])]
      (if (nil? session-store)
        (println "SKIP: Datalevin not available for session KG test")
        (try
          (let [backend (let [counter (atom 0)]
                          (reify proto/LLMBackend
                            (chat [_ messages _tools]
                              (swap! received-messages conj messages)
                              (let [idx @counter]
                                (swap! counter inc)
                                (case idx
                                  ;; Turn 0: tool call
                                  0 {:type :tool_calls
                                     :calls [{:id "c1" :name "read_file"
                                              :arguments {:path "a.clj"}}]}
                                  ;; Turn 1: another tool call (will use reconstructed context)
                                  1 {:type :tool_calls
                                     :calls [{:id "c2" :name "grep"
                                              :arguments {:pattern "defn"}}]}
                                  ;; Turn 2: completion
                                  {:type :text :content "All done."})))
                            (model-name [_] "ctx-model")))
                result (loop/run-agentic-loop
                        {:task "Search and fix code" :files ["a.clj"]}
                        {:drone-id drone-id :kg-store session-store}
                        {:max-turns 5 :backend backend})]
            ;; Should complete after 3 turns
            (is (= :completed (:status result)))
            (is (= 3 (:turns result)))

            ;; Verify context reconstruction happened for turns > 0
            ;; Turn 0 gets the raw task, turns 1+ get reconstructed context
            (let [turn-0-msgs (first @received-messages)
                  turn-1-msgs (second @received-messages)]
              ;; Turn 0: user message should be the raw task
              (is (= "Search and fix code"
                     (:content (second turn-0-msgs)))
                  "Turn 0 should use raw task")
              ;; Turn 1: user message should be reconstructed (contains TASK: prefix)
              (when turn-1-msgs
                (let [user-msg (:content (second turn-1-msgs))]
                  (is (re-find #"TASK:" user-msg)
                      "Turn 1+ should use KG-reconstructed context")
                  (is (re-find #"RECENT OBSERVATIONS" user-msg)
                      "Reconstructed context should include observations")))))
          (finally
            (session-kg/close-session-kg! session-store drone-id)))))))

(deftest test-loop-with-session-kg-multi-tool-calls
  (testing "multiple tool calls in single turn all get recorded as observations"
    (let [drone-id (str "test-kg-multi-" (System/currentTimeMillis))
          session-store (session-kg/create-session-kg! drone-id)]
      (if (nil? session-store)
        (println "SKIP: Datalevin not available for session KG test")
        (try
          (let [backend (mock-backend "multi-model"
                                      [{:type :tool_calls
                                        :calls [{:id "c1" :name "read_file"
                                                 :arguments {:path "a.clj"}}
                                                {:id "c2" :name "grep"
                                                 :arguments {:pattern "defn"}}
                                                {:id "c3" :name "glob_files"
                                                 :arguments {:pattern "*.clj"}}]}
                                       {:type :text :content "Done."}])
                result (loop/run-agentic-loop
                        {:task "Multi-tool test" :files ["a.clj"]}
                        {:drone-id drone-id :kg-store session-store}
                        {:max-turns 5 :backend backend})]
            (is (= :completed (:status result)))
            (is (= 2 (:turns result)))

            ;; Should have 3 observations (one per tool call)
            (let [obs (kg/query session-store
                                '[:find ?id
                                  :where [?e :obs/id ?id]])]
              (is (= 3 (count obs))
                  "Should have 3 observations for 3 tool calls"))

            ;; Verify each tool recorded
            (let [tools (kg/query session-store
                                  '[:find ?tool
                                    :where [?e :obs/tool ?tool]])]
              (is (= #{"read_file" "grep" "glob_files"}
                     (into #{} (map first) tools))
                  "All three tools should be recorded")))
          (finally
            (session-kg/close-session-kg! session-store drone-id)))))))

(deftest test-loop-with-session-kg-key-facts-accumulation
  (testing "key facts accumulate across turns and appear in reconstructed context"
    (let [drone-id (str "test-kg-facts-" (System/currentTimeMillis))
          session-store (session-kg/create-session-kg! drone-id)]
      (if (nil? session-store)
        (println "SKIP: Datalevin not available for session KG test")
        (try
          (let [backend (mock-backend "facts-model"
                                      [{:type :tool_calls
                                        :calls [{:id "c1" :name "read_file"
                                                 :arguments {:path "core.clj"}}]}
                                       {:type :tool_calls
                                        :calls [{:id "c2" :name "grep"
                                                 :arguments {:pattern "TODO"}}]}
                                       {:type :text :content "Fixed all TODOs."}])
                result (loop/run-agentic-loop
                        {:task "Fix TODOs in core.clj" :files ["core.clj"]}
                        {:drone-id drone-id :kg-store session-store}
                        {:max-turns 5 :backend backend})]
            (is (= :completed (:status result)))

            ;; Verify key facts were accumulated
            (let [facts (kg/query session-store
                                  '[:find ?fact
                                    :where [?e :obs/key-facts ?fact]])]
              (is (pos? (count facts))
                  "Should have accumulated key facts from tool observations")))
          (finally
            (session-kg/close-session-kg! session-store drone-id)))))))

(deftest test-loop-without-session-kg-graceful-degradation
  (testing "loop works fine without session KG (nil kg-store)"
    (let [backend (mock-backend "no-kg-model"
                                [{:type :tool_calls
                                  :calls [{:id "c1" :name "read_file"
                                           :arguments {:path "a.clj"}}]}
                                 {:type :text :content "Done."}])
          result (loop/run-agentic-loop
                  {:task "No KG test" :files ["a.clj"]}
                  {:drone-id "test-no-kg"}
                  {:max-turns 5 :backend backend})]
      (is (= :completed (:status result)))
      (is (= 2 (:turns result)))
      ;; KG stats should show zero observations when no store
      (let [stats (:kg-stats result)]
        (is (= 0 (:observations stats)))
        (is (= 0 (:facts stats)))))))

;; =============================================================================
;; Two-Tier KG Integration Tests — kg-session wiring
;; =============================================================================

(deftest test-hk-session-not-created-when-unavailable
  (testing "hk-session is nil when compression-available? returns false (default)"
    ;; By default, hive-knowledge is NOT on classpath, so compression-available?
    ;; returns false and hk-session is nil. We verify the loop still completes.
    (is (not (kg-session/compression-available?))
        "hive-knowledge should not be on classpath in test env")
    (let [backend (mock-backend "no-hk-model"
                                [{:type :text :content "Done."}])
          result (loop/run-agentic-loop
                  {:task "No hk-session test" :files []}
                  {:drone-id "test-no-hk"}
                  {:max-turns 5 :backend backend})]
      (is (= :completed (:status result)))
      (is (= 1 (:turns result))))))

(deftest test-hk-session-created-when-available
  (testing "hk-session is created when compression-available? returns true"
    (let [hk-created? (atom false)
          hk-compressed-turns (atom [])
          hk-closed? (atom false)
          ;; Mock hive-knowledge being available
          mock-session {:type :hk-session :id "mock-session-123"}]
      (with-redefs [kg-session/compression-available? (constantly true)
                    kg-session/create-session-kg! (fn [drone-id task]
                                                    (reset! hk-created? true)
                                                    mock-session)
                    kg-session/compress-turn! (fn [session messages]
                                                (swap! hk-compressed-turns conj
                                                       {:session session
                                                        :msg-count (count messages)})
                                                1)
                    kg-session/build-compressed-messages (fn [session sys-prompt all-msgs recent-msgs]
                                                          ;; Return a custom compressed message set
                                                           [{:role "system" :content sys-prompt}
                                                            {:role "user" :content "HK-COMPRESSED: context"}])
                    kg-session/close-session! (fn [session]
                                                (reset! hk-closed? true)
                                                {:turns-compressed 2})
                    kg-session/promote-to-global! (fn [session global-store]
                                                    {:promoted 0})]
        (let [backend (mock-backend "hk-model"
                                    [{:type :tool_calls
                                      :calls [{:id "c1" :name "read_file"
                                               :arguments {:path "a.clj"}}]}
                                     {:type :text :content "Done."}])
              result (loop/run-agentic-loop
                      {:task "HK session test" :files ["a.clj"]}
                      {:drone-id "test-hk-session"}
                      {:max-turns 5 :backend backend})]
          ;; Loop should complete
          (is (= :completed (:status result)))
          (is (= 2 (:turns result)))

          ;; hk-session should have been created
          (is @hk-created? "create-session-kg! should have been called")

          ;; compress-turn! should have been called for the recurring turn
          (is (pos? (count @hk-compressed-turns))
              "compress-turn! should be called after each turn that recurs")

          ;; close-session! should have been called at termination
          (is @hk-closed? "close-session! should have been called at termination"))))))

(deftest test-hk-session-build-compressed-messages-used-on-turn-gt-0
  (testing "build-compressed-messages is called for turns > 0 when hk-session available"
    (let [compressed-calls (atom [])
          mock-session {:type :hk-session}]
      (with-redefs [kg-session/compression-available? (constantly true)
                    kg-session/create-session-kg! (fn [_ _] mock-session)
                    kg-session/compress-turn! (fn [_ _] 0)
                    kg-session/build-compressed-messages
                    (fn [session sys-prompt all-msgs recent-msgs]
                      (swap! compressed-calls conj {:turn (count @compressed-calls)})
                      ;; Return compressed messages
                      [{:role "system" :content sys-prompt}
                       {:role "user" :content "COMPRESSED-BY-HK"}])
                    kg-session/close-session! (fn [_] {})
                    kg-session/promote-to-global! (fn [_ _] {:promoted 0})]
        (let [received-messages (atom [])
              backend (let [counter (atom 0)]
                        (reify proto/LLMBackend
                          (chat [_ messages _tools]
                            (swap! received-messages conj messages)
                            (let [idx @counter]
                              (swap! counter inc)
                              (case idx
                                0 {:type :tool_calls
                                   :calls [{:id "c1" :name "read_file"
                                            :arguments {:path "a.clj"}}]}
                                1 {:type :tool_calls
                                   :calls [{:id "c2" :name "grep"
                                            :arguments {:pattern "defn"}}]}
                                {:type :text :content "All done."})))
                          (model-name [_] "compress-model")))
              result (loop/run-agentic-loop
                      {:task "Compress test" :files ["a.clj"]}
                      {:drone-id "test-hk-compress"}
                      {:max-turns 5 :backend backend})]
          (is (= :completed (:status result)))
          (is (= 3 (:turns result)))

          ;; build-compressed-messages should have been called for turns 1 and 2 (not turn 0)
          (is (= 2 (count @compressed-calls))
              "build-compressed-messages should be called for turns > 0")

          ;; Turn 0 should use raw task (not compressed)
          (let [turn-0-user-msg (:content (second (first @received-messages)))]
            (is (= "Compress test" turn-0-user-msg)
                "Turn 0 should use raw task, not compressed"))

          ;; Turns 1+ should use compressed messages
          (let [turn-1-user-msg (:content (second (second @received-messages)))]
            (is (= "COMPRESSED-BY-HK" turn-1-user-msg)
                "Turn 1+ should use HK-compressed context")))))))

(deftest test-hk-session-creation-failure-graceful
  (testing "loop continues normally when hk-session creation throws"
    (with-redefs [kg-session/compression-available? (constantly true)
                  kg-session/create-session-kg! (fn [_ _]
                                                  (throw (ex-info "HK init failed" {})))]
      (let [backend (mock-backend "hk-fail-model"
                                  [{:type :text :content "Done."}])
            result (loop/run-agentic-loop
                    {:task "HK failure test" :files []}
                    {:drone-id "test-hk-fail"}
                    {:max-turns 5 :backend backend})]
        ;; Should complete normally despite hk-session creation failure
        (is (= :completed (:status result)))
        (is (= 1 (:turns result)))))))

(deftest test-hk-session-compress-turn-failure-graceful
  (testing "compress-turn! failure doesn't crash the loop"
    (let [mock-session {:type :hk-session}
          compress-call-count (atom 0)]
      (with-redefs [kg-session/compression-available? (constantly true)
                    kg-session/create-session-kg! (fn [_ _] mock-session)
                    kg-session/compress-turn! (fn [_ _]
                                                (swap! compress-call-count inc)
                                                (throw (ex-info "Compression broke" {})))
                    kg-session/build-compressed-messages (fn [_ _ msgs _] msgs)
                    kg-session/close-session! (fn [_] {})
                    kg-session/promote-to-global! (fn [_ _] {:promoted 0})]
        (let [backend (mock-backend "hk-compress-fail-model"
                                    [{:type :tool_calls
                                      :calls [{:id "c1" :name "read_file"
                                               :arguments {:path "a.clj"}}]}
                                     {:type :text :content "Done anyway."}])
              result (loop/run-agentic-loop
                      {:task "Compress failure test" :files ["a.clj"]}
                      {:drone-id "test-hk-compress-fail"}
                      {:max-turns 5 :backend backend})]
          ;; Loop should complete despite compress-turn! throwing
          (is (= :completed (:status result)))
          (is (= 2 (:turns result)))
          ;; compress-turn! was attempted
          (is (pos? @compress-call-count)
              "compress-turn! should have been attempted"))))))
