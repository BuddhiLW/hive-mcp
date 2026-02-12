(ns hive-mcp.agent.sdk.lifecycle-test
  "Comprehensive unit tests for sdk.lifecycle — spawn, dispatch, kill, interrupt, status.

   Strategy:
   - Session registry (atom) operations run against real implementation
   - Python bridge deps (event-loop, execution, options, availability) are mocked with with-redefs
   - Channel-based tests use drain-channel helper with timeout
   - Fixture cleans registry between tests"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.core.async :as async :refer [chan >!! <!! close! timeout alts!!]]
            [hive-mcp.agent.sdk.lifecycle :as lifecycle]
            [hive-mcp.agent.sdk.availability :as avail]
            [hive-mcp.agent.sdk.event-loop :as event-loop]
            [hive-mcp.agent.sdk.execution :as exec]
            [hive-mcp.agent.sdk.options :as opts]
            [hive-mcp.agent.sdk.phase-compress :as phase-compress]
            [hive-mcp.agent.sdk.session :as session]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn drain-channel
  "Read all messages from channel with timeout. Returns vector of messages."
  [ch & {:keys [timeout-ms] :or {timeout-ms 5000}}]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop [msgs []]
      (let [remaining (- deadline (System/currentTimeMillis))]
        (if (<= remaining 0)
          msgs
          (let [to (timeout (min 200 remaining))
                [v port] (alts!! [ch to])]
            (if (and (= port ch) (some? v))
              (recur (conj msgs v))
              msgs)))))))

(defn mock-phase-ch
  "Create a pre-loaded channel that emits messages then closes."
  [messages]
  (let [ch (chan (max 1 (count messages)))]
    (doseq [m messages]
      (>!! ch m))
    (close! ch)
    ch))

(defn make-base-session
  "Create a minimal session map for testing."
  [ling-id & {:keys [phase observations client-ref py-loop-var turn-count cwd]
              :or {phase :idle observations [] client-ref "mock-client"
                   py-loop-var "mock-loop" turn-count 0 cwd "/tmp/test"}}]
  {:ling-id ling-id
   :phase phase
   :phase-history []
   :observations observations
   :plan nil
   :message-ch (chan 1)
   :result-ch (chan 1)
   :started-at (System/currentTimeMillis)
   :cwd cwd
   :turn-count turn-count
   :client-ref client-ref
   :py-loop-var py-loop-var
   :py-safe-id (session/ling-id->safe-id ling-id)})

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn clean-sessions [f]
  (let [registry (session/session-registry-ref)]
    ;; Close channels from previous test
    (doseq [[_ sess] @registry]
      (when-let [ch (:message-ch sess)] (close! ch))
      (when-let [ch (:result-ch sess)] (close! ch)))
    (reset! registry {})
    (f)
    ;; Close channels after test
    (doseq [[_ sess] @registry]
      (when-let [ch (:message-ch sess)] (close! ch))
      (when-let [ch (:result-ch sess)] (close! ch)))
    (reset! registry {})))

(use-fixtures :each clean-sessions)

;; =============================================================================
;; spawn-headless-sdk! Tests
;; =============================================================================

(deftest spawn-happy-path
  (testing "successfully spawns SDK session with correct return value"
    (with-redefs [avail/sdk-status (constantly :available)
                  event-loop/start-session-loop!
                  (fn [safe-id] {:loop-var (str "_loop_" safe-id)
                                 :thread-var (str "_thread_" safe-id)})
                  opts/build-base-options-obj (fn [_] :mock-options)
                  event-loop/connect-session-client!
                  (fn [safe-id _ _] (str "_client_" safe-id))]
      (let [result (lifecycle/spawn-headless-sdk!
                    "test-spawn-1" {:cwd "/tmp/test"})]
        (is (= {:ling-id "test-spawn-1"
                :status :spawned
                :backend :agent-sdk
                :phase :idle} result))))))

(deftest spawn-registers-session-correctly
  (testing "session registry contains correct data after spawn"
    (with-redefs [avail/sdk-status (constantly :available)
                  event-loop/start-session-loop!
                  (fn [safe-id] {:loop-var "mock-loop" :thread-var "mock-thread"})
                  opts/build-base-options-obj (fn [_] :mock-options)
                  event-loop/connect-session-client!
                  (fn [_ _ _] "mock-client-var")]
      (lifecycle/spawn-headless-sdk!
       "test-spawn-data" {:cwd "/tmp/test"
                          :system-prompt "test prompt"
                          :presets ["ling"]})
      (let [sess (session/get-session "test-spawn-data")]
        (is (some? sess))
        (is (= :idle (:phase sess)))
        (is (= [] (:phase-history sess)))
        (is (= [] (:observations sess)))
        (is (= "/tmp/test" (:cwd sess)))
        (is (= "test prompt" (:system-prompt sess)))
        (is (= ["ling"] (:presets sess)))
        (is (= 0 (:turn-count sess)))
        (is (some? (:message-ch sess)))
        (is (some? (:result-ch sess)))
        (is (= "mock-client-var" (:client-ref sess)))
        (is (= "mock-loop" (:py-loop-var sess)))
        (is (number? (:started-at sess)))))))

(deftest spawn-passes-correct-args-to-deps
  (testing "spawn wires ling-id, cwd, agents through to deps correctly"
    (let [start-loop-args (atom nil)
          build-opts-args (atom nil)
          connect-args (atom nil)]
      (with-redefs [avail/sdk-status (constantly :available)
                    event-loop/start-session-loop!
                    (fn [safe-id]
                      (reset! start-loop-args safe-id)
                      {:loop-var "test-loop" :thread-var "test-thread"})
                    opts/build-base-options-obj
                    (fn [opts-map]
                      (reset! build-opts-args opts-map)
                      :test-options)
                    event-loop/connect-session-client!
                    (fn [safe-id base-opts loop-var]
                      (reset! connect-args {:safe-id safe-id
                                            :opts base-opts
                                            :loop-var loop-var})
                      "test-client")]
        (lifecycle/spawn-headless-sdk!
         "my-ling-123" {:cwd "/projects/test"
                        :system-prompt "do stuff"
                        :agents {:helper {:description "helps"}}})
        ;; ling-id->safe-id converts "my-ling-123" to "my_ling_123"
        (is (= "my_ling_123" @start-loop-args))
        (is (= {:cwd "/projects/test"
                :system-prompt "do stuff"
                :agents {:helper {:description "helps"}}}
               @build-opts-args))
        (is (= "my_ling_123" (:safe-id @connect-args)))
        (is (= :test-options (:opts @connect-args)))
        (is (= "test-loop" (:loop-var @connect-args)))))))

(deftest spawn-throws-when-sdk-not-available
  (doseq [status [:no-libpython :no-sdk :not-initialized]]
    (testing (str "throws when sdk-status is " status)
      (with-redefs [avail/sdk-status (constantly status)]
        (let [ex (try (lifecycle/spawn-headless-sdk! "test" {:cwd "/tmp"})
                      nil
                      (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= status (:sdk-status (ex-data ex))))
          (is (some? (:hint (ex-data ex)))))))))

(deftest spawn-throws-on-duplicate-ling-id
  (testing "throws when ling-id already registered"
    (with-redefs [avail/sdk-status (constantly :available)]
      (session/register-session! "existing-ling" {:ling-id "existing-ling"})
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"SDK session already exists"
           (lifecycle/spawn-headless-sdk! "existing-ling" {:cwd "/tmp"}))))))

(deftest spawn-precondition-failures
  (testing "non-string ling-id"
    (is (thrown? AssertionError
                (lifecycle/spawn-headless-sdk! 123 {:cwd "/tmp"}))))
  (testing "non-string cwd"
    (is (thrown? AssertionError
                (lifecycle/spawn-headless-sdk! "test" {:cwd 123}))))
  (testing "nil ling-id"
    (is (thrown? AssertionError
                (lifecycle/spawn-headless-sdk! nil {:cwd "/tmp"})))))

;; =============================================================================
;; dispatch-headless-sdk! Tests
;; =============================================================================

(deftest dispatch-throws-when-session-not-found
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"SDK session not found"
       (lifecycle/dispatch-headless-sdk! "nonexistent" "do task"))))

(deftest dispatch-throws-when-no-client-ref
  (testing "throws when session has no persistent client"
    (session/register-session! "no-client" {:ling-id "no-client"
                                            :client-ref nil})
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"No persistent client"
         (lifecycle/dispatch-headless-sdk! "no-client" "task")))))

(deftest dispatch-precondition-failures
  (testing "non-string ling-id"
    (is (thrown? AssertionError
                (lifecycle/dispatch-headless-sdk! 123 "task"))))
  (testing "non-string task"
    (is (thrown? AssertionError
                (lifecycle/dispatch-headless-sdk! "test" 123)))))

(deftest dispatch-raw-mode
  (testing "raw mode sends task directly without SAA phases"
    (session/register-session! "raw-1" (make-base-session "raw-1"))
    (let [phase-calls (atom [])]
      (with-redefs [exec/execute-phase!
                    (fn [_ _ phase]
                      (swap! phase-calls conj phase)
                      (mock-phase-ch [{:type :message :data "raw-result"}]))]
        (let [out-ch (lifecycle/dispatch-headless-sdk!
                      "raw-1" "do something" {:raw? true})
              msgs (drain-channel out-ch)]
          (is (= [:dispatch] @phase-calls)
              "Raw mode should call execute-phase! with :dispatch")
          (is (some #(= :dispatch (:saa-phase %)) msgs)
              "Messages should be tagged with :dispatch")
          (is (some #(= :saa-complete (:type %)) msgs)
              "Should emit completion message"))))))

(deftest dispatch-full-saa-cycle
  (testing "runs silence -> abstract -> act with compression between phases"
    (session/register-session! "saa-1" (make-base-session "saa-1"))
    (let [phase-calls (atom [])]
      (with-redefs [exec/execute-phase!
                    (fn [_ _ phase]
                      (swap! phase-calls conj phase)
                      (mock-phase-ch [{:type :message
                                       :data (str "result-" (name phase))}]))
                    phase-compress/resolve-compressor
                    (fn [] (phase-compress/->NoOpPhaseCompressor))]
        (let [out-ch (lifecycle/dispatch-headless-sdk!
                      "saa-1" "implement feature")
              msgs (drain-channel out-ch)]
          ;; All three SAA phases should be called in order
          (is (= [:silence :abstract :act] @phase-calls))
          ;; Messages from each phase appear on output
          (is (some #(= :silence (:saa-phase %)) msgs))
          (is (some #(= :abstract (:saa-phase %)) msgs))
          (is (some #(= :act (:saa-phase %)) msgs))
          ;; Completion message emitted
          (is (some #(= :saa-complete (:type %)) msgs)))))))

(deftest dispatch-skip-silence
  (testing "skip-silence? skips silence phase but runs abstract and act"
    (session/register-session! "skip-s" (make-base-session "skip-s"))
    (let [phase-calls (atom [])]
      (with-redefs [exec/execute-phase!
                    (fn [_ _ phase]
                      (swap! phase-calls conj phase)
                      (mock-phase-ch [{:type :message :data "result"}]))
                    phase-compress/resolve-compressor
                    (fn [] (phase-compress/->NoOpPhaseCompressor))]
        (let [out-ch (lifecycle/dispatch-headless-sdk!
                      "skip-s" "task" {:skip-silence? true})
              msgs (drain-channel out-ch)]
          (is (not (some #{:silence} @phase-calls))
              "Silence phase should be skipped")
          (is (some #{:abstract} @phase-calls))
          (is (some #{:act} @phase-calls)))))))

(deftest dispatch-skip-abstract
  (testing "skip-abstract? skips abstract phase but runs silence and act"
    (session/register-session! "skip-a" (make-base-session "skip-a"))
    (let [phase-calls (atom [])]
      (with-redefs [exec/execute-phase!
                    (fn [_ _ phase]
                      (swap! phase-calls conj phase)
                      (mock-phase-ch [{:type :message :data "result"}]))
                    phase-compress/resolve-compressor
                    (fn [] (phase-compress/->NoOpPhaseCompressor))]
        (let [out-ch (lifecycle/dispatch-headless-sdk!
                      "skip-a" "task" {:skip-abstract? true})
              msgs (drain-channel out-ch)]
          (is (some #{:silence} @phase-calls))
          (is (not (some #{:abstract} @phase-calls))
              "Abstract phase should be skipped")
          (is (some #{:act} @phase-calls)))))))

(deftest dispatch-single-phase-only
  (testing ":phase option runs only the specified phase"
    (session/register-session! "single-p" (make-base-session "single-p"))
    (let [phase-calls (atom [])]
      (with-redefs [exec/execute-phase!
                    (fn [_ _ phase]
                      (swap! phase-calls conj phase)
                      (mock-phase-ch [{:type :message :data "result"}]))
                    phase-compress/resolve-compressor
                    (fn [] (phase-compress/->NoOpPhaseCompressor))]
        (let [out-ch (lifecycle/dispatch-headless-sdk!
                      "single-p" "task" {:phase :abstract})
              msgs (drain-channel out-ch)]
          (is (= [:abstract] @phase-calls)
              "Only the specified phase should execute"))))))

(deftest dispatch-error-handling
  (testing "exceptions in phase execution are caught and emitted as :error"
    (session/register-session! "err-1" (make-base-session "err-1"))
    (with-redefs [exec/execute-phase!
                  (fn [_ _ _]
                    (throw (Exception. "phase exploded")))]
      (let [out-ch (lifecycle/dispatch-headless-sdk!
                    "err-1" "task" {:raw? true})
            msgs (drain-channel out-ch)]
        (is (some #(= :error (:type %)) msgs)
            "Error should be emitted on channel")
        (is (some #(= "phase exploded" (:error %)) msgs))))))

(deftest dispatch-stores-dispatch-context
  (testing "dispatch-context from opts is stored in session"
    (session/register-session!
     "ctx-1" (make-base-session "ctx-1"))
    (let [mock-ctx {:ctx-refs {:axioms "ctx-123"}
                    :kg-node-ids ["node-1"]
                    :scope "test"}]
      (with-redefs [exec/execute-phase!
                    (fn [_ _ _] (mock-phase-ch []))
                    phase-compress/resolve-compressor
                    (fn [] (phase-compress/->NoOpPhaseCompressor))]
        (let [out-ch (lifecycle/dispatch-headless-sdk!
                      "ctx-1" "task" {:raw? true
                                      :dispatch-context mock-ctx})
              _ (drain-channel out-ch)]
          (is (= mock-ctx
                 (:dispatch-context (session/get-session "ctx-1")))))))))

(deftest dispatch-silence-accumulates-observations
  (testing "silence phase accumulates :message data as observations"
    (session/register-session! "obs-1" (make-base-session "obs-1"))
    (with-redefs [exec/execute-phase!
                  (fn [_ _ phase]
                    (case phase
                      :silence (mock-phase-ch [{:type :message :data "found file A"}
                                               {:type :message :data "found pattern B"}])
                      (mock-phase-ch [{:type :message :data "ok"}])))
                  phase-compress/resolve-compressor
                  (fn [] (phase-compress/->NoOpPhaseCompressor))]
      (let [out-ch (lifecycle/dispatch-headless-sdk! "obs-1" "explore task")
            msgs (drain-channel out-ch)]
        ;; After full SAA, compression clears observations.
        ;; But we can verify the compression result includes them.
        (let [sess (session/get-session "obs-1")]
          ;; compressed-context from silence->abstract should contain
          ;; the silence observations
          (is (some? (:compressed-context sess))))))))

;; =============================================================================
;; kill-headless-sdk! Tests
;; =============================================================================

(deftest kill-happy-path
  (testing "kills session with graceful teardown"
    (let [disconnect-called (atom false)
          stop-called (atom false)]
      (session/register-session!
       "kill-1" (make-base-session "kill-1"))
      (with-redefs [event-loop/disconnect-session-client!
                    (fn [_ _ _] (reset! disconnect-called true))
                    event-loop/stop-session-loop!
                    (fn [_ _] (reset! stop-called true))]
        (let [result (lifecycle/kill-headless-sdk! "kill-1")]
          (is (= {:killed? true :ling-id "kill-1"} result))
          (is @disconnect-called "disconnect should be called")
          (is @stop-called "stop should be called")
          (is (nil? (session/get-session "kill-1"))
              "Session should be unregistered"))))))

(deftest kill-session-not-found
  (testing "throws when session doesn't exist"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SDK session not found"
         (lifecycle/kill-headless-sdk! "nonexistent")))))

(deftest kill-without-client-loop
  (testing "kills session even without client/loop refs"
    (session/register-session!
     "kill-no-py" (make-base-session "kill-no-py"
                                     :client-ref nil
                                     :py-loop-var nil))
    (let [disconnect-called (atom false)]
      (with-redefs [event-loop/disconnect-session-client!
                    (fn [_ _ _] (reset! disconnect-called true))
                    event-loop/stop-session-loop!
                    (fn [_ _] nil)]
        (let [result (lifecycle/kill-headless-sdk! "kill-no-py")]
          (is (= {:killed? true :ling-id "kill-no-py"} result))
          (is (not @disconnect-called)
              "Should not disconnect when no client/loop")
          (is (nil? (session/get-session "kill-no-py"))))))))

;; =============================================================================
;; interrupt-headless-sdk! Tests
;; =============================================================================

(deftest interrupt-success
  (testing "successful interrupt returns success map"
    (session/register-session!
     "int-1" (make-base-session "int-1" :phase :silence))
    (with-redefs [event-loop/interrupt-session-client!
                  (fn [_ _] {:success? true})]
      (let [result (lifecycle/interrupt-headless-sdk! "int-1")]
        (is (:success? result))
        (is (= "int-1" (:ling-id result)))
        (is (= :silence (:phase result)))))))

(deftest interrupt-session-not-found
  (testing "returns error map when session doesn't exist"
    (let [result (lifecycle/interrupt-headless-sdk! "nonexistent")]
      (is (not (:success? result)))
      (is (= "nonexistent" (:ling-id result)))
      (is (some #(re-find #"not found" %) (:errors result))))))

(deftest interrupt-no-client-ref
  (testing "returns error when no client-ref or py-loop-var"
    (session/register-session!
     "int-no-client" (make-base-session "int-no-client"
                                        :client-ref nil
                                        :py-loop-var nil
                                        :phase :idle))
    (let [result (lifecycle/interrupt-headless-sdk! "int-no-client")]
      (is (not (:success? result)))
      (is (seq (:errors result)))
      (is (some #(re-find #"No active phase" %) (:errors result))))))

(deftest interrupt-bridge-failure
  (testing "returns error map when Python bridge fails"
    (session/register-session!
     "int-fail" (make-base-session "int-fail" :phase :act))
    (with-redefs [event-loop/interrupt-session-client!
                  (fn [_ _] {:success? false :error "Python bridge error"})]
      (let [result (lifecycle/interrupt-headless-sdk! "int-fail")]
        (is (not (:success? result)))
        (is (seq (:errors result)))))))

;; =============================================================================
;; sdk-status-for Tests
;; =============================================================================

(deftest status-for-existing-session
  (testing "returns full status map for registered session"
    (let [now (System/currentTimeMillis)]
      (session/register-session!
       "status-1" {:ling-id "status-1"
                   :phase :silence
                   :phase-history [{:phase :idle}]
                   :observations ["obs1" "obs2"]
                   :started-at now
                   :cwd "/tmp/test"
                   :session-id "sess-123"
                   :turn-count 3
                   :client-ref "client-var"
                   :py-loop-var "loop-var"})
      (let [status (lifecycle/sdk-status-for "status-1")]
        (is (= "status-1" (:ling-id status)))
        (is (= :silence (:phase status)))
        (is (= [{:phase :idle}] (:phase-history status)))
        (is (= 2 (:observations-count status)))
        (is (= :agent-sdk (:backend status)))
        (is (= "sess-123" (:session-id status)))
        (is (= 3 (:turn-count status)))
        (is (true? (:has-persistent-client? status)))
        (is (true? (:interruptable? status)))
        (is (>= (:uptime-ms status) 0))
        (is (= "/tmp/test" (:cwd status)))))))

(deftest status-for-nonexistent
  (testing "returns nil for non-existent session"
    (is (nil? (lifecycle/sdk-status-for "nonexistent")))))

(deftest status-for-minimal-session
  (testing "handles missing turn-count and nil client gracefully"
    (session/register-session!
     "status-min" {:ling-id "status-min"
                   :phase :idle
                   :phase-history []
                   :observations []
                   :started-at (System/currentTimeMillis)
                   :cwd "/tmp"
                   :client-ref nil
                   :py-loop-var nil})
    (let [status (lifecycle/sdk-status-for "status-min")]
      (is (= 0 (:turn-count status))
          "Missing turn-count should default to 0")
      (is (false? (:has-persistent-client? status)))
      (is (false? (:interruptable? status))))))

;; =============================================================================
;; list-sdk-sessions Tests
;; =============================================================================

(deftest list-sessions-empty
  (testing "returns empty vector when no sessions"
    (is (= [] (lifecycle/list-sdk-sessions)))))

(deftest list-sessions-multiple
  (testing "returns status for all registered sessions"
    (let [now (System/currentTimeMillis)]
      (session/register-session!
       "list-1" {:ling-id "list-1" :phase :idle :observations []
                 :started-at now :cwd "/tmp" :turn-count 0
                 :phase-history [] :client-ref nil :py-loop-var nil})
      (session/register-session!
       "list-2" {:ling-id "list-2" :phase :act :observations ["x"]
                 :started-at now :cwd "/tmp" :turn-count 2
                 :phase-history [] :client-ref "c" :py-loop-var "l"})
      (let [sessions (lifecycle/list-sdk-sessions)]
        (is (= 2 (count sessions)))
        (is (every? :ling-id sessions))
        (is (= #{:idle :act} (set (map :phase sessions))))))))

;; =============================================================================
;; sdk-session? Tests
;; =============================================================================

(deftest sdk-session-exists
  (testing "returns true for registered session"
    (session/register-session! "exists-1" {:ling-id "exists-1"})
    (is (true? (lifecycle/sdk-session? "exists-1")))))

(deftest sdk-session-not-exists
  (testing "returns false for non-existent session"
    (is (false? (lifecycle/sdk-session? "nonexistent")))))

;; =============================================================================
;; kill-all-sdk! Tests
;; =============================================================================

(deftest kill-all-happy-path
  (testing "kills all sessions and returns count"
    (session/register-session!
     "all-1" (make-base-session "all-1"))
    (session/register-session!
     "all-2" (make-base-session "all-2"))
    (with-redefs [event-loop/disconnect-session-client! (fn [_ _ _] nil)
                  event-loop/stop-session-loop! (fn [_ _] nil)]
      (let [result (lifecycle/kill-all-sdk!)]
        (is (= 2 (:killed result)))
        (is (= 0 (:errors result)))
        (is (empty? @(session/session-registry-ref)))))))

(deftest kill-all-empty-registry
  (testing "returns zeros when no sessions exist"
    (let [result (lifecycle/kill-all-sdk!)]
      (is (= 0 (:killed result)))
      (is (= 0 (:errors result))))))

(deftest kill-all-partial-failure
  (testing "counts successes and failures separately"
    (session/register-session!
     "fail-1" (make-base-session "fail-1"))
    (session/register-session!
     "fail-2" (make-base-session "fail-2"))
    (with-redefs [event-loop/disconnect-session-client!
                  (fn [safe-id _ _]
                    (when (= safe-id "fail_1")
                      (throw (Exception. "disconnect failed"))))
                  event-loop/stop-session-loop! (fn [_ _] nil)]
      (let [result (lifecycle/kill-all-sdk!)]
        (is (= 2 (+ (:killed result) (:errors result)))
            "Total should equal number of sessions")
        (is (= 1 (:errors result))
            "One session should fail")
        (is (= 1 (:killed result))
            "One session should succeed")))))

;; =============================================================================
;; Private Function Tests
;; =============================================================================

(deftest forward-phase-messages-tags-correctly
  (testing "forwards messages and tags with phase-key"
    (let [in-ch (chan 3)
          out-ch (chan 3)
          forward-fn @#'lifecycle/forward-phase-messages!]
      (>!! in-ch {:type :message :data "msg1"})
      (>!! in-ch {:type :message :data "msg2"})
      (close! in-ch)
      (forward-fn in-ch out-ch :silence)
      (let [msg1 (<!! out-ch)
            msg2 (<!! out-ch)]
        (is (= :silence (:saa-phase msg1)))
        (is (= "msg1" (:data msg1)))
        (is (= :silence (:saa-phase msg2)))
        (is (= "msg2" (:data msg2)))))))

(deftest forward-phase-messages-empty-channel
  (testing "handles empty input channel gracefully"
    (let [in-ch (chan 1)
          out-ch (chan 1)
          forward-fn @#'lifecycle/forward-phase-messages!]
      (close! in-ch)
      (forward-fn in-ch out-ch :act)
      ;; out-ch should have nothing
      (let [[v _] (alts!! [out-ch (timeout 100)])]
        (is (nil? v))))))

(deftest compress-and-transition-updates-session
  (testing "compresses observations and updates session for next phase"
    (let [compress-fn @#'lifecycle/compress-and-transition!]
      (session/register-session!
       "compress-test" {:ling-id "compress-test"
                        :phase :silence
                        :observations ["obs1" "obs2" "obs3"]
                        :cwd "/tmp"
                        :project-id "test-proj"})
      (with-redefs [phase-compress/resolve-compressor
                    (fn [] (phase-compress/->NoOpPhaseCompressor))]
        (let [result (compress-fn "compress-test" :silence :abstract)]
          (is (= :noop (:compressor result)))
          (is (= 0 (:entries-created result)))
          (is (string? (:compressed-context result)))
          ;; Verify session was updated
          (let [sess (session/get-session "compress-test")]
            (is (= :abstract (:phase sess)))
            (is (= [] (:observations sess)))
            (is (some? (:compressed-context sess)))))))))

(deftest emit-dispatch-complete-emits-message
  (testing "puts :saa-complete message on channel"
    (let [emit-fn @#'lifecycle/emit-dispatch-complete!
          out-ch (chan 1)]
      (session/register-session!
       "emit-test" {:ling-id "emit-test"
                    :turn-count 5
                    :observations ["a" "b"]})
      (emit-fn "emit-test" out-ch)
      (let [msg (<!! out-ch)]
        (is (= :saa-complete (:type msg)))
        (is (= "emit-test" (:ling-id msg)))
        (is (= 5 (:turn-count msg)))
        (is (= 2 (:observations-count msg)))))))
