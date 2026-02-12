(ns hive-mcp.server.piggyback-middleware-test
  "Tests for piggyback middleware cursor identity stability.

   Pins down the bug where wrap-handler-piggyback and wrap-handler-memory-piggyback
   used extract-agent-id from tool args, causing dispatch-type tools (which pass
   target agent_id in args) to create spurious cursor keys per target.

   The fix: middleware always uses a stable 'coordinator-{project-id}' identity
   for the piggyback cursor, regardless of what agent_id appears in tool args."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.channel.piggyback :as pb]
            [hive-mcp.channel.memory-piggyback :as mp]
            [hive-mcp.agent.context :as ctx]))

;; The project-id that extract-project-id resolves for our test directory.
;; All shouts and buffers must use this to match the middleware's resolution.
(def ^:private test-dir "/home/lages/PP/hive/hive-mcp")
(def ^:private test-project "hive-mcp")

;; Reset all state between tests
(use-fixtures :each
  (fn [f]
    (pb/reset-all-cursors!)
    (mp/reset-all!)
    (f)
    (pb/reset-all-cursors!)
    (mp/reset-all!)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- dummy-handler
  "Handler that returns normalized content (vector of text items)."
  [_args]
  [{:type "text" :text "{\"success\": true}"}])

(defn- extract-hivemind-block
  "Extract ---HIVEMIND--- block content from handler response."
  [content]
  (some (fn [{:keys [text]}]
          (when text
            (second (re-find #"---HIVEMIND---\n([\s\S]*?)\n---/HIVEMIND---" text))))
        content))

(defn- extract-memory-block
  "Extract ---MEMORY--- block content from handler response."
  [content]
  (some (fn [{:keys [text]}]
          (when text
            (second (re-find #"---MEMORY---\n([\s\S]*?)\n---/MEMORY---" text))))
        content))

(defn- call-wrapped
  "Call a wrapped handler with args, binding ctx from the same args.
   Simulates the full context binding that wrap-handler-context does."
  [wrapped args]
  (let [agent-id (:agent_id args)
        project-id test-project]
    (ctx/with-request-context (cond-> {:project-id project-id
                                       :directory test-dir}
                                agent-id (assoc :agent-id agent-id))
      (wrapped (assoc args :directory test-dir)))))

;; =============================================================================
;; Hivemind Piggyback Middleware — Cursor Identity
;; =============================================================================

(deftest hivemind-piggyback-stable-cursor-across-dispatch-targets-test
  (testing "wrap-handler-piggyback uses stable cursor regardless of agent_id in args"
    (let [shouts (atom [{:agent-id "ling-1" :event-type :progress
                         :message "working" :timestamp 1000
                         :project-id test-project}])]
      (pb/register-message-source! (fn [] @shouts))

      (let [wrapped (routes/wrap-handler-piggyback dummy-handler)]
        ;; Call 1: simulate `agent dispatch agent_id="swarm-target-A"`
        (let [r1 (call-wrapped wrapped {:agent_id "swarm-target-A"})]
          (is (some? (extract-hivemind-block r1))
              "first call: shouts delivered"))

        ;; Call 2: DIFFERENT target — cursor should be the SAME coordinator key
        (let [r2 (call-wrapped wrapped {:agent_id "swarm-target-B"})]
          (is (nil? (extract-hivemind-block r2))
              "second call with different target: NO re-delivery (cursor stable)"))

        ;; Call 3: yet another target
        (let [r3 (call-wrapped wrapped {:agent_id "swarm-target-C"})]
          (is (nil? (extract-hivemind-block r3))
              "third call: still no re-delivery"))))))

(deftest hivemind-piggyback-delivers-new-shouts-after-cursor-advance-test
  (testing "New shouts are still delivered after cursor advances"
    (let [shouts (atom [{:agent-id "ling-1" :event-type :started
                         :message "started" :timestamp 1000
                         :project-id test-project}])]
      (pb/register-message-source! (fn [] @shouts))

      (let [wrapped (routes/wrap-handler-piggyback dummy-handler)]
        ;; First call: drains existing shout
        (call-wrapped wrapped {})

        ;; New shout arrives
        (swap! shouts conj {:agent-id "ling-2" :event-type :completed
                            :message "done" :timestamp 2000
                            :project-id test-project})

        ;; Second call: new shout delivered
        (let [r2 (call-wrapped wrapped {})]
          (is (some? (extract-hivemind-block r2))
              "new shout delivered after cursor advance"))))))

;; =============================================================================
;; Memory Piggyback Middleware — Cursor Identity
;; =============================================================================

(deftest memory-piggyback-stable-cursor-across-dispatch-targets-test
  (testing "wrap-handler-memory-piggyback uses stable cursor regardless of agent_id in args"
    (let [coordinator-id (str "coordinator-" test-project)]
      ;; Enqueue for the coordinator identity (matches middleware's cursor key)
      (mp/enqueue! coordinator-id test-project
                   [{:id "ax-1" :type "axiom" :content "Rule 1" :tags ["axiom"]}
                    {:id "cv-1" :type "convention" :content "Conv 1" :tags ["convention"]}])

      (let [wrapped (routes/wrap-handler-memory-piggyback dummy-handler)]
        ;; Call 1: with target agent_id in args
        (let [r1 (call-wrapped wrapped {:agent_id "swarm-target-X"})]
          (is (some? (extract-memory-block r1))
              "first call: memory entries drained for coordinator key"))

        ;; Call 2: different target — buffer already drained
        (let [r2 (call-wrapped wrapped {:agent_id "swarm-target-Y"})]
          (is (nil? (extract-memory-block r2))
              "second call: buffer already drained, no re-delivery"))))))

(deftest memory-piggyback-no-args-agent-id-uses-coordinator-test
  (testing "Tools without agent_id in args still drain correctly (coordinator default)"
    (let [coordinator-id (str "coordinator-" test-project)]
      (mp/enqueue! coordinator-id test-project
                   [{:id "n-1" :type "note" :content "A note" :tags []}])

      (let [wrapped (routes/wrap-handler-memory-piggyback dummy-handler)]
        ;; Call without agent_id in args (e.g., memory query)
        (let [r1 (call-wrapped wrapped {})]
          (is (some? (extract-memory-block r1))
              "drains using coordinator default when no agent_id in args"))))))

;; =============================================================================
;; Mixed Scenario — Full Middleware Simulation
;; =============================================================================

(deftest mixed-tool-calls-stable-cursor-test
  (testing "Interleaved dispatch and non-dispatch calls share the same cursor"
    (let [shouts (atom [{:agent-id "ling-1" :event-type :started
                         :message "ling started" :timestamp 1000
                         :project-id test-project}])]
      (pb/register-message-source! (fn [] @shouts))

      (let [wrapped (routes/wrap-handler-piggyback dummy-handler)]
        ;; Call 1: memory query (no agent_id in args) — reads shout
        (let [r1 (call-wrapped wrapped {})]
          (is (some? (extract-hivemind-block r1))
              "memory query: reads shout"))

        ;; Call 2: agent dispatch to target A — cursor already advanced
        (let [r2 (call-wrapped wrapped {:agent_id "swarm-target-A"})]
          (is (nil? (extract-hivemind-block r2))
              "dispatch target-A: no re-delivery"))

        ;; Call 3: another memory query — still nothing
        (let [r3 (call-wrapped wrapped {})]
          (is (nil? (extract-hivemind-block r3))
              "second memory query: cursor still advanced"))

        ;; New shout arrives
        (swap! shouts conj {:agent-id "ling-2" :event-type :completed
                            :message "ling done" :timestamp 2000
                            :project-id test-project})

        ;; Call 4: dispatch to target B — picks up only the NEW shout
        (let [r4 (call-wrapped wrapped {:agent_id "swarm-target-B"})]
          (is (some? (extract-hivemind-block r4))
              "dispatch target-B: only NEW shout delivered"))))))

;; =============================================================================
;; Regression: Catchup Enqueue Key Must Match Middleware Drain Key
;; =============================================================================

(deftest catchup-enqueue-key-matches-middleware-drain-key-test
  (testing "REGRESSION: Buffer enqueued with plain 'coordinator' is NOT drained by middleware"
    ;; This test pins down the bug where catchup.clj used (or (:agent_id args) ...)
    ;; to compute the buffer key, which could diverge from the middleware's stable
    ;; 'coordinator-{project-id}' formula. When a caller passed agent_id: "coordinator",
    ;; the buffer was stored at ["coordinator" project-id] but middleware tried to
    ;; drain from ["coordinator-{project-id}" project-id] — a mismatch.
    (let [coordinator-id (str "coordinator-" test-project)]
      ;; Simulate the BUG: enqueue with plain "coordinator" (wrong key)
      (mp/enqueue! "coordinator" test-project
                   [{:id "ax-bug" :type "axiom" :content "Orphaned axiom" :tags ["axiom"]}])

      (let [wrapped (routes/wrap-handler-memory-piggyback dummy-handler)]
        ;; Middleware uses coordinator-{project}: can NOT drain the mismatched buffer
        (let [r1 (call-wrapped wrapped {})]
          (is (nil? (extract-memory-block r1))
              "BUG: buffer at [\"coordinator\" project] is invisible to middleware"))

        ;; Clean up the orphaned buffer
        (mp/clear-buffer! "coordinator" test-project))))

  (testing "FIX: Buffer enqueued with coordinator-{project-id} IS drained by middleware"
    (let [coordinator-id (str "coordinator-" test-project)]
      ;; Simulate the FIX: enqueue with coordinator-{project-id} (correct key)
      (mp/enqueue! coordinator-id test-project
                   [{:id "ax-fix" :type "axiom" :content "Delivered axiom" :tags ["axiom"]}])

      (let [wrapped (routes/wrap-handler-memory-piggyback dummy-handler)]
        ;; Middleware uses same key: successfully drains
        (let [r1 (call-wrapped wrapped {})]
          (is (some? (extract-memory-block r1))
              "FIX: buffer at [\"coordinator-project\" project] is drained by middleware"))))))
