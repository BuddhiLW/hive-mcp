(ns hive-mcp.agent.ling.headless-lifecycle-e2e-test
  "E2E integration test: spawn headless ling, dispatch task, collect stdout, verify output.

   Tests the full headless lifecycle with REAL subprocesses (no mocks):
   1. Low-level: headless module directly (spawn → dispatch → collect → verify)
   2. Facade-level: Ling IAgent protocol (spawn! → dispatch! → get-stdout → verify)
   3. Multi-dispatch: multiple messages through same ling, all captured
   4. Concurrent: parallel dispatches to same ling, all land in ring buffer

   Uses `cat` as the subprocess — reads stdin, echoes to stdout.
   Safe, deterministic, no external dependencies.

   Run with: (clojure.test/run-tests 'hive-mcp.agent.ling.headless-lifecycle-e2e-test)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.context-envelope :as envelope]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.agent.ling.openrouter-strategy :as or-strat]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn reset-all-state [f]
  "Reset DataScript, context-store, and kill headless processes between tests."
  (ds/reset-conn!)
  (context-store/reset-all!)
  (headless/kill-all-headless!)
  (f)
  (headless/kill-all-headless!)
  (context-store/reset-all!)
  (ds/reset-conn!))

(use-fixtures :each reset-all-state)

(defn gen-id
  "Generate unique test ling ID."
  [prefix]
  (str prefix "-" (System/nanoTime)))

(defn wait-for-stdout
  "Poll stdout until predicate matches or timeout.
   Returns stdout vector on success, nil on timeout.

   Arguments:
     ling-id   - Headless ling ID
     pred      - Predicate fn: (fn [stdout-vec] -> bool)
     timeout   - Max wait in ms (default 5000)"
  ([ling-id pred] (wait-for-stdout ling-id pred 5000))
  ([ling-id pred timeout-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (let [stdout (headless/get-stdout ling-id)]
         (cond
           (and stdout (pred stdout))
           stdout

           (> (System/currentTimeMillis) deadline)
           nil

           :else
           (do (Thread/sleep 50) (recur))))))))

;; =============================================================================
;; Section 1: Low-Level E2E — headless module directly
;;
;; spawn-headless! with `cat` → dispatch-via-stdin! → get-stdout → verify
;; No Ling facade, no DataScript, just raw process management.
;; =============================================================================

(deftest headless-spawn-dispatch-collect-verify
  (testing "Full lifecycle: spawn cat → dispatch message → collect stdout → verify output"
    (let [ling-id (gen-id "e2e-cat")
          ;; 1. SPAWN: `cat` reads stdin and writes to stdout
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "cat"
                                            :buffer-capacity 500})]
      (is (map? result) "spawn should return a result map")
      (is (pos? (:pid result)) "should have a positive PID")
      (is (headless/headless? ling-id) "should be registered in process registry")

      ;; 2. DISPATCH: send a task via stdin
      (let [sent? (headless/dispatch-via-stdin! ling-id "hello from E2E test")]
        (is (true? sent?) "dispatch should succeed"))

      ;; 3. COLLECT: wait for stdout to contain our message
      (let [stdout (wait-for-stdout ling-id
                                    #(some (fn [line] (str/includes? line "hello from E2E test")) %))]
        ;; 4. VERIFY: output matches what we dispatched
        (is (some? stdout) "stdout should have been captured before timeout")
        (is (some #(str/includes? % "hello from E2E test") stdout)
            (str "stdout should contain dispatched message, got: " (pr-str stdout))))

      ;; Cleanup
      (let [kill-result (headless/kill-headless! ling-id {:force? true})]
        (is (true? (:killed? kill-result)))
        (is (not (headless/headless? ling-id))
            "should be removed from registry after kill")))))

(deftest headless-multi-dispatch-collects-all
  (testing "Multiple dispatches accumulate in ring buffer"
    (let [ling-id (gen-id "e2e-multi")
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "cat"
                                       :buffer-capacity 500})
          messages ["first message alpha"
                    "second message bravo"
                    "third message charlie"]]

      ;; Dispatch all messages
      (doseq [msg messages]
        (headless/dispatch-via-stdin! ling-id msg)
        ;; Small delay to ensure ordering in ring buffer
        (Thread/sleep 50))

      ;; Wait for the LAST message to appear (implies all earlier ones are captured)
      (let [stdout (wait-for-stdout ling-id
                                    #(some (fn [line] (str/includes? line "charlie")) %))]
        (is (some? stdout) "should capture all messages before timeout")

        ;; Verify ALL messages are present in stdout
        (doseq [msg messages]
          (is (some #(str/includes? % msg) stdout)
              (str "stdout should contain '" msg "'")))

        ;; Verify ordering: first appears before third
        (let [idx-first (some (fn [[i line]] (when (str/includes? line "alpha") i))
                              (map-indexed vector stdout))
              idx-third (some (fn [[i line]] (when (str/includes? line "charlie") i))
                              (map-indexed vector stdout))]
          (is (and idx-first idx-third (< idx-first idx-third))
              "messages should appear in dispatch order")))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest headless-collect-with-since-timestamp
  (testing "Time-based stdout collection via get-stdout-since"
    (let [ling-id (gen-id "e2e-since")
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "cat"
                                       :buffer-capacity 500})]

      ;; Dispatch first batch
      (headless/dispatch-via-stdin! ling-id "before-marker")
      (Thread/sleep 200)

      ;; Record the timestamp AFTER first batch
      (let [marker-ts (System/currentTimeMillis)]
        (Thread/sleep 100)

        ;; Dispatch second batch
        (headless/dispatch-via-stdin! ling-id "after-marker")

        ;; Wait for second batch to appear
        (wait-for-stdout ling-id
                         #(some (fn [line] (str/includes? line "after-marker")) %))

        ;; Collect only entries SINCE the marker
        (let [since-entries (headless/get-stdout-since ling-id marker-ts)]
          (is (vector? since-entries) "should return vector of entries")
          (is (every? #(and (:text %) (:ts %)) since-entries)
              "entries should have :text and :ts")
          ;; "after-marker" should be in since-entries
          (is (some #(str/includes? (:text %) "after-marker") since-entries)
              "should contain entries after marker timestamp")
          ;; "before-marker" should NOT be in since-entries
          (is (not (some #(str/includes? (:text %) "before-marker") since-entries))
              "should NOT contain entries before marker timestamp")))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(deftest headless-collect-last-n
  (testing "Collect last N lines from ring buffer"
    (let [ling-id (gen-id "e2e-lastn")
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "cat"
                                       :buffer-capacity 500})]

      ;; Dispatch 10 numbered messages
      (doseq [i (range 10)]
        (headless/dispatch-via-stdin! ling-id (str "line-" i))
        (Thread/sleep 30))

      ;; Wait for last line
      (wait-for-stdout ling-id
                       #(some (fn [line] (str/includes? line "line-9")) %))

      ;; Collect last 3 lines
      (let [last-3 (headless/get-stdout ling-id {:last-n 3})]
        (is (= 3 (count last-3)) "should return exactly 3 lines")
        (is (some #(str/includes? % "line-9") last-3)
            "last-3 should include the most recent line")
        (is (not (some #(str/includes? % "line-0") last-3))
            "last-3 should NOT include the earliest line"))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

;; =============================================================================
;; Section 2: Facade E2E — Ling IAgent protocol with real subprocess
;;
;; Uses ->ling + proto/spawn! + proto/dispatch! + headless/get-stdout.
;; Real `cat` subprocess, with DataScript registration.
;; L2 context envelope is mocked to nil to isolate headless lifecycle.
;; =============================================================================

(deftest facade-spawn-dispatch-collect-verify
  (testing "Full Ling facade lifecycle: spawn! → dispatch! → get-stdout → verify"
    (let [ling-id (gen-id "facade-e2e")
          ling-inst (ling/->ling ling-id {:cwd "/tmp"
                                          :project-id "test-e2e"
                                          :spawn-mode :headless})]

      ;; Mock build-spawn-envelope so L2 enrichment doesn't interfere
      ;; and use :claude-cmd "cat" via headless spawn override
      (with-redefs [envelope/build-spawn-envelope (fn [_ _] nil)
                    envelope/envelope-from-dispatch-context (fn [_ _] nil)]

        ;; 1. SPAWN via IAgent protocol
        ;; We need to override the claude command for the test subprocess.
        ;; The headless module uses "claude" by default, so we inject :claude-cmd
        ;; through spawn opts. But Ling.spawn! doesn't pass :claude-cmd to
        ;; strategy — so we mock spawn-headless! to inject it.
        (let [original-spawn headless/spawn-headless!]
          (with-redefs [headless/spawn-headless!
                        (fn [id opts]
                          ;; Inject :claude-cmd "cat" for test, keep everything else
                          (original-spawn id (assoc opts :claude-cmd "cat")))]
            (let [slave-id (proto/spawn! ling-inst {:depth 1})]
              (is (= ling-id slave-id) "spawn! should return the ling-id")

              ;; Verify DataScript registration
              (let [slave (ds-queries/get-slave ling-id)]
                (is (some? slave) "should be registered in DataScript")
                (is (= :headless (:ling/spawn-mode slave))
                    "DataScript should record :headless mode")))))

        ;; 2. DISPATCH via IAgent protocol
        (let [ling-for-dispatch (ling/->ling ling-id {:cwd "/tmp"
                                                      :project-id "test-e2e"
                                                      :spawn-mode :headless})
              task-id (proto/dispatch! ling-for-dispatch {:task "facade test message"})]
          (is (string? task-id) "dispatch! should return a task-id string")
          (is (str/starts-with? task-id "task-") "task-id should start with 'task-'"))

        ;; 3. COLLECT stdout (directly from headless module)
        (let [stdout (wait-for-stdout ling-id
                                      #(some (fn [line] (str/includes? line "facade test message")) %))]
          ;; 4. VERIFY
          (is (some? stdout)
              "stdout should contain dispatched message before timeout")
          (is (some #(str/includes? % "facade test message") stdout)
              (str "stdout should contain 'facade test message', got: " (pr-str stdout))))

        ;; 5. STATUS check via IAgent protocol
        (let [ling-for-status (ling/->ling ling-id {:cwd "/tmp"
                                                    :project-id "test-e2e"
                                                    :spawn-mode :headless})
              status (proto/status ling-for-status)]
          (is (map? status) "status should return a map")
          (is (true? (:headless-alive? status))
              "headless process should still be alive"))

        ;; 6. KILL via IAgent protocol
        (let [ling-for-kill (ling/->ling ling-id {:cwd "/tmp"
                                                  :project-id "test-e2e"
                                                  :spawn-mode :headless})
              result (proto/kill! ling-for-kill)]
          (is (true? (:killed? result)) "kill should succeed")
          (is (nil? (ds-queries/get-slave ling-id))
              "should be removed from DataScript after kill"))))))

;; =============================================================================
;; Section 3: Spawn with Initial Task E2E
;;
;; When spawning with :task, the task becomes a CLI arg to the subprocess.
;; For `echo`, this means the task string appears in stdout immediately.
;; =============================================================================

(deftest spawn-with-initial-task-captures-output
  (testing "Spawn with :task bakes the task into CLI args; stdout captures it"
    (let [ling-id (gen-id "task-spawn")
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "echo"
                                            :task "initial task payload"
                                            :buffer-capacity 100})]
      ;; echo exits immediately after printing
      (let [^java.lang.Process proc (:process result)]
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
      (Thread/sleep 200)

      ;; Collect stdout — echo should have printed the task
      (let [stdout (headless/get-stdout ling-id)]
        (is (some? stdout) "stdout should be available")
        (is (some #(str/includes? % "initial task payload") stdout)
            (str "stdout should contain the initial task, got: " (pr-str stdout))))

      ;; Ring buffer stats should show lines captured
      (let [stats (headless/ring-buffer-stats (headless/get-stdout-buffer ling-id))]
        (is (pos? (:total-lines-seen stats))
            "ring buffer should have seen at least one line"))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

;; =============================================================================
;; Section 4: Concurrent Dispatch E2E
;;
;; Multiple threads dispatch to the same `cat` ling simultaneously.
;; All messages should land in the ring buffer (order may vary).
;; =============================================================================

(deftest concurrent-dispatch-all-captured
  (testing "Concurrent dispatches from multiple threads all land in ring buffer"
    (let [ling-id (gen-id "e2e-concurrent")
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "cat"
                                       :buffer-capacity 500})
          n-threads 5
          messages (mapv #(str "concurrent-msg-" %) (range n-threads))
          ;; Launch dispatches from separate threads
          futures (mapv (fn [msg]
                          (future
                            (Thread/sleep (rand-int 50)) ; jitter
                            (headless/dispatch-via-stdin! ling-id msg)))
                        messages)]

      ;; Wait for all futures to complete
      (doseq [f futures]
        (deref f 5000 :timeout))

      ;; Wait for all messages to appear in stdout
      (let [stdout (wait-for-stdout ling-id
                                    (fn [lines]
                                      (every? (fn [msg]
                                                (some #(str/includes? % msg) lines))
                                              messages))
                                    10000)] ; longer timeout for concurrent
        (is (some? stdout) "all concurrent messages should appear before timeout")
        ;; Verify each message individually
        (doseq [msg messages]
          (is (some #(str/includes? % msg) stdout)
              (str "stdout should contain '" msg "'"))))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

;; =============================================================================
;; Section 5: Ring Buffer Capacity Enforcement E2E
;;
;; Dispatch more lines than buffer capacity; verify oldest are dropped.
;; =============================================================================

(deftest ring-buffer-drops-oldest-on-overflow
  (testing "Ring buffer enforces capacity by dropping oldest lines"
    (let [ling-id (gen-id "e2e-overflow")
          capacity 10
          _ (headless/spawn-headless! ling-id
                                      {:cwd "/tmp"
                                       :claude-cmd "cat"
                                       :buffer-capacity capacity})
          n-messages 20]

      ;; Dispatch more messages than capacity
      (doseq [i (range n-messages)]
        (headless/dispatch-via-stdin! ling-id (str "overflow-" i))
        (Thread/sleep 30))

      ;; Wait for the last message
      (wait-for-stdout ling-id
                       #(some (fn [line] (str/includes? line (str "overflow-" (dec n-messages)))) %)
                       10000)

      ;; Verify: buffer should have at most `capacity` lines
      (let [stdout (headless/get-stdout ling-id)
            stats (headless/ring-buffer-stats (headless/get-stdout-buffer ling-id))]
        (is (<= (count stdout) capacity)
            (str "stdout should have at most " capacity " lines, got " (count stdout)))
        (is (= n-messages (:total-lines-seen stats))
            "total-lines-seen should count all dispatched messages")
        (is (pos? (:dropped stats))
            "some lines should have been dropped")
        ;; The newest message should be present
        (is (some #(str/includes? % (str "overflow-" (dec n-messages))) stdout)
            "newest message should be in buffer")
        ;; The oldest messages should have been evicted
        (is (not (some #(str/includes? % "overflow-0") stdout))
            "oldest message should have been evicted"))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

;; =============================================================================
;; Section 6: Process Death Detection
;;
;; Spawn, let process die, verify status reflects death, dispatch fails.
;; =============================================================================

(deftest dead-process-detected-correctly
  (testing "Status reflects process death; dispatch to dead process fails"
    (let [ling-id (gen-id "e2e-dead")
          ;; echo exits immediately
          result (headless/spawn-headless! ling-id
                                           {:cwd "/tmp"
                                            :claude-cmd "echo"
                                            :task "I will die now"
                                            :buffer-capacity 100})]
      ;; Wait for echo to exit
      (let [^java.lang.Process proc (:process result)]
        (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
      (Thread/sleep 200)

      ;; Status should reflect dead process
      (let [status (headless/headless-status ling-id)]
        (is (some? status) "status should be available")
        (is (false? (:alive? status)) "process should not be alive")
        (is (some? (:exit-code status)) "exit code should be available"))

      ;; Dispatch to dead process should throw
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"not alive"
                            (headless/dispatch-via-stdin! ling-id "you are dead"))
          "dispatching to dead process should throw")

      ;; But stdout should still be readable (ring buffer persists)
      (let [stdout (headless/get-stdout ling-id)]
        (is (some #(str/includes? % "I will die now") stdout)
            "stdout should still be readable after process death"))

      ;; Cleanup
      (try (headless/kill-headless! ling-id {:force? true})
           (catch Exception _ nil)))))

(comment
  ;; Run all E2E lifecycle tests
  (clojure.test/run-tests 'hive-mcp.agent.ling.headless-lifecycle-e2e-test))
