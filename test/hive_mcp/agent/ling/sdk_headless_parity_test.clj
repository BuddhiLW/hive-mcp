(ns hive-mcp.agent.ling.sdk-headless-parity-test
  "Integration tests: SDK strategy vs ProcessBuilder (Headless) strategy parity.

   Verifies that AgentSDKStrategy and HeadlessStrategy satisfy the same
   ILingStrategy contract with equivalent behavioral semantics:

   1. Protocol satisfaction — both implement ILingStrategy
   2. Spawn contract    — both return ling-id string on success
   3. Dispatch contract — both return truthy on success, throw on invalid state
   4. Status contract   — both return maps with :slave/id and mode-specific fields
   5. Kill contract     — both return {:killed? true :id ling-id}
   6. Error parity      — equivalent error semantics for edge cases
   7. Facade parity     — Ling record delegates identically for both modes
   8. DataScript parity — both register/cleanup DataScript state consistently

   Since the SDK requires libpython-clj + claude-agent-sdk (typically unavailable
   in test environments), SDK internals are mocked to isolate strategy contract
   testing from Python bridge availability.

   Run with: (clojure.test/run-tests 'hive-mcp.agent.ling.sdk-headless-parity-test)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.strategy :as strategy :refer [ILingStrategy
                                                               strategy-spawn!
                                                               strategy-dispatch!
                                                               strategy-status
                                                               strategy-kill!]]
            [hive-mcp.agent.ling.headless-strategy :as headless-strat]
            [hive-mcp.agent.ling.agent-sdk-strategy :as sdk-strat]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.headless-sdk :as sdk]
            [hive-mcp.agent.context-envelope :as envelope]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.agent.ling.openrouter-strategy :as or-strat]
            [clojure.string :as str]
            [clojure.core.async :as async]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-all-state [f]
  "Reset DataScript, context-store, headless processes, and SDK sessions."
  (ds/reset-conn!)
  (context-store/reset-all!)
  (headless/kill-all-headless!)
  (sdk/kill-all-sdk!)
  (f)
  (sdk/kill-all-sdk!)
  (headless/kill-all-headless!)
  (context-store/reset-all!)
  (ds/reset-conn!))

(use-fixtures :each reset-all-state)

(defn gen-id
  "Generate unique test ling ID with prefix."
  [prefix]
  (str prefix "-" (System/nanoTime)))

;; =============================================================================
;; Section 1: Protocol Satisfaction Parity
;;
;; Both strategies MUST satisfy ILingStrategy. This is the foundational
;; contract — if either fails to satisfy, all other tests are meaningless.
;; =============================================================================

(deftest both-strategies-satisfy-protocol
  (testing "Both AgentSDKStrategy and HeadlessStrategy satisfy ILingStrategy"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)]
      (is (satisfies? ILingStrategy sdk-strat)
          "AgentSDKStrategy must satisfy ILingStrategy")
      (is (satisfies? ILingStrategy hl-strat)
          "HeadlessStrategy must satisfy ILingStrategy"))))

(deftest both-strategies-are-records
  (testing "Both strategies are proper Clojure records"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)]
      (is (record? sdk-strat) "SDK strategy should be a record")
      (is (record? hl-strat) "Headless strategy should be a record")
      (is (map? sdk-strat) "SDK strategy should be map-like")
      (is (map? hl-strat) "Headless strategy should be map-like"))))

;; =============================================================================
;; Section 2: Spawn Contract Parity
;;
;; Both strategies must:
;; - Accept the same ling-ctx shape {:id :cwd :presets :model}
;; - Accept the same opts shape {:task :buffer-capacity}
;; - Return the ling-id string (not a map, not nil)
;; - Be idempotent failure on duplicate IDs
;; =============================================================================

(deftest spawn-returns-ling-id-string
  (testing "Both strategies return ling-id string from strategy-spawn!"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)
          sdk-id    (gen-id "spawn-parity-sdk")
          hl-id     (gen-id "spawn-parity-hl")
          base-ctx  {:cwd "/tmp/test" :presets ["worker"] :model nil}
          base-opts {:task "Test task" :buffer-capacity 100}]

      ;; Mock SDK to simulate available state
      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [ling-id _opts]
                      {:ling-id ling-id :status :spawned :backend :agent-sdk :phase :idle})
                    sdk/dispatch-headless-sdk!
                    (fn [_id _task & _] (async/chan 1))
                    ;; Mock headless ProcessBuilder
                    headless/spawn-headless!
                    (fn [ling-id _opts]
                      {:ling-id ling-id :pid 12345 :process nil
                       :stdout-buf (atom {}) :stderr-buf (atom {})})
                    ;; Suppress L2 envelope enrichment
                    envelope/build-spawn-envelope (fn [_ _] nil)]

        (let [sdk-result (strategy-spawn! sdk-strat (assoc base-ctx :id sdk-id) base-opts)
              hl-result  (strategy-spawn! hl-strat (assoc base-ctx :id hl-id) base-opts)]

          ;; Both must return string ling-id
          (is (string? sdk-result)
              "SDK spawn must return a string")
          (is (string? hl-result)
              "Headless spawn must return a string")

          ;; Both must return the SAME ling-id that was passed in
          (is (= sdk-id sdk-result)
              "SDK spawn must return the input ling-id")
          (is (= hl-id hl-result)
              "Headless spawn must return the input ling-id"))))))

(deftest spawn-passes-cwd-to-backend
  (testing "Both strategies pass :cwd from ling-ctx to their respective backends"
    (let [sdk-strat     (sdk-strat/->agent-sdk-strategy)
          hl-strat      (headless-strat/->headless-strategy)
          sdk-captured  (atom nil)
          hl-captured   (atom nil)
          sdk-id        (gen-id "cwd-sdk")
          hl-id         (gen-id "cwd-hl")]

      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [_id opts]
                      (reset! sdk-captured opts)
                      {:ling-id _id :status :spawned :backend :agent-sdk :phase :idle})
                    headless/spawn-headless!
                    (fn [_id opts]
                      (reset! hl-captured opts)
                      {:ling-id _id :pid 99999})
                    envelope/build-spawn-envelope (fn [_ _] nil)]

        (strategy-spawn! sdk-strat {:id sdk-id :cwd "/projects/alpha" :presets [] :model nil} {})
        (strategy-spawn! hl-strat {:id hl-id :cwd "/projects/beta" :presets [] :model nil} {})

        ;; Both must pass cwd to their backend
        (is (= "/projects/alpha" (:cwd @sdk-captured))
            "SDK strategy must pass cwd to spawn-headless-sdk!")
        (is (= "/projects/beta" (:cwd @hl-captured))
            "Headless strategy must pass cwd to spawn-headless!")))))

(deftest spawn-with-nil-task-does-not-crash
  (testing "Both strategies handle nil task gracefully"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)
          sdk-id    (gen-id "nil-task-sdk")
          hl-id     (gen-id "nil-task-hl")]

      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [id _] {:ling-id id :status :spawned :backend :agent-sdk :phase :idle})
                    headless/spawn-headless!
                    (fn [id _] {:ling-id id :pid 11111})
                    envelope/build-spawn-envelope (fn [_ _] nil)]

        ;; Spawn without :task in opts — should not throw
        (is (= sdk-id (strategy-spawn! sdk-strat {:id sdk-id :cwd "/tmp" :presets [] :model nil} {}))
            "SDK spawn with no task should return ling-id")
        (is (= hl-id (strategy-spawn! hl-strat {:id hl-id :cwd "/tmp" :presets [] :model nil} {}))
            "Headless spawn with no task should return ling-id")))))

;; =============================================================================
;; Section 3: Dispatch Contract Parity
;;
;; Both strategies must:
;; - Accept {:task "string"} in task-opts
;; - Return truthy on success
;; - Throw ExceptionInfo on invalid state (no session / no process)
;; =============================================================================

(deftest dispatch-returns-truthy-on-success
  (testing "Both strategies return truthy from strategy-dispatch! on success"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)
          sdk-id    (gen-id "dispatch-sdk")
          hl-id     (gen-id "dispatch-hl")
          result-ch (async/chan 1)]

      ;; Mock SDK session exists
      (with-redefs [sdk/get-session
                    (fn [id] (when (= id sdk-id) {:ling-id id :phase :idle}))
                    sdk/dispatch-headless-sdk!
                    (fn [_id _task & _] result-ch)
                    ;; Mock headless stdin dispatch
                    headless/dispatch-via-stdin!
                    (fn [_id _msg] true)]

        (let [sdk-result (strategy-dispatch! sdk-strat {:id sdk-id} {:task "Do something"})
              hl-result  (strategy-dispatch! hl-strat {:id hl-id} {:task "Do something"})]

          ;; Both must return truthy
          (is (some? sdk-result)
              "SDK dispatch must return truthy (channel or true)")
          (is (true? hl-result)
              "Headless dispatch must return true"))))))

(deftest dispatch-to-nonexistent-throws
  (testing "Both strategies throw when dispatching to nonexistent agent"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)]

      ;; SDK: no session registered
      (with-redefs [sdk/get-session (fn [_] nil)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (strategy-dispatch! sdk-strat {:id "ghost-sdk"} {:task "boo"}))
            "SDK dispatch to nonexistent session should throw"))

      ;; Headless: no process registered
      (is (thrown? clojure.lang.ExceptionInfo
                   (strategy-dispatch! hl-strat {:id "ghost-hl"} {:task "boo"}))
          "Headless dispatch to nonexistent process should throw"))))

;; =============================================================================
;; Section 4: Status Contract Parity
;;
;; Both strategies must:
;; - Return a map with :slave/id when session/process exists
;; - Return nil when nothing exists and ds-status is nil
;; - Enrich ds-status when DataScript data is provided
;; - Include mode-specific liveness flags
;; =============================================================================

(deftest status-with-active-session-returns-map
  (testing "Both strategies return map with :slave/id when session/process is active"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)
          sdk-id    (gen-id "status-sdk")
          hl-id     (gen-id "status-hl")]

      ;; Mock SDK session
      (with-redefs [sdk/sdk-status-for
                    (fn [id]
                      (when (= id sdk-id)
                        {:ling-id id :phase :silence :session-id "sess-123"
                         :observations-count 5 :started-at 1000000
                         :uptime-ms 5000 :backend :agent-sdk}))
                    ;; Mock headless process
                    headless/headless-status
                    (fn [id]
                      (when (= id hl-id)
                        {:ling-id id :alive? true :pid 44444
                         :uptime-ms 3000
                         :stdout {:current-lines 10}
                         :stderr {:current-lines 0}}))]

        (let [sdk-status (strategy-status sdk-strat {:id sdk-id} nil)
              hl-status  (strategy-status hl-strat {:id hl-id} nil)]

          ;; Both must return maps
          (is (map? sdk-status)
              "SDK status should return a map")
          (is (map? hl-status)
              "Headless status should return a map")

          ;; Both must include :slave/id
          (is (= sdk-id (:slave/id sdk-status))
              "SDK status must include :slave/id")
          (is (= hl-id (:slave/id hl-status))
              "Headless status must include :slave/id")

          ;; Both must include mode identifier
          (is (= :agent-sdk (:ling/spawn-mode sdk-status))
              "SDK status must include :ling/spawn-mode :agent-sdk")
          (is (= :idle (:slave/status hl-status))
              "Headless status with no DS should default to :idle when alive"))))))

(deftest status-without-session-returns-nil
  (testing "Both strategies return nil when no session/process and no ds-status"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)]

      ;; Mock: no SDK session, no headless process
      (with-redefs [sdk/sdk-status-for (fn [_] nil)
                    headless/headless-status (fn [_] nil)]

        (let [sdk-status (strategy-status sdk-strat {:id "absent-sdk"} nil)
              hl-status  (strategy-status hl-strat {:id "absent-hl"} nil)]
          (is (nil? sdk-status)
              "SDK status with no session and no ds-status should be nil")
          (is (nil? hl-status)
              "Headless status with no process and no ds-status should be nil"))))))

(deftest status-enriches-ds-status
  (testing "Both strategies enrich existing ds-status with mode-specific fields"
    (let [sdk-strat  (sdk-strat/->agent-sdk-strategy)
          hl-strat   (headless-strat/->headless-strategy)
          sdk-id     (gen-id "enrich-sdk")
          hl-id      (gen-id "enrich-hl")
          base-ds    (fn [id] {:slave/id id :slave/status :working})]

      ;; Mock active sessions
      (with-redefs [sdk/sdk-status-for
                    (fn [id]
                      (when (= id sdk-id)
                        {:ling-id id :phase :act :session-id "sess-456"
                         :observations-count 3 :started-at 2000000
                         :uptime-ms 7000 :backend :agent-sdk}))
                    headless/headless-status
                    (fn [id]
                      (when (= id hl-id)
                        {:ling-id id :alive? true :pid 55555
                         :uptime-ms 8000
                         :stdout {:current-lines 20}
                         :stderr {:current-lines 2}}))]

        (let [sdk-result (strategy-status sdk-strat {:id sdk-id} (base-ds sdk-id))
              hl-result  (strategy-status hl-strat {:id hl-id} (base-ds hl-id))]

          ;; Both must preserve original ds-status fields
          (is (= sdk-id (:slave/id sdk-result))
              "SDK should preserve :slave/id from ds-status")
          (is (= hl-id (:slave/id hl-result))
              "Headless should preserve :slave/id from ds-status")

          ;; SDK-specific enrichment
          (is (true? (:sdk-alive? sdk-result))
              "SDK should add :sdk-alive?")
          (is (= :act (:sdk-phase sdk-result))
              "SDK should add :sdk-phase")

          ;; Headless-specific enrichment
          (is (true? (:headless-alive? hl-result))
              "Headless should add :headless-alive?")
          (is (= 55555 (:headless-pid hl-result))
              "Headless should add :headless-pid"))))))

(deftest status-with-dead-backend-reflects-liveness
  (testing "Both strategies correctly report liveness when backend is gone but ds-status exists"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)
          sdk-id    (gen-id "dead-sdk")
          hl-id     (gen-id "dead-hl")
          base-ds   (fn [id] {:slave/id id :slave/status :idle})]

      ;; Mock: both backends report nothing
      (with-redefs [sdk/sdk-status-for (fn [_] nil)
                    headless/headless-status (fn [_] nil)]

        (let [sdk-result (strategy-status sdk-strat {:id sdk-id} (base-ds sdk-id))
              hl-result  (strategy-status hl-strat {:id hl-id} (base-ds hl-id))]

          ;; SDK: should mark sdk-alive? false
          (is (false? (:sdk-alive? sdk-result))
              "SDK should report sdk-alive? false when no session")

          ;; Headless: returns ds-status as-is when no headless info
          ;; (no headless-alive? key at all, which is the correct behavior)
          (is (= hl-id (:slave/id hl-result))
              "Headless should preserve ds-status even when process info unavailable"))))))

;; =============================================================================
;; Section 5: Kill Contract Parity
;;
;; Both strategies must:
;; - Return {:killed? true :id ling-id} on success
;; - Handle already-dead/nonexistent gracefully (no crash)
;; - Include :reason for edge cases
;; =============================================================================

(deftest kill-returns-killed-true-on-success
  (testing "Both strategies return {:killed? true} on successful kill"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)
          sdk-id    (gen-id "kill-sdk")
          hl-id     (gen-id "kill-hl")]

      ;; Mock SDK session exists for kill
      (with-redefs [sdk/get-session (fn [id] (when (= id sdk-id) {:ling-id id}))
                    sdk/kill-headless-sdk!
                    (fn [id] {:killed? true :ling-id id})
                    ;; Mock headless process exists for kill
                    headless/kill-headless!
                    (fn [id] {:killed? true :ling-id id :pid 77777 :exit-code 0})]

        (let [sdk-result (strategy-kill! sdk-strat {:id sdk-id})
              hl-result  (strategy-kill! hl-strat {:id hl-id})]

          ;; Both must return {:killed? true}
          (is (true? (:killed? sdk-result))
              "SDK kill must return {:killed? true}")
          (is (true? (:killed? hl-result))
              "Headless kill must return {:killed? true}")

          ;; Both must include the id
          (is (= sdk-id (:id sdk-result))
              "SDK kill must include :id")
          (is (= hl-id (:id hl-result))
              "Headless kill must include :id"))))))

(deftest kill-nonexistent-is-graceful
  (testing "Both strategies handle kill of nonexistent agent gracefully"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)]

      ;; SDK: kill-headless-sdk! throws ExceptionInfo for nonexistent
      ;; AgentSDKStrategy catches it and returns {:killed? true :reason :session-not-found}
      (let [sdk-result (strategy-kill! sdk-strat {:id "nonexistent-sdk"})]
        (is (true? (:killed? sdk-result))
            "SDK kill of nonexistent should still return {:killed? true}")
        (is (= :session-not-found (:reason sdk-result))
            "SDK kill of nonexistent should include :reason"))

      ;; Headless: kill-headless! throws for nonexistent process
      ;; HeadlessStrategy catches it and returns {:killed? true :reason :process-already-dead}
      (let [hl-result (strategy-kill! hl-strat {:id "nonexistent-hl"})]
        (is (true? (:killed? hl-result))
            "Headless kill of nonexistent should still return {:killed? true}")
        (is (= :process-already-dead (:reason hl-result))
            "Headless kill of nonexistent should include :reason")))))

;; =============================================================================
;; Section 6: Facade Parity (Ling Record Delegation)
;;
;; Tests that the Ling facade treats both strategies identically:
;; - Same DataScript registration for both spawn modes
;; - Same status delegation pattern
;; - Same kill cleanup pattern
;; =============================================================================

(deftest facade-spawn-registers-datascript-identically
  (testing "Ling facade registers both modes identically in DataScript"
    (let [sdk-id (gen-id "facade-sdk")
          hl-id  (gen-id "facade-hl")
          sdk-ling (ling/->ling sdk-id {:cwd "/tmp/proj"
                                        :project-id "test"
                                        :spawn-mode :agent-sdk})
          hl-ling  (ling/->ling hl-id {:cwd "/tmp/proj"
                                       :project-id "test"
                                       :spawn-mode :headless})]

      ;; Since 0.12.0, :headless maps to :agent-sdk at the facade level.
      ;; Both lings go through AgentSDKStrategy.
      (with-redefs [;; SDK mocks — handles BOTH lings
                    sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [id _] {:ling-id id :status :spawned :backend :agent-sdk :phase :idle})
                    ;; Suppress context enrichment
                    envelope/build-spawn-envelope (fn [_ _] nil)]

        ;; Spawn both
        (let [sdk-slave-id (proto/spawn! sdk-ling {:depth 1})
              hl-slave-id  (proto/spawn! hl-ling {:depth 1})]

          ;; Both return ling-id
          (is (= sdk-id sdk-slave-id))
          (is (= hl-id hl-slave-id))

          ;; Both registered in DataScript
          (let [sdk-slave (ds-queries/get-slave sdk-id)
                hl-slave  (ds-queries/get-slave hl-id)]
            (is (some? sdk-slave) "SDK ling should be in DataScript")
            (is (some? hl-slave) "Headless ling should be in DataScript")

            ;; Common fields match
            (is (= 1 (:slave/depth sdk-slave)) "SDK depth should be 1")
            (is (= 1 (:slave/depth hl-slave)) "Headless depth should be 1")
            (is (= "claude" (:ling/model sdk-slave)) "SDK model should default to 'claude'")
            (is (= "claude" (:ling/model hl-slave)) "Headless model should default to 'claude'")

            ;; Mode-specific fields differ
            (is (= :agent-sdk (:ling/spawn-mode sdk-slave))
                "SDK should record :agent-sdk mode")
            ;; Since 0.12.0, :headless maps to :agent-sdk in the facade
            (is (= :agent-sdk (:ling/spawn-mode hl-slave))
                ":headless should map to :agent-sdk in DataScript (default headless mechanism since 0.12.0)")))))))

(deftest facade-kill-cleans-datascript-identically
  (testing "Ling facade removes DataScript entries for both modes on kill"
    (let [sdk-id (gen-id "kill-facade-sdk")
          hl-id  (gen-id "kill-facade-hl")]

      ;; Pre-register both in DataScript
      ;; Since 0.12.0, :headless maps to :agent-sdk at the facade level
      (ds-lings/add-slave! sdk-id {:status :idle :depth 1})
      (ds-lings/update-slave! sdk-id {:ling/spawn-mode :agent-sdk :ling/model "claude"})
      (ds-lings/add-slave! hl-id {:status :idle :depth 1})
      (ds-lings/update-slave! hl-id {:ling/spawn-mode :agent-sdk :ling/model "claude"})

      ;; Verify both exist
      (is (some? (ds-queries/get-slave sdk-id)) "SDK should be in DS before kill")
      (is (some? (ds-queries/get-slave hl-id)) "Headless (→agent-sdk) should be in DS before kill")

      ;; Both lings use AgentSDKStrategy kill path
      (with-redefs [;; SDK kill mock — handles both lings
                    sdk/get-session (fn [id] (when (#{sdk-id hl-id} id) {:ling-id id}))
                    sdk/kill-headless-sdk! (fn [id] {:killed? true :ling-id id})]

        (let [sdk-ling (ling/->ling sdk-id {:spawn-mode :agent-sdk})
              hl-ling  (ling/->ling hl-id {:spawn-mode :headless})]

          ;; Kill both
          (let [sdk-result (proto/kill! sdk-ling)
                hl-result  (proto/kill! hl-ling)]

            ;; Both report killed
            (is (true? (:killed? sdk-result)))
            (is (true? (:killed? hl-result)))

            ;; Both removed from DataScript
            (is (nil? (ds-queries/get-slave sdk-id))
                "SDK should be removed from DataScript after kill")
            (is (nil? (ds-queries/get-slave hl-id))
                "Headless should be removed from DataScript after kill")))))))

(deftest facade-dispatch-updates-status-identically
  (testing "Ling facade sets DataScript status to :working for both modes on dispatch"
    (let [sdk-id (gen-id "dispatch-facade-sdk")
          hl-id  (gen-id "dispatch-facade-hl")]

      ;; Pre-register both in DataScript
      (ds-lings/add-slave! sdk-id {:status :idle :depth 1 :cwd "/tmp"})
      (ds-lings/update-slave! sdk-id {:ling/spawn-mode :agent-sdk :ling/model "claude"})
      (ds-lings/add-slave! hl-id {:status :idle :depth 1 :cwd "/tmp"})
      ;; Since 0.12.0 :headless maps to :agent-sdk in the facade, so DS also shows :agent-sdk
      (ds-lings/update-slave! hl-id {:ling/spawn-mode :agent-sdk :ling/model "claude"})

      ;; Both lings go through AgentSDKStrategy at the facade level
      ;; (:headless maps to :agent-sdk since 0.12.0)
      (with-redefs [;; SDK dispatch mock — handles BOTH lings since both use agent-sdk strategy
                    sdk/get-session (fn [id] (when (#{sdk-id hl-id} id) {:ling-id id :phase :idle}))
                    sdk/dispatch-headless-sdk! (fn [_id _task & _] (async/chan 1))
                    ;; Suppress context enrichment
                    envelope/envelope-from-dispatch-context (fn [_ _] nil)]

        (let [sdk-ling (ling/->ling sdk-id {:cwd "/tmp" :spawn-mode :agent-sdk})
              hl-ling  (ling/->ling hl-id {:cwd "/tmp" :spawn-mode :headless})]

          ;; Dispatch to both
          (proto/dispatch! sdk-ling {:task "SDK task"})
          (proto/dispatch! hl-ling {:task "Headless task"})

          ;; Both should be :working now
          (let [sdk-slave (ds-queries/get-slave sdk-id)
                hl-slave  (ds-queries/get-slave hl-id)]
            (is (= :working (:slave/status sdk-slave))
                "SDK should be :working after dispatch")
            (is (= :working (:slave/status hl-slave))
                "Headless (→agent-sdk) should be :working after dispatch")))))))

;; =============================================================================
;; Section 7: Full Lifecycle Parity (spawn → dispatch → status → kill)
;;
;; End-to-end lifecycle through both strategies using mocks.
;; Verifies the complete happy path produces equivalent state transitions.
;; =============================================================================

(deftest full-lifecycle-parity
  (testing "Full lifecycle (spawn → dispatch → status → kill) is equivalent for both strategies"
    (let [sdk-id (gen-id "lifecycle-sdk")
          hl-id  (gen-id "lifecycle-hl")
          ;; Since 0.12.0, :headless maps to :agent-sdk at the facade level.
          ;; Both lings route through AgentSDKStrategy, so we track all dispatches
          ;; in a single atom.
          all-dispatched (atom [])]

      (with-redefs [;; SDK mocks — handles BOTH lings (both use agent-sdk strategy via facade)
                    sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [id _] {:ling-id id :status :spawned :backend :agent-sdk :phase :idle})
                    sdk/dispatch-headless-sdk!
                    (fn [id task & _]
                      (swap! all-dispatched conj {:id id :task task})
                      (async/chan 1))
                    sdk/get-session
                    (fn [id] (when (#{sdk-id hl-id} id) {:ling-id id :phase :act}))
                    sdk/sdk-status-for
                    (fn [id]
                      (when (#{sdk-id hl-id} id)
                        {:ling-id id :phase :act :session-id (str "sess-" id)
                         :observations-count 2 :started-at 1000 :uptime-ms 5000
                         :backend :agent-sdk}))
                    sdk/kill-headless-sdk!
                    (fn [id] {:killed? true :ling-id id})
                    ;; Suppress context enrichment
                    envelope/build-spawn-envelope (fn [_ _] nil)
                    envelope/envelope-from-dispatch-context (fn [_ _] nil)]

        (let [sdk-ling (ling/->ling sdk-id {:cwd "/tmp" :project-id "test" :spawn-mode :agent-sdk})
              hl-ling  (ling/->ling hl-id {:cwd "/tmp" :project-id "test" :spawn-mode :headless})]

          ;; === PHASE 1: SPAWN ===
          (let [sdk-slave-id (proto/spawn! sdk-ling {:depth 1})
                hl-slave-id  (proto/spawn! hl-ling {:depth 1})]
            (is (= sdk-id sdk-slave-id) "SDK spawn should return ling-id")
            (is (= hl-id hl-slave-id) "Headless (via agent-sdk) spawn should return ling-id")

            ;; Both in DataScript
            (is (some? (ds-queries/get-slave sdk-id)))
            (is (some? (ds-queries/get-slave hl-id))))

          ;; === PHASE 2: DISPATCH ===
          (let [sdk-task-id (proto/dispatch! sdk-ling {:task "Analyze codebase"})
                hl-task-id  (proto/dispatch! hl-ling {:task "Analyze codebase"})]
            ;; Both return task-id strings
            (is (string? sdk-task-id))
            (is (string? hl-task-id))
            (is (str/starts-with? sdk-task-id "task-"))
            (is (str/starts-with? hl-task-id "task-"))
            ;; Both dispatched exactly once (each via SDK strategy)
            (is (= 2 (count @all-dispatched))
                "Both lings should have dispatched via SDK strategy")
            (is (= 1 (count (filter #(= sdk-id (:id %)) @all-dispatched)))
                "SDK ling should have dispatched once")
            (is (= 1 (count (filter #(= hl-id (:id %)) @all-dispatched)))
                "Headless (via agent-sdk) ling should have dispatched once"))

          ;; Both should be :working in DataScript
          (is (= :working (:slave/status (ds-queries/get-slave sdk-id))))
          (is (= :working (:slave/status (ds-queries/get-slave hl-id))))

          ;; === PHASE 3: STATUS ===
          (let [sdk-status (proto/status sdk-ling)
                hl-status  (proto/status hl-ling)]
            ;; Both return maps
            (is (map? sdk-status))
            (is (map? hl-status))
            ;; Both have :slave/id
            (is (= sdk-id (:slave/id sdk-status)))
            (is (= hl-id (:slave/id hl-status))))

          ;; === PHASE 4: KILL ===
          (let [sdk-kill (proto/kill! sdk-ling)
                hl-kill  (proto/kill! hl-ling)]
            ;; Both report killed
            (is (true? (:killed? sdk-kill)))
            (is (true? (:killed? hl-kill)))
            ;; Both cleaned from DataScript
            (is (nil? (ds-queries/get-slave sdk-id)))
            (is (nil? (ds-queries/get-slave hl-id)))))))))

;; =============================================================================
;; Section 8: Spawn-Mode Resolution Parity
;;
;; Verifies that ->ling factory correctly resolves strategy for both modes.
;; =============================================================================

(deftest ling-factory-mode-resolution
  (testing "->ling factory resolves correct spawn-mode for both strategies"
    (let [sdk-ling (ling/->ling "factory-sdk" {:cwd "/tmp" :spawn-mode :agent-sdk})
          hl-ling  (ling/->ling "factory-hl" {:cwd "/tmp" :spawn-mode :headless})]
      (is (= :agent-sdk (:spawn-mode sdk-ling))
          "SDK spawn-mode should be :agent-sdk")
      ;; Since 0.12.0, :headless maps to :agent-sdk in ->ling factory
      ;; (agent-sdk is the default headless mechanism)
      (is (= :agent-sdk (:spawn-mode hl-ling))
          ":headless should map to :agent-sdk (default headless mechanism since 0.12.0)"))))

(deftest ling-factory-default-is-not-sdk-or-headless
  (testing "->ling default spawn-mode is :vterm, not :agent-sdk or :headless"
    (let [default-ling (ling/->ling "factory-default" {:cwd "/tmp"})]
      (is (= :vterm (:spawn-mode default-ling))
          "Default should be :vterm")
      (is (not= :headless (:spawn-mode default-ling)))
      (is (not= :agent-sdk (:spawn-mode default-ling))))))

;; =============================================================================
;; Section 9: SDK Graceful Degradation (SDK-specific, but parity-relevant)
;;
;; When SDK is unavailable, the system should fall back gracefully.
;; This ensures headless (ProcessBuilder) is always available as fallback.
;; =============================================================================

(deftest sdk-unavailable-throws-with-hint
  (testing "SDK strategy throws with actionable hint when unavailable"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)]
      ;; SDK typically unavailable in test env (no libpython-clj)
      (when-not (sdk/available?)
        (try
          (strategy-spawn! sdk-strat
                           {:id "unavailable-test" :cwd "/tmp" :presets []}
                           {})
          (is false "Should have thrown")
          (catch clojure.lang.ExceptionInfo e
            (let [data (ex-data e)]
              (is (string? (:hint data))
                  "Error should include actionable :hint")
              (is (keyword? (:sdk-status data))
                  "Error should include :sdk-status")
              (is (= :agent-sdk (:spawn-mode data))
                  "Error should identify the failing spawn-mode"))))))))

(deftest headless-always-available
  (testing "HeadlessStrategy does not require special prerequisites (always available)"
    ;; HeadlessStrategy just wraps ProcessBuilder — no special deps needed
    (let [hl-strat (headless-strat/->headless-strategy)]
      ;; Spawn with echo (safe command) should always work
      (let [ling-id (gen-id "always-avail")
            result (headless/spawn-headless! ling-id
                                             {:cwd "/tmp"
                                              :claude-cmd "echo"
                                              :task "availability test"
                                              :buffer-capacity 10})]
        (is (pos? (:pid result))
            "Headless should always be able to spawn a subprocess")
        ;; Cleanup
        (let [^java.lang.Process proc (:process result)]
          (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
        (try (headless/kill-headless! ling-id {:force? true})
             (catch Exception _ nil))))))

;; =============================================================================
;; Section 10: Contract Summary — Shape Assertions
;;
;; Validates that the return shapes from both strategies are compatible
;; with what the Ling facade and callers expect.
;; =============================================================================

(deftest spawn-result-shape-parity
  (testing "Both strategies' spawn result is a plain string (ling-id)"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)
          sdk-id    (gen-id "shape-sdk")
          hl-id     (gen-id "shape-hl")]

      (with-redefs [sdk/available? (constantly true)
                    sdk/sdk-status (constantly :available)
                    sdk/spawn-headless-sdk!
                    (fn [id _] {:ling-id id :status :spawned :backend :agent-sdk :phase :idle})
                    headless/spawn-headless!
                    (fn [id _] {:ling-id id :pid 1})
                    envelope/build-spawn-envelope (fn [_ _] nil)]

        (let [sdk-r (strategy-spawn! sdk-strat {:id sdk-id :cwd "/tmp" :presets [] :model nil} {})
              hl-r  (strategy-spawn! hl-strat {:id hl-id :cwd "/tmp" :presets [] :model nil} {})]
          ;; Both must be strings, not maps or channels
          (is (string? sdk-r) "SDK spawn result must be string")
          (is (string? hl-r) "Headless spawn result must be string")
          ;; Neither should be a map (common mistake: returning backend result directly)
          (is (not (map? sdk-r)) "SDK spawn result must NOT be a map")
          (is (not (map? hl-r)) "Headless spawn result must NOT be a map"))))))

(deftest kill-result-shape-parity
  (testing "Both strategies' kill result contains {:killed? bool :id string}"
    (let [sdk-strat (sdk-strat/->agent-sdk-strategy)
          hl-strat  (headless-strat/->headless-strategy)]

      (with-redefs [sdk/kill-headless-sdk! (fn [id] {:killed? true :ling-id id})
                    sdk/get-session (fn [id] {:ling-id id})
                    headless/kill-headless! (fn [id] {:killed? true :ling-id id :pid 0 :exit-code 0})]

        (let [sdk-r (strategy-kill! sdk-strat {:id "shape-kill-sdk"})
              hl-r  (strategy-kill! hl-strat {:id "shape-kill-hl"})]
          ;; Both must have :killed? key
          (is (contains? sdk-r :killed?) "SDK kill result must contain :killed?")
          (is (contains? hl-r :killed?) "Headless kill result must contain :killed?")
          ;; Both must have :id key
          (is (contains? sdk-r :id) "SDK kill result must contain :id")
          (is (contains? hl-r :id) "Headless kill result must contain :id")
          ;; :killed? must be boolean
          (is (boolean? (:killed? sdk-r)) "SDK :killed? must be boolean")
          (is (boolean? (:killed? hl-r)) "Headless :killed? must be boolean"))))))

(comment
  ;; Run all parity tests
  (clojure.test/run-tests 'hive-mcp.agent.ling.sdk-headless-parity-test)

  ;; Run a specific section
  (clojure.test/test-vars [#'both-strategies-satisfy-protocol
                           #'spawn-returns-ling-id-string
                           #'full-lifecycle-parity]))
