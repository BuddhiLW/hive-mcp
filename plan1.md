 Ready to code?

 Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Implementation Plan: dispatch_drone_wave

 Overview

 Batch drone execution tool for 4-5x token reduction. Single MCP call to fan-out multiple drones in parallel.

 Current State

 - delegate_drone exists at src/hive_mcp/agent/drone.clj:125-193
 - Swarm coordinator manages file conflicts at src/hive_mcp/swarm/coordinator.clj
 - DataScript schema at src/hive_mcp/graph/schema.clj
 - core.async patterns exist in tools/swarm/channel.clj and registry.clj

 Implementation Steps

 Phase 1: DataScript Schema (S)

 File: src/hive_mcp/graph/schema.clj

 Add entities:
 ;; Change Plan
 :change-plan/id       :db.unique/identity
 :change-plan/status   ;; :pending :executing :complete :partial-fail
 :change-plan/preset
 :change-plan/created-at

 ;; Change Item
 :change-item/id       :db.unique/identity
 :change-item/plan     :db.type/ref
 :change-item/file     :db.index true
 :change-item/task
 :change-item/drone-id
 :change-item/status   ;; :pending :dispatched :completed :error

 ;; Wave
 :wave/id              :db.unique/identity
 :wave/plan            :db.type/ref
 :wave/drone-count
 :wave/completed-count
 :wave/status

 Phase 2: Wave Module (M)

 File: src/hive_mcp/tools/swarm/wave.clj (NEW)

 Core functions:
 (defn create-plan! [tasks preset]
   "Create change-plan + change-items in DataScript")

 (defn execute-wave! [plan-id]
   "Fan-out drones via core.async, track completion")

 (defn dispatch-drone-wave [{:keys [plan-id tasks preset trace]}]
   "MCP handler - create plan if inline, execute wave")

 Phase 3: core.async Fan-Out (M)

 Pattern from exploration:
 (defn fan-out-drones [items preset]
   (let [result-ch (async/chan (count items))
         sem (async/chan 3)] ;; Max 3 concurrent
     (doseq [item items]
       (async/go
         (async/<! sem)
         (let [result (drone/delegate! (:task item) ...)]
           (async/>! result-ch {:item item :result result})
           (async/>! sem :done))))
     (async/into [] result-ch)))

 Phase 4: Event Handlers (S)

 File: src/hive_mcp/events/handlers.clj

 (ev/reg-event :wave/start [] wave-started-handler)
 (ev/reg-event :wave/item-done [] item-done-handler)
 (ev/reg-event :wave/complete [] wave-complete-handler)

 Phase 5: MCP Tool Registration (S)

 File: src/hive_mcp/tools/swarm.clj

 Add to tool definitions:
 {:name "dispatch_drone_wave"
  :description "Dispatch multiple drones in parallel from change plan"
  :inputSchema {...}}

 Phase 6: Tests (M)

 File: test/hive_mcp/tools/wave_test.clj (NEW)

 - Schema validation tests
 - Plan CRUD tests
 - Mock drone execution tests
 - Wave completion aggregation tests

 Files to Modify
 ┌───────────────────────────────────┬──────────────┬──────┐
 │               File                │    Action    │ Size │
 ├───────────────────────────────────┼──────────────┼──────┤
 │ src/hive_mcp/graph/schema.clj     │ Add schema   │ S    │
 ├───────────────────────────────────┼──────────────┼──────┤
 │ src/hive_mcp/tools/swarm/wave.clj │ NEW          │ M    │
 ├───────────────────────────────────┼──────────────┼──────┤
 │ src/hive_mcp/tools/swarm.clj      │ Add tool     │ S    │
 ├───────────────────────────────────┼──────────────┼──────┤
 │ src/hive_mcp/events/handlers.clj  │ Add handlers │ S    │
 ├───────────────────────────────────┼──────────────┼──────┤
 │ test/hive_mcp/tools/wave_test.clj │ NEW          │ M    │
 └───────────────────────────────────┴──────────────┴──────┘
 Verification

 1. Unit tests:
 clojure -M:dev:test -n hive-mcp.tools.wave-test
 2. REPL integration test:
 (require '[hive-mcp.tools.swarm.wave :as wave])

 ;; Create inline plan
 (wave/dispatch-drone-wave
   {:tasks [{:file "test.clj" :task "Add comment"}]
    :preset "drone-worker"
    :trace true})

 ;; Check wave status
 (wave/get-wave-status wave-id)
 3. E2E with actual drones:
   - Spawn coordinator session
   - Call dispatch_drone_wave with 2-3 small tasks
   - Verify HIVEMIND shows wave completion
   - Check all proposed diffs applied

 Complexity Estimate

 - Total: ~500-650 LOC
 - Time: 2-3 pomodoros with ling delegation

 Swarm Strategy

 Spawn 3 lings in parallel:
 1. schema-ling: DataScript schema + validation
 2. wave-ling: Wave module core (create, execute, fan-out)
 3. integration-ling: Events + MCP tool registration + tests

 Future Enhancement: Planner Ling Pipeline

 Insight: A dedicated planner LING runs in parallel with impl lings.

 Wave N executing:  [impl-ling-1] [impl-ling-2] [impl-ling-3]
                           ↓
 Wave N+1 planning:        [planner-ling] ← fed by scout drones
                                 ↑
                           [scout-drone-1] [scout-drone-2]
                           (explore codebase, report findings)

 Tier Strategy:
 - Planner ling (Claude): Strategic planning, plan-mode capable
 - Scout drones (OpenRouter free): Read files, grep patterns, report to planner
 - Impl lings (Claude): Execute the plan

 Benefits:
 - Zero idle time between waves
 - Drones do cheap codebase exploration
 - Planner ling has rich context from scouts
 - Coordinator stays at pure strategic level
 - True pipeline: Plan N+1 while executing N

 Implementation idea:
 (dispatch-drone-wave
   {:impl-tasks [...current-tasks...]
    :scout-tasks [{:query "Find all callers of X" :report-to planner-id}]
    :planner {:ling-id "..." :task "Plan next wave from scout reports"}})

 This could be Phase 2 of drone-wave - after basic batch execution works.

