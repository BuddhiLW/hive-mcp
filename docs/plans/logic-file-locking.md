# Plan: Filesystem Logic Locking Integration

## Status
PARTIALLY IMPLEMENTED (2026-01-14)

## Summary
Wire the existing `logic.clj` module into `drone.clj` and `wave.clj` to provide automatic file locking, conflict detection, and optimal batch computation for drone operations.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     MCP Tool Layer                               │
│  handle-delegate-drone     handle-dispatch-drone-wave            │
└────────────┬──────────────────────────┬─────────────────────────┘
             │                          │
             ▼                          ▼
┌─────────────────────┐    ┌──────────────────────────────────────┐
│    drone.clj        │    │           wave.clj                    │
│    delegate!        │    │        execute-wave!                  │
│                     │    │                                       │
│  1. Lock files      │    │  1. Register all edits               │
│  2. Spawn drone     │    │  2. Compute batches (NEW)            │
│  3. Release locks   │    │  3. Execute batch → await → next     │
└─────────┬───────────┘    └──────────────┬───────────────────────┘
          │                               │
          ▼                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                    coordinator.clj                               │
│  pre-flight-check  register-task-claims!  release-task-claims!  │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                      logic.clj (core.logic pldb)                 │
│  claims relation | check-file-conflicts | compute-batches (NEW) │
└─────────────────────────────────────────────────────────────────┘
```

## Current State (What Already Exists)

### logic.clj
- Relations: `claims`, `task-files`, `depends-on`
- Mutations: `add-claim!`, `remove-claim!`, `add-task-file!`, `release-claims-for-task!`
- Queries: `check-file-conflicts`, `check-would-deadlock`, `check-dependencies-ready`

### coordinator.clj
- `pre-flight-check` - checks conflicts before dispatch
- `register-task-claims!` - registers claims after dispatch
- `release-task-claims!` - releases on completion
- `dispatch-or-queue!` - unified entry point

### Gap: drone.clj and wave.clj NOT integrated with logic module

## Implementation Progress

### Completed (This Session)

1. **Edit Relations Added to logic.clj**
   - `edit` relation (edit-id, file-path, edit-type)
   - `edit-depends` relation (edit-a, edit-b)
   - Mutation functions: `add-edit!`, `remove-edit!`, `add-edit-dependency!`, `reset-edits!`

2. **Edit Predicates Added to logic.clj**
   - `edit-conflicto` - detects file conflicts between edits
   - `edit-depends-on-o` - direct dependency check
   - `edit-reachable-fromo` - transitive closure for dependencies

3. **compute-batches Implemented in logic.clj**
   - Kahn's algorithm with conflict grouping
   - Returns `[[batch-1-edits] [batch-2-edits] ...]`
   - Edits within batch: parallel safe (no file conflicts)
   - Batches: sequential (dependencies respected)

4. **drone.clj Wired with Locking**
   - `delegate!` now wraps with:
     - Pre-flight conflict check via `coordinator/pre-flight-check`
     - Lock acquisition via `coordinator/register-task-claims!`
     - Lock release in finally block via `coordinator/release-task-claims!`
   - Throws ex-info on file conflicts

5. **wave.clj Wired with Batching**
   - `execute-wave!` now:
     - Registers edits in logic db
     - Infers test→source dependencies
     - Computes safe batches via `logic/compute-batches`
     - Executes batches sequentially (parallel within batch)
     - Cleans up edits after completion

### Remaining (For Future Sprint)

1. **Unit Tests for Edit Batching**
   - `compute-batches-test` - independent edits same batch
   - `compute-batches-conflict-test` - conflicting edits separate batches
   - `compute-batches-dependency-test` - dependent edits ordered correctly

2. **Integration Tests**
   - Wave respects batching
   - Drone locking prevents concurrent file access

3. **Extended Dependency Inference**
   - Currently: test files depend on source files
   - Future: parse task descriptions for read/write patterns

## Files Modified

| File | Status | Changes |
|------|--------|---------|
| `src/hive_mcp/swarm/logic.clj` | DONE | +80 lines: edit relations, predicates, compute-batches |
| `src/hive_mcp/agent/drone.clj` | DONE | +25 lines: wrap delegate! with locking |
| `src/hive_mcp/tools/swarm/wave.clj` | DONE | +60 lines: batch computation, sequential execution |
| `test/hive_mcp/swarm/logic_test.clj` | TODO | +40 lines: edit batching tests |

## Verification Commands

```clojure
;; 1. Check logic state
(require '[hive-mcp.swarm.logic :as logic])
(logic/dump-db)

;; 2. Test batch computation
(logic/reset-db!)
(logic/add-edit! "e1" "foo.clj" :modify)
(logic/add-edit! "e2" "bar.clj" :modify)
(logic/add-edit! "e3" "foo.clj" :modify)
(logic/compute-batches ["e1" "e2" "e3"])
;; Expected: [["e1" "e2"] ["e3"]] - e1 and e2 parallel, e3 waits

;; 3. Test drone locking
(require '[hive-mcp.swarm.coordinator :as coord])
(coord/pre-flight-check {:slave-id "drone-1" :files ["foo.clj"]})
(coord/register-task-claims! "task-1" "drone-1" ["foo.clj"])
(coord/pre-flight-check {:slave-id "drone-2" :files ["foo.clj"]})
;; Expected: {:approved? false :conflicts [{:file "foo.clj" :held-by "drone-1"}]}
```

## Related ADRs
- ADR: Logic-Ordered Drone Wave Execution (memory: 20260114225704-6bae6455)
- ADR-001: Swarm Registry Single Source of Truth
- ADR-002: Single Source of Truth for Lings Registry

## Notes
- Chose Option A (extend pldb only) over Option B (DataScript + pldb) for simplicity
- Existing coordinator.clj API was sufficient - no changes needed
- Backward compatible - new logic is additive
