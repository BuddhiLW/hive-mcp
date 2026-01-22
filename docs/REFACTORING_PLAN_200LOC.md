# Refactoring Plan: 200 LOC / SRP Compliance

## Executive Summary
- **Total Files > 200 LOC**: 49
- **Critical Files (> 400 LOC)**: 15
- **Total Complexity Hotspots (> 20)**: 55
- **Estimated Refactoring Scope**: ~15,000 LOC to reorganize

## CLARITY/SOLID/DDD Alignment Principles

### CLARITY Applied to Clojure:
- **C (Composition)**: Use protocols, multimethods, and higher-order functions
- **L (Layers)**: Separate pure functions from side-effects, namespace boundaries
- **A (Performance)**: Lazy sequences, transducers, caching atoms
- **R (Represented Intent)**: Rich domain maps with specs, keyword namespacing
- **I (Inputs Guarded)**: Malli schemas at namespace boundaries
- **T (Telemetry)**: Structured logging via timbre, Prometheus metrics
- **Y (Yield Safe Failure)**: ex-info with error types, graceful degradation

### SOLID for FP:
- **SRP**: One namespace = one reason to change
- **OCP**: Extend via protocols/multimethods, not modification
- **LSP**: Consistent return shapes, spec conformance
- **ISP**: Small focused protocols (2-3 methods max)
- **DIP**: Depend on protocols, inject implementations

---

## Phase 1: Extract Pure Functions (Low Risk)

Extract pure functions to separate namespaces without changing behavior.

| Current File | LOC | Extract To | Responsibility |
|--------------|-----|------------|----------------|
| `events/effects.clj` | 886 | `events/effects/kg.clj` | KG effect handlers |
| `events/effects.clj` | 886 | `events/effects/system.clj` | System effects (log, dispatch) |
| `events/effects.clj` | 886 | `events/effects/session.clj` | Session effects (wrap, git, kanban) |
| `tools/swarm/wave.clj` | 754 | `tools/swarm/wave/validation.clj` | Pre-flight validation |
| `tools/swarm/wave.clj` | 754 | `tools/swarm/wave/batching.clj` | Batch computation helpers |
| `swarm/logic.clj` | 632 | `swarm/logic/predicates.clj` | core.logic predicates |
| `swarm/logic.clj` | 632 | `swarm/logic/batching.clj` | Kahn's algorithm for batches |
| `chroma.clj` | 541 | `chroma/helpers.clj` | DRY utility functions |
| `agent/drone.clj` | 468 | `agent/drone/errors.clj` | Error classification |

---

## Phase 2: Split by Responsibility (Medium Risk)

Split files with multiple responsibilities into bounded modules.

### 2.1 events/effects.clj (886 LOC, complexity 46) → 5 files

**Current Responsibilities:**
1. Effect handler registration infrastructure
2. Coeffect handler registration
3. Core effects (shout, log, dispatch)
4. Session effects (wrap, git, kanban)
5. Knowledge Graph effects (kg-add-edge, etc.)

**Split:**
```
events/
├── effects/
│   ├── core.clj        (~100 LOC) - Registration, helpers
│   ├── system.clj      (~150 LOC) - shout, log, dispatch, channel
│   ├── session.clj     (~150 LOC) - wrap, git, kanban
│   ├── kg.clj          (~200 LOC) - Knowledge graph effects
│   └── coeffects.clj   (~100 LOC) - Coeffect registrations
```

### 2.2 tools/swarm/wave.clj (754 LOC, complexity 58) → 4 files

**Current Responsibilities:**
1. Pre-flight validation (ensure dirs, validate paths)
2. Plan management (delegates to datascript)
3. Drone execution with retry
4. Wave orchestration (core.async batching)
5. MCP handlers

**Split:**
```
tools/swarm/wave/
├── core.clj         (~100 LOC) - Constants, re-exports
├── validation.clj   (~100 LOC) - Pre-flight checks
├── execution.clj    (~300 LOC) - Batch execution
├── handlers.clj     (~100 LOC) - MCP tool handlers
```

### 2.3 events/core.clj (723 LOC, complexity 14) → 4 files

**Current Responsibilities:**
1. Interceptor construction & execution
2. Context access helpers
3. Effect/Coeffect registration
4. Event dispatch
5. Built-in interceptors

**Split:**
```
events/
├── core.clj           (~150 LOC) - Public API, re-exports
├── interceptors.clj   (~200 LOC) - Interceptor construction/execution
├── registry.clj       (~150 LOC) - Handler registration
├── builtin.clj        (~100 LOC) - debug, metrics, validate-event
```

### 2.4 swarm/datascript/coordination.clj (702 LOC, complexity 24) → 5 files

**Current Responsibilities:**
1. Wrap queue operations
2. Change plan operations
3. Wave operations
4. Coordinator lifecycle
5. Completed task registry

**Split:**
```
swarm/datascript/
├── coordination.clj   (~50 LOC)  - Re-exports
├── wrap_queue.clj     (~130 LOC) - Wrap notifications
├── plans.clj          (~150 LOC) - Change plan CRUD
├── waves.clj          (~100 LOC) - Wave CRUD
├── coordinator.clj    (~200 LOC) - Coordinator lifecycle
├── tasks.clj          (~100 LOC) - Completed task registry
```

### 2.5 hivemind.clj (603 LOC, complexity 30) → 4 files

**Current Responsibilities:**
1. State atoms (asks, registry, prompts, results)
2. Shout/ask operations
3. Status queries
4. MCP tool definitions

**Split:**
```
hivemind/
├── core.clj     (~50 LOC)  - Re-exports
├── state.clj    (~100 LOC) - Atoms, state management
├── shout.clj    (~150 LOC) - shout!, ask!, respond!
├── status.clj   (~100 LOC) - get-status, build-agents-map
├── tools.clj    (~150 LOC) - MCP tool definitions
```

### 2.6 swarm/logic.clj (632 LOC, complexity 28) → 4 files

**Current Responsibilities:**
1. Database relations (pldb/db-rel)
2. Mutation functions (add/remove facts)
3. Query predicates (file-conflicto, would-deadlocko)
4. Batch computation (Kahn's algorithm)

**Split:**
```
swarm/logic/
├── core.clj        (~50 LOC)  - Re-exports, db atom
├── relations.clj   (~100 LOC) - pldb/db-rel definitions
├── mutations.clj   (~150 LOC) - Add/remove facts
├── predicates.clj  (~150 LOC) - Query predicates
├── batching.clj    (~150 LOC) - Kahn's algorithm
```

### 2.7 crystal/graph.clj (597 LOC, complexity 25) → 3 files

**Current Responsibilities:**
1. Database state management
2. Entry CRUD operations
3. Query functions (by session, by type, etc.)

**Split:**
```
crystal/graph/
├── core.clj     (~50 LOC)  - Re-exports
├── state.clj    (~100 LOC) - Store, reset, db access
├── entries.clj  (~200 LOC) - Entry CRUD
├── queries.clj  (~200 LOC) - Query functions
```

### 2.8 agent/drone.clj (468 LOC, complexity 54) → 4 files

**Current Responsibilities:**
1. Error classification (nREPL error types)
2. Context preparation (catchup, file contents)
3. Diff management (auto-apply)
4. Delegation API (delegate! function)

**Split:**
```
agent/drone/
├── core.clj     (~50 LOC)  - Re-exports, constants
├── errors.clj   (~130 LOC) - Error classification
├── context.clj  (~100 LOC) - Context preparation
├── delegate.clj (~180 LOC) - Delegation API
```

### 2.9 chroma.clj (541 LOC, complexity 22) → 4 files

**Current Responsibilities:**
1. Configuration
2. Embedding protocol
3. Collection operations
4. Index/Search operations

**Split:**
```
chroma/
├── core.clj       (~50 LOC)  - Re-exports
├── config.clj     (~80 LOC)  - Configuration
├── embedding.clj  (~100 LOC) - Protocol & providers
├── operations.clj (~200 LOC) - Index/Search/CRUD
```

---

## Phase 3: Reduce Complexity (High Value)

Target functions with cyclomatic complexity > 10.

| File | Function | Complexity | Strategy |
|------|----------|------------|----------|
| `wave.clj` | `execute-wave!` | ~15 | Extract phases to helper fns |
| `drone.clj` | `delegate!` | ~12 | Extract lock/unlock wrappers |
| `effects.clj` | `register-effects!` | ~10 | Split into domain-specific registrations |
| `coordination.clj` | `cleanup-stale-coordinators!` | ~8 | Extract filter predicate |
| `logic.clj` | `compute-batches` | ~10 | Extract Kahn steps to fns |

**Strategies:**
1. **Extract conditionals**: Replace nested if/cond with polymorphism
2. **Introduce parameter objects**: Group related params into maps
3. **Use threading macros**: Flatten nested calls
4. **Extract predicates**: Named fns for complex conditions

---

## Phase 4: Additional Files (Lower Priority)

Files between 200-400 LOC that should be reviewed:

| File | LOC | Complexity | Action |
|------|-----|------------|--------|
| `transport.clj` | 485 | 29 | Split protocol impl vs. handlers |
| `evaluator.clj` | 527 | 16 | Extract nREPL retry logic |
| `server.clj` | 519 | 21 | Split initialization vs. routes |
| `presets.clj` | 414 | 26 | Split storage vs. search |
| `tools/cider.clj` | 392 | 31 | Split session mgmt vs. eval |
| `resilience.clj` | 391 | 20 | Already well-structured |
| `tools/diff.clj` | 434 | 27 | Split propose vs. apply |
| `tools/buffer.clj` | 416 | 27 | Group by buffer operation type |
| `crystal/hooks.clj` | 478 | 22 | Split harvesting vs. event handlers |
| `swarm/datascript/lings.clj` | 490 | 17 | Split CRUD vs. queries |
| `knowledge_graph/queries.clj` | 421 | 22 | Split traversal vs. analysis |
| `agora/dialogue.clj` | 442 | - | Split turn mgmt vs. synthesis |
| `agora/schema.clj` | 473 | 11 | Split schema vs. validation |
| `agora/consensus.clj` | 404 | 17 | Split proposing vs. voting |

---

## File-by-File Summary Table

| Current File | LOC | Complexity | Split Into | New Files |
|--------------|-----|------------|------------|-----------|
| `events/effects.clj` | 886 | 46 | 5 modules | 5 |
| `tools/swarm/wave.clj` | 754 | 58 | 4 modules | 4 |
| `events/core.clj` | 723 | 14 | 4 modules | 4 |
| `swarm/datascript/coordination.clj` | 702 | 24 | 5 modules | 5 |
| `swarm/logic.clj` | 632 | 28 | 4 modules | 4 |
| `hivemind.clj` | 603 | 30 | 4 modules | 4 |
| `crystal/graph.clj` | 597 | 25 | 3 modules | 3 |
| `agent/drone.clj` | 468 | 54 | 4 modules | 4 |
| `chroma.clj` | 541 | 22 | 4 modules | 4 |
| **Total** | **5906** | - | - | **37** |

---

## Implementation Order (Risk-Based)

### Sprint 1: Pure Function Extraction (Lowest Risk)
1. `events/effects.clj` → Extract KG effects to `effects/kg.clj`
2. `swarm/logic.clj` → Extract predicates to `logic/predicates.clj`
3. `agent/drone.clj` → Extract errors to `drone/errors.clj`

### Sprint 2: Core Event System
1. `events/core.clj` → Split interceptors, registry, builtin
2. `events/effects.clj` → Complete split (system, session, coeffects)

### Sprint 3: Swarm Coordination
1. `tools/swarm/wave.clj` → Split validation, execution, handlers
2. `swarm/datascript/coordination.clj` → Split by operation type
3. `swarm/logic.clj` → Complete split

### Sprint 4: Communication & Memory
1. `hivemind.clj` → Split state, shout, status, tools
2. `crystal/graph.clj` → Split state, entries, queries
3. `chroma.clj` → Split config, operations

---

## Verification Criteria

After each refactoring:
1. **LOC Check**: Each new file ≤ 200 LOC
2. **SRP Check**: Each namespace has single clear purpose
3. **Test Coverage**: Existing tests pass, add tests for new boundaries
4. **Import Check**: No circular dependencies introduced
5. **Kondo Check**: No new warnings/errors
6. **Complexity Check**: Functions ≤ 10 cyclomatic complexity

---

## Risk Mitigation

1. **Feature Flags**: Use namespace aliases for gradual migration
2. **Test First**: Run full test suite before/after each change
3. **Git Commits**: Atomic commits per file extraction
4. **Rollback Plan**: Each sprint is independently revertible
5. **Incremental**: Extract one function/section at a time

---

## DDD Bounded Contexts

The refactoring aligns with these bounded contexts:

```
┌─────────────────────────────────────────────────────────────┐
│                     HIVEMIND CONTEXT                        │
│  hivemind/, channel/, swarm/coordinator                     │
│  - Agent communication                                       │
│  - Shout/Ask protocol                                        │
│  - Status aggregation                                        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     SWARM CONTEXT                            │
│  swarm/logic, swarm/datascript, tools/swarm                 │
│  - Ling lifecycle                                            │
│  - Wave orchestration                                        │
│  - File claims & conflict resolution                         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     MEMORY CONTEXT                           │
│  chroma/, crystal/, tools/memory, knowledge_graph            │
│  - Memory persistence (Chroma)                               │
│  - Session tracking (Crystal)                                │
│  - Knowledge relationships (KG)                              │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     AGENT CONTEXT                            │
│  agent/, agent/drone, agent/openrouter                      │
│  - Delegation protocols                                      │
│  - Error handling                                            │
│  - Context preparation                                       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     EVENTS CONTEXT                           │
│  events/core, events/effects, events/handlers               │
│  - Interceptor chain                                         │
│  - Effect execution                                          │
│  - Event dispatch                                            │
└─────────────────────────────────────────────────────────────┘
```

---

## Appendix: Files Exceeding 200 LOC

| # | File | LOC | Complexity |
|---|------|-----|------------|
| 1 | events/effects.clj | 886 | 46 |
| 2 | tools/swarm/wave.clj | 754 | 58 |
| 3 | events/core.clj | 723 | 14 |
| 4 | swarm/datascript/coordination.clj | 702 | 24 |
| 5 | swarm/logic.clj | 632 | 28 |
| 6 | hivemind.clj | 603 | 30 |
| 7 | crystal/graph.clj | 597 | 25 |
| 8 | specs/tools.clj | 564 | - |
| 9 | telemetry/prometheus.clj | 541 | - |
| 10 | chroma.clj | 541 | 22 |
| 11 | evaluator.clj | 527 | 16 |
| 12 | server.clj | 519 | 21 |
| 13 | specs/agent.clj | 492 | - |
| 14 | swarm/datascript/lings.clj | 490 | 17 |
| 15 | transport.clj | 485 | 29 |
| 16 | crystal/hooks.clj | 478 | 22 |
| 17 | graph/schema.clj | 475 | 10 |
| 18 | agora/schema.clj | 473 | 11 |
| 19 | agent/drone.clj | 468 | 54 |
| 20 | tools/kg.clj | 461 | 17 |
| 21 | agora/dialogue.clj | 442 | - |
| 22 | tools/swarm.clj | 440 | - |
| 23 | tools/diff.clj | 434 | 27 |
| 24 | knowledge_graph/queries.clj | 421 | 22 |
| 25 | tools/buffer.clj | 416 | 27 |
| ... | ... | ... | ... |

*Full list: 49 files total*

---

Generated: 2026-01-21
Author: swarm-refactor-planner
Tags: refactoring, plan, srp, 200-loc, solid, clarity, ddd, architecture
