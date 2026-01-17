# Sprint Plan: A+ Compliance for hive-mcp

**Created:** 2026-01-14
**Target:** A+ (all categories 9.5+/10)
**Current:** DDD 8/10, SOLID 9/10, CLARITY 9/10, TDD ~57%, Grade B+

---

## Sprint Architecture

```
                    ┌─────────────────────────────────────────────────────────────┐
                    │                    COORDINATOR                               │
                    │          (Monitors progress, resolves conflicts)             │
                    └─────────────────────────────────────────────────────────────┘
                                               │
         ┌─────────────────┬─────────────────┼─────────────────┬─────────────────┐
         │                 │                 │                 │                 │
         ▼                 ▼                 ▼                 ▼                 ▼
    ┌─────────┐       ┌─────────┐       ┌─────────┐       ┌─────────┐       ┌─────────┐
    │Stream A │       │Stream B │       │Stream C │       │Stream D │       │Stream E │
    │ Static  │       │Decompose│       │ Layer   │       │  Test   │       │Deprecated│
    │Analysis │       │Complex  │       │  Pure   │       │Coverage │       │Cleanup   │
    └─────────┘       └─────────┘       └─────────┘       └─────────┘       └─────────┘
```

---

## Work Stream Design

### Stream A: Static Analysis Cleanup (4 tasks, all parallel)
**Focus:** Critical unresolved namespace/var fixes
**Independence:** Each file is independent, can run in full parallel
**Estimated Impact:** +0.5 SOLID, +0.5 CLARITY

| Task | File | Fix | SOLID Pattern |
|------|------|-----|---------------|
| A1 | `events/effects.clj:236` | Add `[clojure.java.shell :as shell]` | SRP - explicit deps |
| A2 | `events/handlers.clj:334` | Add `[clojure.string :as str]` | SRP - explicit deps |
| A3 | `agent/cider.clj:172` | Fix `agent/ollama-backend` reference | DIP - proper import |
| A4 | `chroma.clj:109` | Remove duplicate `->MockEmbedder` defrecord | ISP - single definition |

### Stream B: Complexity Decomposition (6 tasks, some dependencies)
**Focus:** High-complexity hotspots → smaller, focused modules
**Independence:** Each file decomposition is independent
**Estimated Impact:** +1.0 DDD, +0.5 SOLID

| Task | Target | Extraction | Functional Pattern |
|------|--------|------------|-------------------|
| B1 | `prompt_capture.clj` | Extract validation module | **Spec/Malli** contracts |
| B2 | `prompt_capture.clj` | Extract storage adapter | **Protocol** for persistence |
| B3 | `prompt_capture.clj` | Extract search module | **Transducer** pipelines |
| B4 | `tools/swarm/jvm.clj` | Extract process parser | **Multimethod** dispatch by OS |
| B5 | `tools/swarm/jvm.clj` | Extract orphan detector | **Higher-order fn** composition |
| B6 | `chroma.clj` | Extract mock embedder to test ns | **Protocol** test double |

**Dependency Graph:**
```
B1 → B2 → B3  (sequential - storage depends on validation)
B4 → B5       (sequential - orphan detection uses parser)
B6            (independent)
```

### Stream C: Layer Purification (4 tasks, mixed)
**Focus:** DDD layer violations and incomplete abstractions
**Independence:** Most are independent file changes
**Estimated Impact:** +1.0 DDD, +0.5 CLARITY

| Task | Location | Fix | Pattern |
|------|----------|-----|---------|
| C1 | `events/effects.clj` | Replace direct `memory-crud` import with effect | **Effect handler** indirection |
| C2 | `server.clj` | Extract initialization to `server/init.clj` | **Component/Integrant** lifecycle |
| C3 | `server.clj` | Extract routing to `server/routes.clj` | **SRP** - focused responsibility |
| C4 | `resilience.clj` | Complete circuit breaker implementation | **Strategy** pattern for fallbacks |

**Dependency Graph:**
```
C1            (independent)
C2 → C3       (sequential - routing uses init)
C4            (independent)
```

### Stream D: Test Coverage Boost (6 tasks, all parallel)
**Focus:** Low-coverage modules identified in critique
**Independence:** Each test module is independent
**Estimated Impact:** +5-10% TDD ratio, coverage targets

| Task | Module | Test Focus | Coverage Target |
|------|--------|------------|-----------------|
| D1 | `org-clj/parser.clj` | Parsing edge cases | 80% fn coverage |
| D2 | `org-clj/ast_builder.clj` | AST construction | 80% fn coverage |
| D3 | `org-clj/serializer.clj` | Round-trip integrity | Property-based |
| D4 | `diagrams/adapters/mermaid.clj` | Diagram generation | 70% fn coverage |
| D5 | `diagrams/core.clj` | Core diagram ops | 80% fn coverage |
| D6 | `swarm/datascript.clj` | DataScript queries | 60% fn coverage |

### Stream E: Deprecated Var Cleanup (5 tasks, parallel)
**Focus:** Clean up deprecated var usage and unused bindings
**Independence:** Each file cleanup is independent
**Estimated Impact:** +0.25 SOLID, -50 warnings

| Task | Location | Fix | Pattern |
|------|----------|-----|---------|
| E1 | `hooks/handlers.clj:238-262` | Replace deprecated handlers | **Adapter** pattern |
| E2 | `tools/swarm/registry.clj` | Migrate from `lings-registry` | **Repository** pattern |
| E3 | Diagram adapters | Clean unused bindings | Code hygiene |
| E4 | Hook modules | Clean unused bindings | Code hygiene |
| E5 | Test fixtures | Clean unused let bindings | Code hygiene |

---

## Functional Patterns Specification

### Multimethods (Stream B: B4)
```clojure
;; Process parsing dispatch by OS
(defmulti parse-process-line :os)

(defmethod parse-process-line :linux [line]
  ;; Linux-specific ps output parsing
  )

(defmethod parse-process-line :darwin [line]
  ;; macOS-specific ps output parsing
  )
```

### Protocols (Stream B: B2, B6)
```clojure
;; Prompt storage protocol for DIP
(defprotocol PromptStore
  (store-prompt [this prompt-data])
  (query-prompts [this criteria])
  (delete-prompt [this id]))

;; Implementations: chroma-store, memory-store, mock-store
```

### Transducers (Stream B: B3)
```clojure
;; Search pipeline as composable transducers
(def search-xf
  (comp
    (filter valid-prompt?)
    (map enrich-with-metadata)
    (hive-mcp.search/score-by-relevance query)))

(transduce search-xf conj [] prompts)
```

### Component/Integrant (Stream C: C2)
```clojure
;; Server lifecycle management
(defmethod ig/init-key :server/http [_ {:keys [port handler]}]
  (http/start-server handler {:port port}))

(defmethod ig/halt-key! :server/http [_ server]
  (.close server))
```

### Higher-Order Functions (Stream B: B5)
```clojure
;; Orphan detection as composable predicates
(defn orphan-detector [& predicates]
  (fn [process]
    (every? #(% process) predicates)))

(def default-detector
  (orphan-detector
    truly-orphaned?
    old-enough?
    not-protected-type?))
```

### Specs/Malli (Stream B: B1)
```clojure
;; Prompt validation schema
(def PromptData
  [:map
   [:prompt [:string {:min 1}]]
   [:accomplishes [:string {:min 1}]]
   [:well_structured [:string {:min 1}]]
   [:category {:optional true}
    [:enum "coding" "debug" "planning" "meta" "research" "config" "workflow" "architecture"]]
   [:quality {:optional true}
    [:enum "success" "partial" "failure" "untested"]]])

(defn validate-prompt! [data]
  (when-let [error (m/explain PromptData data)]
    (throw (ex-info "Invalid prompt data" {:error (me/humanize error)}))))
```

---

## Parallelization Analysis

### Full Parallel (Wave 1) - Launch simultaneously
```
A1, A2, A3, A4  (4 lings)
B6              (1 ling)
C1, C4          (2 lings)
D1, D2, D3, D4, D5, D6  (6 lings)
E3, E4, E5      (3 lings)
─────────────────────────
Total Wave 1: 16 parallel tasks
```

### Sequenced (Wave 2) - After dependencies complete
```
B1 → B2 → B3    (sequential chain)
B4 → B5         (sequential chain)
C2 → C3         (sequential chain)
E1, E2          (after A1-A4 confirm no conflicts)
─────────────────────────
Total Wave 2: 3 chains + 2 tasks
```

### Estimated Parallel Speedup
- **Sequential execution:** ~25 tasks × 15 min = 6.25 hours
- **Parallel Wave 1:** 16 tasks in parallel = 15-20 min
- **Parallel Wave 2:** 3 chains × 45 min = 45 min (longest chain)
- **Total parallel:** ~1.5 hours
- **Speedup:** ~4x

---

## Verification Criteria

### Stream A Tasks
- [ ] No kondo warnings for unresolved namespaces
- [ ] No duplicate defrecord warnings
- [ ] All imports explicit and used

### Stream B Tasks
- [ ] Complexity per file < 30
- [ ] Each extracted module has single responsibility
- [ ] Protocols have 2-3 methods max
- [ ] Tests pass for extracted modules

### Stream C Tasks
- [ ] No direct infrastructure imports in domain layer
- [ ] Server initialization separated from routing
- [ ] Circuit breaker properly integrated with retry logic

### Stream D Tasks
- [ ] Each target module has matching test file
- [ ] Minimum 70% function coverage
- [ ] Edge cases documented in test names

### Stream E Tasks
- [ ] No deprecated var usage warnings
- [ ] Unused binding count reduced by 50+

---

## Success Metrics

| Metric | Current | Target | Verification |
|--------|---------|--------|--------------|
| DDD Score | 8/10 | 9.5/10 | Namespace graph review |
| SOLID Score | 9/10 | 9.5/10 | Protocol/SRP audit |
| CLARITY Score | 9/10 | 9.5/10 | Layer violation count |
| TDD Ratio | 57% | 65%+ | scc_analyze comparison |
| Kondo Warnings | 147 | <50 | kondo_lint count |
| Complexity Hotspots | 3 | 0 | scc_hotspots threshold=40 |

---

## Risk Mitigation

1. **Merge Conflicts:** Stream B tasks touch same files - sequence within stream
2. **Test Failures:** Run full test suite after each stream completion
3. **Circular Dependencies:** Monitor namespace graph after decomposition
4. **Performance Regression:** Benchmark critical paths before/after

---

## Post-Sprint Validation

```bash
# Run full validation after sprint
clojure -M:dev:test                           # All tests pass
bb mcp-clj --tool kondo_analyze --path src    # <50 warnings
bb mcp-clj --tool scc_hotspots --path src --threshold 40  # 0 hotspots
```

---

*Sprint plan generated by hivemind coordinator*
