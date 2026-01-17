# hive-mcp Compliance Critique Report

**Date:** 2026-01-14
**Analyzer:** Claude Opus 4.5 via hivemind coordination
**Tools Used:** scc_analyze, scc_hotspots, kondo_analyze, kondo_lint, kondo_namespace_graph

---

## Executive Summary

| Framework | Status | Score |
|-----------|--------|-------|
| **DDD** | PASS | 8/10 |
| **SOLID** | PASS | 9/10 |
| **CLARITY** | PASS | 9/10 |
| **TDD Coverage** | PASS | ~57% test-to-code ratio |
| **Static Analysis** | NEEDS ATTENTION | 147 warnings |

**Overall Assessment:** The hive-mcp codebase demonstrates **strong architectural compliance** with DDD, SOLID, and CLARITY principles. The modular design with clear bounded contexts, protocol-based abstractions, and interceptor-based composition shows mature software design. Main areas for improvement are cleaning up unused bindings and addressing deprecated var usage.

---

## 1. SCC Metrics Summary

### Source Code
| Metric | Value |
|--------|-------|
| Files | 119 |
| Total Lines | 26,978 |
| Code Lines | 20,477 |
| Comment Lines | 2,516 |
| Blank Lines | 3,985 |
| Total Complexity | 1,408 |
| Comment Ratio | 12.3% |

### Test Code
| Metric | Value |
|--------|-------|
| Files | 60 |
| Total Lines | 16,335 |
| Code Lines | 11,696 |
| Comment Lines | 2,264 |
| Complexity | 1,387 |

### Test Coverage Assessment
- **Test-to-Source Ratio:** 57.1% (11,696 test LOC / 20,477 source LOC)
- **Test File Count:** 60 tests for 119 source files (50.4% coverage by file)
- **Verdict:** Good test coverage for a project of this complexity

### Complexity Hotspots (threshold: 20)

| File | Complexity | Lines | Concern Level |
|------|------------|-------|---------------|
| `prompt_capture.clj` | 51 | 522 | HIGH |
| `tools/swarm/jvm.clj` | 50 | 360 | HIGH |
| `chroma.clj` | 48 | 510 | HIGH |
| `swarm/sync.clj` | 43 | 375 | MEDIUM |
| `diagrams/adapters/mermaid.clj` | 41 | 373 | MEDIUM |
| `transport.clj` | 40 | 537 | MEDIUM |
| `events/handlers.clj` | 37 | 624 | MEDIUM |
| `org_clj/ast_builder.clj` | 32 | 233 | LOW |
| `tools/catchup.clj` | 32 | 234 | LOW |
| `swarm/datascript.clj` | 31 | 1,111 | MEDIUM (by size) |

**Recommendation:** Consider decomposing `prompt_capture.clj`, `tools/swarm/jvm.clj`, and `chroma.clj` following the same refactoring pattern applied to the swarm module.

---

## 2. Kondo Static Analysis Summary

### Overview
| Metric | Count |
|--------|-------|
| Var Definitions | 1,501 |
| Var Usages | 14,491 |
| Namespaces | 119 |
| Total Findings | 147 |

### Findings by Category

| Category | Count | Severity |
|----------|-------|----------|
| Unused bindings | ~75 | LOW |
| Deprecated var usage | ~15 | MEDIUM |
| Unused referred vars | ~12 | LOW |
| Unused namespaces | ~10 | LOW |
| Unresolved namespaces | 3 | HIGH |
| Redefined var | 1 | HIGH |
| Redundant let | 2 | LOW |
| Duplicate require | 1 | LOW |

### Critical Issues Requiring Attention

1. **Unresolved Namespace** (`events/effects.clj:236`):
   ```clojure
   clojure.java.shell  ; Missing require
   ```

2. **Unresolved Namespace** (`events/handlers.clj:334`):
   ```clojure
   clojure.string  ; Missing require
   ```

3. **Unresolved Var** (`agent/cider.clj:172`):
   ```clojure
   agent/ollama-backend  ; Likely missing import
   ```

4. **Redefined Var** (`chroma.clj:109`):
   ```clojure
   ->MockEmbedder  ; Duplicate defrecord definition
   ```

### Deprecated Var Usage Locations
- `hooks/handlers.clj:238-262` - Multiple deprecated handlers still in use
- `tools/swarm.clj:40` - `lings-registry` deprecated
- `tools/swarm/registry.clj:64,81,95,107` - Internal usage of deprecated `lings-registry`

---

## 3. DDD Compliance Analysis

### Bounded Contexts (Score: 8/10)

The codebase exhibits **clear bounded context separation**:

| Context | Namespace Prefix | Responsibility |
|---------|------------------|----------------|
| **Swarm** | `hive-mcp.swarm.*`, `hive-mcp.tools.swarm.*` | Multi-agent orchestration |
| **Events** | `hive-mcp.events.*` | Event-driven architecture |
| **Memory** | `hive-mcp.tools.memory.*` | Project-scoped persistence |
| **Crystal** | `hive-mcp.crystal.*` | Session crystallization |
| **Agent** | `hive-mcp.agent.*` | LLM backend delegation |
| **Channel** | `hive-mcp.channel.*` | WebSocket communication |
| **Org-CLJ** | `hive-mcp.org-clj.*` | Org-mode parsing |
| **Diagrams** | `hive-mcp.diagrams.*` | Diagram generation |

### Layering (Score: 8/10)

```
┌─────────────────────────────────────────────┐
│              Application Layer              │
│    (tools/*.clj, server.clj, hivemind.clj)  │
├─────────────────────────────────────────────┤
│               Domain Layer                  │
│  (swarm/logic.clj, crystal/core.clj,       │
│   events/core.clj, org-clj/parser.clj)     │
├─────────────────────────────────────────────┤
│            Infrastructure Layer             │
│  (emacsclient.clj, transport.clj, chroma.clj│
│   embeddings/*.clj, channel/websocket.clj) │
└─────────────────────────────────────────────┘
```

**Positive Findings:**
- Domain logic (`swarm/logic.clj`) uses core.logic for declarative rules
- Clear separation between domain (`crystal/core.clj`) and persistence (`chroma.clj`)
- Infrastructure adapters (`embeddings/ollama.clj`, `embeddings/openai.clj`) are interchangeable

**Issues:**
- `events/effects.clj` directly imports `memory-crud` (layer violation)
- `server.clj` has 21 requires - too many responsibilities

---

## 4. SOLID Compliance Analysis

### Single Responsibility Principle (Score: 9/10)

**Excellent Examples:**
- `agent/protocol.clj` - Pure protocol definition (14 lines)
- `events/schemas.clj` - Only schema definitions
- `tools/memory/*.clj` - Decomposed into 8 focused modules

**Violations:**
- `server.clj` (479 lines, 21 requires) - Server setup, initialization, routing
- `swarm/datascript.clj` (1,111 lines) - Database, queries, and business logic mixed

### Open/Closed Principle (Score: 10/10)

**Excellent implementation through protocols:**

```clojure
;; agent/protocol.clj - Extension point for LLM backends
(defprotocol LLMBackend
  (chat [this messages tools])
  (model-name [this]))

;; Implementations: ollama.clj, openrouter.clj, cider.clj
```

```clojure
;; graph/protocol.clj - Extension point for graph stores
(defprotocol GraphStore ...)

;; Implementation: graph/datascript.clj
```

**New backends can be added without modifying existing code.**

### Liskov Substitution Principle (Score: 9/10)

Protocol implementations are substitutable:
- `OllamaBackend`, `OpenRouterBackend`, `CiderBackend` all implement `LLMBackend`
- `DataScriptStore` implements `GraphStore`

Minor issue: Some implementations extend beyond protocol contracts with helper functions.

### Interface Segregation Principle (Score: 9/10)

Protocols are focused:
- `LLMBackend` - 2 methods only
- `GraphStore` - CRUD operations only
- `Embedder` - Single `embed` method

### Dependency Inversion Principle (Score: 9/10)

**Good patterns:**
- High-level modules depend on `LLMBackend` protocol, not concrete implementations
- `agent/loop.clj` takes protocol instance, not concrete backend
- `events/core.clj` uses registered handlers, not direct dependencies

**Minor violations:**
- Some tool namespaces directly require `emacsclient` instead of abstracting

---

## 5. CLARITY Compliance Analysis

### C - Composition over Modification (Score: 9/10)

**Interceptor chain pattern in events:**
```clojure
;; events/core.clj - Interceptors compose behavior
(def metrics
  (->interceptor
   :id :metrics
   :before (fn [ctx] ...)
   :after (fn [ctx] ...)))

(def validate-event
  (->interceptor
   :id :validate-event
   :before (fn [ctx] ...)))

;; Composed via registration
(reg-event :task-complete
  [metrics validate-event]
  handler-fn)
```

### L - Layers Stay Pure (Score: 8/10)

**Good:**
- Domain logic in `swarm/logic.clj` has no I/O
- `crystal/core.clj` is pure transformation

**Violation:**
- `events/effects.clj:236` - Shell command in effect (should be delegated)
- `server.clj` mixes initialization with domain setup

### A - Architectural Performance (Score: 9/10)

**Non-blocking patterns observed:**
- `core.async` channels for swarm communication
- WebSocket streams via manifold
- Future-based nREPL transport

### R - Represented Intent (Score: 10/10)

**Excellent naming throughout:**
- `hivemind_shout` - Clear communication metaphor
- `crystal/recall.clj` - Memory retrieval intent
- `tools/memory/lifecycle.clj` - Duration management
- `events/interceptors.clj` - Self-documenting

### I - Inputs are Guarded (Score: 10/10)

**Malli validation at boundaries:**
```clojure
;; events/core.clj:411
(schemas/validate-event! event) ;; CLARITY: Guard inputs at boundary

;; events/schemas.clj - Dedicated schema module
(def Event [:vector [:cat :keyword [:* :any]]])
```

**Validation interceptor:**
```clojure
(validate-event TaskData)  ;; Schema validation at handler level
```

### T - Telemetry First (Score: 10/10)

**Metrics built into event system:**
```clojure
;; events/core.clj:54-112
(def ^:private *metrics
  (atom {:events-dispatched 0
         :events-by-type {}
         :effects-executed 0
         :errors 0
         :timings-by-type {}
         :timings []}))
```

- Telemetry module: `telemetry.clj`
- Structured logging via timbre throughout
- Metrics interceptor tracks per-event timing

### Y - Yield Safe Failure (Score: 8/10)

**Error handling patterns:**
```clojure
;; events/core.clj:216-224
(try
  (f context)
  (catch Exception e
    (throw (ex-info (str "Interceptor " (:id interceptor) " threw")
                    {:interceptor-id (:id interceptor)
                     :direction direction
                     :cause e}
                    e))))
```

**Graceful degradation observed in:**
- `chroma.clj` - Mock embedder fallback
- `emacsclient.clj` - Error wrapping

**Missing:**
- Circuit breaker usage in `resilience.clj` appears incomplete (unused bindings)

---

## 6. TDD Assessment

### Test Organization

| Category | Files | Description |
|----------|-------|-------------|
| Unit Tests | ~45 | Isolated function testing |
| Integration Tests | ~10 | Cross-module interactions |
| Pinning Tests | ~5 | API contract verification |

### Test Pattern Quality

**Good patterns observed:**
- `with-clean-registry` macro for test isolation
- Dedicated test fixtures
- Property-based testing in some modules

**Areas for improvement:**
- `swarm/datascript.clj` (1,111 lines) has only 1 test file
- Complexity hotspots lack proportional test coverage

### Coverage Estimation by Module

| Module | Source LOC | Test Coverage |
|--------|------------|---------------|
| `events/*` | ~1,200 | HIGH |
| `swarm/*` | ~2,500 | MEDIUM |
| `memory/*` | ~800 | HIGH |
| `crystal/*` | ~1,400 | MEDIUM |
| `agent/*` | ~1,000 | MEDIUM |
| `org-clj/*` | ~900 | LOW |
| `diagrams/*` | ~1,100 | LOW |

---

## 7. Prioritized Remediation Recommendations

### Priority 1: Critical (Address Immediately)

1. **Fix unresolved namespaces** - 3 occurrences causing potential runtime errors
   - `events/effects.clj:236` - Add `[clojure.java.shell :as shell]`
   - `events/handlers.clj:334` - Add `[clojure.string :as str]`
   - `agent/cider.clj:172` - Fix `agent/ollama-backend` reference

2. **Remove redefined var** in `chroma.clj:109`
   - Duplicate `->MockEmbedder` definition

### Priority 2: High (Address This Sprint)

3. **Clean up deprecated var usage** - 15 occurrences
   - `hooks/handlers.clj` - Replace deprecated handlers
   - `tools/swarm/registry.clj` - Migrate from `lings-registry`

4. **Decompose complexity hotspots:**
   - `prompt_capture.clj` (complexity 51) - Extract validation, storage, search
   - `tools/swarm/jvm.clj` (complexity 50) - Extract process parsing, orphan detection

### Priority 3: Medium (Technical Debt)

5. **Clean up unused bindings** - ~75 occurrences
   - Many are in diagram adapters and hooks - likely copy-paste artifacts

6. **Address layer violations:**
   - `events/effects.clj` importing `memory-crud` directly
   - `server.clj` - Extract initialization into separate modules

7. **Complete resilience patterns:**
   - `resilience.clj` has unused circuit breaker bindings

### Priority 4: Low (Continuous Improvement)

8. **Increase test coverage for:**
   - `org-clj/*` modules
   - `diagrams/*` adapters
   - `swarm/datascript.clj`

9. **Documentation debt:**
   - Add ADRs for key architectural decisions
   - Document bounded context boundaries

---

## 8. Conclusion

The hive-mcp codebase demonstrates **strong adherence to modern software design principles**. The event-driven architecture with interceptor chains, protocol-based abstractions, and clear bounded contexts show thoughtful design.

**Key Strengths:**
- Excellent OCP compliance through protocols
- Re-frame inspired event system with proper CLARITY implementation
- Clear domain separation with focused modules
- Strong input validation patterns

**Key Improvements Needed:**
- Static analysis cleanup (147 warnings)
- Decomposition of complexity hotspots
- Completing the resilience module

**Overall Grade: B+**

The architecture is solid and maintainable. With the priority 1 and 2 fixes addressed, this would easily be an A-grade codebase.

---

*Report generated by hivemind codebase critique agent*
