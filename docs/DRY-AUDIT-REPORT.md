# DRY Violation Audit Report - hive-mcp (2026-01-15)

## Executive Summary
Comprehensive audit identified **21 major DRY violations** across 3 categories:
- Test files: 8 violations (60+ files affected)
- Tool handlers: 7 violations (39 files, 128 handlers)
- API/validation: 6 violations (5 core files)

**Total impact:** ~70% of handlers have at least one DRY violation

---

## CATEGORY A: TEST FILE VIOLATIONS

### A1. Fixture Boilerplate (HIGH)
- **Pattern:** Reset state fixtures duplicated across 5+ test files
- **Files:** make_tool_test.clj, core_test.clj, swarm_test.clj, channel_test.clj
- **Solution:** Create `test-utils/make-reset-fixture` helper

### A2. Mock Factory Functions (MEDIUM)
- **Pattern:** mock-emacsclient-*, make-*-evaluator duplicated
- **Files:** tools_duration_test.clj, resilience_test.clj
- **Solution:** Create `test-mocks.clj` with generic mock builders

### A3. Elisp Helper Wrappers (LOW)
- **Pattern:** Thin `*-elisp` wrappers around `el/require-and-call-json`
- **Files:** memory_dedup_test.clj, memory_scope_test.clj, memory_duration_test.clj
- **Solution:** Consider macro `defn-elisp-helper` or inline

### A4. JSON Response Parsing (HIGH)
- **Pattern:** `(json/read-str (:text result) :key-fn keyword)` - 20+ times
- **Files:** swarm_test.clj, memory_test.clj, tools_duration_test.clj
- **Solution:** Create `test-assertions/parse-json-response`

### A5. Test Data Duplication (LOW)
- **Pattern:** Tool sets duplicated instead of derived
- **Files:** agent_test.clj, permissions_test.clj
- **Solution:** Use set operations to derive from base sets

### A6. Thread.sleep Magic Numbers (LOW)
- **Pattern:** `(Thread/sleep 100)` repeated 10+ times
- **Files:** swarm_channel_test.clj, channel_test.clj
- **Solution:** Create timing constants namespace

### A7. with-redefs Boilerplate (MEDIUM)
- **Pattern:** Same mock setups repeated in multiple tests
- **Files:** swarm_test.clj, tools_duration_test.clj
- **Solution:** Create `with-default-swarm-mocks` macro

### A8. String Assertions (HIGH)
- **Pattern:** `(is (str/includes? ...))` chains - 50+ times
- **Files:** elisp_test.clj, memory_*_test.clj
- **Solution:** Create `assert-elisp-includes`, `assert-valid-elisp-wrapper`

---

## CATEGORY B: TOOL HANDLER VIOLATIONS

### B1. Elisp Eval Error Handling (CRITICAL)
- **Pattern:** `(let [{:keys [success result error]} (ec/eval-elisp ...)]` - 25+ handlers
- **Files:** projectile.clj, memory_kanban.clj, buffer.clj, cider.clj, magit.clj, kanban.clj
- **Solution:** Create `call-elisp-safe` macro in tools/core.clj

### B2. Addon Availability Check (HIGH)
- **Pattern:** `*-addon-available?` functions duplicated in 7 files
- **Files:** magit.clj, kanban.clj, projectile.clj, cider.clj, memory_kanban.clj, buffer.clj
- **Solution:** Create generic `with-addon` macro

### B3. Elisp Builder Conditionals (MEDIUM)
- **Pattern:** Conditional elisp string building repeated
- **Files:** projectile.clj, magit.clj, kanban.clj, cider.clj, memory_kanban.clj
- **Solution:** Create `require-and-call*`, `build-elisp-props` helpers

### B4. Try/Catch Validation (MEDIUM)
- **Pattern:** Identical try-catch validation wrappers - 10+ handlers
- **Files:** buffer.clj, cider.clj, diff.clj
- **Solution:** Create `with-validation` wrapper (validation.clj)

### B5. Handler Logging (LOW)
- **Pattern:** `(log/info ...)` as first line - 40+ handlers
- **Solution:** Optional `with-logging` macro

### B6. Memory Response Formatting (HIGH)
- **Pattern:** Raw `{:type "text" :text (json/write-str ...)}` instead of mcp-json
- **Files:** memory/crud.clj, memory/lifecycle.clj, memory/search.clj
- **Solution:** Use existing `mcp-json` from tools/core.clj

### B7. Parameter Destructuring (LOW)
- **Pattern:** Repeated `:keys` destructuring
- **Solution:** Metadata-driven approach (future)

---

## CATEGORY C: API/VALIDATION VIOLATIONS

### C1. Spec Definition Duplication (CRITICAL)
- **Pattern:** Specs defined TWICE with inconsistency (keyword? vs string?)
- **File:** server.clj:29-47 and 74-84
- **Solution:** Remove duplicate, keep single definition

### C2. Agent ID Extraction (MEDIUM)
- **Pattern:** 4-way fallback for agent-id extraction
- **File:** server.clj:152-158
- **Solution:** Create `extract-agent-id` helper

### C3. Validation Error Handling (MEDIUM)
- **Pattern:** try-catch with validation error check - repeated
- **Files:** buffer.clj (3 instances)
- **Solution:** Create `with-validation` wrapper

### C4. Elisp String Escaping (HIGH)
- **Pattern:** `(str "\"" (v/escape-elisp-string x) "\"")` - 33+ uses
- **Files:** memory_kanban.clj, other tool files
- **Solution:** Create `elisp-string`, `elisp-optional` helpers

### C5. Response Construction (MEDIUM)
- **Pattern:** Manual response construction ignoring existing helpers
- **Files:** memory/* modules
- **Solution:** Use `mcp-json`, `mcp-success` from tools/core.clj

### C6. Validator Functions (MEDIUM)
- **Pattern:** All validators follow identical structure
- **File:** validation.clj:13-129
- **Solution:** Create `build-validator` factory function

---

## PRIORITY MATRIX

| ID | Violation | Severity | Effort | Dependencies |
|----|-----------|----------|--------|--------------|
| C1 | Spec duplication | CRITICAL | Very Low | None |
| B1 | Elisp eval error | CRITICAL | Low | None |
| B2 | Addon availability | HIGH | Low | B1 |
| C4 | Elisp string escaping | HIGH | Low | None |
| A4 | JSON response parsing | HIGH | Low | None |
| A8 | String assertions | HIGH | Low | None |
| B6 | Memory response format | HIGH | Very Low | None |
| A1 | Fixture boilerplate | MEDIUM | Low | None |
| B3 | Elisp builder | MEDIUM | Medium | C4 |
| B4/C3 | Validation wrapper | MEDIUM | Low | None |
| A7 | with-redefs | MEDIUM | Low | A1 |
| C6 | Validator factory | MEDIUM | Medium | None |
| A2 | Mock factories | MEDIUM | Medium | A1 |
| C2 | Agent ID extraction | MEDIUM | Very Low | None |
| A6 | Thread.sleep | LOW | Very Low | None |
| A3 | Elisp helpers | LOW | Low | C4 |
| A5 | Test data derive | LOW | Very Low | None |
| B5 | Logging | LOW | Very Low | None |
| B7 | Param destructure | LOW | High | Future |

---

## PARALLELIZABLE REFACTOR PLAN

### Track 1: Foundation Macros (SEQUENTIAL - must be first)
```
1.1 [C1] Fix spec duplication in server.clj (5 min)
1.2 [B1] Create call-elisp-safe macro in tools/core.clj (30 min)
1.3 [C4] Create elisp-string/elisp-optional in elisp.clj (20 min)
```

### Track 2: Test Infrastructure (PARALLEL with Track 1 after step 1.1)
```
2.1 [A1] Create test/hive_mcp/test_utils.clj with fixture helpers
2.2 [A4] Create test/hive_mcp/test_assertions.clj with JSON/elisp assertions
2.3 [A6] Create test/hive_mcp/test_timing.clj with constants
2.4 [A8] Add assert-elisp-* functions to test_assertions.clj
```

### Track 3: Handler Consumers (DEPENDS ON Track 1 completion)
```
3.1 [B2] Create with-addon macro, refactor 7 addon files
3.2 [B6] Replace raw responses in memory/* with mcp-json
3.3 [B3] Refactor elisp builders to use new helpers
3.4 [B4/C3] Create with-validation, refactor buffer/cider/diff
```

### Track 4: Test Consumers (DEPENDS ON Track 2 completion)
```
4.1 [A7] Create with-default-swarm-mocks, update swarm tests
4.2 [A2] Create test_mocks.clj, refactor mock factories
4.3 Update all tests to use new assertion helpers
```

### Track 5: Cleanup (FINAL - after all tracks complete)
```
5.1 [C2] Extract agent-id helper
5.2 [C6] Create build-validator factory (optional)
5.3 [A5] Derive test data sets from base definitions
5.4 Run kondo, fix any new warnings
```

---

## DEPENDENCY GRAPH

```
                    [C1: Fix Specs]
                          │
          ┌───────────────┼───────────────┐
          │               │               │
    [Track 2: Tests]  [B1: elisp-safe]  [C4: elisp-string]
          │               │               │
          │               └───────┬───────┘
          │                       │
          │               [Track 3: Handlers]
          │                       │
    [Track 4: Test              │
     Consumers]                  │
          │                       │
          └───────────┬───────────┘
                      │
              [Track 5: Cleanup]
```

---

## FILES TO CREATE

### New Test Infrastructure
```
test/hive_mcp/
├── test_assertions.clj    # JSON/MCP/elisp assertions
├── test_utils.clj         # Fixture factories
├── test_timing.clj        # Timing constants
└── test_mocks.clj         # Mock builders
```

### Modified Production Code
```
src/hive_mcp/
├── server.clj             # Remove duplicate specs
├── tools/core.clj         # +call-elisp-safe, +with-addon
├── elisp.clj              # +elisp-string, +elisp-optional
└── validation.clj         # +with-validation, +build-validator
```

---

## ESTIMATED EFFORT

| Track | Tasks | Effort | Parallelizable |
|-------|-------|--------|----------------|
| 1 | Foundation | 1 hour | No (sequential) |
| 2 | Test Infrastructure | 2 hours | Yes (after 1.1) |
| 3 | Handler Consumers | 3 hours | Yes (after Track 1) |
| 4 | Test Consumers | 2 hours | Yes (after Track 2) |
| 5 | Cleanup | 1 hour | No (final) |

**Total sequential time:** ~9 hours
**With parallelization:** ~5 hours (Tracks 2-4 can run concurrently)
