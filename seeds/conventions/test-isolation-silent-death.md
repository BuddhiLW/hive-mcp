---
type: convention
tags:
  - tdd
  - testing
  - isolation
  - silent-failure
  - server-state
  - critical
  - catchup-priority
duration: permanent
---

# Convention: Test Isolation - Silent Server Death

## The Specific Horror

Running tests would **silently kill MCP server core states**. The server would continue running but be **internally broken** - no error, no crash, just corrupted state that manifests as mysterious failures later.

## Why It's Silent

```clojure
;; Test runs, "passes"
(deftest some-test
  (reset! core/conn nil)  ; BOOM - killed the DataScript connection
  (is (= 1 1)))           ; Test passes! No indication of damage.

;; Later, in production...
(d/q '[:find ?e ...] @core/conn)  ; NPE or weird behavior
;; "Why is the server broken? All tests passed!"
```

## The Trap

1. Test suite runs ✓
2. All tests pass ✓
3. Server is now silently corrupted ✗
4. Next MCP call fails mysteriously
5. Hours of debugging "what changed?"

## Critical hive-mcp State

These atoms, if touched by tests without isolation, **break the running server**:

| Atom | Effect if Corrupted |
|------|---------------------|
| `core/conn` | All DataScript queries fail |
| `swarm/conn` | Ling tracking breaks |
| `channel/*ws-clients*` | WebSocket broadcast fails |
| `hivemind/*agents*` | Agent status lost |
| `claims/*file-claims*` | Drone file locking breaks |

## Prevention Patterns

### 1. Fixture-based Snapshot/Restore
```clojure
(use-fixtures :each
  (fn [f]
    (let [saved-conn @core/conn
          saved-swarm @swarm/conn]
      (try
        (f)
        (finally
          (reset! core/conn saved-conn)
          (reset! swarm/conn saved-swarm))))))
```

### 2. Test-specific Binding (Preferred)
```clojure
(deftest isolated-test
  (with-redefs [core/conn (atom (d/empty-db schema))]
    (is (= ... ))))
```

### 3. Never Touch Production Atoms (Best)
```clojure
;; Design functions to accept connections as parameters
(defn query-data [conn query]  ; Testable with test-only conn
  (d/q query @conn))
```

## Lesson Learned

Tests passing but server silently broken. Hours of debugging. Root cause: test modified production atom without restoration. The fix: mandatory isolation fixtures in all test namespaces that touch state.
