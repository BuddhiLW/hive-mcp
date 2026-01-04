# Swarm Conflict Detection Architecture

A logic-programming approach to coordinating parallel AI agents.

## The Problem

When multiple AI agents (Claude instances) work in parallel on a codebase, conflicts arise:

1. **File Collisions** — Two agents editing the same file simultaneously
2. **Dependency Violations** — Agent B starts before Agent A completes a prerequisite
3. **Circular Deadlocks** — A waits for B, B waits for C, C waits for A

Traditional solutions (mutexes, locks) don't work well because:
- Agents are separate processes, not threads
- We want **queuing**, not blocking
- We need **relational reasoning** about dependencies

## The Solution: Logic-Based Coordination

```
┌─────────────────────────────────────────────────────────────────┐
│                     SWARM COORDINATOR                           │
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐        │
│  │  Pre-flight │───▶│   Logic DB  │◀───│    Sync     │        │
│  │   Check     │    │ (core.logic)│    │  (Events)   │        │
│  └─────────────┘    └─────────────┘    └─────────────┘        │
│         │                  │                  ▲                │
│         ▼                  ▼                  │                │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐        │
│  │  Dispatch   │    │   Claims    │    │  Channel    │        │
│  │  or Queue   │    │  Registry   │    │  (Events)   │        │
│  └─────────────┘    └─────────────┘    └─────────────┘        │
│                                               ▲                │
└───────────────────────────────────────────────│────────────────┘
                                                │
        ┌───────────────────────────────────────┼───────────────┐
        │                                       │               │
   ┌────▼────┐    ┌─────────┐    ┌─────────┐   │               │
   │  Alice  │    │   Bob   │    │  Carol  │   │  (Swarm)      │
   │ (agent) │    │ (agent) │    │ (agent) │   │               │
   └─────────┘    └─────────┘    └─────────┘                   │
        │              │              │                         │
        └──────────────┴──────────────┴─────────────────────────┘
                    Events: started, completed, error
```

## Core Components

### 1. Logic Database (`logic.clj`)

Uses [core.logic](https://github.com/clojure/core.logic) (miniKanren) for relational reasoning:

```clojure
;; Facts stored as relations
(pldb/db-rel slave slave-id status)      ; Agent registry
(pldb/db-rel claims file-path slave-id)  ; File ownership
(pldb/db-rel task task-id slave-id status)
(pldb/db-rel depends-on task-id dep-task-id)

;; Logic predicates for conflict detection
(defn file-conflicto [file-path requesting-slave conflicting-slave]
  (l/all
    (claims file-path conflicting-slave)
    (l/!= requesting-slave conflicting-slave)))

;; Transitive reachability for deadlock detection
(defn reachable-fromo [source target]
  (l/conde
    [(depends-on source target)]
    [(l/fresh [mid]
       (depends-on source mid)
       (reachable-fromo mid target))]))
```

**Why core.logic?**
- Declarative rules, not imperative code
- Composable predicates
- Natural fit for relational data (agents, tasks, files, dependencies)
- Backtracking finds all solutions (all conflicts, not just first)

### 2. Pre-flight Check (`coordinator.clj`)

Before any task dispatch, validates:

```clojure
(defn pre-flight-check [{:keys [slave-id files prompt dependencies]}]
  (let [;; Extract files from prompt if not explicit
        effective-files (or files (extract-files-from-prompt prompt))
        
        ;; Check file conflicts via logic predicates
        file-conflicts (logic/check-file-conflicts slave-id effective-files)
        
        ;; Check circular dependencies
        deadlock-pairs (filter logic/check-would-deadlock dependencies)]
    
    {:approved? (and (empty? file-conflicts) (empty? deadlock-pairs))
     :conflicts file-conflicts
     :queue? (seq file-conflicts)
     :would-deadlock deadlock-pairs}))
```

### 3. Dispatch or Queue (`coordinator.clj`)

Three possible outcomes:

| Outcome | Condition | Action |
|---------|-----------|--------|
| `:dispatch` | No conflicts | Proceed immediately |
| `:queued` | File conflict | Store in queue, retry when conflict clears |
| `:blocked` | Circular dependency | Cannot proceed, alert human |

```clojure
(defn dispatch-or-queue! [task-spec]
  (let [check (pre-flight-check task-spec)]
    (cond
      (seq (:would-deadlock check)) {:action :blocked}
      (:queue? check)               {:action :queued, :task-id (queue-task! task-spec)}
      :else                         {:action :dispatch})))
```

### 4. Event Sync (`sync.clj`)

Bridges the event bus to the logic database:

| Event | Logic Update |
|-------|--------------|
| `:slave-spawned` | `(add-slave! id :idle)` |
| `:task-dispatched` | `(add-task!)`, `(add-claim!)` for each file |
| `:task-completed` | `(release-claims!)`, `(process-queue!)` |
| `:task-failed` | `(release-claims!)` |

This keeps the logic database in sync with actual swarm state.

## The Concurrency Pattern

**What architectural pattern does this "claim"-based system follow?**

| Pattern | How it applies |
|---------|----------------|
| **Pessimistic Locking** | Assumes conflicts *will* happen, blocks *before* the operation rather than detecting conflicts after |
| **Advisory Locks** | Like Unix `flock()` — the lock is cooperative, agents *could* ignore it but choose to respect it |
| **Lease/Claim pattern** | Agent acquires a "lease" on files, releases when done — similar to distributed lease coordination |
| **Pre-flight Check** | Validates constraints before dispatch (like airline check-in before boarding) |

**Key difference from mutex:** A mutex is kernel-enforced and blocking. This is:

- **Coordinator-mediated** — logic database tracks claims, not kernel
- **Non-blocking** — conflicting tasks get *queued*, not blocked
- **Relational** — uses core.logic to reason about claims, dependencies, cycles

The queue-on-conflict behavior is closer to **Optimistic Scheduling with Pessimistic Detection**:
- *Optimistic* that the conflict will clear (task gets queued for retry)
- *Pessimistic* in preventing simultaneous access

### Lease Lifecycle

| Phase | Behavior |
|-------|----------|
| **Acquire** | Agent claims files before editing |
| **Hold** | Conflicting requests get queued, not blocked |
| **Release** | Task completes/fails → claims released → queue processed |

No timeout-based expiry — events drive the lifecycle.

```
Timeline:

T0: Alice claims core.clj
T1: Bob tries core.clj → QUEUED
T2: Carol tries core.clj → QUEUED
T3: Alice completes → releases claim
T4: Bob and Carol both ready (first-come-first-serve or priority)
```

## Emergent Property: Rewards Good Architecture

The system naturally incentivizes SOLID principles:

| Practice | Effect |
|----------|--------|
| **Single Responsibility** | Small files = fewer collisions |
| **Interface Segregation** | Isolated concerns = parallel work |
| **Dependency Inversion** | Clean boundaries = clear task ownership |

> "Parallel agent work scales with code modularity"

Codebases with large god-files bottleneck the swarm. Well-factored code parallelizes naturally.

## File Extraction Heuristics

When explicit file list not provided, extracts from prompt:

```clojure
(def file-patterns
  [#"(?:^|\s)(/[^\s]+\.[a-z]+)"           ; /absolute/path.ext
   #"(?:^|\s)(src/[^\s]+)"                 ; src/relative/path
   #"(?:^|\s)(test/[^\s]+)"                ; test/relative/path
   #"`([^`]+\.[a-z]+)`"                    ; `backtick-quoted.ext`
   #"\"([^\"]+\.[a-z]+)\""                 ; "quoted-path.ext"
   #"file[:\s]+([^\s,]+\.[a-z]+)"          ; file: path.ext
   #"edit[ing]*[:\s]+([^\s,]+\.[a-z]+)"]) ; editing path.ext
```

This allows natural language task prompts:
```
"Please refactor src/core.clj to extract the validation logic"
→ Extracts: ["src/core.clj"]
```

## Test Coverage

The `coordinator_test.clj` covers:

1. **Two agents same file** — Conflict detected, second queued
2. **Different files** — No conflict, both proceed
3. **Claim release** — Queued task becomes ready
4. **Partial conflicts** — Some files conflict, task still queued
5. **Three-agent chains** — Multiple waiters, all unblock together
6. **Circular dependencies** — Detected and blocked
7. **File extraction** — Prompts parsed correctly

```
Ran 9 tests containing 28 assertions.
0 failures, 0 errors.
```

## Integration with Hivemind

The conflict detection integrates with the broader hivemind system:

```clojure
;; Agent reports progress
(hivemind_shout :started {:task "Refactor auth" :files ["src/auth.clj"]})

;; Coordinator registers claims
(register-task-claims! task-id slave-id files)

;; Another agent tries same file
(hivemind_shout :started {:task "Fix auth bug" :files ["src/auth.clj"]})
;; → Pre-flight fails, task queued

;; First agent completes
(hivemind_shout :completed {:task "Refactor auth"})
;; → Claims released, queued task becomes ready
```

## Future Directions

### Priority-Based Queue Processing
Currently FIFO. Could add priority based on:
- Task urgency
- Agent reputation (completion rate)
- Estimated duration

### Predictive Conflict Detection
Use semantic analysis to predict file touches before explicit declaration.

### Partial File Locking
Lock specific functions/regions within files, not whole files.
(Requires AST-level coordination)

### Distributed Coordination
Current design assumes single coordinator. For multi-machine swarms,
could use distributed consensus (Raft, CRDT).

## References

- [core.logic](https://github.com/clojure/core.logic) — Logic programming for Clojure
- [miniKanren](http://minikanren.org/) — The underlying logic system
- [Pessimistic Concurrency Control](https://en.wikipedia.org/wiki/Lock_(computer_science)) — Database theory
- [Advisory Locking](https://www.gnu.org/software/libc/manual/html_node/File-Locks.html) — Unix file locks

## Authors

Architecture designed through human-AI collaboration:
- Human coordinator: System design, requirements, testing
- Claude instances: Implementation, documentation, TDD

---

*"The best way to coordinate AI agents is to give them a shared understanding of the world — and let logic do the rest."*
