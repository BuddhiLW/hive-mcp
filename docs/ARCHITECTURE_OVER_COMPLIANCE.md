# Architecture Over Compliance

> Instead of hoping LLMs follow instructions, we build systems that work regardless of compliance.

## The Problem

LLM agents (lings, drones) are probabilistic. They:
- Sometimes forget to shout progress
- May not follow preset instructions precisely
- Can duplicate work without coordination
- Occasionally miss cleanup steps

**Traditional approach**: Write better prompts, add more instructions, hope for compliance.

**hive-mcp approach**: Build architecture that guarantees outcomes.

## The Solution: Layers of Guarantees

### 1. Hooks System (`hooks.clj`)
Instead of hoping lings shout on task completion:
```clojure
;; Hook triggers automatically on :task-complete event
(def builtin-handlers
  {:task-complete [shout-completion commit-if-files-modified]
   :session-end   [run-wrap sync-kanban]
   :error-occurred [shout-error]})
```
**Guarantee**: Task completion always broadcasts, regardless of ling behavior.

### 2. DataScript State (`swarm/datascript.clj`)
Instead of trusting lings to track their own state:
```clojure
;; Single source of truth with transactional semantics
(ds/add-slave! slave-id {:status :idle :depth 1})
(ds/complete-task! task-id)  ;; Atomically updates status + releases claims
```
**Guarantee**: State is consistent, queryable, and survives ling failures.

### 3. File Claims & Conflict Detection
Instead of hoping lings don't edit the same file:
```clojure
(ds/has-conflict? "/src/core.clj" "ling-2")
;; => {:conflict? true, :held-by "ling-1"}
```
**Guarantee**: Conflicts detected before dispatch, not after damage.

### 4. Behavioral Broadcasting
Instead of hoping lings check for updates:
```
;; Hivemind broadcasts to all working lings:
"Warning: ling-1 completed task X which modified /src/core.clj. 
 Don't redo this work. Run tests. Trust as done if green."
```
**Guarantee**: Lings receive coordination signals regardless of their polling behavior.

### 5. TDD as Arbiter
Instead of trusting ling's judgment on completion:
```
;; Tests determine done-ness, not ling self-assessment
clojure -M:dev:test -n hive-mcp.swarm.datascript-test
;; 63 tests, 0 failures = actually done
```
**Guarantee**: Objective completion criteria.

## Design Principles

### Don't Fight Probability - Design Around It

| Compliance Approach | Architecture Approach |
|---------------------|----------------------|
| "Always shout progress" in preset | Hooks auto-shout on events |
| "Don't edit files others are working on" | Claims prevent dispatch |
| "Check if work was already done" | Broadcast notifies all lings |
| "Report when task is complete" | DataScript tracks state |

### The 4-Layer Convergence Pattern

For any critical behavior, apply multiple layers that **converge** to deterministic outcomes.

**Philosophy:** Multiple probabilistic mechanisms converge to guarantee outcomes.
- Layer 1 works ~70% → great
- Layer 2 adds ~20% → now 90%
- Layer 3 adds ~5% → now 95%
- Layer 4 guarantees the rest → 100%

Example: **Guaranteed Completion Detection**

| Layer | Mechanism | Reliability | Implementation |
|-------|-----------|-------------|----------------|
| **Layer 1** | Instruct the ling | ~70% | Preset says "shout on completion" |
| **Layer 2** | Terminal introspection | ~90% | Detect idle state + no recent activity |
| **Layer 3** | Wrapper injection | ~95% | Append "...then shout completion" to dispatch prompt |
| **Layer 4** | Hook enforcement | 100% | `:task-complete` hook triggers synthetic shout |

**Layer 1: Instruction (Probabilistic)**
```markdown
# In ling preset:
"After completing any task, ALWAYS call hivemind_shout with event_type 'completed'"
```
Works when ling follows instructions. Fails silently when forgotten.

**Layer 2: Detection (Heuristic)**
```clojure
;; Terminal introspection - detect completion by behavior
(defn detect-idle? [buffer]
  (and (no-output-for-seconds buffer 30)
       (prompt-visible? buffer)
       (no-pending-tool-calls? buffer)))
```
Detects completion even if ling forgot to shout. May have false positives.

**Layer 3: Injection (Structural)**
```clojure
;; swarm_dispatch wraps every prompt
(defn dispatch-with-shout [slave-id prompt]
  (let [wrapped (str prompt "\n\nAfter completing this task, call hivemind_shout.")]
    (send-to-slave slave-id wrapped)))
```
Adds instruction at dispatch time. Still depends on compliance but increases surface area.

**Layer 4: Enforcement (Architectural)**
```clojure
;; hooks/handlers.clj - triggers on channel event, not ling action
(def builtin-handlers
  {:task-complete [shout-completion]})

;; sync.clj - fires hook when task-completed event received
(defn handle-task-completed [event]
  (ds/complete-task! (:task-id event))
  (hooks/trigger-hooks :task-complete event))  ;; <-- Guaranteed shout
```
System enforces behavior regardless of ling compliance. **This is the guarantee.**

### Convergence in Action

The layers converge to deterministic outcomes:
- If Layer 1 works → great, ling shouted
- If Layer 1 fails, Layer 2 detects → system shouts
- If Layer 2 misses, Layer 3 retries → ling prompted again
- If all else fails, Layer 4 enforces → hook guarantees outcome

**Cost of each layer:**
| Layer | Complexity | Token Cost | Reliability Gain |
|-------|------------|------------|------------------|
| 1 | Low | 0 | +70% |
| 2 | Medium | ~100/check | +20% |
| 3 | Low | ~50/dispatch | +5% |
| 4 | Medium | 0 | +5% (to 100%) |

Layer 4 is the only one that provides a **guarantee**. Layers 1-3 are optimizations that converge probability toward 100%, reducing load on Layer 4.

### Fail-Safe Defaults

```clojure
;; Claims auto-release on task completion
(ds/complete-task! task-id)  ;; Releases all claims automatically

;; Orphan cleanup on session end  
:session-end [run-wrap sync-kanban]  ;; Always runs, even if ling crashed
```

## Implementation Status

### Core Infrastructure
| Component | File | Status |
|-----------|------|--------|
| Hooks system | `hooks.clj`, `handlers.clj` | ✅ Complete |
| DataScript state | `swarm/datascript.clj` | ✅ Complete |
| File claims | `ds/has-conflict?` | ✅ Complete |
| Sync handlers | `sync.clj` | ✅ Complete |

### 4-Layer Completion Detection
| Layer | Component | Status |
|-------|-----------|--------|
| Layer 1 | Preset instructions | ✅ Complete (in presets/*.md) |
| Layer 2 | Terminal introspection | ✅ Complete (hive-mcp-swarm-terminal.el) |
| Layer 3 | Dispatch wrapper injection | ✅ Complete (dispatch.clj) |
| Layer 4 | Hook enforcement | ✅ Complete (sync.clj + handlers.clj) |

### Coordination Features
| Feature | Status |
|---------|--------|
| Behavioral broadcast (file overlap warnings) | ⏳ Designed, pending |
| Prompt interception (stuck ling detection) | ⏳ Feature idea documented |

## The Fractal Insight

Each architectural layer we add reveals new edge cases. But these refinements compound:

- Hooks enabled guaranteed completion detection
- DataScript enabled consistent state queries  
- Claims enabled safe parallel work
- Together they enable **trustworthy multi-agent coordination**

This is the direction toward next-generation LLM systems: **architecture that assumes imperfection and guarantees outcomes anyway**.

---

*"Trust the swarm, but verify through architecture."*
