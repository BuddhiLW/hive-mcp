# Integration Gaps Analysis: Memory, Channel, Hivemind, and core.logic

**Status:** Active Gap Analysis  
**Date:** 2026-01-03  
**Author:** Claude (Opus 4.5)

## Executive Summary

The emacs-mcp project has three sophisticated subsystems that are currently **sparse** rather than integrated:

1. **Memory System** (Chroma vector DB) - Persistent knowledge storage
2. **Bidirectional Channel** (TCP port 9998) - Push-based Clojure↔Emacs communication  
3. **Hivemind/core.logic** - Swarm coordination with conflict detection

These systems were designed to work together but several critical connections are missing.

---

## Architecture Overview

### Current State: Disconnected Islands

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        CURRENT STATE: DISCONNECTED                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌─────────────┐         ┌─────────────┐         ┌─────────────┐          │
│   │   MEMORY    │         │   CHANNEL   │         │  HIVEMIND   │          │
│   │  (Chroma)   │         │   (TCP)     │         │  (Clojure)  │          │
│   └──────┬──────┘         └──────┬──────┘         └──────┬──────┘          │
│          │                       │                       │                  │
│          ▼                       ▼                       ▼                  │
│   ┌─────────────┐         ┌─────────────┐         ┌─────────────┐          │
│   │  hive.clj   │         │  sync.clj   │         │ agent_reg   │          │
│   │ (injection  │         │ (NEVER      │         │ (separate   │          │
│   │  only)      │         │  STARTED!)  │         │  state)     │          │
│   └──────┬──────┘         └──────┬──────┘         └──────┬──────┘          │
│          │                       ╳                       │                  │
│          ▼                       ╳ BROKEN                ╳ NO LINK          │
│   ┌─────────────┐         ╳──────┴──────╳         ╳──────┴──────╳          │
│   │ coordinator │◄────────╳  logic.clj  ╳─────────╳ coordinator ╳          │
│   │  (uses it)  │         │ (core.logic)│         │ (not used)  │          │
│   └─────────────┘         └─────────────┘         └─────────────┘          │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Desired State: Unified Hivemind

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        DESIRED STATE: UNIFIED                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────────┐      │
│   │                    UNIFIED HIVEMIND                              │      │
│   │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐            │      │
│   │  │ Memory  │──│ Channel │──│  Logic  │──│ Agents  │            │      │
│   │  │ (read/  │  │ (push)  │  │(conflicts│  │(registry│            │      │
│   │  │ write)  │  │         │  │ detect) │  │ status) │            │      │
│   │  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘            │      │
│   │       │            │            │            │                   │      │
│   │       └────────────┴────────────┴────────────┘                   │      │
│   │                         │                                        │      │
│   │                    sync.clj (event loop)                         │      │
│   └─────────────────────────┬───────────────────────────────────────┘      │
│                             │                                               │
│                     Bidirectional Channel                                   │
│                             │                                               │
│                      ┌──────┴──────┐                                       │
│                      │    EMACS    │                                       │
│                      │ (swarm UI)  │                                       │
│                      └─────────────┘                                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Gap #1: sync.clj Never Started (CRITICAL)

### The Problem

The `sync.clj` module exists with complete implementation but is **never started** in `server.clj`.

**File:** `src/emacs_mcp/swarm/sync.clj`

### What sync.clj Does (When Started)

1. Subscribes to channel events:
   - `:slave-spawned` → Registers slave in logic DB
   - `:slave-killed` → Removes slave, releases claims
   - `:task-dispatched` → Registers task and file claims
   - `:task-completed` → Marks complete, releases claims, processes queue
   - `:task-failed` → Marks failed, releases claims

2. Bootstraps from Emacs state on startup
3. Keeps logic DB in sync for accurate conflict detection

### Current Behavior

```clojure
;; src/emacs_mcp/server.clj - sync is NOT required or started!
(ns emacs-mcp.server
  (:require [emacs-mcp.channel :as channel]
            [emacs-mcp.hivemind :as hivemind]
            ;; Missing: [emacs-mcp.swarm.sync :as sync]
            ...))

(defn start! [& _args]
  ...
  (channel/start-server! ...)  ;; Channel starts
  ;; Missing: (sync/start-sync!)
  ...)
```

### Impact

- Pre-flight conflict checks work initially
- File claims are registered on dispatch
- **Claims are NEVER released** (task-completed doesn't update logic DB)
- Queue never processes (conflicts never clear)
- Logic DB becomes stale after first dispatch

### Fix

```clojure
;; In server.clj requires:
[emacs-mcp.swarm.sync :as sync]

;; In start! function, after channel:
(try
  (sync/start-sync!)
  (log/info "Swarm sync started")
  (catch Exception e
    (log/warn "Swarm sync failed (non-fatal):" (.getMessage e))))
```

---

## Gap #2: Hivemind ↔ Coordinator Disconnect (MAJOR)

### The Problem

Hivemind (`hivemind.clj`) and the coordinator/logic system (`coordinator.clj`, `logic.clj`) maintain **separate state** and don't communicate.

### Hivemind's State

```clojure
;; hivemind.clj
(defonce agent-registry (atom {}))   ;; Tracks agent status
(defonce pending-asks (atom {}))     ;; Tracks human-in-loop questions
```

### Logic's State

```clojure
;; logic.clj  
(defonce logic-db (atom (pldb/db)))  ;; Tracks slaves, tasks, file claims
```

### The Disconnect

| Event | Hivemind Does | Logic/Coordinator Does |
|-------|---------------|------------------------|
| `hivemind_shout :started` | Updates agent-registry | Nothing |
| `hivemind_shout :completed` | Updates agent-registry | Nothing |
| `hivemind_ask` | Creates pending-ask | Nothing |
| `swarm_dispatch` | Nothing | Checks conflicts, registers claims |

### Impact

- Agents using `hivemind_shout` don't trigger file claim releases
- Logic DB doesn't know about hivemind agent state
- Two sources of truth = inconsistent state

### Fix Options

**Option A: Hivemind uses coordinator**
```clojure
;; In hivemind.clj shout! function:
(when (= event-type :completed)
  (when-let [task-id (:task-id data)]
    (coord/release-task-claims! task-id)))
```

**Option B: sync.clj subscribes to hivemind events**
```clojure
;; In sync.clj event-handlers:
{:hivemind-completed handle-hivemind-completed
 :hivemind-started handle-hivemind-started}
```

---

## Gap #3: Memory → Swarm is One-Way (MINOR)

### Current Flow

```
Memory (Chroma)
    │
    │ READ ONLY (via hive.clj)
    ▼
Swarm Dispatch (inject_hive=true)
    │
    │ NO WRITEBACK
    ╳
Memory (findings lost)
```

### What hive.clj Does

- Queries memory for conventions, decisions, notes
- Formats as context for prompt injection
- Used when `inject_hive: true` in swarm_dispatch

### What's Missing

- Agents don't automatically write findings back to memory
- No claim coordination via memory (per `swarm-memory-coordination.md` RFC)
- Session knowledge doesn't persist without manual intervention

### Impact

- Each swarm session starts without previous session's discoveries
- Duplicate work across sessions
- Knowledge accumulation requires manual memory_add calls

### Fix (Future Enhancement)

```clojure
;; Auto-capture agent findings on task completion
(defn capture-agent-findings! [agent-id task-result]
  (when-let [findings (extract-findings task-result)]
    (doseq [finding findings]
      (memory/add! :note finding 
                   {:tags ["agent-finding" 
                           (str "agent:" agent-id)
                           "scope:project:emacs-mcp"]}))))
```

---

## Summary Table

| Component | Status | Issue | Priority | Fix Effort |
|-----------|--------|-------|----------|------------|
| sync.clj | ✗ Dead | Never started in server.clj | CRITICAL | 5 min |
| hivemind↔logic | ⚠️ Isolated | Separate state, no sync | HIGH | 2 hours |
| memory→swarm | ⚠️ One-way | Read-only, no writeback | MEDIUM | 4 hours |
| channel | ✓ Working | Started correctly | - | - |
| coordinator | ✓ Partial | Used by dispatch, not by hivemind | HIGH | 1 hour |

---

## Related Files

- `src/emacs_mcp/server.clj` - Server startup (needs sync)
- `src/emacs_mcp/swarm/sync.clj` - Event synchronization (dead code)
- `src/emacs_mcp/swarm/logic.clj` - core.logic conflict detection
- `src/emacs_mcp/swarm/coordinator.clj` - High-level coordination API
- `src/emacs_mcp/hivemind.clj` - Agent communication tools
- `src/emacs_mcp/swarm/hive.clj` - Memory injection for swarm
- `src/emacs_mcp/channel.clj` - Bidirectional communication

---

## References

- Decision: [Separate Bidirectional Channel](memory:20260103005910-3763982b)
- Decision: [core.logic for Swarm Coordination](memory:20260102192747-9e7d07d5)
- RFC: [Swarm Memory Coordination Pattern](docs/swarm-memory-coordination.md)
