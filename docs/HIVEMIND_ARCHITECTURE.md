# Hivemind Architecture: AI Swarm Coordination System

**Version:** 1.0.0
**Status:** Production
**Authors:** Claude + BuddhiLW (Pedro G. Branquinho)

## Executive Summary

The Hivemind is an **event-driven coordination system** for orchestrating multiple Claude AI agents ("lings") working in parallel. It enables:

- **Real-time communication** between agents and a human coordinator
- **Conflict-free parallel execution** via logic programming
- **Human-in-the-loop decisions** with sub-100ms notification latency
- **Collective memory** shared across all agents

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           HUMAN COORDINATOR                                  │
│                     (You, using Master Claude)                               │
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │ hivemind_respond
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              HIVEMIND                                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │   Channel   │  │  Registry   │  │   Logic     │  │   Memory    │        │
│  │  (TCP 9998) │  │  (Agents)   │  │ (core.logic)│  │  (Chroma)   │        │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘        │
└─────────┼────────────────┼────────────────┼────────────────┼────────────────┘
          │                │                │                │
          │  hivemind_shout│                │                │
          ▼                ▼                ▼                ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           SWARM AGENTS (Lings)                               │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐           │
│  │ Ling 1  │  │ Ling 2  │  │ Ling 3  │  │ Ling 4  │  │ Ling N  │           │
│  │(vterm)  │  │(vterm)  │  │(vterm)  │  │(vterm)  │  │(vterm)  │           │
│  │ claude  │  │ claude  │  │ claude  │  │ claude  │  │ claude  │           │
│  └─────────┘  └─────────┘  └─────────┘  └─────────┘  └─────────┘           │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [The Three Layers](#the-three-layers)
3. [Hivemind Communication Protocol](#hivemind-communication-protocol)
4. [Swarm Orchestration](#swarm-orchestration)
5. [Logic Programming Engine](#logic-programming-engine)
6. [Bidirectional Channel](#bidirectional-channel)
7. [Memory Integration](#memory-integration)
8. [Complete Workflow Example](#complete-workflow-example)
9. [MCP Tools Reference](#mcp-tools-reference)
10. [Safety & Constraints](#safety--constraints)

---

## Core Concepts

### What is a "Ling"?

A **ling** (short for "underling" or inspired by Starcraft's Zerglings) is a Claude Code instance running in an Emacs vterm buffer. Each ling:

- Runs the `claude` CLI with preset system prompts
- Has access to MCP tools (file operations, git, memory, etc.)
- Communicates with the hivemind via MCP tools
- Can be spawned, tasked, and killed by the coordinator

### What is the Hivemind?

The **hivemind** is the collective coordination layer that:

- Tracks all active agents and their status
- Routes messages between agents and coordinator
- Detects and prevents file conflicts
- Manages the ask/respond decision queue
- Maintains collective memory

### Key Identifiers

| ID Type | Format | Example |
|---------|--------|---------|
| `slave_id` | `swarm-{name}-{timestamp}` | `swarm-tester-1767461382` |
| `task_id` | `task-{name}-{sequence}` | `task-tester-001` |
| `agent_id` | User-defined string | `hivemind-test` |
| `ask_id` | UUID | `7ec922f5-c45d-4812-a781-02d1ff1cc57c` |
| `session_id` | `session-{date}-{random}` | `session-20260103-a893` |

---

## The Three Layers

### Layer 1: Emacs (Elisp) - User Interface & Terminal Management

```elisp
;; emacs-mcp-swarm.el - Terminal orchestration
(emacs-mcp-swarm-spawn "worker" :presets '("hivemind" "tdd"))
(emacs-mcp-swarm-dispatch slave-id "Run all tests")
(emacs-mcp-swarm-collect task-id)
```

**Responsibilities:**
- Spawn/kill vterm buffers running `claude`
- Send prompts to slaves via `vterm-send-string`
- Collect responses by parsing buffer content
- Handle permission prompts (auto-approve, human mode, or bypass)
- Emit events to channel for push notifications

### Layer 2: Clojure (MCP Server) - Coordination Logic

```clojure
;; hivemind.clj - Agent coordination
(shout! "agent-1" :progress {:task "Testing" :percent 50})
(ask! "agent-1" "Deploy to prod?" ["yes" "no" "wait"])
(respond-ask! ask-id "yes" :by "human")
```

**Responsibilities:**
- Register agents and track status
- Manage ask/respond queue with blocking semantics
- Detect conflicts via core.logic predicates
- Broadcast events via channel
- Provide MCP tools to agents

### Layer 3: MCP Protocol - Tool Interface

```json
{
  "name": "hivemind_shout",
  "inputSchema": {
    "agent_id": "string",
    "event_type": "started|progress|completed|error|blocked",
    "message": "string",
    "task": "string",
    "data": "object"
  }
}
```

**Responsibilities:**
- Expose coordination as MCP tools
- Serialize/deserialize between JSON and Clojure
- Handle timeouts and errors gracefully

---

## Hivemind Communication Protocol

### Event Types

Agents communicate with the hivemind using **shout** events:

| Event Type | When to Use | Data Fields |
|------------|-------------|-------------|
| `started` | Beginning a task | `task`, `message` |
| `progress` | Mid-task update | `task`, `message`, `data.percent` |
| `completed` | Task finished successfully | `task`, `result` |
| `error` | Task failed | `task`, `error` |
| `blocked` | Need input/resource | `task`, `reason` |

### Shout Flow

```
Agent                    Hivemind                    Coordinator
  │                         │                            │
  │─── hivemind_shout ─────>│                            │
  │    (event: started)     │                            │
  │                         │──── channel broadcast ────>│
  │                         │                            │
  │                         │    (agent registered)      │
  │                         │    (status: started)       │
```

### Ask/Respond Flow (Human-in-the-Loop)

```
Agent                    Hivemind                    Coordinator
  │                         │                            │
  │─── hivemind_ask ───────>│                            │
  │    (blocks agent)       │──── push notification ────>│
  │                         │    "Agent needs decision"  │
  │                         │                            │
  │        (waiting)        │                            │
  │                         │<─── hivemind_respond ──────│
  │                         │     decision: "yes"        │
  │<── response ────────────│                            │
  │    {decision: "yes",    │                            │
  │     by: "human"}        │                            │
  │                         │                            │
  │    (agent continues)    │                            │
```

### MCP Tool: `hivemind_shout`

```clojure
(hivemind_shout
  :agent_id "my-agent"
  :event_type "progress"
  :message "Processing files..."
  :task "File migration"
  :data {:percent 50 :files-done 25 :files-total 50})
```

**Returns:** `{:success true}`

### MCP Tool: `hivemind_ask`

```clojure
(hivemind_ask
  :agent_id "my-agent"
  :question "Should I delete these 50 orphaned files?"
  :options ["yes" "no" "show me first"]
  :timeout_ms 300000)  ; 5 minutes
```

**Returns:** `{:decision "yes", :by "human"}` (after human responds)

### MCP Tool: `hivemind_respond`

```clojure
(hivemind_respond
  :ask_id "7ec922f5-c45d-4812-a781-02d1ff1cc57c"
  :decision "yes")
```

**Returns:** `{:success true}`

---

## Swarm Orchestration

### Spawning Agents

```
Master Claude ──> swarm_spawn ──> emacs-mcp-swarm.el
                                         │
                                         ▼
                              ┌──────────────────┐
                              │  vterm buffer    │
                              │  ┌────────────┐  │
                              │  │  claude    │  │
                              │  │  --system  │  │
                              │  │  preset.md │  │
                              │  └────────────┘  │
                              └──────────────────┘
```

### Presets (System Prompts)

Presets are markdown files that configure agent behavior:

| Preset | Purpose |
|--------|---------|
| `hivemind` | Enable hivemind communication |
| `tdd` | Test-driven development |
| `clarity` | CLARITY framework principles |
| `mcp-first` | Use MCP tools over native tools |
| `minimal` | Bare minimum instructions |

**Preset stacking:** Agents can load multiple presets:
```elisp
(emacs-mcp-swarm-spawn "worker" :presets '("hivemind" "tdd" "clarity"))
```

### Task Dispatch

```
Master ──> swarm_dispatch ──> Slave Buffer
                                   │
                                   ▼
                            vterm-send-string
                                   │
                                   ▼
                            Claude processes
                                   │
                                   ▼
                            hivemind_shout
                            (status updates)
```

### State Machine

```
         spawn
           │
           ▼
    ┌────────────┐
    │  SPAWNING  │ (async, non-blocking)
    └─────┬──────┘
          │ buffer ready
          ▼
    ┌────────────┐
    │  STARTING  │ (claude loading)
    └─────┬──────┘
          │ claude ready
          ▼
    ┌────────────┐        dispatch
    │    IDLE    │ ◄────────────────┐
    └─────┬──────┘                  │
          │ dispatch                │ complete/fail
          ▼                         │
    ┌────────────┐                  │
    │  WORKING   │ ─────────────────┘
    └─────┬──────┘
          │ error
          ▼
    ┌────────────┐
    │   ERROR    │
    └─────┬──────┘
          │ kill
          ▼
    ┌────────────┐
    │ TERMINATED │
    └────────────┘
```

---

## Logic Programming Engine

### Why core.logic?

Parallel agents create conflict potential:
- Multiple agents editing the same file
- Circular dependencies causing deadlocks
- Race conditions on shared resources

**core.logic** (miniKanren) provides:
- Declarative conflict rules
- Pre-flight checks before dispatch
- Automatic deadlock detection

### File Conflict Detection

```clojure
;; logic.clj
(defn file-conflicto [file-path requesting-slave conflicting-slave]
  "Logic predicate: true if file-path is claimed by another slave"
  (l/fresh [task-id]
    (claims file-path conflicting-slave)
    (l/!= requesting-slave conflicting-slave)))

(defn check-file-conflicts [requesting-slave files]
  "Return list of conflicts for files"
  (with-db
    (l/run* [file conflicting-slave]
      (l/membero file files)
      (file-conflicto file requesting-slave conflicting-slave))))
```

### Coordinator Pre-Flight Check

```clojure
;; coordinator.clj
(defn pre-flight-check [{:keys [slave-id files prompt]}]
  "Check if task can proceed without conflicts"
  (let [conflicts (logic/check-file-conflicts slave-id files)]
    (if (empty? conflicts)
      {:safe? true :slave-id slave-id}
      {:safe? false
       :conflicts conflicts
       :recommendation :queue})))
```

### Task Queue for Conflicts

When conflicts are detected, tasks are queued:

```clojure
(defn dispatch-or-queue! [task-spec]
  (let [check (pre-flight-check task-spec)]
    (if (:safe? check)
      (do
        (register-task-claims! (:task-id task-spec)
                               (:slave-id task-spec)
                               (:files task-spec))
        {:dispatched true})
      (do
        (queue-task! task-spec)
        {:queued true :reason (:conflicts check)}))))
```

---

## Bidirectional Channel

### Why a Separate Channel?

nREPL is **request-response** only. For real-time coordination, we need **push notifications**:

```
Without Channel:              With Channel:

Agent ──> polls ──> ?         Agent ──> shout ──> Channel
      <── nothing             Coordinator <── push <── Channel
      (100ms+ latency)        (sub-10ms latency)
```

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Clojure MCP Server                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │               TCP Server (port 9998)                 │    │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐              │    │
│  │  │ Client1 │  │ Client2 │  │ ClientN │              │    │
│  │  └────┬────┘  └────┬────┘  └────┬────┘              │    │
│  │       │            │            │                    │    │
│  │       └────────────┴────────────┘                    │    │
│  │                    │                                 │    │
│  │               EventBus (core.async pub/sub)          │    │
│  │                    │                                 │    │
│  │       ┌────────────┴────────────┐                    │    │
│  │       ▼            ▼            ▼                    │    │
│  │  :task-completed  :prompt-shown  :state-changed      │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
                         │
                         │ TCP (bencode)
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                         Emacs                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │            emacs-mcp-channel.el                      │    │
│  │                                                      │    │
│  │  (emacs-mcp-channel-on :task-completed              │    │
│  │    (lambda (event)                                  │    │
│  │      (message "Task %s done!" (alist-get 'task-id   │    │
│  │                                           event)))) │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Event Types

| Event | Direction | Purpose |
|-------|-----------|---------|
| `task-completed` | Clojure → Emacs | Notify task finished |
| `task-failed` | Clojure → Emacs | Notify task error |
| `prompt-shown` | Emacs → Clojure | Permission prompt detected |
| `state-changed` | Clojure → Emacs | Agent state transition |
| `slave-spawned` | Emacs → Clojure | New agent created |
| `slave-killed` | Emacs → Clojure | Agent terminated |

### Usage

**Clojure (broadcast):**
```clojure
(channel/broadcast! {:type "custom-event" :data {:foo "bar"}})
```

**Emacs (subscribe):**
```elisp
(emacs-mcp-channel-on :custom-event
  (lambda (event)
    (message "Got: %s" (alist-get 'data event))))
```

---

## Memory Integration

### Collective Knowledge

All agents share access to project memory:

```
┌─────────────────────────────────────────────────────────────┐
│                    Memory System (Chroma)                    │
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │
│  │    Notes     │  │  Decisions   │  │ Conventions  │       │
│  │              │  │              │  │              │       │
│  │ Session logs │  │ Architecture │  │ Code style   │       │
│  │ Debug notes  │  │ Trade-offs   │  │ Patterns     │       │
│  └──────────────┘  └──────────────┘  └──────────────┘       │
│                                                              │
│  ┌──────────────┐  ┌──────────────┐                         │
│  │   Snippets   │  │   Presets    │                         │
│  │              │  │              │                         │
│  │ Code samples │  │ Agent system │                         │
│  │ Templates    │  │ prompts      │                         │
│  └──────────────┘  └──────────────┘                         │
└─────────────────────────────────────────────────────────────┘
                         │
                         │ inject_hive: true
                         ▼
                    Agent Context
```

### Hive Knowledge Injection

When dispatching with `inject_hive: true`:

```clojure
(swarm_dispatch
  :slave_id "swarm-worker-123"
  :prompt "Implement the new feature"
  :inject_hive true)
```

The agent receives:
1. Project conventions (code style, patterns)
2. Relevant decisions (architecture choices)
3. Recent notes (session context)

### Memory-First Preset Lookup

Presets can be stored in memory (project-scoped):

```clojure
;; Adding a custom preset to memory
(mcp_memory_add
  :type "convention"
  :content "You are a specialized agent for..."
  :tags ["swarm-preset" "my-custom-preset" "scope:project:my-project"])
```

Priority: Memory presets → File presets (`.md` files)

---

## Complete Workflow Example

### Scenario: Parallel Test + Review

```
┌─────────────────────────────────────────────────────────────┐
│                    Step 1: Spawn Agents                      │
└─────────────────────────────────────────────────────────────┘

Master Claude:
  "Spawn two agents - one for testing, one for code review"

swarm_spawn(name: "tester", presets: ["hivemind", "tdd"])
  → swarm-tester-1767461382

swarm_spawn(name: "reviewer", presets: ["hivemind", "reviewer"])
  → swarm-reviewer-1767461383

┌─────────────────────────────────────────────────────────────┐
│                  Step 2: Dispatch Tasks                      │
└─────────────────────────────────────────────────────────────┘

swarm_dispatch(
  slave_id: "swarm-tester-1767461382",
  prompt: "Run all tests in src/core_test.clj")
  → task-tester-001

swarm_dispatch(
  slave_id: "swarm-reviewer-1767461383",
  prompt: "Review src/core.clj for bugs and security issues")
  → task-reviewer-001

┌─────────────────────────────────────────────────────────────┐
│              Step 3: Agents Work (in parallel)               │
└─────────────────────────────────────────────────────────────┘

Tester Agent:                    Reviewer Agent:
  hivemind_shout(started)          hivemind_shout(started)
  Running tests...                  Reading file...
  hivemind_shout(progress: 50%)    hivemind_shout(progress: 30%)
  ...                               ...
  Found issue!                      Found potential bug!
  hivemind_ask("Fix this?")        hivemind_shout(completed)

┌─────────────────────────────────────────────────────────────┐
│          Step 4: Human Decision (hivemind_ask)               │
└─────────────────────────────────────────────────────────────┘

Hivemind Status:
  agents:
    tester: status=blocked, task="Fix test failure?"
    reviewer: status=completed
  pending-asks:
    - ask_id: "abc123"
      agent_id: "tester"
      question: "Test failed on line 42. Should I fix it?"
      options: ["yes", "no", "show details"]

Coordinator (you):
  hivemind_respond(ask_id: "abc123", decision: "yes")

┌─────────────────────────────────────────────────────────────┐
│              Step 5: Agent Continues & Completes             │
└─────────────────────────────────────────────────────────────┘

Tester Agent receives: {decision: "yes", by: "human"}
  Fixing the test...
  hivemind_shout(completed, result: "All tests passing")

┌─────────────────────────────────────────────────────────────┐
│                   Step 6: Collect Results                    │
└─────────────────────────────────────────────────────────────┘

swarm_collect(task_id: "task-tester-001")
  → {status: "completed", result: "Fixed bug, all 42 tests pass"}

swarm_collect(task_id: "task-reviewer-001")
  → {status: "completed", result: "Found 2 potential issues..."}

┌─────────────────────────────────────────────────────────────┐
│                      Step 7: Cleanup                         │
└─────────────────────────────────────────────────────────────┘

swarm_kill(slave_id: "all")
  → {killed_count: 2}
```

---

## MCP Tools Reference

### Hivemind Tools

| Tool | Purpose | Blocking? |
|------|---------|-----------|
| `hivemind_shout` | Report status to coordinator | No |
| `hivemind_ask` | Request human decision | Yes (until response) |
| `hivemind_respond` | Answer pending ask | No |
| `hivemind_status` | Get coordinator state | No |

### Swarm Tools

| Tool | Purpose | Blocking? |
|------|---------|-----------|
| `swarm_spawn` | Create new agent | No (async) |
| `swarm_dispatch` | Send task to agent | No |
| `swarm_collect` | Get task result | Polling |
| `swarm_status` | Get swarm state | No |
| `swarm_broadcast` | Send to all agents | No |
| `swarm_kill` | Terminate agent(s) | No |
| `swarm_list_presets` | List available presets | No |
| `swarm_pending_prompts` | Get permission prompts | No |
| `swarm_respond_prompt` | Answer permission prompt | No |

### Channel Tools

| Tool | Purpose |
|------|---------|
| `channel_emacs_connect` | Connect Emacs to channel |
| `channel_emacs_disconnect` | Disconnect from channel |
| `channel_status` | Get connection status |
| `channel_recent_events` | Get event history |

### Coordinator Tools

| Tool | Purpose |
|------|---------|
| `swarm_coordinator_status` | Get queue and claims |
| `swarm_process_queue` | Dispatch ready tasks |
| `swarm_hive_stats` | Get memory stats |

---

## Safety & Constraints

### Recursion Prevention

Agents spawning agents can cause runaway recursion:

```
Master → Slave → GrandSlave → GreatGrandSlave → ...
  (0)     (1)       (2)            (3)          BLOCKED
```

**Protection:**
- `CLAUDE_SWARM_DEPTH` environment variable
- Default max depth: 3
- Spawn attempts beyond limit throw error

### Rate Limiting

Prevents spawn storms:

```clojure
{:rate-limit
  {:window-seconds 60
   :max-spawns 10}}
```

### Resource Guard

Before spawning, check system resources:

```clojure
(resource_guard)
→ {:can-spawn true
   :memory {:percent-used 45, :available-mb 16000}
   :recommendation "Safe to spawn"}
```

### File Conflict Detection

Pre-flight check before dispatch:

```clojure
(pre-flight-check {:slave-id "agent-1" :files ["src/core.clj"]})
→ {:safe? false
   :conflicts [["src/core.clj" "agent-2"]]
   :recommendation :queue}
```

### JVM Cleanup

Orphaned JVM processes from crashed agents:

```clojure
(jvm_cleanup :dry_run true)
→ {:orphans [{:pid 12345 :type "leiningen" :age-minutes 45}]
   :recommendation "Run with dry_run: false to kill"}
```

---

## File Structure

```
emacs-mcp/
├── src/emacs_mcp/
│   ├── hivemind.clj          # Agent coordination (shout/ask/respond)
│   ├── channel.clj           # Bidirectional TCP channel
│   └── swarm/
│       ├── coordinator.clj   # Pre-flight checks, task queue
│       ├── logic.clj         # core.logic conflict detection
│       ├── hive.clj          # Memory integration
│       └── sync.clj          # Channel → Logic sync
├── elisp/addons/
│   ├── emacs-mcp-swarm.el    # Terminal orchestration
│   └── emacs-mcp-channel.el  # Emacs channel client
└── elisp/presets/
    ├── hivemind.md           # Hivemind agent preset
    ├── tdd.md                # Test-driven development
    ├── clarity.md            # CLARITY principles
    └── ...
```

---

## Quick Start

### 1. Start the System

```elisp
;; Emacs
(require 'emacs-mcp-swarm)
(emacs-mcp-swarm-mode 1)
```

### 2. Connect Channel

```clojure
(channel_emacs_connect)
→ {:connected true}
```

### 3. Spawn an Agent

```clojure
(swarm_spawn :name "worker" :presets ["hivemind"])
→ "swarm-worker-1767461382"
```

### 4. Dispatch a Task

```clojure
(swarm_dispatch
  :slave_id "swarm-worker-1767461382"
  :prompt "Your task here...")
→ {:task-id "task-worker-001" :status "dispatched"}
```

### 5. Monitor via Hivemind

```clojure
(hivemind_status)
→ {:agents {"worker" {:status "progress" :task "Your task"}}
   :pending-asks []
   :channel-connected true}
```

### 6. Respond to Asks

```clojure
(hivemind_respond :ask_id "..." :decision "yes")
→ {:success true}
```

### 7. Collect and Cleanup

```clojure
(swarm_collect :task_id "task-worker-001")
→ {:status "completed" :result "..."}

(swarm_kill :slave_id "all")
→ {:killed-count 1}
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-03 | Initial comprehensive documentation |

---

## Related Documentation

- [swarm-protocol.md](./swarm-protocol.md) - Detailed protocol spec
- [BIDIRECTIONAL_CHANNEL.md](./BIDIRECTIONAL_CHANNEL.md) - Channel deep dive
- [swarm-memory-coordination.md](./swarm-memory-coordination.md) - Memory integration
- [INTEGRATION_GAPS.md](./INTEGRATION_GAPS.md) - Known issues and roadmap
