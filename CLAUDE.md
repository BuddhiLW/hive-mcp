# CLAUDE.md - hive-mcp Project Guide

## Project Overview

**hive-mcp** is an Emacs integration for Claude MCP (Model Context Protocol) providing:
- Persistent, project-scoped memory (notes, conventions, decisions, snippets)
- Semantic search via Chroma/Ollama embeddings
- Multi-agent swarm orchestration for parallel task execution
- Full Emacs integration (buffers, git/Magit, projects, CIDER, org-mode)

## Architecture

### Module Organization (SOLID/CLARITY/DDD)

```
elisp/
├── hive-mcp.el                    # Core module, entry point
├── hive-mcp-api.el               # MCP tool handlers (API layer)
├── hive-mcp-memory.el            # Memory persistence (domain layer)
├── hive-mcp-context.el           # Context gathering
├── hive-mcp-channel.el           # Bidirectional WebSocket channel
├── hive-mcp-graceful.el          # Error handling utilities
└── addons/
    ├── hive-mcp-swarm.el         # Swarm orchestration (facade)
    └── swarm/                     # Decomposed swarm modules
        ├── hive-mcp-swarm-terminal.el  # Terminal backend abstraction
        ├── hive-mcp-swarm-presets.el   # Preset loading/management
        ├── hive-mcp-swarm-prompts.el   # Prompt detection/response
        └── hive-mcp-swarm-events.el    # Channel event emission
```

### Design Principles Applied

**SOLID:**
- **S**ingle Responsibility: Each swarm module handles one concern
- **O**pen/Closed: New backends via dispatch, not modification
- **L**iskov Substitution: All preset sources return same format
- **I**nterface Segregation: Separate APIs for auto/human prompt modes
- **D**ependency Inversion: Callers depend on abstractions, not implementations

**CLARITY Framework:**
- **C**omposition over modification: Strategy pattern for terminal backends
- **L**ayers stay pure: Domain logic separated from I/O
- **A**rchitectural performance: Non-blocking async operations
- **R**epresented intent: Clear module naming by responsibility
- **I**nputs are guarded: Validation at module boundaries
- **T**elemetry first: Event emission for monitoring
- **Y**ield safe failure: Graceful fallbacks via `hive-mcp-with-fallback`

## Swarm Module Decomposition

The swarm system was refactored from a monolithic 1400-line file into focused modules:

| Module | Lines | Responsibility | SOLID Principle |
|--------|-------|----------------|-----------------|
| `hive-mcp-swarm-terminal.el` | 250 | Terminal I/O abstraction | SRP, DIP |
| `hive-mcp-swarm-presets.el` | 200 | Preset loading (file + memory) | OCP, LSP |
| `hive-mcp-swarm-prompts.el` | 380 | Prompt detection & response | SRP, ISP |
| `hive-mcp-swarm-events.el` | 130 | Channel event emission | SRP, DIP |
| `hive-mcp-swarm.el` | 1450 | Facade + orchestration | Facade pattern |

### Terminal Backend Abstraction

```elisp
;; Strategy pattern - callers don't know about vterm/eat/claude-code-ide
(hive-mcp-swarm-terminal-send buffer text)  ; Auto-detects backend
(hive-mcp-swarm-terminal-wait-ready buffer callback)  ; Non-blocking!
```

Backends supported:
- `claude-code-ide` (recommended) - MCP WebSocket integration
- `vterm` - Native terminal compilation
- `eat` - Pure Emacs Lisp (experimental)

### Preset Sources (Priority Order)

1. **Memory-based**: Project-scoped conventions tagged `swarm-preset`
2. **File-based**: `.md` files from preset directories

```elisp
;; Both sources return same format (LSP compliance)
(hive-mcp-swarm-presets-get "tdd")  ; Checks memory first, then files
```

### Prompt Handling Modes

| Mode | Description | Implementation |
|------|-------------|----------------|
| `bypass` | CLI flag `--permission-mode bypassPermissions` | No Emacs involvement |
| `auto` | Timer-based auto-approve | `hive-mcp-swarm-prompts--auto-tick` |
| `human` | Forward to master for decision | Push events via channel |

## Key Commands

```bash
# Start the MCP server
./start-mcp.sh

# Run tests
clojure -M:dev:test

# REPL development
clojure -M:dev:nrepl
```

## Common Workflows

### Adding a New Terminal Backend

1. Add detection in `hive-mcp-swarm-terminal-backend-available-p`
2. Add send function `hive-mcp-swarm-terminal--send-<backend>`
3. Update `hive-mcp-swarm-terminal-send` dispatch
4. Add buffer creation in `hive-mcp-swarm-terminal-create-buffer`

### Adding a New Preset Source

1. Create query function returning `(list :content "..." :tags '(...))`
2. Add to `hive-mcp-swarm-presets-get` priority chain
3. Update `hive-mcp-swarm-presets-list` to include source

### Adding a New Event Type

1. Add typed emitter in `hive-mcp-swarm-events.el`:
   ```elisp
   (defun hive-mcp-swarm-events-emit-<event-type> (...)
     (hive-mcp-swarm-events-emit "<event-type>" `((...))))
   ```
2. Call from swarm.el where event occurs

## Testing

```bash
# Unit tests
clojure -M:dev:test

# Emacs integration (manual)
M-x hive-mcp-swarm-mode
M-x hive-mcp-swarm-spawn "test" :presets '("tdd")
M-x hive-mcp-swarm-status
```

## Dependencies

- Emacs 28.1+
- Clojure CLI 1.11+
- Java 17+
- Optional: Ollama (semantic search), vterm/eat (terminal backends)

## Hivemind Operations

### Token-Tiered Hierarchy

```
Hivemind (premium Claude) ─── Coordinator, spawns lings, receives shouts
    │
    └── Lings (Claude instances) ─── Tactical leads, use delegate_drone
            │
            └── Drones (OpenRouter free-tier) ─── Heavy lifting, propose diffs
```

### Hivemind Coordinator Patterns

**DO:**
- Spawn lings with presets: `swarm_spawn(name, presets: ["ling", "tdd"])`
- Trust HIVEMIND piggyback messages (arrive on any MCP tool call)
- Dispatch tasks with clear instructions, then move on
- Check results once when task should be complete
- Keep context minimal - delegate reading/exploration to lings

**DON'T (Anti-patterns):**
- Poll with `mcp_watch_buffer` (blows up tokens)
- Call `swarm_collect` in loops
- Call `delegate_drone` directly (bypasses ling coordination)
- Read large files as coordinator (delegate to lings)
- Micromanage - trust the hierarchy

### Ling Communication Protocol

Lings MUST shout progress, not just start/end:
```
hivemind_shout(event_type: "progress", task: "...", message: "Step 2/4: ...")
```

**MCP-First Tools** - Lings use hive-mcp tools instead of raw bash:

| Instead of | Use |
|------------|-----|
| `cat file` | `mcp__emacs__read_file` |
| `grep pattern` | `mcp__emacs__grep` |
| `git status` | `mcp__emacs__magit_status` |
| Raw file write | `mcp__emacs__file_write` |

### Spawning Lings

```elisp
;; From coordinator
swarm_spawn(name: "task-name", presets: ["ling", "tdd", "clarity"])
swarm_dispatch(slave_id: "swarm-task-name-...", prompt: "...", files: ["..."])
```

Lings consult `kanban.org` for task guidance and use `delegate_drone` for implementation work.

### Crystal Convergence (Auto-Permeate)

When lings run `/wrap`, the system emits a `:wrap_notify` event that appears in HIVEMIND piggyback messages. The coordinator should **automatically permeate** crystals when seeing this notification.

**How wrap_notify appears:**
```
---HIVEMIND---
[{:a "ling-task-name-123" :e "wrap_notify" :m "Session wrapped: 3 decisions, 2 conventions"}]
---/HIVEMIND---
```

**Coordinator action:** When you see `wrap_notify` in any HIVEMIND piggyback, call:
```clojure
mcp_permeate_crystals
```

This processes the wrap queue, permeating session learnings from lings into permanent memory.

**Full flow:**
```
1. Ling completes task, runs /wrap
2. wrap_crystallize dispatches :crystal/wrap-notify event
3. Event adds entry to wrap-queue in DataScript
4. HIVEMIND piggybacks wrap_notify on next MCP tool call
5. Coordinator sees wrap_notify, calls mcp_permeate_crystals
6. Queue is processed, entries marked processed
7. Session learnings become permanent memory
```

**Why auto-permeate?**
- Ling sessions contain valuable learnings (decisions, conventions, snippets)
- Without permeation, these remain in wrap-queue indefinitely
- Coordinator has global context to assess cross-session value
- Maintains the "coordinator as memory curator" pattern

## Swarm Trust & Delegation (CRITICAL)

**This section is loaded via /catchup at session start. Trust these patterns immediately.**

### The Trust Problem

Each new session, the coordinator (Claude) takes time to "learn" the swarm architecture. This wastes tokens on:
- Re-reading architecture docs
- Hesitant delegation (doing work instead of spawning lings)
- Over-monitoring (polling buffers instead of trusting shouts)

### Trust Axioms (Follow Immediately)

1. **Spawn First, Don't Do** - For any multi-file task, spawn a ling before reading code yourself
2. **Shouts Are Truth** - `hivemind_status` and `---HIVEMIND---` messages are authoritative
3. **TDD Is The Arbiter** - Don't verify ling work manually; tests decide correctness
4. **Files Are Claimed** - Once dispatched, trust the ling owns those files until completion
5. **One Check Per Task** - Check ling results ONCE when task should be complete

### Coordinator Session Flow

```
1. /catchup                     # Load memory, conventions, swarm patterns
2. Check hivemind_status        # See active lings from previous session
3. Review kanban                # Get task list
4. Spawn lings for tasks        # Don't read files yourself
5. Work on coordinator-only tasks (decisions, architecture)
6. Check results when lings shout "completed"
7. /wrap at end                 # Crystallize session
```

### Token Budget Mental Model

```
Coordinator budget: EXPENSIVE (Opus 4.5)
├── Strategic decisions
├── Spawning/dispatch
├── Reviewing completed work
└── Memory operations

Ling budget: MODERATE (Claude instances)
├── Code reading/exploration
├── Implementation
├── Testing
└── Shouting progress

Drone budget: CHEAP (OpenRouter free-tier)
├── Bulk file mutations
└── Repetitive changes
```

**Coordinator should NEVER:** Read large files, grep codebases, run tests, or do implementation work.

### Quick Reference: When To Spawn

| Task Type | Action |
|-----------|--------|
| "Fix bug in X" | Spawn ling with TDD preset |
| "Add feature Y" | Spawn ling with clarity preset |
| "Refactor Z" | Spawn ling, files param critical |
| "Research Q" | Spawn ling with research focus |
| "Review PR" | Spawn ling with reviewer preset |
| "Update docs" | Spawn ling with docs preset |
| "Decide architecture" | Coordinator handles (decision = expensive token justified) |

## File Conventions

- Module files: `hive-mcp-<module>.el`
- Addon files: `elisp/addons/hive-mcp-<addon>.el`
- Submodule files: `elisp/addons/<addon>/hive-mcp-<addon>-<submodule>.el`
- Test files: `test/hive_mcp/<module>_test.clj`
