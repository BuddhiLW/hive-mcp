# Swarm Backends: vterm vs claude-code-ide

This document compares the two available swarm backends for orchestrating multiple Claude instances.

## Overview

| Feature | vterm (default) | claude-code-ide |
|---------|-----------------|-----------------|
| Communication | Terminal scraping | JSON-RPC/WebSocket |
| Response collection | Buffer output parsing | Hivemind shout events |
| Reliability | Timing-sensitive | Structured & reliable |
| Setup complexity | Minimal | Requires claude-code-ide |
| Terminal emulator | vterm or eat | Not applicable |
| Best for | Simple tasks | Complex multi-agent workflows |

## Backend 1: vterm/eat (emacs-mcp-swarm)

The default swarm backend spawns Claude instances in terminal emulators and communicates via terminal I/O.

### Architecture

```
Master Claude
      │
      v
emacs-mcp-swarm.el
      │ vterm/eat process
      v
┌─────────────────┐
│ Claude (shell)  │
│ - Terminal I/O  │
│ - Prompt marker │
│ - Output buffer │
└─────────────────┘
```

### Pros
- No additional dependencies (vterm is common)
- Direct terminal access for debugging
- Works with any Claude CLI version

### Cons
- **Timing issues**: Terminal output parsing is inherently racy
- **Marker detection**: Relies on detecting prompt markers (`❯`)
- **Response collection**: Scraping terminal buffer can miss or duplicate content
- **Stateful**: Terminal state must be tracked carefully

### Usage

```elisp
(require 'emacs-mcp-swarm)
(emacs-mcp-swarm-mode 1)

;; Spawn slave
(emacs-mcp-swarm-spawn "tester" :presets '("tdd"))

;; Dispatch and wait for response
(let ((task-id (emacs-mcp-swarm-dispatch "swarm-tester-xxx" "Run tests")))
  (emacs-mcp-swarm-collect task-id 30000))
```

### Configuration

```elisp
(setq emacs-mcp-swarm-terminal 'vterm)  ; or 'eat
(setq emacs-mcp-swarm-max-slaves 5)
(setq emacs-mcp-swarm-prompt-marker "❯")
```

---

## Backend 2: claude-code-ide (emacs-mcp-claude-code-ide)

An alternative backend using [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el) with hivemind for completion tracking.

### Architecture

```
Master Claude
      │ MCP tools
      v
emacs-mcp-claude-code-ide.el
      │ claude-code-ide API
      v
┌─────────────────┐
│ Lings (Claude)  │
│ - JSON-RPC      │
│ - WebSocket     │
└────────┬────────┘
         │ hivemind_shout :completed
         v
┌─────────────────┐
│ Hivemind Coord  │ ← Master polls/subscribes
└─────────────────┘
```

### Completion Flow

1. Master dispatches task with task-id to ling
2. Ling executes task autonomously
3. Ling calls `hivemind_shout` with `event_type="completed"`
4. Hivemind coordinator receives structured completion data
5. This addon polls hivemind to update local task status

### Pros
- **Structured communication**: No terminal parsing
- **Reliable completion**: Lings report via hivemind_shout
- **Clean separation**: No terminal state management
- **Better for parallel work**: Multiple lings coordinate via hivemind
- **Structured results**: Completion includes status, result, files_modified

### Cons
- Requires claude-code-ide package
- Depends on hivemind being available
- Polling-based (configurable interval)

### Usage

```elisp
(require 'emacs-mcp-claude-code-ide)
(emacs-mcp-claude-code-ide-mode 1)

;; Spawn ling
(emacs-mcp-cci-spawn "worker" :presets '("hivemind" "tdd"))
;; => "ling-worker-123456"

;; Dispatch task
(emacs-mcp-cci-dispatch "ling-worker-123456" "Implement feature X")
;; => "task-worker-123456-001"

;; Task completion happens via hivemind
;; Auto-sync is enabled by default

;; Manual sync if needed
(emacs-mcp-cci-sync-from-hivemind)

;; Check status
(emacs-mcp-cci-status)
;; => (:backend "claude-code-ide"
;;     :completion-mechanism "hivemind"
;;     :lings (:total 1 :idle 0 :working 1 :error 0)
;;     :tasks (:total 1 :pending 1)
;;     ...)
```

### Configuration

```elisp
(setq emacs-mcp-cci-default-timeout 300000)  ; 5 minutes
(setq emacs-mcp-cci-max-lings 10)
(setq emacs-mcp-cci-hivemind-poll-interval 5)  ; seconds
(setq emacs-mcp-cci-auto-sync t)
```

### Task Completion via Hivemind

When you dispatch a task, the ling receives instructions to report completion:

```
## Task ID: task-worker-123456-001

<your prompt>

---
**IMPORTANT - Completion Reporting:**
When you complete this task, use `hivemind_shout` to report:

hivemind_shout(
  agent_id: "worker",
  event_type: "completed",
  task: "task-worker-123456-001",
  message: "<brief summary>",
  data: {
    "task_id": "task-worker-123456-001",
    "status": "success" | "error" | "partial",
    "result": "<your findings/output>",
    "files_modified": ["<list of changed files>"]
  }
)
```

---

## Choosing a Backend

### Use vterm (default) when:
- Simple, single-agent tasks
- Debugging terminal interaction
- Minimal setup requirements
- You need direct terminal access

### Use claude-code-ide when:
- Complex multi-agent workflows
- Reliability is critical
- You need structured completion data
- You're already using hivemind for coordination
- Terminal timing issues are causing problems

---

## Migration

### From vterm to claude-code-ide

1. Install claude-code-ide:
   ```elisp
   ;; For Doom Emacs
   (package! claude-code-ide :recipe (:host github :repo "manzaltu/claude-code-ide"))
   ```

2. Enable the addon:
   ```elisp
   (require 'emacs-mcp-claude-code-ide)
   (emacs-mcp-claude-code-ide-mode 1)
   ```

3. Update your swarm calls:
   ```elisp
   ;; Before (vterm)
   (emacs-mcp-swarm-spawn "worker" :presets '("tdd"))
   (emacs-mcp-swarm-dispatch "swarm-worker-xxx" "Task")
   (emacs-mcp-swarm-collect "task-xxx" 30000)

   ;; After (claude-code-ide)
   (emacs-mcp-cci-spawn "worker" :presets '("tdd"))
   (emacs-mcp-cci-dispatch "ling-worker-xxx" "Task")
   ;; No explicit collect - completion via hivemind
   ```

4. Ensure hivemind is available for lings to report completion

---

## API Comparison

| Operation | vterm API | claude-code-ide API |
|-----------|-----------|---------------------|
| Spawn | `emacs-mcp-swarm-spawn` | `emacs-mcp-cci-spawn` |
| Dispatch | `emacs-mcp-swarm-dispatch` | `emacs-mcp-cci-dispatch` |
| Collect | `emacs-mcp-swarm-collect` | `emacs-mcp-cci-api-collect` |
| Status | `emacs-mcp-swarm-status` | `emacs-mcp-cci-status` |
| Kill | `emacs-mcp-swarm-kill` | `emacs-mcp-cci-kill` |
| Kill all | `emacs-mcp-swarm-kill-all` | `emacs-mcp-cci-kill-all` |
| Broadcast | `emacs-mcp-swarm-broadcast` | Not implemented |
| Sync hivemind | N/A | `emacs-mcp-cci-sync-from-hivemind` |

---

## Hybrid Approach

Both backends can coexist. Use vterm for quick tasks and claude-code-ide for complex workflows:

```elisp
;; Load both
(require 'emacs-mcp-swarm)
(require 'emacs-mcp-claude-code-ide)
(emacs-mcp-swarm-mode 1)
(emacs-mcp-claude-code-ide-mode 1)

;; Quick task via vterm
(emacs-mcp-swarm-spawn "quick" :presets '("fixer"))
(emacs-mcp-swarm-dispatch "swarm-quick-xxx" "Fix typo in README")

;; Complex workflow via claude-code-ide
(emacs-mcp-cci-spawn "architect" :presets '("hivemind" "ddd"))
(emacs-mcp-cci-dispatch "ling-architect-xxx" "Design new authentication module")
;; Architect reports completion via hivemind when done
```
