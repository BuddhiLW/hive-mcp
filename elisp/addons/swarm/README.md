# hive-mcp-swarm Module Decomposition

This directory contains the decomposed swarm modules following SOLID/CLARITY/DDD principles.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    hive-mcp-swarm.el (Facade)                   │
│                                                                 │
│  - Spawn/kill slaves          - Dispatch/collect tasks          │
│  - API functions              - Mode/lifecycle management       │
└────────────┬────────────┬────────────┬────────────┬────────────┘
             │            │            │            │
             v            v            v            v
┌────────────────┐ ┌────────────────┐ ┌────────────────┐ ┌────────────────┐
│   terminal.el  │ │   presets.el   │ │   prompts.el   │ │   events.el    │
│                │ │                │ │                │ │                │
│ Backend I/O    │ │ Preset loading │ │ Prompt detect  │ │ Channel emit   │
│ abstraction    │ │ & building     │ │ & response     │ │ for push       │
└────────────────┘ └────────────────┘ └────────────────┘ └────────────────┘
```

## Module Responsibilities

### hive-mcp-swarm-terminal.el

**Single Responsibility:** Terminal I/O abstraction

**Key Features:**
- Unified interface for vterm, eat, and claude-code-ide backends
- Non-blocking operations using timers (never freezes Emacs)
- Backend auto-detection from buffer mode
- Ready-state polling with callbacks

**Public API:**
```elisp
;; Send text to any terminal backend (auto-detects)
(hive-mcp-swarm-terminal-send BUFFER TEXT &optional BACKEND)

;; Check if terminal is ready for input
(hive-mcp-swarm-terminal-ready-p BUFFER)

;; Wait for ready with callback (non-blocking!)
(hive-mcp-swarm-terminal-wait-ready BUFFER CALLBACK &optional TIMEOUT)

;; Create terminal buffer with specified backend
(hive-mcp-swarm-terminal-create-buffer NAME DIR BACKEND &optional SLAVE-ID)

;; Kill buffer without prompts (handles process cleanup)
(hive-mcp-swarm-terminal-kill-buffer BUFFER)
```

**Design Pattern:** Strategy Pattern - each backend is a strategy for terminal I/O.

---

### hive-mcp-swarm-presets.el

**Single Responsibility:** Preset loading and building

**Key Features:**
- File-based presets from `.md` files (recursive directory scan)
- Memory-based presets from conventions tagged `swarm-preset`
- Priority: memory (project-scoped) → file (global fallback)
- Role-to-preset mapping for convenience

**Public API:**
```elisp
;; List all available presets
(hive-mcp-swarm-presets-list)

;; Get preset content (checks memory first, then files)
(hive-mcp-swarm-presets-get NAME)

;; Build combined system prompt from preset list
(hive-mcp-swarm-presets-build-system-prompt PRESETS)

;; Convert role name to preset list
(hive-mcp-swarm-presets-role-to-presets ROLE)

;; Reload presets from disk
(hive-mcp-swarm-presets-reload)
```

**Design Pattern:** Template Method - all preset sources return same format.

---

### hive-mcp-swarm-prompts.el

**Single Responsibility:** Prompt detection and response handling

**Key Features:**
- Three modes: bypass (CLI flag), auto (timer), human (forward to master)
- Pattern-based prompt detection in terminal buffers
- Desktop notifications via `notify-send`
- Push events via channel for human mode
- Pending prompt queue with interactive commands

**Public API:**
```elisp
;; Start/stop the prompt watcher timer
(hive-mcp-swarm-prompts-start-watcher SLAVES-HASH GET-TERMINAL-FN)
(hive-mcp-swarm-prompts-stop-watcher)

;; Interactive commands (human mode)
(hive-mcp-swarm-prompts-respond)   ; Custom response
(hive-mcp-swarm-prompts-approve)   ; Send 'y'
(hive-mcp-swarm-prompts-deny)      ; Send 'n'
(hive-mcp-swarm-prompts-list)      ; Show pending

;; Programmatic API
(hive-mcp-swarm-prompts-get-pending)
(hive-mcp-swarm-prompts-respond-to SLAVE-ID RESPONSE)
```

**Design Pattern:** Observer Pattern - prompts emit events for listeners.

---

### hive-mcp-swarm-events.el

**Single Responsibility:** Channel event emission

**Key Features:**
- Centralized event emission through bidirectional channel
- Typed emitters for each event kind
- Session ID correlation for event tracking
- Graceful handling when channel unavailable

**Public API:**
```elisp
;; Core emit function
(hive-mcp-swarm-events-emit EVENT-TYPE DATA)

;; Typed emitters
(hive-mcp-swarm-events-emit-slave-spawned SLAVE-ID NAME PRESETS)
(hive-mcp-swarm-events-emit-slave-killed SLAVE-ID)
(hive-mcp-swarm-events-emit-task-completed TASK-ID SLAVE-ID RESULT)
(hive-mcp-swarm-events-emit-task-failed TASK-ID SLAVE-ID ERROR)
(hive-mcp-swarm-events-emit-prompt-shown SLAVE-ID PROMPT-TEXT)
(hive-mcp-swarm-events-emit-state-changed SLAVE-ID OLD-STATE NEW-STATE)

;; Session management
(hive-mcp-swarm-events-set-session-id SESSION-ID)
(hive-mcp-swarm-events-get-session-id)
```

**Design Pattern:** Mediator Pattern - centralized event dispatch.

---

## SOLID Principles Applied

| Principle | Application |
|-----------|-------------|
| **Single Responsibility** | Each module has one reason to change |
| **Open/Closed** | New backends/sources via extension, not modification |
| **Liskov Substitution** | All preset sources return same format |
| **Interface Segregation** | Separate APIs for auto vs human prompt modes |
| **Dependency Inversion** | Facade depends on abstractions, not implementations |

## CLARITY Framework Applied

| Principle | Application |
|-----------|-------------|
| **Composition** | Terminal backend is composed, not inherited |
| **Layers** | Domain (presets) separated from I/O (terminal) |
| **Architectural Performance** | Non-blocking async with timers |
| **Represented Intent** | Module names describe responsibility |
| **Inputs Guarded** | Validation at module boundaries |
| **Telemetry** | Events module for observability |
| **Yield Safe Failure** | Graceful fallbacks on error |

## Lifecycle Management

Each module provides init/shutdown functions:

```elisp
;; Module lifecycle
(hive-mcp-swarm-terminal-init)     ; No-op (stateless)
(hive-mcp-swarm-presets-init)      ; Load presets cache
(hive-mcp-swarm-prompts-init)      ; Clear state
(hive-mcp-swarm-events-init ID)    ; Set session ID

(hive-mcp-swarm-terminal-shutdown)  ; No-op
(hive-mcp-swarm-presets-shutdown)   ; Clear cache
(hive-mcp-swarm-prompts-shutdown)   ; Stop timer, clear state
(hive-mcp-swarm-events-shutdown)    ; Clear session
```

## Adding New Functionality

### New Terminal Backend

1. Add availability check:
   ```elisp
   ;; In hive-mcp-swarm-terminal.el
   (defun hive-mcp-swarm-terminal-backend-available-p (backend)
     (pcase backend
       ('my-new-backend (require 'my-new-backend nil t))
       ...))
   ```

2. Add send implementation:
   ```elisp
   (defun hive-mcp-swarm-terminal--send-my-new-backend (buffer text)
     "Send TEXT to my-new-backend BUFFER, non-blocking."
     ...)
   ```

3. Update dispatch in `hive-mcp-swarm-terminal-send`

### New Preset Source

1. Add query function in `hive-mcp-swarm-presets.el`:
   ```elisp
   (defun hive-mcp-swarm-presets--get-from-new-source (name)
     "Get preset from new source."
     ...)
   ```

2. Add to priority chain in `hive-mcp-swarm-presets-get`

### New Event Type

1. Add typed emitter in `hive-mcp-swarm-events.el`:
   ```elisp
   (defun hive-mcp-swarm-events-emit-my-event (arg1 arg2)
     (hive-mcp-swarm-events-emit
      "my-event"
      `(("field1" . ,arg1) ("field2" . ,arg2))))
   ```

2. Call from swarm.el where event occurs
