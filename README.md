# emacs-mcp

MCP (Model Context Protocol) server that allows Claude to interact with a running Emacs instance via `emacsclient`.

## Features

### Core Tools

| Tool | Description |
|------|-------------|
| `eval_elisp` | Execute arbitrary Emacs Lisp code |
| `emacs_status` | Check if Emacs server is running |
| `list_buffers` | List all open buffers |
| `get_buffer_content` | Read content from a buffer |
| `current_buffer` | Get current buffer name and file |
| `switch_to_buffer` | Switch to a specific buffer |
| `find_file` | Open a file in Emacs |
| `save_buffer` | Save the current buffer |
| `goto_line` | Move cursor to a line number |
| `insert_text` | Insert text at cursor |
| `project_root` | Get current project root |
| `recent_files` | Get recently opened files |

### Memory & Context Tools

| Tool | Description |
|------|-------------|
| `mcp_capabilities` | Check emacs-mcp.el availability and features |
| `mcp_get_context` | Get full context (buffer, project, git, memory) |
| `mcp_memory_add` | Add notes, snippets, conventions, or decisions |
| `mcp_memory_query` | Query stored memory entries by type |
| `mcp_memory_query_metadata` | Efficient metadata-only queries (10x fewer tokens) |
| `mcp_memory_get_full` | Get full content of a memory entry by ID |
| `mcp_memory_search_semantic` | **NEW!** Vector similarity search via Chroma |

### Workflow & Notification Tools

| Tool | Description |
|------|-------------|
| `mcp_list_workflows` | List available user-defined workflows |
| `mcp_run_workflow` | Execute a workflow by name |
| `mcp_notify` | Show notification messages in Emacs |
| `mcp_list_special_buffers` | List special buffers (*Messages*, etc.) |
| `mcp_watch_buffer` | Monitor buffer content (logs, warnings) |

### Kanban Tools

| Tool | Description |
|------|-------------|
| `mcp_kanban_status` | Get kanban board status and progress |
| `mcp_kanban_list_tasks` | List tasks with optional status filter |
| `mcp_kanban_create_task` | Create a new task |
| `mcp_kanban_update_task` | Update task title/status |
| `mcp_kanban_move_task` | Move task to new status column |
| `mcp_kanban_my_tasks` | Get tasks assigned to current agent |
| `mcp_kanban_roadmap` | View roadmap with milestones |
| `mcp_kanban_sync` | Sync between vibe-kanban and org-kanban |

### CIDER Tools (Clojure Development)

| Tool | Description |
|------|-------------|
| `cider_status` | Get CIDER connection status |
| `cider_eval_silent` | Fast silent evaluation |
| `cider_eval_explicit` | Interactive evaluation with REPL output |
| `cider_list_sessions` | List all active CIDER sessions |
| `cider_spawn_session` | Create isolated nREPL session for parallel agents |
| `cider_eval_session` | Evaluate in specific named session |
| `cider_kill_session` | Kill a named session |
| `cider_kill_all_sessions` | Clean up all sessions |

### Swarm Tools (Multi-Agent)

| Tool | Description |
|------|-------------|
| `swarm_spawn` | Spawn a new Claude slave instance |
| `swarm_dispatch` | Send prompt to a slave |
| `swarm_collect` | Collect response from a task |
| `swarm_broadcast` | Send same prompt to all slaves |
| `swarm_status` | Get swarm status and task counts |
| `swarm_kill` | Kill slave instance(s) |
| `swarm_list_presets` | List available specialization presets |

### Git Tools (via Magit)

| Tool | Description |
|------|-------------|
| `magit_status` | Full repo status with staged/unstaged/untracked |
| `magit_branches` | Branch info (current, upstream, local, remote) |
| `magit_log` | Recent commits |
| `magit_diff` | Show staged/unstaged/all diffs |
| `magit_stage` | Stage files or all changes |
| `magit_commit` | Create commit with message |
| `magit_push` | Push to remote |
| `magit_pull` | Pull from upstream |
| `magit_fetch` | Fetch from remotes |
| `magit_feature_branches` | List feature/fix/feat branches |

### Project Tools (via Projectile)

| Tool | Description |
|------|-------------|
| `projectile_info` | Project name, type, root, file count |
| `projectile_files` | List files with optional glob filter |
| `projectile_find_file` | Find file by name |
| `projectile_search` | Search project with ripgrep |
| `projectile_recent` | Recently visited project files |
| `projectile_list_projects` | List all known projects |

### Prompt Capture Tools (RAG Knowledge Base)

| Tool | Description |
|------|-------------|
| `prompt_capture` | Capture well-structured prompts with analysis |
| `prompt_analyze` | Analyze prompt structure without saving |
| `prompt_search` | Search captured prompts by keyword |
| `prompt_list` | List prompts with filters |
| `prompt_stats` | Statistics about captured prompts |

### Org-Mode Tools

| Tool | Description |
|------|-------------|
| `org_clj_parse` | Parse org file to JSON structure |
| `org_clj_query` | Query headlines by ID, status, etc. |
| `org_clj_write` | Write org structure back to file |
| `org_kanban_native_status` | Get kanban status from org file |
| `org_kanban_native_move` | Move task to new status |
| `org_kanban_render` | Render visual kanban board |

> **Auto-detection**: Tools automatically check if required packages are loaded and provide helpful error messages if not

## Prerequisites

1. **Emacs** with server mode enabled:
   ```elisp
   (server-start)
   ```

2. **Clojure CLI** (deps.edn)

3. **emacsclient** in your PATH

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/BuddhiLW/emacs-mcp.git
cd emacs-mcp
```

### 2. Add to Claude Code

Add to `~/.claude/settings.json` under `mcpServers`:

```json
{
  "mcpServers": {
    "emacs-mcp": {
      "command": "/home/you/path/to/emacs-mcp/start-mcp.sh"
    }
  }
}
```

Or directly with clojure:

```json
{
  "mcpServers": {
    "emacs-mcp": {
      "command": "clojure",
      "args": ["-X:mcp"],
      "cwd": "/home/you/path/to/emacs-mcp"
    }
  }
}
```

### 3. Ensure Emacs server is running

```elisp
;; In your Emacs init.el
(server-start)
```

### 4. (Optional) Load emacs-mcp.el for full features

```elisp
(add-to-list 'load-path "/path/to/emacs-mcp/elisp")
(require 'emacs-mcp)
(emacs-mcp-mode 1)
```

**No nREPL needed!** The MCP server runs standalone via `clojure -X:mcp`.

## Development

### Start nREPL for development

```bash
clojure -M:nrepl
```

### Run tests

```bash
clojure -M:test
```

### Run MCP server directly

```bash
clojure -X:mcp
```

## Architecture

```
┌─────────────┐     MCP/STDIO      ┌─────────────┐
│   Claude    │ ◄────────────────► │  emacs-mcp  │
│   (AI)      │                    │  (Clojure)  │
└─────────────┘                    └──────┬──────┘
                                          │
                                          │ emacsclient --eval
                                          ▼
                                   ┌─────────────┐
                                   │   Emacs     │
                                   │  (daemon)   │
                                   │             │
                                   │ emacs-mcp.el│ ← NEW!
                                   └─────────────┘
```

## emacs-mcp.el - Emacs Package

The `elisp/` directory contains an Emacs package that enables **bidirectional** collaboration:

- **Memory**: Persistent notes, snippets, conventions, decisions per-project
- **Context**: Rich information about buffer, region, project, git
- **Workflows**: User-defined multi-step automations
- **Triggers**: Hooks and keybindings for automation

### Installation

```elisp
;; Add to load-path
(add-to-list 'load-path "/path/to/emacs-mcp/elisp")

;; Load and enable
(require 'emacs-mcp)
(emacs-mcp-mode 1)
```

### Keybindings (C-c m prefix)

| Key       | Command                    |
|-----------|----------------------------|
| `C-c m m` | Open transient menu        |
| `C-c m n` | Add note to memory         |
| `C-c m s` | Save region as snippet     |
| `C-c m c` | Add project convention     |
| `C-c m d` | Record architecture decision|
| `C-c m l` | Browse project memory      |
| `C-c m w` | Run workflow               |
| `C-c m h` | Show conversation history  |
| `C-c m x` | Show current context       |

### API for Claude

Claude can use these functions via `eval_elisp`:

```clojure
;; Get full context including memory
(ec/eval-elisp "(emacs-mcp-api-get-context)")

;; Add a note
(ec/eval-elisp "(emacs-mcp-api-memory-add \"note\" \"Remember this\")")

;; Query conventions
(ec/eval-elisp "(emacs-mcp-api-memory-query \"convention\")")

;; Run user workflow
(ec/eval-elisp "(emacs-mcp-api-run-workflow \"test-and-commit\")")
```

### Package Structure

```
elisp/
├── emacs-mcp.el           # Main entry, minor mode
├── emacs-mcp-memory.el    # Persistent JSON storage
├── emacs-mcp-context.el   # Context gathering
├── emacs-mcp-triggers.el  # Keybindings, hooks
├── emacs-mcp-transient.el # Transient menus
├── emacs-mcp-workflows.el # Workflow system
├── emacs-mcp-api.el       # Stable API for Claude
├── emacs-mcp-addons.el    # Addon system with lifecycle hooks
└── addons/                # Built-in and custom addons
    ├── emacs-mcp-addon-template.el  # Template for new addons
    ├── emacs-mcp-chroma.el          # Semantic search via Chroma + Ollama
    ├── emacs-mcp-cider.el           # CIDER + async nREPL
    ├── emacs-mcp-claude-code.el     # Claude Code CLI integration
    ├── emacs-mcp-docs.el            # Documentation generation
    ├── emacs-mcp-magit.el           # Git via Magit with shell fallback
    ├── emacs-mcp-melpazoid.el       # MELPA submission tools
    ├── emacs-mcp-org-ai.el          # org-ai conversation context
    ├── emacs-mcp-org-kanban.el      # Org-mode kanban boards
    ├── emacs-mcp-package-lint.el    # MELPA compliance tools
    ├── emacs-mcp-presentation.el    # Presentation mode
    ├── emacs-mcp-projectile.el      # Project management via Projectile
    ├── emacs-mcp-swarm.el           # Multi-agent orchestration
    └── emacs-mcp-vibe-kanban.el     # Cloud task management server
```

## Addon System

Modular integrations with other Emacs packages. Addons are **lazy-loaded** when target packages are detected and support lifecycle hooks for async initialization.

| Addon | Integration | Features |
|-------|-------------|----------|
| chroma | [Chroma](https://www.trychroma.com/) + [Ollama](https://ollama.com/) | Semantic memory search, vector embeddings, docker-compose management |
| cider | [CIDER](https://github.com/clojure-emacs/cider) | Async nREPL startup, auto-connect, isolated sessions for parallel agents |
| claude-code | [claude-code.el](https://github.com/karthink/claude-code) | Context injection for Claude CLI |
| docs | Built-in | Documentation generation utilities |
| magit | [Magit](https://magit.vc/) | Git status, staging, commits, push/pull, branch management (shell fallback) |
| melpazoid | [melpazoid](https://github.com/riscy/melpazoid) | MELPA recipe testing and submission |
| org-ai | [org-ai](https://github.com/rksm/org-ai) | AI conversation context |
| org-kanban | org-mode | Native Clojure org-mode parser for kanban boards |
| package-lint | [package-lint](https://github.com/purcell/package-lint) | MELPA compliance checking |
| presentation | Built-in | Presentation mode for demos |
| projectile | [Projectile](https://github.com/bbatsov/projectile) | Project info, file listing, search with ripgrep |
| swarm | vterm/eat | Multi-agent orchestration with presets, parallel Claude instances |
| vibe-kanban | [vibe-kanban](https://vibekanban.dev/) | Cloud task management server (npx subprocess) |

**Quick start:**
```elisp
;; Auto-load addons when packages are detected
(emacs-mcp-addons-auto-load)

;; Or always load specific addons on startup
(setq emacs-mcp-addon-always-load '(cider vibe-kanban))

;; For async nREPL startup
(setq emacs-mcp-cider-auto-start-nrepl t)
```

**Addon lifecycle hooks:**
- `:init` - Synchronous setup (keybindings, config)
- `:async-init` - Non-blocking startup (servers, subprocesses)
- `:shutdown` - Cleanup on unload

📖 **Documentation:**
- [Addon Development Guide](docs/addon-development.md)
- [Addon API Reference](docs/addon-api.md)
- [Full Addon Docs](docs/ADDONS.org)

## Tested & Working

All features verified through the Clojure MCP → Emacs integration:

| Feature | Status | 
|---------|--------|
| Context API | ✓ Full buffer/project/git/memory info |
| Memory persistence | ✓ Notes saved to JSON per-project |
| Memory query | ✓ Retrieves stored notes, conventions |
| Workflows | ✓ `quick-note`, `commit` registered |
| Notifications | ✓ Messages displayed in Emacs |
| Jump to file:line | ✓ Opens file with line highlight |
| Show in buffer | ✓ Creates buffer with content |
| Synergy functions | ✓ Full dev-tools + emacs-bridge integration |

```clojure
;; Example: Get full context with memory
(require '[emacs-mcp.synergy :as syn])
(syn/get-full-context!)
;; => {:buffer {...} :project {...} :git {...} :memory {:notes [...]}}

;; Jump to a specific location
(syn/jump-to! "src/myfile.clj" 42)

;; Show results in Emacs
(syn/show-in-buffer! "*Results*" "# Analysis\n..." "markdown-mode")
```

## Semantic Memory Search

Enable vector similarity search across your project memory using **Chroma** (vector database) and **Ollama** (local embeddings).

### Prerequisites

1. **Docker** and **docker-compose** (for Chroma)
2. **Ollama** with embedding model:
   ```bash
   # Install Ollama
   curl -fsSL https://ollama.com/install.sh | sh
   
   # Pull embedding model (768 dimensions)
   ollama pull nomic-embed-text
   ```

### Quick Start

```bash
# Start Chroma container
cd /path/to/emacs-mcp
docker compose up -d

# Verify health
curl http://localhost:8000/api/v2/heartbeat
```

### Configuration

Environment variables (set in shell, Emacs, or systemd):

| Variable | Default | Description |
|----------|---------|-------------|
| `CHROMA_HOST` | `localhost` | Chroma server host |
| `CHROMA_PORT` | `8000` | Chroma server port |
| `OLLAMA_HOST` | `http://localhost:11434` | Ollama API URL |

**Emacs configuration** (in `init.el` or `config.el`):

```elisp
;; Set before loading emacs-mcp
(setenv "CHROMA_HOST" "localhost")
(setenv "CHROMA_PORT" "8000")
(setenv "OLLAMA_HOST" "http://localhost:11434")

;; Load chroma addon
(require 'emacs-mcp-chroma nil t)
(when (featurep 'emacs-mcp-chroma)
  (emacs-mcp-chroma-mode 1))
```

### Usage

Once configured, use the `mcp_memory_search_semantic` tool:

```clojure
;; Search for conceptually similar entries
(mcp_memory_search_semantic {:query "authentication flow" :limit 5})

;; Filter by type
(mcp_memory_search_semantic {:query "error handling" :type "convention"})
```

The semantic search finds conceptually related entries even without exact keyword matches - "auth flow" will find entries about "login", "session management", etc.

### Fallback Behavior

If Chroma is unavailable, the system automatically falls back to local text-based search, ensuring memory queries always work.

## Meta: MCP Servers Editing MCP Servers

This project demonstrates an interesting recursive pattern: **an MCP server can be developed using another MCP server**.

### The Setup

| Server | Function | Tools Provided |
|--------|----------|----------------|
| **clojure-mcp** (mcp-dev₁) | Clojure development | read, edit, eval, grep, glob |
| **emacs-mcp** (mcp-emacs₂) | Emacs interaction | eval-elisp, list-buffers, find-file |

Both servers are *implemented* in Clojure, but they serve different *domains*:

```
┌─────────────────────────────────────────────────────────────┐
│                         Claude                              │
├─────────────────────────────┬───────────────────────────────┤
│      clojure-mcp            │         emacs-mcp             │
│   (dev-tools server)        │    (emacs-bridge server)      │
│                             │                               │
│  • read/edit Clojure files  │  • eval elisp                 │
│  • REPL evaluation          │  • buffer management          │
│  • project navigation       │  • file operations            │
├─────────────────────────────┴───────────────────────────────┤
│                    can edit ───────►                        │
│   clojure-mcp edits emacs-mcp source files                  │
└─────────────────────────────────────────────────────────────┘
```

### Naming Clarity (General Semantics)

To avoid confusion when discussing MCP servers at multiple levels:

1. **Index by function, not implementation**: 
   - "dev-tools server" vs "emacs-bridge server"
   - Not "the Clojure one" (ambiguous - both use Clojure)

2. **Use subscripts for instances**:
   - mcp₁ (dev tools), mcp₂ (emacs bridge)
   
3. **Distinguish layers**:
   | Layer | clojure-mcp | emacs-mcp |
   |-------|-------------|-----------|
   | Implementation | Clojure | Clojure |
   | Target domain | Clojure dev | Emacs control |
   | Provides tools for | Editing code | Controlling editor |

4. **The map ≠ territory**: The *name* "clojure-mcp" refers to its *target domain* (Clojure development), not its implementation language.

## Documentation

| Document | Description |
|----------|-------------|
| [Addon Development Guide](docs/addon-development.md) | How to create addons |
| [Addon API Reference](docs/addon-api.md) | Complete API documentation |
| [Contributing](docs/CONTRIBUTING.md) | Contribution guidelines |
| [Project Summary](docs/PROJECT_SUMMARY.md) | Architecture overview |
| [Implementation Summary](docs/IMPLEMENTATION_SUMMARY.md) | Technical details |
| [Telemetry](docs/telemetry.md) | Telemetry & metrics |
| [Resilience](docs/resilience.md) | Error handling patterns |
| [Validation](docs/validation.md) | Input validation |
| [Evaluator](docs/evaluator.md) | Elisp evaluation |

## License

MIT
