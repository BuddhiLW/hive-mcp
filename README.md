# emacs-mcp

**Your AI finally remembers.**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![MCP](https://img.shields.io/badge/MCP-Compatible-green.svg)](https://modelcontextprotocol.io)
[![Emacs](https://img.shields.io/badge/Emacs-28.1+-purple.svg)](https://www.gnu.org/software/emacs/)

> *"Every other AI tool forgets everything between sessions. This one doesn't."*

---

## The Problem

You're deep in a debugging session with Claude. You've explained the architecture, the constraints, the patterns. Then you hit the context limit. New session. **Claude forgets everything.**

Or worse: you come back tomorrow. Same project. Same questions. Same explanations. **Every. Single. Time.**

## The Solution

```
Session 1                         Session 2
─────────────────────────────────────────────────
You: "Our auth uses JWT with..."
Claude: *learns*                  You: /catchup
         ↓                        Claude: "I see from memory:
     /wrap                         - Auth uses JWT with refresh
         ↓                         - Convention: validate at boundaries
    [Memory]  ─────────────────►   - Decision: chose bcrypt over argon2
                                   What should we work on?"
```

**emacs-mcp** gives Claude persistent, project-scoped memory with semantic search. Conventions, decisions, snippets—stored locally, queryable by meaning, never forgotten.

---

## Key Features

### 1. Persistent Project Memory

```elisp
;; Claude stores what it learns
(mcp_memory_add :type "convention"
                :content "Always validate at API boundaries"
                :tags ["security" "validation"])

;; And retrieves it next session
(mcp_memory_search_semantic :query "input validation patterns")
;; => Finds the convention, even without exact keywords
```

**Memory types:** notes, conventions, decisions, snippets
**Durations:** ephemeral (1 day) → short (7 days) → long (90 days) → permanent
**Search:** Keyword or semantic (via local Ollama embeddings)

### 2. Session Continuity

```bash
# End of session
/wrap   # Stores accomplishments, decisions, conventions

# Start of next session
/catchup   # Restores full context from memory
```

No more re-explaining your codebase. No more lost context.

### 3. Multi-Agent Swarm

```elisp
;; Spawn parallel Claude instances with specialized roles
(swarm_spawn :name "tester" :preset "tdd")
(swarm_spawn :name "reviewer" :preset "code-review")

;; Dispatch tasks
(swarm_dispatch :slave-id "tester" :prompt "Write tests for auth.clj")

;; Human-in-the-loop for risky operations
;; File claims prevent conflicts between agents
```

**Conflict Detection:** *Pessimistic Concurrency Control* with *Advisory Locking* — agents claim files before editing, conflicts get queued (not blocked), core.logic reasons about dependencies and deadlocks. See [SWARM_CONFLICT_DETECTION.md](docs/SWARM_CONFLICT_DETECTION.md).

### 4. Full Emacs Integration

50+ MCP tools for buffer management, git (Magit), projects (Projectile), Clojure (CIDER), and org-mode. Claude controls your editor, not just reads files.

---

## Prerequisites

| Requirement | Version | Install Guide |
|-------------|---------|---------------|
| **Emacs** | 28.1+ | [gnu.org/software/emacs](https://www.gnu.org/software/emacs/) |
| **Claude Code CLI** | Latest | [claude.ai/download](https://claude.ai/download) |
| **Java** | 17+ | `sudo apt install openjdk-17-jdk` |
| **Clojure CLI** | 1.11+ | [clojure.org/guides/install_clojure](https://clojure.org/guides/install_clojure) |

**Optional (for semantic search):**
- [Ollama](https://ollama.ai) - local embeddings
- [Docker](https://docker.com) - for Chroma vector DB

---

## Installation

Choose your setup:

### Option A: Lightweight via bb-mcp (Recommended)

**[bb-mcp](https://github.com/BuddhiLW/bb-mcp)** is a Babashka wrapper that uses ~50MB RAM vs ~500MB for direct JVM. Multiple Claude instances share one Emacs connection.

```bash
# Install bb-mcp (includes emacs-mcp as dependency)
git clone https://github.com/BuddhiLW/bb-mcp.git
cd bb-mcp

# Follow bb-mcp setup instructions
# It will connect to emacs-mcp running on port 7910
```

See [bb-mcp README](https://github.com/BuddhiLW/bb-mcp) for complete setup.

### Option B: Direct Clojure (Full Control)

#### Step 1: Clone & Install Dependencies

```bash
git clone https://github.com/BuddhiLW/emacs-mcp.git
cd emacs-mcp
clojure -P  # Download all dependencies (may take a minute)
```

#### Step 2: Configure Emacs

Add to your Emacs config (`~/.emacs.d/init.el` or Doom: `~/.doom.d/config.el`):

```elisp
;; Add emacs-mcp to load path
(add-to-list 'load-path "/path/to/emacs-mcp/elisp")
(add-to-list 'load-path "/path/to/emacs-mcp/elisp/addons")

;; Load core
(require 'emacs-mcp)
(emacs-mcp-mode 1)

;; Load addons you want (all optional)
(require 'emacs-mcp-magit nil t)      ;; Git integration
(require 'emacs-mcp-projectile nil t) ;; Project navigation
(require 'emacs-mcp-cider nil t)      ;; Clojure REPL
(require 'emacs-mcp-swarm nil t)      ;; Multi-agent support

;; REQUIRED: Start Emacs server for emacsclient
(server-start)
```

#### Step 3: Start Emacs Daemon

```bash
# Start Emacs in daemon mode
emacs --daemon

# Verify it's running
emacsclient -e '(emacs-version)'
# Should print your Emacs version
```

#### Step 4: Register with Claude Code

```bash
# Automatic registration
claude mcp add emacs-mcp --scope user -- \
  clojure -X:mcp :project-dir '"/path/to/your/project"'

# Verify registration
claude mcp list
# Should show: emacs-mcp
```

**Or manually** add to `~/.claude.json`:

```json
{
  "mcpServers": {
    "emacs-mcp": {
      "command": "clojure",
      "args": ["-X:mcp"],
      "cwd": "/path/to/emacs-mcp"
    }
  }
}
```

#### Step 5: Verify Installation

```bash
# Start Claude Code
claude

# Test Emacs connection
> Check if Emacs is available using emacs_status

# Expected response:
# {:emacs-available true, :server-running true, ...}
```

---

## Optional: Enable Semantic Search

For vector-based memory search (find by meaning, not just keywords):

```bash
# 1. Install Ollama and pull embedding model
ollama pull nomic-embed-text

# 2. Start Chroma vector database
cd /path/to/emacs-mcp
docker compose up -d

# 3. Verify Chroma is running
curl http://localhost:8000/api/v1/heartbeat
# Should return: {"nanosecond heartbeat": ...}
```

Without semantic search, memory still works with keyword/tag queries.

---

## Troubleshooting

### "Cannot connect to Emacs server"

```bash
# Check if Emacs daemon is running
pgrep -f "emacs --daemon"

# If not, start it
emacs --daemon

# Check server socket exists
ls /run/user/$(id -u)/emacs/server
# or
ls /tmp/emacs$(id -u)/server
```

### "clojure: command not found"

Install Clojure CLI:
```bash
# Linux
curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
chmod +x linux-install.sh
sudo ./linux-install.sh

# macOS
brew install clojure/tools/clojure
```

### "MCP server failed to start"

```bash
# Download dependencies first
cd /path/to/emacs-mcp
clojure -P

# Test server starts manually
clojure -X:mcp
# Should print: "Starting emacs-mcp server..."
# Ctrl+C to stop
```

### "emacs-mcp.el not found"

Verify your load-path in Emacs:
```elisp
M-x eval-expression RET
(member "/path/to/emacs-mcp/elisp" load-path)
;; Should return non-nil
```

### Memory not persisting

Check memory directory exists:
```bash
ls ~/.emacs-mcp/memory/
# Should contain .json files after first use
```

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         Claude                              │
│                           │                                 │
│                     MCP Protocol                            │
│                           │                                 │
├───────────────────────────┼─────────────────────────────────┤
│                           ▼                                 │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              emacs-mcp (Clojure)                    │   │
│  │                                                     │   │
│  │  Memory ──► JSON + Chroma (semantic)               │   │
│  │  Channel ──► TCP:9999 (push events)                │   │
│  │  Swarm ──► Parallel Claude instances               │   │
│  │  Hivemind ──► Agent coordination                   │   │
│  └─────────────────────────────────────────────────────┘   │
│                           │                                 │
│                     emacsclient                             │
│                           │                                 │
│                           ▼                                 │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                 Emacs (daemon)                      │   │
│  │                                                     │   │
│  │  emacs-mcp.el ──► Memory, Context, Workflows       │   │
│  │  Addons ──► Magit, CIDER, Projectile, Org...       │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## Tools Reference

<details>
<summary><b>Memory & Context</b> (click to expand)</summary>

| Tool | Description |
|------|-------------|
| `mcp_memory_add` | Store notes, conventions, decisions, snippets |
| `mcp_memory_query` | Query by type with optional tag filter |
| `mcp_memory_search_semantic` | Vector similarity search |
| `mcp_memory_promote` / `demote` | Adjust retention duration |
| `mcp_get_context` | Full context (buffer, project, git, memory) |

</details>

<details>
<summary><b>Workflows & Automation</b></summary>

| Tool | Description |
|------|-------------|
| `mcp_run_workflow` | Execute named workflow with args |
| `mcp_list_workflows` | List available workflows |
| `mcp_notify` | Show notification in Emacs |

**Built-in workflows:** `wrap`, `catchup`, `quick-note`, `commit`

</details>

<details>
<summary><b>Swarm (Multi-Agent)</b></summary>

| Tool | Description |
|------|-------------|
| `swarm_spawn` | Create Claude instance with preset |
| `swarm_dispatch` | Send task to agent |
| `swarm_collect` | Get task result |
| `swarm_broadcast` | Send to all agents |
| `swarm_status` | View all agents and tasks |
| `hivemind_shout` | Broadcast to coordinator |
| `hivemind_ask` | Request human decision |

**Presets:** tdd, code-review, docs, clarity, security, hivemind

</details>

<details>
<summary><b>Git (Magit)</b></summary>

| Tool | Description |
|------|-------------|
| `magit_status` | Full repo status |
| `magit_stage` / `commit` / `push` | Standard git ops |
| `magit_branches` | Branch management |
| `magit_log` / `diff` | History and changes |

</details>

<details>
<summary><b>Emacs Core</b></summary>

| Tool | Description |
|------|-------------|
| `eval_elisp` | Execute arbitrary elisp |
| `list_buffers` / `get_buffer_content` | Buffer ops |
| `find_file` / `save_buffer` | File ops |
| `projectile_*` | Project navigation |
| `cider_*` | Clojure REPL |

</details>

<details>
<summary><b>Org-Mode</b></summary>

| Tool | Description |
|------|-------------|
| `org_clj_parse` | Parse org to JSON |
| `org_kanban_render` | Visual kanban board |
| `mcp_kanban_*` | Task management |

</details>

---

## Why Emacs?

Every "programmable editor" eventually reinvents:
- A config language (that becomes Turing complete)
- A plugin system (with lifecycle hooks)
- An eval mechanism (for "dynamic" features)
- A REPL (for debugging)

...and at that point you've just built a worse Lisp.

**McCarthy figured this out in 1958.** Emacs is the editor that didn't fight it.

| VSCode | Emacs |
|--------|-------|
| Extension API → LSP → JSON-RPC → Sandbox | `(eval elisp)` → done |
| Security boundary, creativity constraint | Everything is data |

LLMs need exactly what Lisp provides: homoiconicity, runtime metaprogramming, data-as-code. The "weird" features are suddenly the killer features.

---

## Documentation

| Document | Description |
|----------|-------------|
| [Tool Reference](docs/TOOLS.md) | Complete tool documentation |
| [Addon Guide](docs/addon-development.md) | Create custom addons |
| [Architecture](docs/PROJECT_SUMMARY.md) | Technical deep-dive |
| [Conflict Detection](docs/SWARM_CONFLICT_DETECTION.md) | Logic-based agent coordination |
| [Hivemind Demo](docs/DEMO_LING_DIALOGUE.md) | Multi-agent philosophical dialogue |
| [Contributing](docs/CONTRIBUTING.md) | How to contribute |

---

## Related Projects

- **[bb-mcp](https://github.com/BuddhiLW/bb-mcp)** - Lightweight Babashka MCP wrapper (~50MB vs ~500MB)
- **[clojure-mcp](https://github.com/BuddhiLW/clojure-mcp)** - Standalone Clojure MCP tools

---

## License

MIT

---

<p align="center">
  <i>"Lisp was dismissed as 'academic' for decades. Now LLMs need exactly what Lisp provides."</i>
</p>
