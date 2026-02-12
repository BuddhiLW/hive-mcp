# hive-mcp

**Your AI finally remembers.**

[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL--3.0-blue.svg)](LICENSE)
[![MCP](https://img.shields.io/badge/MCP-Compatible-green.svg)](https://modelcontextprotocol.io)

---

## The Problem

You explain your codebase to Claude. Architecture, constraints, patterns. Then you hit the context limit. New session. **Claude forgets everything.**

## The Solution

```
Session 1                         Session 2
───────────────────────────────────────────────────
You: "Our auth uses JWT..."       You: /catchup
Claude: *learns*                  Claude: "I remember:
         ↓                         - Auth uses JWT with refresh
     /wrap                         - Convention: validate at boundaries
         ↓                         What should we work on?"
    [Memory]  ────────────────►
```

Persistent, project-scoped memory with semantic search. Conventions, decisions, snippets — stored locally, never forgotten.

---

## What Sets hive-mcp Apart

| Capability | hive-mcp | Typical MCP servers |
|---|---|---|
| **Knowledge Graph** | Structural edges - how knowledge relate? | Flat key-value or vector-only |
| **Session Continuity** | `/wrap` crystallizes, `/catchup` reconstructs — zero-down re-explaining across sessions | Manual copy-paste or lost |
| **Multi-Agent Coordination** | Lings (planners) + drones (executors) with file claims, hivemind shouts, and a continuous production belt | Single-agent only |
| **Scoped Memory** | Hierarchical Context Retrieval (HCR) - project scoping - with TTL decay | Global namespace or none |
| **Extension Architecture** | `requiring-resolve` stubs with noop fallbacks - plug your extensions and play | Monolithic |

---

## Quick Start

### 1. Install

**Option A: Automated with [hive-mcp-cli](https://github.com/hive-agi/hive-mcp-cli) (Recommended)**

```bash
# Requires Go 1.21+
go install github.com/hive-agi/hive-mcp-cli/cmd/hive@latest
go install github.com/hive-agi/hive-mcp-cli/cmd/hive-setup-mcp@latest

# Register and let Claude guide setup
claude mcp add hive-setup --scope user -- hive-setup-mcp
claude
# Ask: "Help me setup hive-mcp"
```

**Option B: Manual**

```bash
export HIVE_MCP_DIR="$HOME/hive-mcp"
export BB_MCP_DIR="$HOME/bb-mcp"

git clone https://github.com/hive-agi/hive-mcp.git "$HIVE_MCP_DIR"
git clone https://github.com/hive-agi/bb-mcp.git "$BB_MCP_DIR"

claude mcp add hive --scope user -- "$HIVE_MCP_DIR/start-bb-mcp.sh"
```

### 2. Verify

```bash
claude mcp list | grep -q "hive" && echo "OK" || echo "FAILED"
```

### 3. Optional: Semantic Search

```bash
ollama pull nomic-embed-text      # Local embeddings
docker compose up -d              # Chroma vector DB
```

### Prerequisites

| Requirement | Version | Install |
|---|---|---|
| Claude Code | Latest | [claude.ai/download](https://claude.ai/download) |
| Babashka | 1.3+ | [babashka.org](https://babashka.org) |
| Java | 17+ | `apt install openjdk-17-jdk` |

**Optional**: 
- Emacs 28.1+ for swarm vterm UI and buffer integration. See [Emacs Configuration](https://github.com/hive-agi/hive-mcp/wiki/Emacs-Configuration). 
- Headless mode works without Emacs (but WIP for stability of headless - recommended to use Emacs as a dependency).

---

## 20 Consolidated Tools

| Tool | Purpose |
|---|---|
| `memory` | Persistent entries with semantic search, TTL decay, scoping |
| `kg` | Knowledge Graph — edges, subgraphs |
| `agent` | Spawn/kill/dispatch lings and drones |
| `wave` | Parallel drone dispatch with validation |
| `hivemind` | Shout/ask coordination between agents |
| `session` | Wrap, catchup, whoami, context store |
| `workflow` | Forge belt, FSM-driven production cycles |
| `kanban` | Task management with plan-to-kanban |
| `magit` | Git operations — status, stage, commit, push |
| `cider` | Clojure nREPL eval, doc, completions |
| `preset` | Agent presets — list, search, generate headers |
| `analysis` | Kondo lint, SCC metrics, complexity hotspots |
| `lsp` | Code analysis — callers, calls, namespace graph |
| `project` | Projectile — files, search, hierarchy scan |
| `emacs` | Eval elisp, buffers, notifications |
| `olympus` | Grid layout control for multi-agent UI |
| `agora` | Multi-agent deliberation and debates |
| `config` | Runtime configuration management |
| `migration` | KG/memory backup, restore, backend switching |
| `multi` | Meta-facade — batch operations |

Each tool is a consolidated namespace with subcommands (e.g., `memory add`, `memory query`, `kg traverse`).

---

## Architecture

```
Claude ──MCP──► hive-mcp (Clojure)
                    │
                    ├── Memory (Chroma vectors + scoped entries)
                    ├── Knowledge Graph (DataScript, Datalevin, or Datahike)
                    ├── Swarm (lings + drones + hivemind)
                    ├── Workflows for AIs (building blocks for repeatable behavior)
                    └── Extension Registry (requiring-resolve stubs)
                            │
                            └──► [optional] your extensions (e.g., plug openclaw, langchain, whatever etc)
```

---

## For LLMs

See [`CLAUDE.md`](CLAUDE.md) for project conventions, tool patterns, and memory usage guidelines.

## Documentation

| Resource | Description |
|---|---|
| **[Wiki](https://github.com/hive-agi/hive-mcp/wiki)** | Complete guides |
| [Installation](https://github.com/hive-agi/hive-mcp/wiki/Installation) | Detailed setup |
| [Troubleshooting](https://github.com/hive-agi/hive-mcp/wiki/Troubleshooting) | Common issues |

---

## Related

- **[bb-mcp](https://github.com/hive-agi/bb-mcp)** — Lightweight Babashka wrapper (~50MB RAM)
- **[mcp-clojure-sdk](https://github.com/hive-agi/mcp-clojure-sdk)** — Clojure MCP SDK

---

[AGPL-3.0-or-later](LICENSE)
