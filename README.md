# hive-mcp

**Your AI finally remembers.**

[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL--3.0-blue.svg)](LICENSE)
[![MCP](https://img.shields.io/badge/MCP-Compatible-green.svg)](https://modelcontextprotocol.io)
[![Emacs](https://img.shields.io/badge/Emacs-28.1+-purple.svg)](https://www.gnu.org/software/emacs/)

> What could be more absurd than to increase one’s supplies for the road when the journey itself is almost at an end?
> -- Cicero

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

Persistent, project-scoped memory with semantic search. Conventions, decisions, snippets—stored locally, never forgotten.

---

## Quick Start

### 1. Environment Setup

```bash
# Add to ~/.bashrc (adjust paths to your system)
export HIVE_MCP_DIR="$HOME/hive-mcp"
export BB_MCP_DIR="$HOME/bb-mcp"
```

### 2. Install

**Option A: Automated with [hive-mcp-cli](https://github.com/hive-agi/hive-mcp-cli) (Recommended)**
First, be sure you have setup the environment variables above, and be sure that you have the go executable install directory on your path. 
```bash
# 1. Install CLI and MCP server (requires Go 1.21+)
go install github.com/hive-agi/hive-mcp-cli/cmd/hive@latest
go install github.com/hive-agi/hive-mcp-cli/cmd/hive-setup-mcp@latest

# 2. Register MCP server with Claude
claude mcp add hive-setup --scope user -- hive-setup-mcp

# 3. Start Claude and ask: "Help me setup hive-mcp"
claude
```

**Option B: Manual**

```bash
git clone https://github.com/hive-agi/hive-mcp.git "$HIVE_MCP_DIR"
git clone https://github.com/hive-agi/bb-mcp.git "$BB_MCP_DIR"
```

### 3. Configure Emacs

Add to your Emacs config (`~/.emacs.d/init.el` or `~/.doom.d/config.el`):

```elisp
(add-to-list 'load-path (concat (getenv "HIVE_MCP_DIR") "/elisp"))
(require 'hive-mcp)
(hive-mcp-mode 1)
(server-start)  ; Required for emacsclient
```

**Full configs:** [Doom](https://github.com/hive-agi/hive-mcp/wiki/Emacs-Configuration#doom-emacs) | [Vanilla](https://github.com/hive-agi/hive-mcp/wiki/Emacs-Configuration#vanilla-emacs)

### 4. Start Emacs & Register

```bash
emacs --daemon
claude mcp add emacs --scope user -- "$HIVE_MCP_DIR/start-bb-mcp.sh"
```

### 5. Verify

```bash
claude mcp list | grep -q "emacs" && echo "SUCCESS: hive-mcp registered" || echo "FAILED: check installation"
```

---

## Prerequisites

| Requirement | Version | Install |
|-------------|---------|---------|
| Emacs | 28.1+ | [gnu.org/software/emacs](https://www.gnu.org/software/emacs/) |
| Claude Code | Latest | [claude.ai/download](https://claude.ai/download) |
| Babashka | 1.3+ | [babashka.org](https://babashka.org) |
| Java | 17+ | `apt install openjdk-17-jdk` |

---

## Features

**Persistent Memory** — Store conventions, decisions, snippets. Query by keyword or semantic similarity.

**Session Continuity** — `/wrap` saves context, `/catchup` restores it. No re-explaining.

**Multi-Agent Swarm** — Parallel Claude instances with specialized roles. File claims prevent conflicts.

**Emacs Integration** — 172 MCP tools for buffers, git, projects, Clojure REPL, org-mode.

---

## Architecture

```
Claude ──MCP──► hive-mcp (Clojure) ──emacsclient──► Emacs
                    │
                    ├── Memory (JSON + Chroma vectors)
                    ├── Swarm (parallel agents)
                    └── Hivemind (coordination)
```

---

## For LLMs

See [`CLAUDE.md`](CLAUDE.md) for project conventions, tool patterns, and memory usage guidelines.

---

## Documentation

| Resource | Description |
|----------|-------------|
| **[Wiki](https://github.com/hive-agi/hive-mcp/wiki)** | Complete guides |
| [Installation](https://github.com/hive-agi/hive-mcp/wiki/Installation) | Detailed setup options |
| [Emacs Config](https://github.com/hive-agi/hive-mcp/wiki/Emacs-Configuration) | Full Doom & vanilla configs |
| [Troubleshooting](https://github.com/hive-agi/hive-mcp/wiki/Troubleshooting) | Common issues |
| [Tools Reference](https://github.com/hive-agi/hive-mcp/wiki/Tools-Reference) | All 172 tools |

---

## Optional: Semantic Search

```bash
ollama pull nomic-embed-text      # Local embeddings
docker compose up -d              # Chroma vector DB
```

---

## Related

- **[bb-mcp](https://github.com/hive-agi/bb-mcp)** — Lightweight wrapper (~50MB RAM)
- **[mcp-clojure-sdk](https://github.com/hive-agi/mcp-clojure-sdk)** — Clojure MCP SDK

---

[AGPL-3.0-or-later](LICENSE)
