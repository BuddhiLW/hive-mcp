---
type: convention
tags: [dependencies, prerequisites, infrastructure, scope:project:hive-mcp]
duration: permanent
---

# hive-mcp Dependency Tree

Complete dependency structure for running hive-mcp. Use this for troubleshooting startup issues or onboarding new environments.

## Runtime Stack (required)

```
+-- Java 17+ --------------- JVM for Clojure runtime
+-- Clojure CLI 1.11+ ------ deps.edn dependency management
+-- Babashka 1.3+ ---------- bb-mcp lightweight proxy (~50MB vs 500MB JVM)
+-- Emacs 28.1+ ------------ Must run in daemon mode (emacs --daemon)
+-- Docker 20+ ------------- Container runtime for infrastructure
```

### Why each is needed:
- **Java 17+**: Clojure compiles to JVM bytecode; required for nREPL server
- **Clojure CLI**: Resolves deps.edn dependencies, starts nREPL on port 7910
- **Babashka**: Provides fast-startup MCP proxy that Claude Code connects to; avoids 30s JVM cold start
- **Emacs daemon**: MCP tools call emacsclient; daemon must be running first
- **Docker**: Runs Chroma, Ollama, and optional observability stack

## Infrastructure Services

```
+-- Chroma 0.4.24 ---------- Vector database (required, port 8000)
+-- Ollama ----------------- Local LLM runtime (required for embeddings)
|   +-- nomic-embed-text --- Embedding model for semantic search
+-- OpenRouter API --------- Cloud LLM delegation (optional)
```

### Why each is needed:
- **Chroma**: Stores memory entries, presets, kanban tasks with vector embeddings
- **Ollama + nomic-embed-text**: Generates embeddings for semantic search; Chroma calls Ollama's `/api/embeddings` endpoint
- **OpenRouter**: Optional; enables `delegate_drone` and `agent_delegate` to use cloud LLMs

## Emacs Packages

```
+-- hive-mcp.el ------------ Core MCP integration (websocket, channels)
+-- hive-mcp-magit.el ------ Git operations via Magit
+-- hive-mcp-cider.el ------ Clojure REPL via CIDER
+-- hive-mcp-projectile.el - Project navigation and file discovery
+-- hive-mcp-swarm.el ------ Swarm ling management (spawn, dispatch, collect)
+-- claude-code-ide.el ----- vterm terminals for lings, IDE integration
```

### Why each is needed:
- **hive-mcp.el**: Core protocol handler; all other packages depend on it
- **hive-mcp-magit.el**: Enables `magit_status`, `magit_commit`, `magit_push` tools
- **hive-mcp-cider.el**: Enables `cider_eval_silent`, `cider_doc`, REPL integration
- **hive-mcp-projectile.el**: Enables `projectile_files`, `projectile_search` tools
- **hive-mcp-swarm.el**: Enables `swarm_spawn`, `swarm_dispatch`, `swarm_collect`
- **claude-code-ide.el**: Provides vterm buffer management for ling terminals

## Repositories

```
+-- hive-mcp --------------- Main repo (clone --recursive for submodules)
|   +-- mcp-clojure-sdk --- Submodule: Clojure MCP protocol implementation
+-- bb-mcp ----------------- Babashka MCP proxy (separate repo)
```

### Clone commands:
```bash
git clone --recursive https://github.com/lagesguitars/hive-mcp.git
git clone https://github.com/lagesguitars/bb-mcp.git
```

## Optional Docker Profiles

```
+-- observability ---------- Prometheus, Grafana, Loki, Promtail
+-- milvus ----------------- Alternative vector DB for large deployments
```

### Usage:
```bash
# Core only
docker-compose up -d

# With observability
docker-compose --profile observability up -d

# With Milvus instead of Chroma
docker-compose --profile milvus up -d
```

## Startup Order

1. `docker-compose up -d` (Chroma, Ollama)
2. `ollama pull nomic-embed-text` (if not already pulled)
3. `emacs --daemon`
4. `clj -M:nrepl` (starts nREPL on 7910)
5. Claude Code connects via bb-mcp proxy
