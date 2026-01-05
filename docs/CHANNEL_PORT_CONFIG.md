# Channel Port Configuration

## Overview

The bidirectional channel uses TCP for Emacs↔Clojure push-based communication. The port is configurable via environment variable.

## Configuration

```bash
# Default: 9998
export EMACS_MCP_CHANNEL_PORT=9998
```

## How It Works

```
start-mcp.sh
  ├─ Set EMACS_MCP_CHANNEL_PORT (default 9998)
  ├─ Kill stale process: fuser -k ${PORT}/tcp
  └─ Start clojure -X:mcp
       └─ server.clj reads env var, binds to port

Emacs (separate process)
  └─ emacs-mcp-channel.el reads same env var
       └─ Connects to matching port
```

## Files Involved

| File | Role |
|------|------|
| `start-mcp.sh` | Sets env var, kills stale port |
| `src/emacs_mcp/server.clj` | Reads env var, starts channel server |
| `elisp/emacs-mcp-channel.el` | Reads env var, connects to server |

## Design Decision (2026-01-03)

**Chosen approach**: Environment variable + kill on startup

**Rationale**:
- Simple: 3 small changes, follows existing pattern (like `CHROMA_PORT`)
- Sufficient for single-instance: emacs-mcp is designed as unique instance
- Handles stale processes: `fuser -k` cleans up zombie processes

**Architecture context**:
- emacs-mcp is the heavyweight JVM (unique instance)
- Multiple lightweight bb-mcp servers connect to it via nREPL
- Channel port conflicts only occur with stale processes from previous runs

**Future improvements** (if multi-instance support needed):
- Port file discovery (`.channel-port`)
- Dynamic port allocation
- Unix socket instead of TCP
