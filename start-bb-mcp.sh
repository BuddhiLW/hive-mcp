#!/bin/bash
# Start bb-mcp - Lightweight MCP server using Babashka
#
# This is an alternative to start-mcp.sh (JVM) for environments
# where memory is constrained or fast startup is needed.
#
# Memory: ~50MB vs ~500MB (JVM)
# Startup: ~5ms vs ~2-3s (JVM)
#
# Tools provided:
#   - bash: Execute shell commands
#   - read_file: Read file contents
#   - file_write: Write files
#   - glob_files: Find files by pattern
#   - grep: Search content with ripgrep
#   - clojure_eval: Evaluate Clojure (requires shared nREPL)
#
# For full Emacs integration (eval_elisp, buffers, memory, kanban),
# use start-mcp.sh instead.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Ensure babashka is available
if ! command -v bb &> /dev/null; then
    echo "Error: babashka (bb) not found in PATH" >&2
    echo "Install from: https://babashka.org/" >&2
    exit 1
fi

# bb-mcp location - can be overridden via BB_MCP_DIR
BB_MCP_DIR="${BB_MCP_DIR:-$HOME/PP/bb-mcp}"

if [[ ! -d "$BB_MCP_DIR" ]]; then
    echo "Error: bb-mcp not found at $BB_MCP_DIR" >&2
    echo "Clone from: https://github.com/your-user/bb-mcp" >&2
    echo "Or set BB_MCP_DIR environment variable" >&2
    exit 1
fi

# nREPL configuration for clojure_eval
# Port resolution: BB_MCP_NREPL_PORT > .nrepl-port file > 7888
export BB_MCP_NREPL_PORT="${BB_MCP_NREPL_PORT:-7910}"

# Project directory for .nrepl-port lookup
export BB_MCP_PROJECT_DIR="${BB_MCP_PROJECT_DIR:-$SCRIPT_DIR}"

cd "$BB_MCP_DIR"

# Run bb-mcp
exec bb -m bb-mcp.core
