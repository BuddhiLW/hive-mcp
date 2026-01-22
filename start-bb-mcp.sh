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
#
# Usage:
#   ./start-bb-mcp.sh          # Normal startup
#   ./start-bb-mcp.sh --check  # Pre-flight check only (no server)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="${HOME}/.config/hive-mcp/server.log"

# Colors for terminal output (stderr only, stdout is for MCP JSON)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

err() { echo -e "${RED}ERROR:${NC} $*" >&2; }
warn() { echo -e "${YELLOW}WARN:${NC} $*" >&2; }
info() { echo -e "${GREEN}INFO:${NC} $*" >&2; }

# Ensure babashka is available
if ! command -v bb &> /dev/null; then
    err "babashka (bb) not found in PATH"
    echo "  Install from: https://babashka.org/" >&2
    exit 1
fi

# bb-mcp location - can be overridden via BB_MCP_DIR
BB_MCP_DIR="${BB_MCP_DIR:-$HOME/PP/bb-mcp}"

if [[ ! -d "$BB_MCP_DIR" ]]; then
    err "bb-mcp not found at $BB_MCP_DIR"
    echo "  Clone from: https://github.com/hive-agi/bb-mcp" >&2
    echo "  Or set BB_MCP_DIR environment variable" >&2
    exit 1
fi

# hive-mcp location (for pre-flight checks)
HIVE_MCP_DIR="${HIVE_MCP_DIR:-$SCRIPT_DIR}"

# nREPL configuration for clojure_eval
export BB_MCP_NREPL_PORT="${BB_MCP_NREPL_PORT:-7910}"
export BB_MCP_PROJECT_DIR="${BB_MCP_PROJECT_DIR:-$SCRIPT_DIR}"
export HIVE_MCP_DIR

# Pre-flight compilation check
check_hive_mcp_compiles() {
    info "Checking hive-mcp compilation..."

    if [[ ! -d "$HIVE_MCP_DIR" ]]; then
        err "hive-mcp not found at $HIVE_MCP_DIR"
        return 1
    fi

    # Try to compile the main namespace
    local output
    if ! output=$(cd "$HIVE_MCP_DIR" && clojure -M:dev -e "(require 'hive-mcp.server) (println :ok)" 2>&1); then
        err "hive-mcp compilation failed!"
        echo "" >&2
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" >&2
        echo "$output" >&2
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" >&2
        echo "" >&2
        echo "Fix the error above, then retry." >&2
        return 1
    fi

    info "Compilation OK"
    return 0
}

# Check if hive-mcp nREPL is already running
check_nrepl_running() {
    nc -z localhost "$BB_MCP_NREPL_PORT" 2>/dev/null
}

# Show recent errors from server log
show_recent_errors() {
    if [[ -f "$LOG_FILE" ]]; then
        local errors
        errors=$(tail -50 "$LOG_FILE" | grep -E "(Error|Exception|error|exception|Unable to resolve)" | tail -10)
        if [[ -n "$errors" ]]; then
            err "Recent errors from $LOG_FILE:"
            echo "" >&2
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" >&2
            echo "$errors" >&2
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" >&2
            echo "" >&2
            echo "Full log: $LOG_FILE" >&2
        fi
    fi
}

# Handle --check flag
if [[ "$1" == "--check" ]]; then
    echo "Running pre-flight checks..." >&2
    echo "" >&2

    # Check bb-mcp
    info "bb-mcp directory: $BB_MCP_DIR"
    if [[ -f "$BB_MCP_DIR/bb.edn" ]]; then
        info "bb.edn found"
    else
        err "bb.edn not found in $BB_MCP_DIR"
        exit 1
    fi

    # Check hive-mcp
    info "hive-mcp directory: $HIVE_MCP_DIR"

    # Check nREPL
    if check_nrepl_running; then
        info "nREPL already running on port $BB_MCP_NREPL_PORT"
    else
        warn "nREPL not running on port $BB_MCP_NREPL_PORT (will auto-spawn)"

        # If not running, check compilation
        if ! check_hive_mcp_compiles; then
            exit 1
        fi
    fi

    echo "" >&2
    info "All checks passed!"
    exit 0
fi

# Start hive-mcp if not running
start_hive_mcp_if_needed() {
    if check_nrepl_running; then
        return 0  # Already running
    fi

    # Pre-flight compilation check
    if ! check_hive_mcp_compiles; then
        exit 1
    fi

    # Start hive-mcp server
    info "Starting hive-mcp server..."
    mkdir -p "$(dirname "$LOG_FILE")"

    # Start in background and wait for port
    (cd "$HIVE_MCP_DIR" && nohup clojure -X:mcp >> "$LOG_FILE" 2>&1 &)

    # Wait for server to be ready (max 30 seconds)
    local waited=0
    while [ $waited -lt 30 ]; do
        if check_nrepl_running; then
            info "hive-mcp ready on port $BB_MCP_NREPL_PORT"
            return 0
        fi
        sleep 1
        waited=$((waited + 1))
    done

    err "hive-mcp failed to start within 30 seconds"
    show_recent_errors
    exit 1
}

# Normal startup
cd "$BB_MCP_DIR"

# Ensure hive-mcp is running before starting bb-mcp
start_hive_mcp_if_needed

# Run bb-mcp (server is guaranteed to be running now)
exec bb -m bb-mcp.core
