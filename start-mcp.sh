#!/bin/bash
# Start the Hive MCP server
# This script is called by Claude's MCP configuration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="${HOME}/.config/hive-mcp"
LOCK_FILE="${CONFIG_DIR}/starting.lock"
LOG_FILE="${CONFIG_DIR}/server.log"

# Ensure config directory exists
mkdir -p "$CONFIG_DIR"

# Cleanup lock file on exit
cleanup() {
    rm -f "$LOCK_FILE"
}
trap cleanup EXIT

# Create lock file immediately (signals "I'm starting" to bb-mcp)
echo "$$:$(date +%s)" > "$LOCK_FILE"

# Ensure emacsclient is available
if ! command -v emacsclient &> /dev/null; then
    echo "Error: emacsclient not found in PATH" >&2
    exit 1
fi

# Check if Emacs server is running
if ! emacsclient -e "t" &> /dev/null; then
    echo "Warning: Emacs server not running. Start Emacs with (server-start)" >&2
fi

cd "$SCRIPT_DIR"

# Chroma configuration - override via environment or Emacs config
# These can be set in your shell profile, Emacs config, or systemd unit
export CHROMA_HOST="${CHROMA_HOST:-localhost}"
export CHROMA_PORT="${CHROMA_PORT:-8000}"
export OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"

# Channel port for bidirectional Emacs communication
export HIVE_MCP_CHANNEL_PORT="${HIVE_MCP_CHANNEL_PORT:-9998}"

# nREPL port - embedded in MCP server for bb-mcp tool forwarding
export HIVE_MCP_NREPL_PORT="${HIVE_MCP_NREPL_PORT:-7910}"

# Check if a healthy hive-mcp is already running
# (bb-mcp might have spawned one already)
if timeout 1 bash -c "echo > /dev/tcp/localhost/${HIVE_MCP_NREPL_PORT}" 2>/dev/null; then
    echo "hive-mcp already running on port ${HIVE_MCP_NREPL_PORT}" >&2
    # Don't kill it! Just exit - bb-mcp will handle tool forwarding
    # This prevents killing a healthy server spawned by bb-mcp
    rm -f "$LOCK_FILE"
    exit 0
fi

# Kill any stale processes on our ports before starting
# Only reached if no healthy server is running
fuser -k "${HIVE_MCP_CHANNEL_PORT}/tcp" 2>/dev/null || true
fuser -k "${HIVE_MCP_NREPL_PORT}/tcp" 2>/dev/null || true

# Log startup
echo "=== Starting hive-mcp at $(date) ===" >> "$LOG_FILE"

# Run the MCP server
exec clojure -X:mcp
