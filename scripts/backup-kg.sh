#!/bin/bash
# Periodic Knowledge Graph (Datahike) backup via nREPL
# Exports KG edges to EDN using the running JVM (no cold start)
# Retains last 14 days, skips if recent backup exists

set -e

BACKUP_DIR="${KG_BACKUP_DIR:-$HOME/backups/kg}"
NREPL_PORT="${NREPL_PORT:-7910}"
RETENTION_DAYS=14
MIN_AGE_HOURS=8

# Create backup directory if needed
mkdir -p "$BACKUP_DIR"

# Check if recent backup exists
LATEST_BACKUP=$(find "$BACKUP_DIR" -name "kg-datahike-*.edn" -type f -printf '%T@ %p\n' 2>/dev/null | sort -n | tail -1 | cut -d' ' -f2-)
if [[ -n "$LATEST_BACKUP" ]]; then
    BACKUP_AGE_SECONDS=$(( $(date +%s) - $(stat -c %Y "$LATEST_BACKUP") ))
    MIN_AGE_SECONDS=$(( MIN_AGE_HOURS * 3600 ))
    if [[ $BACKUP_AGE_SECONDS -lt $MIN_AGE_SECONDS ]]; then
        HOURS_OLD=$(( BACKUP_AGE_SECONDS / 3600 ))
        echo "Skipping: Latest backup is ${HOURS_OLD}h old (< ${MIN_AGE_HOURS}h threshold)"
        echo "  $LATEST_BACKUP"
        exit 0
    fi
fi

# Check if nREPL is reachable
if ! nc -z localhost "$NREPL_PORT" 2>/dev/null; then
    echo "Error: nREPL not reachable on port $NREPL_PORT" >&2
    exit 1
fi

BACKUP_FILE="$BACKUP_DIR/kg-datahike-$(date +%Y%m%dT%H%M%S).edn"

# Export KG via nREPL (hot JVM — no cold start, ~1s)
# Uses bencode to send eval op to running nREPL server
echo "(do (require '[hive-mcp.knowledge-graph.migration :as mig]) (let [r (mig/export-to-file! \"$BACKUP_FILE\")] (str \"Backup: \" (:counts r))))" | \
    nrepl-client localhost "$NREPL_PORT" 2>/dev/null || {
    # Fallback: use bb nrepl-client if available
    echo "(do (require '[hive-mcp.knowledge-graph.migration :as mig]) (let [r (mig/export-to-file! \"$BACKUP_FILE\")] (str \"Backup: \" (:counts r))))" | \
        bb -e "(require '[babashka.nrepl-client :as nrepl]) (let [r (nrepl/message {:host \"localhost\" :port $NREPL_PORT} {:op \"eval\" :code (slurp *in*)})] (doseq [m r] (when (:value m) (println (:value m)))))" 2>/dev/null || {
        echo "Warning: No nREPL client available, falling back to MCP tool"
        # Last resort: use the MCP migration tool via curl if server is running
        exit 1
    }
}

# Verify backup was created and has content
if [[ ! -f "$BACKUP_FILE" ]] || [[ ! -s "$BACKUP_FILE" ]]; then
    echo "Error: Backup file missing or empty: $BACKUP_FILE" >&2
    exit 1
fi

# Prune old backups
find "$BACKUP_DIR" -name "kg-datahike-*.edn" -mtime +${RETENTION_DAYS} -delete

echo "Backup complete: $BACKUP_FILE"
ls -lh "$BACKUP_FILE"
