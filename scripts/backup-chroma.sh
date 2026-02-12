#!/bin/bash
# Periodic Chroma memory database backup
# Copies SQLite from Docker container, retains last 7 days
# Skips if latest backup is less than MIN_AGE_HOURS old

set -e

BACKUP_DIR="${BACKUP_DIR:-$HOME/backups/chroma}"
CONTAINER_NAME="${CHROMA_CONTAINER:-hive-mcp-chroma}"
CHROMA_DB_PATH="/data/chroma.sqlite3"
RETENTION_DAYS=7
MIN_AGE_HOURS=8

# Create backup directory if needed
mkdir -p "$BACKUP_DIR"

# Check if recent backup exists (skip if less than MIN_AGE_HOURS old)
LATEST_BACKUP=$(find "$BACKUP_DIR" -name "chroma-*.sqlite3" -type f -printf '%T@ %p\n' 2>/dev/null | sort -n | tail -1 | cut -d' ' -f2-)
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

# Check if container is running
if ! docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    echo "Error: Container '$CONTAINER_NAME' is not running" >&2
    exit 1
fi

# Create timestamped backup (with hour for multiple per day)
BACKUP_FILE="$BACKUP_DIR/chroma-$(date +%Y%m%d-%H%M).sqlite3"
docker cp "${CONTAINER_NAME}:${CHROMA_DB_PATH}" "$BACKUP_FILE"

# Prune backups older than retention period
find "$BACKUP_DIR" -name "chroma-*.sqlite3" -mtime +${RETENTION_DAYS} -delete

echo "Backup complete: $BACKUP_FILE"
ls -lh "$BACKUP_FILE"
