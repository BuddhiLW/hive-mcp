#!/bin/bash
# Periodic Chroma memory database backup
# Copies SQLite from Docker container, retains last 7 days

set -e

BACKUP_DIR="${BACKUP_DIR:-$HOME/backups/chroma}"
CONTAINER_NAME="${CHROMA_CONTAINER:-hive-mcp-chroma}"
CHROMA_DB_PATH="/data/chroma.sqlite3"
RETENTION_DAYS=7

# Create backup directory if needed
mkdir -p "$BACKUP_DIR"

# Check if container is running
if ! docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    echo "Error: Container '$CONTAINER_NAME' is not running" >&2
    exit 1
fi

# Create timestamped backup
BACKUP_FILE="$BACKUP_DIR/chroma-$(date +%Y%m%d).sqlite3"
docker cp "${CONTAINER_NAME}:${CHROMA_DB_PATH}" "$BACKUP_FILE"

# Prune backups older than retention period
find "$BACKUP_DIR" -name "chroma-*.sqlite3" -mtime +${RETENTION_DAYS} -delete

echo "Backup complete: $BACKUP_FILE"
ls -lh "$BACKUP_FILE"
