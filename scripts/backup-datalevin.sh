#!/bin/bash
# Periodic Datalevin (LMDB) knowledge graph backup
# Copies data/kg/datalevin/ as tar.gz, retains last 7 days
# Skips if latest backup is less than MIN_AGE_HOURS old
#
# LMDB note: LMDB is crash-consistent and safe to copy while running,
# but backups are scheduled at 3:15am when the system is typically idle.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
DATALEVIN_DIR="$PROJECT_DIR/data/kg/datalevin"
BACKUP_DIR="${BACKUP_DIR:-$HOME/backups/datalevin}"
RETENTION_DAYS=7
MIN_AGE_HOURS=8

# Create backup directory if needed
mkdir -p "$BACKUP_DIR"

# Check if source directory exists and is non-empty
if [ ! -d "$DATALEVIN_DIR" ]; then
    echo "Error: Datalevin directory not found: $DATALEVIN_DIR" >&2
    exit 1
fi

if [ -z "$(ls -A "$DATALEVIN_DIR" 2>/dev/null)" ]; then
    echo "Warning: Datalevin directory is empty: $DATALEVIN_DIR"
    exit 0
fi

# Check if recent backup exists (skip if less than MIN_AGE_HOURS old)
LATEST_BACKUP=$(find "$BACKUP_DIR" -name "datalevin-*.tar.gz" -type f -printf '%T@ %p\n' 2>/dev/null | sort -n | tail -1 | cut -d' ' -f2-)
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

# Create timestamped backup
BACKUP_FILE="$BACKUP_DIR/datalevin-$(date +%Y%m%d-%H%M).tar.gz"
tar czf "$BACKUP_FILE" -C "$(dirname "$DATALEVIN_DIR")" "$(basename "$DATALEVIN_DIR")"

# Prune backups older than retention period
find "$BACKUP_DIR" -name "datalevin-*.tar.gz" -mtime +${RETENTION_DAYS} -delete

echo "Backup complete: $BACKUP_FILE"
ls -lh "$BACKUP_FILE"
