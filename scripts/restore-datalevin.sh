#!/bin/bash
# Restore Datalevin (LMDB) knowledge graph from backup
# Usage: ./scripts/restore-datalevin.sh <backup_file.tar.gz>

set -e

BACKUP_FILE="${1:?Usage: $0 <backup_file.tar.gz>}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
DATALEVIN_DIR="$PROJECT_DIR/data/kg/datalevin"

if [ ! -f "$BACKUP_FILE" ]; then
    echo "Error: Backup file not found: $BACKUP_FILE" >&2
    exit 1
fi

echo "=== Datalevin Restore ==="
echo "Backup file: $BACKUP_FILE"
echo "Restore to:  $DATALEVIN_DIR"
echo ""

# Verify the archive is valid before restoring
if ! tar tzf "$BACKUP_FILE" >/dev/null 2>&1; then
    echo "Error: Backup file is corrupt or not a valid tar.gz archive" >&2
    exit 1
fi
echo "Archive integrity: OK"

# Warn if target directory has existing data
if [ -d "$DATALEVIN_DIR" ] && [ -n "$(ls -A "$DATALEVIN_DIR" 2>/dev/null)" ]; then
    echo ""
    echo "Warning: Target directory is not empty."
    echo "  $DATALEVIN_DIR"
    read -p "Overwrite existing data? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 1
    fi
    rm -rf "$DATALEVIN_DIR"
fi

# Ensure parent directory exists
mkdir -p "$(dirname "$DATALEVIN_DIR")"

# Extract backup
tar xzf "$BACKUP_FILE" -C "$(dirname "$DATALEVIN_DIR")"

# Verify restored files exist
if [ ! -d "$DATALEVIN_DIR" ] || [ -z "$(ls -A "$DATALEVIN_DIR" 2>/dev/null)" ]; then
    echo "Error: Restore failed - directory is empty after extraction" >&2
    exit 1
fi

echo ""
echo "=== Restore Complete ==="
echo "Restored files:"
ls -lh "$DATALEVIN_DIR"
echo ""
echo "Total: $(du -sh "$DATALEVIN_DIR" | cut -f1)"
