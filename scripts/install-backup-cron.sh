#!/bin/bash
# Install daily cron job for Chroma backup at 3am

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_SCRIPT="$SCRIPT_DIR/backup-chroma.sh"

if [ ! -x "$BACKUP_SCRIPT" ]; then
    echo "Error: Backup script not found or not executable: $BACKUP_SCRIPT" >&2
    exit 1
fi

# Check if cron job already exists
if crontab -l 2>/dev/null | grep -q "backup-chroma.sh"; then
    echo "Cron job already exists for backup-chroma.sh"
    crontab -l | grep "backup-chroma.sh"
    exit 0
fi

# Add cron job: daily at 3am
(crontab -l 2>/dev/null; echo "0 3 * * * $BACKUP_SCRIPT >> $HOME/backups/chroma/backup.log 2>&1") | crontab -

echo "Cron job installed. Verify with: crontab -l"
crontab -l | grep "backup-chroma.sh"
