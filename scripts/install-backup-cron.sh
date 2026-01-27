#!/bin/bash
# Install daily cron jobs for Chroma and Datalevin backups

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

CHROMA_SCRIPT="$SCRIPT_DIR/backup-chroma.sh"
DATALEVIN_SCRIPT="$SCRIPT_DIR/backup-datalevin.sh"

# Verify scripts exist and are executable
for script in "$CHROMA_SCRIPT" "$DATALEVIN_SCRIPT"; do
    if [ ! -x "$script" ]; then
        echo "Error: Backup script not found or not executable: $script" >&2
        echo "  Run: chmod +x $script"
        exit 1
    fi
done

CURRENT_CRON=$(crontab -l 2>/dev/null || true)
CHANGED=false

# Install Chroma backup cron (daily at 3:00am)
if echo "$CURRENT_CRON" | grep -q "backup-chroma.sh"; then
    echo "Cron job already exists for backup-chroma.sh"
    echo "  $(echo "$CURRENT_CRON" | grep "backup-chroma.sh")"
else
    CURRENT_CRON="$CURRENT_CRON
0 3 * * * $CHROMA_SCRIPT >> \$HOME/backups/chroma/backup.log 2>&1"
    CHANGED=true
    echo "Added: Chroma backup at 3:00am"
fi

# Install Datalevin backup cron (daily at 3:15am)
if echo "$CURRENT_CRON" | grep -q "backup-datalevin.sh"; then
    echo "Cron job already exists for backup-datalevin.sh"
    echo "  $(echo "$CURRENT_CRON" | grep "backup-datalevin.sh")"
else
    CURRENT_CRON="$CURRENT_CRON
15 3 * * * $DATALEVIN_SCRIPT >> \$HOME/backups/datalevin/backup.log 2>&1"
    CHANGED=true
    echo "Added: Datalevin backup at 3:15am"
fi

if [ "$CHANGED" = true ]; then
    echo "$CURRENT_CRON" | crontab -
    echo ""
    echo "Cron jobs installed. Verify with: crontab -l"
fi

echo ""
echo "Current backup cron entries:"
crontab -l 2>/dev/null | grep -E "backup-(chroma|datalevin)\.sh" || echo "  (none)"
