#!/bin/bash
# Backup Milvus and Chroma vector database volumes
# Usage: ./scripts/backup-indexes.sh [backup_dir]

set -e

BACKUP_DIR="${1:-./backups}"
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
BACKUP_PATH="$BACKUP_DIR/$TIMESTAMP"

mkdir -p "$BACKUP_PATH"

echo "=== Vector Index Backup ==="
echo "Backup location: $BACKUP_PATH"
echo ""

# Check if containers are running
if docker ps --format '{{.Names}}' | grep -q "emacs-mcp-milvus"; then
    echo "Warning: Containers are running. For consistent backup, consider stopping them first."
    echo "  docker compose stop"
    echo ""
    read -p "Continue anyway? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 1
    fi
fi

# Backup Milvus volumes
echo "Backing up Milvus data..."
docker run --rm \
    -v emacs-mcp_milvus-data:/data:ro \
    -v "$BACKUP_PATH":/backup \
    alpine tar czf /backup/milvus-data.tar.gz -C /data .

echo "Backing up Milvus etcd..."
docker run --rm \
    -v emacs-mcp_milvus-etcd:/data:ro \
    -v "$BACKUP_PATH":/backup \
    alpine tar czf /backup/milvus-etcd.tar.gz -C /data .

echo "Backing up Milvus minio..."
docker run --rm \
    -v emacs-mcp_milvus-minio:/data:ro \
    -v "$BACKUP_PATH":/backup \
    alpine tar czf /backup/milvus-minio.tar.gz -C /data .

# Backup Chroma volume
echo "Backing up Chroma data..."
docker run --rm \
    -v emacs-mcp_chroma-data:/data:ro \
    -v "$BACKUP_PATH":/backup \
    alpine tar czf /backup/chroma-data.tar.gz -C /data . 2>/dev/null || echo "Chroma volume not found, skipping."

# Create metadata file
cat > "$BACKUP_PATH/metadata.json" << EOF
{
  "timestamp": "$TIMESTAMP",
  "date": "$(date -Iseconds)",
  "hostname": "$(hostname)",
  "milvus_version": "v2.4.4",
  "volumes": [
    "milvus-data",
    "milvus-etcd",
    "milvus-minio",
    "chroma-data"
  ],
  "docker_compose": "docker-compose.yml"
}
EOF

# Calculate sizes
echo ""
echo "=== Backup Complete ==="
du -sh "$BACKUP_PATH"/*
echo ""
echo "Total: $(du -sh "$BACKUP_PATH" | cut -f1)"
echo ""
echo "To restore on another machine:"
echo "  ./scripts/restore-indexes.sh $BACKUP_PATH"
