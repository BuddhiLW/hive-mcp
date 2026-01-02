#!/bin/bash
# Restore Milvus and Chroma vector database volumes from backup
# Usage: ./scripts/restore-indexes.sh <backup_dir>

set -e

BACKUP_PATH="${1:?Usage: $0 <backup_dir>}"

if [ ! -d "$BACKUP_PATH" ]; then
    echo "Error: Backup directory not found: $BACKUP_PATH"
    exit 1
fi

echo "=== Vector Index Restore ==="
echo "Restoring from: $BACKUP_PATH"
echo ""

# Show metadata if exists
if [ -f "$BACKUP_PATH/metadata.json" ]; then
    echo "Backup info:"
    cat "$BACKUP_PATH/metadata.json" | head -10
    echo ""
fi

# Check if containers are running
if docker ps --format '{{.Names}}' | grep -q "emacs-mcp-milvus"; then
    echo "Stopping containers..."
    docker compose stop
fi

# Create volumes if they don't exist
echo "Ensuring volumes exist..."
docker volume create emacs-mcp_milvus-data 2>/dev/null || true
docker volume create emacs-mcp_milvus-etcd 2>/dev/null || true
docker volume create emacs-mcp_milvus-minio 2>/dev/null || true
docker volume create emacs-mcp_chroma-data 2>/dev/null || true

# Restore Milvus data
if [ -f "$BACKUP_PATH/milvus-data.tar.gz" ]; then
    echo "Restoring Milvus data..."
    docker run --rm \
        -v emacs-mcp_milvus-data:/data \
        -v "$BACKUP_PATH":/backup:ro \
        alpine sh -c "rm -rf /data/* && tar xzf /backup/milvus-data.tar.gz -C /data"
fi

# Restore Milvus etcd
if [ -f "$BACKUP_PATH/milvus-etcd.tar.gz" ]; then
    echo "Restoring Milvus etcd..."
    docker run --rm \
        -v emacs-mcp_milvus-etcd:/data \
        -v "$BACKUP_PATH":/backup:ro \
        alpine sh -c "rm -rf /data/* && tar xzf /backup/milvus-etcd.tar.gz -C /data"
fi

# Restore Milvus minio
if [ -f "$BACKUP_PATH/milvus-minio.tar.gz" ]; then
    echo "Restoring Milvus minio..."
    docker run --rm \
        -v emacs-mcp_milvus-minio:/data \
        -v "$BACKUP_PATH":/backup:ro \
        alpine sh -c "rm -rf /data/* && tar xzf /backup/milvus-minio.tar.gz -C /data"
fi

# Restore Chroma
if [ -f "$BACKUP_PATH/chroma-data.tar.gz" ]; then
    echo "Restoring Chroma data..."
    docker run --rm \
        -v emacs-mcp_chroma-data:/data \
        -v "$BACKUP_PATH":/backup:ro \
        alpine sh -c "rm -rf /data/* && tar xzf /backup/chroma-data.tar.gz -C /data"
fi

echo ""
echo "=== Restore Complete ==="
echo ""
echo "Start containers with:"
echo "  docker compose up -d"
echo ""
echo "Then reconnect MCP:"
echo "  /mcp (in Claude Code)"
