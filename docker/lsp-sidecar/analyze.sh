#!/usr/bin/env bash
set -euo pipefail

# Default values
WORKSPACE=${WORKSPACE:-/workspace}
CACHE_DIR=${CACHE_DIR:-/cache}
INTERVAL=${ANALYSIS_INTERVAL_SECONDS:-300}
LSP_JAR=/opt/clojure-lsp.jar
REQUEST_FILE="$CACHE_DIR/_request.edn"

analyze_project() {
    local project_root=$1
    local project_id=$2
    local cache_path="$CACHE_DIR/$project_id"
    mkdir -p "$cache_path"
    local start_time
    start_time=$(date +%s)

    echo "Analyzing project $project_id at $project_root"

    if java $JAVA_OPTS -jar "$LSP_JAR" dump --project-root "$project_root" \
        --output '{:format :edn :filter-keys [:analysis :dep-graph]}' \
        --analysis '{:type :project-only}' \
        > "$cache_path/dump.edn.tmp" 2>"$cache_path/dump.log"; then
        local end_time
        end_time=$(date +%s)
        local duration_ms=$(( (end_time - start_time) * 1000 ))
        mv "$cache_path/dump.edn.tmp" "$cache_path/dump.edn"
        echo "{:timestamp $start_time :duration-ms $duration_ms :project-root \"$project_root\" :project-id \"$project_id\" :status :ok}" > "$cache_path/meta.edn"
        echo "Analysis successful for $project_id (${duration_ms}ms)"
    else
        local exit_code=$?
        rm -f "$cache_path/dump.edn.tmp"
        echo "{:timestamp $start_time :project-root \"$project_root\" :project-id \"$project_id\" :status :error :exit-code $exit_code}" > "$cache_path/meta.edn"
        echo "Analysis failed for $project_id with exit code $exit_code"
    fi
}

discover_projects() {
    if [ -n "${LSP_PROJECTS:-}" ]; then
        IFS=',' read -ra PROJECTS <<< "$LSP_PROJECTS"
        for proj in "${PROJECTS[@]}"; do
            echo "/workspace/$proj:$proj"
        done
    else
        find "$WORKSPACE" -maxdepth 2 -name deps.edn -exec dirname {} \; | while read -r dir; do
            echo "$dir:$(basename "$dir")"
        done
    fi
}

# Process dynamic request file (written by MCP tool, consumed here)
# Format: one project-id per line
process_requests() {
    if [ -f "$REQUEST_FILE" ]; then
        echo "Processing dynamic request: $REQUEST_FILE"
        while IFS= read -r project_id; do
            # Strip whitespace and skip empty/comment lines
            project_id=$(echo "$project_id" | tr -d '[:space:]')
            [[ -z "$project_id" || "$project_id" == \#* ]] && continue
            local project_root="$WORKSPACE/$project_id"
            if [ -d "$project_root" ]; then
                analyze_project "$project_root" "$project_id"
            else
                echo "Requested project not found: $project_root"
            fi
        done < "$REQUEST_FILE"
        rm -f "$REQUEST_FILE"
        return 0
    fi
    return 1
}

echo "Starting clojure-lsp sidecar analysis with interval: ${INTERVAL}s"
echo "Workspace: $WORKSPACE"
echo "Cache directory: $CACHE_DIR"
echo "Request file: $REQUEST_FILE"
echo "LSP JAR: $LSP_JAR"

# Flag for immediate re-run on SIGHUP
rerun_immediately=0

trap 'rerun_immediately=1' SIGHUP

while true; do
    # Check for dynamic requests first (on-demand indexing)
    if ! process_requests; then
        # No requests — run scheduled discovery
        discover_projects | while IFS=: read -r project_dir project_id; do
            analyze_project "$project_dir" "$project_id"
        done
    fi

    if [ $rerun_immediately -eq 1 ]; then
        rerun_immediately=0
        echo "Re-running immediately due to SIGHUP"
    else
        echo "Sleeping for ${INTERVAL}s"
        sleep "$INTERVAL" &
        wait $! || true
    fi
done
