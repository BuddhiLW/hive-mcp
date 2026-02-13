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
# Uses bb + bencode for raw nREPL communication
BB="${BB:-/home/linuxbrew/.linuxbrew/bin/bb}"
NREPL_CODE="(binding [*out* (java.io.StringWriter.)] (require '[hive-mcp.knowledge-graph.migration :as mig]) (mig/export-to-file! \"${BACKUP_FILE}\") :done)"

NREPL_PORT="$NREPL_PORT" NREPL_CODE="$NREPL_CODE" "$BB" -e '
(require (quote [bencode.core :as b]))
(import (quote [java.net Socket])
        (quote [java.io PushbackInputStream]))
(defn bytes->str [x] (if (bytes? x) (String. x) (str x)))
(defn has-done? [status]
  (and (sequential? status)
       (some #(= "done" (bytes->str %)) status)))
(let [port (Integer/parseInt (System/getenv "NREPL_PORT"))
      code (System/getenv "NREPL_CODE")
      sock (doto (Socket. "localhost" port) (.setSoTimeout 120000))
      in (PushbackInputStream. (.getInputStream sock))
      out (.getOutputStream sock)]
  (b/write-bencode out {"op" "eval" "code" code})
  (loop [result nil]
    (let [msg (try (b/read-bencode in) (catch Exception _ nil))]
      (if (nil? msg)
        (do (println (or result "No response")) (.close sock))
        (let [v (get msg "value")
              e (get msg "err")
              status (get msg "status")]
          (when e (binding [*out* *err*] (print (bytes->str e))))
          (if (has-done? status)
            (do (println (bytes->str (or v result "done"))) (.close sock))
            (recur (or v result))))))))
' || {
    echo "Error: bb nREPL eval failed" >&2
    exit 1
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
