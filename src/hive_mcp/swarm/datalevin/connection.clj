(ns hive-mcp.swarm.datalevin.connection
  "Datalevin LMDB connection management for swarm state.

   Provides:
   - Persistent connection backed by LMDB (survives JVM restart)
   - Connection lifecycle (open, get, close, reset)
   - Shared helper functions (now, gen-id)
   - db accessor (replaces DataScript's @conn deref pattern)

   Boundary: This is the ONLY namespace that touches datalevin.core directly
   for connection lifecycle. All other datalevin.* namespaces go through here."

  (:require [datalevin.core :as dl]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.datalevin.schema :as schema]
            [hive-mcp.server.guards :as guards]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; DB Path Configuration
;;; =============================================================================

(def ^:private default-db-path
  "Default LMDB directory for swarm state."
  (str (System/getProperty "user.home") "/.local/share/hive-mcp/swarm-db"))

(defonce ^:private db-path-override (atom nil))

(defn set-db-path!
  "Override the default DB path. Must be called before first get-conn."
  [path]
  (reset! db-path-override path))

(defn get-db-path
  "Get the configured DB path."
  []
  (or @db-path-override
      (System/getenv "HIVE_SWARM_DB_PATH")
      default-db-path))

;;; =============================================================================
;;; Connection Management
;;; =============================================================================

(defonce ^:private conn (atom nil))

(defn- ensure-db-dir!
  "Ensure the parent directory exists for the LMDB database."
  [path]
  (let [dir (java.io.File. path)]
    (when-not (.exists dir)
      (.mkdirs dir)
      (log/info "Created Datalevin DB directory:" path))))

(defn create-conn
  "Create a new Datalevin connection with swarm schema.
   Opens an LMDB database at the configured path.
   Returns the Datalevin connection."
  []
  (let [path (get-db-path)]
    (ensure-db-dir! path)
    (dl/get-conn path schema/schema)))

(defn get-conn
  "Get the global swarm connection, creating if needed."
  []
  (or @conn
      (do
        (reset! conn (create-conn))
        (log/info "Created swarm Datalevin connection at:" (get-db-path))
        @conn)))

(defn close!
  "Close the Datalevin connection and release LMDB resources.
   Safe to call multiple times."
  []
  (when-let [c @conn]
    (try
      (dl/close c)
      (log/info "Closed swarm Datalevin connection")
      (catch Exception e
        (log/warn "Error closing Datalevin connection:" (ex-message e))))
    (reset! conn nil)))

(defn reset-conn!
  "Reset the global connection: close existing, open fresh.
   Datalevin is persistent so this reopens the same LMDB, not a blank DB.
   Use clear-db! if you need a truly empty database."
  []
  (guards/when-not-coordinator
   "dl/reset-conn! blocked"
   (close!)
   (reset! conn (create-conn))
   (log/debug "Swarm Datalevin connection reset")))

(defn ensure-conn
  "Ensure connection exists, return it."
  []
  (get-conn))

;;; =============================================================================
;;; DB Accessor (Pipeline boundary)
;;; =============================================================================

(defn current-db
  "Get the current database value from the connection.
   Replaces DataScript's @conn deref pattern.

   DataScript:  @conn
   Datalevin:   (dl/db conn)  → (current-db)"
  []
  (dl/db (get-conn)))

(defn db
  "Get the current database value from a specific connection.
   For use in functions that receive a connection argument."
  [c]
  (dl/db c))

;;; =============================================================================
;;; Helper Functions (shared with DataScript module)
;;; =============================================================================

(defn now
  "Current timestamp as java.util.Date."
  []
  (java.util.Date.))

(defn gen-id
  "Generate a unique ID with optional prefix."
  ([] (str (java.util.UUID/randomUUID)))
  ([prefix] (str prefix "-" (java.util.UUID/randomUUID))))

;;; =============================================================================
;;; Clear / Destroy (for testing and hard reset)
;;; =============================================================================

(defn clear-db!
  "Destroy and recreate the LMDB database.
   WARNING: Permanently deletes all swarm state."
  []
  (guards/when-not-coordinator
   "dl/clear-db! blocked"
   (close!)
   (let [path (get-db-path)
         dir (java.io.File. path)]
     (when (.exists dir)
       (doseq [f (reverse (file-seq dir))]
         (.delete f))
       (log/warn "Destroyed Datalevin DB at:" path)))
   (reset! conn (create-conn))
   (log/info "Recreated empty Datalevin DB")))
