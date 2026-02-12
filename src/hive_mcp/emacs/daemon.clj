(ns hive-mcp.emacs.daemon
  "Protocol definition for Emacs daemon lifecycle management.

   Abstracts daemon registration, heartbeat, ling binding, and
   stale detection so that different storage backends (DataScript,
   Datalevin) can be used interchangeably.

   daemon domain logic and storage implementation.

   Mirrors the coordinator pattern in:
   - hive-mcp.swarm.datascript.coordination (coordinator lifecycle)
   - hive-mcp.knowledge-graph.protocol (IGraphStore)")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def daemon-statuses
  "Valid Emacs daemon status values.

   :active     - Running and sending heartbeats
   :stale      - Not sending heartbeats (likely crashed)
   :error      - In error state (Emacs reported errors)
   :terminated - Gracefully shutdown"
  #{:active :stale :error :terminated})

(defprotocol IEmacsDaemon
  "Lifecycle management protocol for Emacs daemon instances.

   Each Emacs daemon is identified by a unique ID (typically the socket name).
   Daemons go through a lifecycle: register -> heartbeat -> terminate.
   Lings can be bound/unbound to track which agents use which daemon.

   Implementations:
   - DataScriptDaemonStore: In-memory, used by swarm coordination"

  ;; --- Lifecycle ---

  (register! [this daemon-id opts]
    "Register a new Emacs daemon instance.

     Arguments:
       daemon-id - Unique identifier (e.g., socket name)
       opts      - Map with optional keys:
                   :socket-name - Emacs socket name (defaults to daemon-id)
                   :pid         - OS process ID
                   :emacsclient - Path to emacsclient binary

     Returns:
       Transaction report")

  (heartbeat! [this daemon-id]
    "Update a daemon's heartbeat timestamp.
     Also ensures status is :active (reactivates stale/error daemons).

     Arguments:
       daemon-id - Daemon to update

     Returns:
       Transaction report or nil if daemon not found")

  (mark-error! [this daemon-id error-message]
    "Mark a daemon as being in error state.

     Arguments:
       daemon-id     - Daemon to mark
       error-message - Human-readable error description

     Returns:
       Transaction report or nil if daemon not found")

  (mark-terminated! [this daemon-id]
    "Mark a daemon as gracefully terminated.

     Arguments:
       daemon-id - Daemon to mark

     Returns:
       Transaction report or nil if daemon not found")

  ;; --- Ling Binding ---

  (bind-ling! [this daemon-id ling-id]
    "Bind a ling to this daemon (track which agents use which Emacs).

     Arguments:
       daemon-id - Daemon to bind to
       ling-id   - Ling/slave ID to bind

     Returns:
       Transaction report or nil if daemon not found")

  (unbind-ling! [this daemon-id ling-id]
    "Unbind a ling from this daemon.

     Arguments:
       daemon-id - Daemon to unbind from
       ling-id   - Ling/slave ID to unbind

     Returns:
       Transaction report or nil if daemon not found")

  ;; --- Queries ---

  (get-daemon [this daemon-id]
    "Get a daemon by ID.

     Returns:
       Map with daemon attributes or nil if not found")

  (get-all-daemons [this]
    "Get all registered daemons.

     Returns:
       Seq of daemon attribute maps")

  (get-daemons-by-status [this status]
    "Get daemons filtered by status.

     Arguments:
       status - Status to filter by (:active :stale :error :terminated)

     Returns:
       Seq of daemon maps")

  (get-daemon-for-ling [this ling-id]
    "Find the daemon a specific ling is bound to.

     Arguments:
       ling-id - Ling/slave ID to look up

     Returns:
       Daemon map or nil if ling is not bound to any daemon")

  ;; --- Cleanup ---

  (cleanup-stale! [this] [this opts]
    "Find and mark daemons as stale if their heartbeat is too old.

     Arguments:
       opts - Optional map with :threshold-ms (default: 2 minutes)

     Returns:
       Seq of daemon-ids that were marked stale

     CLARITY-Y: Graceful degradation - marks as stale rather than deleting."))
