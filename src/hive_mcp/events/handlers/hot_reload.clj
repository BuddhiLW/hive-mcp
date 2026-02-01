(ns hive-mcp.events.handlers.hot-reload
  "Hot-reload event handlers.

   Handles events from hive-hot file watcher and reload system:
   - :hot/reload-start   - Hot reload process started
   - :hot/reload-success - Hot reload completed successfully
   - :file/changed       - File modification detected by watcher

   These events enable swarm coordination around hot-reload:
   - Lings can pause work during reload
   - Coordinator can track file changes across drones
   - Telemetry captures reload latency and success rates

   CLARITY-T: Telemetry first - structured events for monitoring
   SOLID: SRP - Hot-reload lifecycle only"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :hot/reload-start
;; =============================================================================

(defn handle-hot-reload-start
  "Handler for :hot/reload-start events.

   Called when hive-hot begins a hot-reload cycle.
   Publishes event to channel for swarm coordination.

   Expects event data:
   {:dirs      [\"src\" \"test\"]   ; Directories being watched
    :trigger   :file-change | :manual | :flush
    :timestamp inst}

   Produces effects:
   - :channel-publish - Broadcast reload-start to WebSocket clients
   - :log             - Log reload start message
   - :prometheus      - Increment hot_reload_started counter"
  [_coeffects [_ {:keys [dirs trigger timestamp]}]]
  {:channel-publish {:event :hot-reload-start
                     :data {:dirs (or dirs [])
                            :trigger (or trigger :unknown)
                            :timestamp (or timestamp (java.time.Instant/now))}}
   :log {:level :info
         :message (str "Hot-reload started"
                       (when dirs (str " dirs: " (count dirs)))
                       (when trigger (str " trigger: " (name trigger))))}
   :prometheus {:counter :hot_reload_started
                :labels {:trigger (name (or trigger :unknown))}}})

;; =============================================================================
;; Handler: :hot/reload-success
;; =============================================================================

(defn handle-hot-reload-success
  "Handler for :hot/reload-success events.

   Called when hive-hot completes a reload cycle successfully.
   Publishes completion event with reload statistics and refreshes tool registry.

   Expects event data:
   {:loaded   [ns1 ns2 ...]  ; Namespaces that were reloaded
    :unloaded [ns3 ...]      ; Namespaces that were unloaded first
    :ms       123            ; Duration in milliseconds
    :timestamp inst}

   Produces effects:
   - :channel-publish        - Broadcast reload-success to WebSocket clients
   - :log                    - Log reload completion message
   - :prometheus             - Increment counter, record duration histogram
   - :tool-registry-refresh  - Refresh MCP tool handlers to pick up new code"
  [_coeffects [_ {:keys [loaded unloaded ms timestamp]}]]
  {:channel-publish {:event :hot-reload-success
                     :data {:loaded (or loaded [])
                            :unloaded (or unloaded [])
                            :duration-ms ms
                            :timestamp (or timestamp (java.time.Instant/now))}}
   :log {:level :info
         :message (str "Hot-reload success: "
                       (count loaded) " loaded, "
                       (count unloaded) " unloaded"
                       (when ms (str " in " ms "ms")))}
   :prometheus {:counter :hot_reload_success
                :labels {}
                :histogram {:name :hot_reload_duration_seconds
                            :value (when ms (/ ms 1000.0))}}
   ;; FIX: Refresh tool handlers to pick up reloaded code
   :tool-registry-refresh true})

;; =============================================================================
;; Handler: :file/changed
;; =============================================================================

(defn handle-file-changed
  "Handler for :file/changed events.

   Called when the file watcher detects a file modification.
   Used for swarm coordination - claimed files may skip hot-reload.

   Expects event data:
   {:file      \"/path/to/file.clj\"
    :event-type :modify | :create | :delete
    :timestamp inst}

   Produces effects:
   - :channel-publish - Broadcast file-changed to WebSocket clients
   - :log             - Log file change at debug level
   - :prometheus      - Increment file_changed counter by event type"
  [_coeffects [_ {:keys [file event-type timestamp]}]]
  {:channel-publish {:event :file-changed
                     :data {:file file
                            :event-type (or event-type :modify)
                            :timestamp (or timestamp (java.time.Instant/now))}}
   :log {:level :debug
         :message (str "File changed: " file
                       (when event-type (str " (" (name event-type) ")")))}
   :prometheus {:counter :file_changed
                :labels {:event_type (name (or event-type :modify))}}})

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register hot-reload event handlers."
  []
  (ev/reg-event :hot/reload-start
                [interceptors/debug]
                handle-hot-reload-start)

  (ev/reg-event :hot/reload-success
                [interceptors/debug]
                handle-hot-reload-success)

  (ev/reg-event :file/changed
                [interceptors/debug]
                handle-file-changed))
