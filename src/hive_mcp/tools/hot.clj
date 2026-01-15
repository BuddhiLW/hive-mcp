(ns hive-mcp.tools.hot
  "MCP tools for hot-reload coordination.

   Exposes file watcher status, reload triggers, and pending event management
   for coordination-aware hot reload in the hive swarm.

   Integrates with:
   - hive-hot.core for reload! functionality
   - hive-hot.watcher for file watching (Phase 1)
   - hive-hot.debounce for pending event management (Phase 2)
   - hive-mcp.swarm.logic for file claims

   CLARITY: Yield safe failure - gracefully degrades when hive-hot
   modules are not yet loaded (Phases 1-3 dependency)."
  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.swarm.logic :as logic]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Dynamic Module Resolution (Graceful Degradation)
;; =============================================================================

(defn- resolve-fn
  "Dynamically resolve a function from a namespace.
   Returns nil if namespace or var doesn't exist.
   CLARITY: Yield safe failure."
  [ns-sym fn-sym]
  (try
    (require ns-sym)
    (when-let [v (ns-resolve (find-ns ns-sym) fn-sym)]
      @v)
    (catch Exception _
      nil)))

;; Public for testability (with-redefs)
(defn hot-core-reload!
  "Call hive-hot.core/reload! if available."
  []
  (if-let [reload-fn (resolve-fn 'hive-hot.core 'reload!)]
    (reload-fn)
    {:error "hive-hot.core not loaded"}))

(defn hot-core-status
  "Call hive-hot.core/status if available."
  []
  (if-let [status-fn (resolve-fn 'hive-hot.core 'status)]
    (status-fn)
    nil))

(defn watcher-status
  "Get watcher status if hive-hot.watcher is available."
  []
  (if-let [status-fn (resolve-fn 'hive-hot.watcher 'status)]
    (status-fn)
    nil))

(defn watcher-watching-paths
  "Get watched paths if hive-hot.watcher is available."
  []
  (if-let [paths-fn (resolve-fn 'hive-hot.watcher 'watching-paths)]
    (paths-fn)
    []))

(defn debounce-pending-count
  "Get pending event count if hive-hot.debounce is available."
  []
  (if-let [count-fn (resolve-fn 'hive-hot.debounce 'pending-count)]
    (count-fn)
    0))

(defn debounce-flush!
  "Flush pending events if hive-hot.debounce is available."
  []
  (if-let [flush-fn (resolve-fn 'hive-hot.debounce 'flush!)]
    (flush-fn)
    {:error "hive-hot.debounce not loaded"}))

;; =============================================================================
;; Handler: hot_watcher_status
;; =============================================================================

(defn handle-hot-watcher-status
  "Get file watcher status including paths, pending events, and claimed files.

   Returns:
   {:watching? bool        - Whether watcher is active
    :paths [...]           - Directories being watched
    :pending-count N       - Events buffered in debounce layer
    :claimed-files [...]   - Files currently claimed by swarm tasks
    :hot-core-status {...} - Status from hive-hot.core (if available)}"
  [_args]
  (try
    (let [watcher-st (watcher-status)
          watching? (boolean (and watcher-st (:running? watcher-st)))
          paths (or (watcher-watching-paths) [])
          pending (debounce-pending-count)
          claims (logic/get-all-claims)
          claimed-files (mapv :file claims)
          core-status (hot-core-status)]
      (mcp-json {:watching? watching?
                 :paths paths
                 :pending-count pending
                 :claimed-files claimed-files
                 :hot-core-status core-status
                 :claims-detail claims}))
    (catch Exception e
      (log/error e "Error getting hot watcher status")
      (mcp-json {:error (str "Failed to get watcher status: " (.getMessage e))}))))

;; =============================================================================
;; Handler: hot_reload
;; =============================================================================

(defn handle-hot-reload
  "Trigger hot reload of changed namespaces.

   Calls hive-hot.core/reload! which:
   - Detects changed files since last reload
   - Unloads affected namespaces in dependency order
   - Reloads namespaces in dependency order
   - Runs registered component callbacks

   Returns reload result from hive-hot.core or error if not available."
  [_args]
  (try
    (let [result (hot-core-reload!)]
      (if (:error result)
        (mcp-json {:success false :error (:error result)})
        (mcp-json {:success (:success result true)
                   :unloaded (:unloaded result [])
                   :loaded (:loaded result [])
                   :failed (:failed result)
                   :ms (:ms result)})))
    (catch Exception e
      (log/error e "Error during hot reload")
      (mcp-error (str "Hot reload failed: " (.getMessage e))))))

;; =============================================================================
;; Handler: hot_flush_pending
;; =============================================================================

(defn handle-hot-flush-pending
  "Force reload of pending file changes.

   Bypasses the claim check in the debounce layer, immediately
   triggering reload for all buffered file events.

   Use this for debugging or when you know claimed files are safe to reload.

   Returns flush result or error if debounce module not available."
  [_args]
  (try
    (let [result (debounce-flush!)]
      (if (:error result)
        (mcp-json {:success false :error (:error result)})
        (mcp-json {:success true
                   :flushed-count (or (:flushed-count result) 0)
                   :reload-result (:reload-result result)})))
    (catch Exception e
      (log/error e "Error flushing pending changes")
      (mcp-error (str "Flush failed: " (.getMessage e))))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  "Hot reload MCP tool definitions."
  [{:name "hot_watcher_status"
    :description "Get file watcher status: watching paths, pending events, claimed files"
    :inputSchema {:type "object" :properties {}}
    :handler handle-hot-watcher-status}

   {:name "hot_reload"
    :description "Trigger hot reload of changed namespaces"
    :inputSchema {:type "object" :properties {}}
    :handler handle-hot-reload}

   {:name "hot_flush_pending"
    :description "Force reload of pending file changes (bypasses claim check)"
    :inputSchema {:type "object" :properties {}}
    :handler handle-hot-flush-pending}])
