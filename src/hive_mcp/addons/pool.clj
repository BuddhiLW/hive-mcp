(ns hive-mcp.addons.pool
  "Connection pool manager for MCP bridges.

   Manages N connections per bridge with borrow/return semantics.
   Thread-safe via j.u.c.ArrayBlockingQueue.

   Architecture:
     Each pool manages N independently-started bridge instances of the
     same type. Callers borrow a bridge, use it for a single call-tool
     invocation, then return it. This enables N concurrent tool calls
     to the same external MCP server.

   Connection lifecycle:
     factory-fn → bridge instance → start-bridge! → pool
       ↕ borrow/return (hot path)
     pool → stop-bridge! → discard (eviction or drain)

   Usage:
     (def pool (create-pool! my-factory 3 {:borrow-timeout-ms 5000}))
     (with-connection [conn pool]
       (bridge/call-tool conn \"echo\" {\"message\" \"hi\"}))
     (drain! pool)

   See also:
   - hive-mcp.addons.mcp-bridge — IMcpBridge protocol
   - hive-mcp.addons.stdio-bridge — Concrete stdio bridge"
  (:require [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.addons.protocol :as proto]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent
            ArrayBlockingQueue
            TimeUnit
            ScheduledExecutorService
            Executors
            ScheduledFuture]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Default Configuration
;; =============================================================================

(def default-pool-opts
  "Default pool configuration.

   :pool-size            — Number of connections to maintain
   :borrow-timeout-ms    — Max wait for available connection (0 = non-blocking)
   :health-check-on-borrow — Validate connection before returning from borrow
   :health-check-interval-ms — Periodic health check (0 = disabled)
   :max-idle-ms          — Evict connections idle longer than this (0 = disabled)
   :replace-on-evict     — Spawn replacement when evicting dead connection"
  {:pool-size              3
   :borrow-timeout-ms      5000
   :health-check-on-borrow true
   :health-check-interval-ms 0
   :max-idle-ms            0
   :replace-on-evict       true})

;; =============================================================================
;; Connection Wrapper
;; =============================================================================

(defrecord PooledConnection [bridge created-at last-used-at idle-since])

(defn- make-pooled-conn
  "Wrap a bridge instance in a PooledConnection with timestamps."
  [bridge]
  (let [now (System/currentTimeMillis)]
    (->PooledConnection bridge now (atom now) (atom now))))

(defn- touch!
  "Mark a pooled connection as recently used."
  [^PooledConnection conn]
  (let [now (System/currentTimeMillis)]
    (reset! (:last-used-at conn) now)
    (reset! (:idle-since conn) now)
    conn))

(defn- mark-idle!
  "Mark a pooled connection as idle (returned to pool)."
  [^PooledConnection conn]
  (reset! (:idle-since conn) (System/currentTimeMillis))
  conn)

(defn- conn-healthy?
  "Check if a pooled connection's underlying bridge is still alive."
  [^PooledConnection conn]
  (try
    (let [status (bridge/bridge-status (:bridge conn))]
      (:connected? status))
    (catch Exception e
      (log/debug "Health check failed" {:error (.getMessage e)})
      false)))

(defn- conn-idle-ms
  "Milliseconds since this connection was last returned to pool."
  [^PooledConnection conn]
  (- (System/currentTimeMillis) @(:idle-since conn)))

;; =============================================================================
;; Pool State
;; =============================================================================

(defrecord ConnectionPool
           [^ArrayBlockingQueue queue     ;; Available connections (borrow source)
            all-conns                      ;; atom: set of ALL connections (for drain)
            factory-fn                     ;; (fn [] -> started bridge instance)
            opts                           ;; Merged pool options
            stats                          ;; atom: {:borrows :returns :evictions :timeouts :errors}
            scheduler                      ;; atom: ScheduledExecutorService for periodic health
            health-future                  ;; atom: ScheduledFuture for health check task
            drained?])                     ;; atom: boolean — pool shut down

;; =============================================================================
;; Connection Factory
;; =============================================================================

(defn- spawn-connection!
  "Use the factory to create and start a new bridge connection.

   Validates that the factory returns an IMcpBridge-satisfying object.
   Does NOT require {:connected? true} — some bridges connect lazily.
   Health-check-on-borrow handles liveness at borrow time.

   Returns PooledConnection on success, nil on failure."
  [factory-fn pool-id]
  (try
    (let [bridge (factory-fn)]
      (if (bridge/bridge? bridge)
        (do
          (log/debug "Pool spawned connection"
                     {:pool pool-id
                      :transport (try (bridge/transport-type bridge)
                                      (catch Exception _ :unknown))})
          (make-pooled-conn bridge))
        (do
          (log/warn "Pool factory returned non-bridge object" {:pool pool-id})
          nil)))
    (catch Exception e
      (log/error e "Pool connection factory failed" {:pool pool-id})
      nil)))

;; =============================================================================
;; Core Pool Operations
;; =============================================================================

(defn borrow
  "Borrow a connection from the pool. Blocks up to timeout-ms.

   If health-check-on-borrow is enabled, validates the connection before
   returning. Dead connections are evicted and a fresh one is tried.

   Arguments:
     pool       - ConnectionPool instance
     timeout-ms - Override borrow timeout (optional, uses pool default)

   Returns:
     PooledConnection on success
     nil on timeout (no available connection)

   Throws:
     ExceptionInfo if pool is drained"
  ([pool] (borrow pool (get-in pool [:opts :borrow-timeout-ms])))
  ([pool timeout-ms]
   (when @(:drained? pool)
     (throw (ex-info "Cannot borrow from drained pool" {})))
   (let [check-health? (get-in pool [:opts :health-check-on-borrow])
         max-attempts  (get-in pool [:opts :pool-size])
         queue         (:queue pool)]
     (loop [attempt 0]
       (if-let [conn (.poll queue timeout-ms TimeUnit/MILLISECONDS)]
         (if (or (not check-health?) (conn-healthy? conn))
           (do
             (touch! conn)
             (swap! (:stats pool) update :borrows (fnil inc 0))
             conn)
           ;; Dead connection — evict and try again
           (do
             (log/info "Evicting dead connection on borrow"
                       {:attempt attempt :created-at (:created-at conn)})
             (swap! (:all-conns pool) disj conn)
             (swap! (:stats pool) update :evictions (fnil inc 0))
             ;; Try to stop the dead bridge (best effort)
             (try (bridge/stop-bridge! (:bridge conn)) (catch Exception _))
             ;; Spawn replacement if configured
             (when (get-in pool [:opts :replace-on-evict])
               (when-let [fresh (spawn-connection! (:factory-fn pool) :pool)]
                 (swap! (:all-conns pool) conj fresh)
                 (.offer queue fresh)))
             ;; Retry up to pool-size times (avoid infinite loop)
             (if (< attempt max-attempts)
               (recur (inc attempt))
               (do
                 (swap! (:stats pool) update :timeouts (fnil inc 0))
                 nil))))
         ;; Queue poll returned nil — timeout
         (do
           (swap! (:stats pool) update :timeouts (fnil inc 0))
           nil))))))

(defn return!
  "Return a borrowed connection to the pool.

   Arguments:
     pool - ConnectionPool instance
     conn - PooledConnection previously obtained via borrow

   Returns true if successfully returned, false if pool is full/drained."
  [pool conn]
  (when-not @(:drained? pool)
    (mark-idle! conn)
    (let [offered (.offer ^ArrayBlockingQueue (:queue pool) conn)]
      (when offered
        (swap! (:stats pool) update :returns (fnil inc 0)))
      (when-not offered
        (log/warn "Pool full on return — connection lost"
                  {:pool-size (get-in pool [:opts :pool-size])}))
      offered)))

(defn evict!
  "Explicitly evict a connection from the pool.

   Stops the bridge and optionally spawns a replacement.

   Arguments:
     pool - ConnectionPool instance
     conn - PooledConnection to evict

   Returns:
     {:evicted true :replaced true/false}"
  [pool conn]
  (swap! (:all-conns pool) disj conn)
  (swap! (:stats pool) update :evictions (fnil inc 0))
  (try
    (bridge/stop-bridge! (:bridge conn))
    (catch Exception e
      (log/debug "Error stopping evicted connection" {:error (.getMessage e)})))
  (let [replaced? (when (and (get-in pool [:opts :replace-on-evict])
                             (not @(:drained? pool)))
                    (when-let [fresh (spawn-connection! (:factory-fn pool) :pool)]
                      (swap! (:all-conns pool) conj fresh)
                      (.offer ^ArrayBlockingQueue (:queue pool) fresh)
                      true))]
    {:evicted true :replaced (boolean replaced?)}))

(defn pool-status
  "Return pool health metrics.

   Returns:
     {:pool-size      int    — Configured pool size
      :available      int    — Connections currently in queue (borrowable)
      :total          int    — All tracked connections (available + borrowed)
      :borrowed       int    — Currently checked out
      :stats          map    — Cumulative {:borrows :returns :evictions :timeouts}
      :drained?       bool   — Pool shut down
      :connections    [map]  — Per-connection health (when detail requested)}"
  ([pool] (pool-status pool false))
  ([pool include-detail?]
   (let [available (.size ^ArrayBlockingQueue (:queue pool))
         total     (count @(:all-conns pool))
         base      {:pool-size  (get-in pool [:opts :pool-size])
                    :available  available
                    :total      total
                    :borrowed   (- total available)
                    :stats      @(:stats pool)
                    :drained?   @(:drained? pool)}]
     (if include-detail?
       (assoc base :connections
              (mapv (fn [c]
                      {:created-at   (:created-at c)
                       :last-used-at @(:last-used-at c)
                       :idle-ms      (conn-idle-ms c)
                       :healthy?     (conn-healthy? c)})
                    @(:all-conns pool)))
       base))))

(defn drain!
  "Shutdown all connections and mark pool as drained.

   Stops all bridge connections, clears the queue, and prevents
   future borrows. Idempotent — safe to call multiple times.

   Returns:
     {:drained true :stopped int :errors [str]}"
  [pool]
  (reset! (:drained? pool) true)
  ;; Cancel health check scheduler
  (when-let [^ScheduledFuture hf @(:health-future pool)]
    (try (.cancel hf false) (catch Exception _)))
  (when-let [^ScheduledExecutorService sched @(:scheduler pool)]
    (try (.shutdownNow sched) (catch Exception _)))
  ;; Drain queue
  (.clear ^ArrayBlockingQueue (:queue pool))
  ;; Stop all connections
  (let [conns @(:all-conns pool)
        errors (atom [])]
    (doseq [conn conns]
      (try
        (bridge/stop-bridge! (:bridge conn))
        (catch Exception e
          (swap! errors conj (.getMessage e)))))
    (reset! (:all-conns pool) #{})
    (log/info "Pool drained" {:stopped (count conns)
                              :errors (count @errors)})
    {:drained true
     :stopped (count conns)
     :errors  @errors}))

;; =============================================================================
;; Periodic Health Check
;; =============================================================================

(defn- run-health-check!
  "Check all available connections, evict dead ones.
   Runs inside the scheduled executor."
  [pool]
  (when-not @(:drained? pool)
    (let [queue ^ArrayBlockingQueue (:queue pool)
          size  (.size queue)
          conns (java.util.ArrayList.)]
      ;; Drain available connections for inspection
      (.drainTo queue conns)
      (let [dead   (filterv (complement conn-healthy?) conns)
            alive  (filterv conn-healthy? conns)]
        ;; Return alive connections
        (doseq [c alive]
          (.offer queue c))
        ;; Evict dead + replace
        (doseq [c dead]
          (log/info "Health check: evicting dead connection"
                    {:created-at (:created-at c)
                     :idle-ms    (conn-idle-ms c)})
          (swap! (:all-conns pool) disj c)
          (swap! (:stats pool) update :evictions (fnil inc 0))
          (try (bridge/stop-bridge! (:bridge c)) (catch Exception _))
          (when (get-in pool [:opts :replace-on-evict])
            (when-let [fresh (spawn-connection! (:factory-fn pool) :pool)]
              (swap! (:all-conns pool) conj fresh)
              (.offer queue fresh))))
        (when (seq dead)
          (log/info "Health check completed"
                    {:checked size :dead (count dead) :alive (count alive)}))))))

(defn- start-health-scheduler!
  "Start periodic health check thread if interval > 0."
  [pool interval-ms]
  (when (pos? interval-ms)
    (let [scheduler (Executors/newSingleThreadScheduledExecutor)
          future    (.scheduleAtFixedRate
                     scheduler
                     ^Runnable (fn [] (try (run-health-check! pool)
                                           (catch Exception e
                                             (log/error e "Health check error"))))
                     interval-ms
                     interval-ms
                     TimeUnit/MILLISECONDS)]
      [scheduler future])))

;; =============================================================================
;; Pool Constructor
;; =============================================================================

(defn create-pool!
  "Create a connection pool and fill it with N connections.

   The factory-fn is called pool-size times to create bridge instances.
   Each must return a started IMcpBridge-satisfying object.

   Arguments:
     factory-fn - (fn [] -> started-bridge)
                  Called to create each connection.
                  Must return an object satisfying IMcpBridge with
                  bridge-status showing {:connected? true}.
     pool-size  - Number of connections (overrides opts :pool-size)
     opts       - Pool options map (merged with defaults):
       :borrow-timeout-ms      — Max wait for borrow (default 5000)
       :health-check-on-borrow — Check health before returning conn (default true)
       :health-check-interval-ms — Periodic health check ms (default 0 = off)
       :replace-on-evict       — Auto-replace dead connections (default true)

   Returns:
     ConnectionPool instance

   Throws:
     ExceptionInfo if no connections could be created"
  ([factory-fn] (create-pool! factory-fn 3 {}))
  ([factory-fn pool-size] (create-pool! factory-fn pool-size {}))
  ([factory-fn pool-size opts]
   (let [merged-opts (merge default-pool-opts opts {:pool-size pool-size})
         queue       (ArrayBlockingQueue. (int pool-size))
         all-conns   (atom #{})
         stats       (atom {:borrows 0 :returns 0 :evictions 0
                            :timeouts 0 :errors 0})
         drained?    (atom false)
         sched-atom  (atom nil)
         hf-atom     (atom nil)
         pool        (->ConnectionPool queue all-conns factory-fn merged-opts
                                       stats sched-atom hf-atom drained?)
         ;; Fill pool
         created     (doall
                      (for [_ (range pool-size)]
                        (spawn-connection! factory-fn :pool)))
         live        (filterv some? created)]
     (when (empty? live)
       (throw (ex-info "Pool creation failed: no connections could be created"
                       {:pool-size pool-size :attempted pool-size})))
       ;; Add to pool
     (doseq [conn live]
       (swap! all-conns conj conn)
       (.offer queue conn))
     (log/info "Pool created"
               {:pool-size pool-size
                :connected (count live)
                :failed    (- pool-size (count live))})
       ;; Start health checker if configured
     (let [interval (get merged-opts :health-check-interval-ms 0)]
       (when-let [[sched hf] (start-health-scheduler! pool interval)]
         (reset! (:scheduler pool) sched)
         (reset! (:health-future pool) hf)))
     pool)))

;; =============================================================================
;; with-connection Macro
;; =============================================================================

(defmacro with-connection
  "Borrow a connection, execute body, and return it.

   Guarantees return even on exception (finally block).
   The binding receives the underlying bridge (not the PooledConnection),
   ready for call-tool / list-remote-tools.

   Usage:
     (with-connection [conn pool]
       (bridge/call-tool conn \"echo\" {\"message\" \"hello\"}))

   Throws:
     ExceptionInfo if borrow times out (no available connection)"
  [[binding pool-expr] & body]
  `(let [pool# ~pool-expr
         pc#   (borrow pool#)]
     (when-not pc#
       (throw (ex-info "Pool borrow timed out — no available connection"
                       {:pool-status (pool-status pool#)})))
     (try
       (let [~binding (:bridge pc#)]
         ~@body)
       (finally
         (return! pool# pc#)))))

;; =============================================================================
;; PooledBridge — Transparent pool-backed IMcpBridge
;; =============================================================================

(defrecord PooledBridge [id pool transport-kw init-opts]
  proto/IAddon

  (addon-id [_] id)

  (addon-type [_] :mcp-bridge)

  (capabilities [_] #{:mcp-bridge :tools})

  (initialize! [_ _opts]
    ;; Pool is already created — init is a no-op
    {:success? true :errors []})

  (shutdown! [_]
    (drain! pool))

  (tools [this]
    ;; Borrow one connection to discover tools, then proxy via pool
    (with-connection [conn pool]
      (let [remote-tools (bridge/list-remote-tools conn)
            prefix       (clojure.core/name id)]
        (mapv (fn [rt]
                (bridge/proxy-tool-def
                 this prefix rt {}))
              remote-tools))))

  (schema-extensions [_] {})

  (health [_]
    (let [status (pool-status pool)]
      {:status (if (pos? (:available status)) :ok :down)
       :details {:version "0.1.0"
                 :pool status}}))

  bridge/IMcpBridge

  (transport-type [_] transport-kw)

  (start-bridge! [_ _opts]
    ;; Pool manages connections — this is a no-op
    {:success? true :errors [] :metadata {:pooled true}})

  (stop-bridge! [_]
    (drain! pool))

  (bridge-status [_]
    (let [status (pool-status pool)]
      {:connected?        (pos? (:available status))
       :transport         transport-kw
       :uptime-ms         nil ;; pool-level, not per-conn
       :remote-tool-count 0
       :pool              status}))

  (call-tool [_ tool-name params]
    (with-connection [conn pool]
      (bridge/call-tool conn tool-name params)))

  (list-remote-tools [_]
    (with-connection [conn pool]
      (bridge/list-remote-tools conn))))

;; =============================================================================
;; PooledBridge Constructor
;; =============================================================================

(defn ->pooled-bridge
  "Create a PooledBridge with a connection pool.

   The factory-fn creates individual bridge instances. Each is started
   independently and managed by the pool.

   Arguments:
     id          - Keyword identifier (e.g. :bridge/haystack)
     factory-fn  - (fn [] -> started-IMcpBridge)
     opts        - Options map:
       :pool-size              — Number of connections (default 3)
       :transport              — Transport keyword :stdio/:sse/:http
       :borrow-timeout-ms      — Max wait for connection (default 5000)
       :health-check-on-borrow — Validate on borrow (default true)
       :health-check-interval-ms — Periodic check ms (default 0)
       :replace-on-evict       — Replace dead connections (default true)
       :init-opts              — Options passed to factory (for reference)

   Returns:
     PooledBridge instance (satisfies both IAddon and IMcpBridge)

   Usage:
     (def pb (->pooled-bridge
               :bridge/echo
               (fn [] (let [b (->stdio-bridge (keyword (gensym \"bridge/echo-\")))
                            _ (bridge/start-bridge! b {:command [\"python3\" \"echo.py\"]})]
                        b))
               {:pool-size 3 :transport :stdio}))
     (bridge/register-bridge! pb {})"
  [id factory-fn opts]
  (let [pool-size   (get opts :pool-size (:pool-size default-pool-opts))
        transport   (get opts :transport :stdio)
        pool-opts   (select-keys opts [:borrow-timeout-ms
                                       :health-check-on-borrow
                                       :health-check-interval-ms
                                       :max-idle-ms
                                       :replace-on-evict])
        pool        (create-pool! factory-fn pool-size pool-opts)]
    (->PooledBridge id pool transport (get opts :init-opts {}))))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn pool?
  "Check if object is a ConnectionPool."
  [x]
  (instance? ConnectionPool x))

(defn pooled-bridge?
  "Check if object is a PooledBridge."
  [x]
  (instance? PooledBridge x))

;; =============================================================================
;; REPL / Comment
;; =============================================================================

(comment
  ;; Development REPL examples

  ;; 1. Create a pool with noop bridges (for testing)
  (require '[hive-mcp.addons.mcp-bridge :as bridge])
  (def noop-pool
    (create-pool! #(bridge/->noop-bridge (keyword (gensym "noop-")))
                  3
                  {:health-check-on-borrow false}))

  ;; 2. Borrow and return
  (let [conn (borrow noop-pool)]
    (println "Borrowed:" conn)
    (return! noop-pool conn))

  ;; 3. with-connection macro
  (with-connection [conn noop-pool]
    (bridge/call-tool conn "echo" {"message" "hi"}))

  ;; 4. Pool status
  (pool-status noop-pool)
  (pool-status noop-pool true) ;; with per-connection detail

  ;; 5. PooledBridge (transparent pool wrapper)
  (def pb (->pooled-bridge
           :bridge/test
           #(bridge/->noop-bridge (keyword (gensym "noop-")))
           {:pool-size 3 :transport :stdio :health-check-on-borrow false}))
  (bridge/bridge-status pb)
  (bridge/call-tool pb "echo" {"message" "hi"})

  ;; 6. Drain
  (drain! noop-pool)
  (pool-status noop-pool))

  ;; 7. Real usage with stdio bridge
  ;; (def pb (->pooled-bridge
  ;;           :bridge/echo
  ;;           (fn []
  ;;             (let [b (stdio/->stdio-bridge (keyword (gensym "echo-")))
  ;;                   r (bridge/start-bridge! b {:command ["python3" "scripts/echo-mcp-server.py"]})]
  ;;               (when (:success? r) b)))
  ;;           {:pool-size 3 :transport :stdio}))
  ;; (bridge/call-tool pb "echo" {"message" "hello from pool!"})

