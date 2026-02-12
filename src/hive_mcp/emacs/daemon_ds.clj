(ns hive-mcp.emacs.daemon-ds
  "DataScript implementation of IEmacsDaemon protocol.

   Manages Emacs daemon lifecycle state in the swarm DataScript database.
   Mirrors the coordinator pattern in hive-mcp.swarm.datascript.coordination.

   Uses the shared swarm connection via ensure-conn to colocate daemon
   entities with slave, task, and coordinator entities.

   DDD: Repository pattern for daemon entity persistence."
  (:require [datascript.core :as d]
            [taoensso.timbre :as log]
            [hive-mcp.emacs.daemon :as proto]
            [hive-mcp.swarm.datascript.schema :as schema]
            [hive-mcp.swarm.datascript.connection :as conn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(def ^:private stale-threshold-ms
  "Daemon is considered stale after this many milliseconds without heartbeat.
   Default: 2 minutes (coordinated with heartbeat interval of ~30s)"
  (* 2 60 1000))


(defn- daemon-entity->map
  "Convert a DataScript entity to a plain map, stripping :db/id."
  [db eid]
  (when eid
    (let [e (d/entity db eid)]
      (when e
        (-> (into {} e)
            (dissoc :db/id)
            ;; Materialize :db.cardinality/many into a plain set
            (update :emacs-daemon/lings #(when % (set %))))))))

(defn- find-daemon-eid
  "Find the entity ID for a daemon by its string ID. Returns nil if not found."
  [db daemon-id]
  (:db/id (d/entity db [:emacs-daemon/id daemon-id])))


(defrecord DataScriptDaemonStore []
  proto/IEmacsDaemon

  ;; --- Lifecycle ---

  (register! [_this daemon-id opts]
    {:pre [(string? daemon-id) (seq daemon-id)]}
    (let [{:keys [socket-name pid emacsclient]} opts
          c (conn/ensure-conn)
          tx-data {:emacs-daemon/id daemon-id
                   :emacs-daemon/socket-name (or socket-name daemon-id)
                   :emacs-daemon/status :active
                   :emacs-daemon/error-count 0
                   :emacs-daemon/started-at (conn/now)
                   :emacs-daemon/heartbeat-at (conn/now)}
          tx-data (cond-> tx-data
                    pid (assoc :emacs-daemon/pid pid)
                    emacsclient (assoc :emacs-daemon/emacsclient emacsclient))]
      (log/info "Registering Emacs daemon:" daemon-id
                "socket:" (or socket-name daemon-id)
                "pid:" pid)
      (d/transact! c [tx-data])))

  (heartbeat! [_this daemon-id]
    (let [c (conn/ensure-conn)
          db @c]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (log/trace "Heartbeat for daemon:" daemon-id)
        (d/transact! c [{:db/id eid
                         :emacs-daemon/heartbeat-at (conn/now)
                         :emacs-daemon/status :active}]))))

  (mark-error! [_this daemon-id error-message]
    (let [c (conn/ensure-conn)
          db @c]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (let [current (d/entity db eid)
              prev-count (or (:emacs-daemon/error-count current) 0)]
          (log/warn "Marking daemon error:" daemon-id "-" error-message)
          (d/transact! c [{:db/id eid
                           :emacs-daemon/status :error
                           :emacs-daemon/error-message error-message
                           :emacs-daemon/error-count (inc prev-count)}])))))

  (mark-terminated! [_this daemon-id]
    (let [c (conn/ensure-conn)
          db @c]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (log/info "Marking daemon terminated:" daemon-id)
        (d/transact! c [{:db/id eid
                         :emacs-daemon/status :terminated}]))))

  ;; --- Ling Binding ---

  (bind-ling! [_this daemon-id ling-id]
    (let [c (conn/ensure-conn)
          db @c]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (log/debug "Binding ling" ling-id "to daemon" daemon-id)
        (d/transact! c [[:db/add eid :emacs-daemon/lings ling-id]]))))

  (unbind-ling! [_this daemon-id ling-id]
    (let [c (conn/ensure-conn)
          db @c]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (log/debug "Unbinding ling" ling-id "from daemon" daemon-id)
        (d/transact! c [[:db/retract eid :emacs-daemon/lings ling-id]]))))

  ;; --- Queries ---

  (get-daemon [_this daemon-id]
    (let [c (conn/ensure-conn)
          db @c]
      (daemon-entity->map db (find-daemon-eid db daemon-id))))

  (get-all-daemons [_this]
    (let [c (conn/ensure-conn)
          db @c
          eids (d/q '[:find [?e ...]
                      :where [?e :emacs-daemon/id _]]
                    db)]
      (mapv #(daemon-entity->map db %) eids)))

  (get-daemons-by-status [_this status]
    {:pre [(contains? schema/daemon-statuses status)]}
    (let [c (conn/ensure-conn)
          db @c
          eids (d/q '[:find [?e ...]
                      :in $ ?status
                      :where
                      [?e :emacs-daemon/id _]
                      [?e :emacs-daemon/status ?status]]
                    db status)]
      (mapv #(daemon-entity->map db %) eids)))

  (get-daemon-for-ling [_this ling-id]
    (let [c (conn/ensure-conn)
          db @c
          eid (d/q '[:find ?e .
                     :in $ ?ling-id
                     :where
                     [?e :emacs-daemon/lings ?ling-id]]
                   db ling-id)]
      (daemon-entity->map db eid)))

  ;; --- Cleanup ---

  (cleanup-stale! [this]
    (proto/cleanup-stale! this {}))

  (cleanup-stale! [_this opts]
    (let [threshold (or (:threshold-ms opts) stale-threshold-ms)
          c (conn/ensure-conn)
          db @c
          cutoff-ms (- (System/currentTimeMillis) threshold)
          active-daemons (d/q '[:find ?e ?id ?hb
                                :where
                                [?e :emacs-daemon/id ?id]
                                [?e :emacs-daemon/status :active]
                                [?e :emacs-daemon/heartbeat-at ?hb]]
                              db)
          stale-pairs (->> active-daemons
                           (filter (fn [[_eid _id hb]]
                                     (< (.getTime hb) cutoff-ms)))
                           (map (fn [[eid id _]] [eid id])))]
      (when (seq stale-pairs)
        (log/warn "Found" (count stale-pairs) "stale Emacs daemons")
        (doseq [[eid daemon-id] stale-pairs]
          (log/warn "Marking daemon stale:" daemon-id)
          (d/transact! c [{:db/id eid :emacs-daemon/status :stale}]))
        (mapv second stale-pairs)))))


(defn create-store
  "Create a new DataScriptDaemonStore instance.
   Uses the shared swarm DataScript connection."
  []
  (->DataScriptDaemonStore))
