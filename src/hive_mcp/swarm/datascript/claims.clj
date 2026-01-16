;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.datascript.claims
  "Wait-queue management for file-claim event cascade.

   When a ling wants to modify a file that is already claimed,
   it joins the wait-queue for that file. When the claim is released,
   the event system notifies all waiting lings.

   Functions:
   - add-to-wait-queue!      - Add ling to wait queue for file
   - remove-from-wait-queue! - Remove ling from queue
   - get-waiting-lings       - Query lings waiting on file (for coeffect)
   - get-ling-wait-queue     - What files is a ling waiting for?

   SOLID-S: Single Responsibility - wait-queue lifecycle only.
   DDD: Aggregate root for wait-queue entries."
  (:require [datascript.core :as d]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.datascript.connection :as conn]))

;;; =============================================================================
;;; Wait-Queue CRUD Functions
;;; =============================================================================

(defn add-to-wait-queue!
  "Add a ling to the wait queue for a specific file.

   If the ling is already waiting for this file, this is a no-op
   (upsert behavior via composite unique key).

   Arguments:
     ling-id   - ID of the ling waiting for access
     file-path - Path to the file being waited on

   Returns:
     Transaction report"
  [ling-id file-path]
  {:pre [(string? ling-id)
         (string? file-path)]}
  (let [c (conn/ensure-conn)
        ;; Create composite ID for uniqueness: ling + file
        wait-id (str "wait:" ling-id ":" file-path)
        tx-data {:wait-queue/id wait-id
                 :wait-queue/ling-id ling-id
                 :wait-queue/file file-path
                 :wait-queue/queued-at (conn/now)}]
    (log/debug "Adding to wait-queue: ling" ling-id "waiting for" file-path)
    (d/transact! c [tx-data])))

(defn remove-from-wait-queue!
  "Remove a ling from the wait queue for a specific file.

   Arguments:
     ling-id   - ID of the ling to remove
     file-path - Path to the file

   Returns:
     Transaction report or nil if not found"
  [ling-id file-path]
  (let [c (conn/ensure-conn)
        db @c
        wait-id (str "wait:" ling-id ":" file-path)]
    (when-let [eid (:db/id (d/entity db [:wait-queue/id wait-id]))]
      (log/debug "Removing from wait-queue: ling" ling-id "for file" file-path)
      (d/transact! c [[:db/retractEntity eid]]))))

(defn remove-all-from-wait-queue-for-file!
  "Remove all lings from the wait queue for a specific file.

   Called when a file claim is released to clean up the queue.

   Arguments:
     file-path - Path to the file

   Returns:
     Number of entries removed"
  [file-path]
  (let [c (conn/ensure-conn)
        db @c
        entries (d/q '[:find ?e
                       :in $ ?file
                       :where
                       [?e :wait-queue/file ?file]]
                     db file-path)]
    (when (seq entries)
      (log/debug "Removing" (count entries) "wait-queue entries for file" file-path)
      (doseq [[eid] entries]
        (d/transact! c [[:db/retractEntity eid]])))
    (count entries)))

;;; =============================================================================
;;; Wait-Queue Query Functions (for coeffects)
;;; =============================================================================

(defn get-waiting-lings
  "Get all lings waiting for access to a specific file.

   Returns entries sorted by queued-at (FIFO order - earliest first).

   Arguments:
     file-path - Path to the file

   Returns:
     Vector of maps with :ling-id and :queued-at, sorted FIFO"
  [file-path]
  (let [c (conn/ensure-conn)
        db @c
        entries (d/q '[:find ?ling-id ?queued-at
                       :in $ ?file
                       :where
                       [?e :wait-queue/file ?file]
                       [?e :wait-queue/ling-id ?ling-id]
                       [?e :wait-queue/queued-at ?queued-at]]
                     db file-path)]
    (->> entries
         (map (fn [[ling-id queued-at]]
                {:ling-id ling-id
                 :queued-at queued-at}))
         (sort-by :queued-at)
         vec)))

(defn get-ling-wait-queue
  "Get all files a specific ling is waiting for.

   Arguments:
     ling-id - ID of the ling

   Returns:
     Vector of file paths the ling is waiting for"
  [ling-id]
  (let [c (conn/ensure-conn)
        db @c
        files (d/q '[:find ?file
                     :in $ ?ling-id
                     :where
                     [?e :wait-queue/ling-id ?ling-id]
                     [?e :wait-queue/file ?file]]
                   db ling-id)]
    (vec (map first files))))

;;; =============================================================================
;;; Utility Functions
;;; =============================================================================

(defn ling-is-waiting?
  "Check if a ling is waiting for any files.

   Arguments:
     ling-id - ID of the ling

   Returns:
     true if ling has entries in wait-queue"
  [ling-id]
  (let [c (conn/ensure-conn)
        db @c]
    (some? (d/q '[:find ?e .
                  :in $ ?ling-id
                  :where
                  [?e :wait-queue/ling-id ?ling-id]]
                db ling-id))))

(defn file-has-waiters?
  "Check if any lings are waiting for a specific file.

   Arguments:
     file-path - Path to the file

   Returns:
     true if file has entries in wait-queue"
  [file-path]
  (let [c (conn/ensure-conn)
        db @c]
    (some? (d/q '[:find ?e .
                  :in $ ?file
                  :where
                  [?e :wait-queue/file ?file]]
                db file-path))))
