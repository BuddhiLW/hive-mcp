(ns hive-mcp.knowledge-graph.disc.crud
  "CRUD operations for disc entities in DataScript.

   Manages disc entity lifecycle: create, read, update, delete, query.
   Extracted from disc.clj (Sprint 2 decomposition).

   CLARITY-Y: Graceful failure with status codes instead of exceptions."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.disc.hash :as hash]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Disc Entity CRUD
;; =============================================================================

(defn add-disc!
  "Create or update a disc entity for a file path.

   Initializes Bayesian certainty fields based on file volatility:
   - Classifies volatility from path patterns (:stable/:moderate/:volatile)
   - Sets initial alpha/beta priors (higher alpha for stable files)
   - Sets last-observation to now

   Arguments:
     opts - Map with:
       :path         - File path (required, unique identity)
       :content-hash - SHA256 of file content
       :analyzed-at  - Timestamp of analysis (defaults to now)
       :git-commit   - Git commit hash when analyzed
       :project-id   - Project scope

   Returns the entity ID."
  [{:keys [path content-hash analyzed-at git-commit project-id]}]
  {:pre [(string? path) (seq path)]}
  (let [now (java.util.Date.)
        volatility (vol/classify-volatility path)
        initial-alpha (get vol/initial-alpha-by-volatility volatility 5.0)
        tx-data [{:disc/path path
                  :disc/content-hash (or content-hash "")
                  :disc/analyzed-at (or analyzed-at now)
                  :disc/git-commit (or git-commit "")
                  :disc/project-id (or project-id "global")
                  ;; Initialize Bayesian certainty fields
                  :disc/certainty-alpha initial-alpha
                  :disc/certainty-beta 2.0
                  :disc/volatility-class volatility
                  :disc/last-observation now}]
        result (conn/transact! tx-data)]
    (log/debug "Added/updated disc entity" {:path path :volatility volatility
                                            :initial-certainty (/ initial-alpha (+ initial-alpha 2.0))})
    ;; Return the entity ID
    (-> result :tx-data first :e)))

(defn get-disc
  "Get disc entity by file path.
   Returns entity map or nil if not found."
  [path]
  {:pre [(string? path)]}
  (let [result (conn/query '[:find (pull ?e [*])
                             :in $ ?path
                             :where [?e :disc/path ?path]]
                           path)]
    (ffirst result)))

(defn disc-exists?
  "Check if a disc entity exists for the given path."
  [path]
  (some? (get-disc path)))

(defn update-disc!
  "Update a disc entity.
   Path is used to find the entity; other fields are updated."
  [path updates]
  {:pre [(string? path)]}
  (when-let [_existing (get-disc path)]
    (let [tx-data [(merge {:disc/path path} updates)]
          _ (conn/transact! tx-data)]
      (log/debug "Updated disc entity" {:path path :updates (keys updates)})
      (get-disc path))))

(defn remove-disc!
  "Remove a disc entity by path.
   Returns true if removed, nil if not found."
  [path]
  {:pre [(string? path)]}
  (when-let [disc (get-disc path)]
    (let [eid (:db/id disc)]
      (conn/transact! [[:db.fn/retractEntity eid]])
      (log/debug "Removed disc entity" {:path path})
      true)))

;; =============================================================================
;; Disc Queries
;; =============================================================================

(defn get-all-discs
  "Get all disc entities.
   Optional project-id filter."
  [& {:keys [project-id]}]
  (let [results (if project-id
                  (conn/query '[:find (pull ?e [*])
                                :in $ ?pid
                                :where
                                [?e :disc/path _]
                                [?e :disc/project-id ?pid]]
                              project-id)
                  (conn/query '[:find (pull ?e [*])
                                :where [?e :disc/path _]]))]
    (map first results)))

;; =============================================================================
;; Read Tracking
;; =============================================================================

(defn touch-disc!
  "Record that a file was read by an agent.
   Creates the disc entity if it doesn't exist, updates last-read-at and
   increments read-count. Returns the updated disc entity.

   Arguments:
     path       - File path (required)
     project-id - Project scope (optional, defaults to 'global')"
  [path & {:keys [project-id]}]
  {:pre [(string? path) (seq path)]}
  (let [now (java.util.Date.)]
    (if (disc-exists? path)
      ;; Update existing: bump last-read-at and read-count
      (let [existing (get-disc path)
            current-count (or (:disc/read-count existing) 0)]
        (update-disc! path {:disc/last-read-at now
                            :disc/read-count (inc current-count)})
        (get-disc path))
      ;; Create new: also compute content hash for fresh tracking
      (let [{:keys [hash]} (hash/file-content-hash path)]
        (add-disc! {:path path
                    :content-hash (or hash "")
                    :project-id (or project-id "global")})
        (update-disc! path {:disc/last-read-at now
                            :disc/read-count 1})
        (get-disc path)))))
