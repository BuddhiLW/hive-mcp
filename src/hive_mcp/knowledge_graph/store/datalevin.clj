(ns hive-mcp.knowledge-graph.store.datalevin
  "Datalevin implementation of IGraphStore protocol.

   Persistent Datalog store backed by LMDB. Data survives restarts.
   Schema translation from DataScript format (no :db/valueType)
   to Datalevin format (explicit :db/valueType for range queries).

   CLARITY-I: Validates schema before connecting.
   CLARITY-T: Logs backend selection, path, schema translation.
   CLARITY-Y: Falls back to DataScript with warning if Datalevin fails."
  (:require [datalevin.core :as dtlv]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.schema :as schema]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schema Translation (DataScript → Datalevin)
;; =============================================================================

(def ^:private value-type-map
  "Maps DataScript attribute names to their Datalevin :db/valueType.
   DataScript is type-agnostic; Datalevin requires explicit types
   for range queries to work correctly."
  {;; KG Edge attributes
   :kg-edge/id            :db.type/string
   :kg-edge/from          :db.type/string
   :kg-edge/to            :db.type/string
   :kg-edge/relation      :db.type/keyword
   :kg-edge/scope         :db.type/string
   :kg-edge/confidence    :db.type/double
   :kg-edge/created-by    :db.type/string
   :kg-edge/created-at    :db.type/instant
   :kg-edge/last-verified :db.type/instant
   :kg-edge/source-type   :db.type/keyword

   ;; Knowledge abstraction attributes
   :knowledge/abstraction-level :db.type/long
   :knowledge/grounded-at       :db.type/instant
   :knowledge/grounded-from     :db.type/string
   :knowledge/gaps              :db.type/keyword
   :knowledge/source-hash       :db.type/string
   :knowledge/source-type       :db.type/keyword

   ;; Disc (file state) attributes
   :disc/path         :db.type/string
   :disc/content-hash :db.type/string
   :disc/analyzed-at  :db.type/instant
   :disc/git-commit   :db.type/string
   :disc/project-id   :db.type/string
   :disc/last-read-at :db.type/instant
   :disc/read-count   :db.type/long})

(defn translate-schema
  "Translate DataScript schema to Datalevin schema.

   Transformations:
   1. Strip :db/doc (not supported by Datalevin)
   2. Add :db/valueType from value-type-map
   3. Preserve :db/unique and :db/cardinality

   Arguments:
     ds-schema - DataScript schema (map of attribute → props)

   Returns:
     Datalevin schema (map of attribute → props with :db/valueType)"
  [ds-schema]
  (reduce-kv
   (fn [acc attr props]
     (let [;; Strip :db/doc (not supported by Datalevin)
           clean-props (dissoc props :db/doc)
           ;; Add :db/valueType if we know the type
           typed-props (if-let [vt (get value-type-map attr)]
                         (assoc clean-props :db/valueType vt)
                         clean-props)]
       (assoc acc attr typed-props)))
   {}
   ds-schema))

(defn datalevin-schema
  "Get the full Datalevin-compatible KG schema.
   Translates from DataScript format."
  []
  (translate-schema (schema/full-schema)))

;; =============================================================================
;; Input Validation (CLARITY-I)
;; =============================================================================

(defn- validate-db-path!
  "Validate and ensure the database directory path exists.
   Creates parent directories if needed.
   Throws on invalid path."
  [db-path]
  (when (or (nil? db-path) (empty? db-path))
    (throw (ex-info "Datalevin db-path cannot be nil or empty"
                    {:db-path db-path})))
  (let [dir (io/file db-path)]
    (when-not (.exists (.getParentFile dir))
      (log/info "Creating Datalevin parent directory" {:path (.getParent dir)})
      (.mkdirs (.getParentFile dir)))
    db-path))

;; =============================================================================
;; Datalevin Store Implementation
;; =============================================================================

(defrecord DatalevinStore [conn-atom db-path]
  proto/IGraphStore

  (ensure-conn! [_this]
    (when (nil? @conn-atom)
      (log/info "Initializing Datalevin KG store" {:path db-path})
      (validate-db-path! db-path)
      (let [dtlv-schema (datalevin-schema)]
        (log/debug "Datalevin schema translated"
                   {:attributes (count dtlv-schema)})
        (reset! conn-atom (dtlv/get-conn db-path dtlv-schema))))
    @conn-atom)

  (transact! [this tx-data]
    (dtlv/transact! (proto/ensure-conn! this) tx-data))

  (query [this q]
    (dtlv/q q (dtlv/db (proto/ensure-conn! this))))

  (query [this q inputs]
    (apply dtlv/q q (dtlv/db (proto/ensure-conn! this)) inputs))

  (entity [this eid]
    (dtlv/entity (dtlv/db (proto/ensure-conn! this)) eid))

  (entid [this lookup-ref]
    (dtlv/entid (dtlv/db (proto/ensure-conn! this)) lookup-ref))

  (pull-entity [this pattern eid]
    (dtlv/pull (dtlv/db (proto/ensure-conn! this)) pattern eid))

  (db-snapshot [this]
    (dtlv/db (proto/ensure-conn! this)))

  (reset-conn! [this]
    (log/info "Resetting Datalevin KG store" {:path db-path})
    ;; Close existing connection if open
    (when-let [c @conn-atom]
      (try
        (dtlv/close c)
        (catch Exception e
          (log/warn "Failed to close Datalevin conn during reset"
                    {:error (.getMessage e)}))))
    ;; Delete the database directory contents
    (let [dir (io/file db-path)]
      (when (.exists dir)
        (doseq [f (reverse (file-seq dir))]
          (.delete f))))
    ;; Reconnect with fresh schema
    (reset! conn-atom nil)
    (proto/ensure-conn! this))

  (close! [_this]
    (when-let [c @conn-atom]
      (log/info "Closing Datalevin KG store" {:path db-path})
      (try
        (dtlv/close c)
        (catch Exception e
          (log/warn "Failed to close Datalevin connection"
                    {:error (.getMessage e)})))
      (reset! conn-atom nil))))

(def ^:private default-db-path "data/kg/datalevin")

(defn create-store
  "Create a new Datalevin-backed graph store.

   Arguments:
     opts - Optional map with:
       :db-path - Path for LMDB storage (default: data/kg/datalevin)

   Returns an IGraphStore implementation.

   CLARITY-Y: If Datalevin fails to initialize, logs warning
   and returns nil (caller should fall back to DataScript)."
  [& [{:keys [db-path] :or {db-path default-db-path}}]]
  (try
    (log/info "Creating Datalevin graph store" {:path db-path})
    (->DatalevinStore (atom nil) db-path)
    (catch Exception e
      (log/error "Failed to create Datalevin store, falling back to DataScript"
                 {:error (.getMessage e) :path db-path})
      nil)))
