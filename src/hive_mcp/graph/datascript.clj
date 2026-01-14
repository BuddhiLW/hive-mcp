(ns hive-mcp.graph.datascript
  "Datascript implementation of GraphStore protocol.
   
   Provides in-memory graph storage with EDN-based persistence.
   Uses Datascript for transaction semantics and Datalog queries.
   
   Features:
   - Full GraphStore protocol implementation
   - EDN file persistence (configurable path)
   - Simple string matching for find-similar (no vector embeddings)
   - Thread-safe via Datascript's atom-based connection
   
   SOLID: Dependency Inversion - depends on protocol, not concrete stores.
   DDD: Repository pattern for graph persistence."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [datascript.core :as d]
            [hive-mcp.graph.protocol :as proto]
            [hive-mcp.graph.schema :as schema]
            [taoensso.timbre :as log]))

;;; -----------------------------------------------------------------------------
;;; String Similarity (for find-similar)
;;; -----------------------------------------------------------------------------

(defn- normalize-text
  "Normalize text for comparison: lowercase, collapse whitespace."
  [s]
  (when s
    (-> s
        str/lower-case
        str/trim
        (str/replace #"\s+" " "))))

(defn- word-set
  "Extract set of words from text for comparison."
  [s]
  (when s
    (-> s
        normalize-text
        (str/split #"\s+")
        set)))

(defn- jaccard-similarity
  "Compute Jaccard similarity between two word sets."
  [set-a set-b]
  (if (or (empty? set-a) (empty? set-b))
    0.0
    (let [intersection-size (count (set/intersection set-a set-b))
          union-size (count (set/union set-a set-b))]
      (if (zero? union-size)
        0.0
        (double (/ intersection-size union-size))))))

(defn- text-similarity
  "Compute text similarity score (0.0-1.0) using Jaccard word similarity."
  [text-a text-b]
  (jaccard-similarity (word-set text-a) (word-set text-b)))

;;; -----------------------------------------------------------------------------
;;; Content Extraction Helpers
;;; -----------------------------------------------------------------------------

(defn- entity-content-attr
  "Return the content attribute for an entity type."
  [entity-type]
  (case entity-type
    :friction :friction/context
    :knowledge :knowledge/content
    :agent :agent/id
    nil))

(defn- entity-type-attr
  "Return the type attribute for an entity type."
  [entity-type]
  (case entity-type
    :friction :friction/type
    :knowledge :knowledge/type
    :agent :agent/type
    nil))

;;; -----------------------------------------------------------------------------
;;; DatascriptStore Record
;;; -----------------------------------------------------------------------------

(defrecord DatascriptStore [conn persist-path]
  proto/GraphStore

  (transact! [this tx-data]
    (log/debug "Transacting" (count tx-data) "facts")
    (try
      (let [report (d/transact! conn tx-data)]
        {:tx-data (:tx-data report)
         :tempids (:tempids report)})
      (catch Exception e
        (log/error e "Transaction failed")
        (throw (ex-info "Transaction failed"
                        {:cause (.getMessage e)
                         :tx-data tx-data}
                        e)))))

  (query [this datalog-query]
    (d/q datalog-query @conn))

  (query [this datalog-query args]
    (apply d/q datalog-query @conn args))

  (entity [this eid]
    (when-let [e (d/entity @conn eid)]
      (into {} e)))

  (find-similar [this entity-type content]
    (when-let [content-attr (entity-content-attr entity-type)]
      (let [db @conn
            type-attr (entity-type-attr entity-type)
            ;; Find all entities of this type with content
            query '[:find ?e ?content
                    :in $ ?type-attr ?content-attr
                    :where
                    [?e ?type-attr _]
                    [?e ?content-attr ?content]]
            results (d/q query db type-attr content-attr)
            ;; Score by similarity
            scored (->> results
                        (map (fn [[eid existing-content]]
                               {:eid eid
                                :similarity (text-similarity content existing-content)
                                :content existing-content}))
                        (filter #(> (:similarity %) 0.1)) ; Threshold
                        (sort-by :similarity >)
                        (take 10))]
        scored)))

  (history [this eid]
    ;; Datascript doesn't have native history support
    nil)

  (persist! [this]
    (when persist-path
      (let [db @conn
            db-data (d/serializable db)
            file (io/file persist-path)]
        ;; Ensure parent directories exist
        (when-let [parent (.getParentFile file)]
          (.mkdirs parent))
        (spit file (pr-str db-data))
        (log/info "Persisted graph to" persist-path)
        {:persisted-at (java.time.Instant/now)})))

  (restore! [this]
    (when persist-path
      (let [file (io/file persist-path)]
        (if (.exists file)
          (try
            (let [data (edn/read-string (slurp file))
                  db (d/from-serializable data)]
              ;; Reset connection with restored db
              (reset! conn db)
              (let [entity-count (count (d/q '[:find ?e :where [?e _ _]] db))]
                (log/info "Restored graph from" persist-path "- entities:" entity-count)
                {:restored-at (java.time.Instant/now)
                 :entity-count entity-count}))
            (catch Exception e
              (log/error e "Failed to restore from" persist-path)
              (throw (ex-info "Restore failed"
                              {:path persist-path
                               :cause (.getMessage e)}
                              e))))
          (do
            (log/debug "No persisted state found at" persist-path)
            nil))))))

;;; -----------------------------------------------------------------------------
;;; Constructor
;;; -----------------------------------------------------------------------------

(defn create-store
  "Create a new DatascriptStore.
   
   Arguments:
     schema - Datascript schema map (use hive-mcp.graph.schema/schema)
     persist-path - Path for EDN persistence (optional, nil to disable)
   
   Returns:
     DatascriptStore record implementing GraphStore protocol.
   
   Example:
     (create-store schema/schema \"/tmp/hive-graph.edn\")"
  [schema persist-path]
  (let [conn (d/create-conn schema)]
    (log/info "Created DatascriptStore"
              (when persist-path (str "with persistence at " persist-path)))
    (->DatascriptStore conn persist-path)))

(defn create-default-store
  "Create a DatascriptStore with the default hive-mcp schema.
   
   Arguments:
     persist-path - Path for EDN persistence (optional, nil to disable)
   
   Example:
     (create-default-store \"/tmp/hive-graph.edn\")"
  [persist-path]
  (create-store schema/schema persist-path))

;;; -----------------------------------------------------------------------------
;;; Convenience Functions (for crystal/graph.clj compatibility)
;;; -----------------------------------------------------------------------------

(defn make-datascript-store
  "Create a DatascriptStore with default schema and no persistence.
   Convenience function for modules that need a quick store."
  []
  (create-store schema/schema nil))

(defn ds-reset!
  "Reset a DatascriptStore to empty state."
  [store]
  (let [new-db (d/empty-db schema/schema)]
    (clojure.core/reset! (:conn store) new-db)))

(defn db
  "Get the current database value from a store."
  [store]
  @(:conn store))

(defn retract!
  "Retract an entity or specific datom from the store.
   
   lookup-ref: [:attr value] to identify entity
   OR [eid attr value] to retract specific datom"
  [store lookup-ref]
  (let [conn (:conn store)]
    (if (= 2 (count lookup-ref))
      ;; Entity retraction by lookup ref
      (when-let [eid (:db/id (d/entity @conn lookup-ref))]
        (d/transact! conn [[:db/retractEntity eid]]))
      ;; Specific datom retraction [eid attr value]
      (let [[eid attr value] lookup-ref]
        (d/transact! conn [[:db/retract eid attr value]])))))

(defn query
  "Query the store with optional argument map.
   
   Extends protocol query to support {:id val :ctx val} style args.
   Maps ?id in query to :id in args-map (strips ? prefix)."
  ([store datalog-query]
   (d/q datalog-query @(:conn store)))
  ([store datalog-query args-map]
   (if (map? args-map)
     ;; Map-style args: extract values in order they appear in query
     (let [in-vars (->> datalog-query
                        (drop-while #(not= % :in))
                        rest
                        (take-while #(not= % :where))
                        (remove #{'$}))
           ;; Strip ? prefix from var names to get map keys
           var->key (fn [v] (keyword (subs (name v) 1)))]
       (if (seq in-vars)
         (apply d/q datalog-query @(:conn store)
                (map #(get args-map (var->key %)) in-vars))
         (d/q datalog-query @(:conn store))))
     ;; Vector-style args (passthrough)
     (apply d/q datalog-query @(:conn store) (if (sequential? args-map) args-map [args-map])))))

(defn ds-entity
  "Get entity by eid or lookup ref."
  [store eid-or-ref]
  (d/entity @(:conn store) eid-or-ref))

(defn transact!
  "Transact data into the store. Wrapper for protocol method."
  [store tx-data]
  (d/transact! (:conn store) tx-data))

;;; -----------------------------------------------------------------------------
;;; Convenience Functions
;;; -----------------------------------------------------------------------------

(defn store-stats
  "Get statistics about a DatascriptStore."
  [store]
  (let [db @(:conn store)]
    {:entity-count (count (d/q '[:find ?e :where [?e _ _]] db))
     :friction-count (count (d/q '[:find ?e :where [?e :friction/type _]] db))
     :knowledge-count (count (d/q '[:find ?e :where [?e :knowledge/type _]] db))
     :agent-count (count (d/q '[:find ?e :where [?e :agent/id _]] db))
     :persist-path (:persist-path store)}))
