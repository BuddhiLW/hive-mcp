(ns hive-mcp.crystal.graph
  "Knowledge graph for memory entries using Datascript GraphStore.
   
   Builds a queryable graph from memory entries, enabling:
   - Session lineage tracking (which session produced which entries)
   - Cross-reference discovery (entry-to-entry links)
   - Recall pattern analysis (access frequency by context)
   - Promotion candidate identification
   
   Reconstructed on startup from Emacs memory store.
   
   SOLID: Single responsibility - graph queries only.
   DDD: Persistence adapter for memory domain."
  (:require [clojure.data.json :as json]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.graph.datascript :as ds]
            [hive-mcp.graph.schema :as schema]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Database State (Datascript GraphStore)
;; =============================================================================

(defonce ^:private graph-store
  (delay (ds/make-datascript-store)))

(defonce ^:private last-rebuild
  (atom nil))

(defn reset-db!
  "Reset the graph database to empty state."
  []
  (ds/ds-reset! @graph-store)
  (reset! last-rebuild nil)
  (log/debug "Graph database reset"))

(defn get-db
  "Get current database (for debugging/testing)."
  []
  (ds/db @graph-store))

;; =============================================================================
;; Database Mutation Functions
;; =============================================================================

(defn add-entry-fact!
  "Add a memory entry to the graph database.
   
   entry: {:id :type :duration :tags :recalls :created}
   
   Extracts session from tags and populates all relations."
  [{:keys [id type duration tags recalls] :as entry}]
  (when id
    ;; Add base memory entry
    (let [memory-entity (schema/make-memory
                         {:id id
                          :type (keyword type)
                          :duration (keyword duration)})
          recall-entities (map (fn [{:keys [context count] :or {count 1}}]
                                 (schema/make-recall
                                  {:memory [:memory/id id]
                                   :context (keyword context)
                                   :count count}))
                               recalls)]

      ;; Transact all entities
      (ds/transact! @graph-store [memory-entity])

      ;; Add session if present
      (when-let [session (crystal/extract-session-from-tags tags)]
        (ds/transact! @graph-store [{:memory/id id
                                     :memory/session-id session}]))

      ;; Add tags
      (when (seq tags)
        (ds/transact! @graph-store (map (fn [tag]
                                          {:memory/id id
                                           :memory/tags tag})
                                        tags)))

      ;; Add recalls
      (when (seq recall-entities)
        (ds/transact! @graph-store recall-entities))

      (log/trace "Added entry to graph:" id type duration))))

(defn add-reference!
  "Add a cross-reference between entries."
  [from-entry-id to-entry-id]
  ;; Need to look up the target entity ID since :memory/references is a ref
  (when-let [to-eid (:db/id (ds/ds-entity @graph-store [:memory/id to-entry-id]))]
    (ds/transact! @graph-store [{:memory/id from-entry-id
                                 :memory/references to-eid}])
    (log/trace "Added reference:" from-entry-id "->" to-entry-id)))

(defn remove-entry!
  "Remove an entry and all its relations from the graph."
  [entry-id]
  ;; Get all facts related to this entry
  (let [entry-entity (ds/ds-entity @graph-store [:memory/id entry-id])
        session-facts (ds/query @graph-store
                                '[:find ?s
                                  :where
                                  [?e :memory/id ?id]
                                  [?e :memory/session-id ?s]]
                                {:id entry-id})
        tag-facts (ds/query @graph-store
                            '[:find ?t
                              :where
                              [?e :memory/id ?id]
                              [?e :memory/tags ?t]]
                            {:id entry-id})
        recall-facts (ds/query @graph-store
                               '[:find ?ctx ?cnt
                                 :where
                                 [?r :recall/memory ?id]
                                 [?r :recall/context ?ctx]
                                 [?r :recall/count ?cnt]]
                               {:id entry-id})
        refs-from (ds/query @graph-store
                            '[:find ?ref
                              :where
                              [?e :memory/id ?id]
                              [?e :memory/references ?ref]]
                            {:id entry-id})
        refs-to (ds/query @graph-store
                          '[:find ?ref
                            :where
                            [?e :memory/references ?id]
                            [?e :memory/id ?ref]]
                          {:id entry-id})]

    ;; Retract all facts
    (when entry-entity
      (ds/retract! @graph-store [:memory/id entry-id]))

    ;; Retract session facts
    (doseq [[session] session-facts]
      (ds/retract! @graph-store [:memory/id entry-id :memory/session-id session]))

    ;; Retract tag facts
    (doseq [[tag] tag-facts]
      (ds/retract! @graph-store [:memory/id entry-id :memory/tags tag]))

    ;; Retract recall facts
    (doseq [[ctx cnt] recall-facts]
      (let [recall-id (ffirst (ds/query @graph-store
                                        '[:find ?r
                                          :where
                                          [?r :recall/memory ?id]
                                          [?r :recall/context ?ctx]
                                          [?r :recall/count ?cnt]]
                                        {:id entry-id :ctx ctx :cnt cnt}))]
        (when recall-id
          (ds/retract! @graph-store [:recall/id recall-id]))))

    ;; Retract reference facts
    (doseq [[ref] refs-from]
      (ds/retract! @graph-store [:memory/id entry-id :memory/references ref]))

    (doseq [[ref] refs-to]
      (ds/retract! @graph-store [:memory/id ref :memory/references entry-id]))

    (log/debug "Removed entry from graph:" entry-id)))

(defn update-entry-duration!
  "Update an entry's duration (e.g., after promotion)."
  [entry-id new-duration]
  (let [current (ds/ds-entity @graph-store [:memory/id entry-id])]
    (when current
      (ds/transact! @graph-store [[:db/retract (:db/id current) :memory/duration (:memory/duration current)]
                                  [:db/add (:db/id current) :memory/duration (keyword new-duration)]])
      (log/debug "Updated entry duration:" entry-id new-duration))))

(defn log-access!
  "Log an access to an entry with context."
  [entry-id context]
  ;; First lookup the memory entity
  (when-let [memory-eid (:db/id (ds/ds-entity @graph-store [:memory/id entry-id]))]
    ;; Find existing recall entity
    (let [existing (first (ds/query @graph-store
                                    '[:find ?r ?cnt
                                      :in $ ?mem ?ctx
                                      :where
                                      [?r :recall/memory ?mem]
                                      [?r :recall/context ?ctx]
                                      [?r :recall/count ?cnt]]
                                    {:mem memory-eid :ctx (keyword context)}))]
      (if existing
        (let [[recall-id cnt] existing]
          ;; Update existing recall
          (ds/transact! @graph-store [{:db/id recall-id :recall/count (inc cnt)}]))
        ;; Create new recall
        (ds/transact! @graph-store [(schema/make-recall
                                     {:memory memory-eid
                                      :context (keyword context)
                                      :count 1})]))
      (log/trace "Logged access:" entry-id context))))

;; =============================================================================
;; Memory Integration - Rebuild from Emacs
;; =============================================================================

(defn- query-emacs-memory
  "Query Emacs memory for entries of a given type.
   Returns parsed JSON or nil on error."
  [entry-type]
  (let [elisp (format "(json-encode (hive-mcp-api-memory-query %s nil 1000 \"all\"))"
                      (pr-str entry-type))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (try
        (json/read-str result :key-fn keyword)
        (catch Exception e
          (log/warn "Failed to parse memory query result:" (.getMessage e))
          nil))
      (do
        (log/warn "Memory query failed:" error)
        nil))))

(defn rebuild-from-memory!
  "Rebuild the graph database from Emacs memory store.
   
   Queries all memory types and populates the logic database.
   Returns {:entries-loaded int :errors []}."
  []
  (log/info "Rebuilding graph from Emacs memory...")
  (reset-db!)

  (let [entry-types ["note" "snippet" "convention" "decision"]
        results (atom {:entries-loaded 0 :errors []})
        start-time (System/currentTimeMillis)]

    (doseq [entry-type entry-types]
      (if-let [entries (query-emacs-memory entry-type)]
        (doseq [entry entries]
          (try
            (add-entry-fact! entry)
            (swap! results update :entries-loaded inc)
            (catch Exception e
              (swap! results update :errors conj
                     {:entry-id (:id entry) :error (.getMessage e)}))))
        (swap! results update :errors conj
               {:type entry-type :error "Query failed"})))

    (let [elapsed (- (System/currentTimeMillis) start-time)]
      (reset! last-rebuild (java.time.Instant/now))
      (log/info "Graph rebuild complete:"
                (:entries-loaded @results) "entries in" elapsed "ms")
      @results)))

(defn ensure-rebuilt!
  "Ensure the graph is rebuilt if stale or empty.
   
   Options:
   - :force - always rebuild
   - :max-age-ms - rebuild if older than this (default: 5 min)"
  [& {:keys [force max-age-ms] :or {max-age-ms 300000}}]
  (let [should-rebuild? (or force
                            (nil? @last-rebuild)
                            (> (- (System/currentTimeMillis)
                                  (.toEpochMilli @last-rebuild))
                               max-age-ms))]
    (when should-rebuild?
      (rebuild-from-memory!))))

;; =============================================================================
;; Query Functions (Public API)
;; =============================================================================

(defn session-entries
  "Get all entries produced in a session.
   
   session-id: date string or session tag
   
   Returns: [{:id :type :duration} ...]"
  [session-id]
  (let [results (ds/query @graph-store
                          '[:find ?id ?type ?duration
                            :in $ ?session
                            :where
                            [?e :memory/id ?id]
                            [?e :memory/type ?type]
                            [?e :memory/duration ?duration]
                            [?e :memory/session-id ?session]]
                          {:session session-id})]
    (map (fn [[id type duration]]
           {:id id :type type :duration duration})
         results)))

(defn entry-lineage
  "Find all entries that reference a given entry (reverse lookup).
   
   entry-id: the entry to find references to
   
   Returns: [{:id :type :duration} ...] of entries that reference this one"
  [entry-id]
  (let [results (ds/query @graph-store
                          '[:find ?src-id ?type ?duration
                            :in $ ?id
                            :where
                            [?target :memory/id ?id]
                            [?src :memory/references ?target]
                            [?src :memory/id ?src-id]
                            [?src :memory/type ?type]
                            [?src :memory/duration ?duration]]
                          {:id entry-id})]
    (map (fn [[src-id type duration]]
           {:id src-id :type type :duration duration})
         results)))

(defn entry-references-to
  "Find all entries that a given entry references (forward lookup).
   
   entry-id: the entry to find outgoing references from
   
   Returns: [{:id :type :duration} ...]"
  [entry-id]
  (let [results (ds/query @graph-store
                          '[:find ?ref-id ?type ?duration
                            :in $ ?id
                            :where
                            [?e :memory/id ?id]
                            [?e :memory/references ?ref]
                            [?ref :memory/id ?ref-id]
                            [?ref :memory/type ?type]
                            [?ref :memory/duration ?duration]]
                          {:id entry-id})]
    (map (fn [[ref-id type duration]]
           {:id ref-id :type type :duration duration})
         results)))

(defn entries-by-tag
  "Find all entries with a given tag.
   
   tag: the tag to search for
   
   Returns: [{:id :type :duration} ...]"
  [tag]
  (let [results (ds/query @graph-store
                          '[:find ?id ?type ?duration
                            :in $ ?tag
                            :where
                            [?e :memory/id ?id]
                            [?e :memory/type ?type]
                            [?e :memory/duration ?duration]
                            [?e :memory/tags ?tag]]
                          {:tag tag})]
    (map (fn [[id type duration]]
           {:id id :type type :duration duration})
         results)))

(defn entries-by-duration
  "Find all entries with a given duration.
   
   duration: :ephemeral :short :medium :long :permanent
   
   Returns: [{:id :type} ...]"
  [duration]
  (let [results (ds/query @graph-store
                          '[:find ?id ?type
                            :in $ ?dur
                            :where
                            [?e :memory/id ?id]
                            [?e :memory/type ?type]
                            [?e :memory/duration ?dur]]
                          {:dur (keyword duration)})]
    (map (fn [[id type]]
           {:id id :type type})
         results)))

(defn entry-recall-summary
  "Get recall summary for an entry.

   entry-id: the entry to summarize (string memory/id)

   Returns: [{:context :count} ...]"
  [entry-id]
  (let [results (ds/query @graph-store
                          '[:find ?ctx ?cnt
                            :in $ ?mem-id
                            :where
                            [?mem :memory/id ?mem-id]
                            [?r :recall/memory ?mem]
                            [?r :recall/context ?ctx]
                            [?r :recall/count ?cnt]]
                          {:mem-id entry-id})]
    (map (fn [[ctx cnt]]
           {:context ctx :count cnt})
         results)))

(defn promotion-candidates
  "Find entries that should be promoted based on recall patterns.
   
   Uses crystal/core scoring logic.
   
   Returns: [{:id :type :current-duration :next-duration :score} ...]"
  []
  (let [;; Get all non-permanent entries
        candidates (ds/query @graph-store
                             '[:find ?id ?type ?duration
                               :where
                               [?e :memory/id ?id]
                               [?e :memory/type ?type]
                               [?e :memory/duration ?duration]
                               [(not= ?duration :permanent)]])

        ;; Calculate promotion scores
        scored (->> candidates
                    (map (fn [[id type duration]]
                           (let [recalls (entry-recall-summary id)
                                 ;; Convert to format expected by crystal/core
                                 recalls-for-scoring (map (fn [{:keys [context count]}]
                                                            {:context context :count count})
                                                          recalls)
                                 {:keys [promote? current-score next-duration]}
                                 (crystal/should-promote? {:duration duration
                                                           :recalls recalls-for-scoring})]
                             (when promote?
                               {:id id
                                :type type
                                :current-duration duration
                                :next-duration next-duration
                                :score current-score}))))
                    (filter some?)
                    (sort-by :score >))]
    scored))

(defn orphaned-entries
  "Find entries with no session tag (potentially orphaned).
   
   Returns: [{:id :type :duration} ...]"
  []
  (let [all-entries (ds/query @graph-store
                              '[:find ?id ?type ?duration
                                :where
                                [?e :memory/id ?id]
                                [?e :memory/type ?type]
                                [?e :memory/duration ?duration]])
        ;; Query returns #{[id] [id2]...} - extract ids into flat set
        entries-with-sessions (->> (ds/query @graph-store
                                             '[:find ?id
                                               :where
                                               [?e :memory/id ?id]
                                               [?e :memory/session-id _]])
                                   (map first)
                                   set)

        ;; Filter out entries with sessions
        orphaned (remove (fn [[id _ _]] (contains? entries-with-sessions id)) all-entries)]
    (map (fn [[id type duration]]
           {:id id :type type :duration duration})
         orphaned)))

(defn connected-entries
  "Find entries connected to a root entry via references (both directions).

   root-id: starting entry (string memory/id)
   max-depth: maximum traversal depth (default: 3)

   Returns: [{:id :type :duration :direction :depth} ...]"
  [root-id & {:keys [max-depth] :or {max-depth 3}}]
  (let [visited (atom #{root-id})
        result (atom [])]

    ;; BFS traversal
    (loop [frontier [{:id root-id :depth 0}]]
      (when (and (seq frontier)
                 (< (:depth (first frontier)) max-depth))
        (let [{:keys [id depth]} (first frontier)

              ;; Find outgoing references (entries this one references)
              outgoing (ds/query @graph-store
                                 '[:find ?ref-id
                                   :in $ ?id
                                   :where
                                   [?e :memory/id ?id]
                                   [?e :memory/references ?ref]
                                   [?ref :memory/id ?ref-id]]
                                 {:id id})

              ;; Find incoming references (entries that reference this one)
              incoming (ds/query @graph-store
                                 '[:find ?src-id
                                   :in $ ?id
                                   :where
                                   [?target :memory/id ?id]
                                   [?src :memory/references ?target]
                                   [?src :memory/id ?src-id]]
                                 {:id id})

              ;; Process new nodes
              new-nodes (->> (concat
                              (map #(hash-map :id (first %) :depth (inc depth) :direction :outgoing) outgoing)
                              (map #(hash-map :id (first %) :depth (inc depth) :direction :incoming) incoming))
                             (remove #(contains? @visited (:id %))))]

          ;; Add to results and visited
          (doseq [node new-nodes]
            (swap! visited conj (:id node))
            (when-let [entry-info (first (ds/query @graph-store
                                                   '[:find ?type ?duration
                                                     :in $ ?id
                                                     :where
                                                     [?e :memory/id ?id]
                                                     [?e :memory/type ?type]
                                                     [?e :memory/duration ?duration]]
                                                   {:id (:id node)}))]
              (swap! result conj (merge node {:type (first entry-info) :duration (second entry-info)}))))

          ;; Continue BFS
          (recur (concat (rest frontier) new-nodes)))))

    @result))

;; =============================================================================
;; Statistics & Debugging
;; =============================================================================

(defn graph-stats
  "Get statistics about the current graph state."
  []
  {:entries (count (ds/query @graph-store '[:find ?e :where [?e :memory/id _]]))
   :sessions (count (set (ds/query @graph-store '[:find ?s :where [?e :memory/session-id ?s]])))
   :references (count (ds/query @graph-store '[:find ?a ?b :where [?e :memory/id ?a] [?e :memory/references ?b]]))
   :access-records (count (ds/query @graph-store '[:find ?e ?c :where [?r :recall/memory ?e] [?r :recall/context ?c]]))
   :last-rebuild @last-rebuild})

(defn entries-by-type-stats
  "Get count of entries by type."
  []
  (let [types [:note :snippet :convention :decision]
        counts (into {}
                     (for [t types]
                       [t (count (ds/query @graph-store
                                           '[:find ?id
                                             :in $ ?type
                                             :where
                                             [?e :memory/id ?id]
                                             [?e :memory/type ?type]]
                                           {:type t}))]))]
    counts))

(defn dump-graph
  "Dump the current graph state for debugging."
  []
  {:entries (ds/query @graph-store
                      '[:find ?id ?type ?duration
                        :where
                        [?e :memory/id ?id]
                        [?e :memory/type ?type]
                        [?e :memory/duration ?duration]])
   :sessions (ds/query @graph-store
                       '[:find ?session ?entry
                         :where
                         [?e :memory/id ?entry]
                         [?e :memory/session-id ?session]])
   :references (ds/query @graph-store
                         '[:find ?from ?to
                           :where
                           [?e :memory/id ?from]
                           [?e :memory/references ?to]])
   :accesses (take 100 (ds/query @graph-store
                                 '[:find ?entry ?context ?count
                                   :where
                                   [?r :recall/memory ?entry]
                                   [?r :recall/context ?context]
                                   [?r :recall/count ?count]]))})

(comment
  ;; Example usage

  ;; Rebuild graph from Emacs memory
  (rebuild-from-memory!)

  ;; Get stats
  (graph-stats)
  ;; => {:entries 42, :sessions 5, :references 12, :access-records 89}

  ;; Find entries from today's session
  (session-entries (crystal/session-id))

  ;; Find promotion candidates
  (promotion-candidates)
  ;; => [{:id "abc-123" :type :note :current-duration :ephemeral 
  ;;      :next-duration :short :score 5.5}]

  ;; Find entries referencing a specific entry
  (entry-lineage "some-entry-id")

  ;; Find connected subgraph
  (connected-entries "root-entry-id" :max-depth 2))
