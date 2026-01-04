(ns emacs-mcp.crystal.graph
  "Knowledge graph for memory entries using core.logic pldb.
   
   Builds a queryable graph from memory entries, enabling:
   - Session lineage tracking (which session produced which entries)
   - Cross-reference discovery (entry-to-entry links)
   - Recall pattern analysis (access frequency by context)
   - Promotion candidate identification
   
   Reconstructed on startup from Emacs memory store.
   
   SOLID: Single responsibility - graph queries only.
   DDD: Persistence adapter for memory domain."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [emacs-mcp.emacsclient :as ec]
            [emacs-mcp.crystal.core :as crystal]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Database Relations (pldb/db-rel)
;; =============================================================================

;; Memory entry: basic entry facts
;; id: unique identifier (UUID string)
;; type: :note :snippet :convention :decision
;; duration: :ephemeral :short :medium :long :permanent
(pldb/db-rel memory-entry ^:index id type duration)

;; Session production: which session created which entry
;; session-id: date string (e.g., "2026-01-04") or full session tag
;; entry-id: the memory entry id
(pldb/db-rel session-produced ^:index session-id ^:index entry-id)

;; Entry references: cross-references between entries
;; entry-id: the entry that references another
;; referenced-id: the entry being referenced
(pldb/db-rel entry-references ^:index entry-id ^:index referenced-id)

;; Entry accessed: recall tracking
;; entry-id: the entry that was accessed
;; context: :catchup-structural :wrap-structural :explicit-reference :cross-session :cross-project :user-feedback
;; count: number of times accessed in this context
(pldb/db-rel entry-accessed ^:index entry-id context count)

;; Entry tags: for tag-based queries
;; entry-id: the entry
;; tag: a tag string
(pldb/db-rel entry-tag ^:index entry-id ^:index tag)

;; =============================================================================
;; Database State (Thread-Safe Atom)
;; =============================================================================

(defonce ^:private graph-db
  (atom (pldb/db)))

(defonce ^:private last-rebuild
  (atom nil))

(defn reset-db!
  "Reset the graph database to empty state."
  []
  (reset! graph-db (pldb/db))
  (reset! last-rebuild nil)
  (log/debug "Graph database reset"))

(defn get-db
  "Get current database (for debugging/testing)."
  []
  @graph-db)

(defmacro with-db
  "Execute a logic query against the current graph database."
  [& body]
  `(pldb/with-db @graph-db ~@body))

;; =============================================================================
;; Database Mutation Functions
;; =============================================================================

(defn add-entry-fact!
  "Add a memory entry to the graph database.
   
   entry: {:id :type :duration :tags :recalls :created}
   
   Extracts session from tags and populates all relations."
  [{:keys [id type duration tags recalls] :as entry}]
  (when id
    ;; Add base entry fact
    (swap! graph-db pldb/db-fact memory-entry id (keyword type) (keyword duration))

    ;; Extract and add session production
    (when-let [session (crystal/extract-session-from-tags tags)]
      (swap! graph-db pldb/db-fact session-produced session id))

    ;; Add tags
    (doseq [tag tags]
      (swap! graph-db pldb/db-fact entry-tag id tag))

    ;; Add recall facts
    (doseq [{:keys [context count] :or {count 1}} recalls]
      (swap! graph-db pldb/db-fact entry-accessed id (keyword context) count))

    (log/trace "Added entry to graph:" id type duration)))

(defn add-reference!
  "Add a cross-reference between entries."
  [from-entry-id to-entry-id]
  (swap! graph-db pldb/db-fact entry-references from-entry-id to-entry-id)
  (log/trace "Added reference:" from-entry-id "->" to-entry-id))

(defn remove-entry!
  "Remove an entry and all its relations from the graph."
  [entry-id]
  ;; Get current entry info
  (let [entry-info (first (with-db
                            (l/run 1 [q]
                                   (l/fresh [t d]
                                            (memory-entry entry-id t d)
                                            (l/== q {:type t :duration d})))))
        sessions (with-db (l/run* [s] (session-produced s entry-id)))
        tags (with-db (l/run* [t] (entry-tag entry-id t)))
        accesses (with-db (l/run* [q]
                                  (l/fresh [ctx cnt]
                                           (entry-accessed entry-id ctx cnt)
                                           (l/== q {:context ctx :count cnt}))))
        refs-from (with-db (l/run* [r] (entry-references entry-id r)))
        refs-to (with-db (l/run* [r] (entry-references r entry-id)))]

    ;; Retract all facts
    (when entry-info
      (swap! graph-db pldb/db-retraction memory-entry entry-id
             (:type entry-info) (:duration entry-info)))
    (doseq [s sessions]
      (swap! graph-db pldb/db-retraction session-produced s entry-id))
    (doseq [t tags]
      (swap! graph-db pldb/db-retraction entry-tag entry-id t))
    (doseq [{:keys [context count]} accesses]
      (swap! graph-db pldb/db-retraction entry-accessed entry-id context count))
    (doseq [r refs-from]
      (swap! graph-db pldb/db-retraction entry-references entry-id r))
    (doseq [r refs-to]
      (swap! graph-db pldb/db-retraction entry-references r entry-id))

    (log/debug "Removed entry from graph:" entry-id)))

(defn update-entry-duration!
  "Update an entry's duration (e.g., after promotion)."
  [entry-id new-duration]
  (let [current (first (with-db
                         (l/run 1 [q]
                                (l/fresh [t d]
                                         (memory-entry entry-id t d)
                                         (l/== q {:type t :duration d})))))]
    (when current
      (swap! graph-db pldb/db-retraction memory-entry entry-id
             (:type current) (:duration current))
      (swap! graph-db pldb/db-fact memory-entry entry-id
             (:type current) (keyword new-duration))
      (log/debug "Updated entry duration:" entry-id new-duration))))

(defn log-access!
  "Log an access to an entry with context."
  [entry-id context]
  ;; Find existing count for this context
  (let [existing (first (with-db
                          (l/run 1 [c]
                                 (entry-accessed entry-id (keyword context) c))))]
    (when existing
      (swap! graph-db pldb/db-retraction entry-accessed entry-id (keyword context) existing))
    (swap! graph-db pldb/db-fact entry-accessed entry-id (keyword context)
           (inc (or existing 0)))
    (log/trace "Logged access:" entry-id context)))

;; =============================================================================
;; Memory Integration - Rebuild from Emacs
;; =============================================================================

(defn- query-emacs-memory
  "Query Emacs memory for entries of a given type.
   Returns parsed JSON or nil on error."
  [entry-type]
  (let [elisp (format "(json-encode (emacs-mcp-api-memory-query %s nil 1000 \"all\"))"
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
;; Core Logic Predicates
;; =============================================================================

(defn entry-from-sessiono
  "Goal: succeeds if entry-id was produced in session-id."
  [session-id entry-id]
  (session-produced session-id entry-id))

(defn entry-has-tago
  "Goal: succeeds if entry-id has the given tag."
  [entry-id tag]
  (entry-tag entry-id tag))

(defn references-transitively
  "Goal: succeeds if source reaches target via references (transitive closure)."
  [source target]
  (l/conde
   ;; Direct reference
   [(entry-references source target)]
   ;; Transitive reference
   [(l/fresh [mid]
             (entry-references source mid)
             (references-transitively mid target))]))

(defn high-recall-entryo
  "Goal: succeeds if entry has meaningful recalls (not just structural)."
  [entry-id]
  (l/fresh [ctx cnt]
           (entry-accessed entry-id ctx cnt)
           (l/!= ctx :catchup-structural)
           (l/!= ctx :wrap-structural)
           (l/project [cnt]
                      (l/== true (> cnt 0)))))

;; =============================================================================
;; Query Functions (Public API)
;; =============================================================================

(defn session-entries
  "Get all entries produced in a session.
   
   session-id: date string or session tag
   
   Returns: [{:id :type :duration} ...]"
  [session-id]
  (with-db
    (l/run* [q]
            (l/fresh [id type duration]
                     (session-produced session-id id)
                     (memory-entry id type duration)
                     (l/== q {:id id :type type :duration duration})))))

(defn entry-lineage
  "Find all entries that reference a given entry (reverse lookup).
   
   entry-id: the entry to find references to
   
   Returns: [{:id :type :duration} ...] of entries that reference this one"
  [entry-id]
  (with-db
    (l/run* [q]
            (l/fresh [ref-id type duration]
                     (entry-references ref-id entry-id)
                     (memory-entry ref-id type duration)
                     (l/== q {:id ref-id :type type :duration duration})))))

(defn entry-references-to
  "Find all entries that a given entry references (forward lookup).
   
   entry-id: the entry to find outgoing references from
   
   Returns: [{:id :type :duration} ...]"
  [entry-id]
  (with-db
    (l/run* [q]
            (l/fresh [ref-id type duration]
                     (entry-references entry-id ref-id)
                     (memory-entry ref-id type duration)
                     (l/== q {:id ref-id :type type :duration duration})))))

(defn entries-by-tag
  "Find all entries with a given tag.
   
   tag: the tag to search for
   
   Returns: [{:id :type :duration} ...]"
  [tag]
  (with-db
    (l/run* [q]
            (l/fresh [id type duration]
                     (entry-tag id tag)
                     (memory-entry id type duration)
                     (l/== q {:id id :type type :duration duration})))))

(defn entries-by-duration
  "Find all entries with a given duration.
   
   duration: :ephemeral :short :medium :long :permanent
   
   Returns: [{:id :type} ...]"
  [duration]
  (with-db
    (l/run* [q]
            (l/fresh [id type]
                     (memory-entry id type (keyword duration))
                     (l/== q {:id id :type type})))))

(defn entry-recall-summary
  "Get recall summary for an entry.
   
   entry-id: the entry to summarize
   
   Returns: [{:context :count} ...]"
  [entry-id]
  (with-db
    (l/run* [q]
            (l/fresh [ctx cnt]
                     (entry-accessed entry-id ctx cnt)
                     (l/== q {:context ctx :count cnt})))))

(defn promotion-candidates
  "Find entries that should be promoted based on recall patterns.
   
   Uses crystal/core scoring logic.
   
   Returns: [{:id :type :current-duration :next-duration :score} ...]"
  []
  (let [;; Get all non-permanent entries
        candidates (with-db
                     (l/run* [q]
                             (l/fresh [id type duration]
                                      (memory-entry id type duration)
                                      (l/!= duration :permanent)
                                      (l/== q {:id id :type type :duration duration}))))]
    ;; Calculate promotion scores
    (->> candidates
         (map (fn [{:keys [id type duration]}]
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
         (sort-by :score >))))

(defn orphaned-entries
  "Find entries with no session tag (potentially orphaned).
   
   Returns: [{:id :type :duration} ...]"
  []
  (let [all-entries (with-db
                      (l/run* [q]
                              (l/fresh [id type duration]
                                       (memory-entry id type duration)
                                       (l/== q {:id id :type type :duration duration}))))
        entries-with-sessions (set (with-db (l/run* [id]
                                                    (l/fresh [session]
                                                             (session-produced session id)))))]
    (remove #(contains? entries-with-sessions (:id %)) all-entries)))

(defn connected-entries
  "Find entries connected to a root entry via references (both directions).
   
   root-id: starting entry
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
              ;; Find outgoing references
              outgoing (with-db
                         (l/run* [ref]
                                 (entry-references id ref)))
              ;; Find incoming references
              incoming (with-db
                         (l/run* [ref]
                                 (entry-references ref id)))
              ;; Process new nodes
              new-nodes (->> (concat
                              (map #(hash-map :id % :depth (inc depth) :direction :outgoing) outgoing)
                              (map #(hash-map :id % :depth (inc depth) :direction :incoming) incoming))
                             (remove #(contains? @visited (:id %))))]
          ;; Add to results and visited
          (doseq [node new-nodes]
            (swap! visited conj (:id node))
            (when-let [entry-info (first (with-db
                                           (l/run 1 [q]
                                                  (l/fresh [t d]
                                                           (memory-entry (:id node) t d)
                                                           (l/== q {:type t :duration d})))))]
              (swap! result conj (merge node entry-info))))
          ;; Continue BFS
          (recur (concat (rest frontier) new-nodes)))))
    @result))

;; =============================================================================
;; Statistics & Debugging
;; =============================================================================

(defn graph-stats
  "Get statistics about the current graph state."
  []
  {:entries (count (with-db (l/run* [id]
                                    (l/fresh [t d]
                                             (memory-entry id t d)))))
   :sessions (count (set (with-db (l/run* [s]
                                          (l/fresh [e]
                                                   (session-produced s e))))))
   :references (count (with-db (l/run* [q]
                                       (l/fresh [a b]
                                                (entry-references a b)
                                                (l/== q [a b])))))
   :access-records (count (with-db (l/run* [q]
                                           (l/fresh [e c n]
                                                    (entry-accessed e c n)
                                                    (l/== q [e c])))))
   :last-rebuild @last-rebuild})

(defn entries-by-type-stats
  "Get count of entries by type."
  []
  (let [types [:note :snippet :convention :decision]]
    (into {}
          (for [t types]
            [t (count (with-db
                        (l/run* [id]
                                (l/fresh [d]
                                         (memory-entry id t d)))))]))))

(defn dump-graph
  "Dump the current graph state for debugging."
  []
  {:entries (with-db
              (l/run* [q]
                      (l/fresh [id type duration]
                               (memory-entry id type duration)
                               (l/== q {:id id :type type :duration duration}))))
   :sessions (with-db
               (l/run* [q]
                       (l/fresh [s e]
                                (session-produced s e)
                                (l/== q {:session s :entry e}))))
   :references (with-db
                 (l/run* [q]
                         (l/fresh [from to]
                                  (entry-references from to)
                                  (l/== q {:from from :to to}))))
   :accesses (take 100 (with-db
                         (l/run* [q]
                                 (l/fresh [e c n]
                                          (entry-accessed e c n)
                                          (l/== q {:entry e :context c :count n})))))})

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
