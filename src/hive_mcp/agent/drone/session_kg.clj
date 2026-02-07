(ns hive-mcp.agent.drone.session-kg
  "Per-drone session Knowledge Graph — AGPL implementation.

   Provides:
   1. Session KG schema (Datalevin-compatible attribute definitions)
   2. DatalevinStore factory for per-drone isolated paths (/tmp/drone-{id}/kg)
   3. Lifecycle management (create, close, cleanup temp dir)
   4. AGPL implementations of session KG operations:
      - record-observation!  — compress tool result into KG observation node
      - record-reasoning!    — store LLM intent/rationale as reasoning node
      - reconstruct-context  — build compact prompt from KG state (~200-300 tokens)
      - merge-session-to-global! — merge valuable session edges to global KG
      - seed-from-global!    — seed session with relevant global context

   When hive-knowledge IS on classpath, it overrides with proprietary scoring
   (structural similarity, emergence detection). The AGPL layer provides
   functional but simpler implementations.

   CLARITY-Y: Graceful degradation — nil stores produce noops, not exceptions.
   CLARITY-T: All operations logged with drone-id for tracing."
  (:require [hive-mcp.knowledge-graph.store.datalevin :as dtlv-store]
            [hive-mcp.protocols.kg :as kg]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Dynamic Resolution Helpers (hive-knowledge override layer)
;; =============================================================================

(defn- try-resolve-hk
  "Attempt to resolve a symbol from hive-knowledge.session-kg.
   Returns the var if available, nil otherwise.
   When hive-knowledge IS on classpath, its proprietary implementations
   override the AGPL defaults below (structural similarity, scoring, etc.)."
  [sym-name]
  (try
    (requiring-resolve (symbol "hive-knowledge.session-kg" sym-name))
    (catch Exception _
      nil)))

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- truncate
  "Truncate a string to max-len characters with ellipsis."
  [s max-len]
  (if (and (string? s) (> (count s) max-len))
    (str (subs s 0 max-len) "...")
    (str s)))

(defn- extract-key-facts
  "Extract key facts from a tool result.
   AGPL implementation: simple structural extraction.
   The proprietary override uses NLP-based fact extraction and scoring.

   Returns a vector of short fact strings."
  [tool result]
  (let [success? (:success result)
        text (or (get-in result [:result :text])
                 (get-in result [:result])
                 "")]
    (cond-> []
      ;; Always record success/failure
      true (conj (if success? (str tool " succeeded") (str tool " failed")))

      ;; For read_file: extract namespace if Clojure
      (and success? (= tool "read_file") (string? text))
      (into (when-let [ns-match (re-find #"\(ns\s+([\w\.\-]+)" text)]
              [(str "namespace: " (second ns-match))]))

      ;; For grep: count matches
      (and success? (= tool "grep") (string? text))
      (conj (str "matches: " (count (str/split-lines text))))

      ;; For errors: capture error message
      (and (not success?) (:error result))
      (conj (str "error: " (truncate (:error result) 80))))))

(defn- summarize-result
  "Create a short summary of a tool result.
   AGPL implementation: simple structural summarization.

   Returns a string summary (<= 120 chars)."
  [tool result]
  (let [success? (:success result)
        text (or (get-in result [:result :text])
                 (str (get-in result [:result]))
                 "")]
    (if success?
      (str tool ": " (truncate text 100))
      (str tool " FAILED: " (truncate (or (:error result) "unknown") 80)))))

;; =============================================================================
;; Session KG Schema (AGPL — data structure definitions are open)
;; =============================================================================

(def session-schema
  "Additional schema attributes for drone session KG.
   These extend the base KG schema with observation/reasoning tracking.

   Open (AGPL): Schema definitions are structural, not algorithmic."
  {;; Observation nodes (tool results compressed into meaning)
   :obs/id          {:db/unique :db.unique/identity}
   :obs/turn        {}  ; integer turn number
   :obs/tool        {}  ; tool name string
   :obs/summary     {}  ; compressed meaning (not raw output)
   :obs/success     {}  ; boolean
   :obs/timestamp   {}  ; inst
   :obs/file        {}  ; optional file path involved
   :obs/key-facts   {:db/cardinality :db.cardinality/many}  ; extracted facts

   ;; Reasoning nodes (LLM thoughts compressed)
   :reason/id       {:db/unique :db.unique/identity}
   :reason/turn     {}  ; integer turn number
   :reason/intent   {}  ; what the LLM decided to do
   :reason/rationale {} ; why (compressed)
   :reason/timestamp {}

   ;; Goal tracking
   :goal/id         {:db/unique :db.unique/identity}
   :goal/description {}
   :goal/status     {}  ; :active :achieved :abandoned
   :goal/turn-set   {}  ; turn when goal was identified

   ;; Dependency edges between observations
   :dep/from        {}  ; obs/id or reason/id
   :dep/to          {}  ; obs/id or reason/id
   :dep/relation    {}  ; :caused-by :enables :contradicts :refines
   })

;; =============================================================================
;; Session KG Lifecycle (AGPL — factory and CRUD are open)
;; =============================================================================

(defn session-db-path
  "Compute the temp database path for a drone session.
   Returns string path like /tmp/drone-{id}/kg"
  [drone-id]
  (str "/tmp/drone-" drone-id "/kg"))

(defn create-session-kg!
  "Create an isolated Datalevin-backed session KG for a drone.

   The session schema (obs/*, reason/*, goal/*, dep/*) is merged with the
   base KG schema via :extra-schema. This ensures :db/unique constraints
   are applied for upsert behavior (e.g., same obs-id updates instead of
   creating duplicates).

   Arguments:
     drone-id - Unique drone identifier

   Returns:
     IKGStore instance (DatalevinStore) or nil on failure.

   CLARITY-Y: Falls back gracefully if Datalevin unavailable."
  [drone-id]
  (let [db-path (session-db-path drone-id)]
    (log/info "Creating session KG for drone" {:drone-id drone-id :path db-path})
    (try
      (let [store (dtlv-store/create-store {:db-path db-path
                                            :extra-schema session-schema})]
        (when store
          (kg/ensure-conn! store)
          (log/info "Session KG initialized" {:drone-id drone-id :path db-path
                                              :session-attrs (count session-schema)})
          store))
      (catch Exception e
        (log/warn "Failed to create session KG, drone will use in-memory fallback"
                  {:drone-id drone-id :error (.getMessage e)})
        nil))))

(defn close-session-kg!
  "Close a session KG store and optionally clean up temp directory.

   Arguments:
     store     - IKGStore instance to close
     drone-id  - Drone identifier (for path computation)
     cleanup?  - If true, delete the temp directory (default: true on success)"
  [store drone-id & {:keys [cleanup?] :or {cleanup? true}}]
  (when store
    (try
      (kg/close! store)
      (when cleanup?
        (let [dir (io/file (session-db-path drone-id))]
          (when (.exists dir)
            (doseq [f (reverse (file-seq dir))]
              (.delete f))
            ;; Also delete parent drone dir if empty
            (let [parent (.getParentFile dir)]
              (when (and (.exists parent) (empty? (.listFiles parent)))
                (.delete parent))))))
      (log/info "Session KG closed" {:drone-id drone-id :cleaned-up? cleanup?})
      (catch Exception e
        (log/warn "Error closing session KG" {:drone-id drone-id :error (.getMessage e)})))))

;; =============================================================================
;; Observation Recording — AGPL implementation
;; hive-knowledge overrides via try-resolve-hk when on classpath
;; =============================================================================

(defn record-observation!
  "Record a tool execution result as a compressed observation in the session KG.

   AGPL implementation: transacts observation node with summary and key facts.
   When hive-knowledge is on classpath, delegates to proprietary implementation
   (NLP-based compression, relevance scoring, dependency edge creation).

   Arguments:
     store   - IKGStore instance (session KG), nil-safe
     turn    - Integer turn number
     tool    - Tool name string
     result  - Tool execution result map {:success :result :error}
     opts    - Optional map with :file, :summary-fn

   Returns:
     The observation ID string."
  [store turn tool result & [opts]]
  (let [obs-id (str "obs-" turn "-" tool)]
    ;; Try hive-knowledge override first
    (if-let [hk-fn (try-resolve-hk "record-observation!")]
      (hk-fn store turn tool result opts)
      ;; AGPL implementation: structural observation recording
      (do
        (when store
          (try
            (let [summary (summarize-result tool result)
                  key-facts (extract-key-facts tool result)
                  obs-entity (cond-> {:obs/id obs-id
                                      :obs/turn turn
                                      :obs/tool tool
                                      :obs/summary summary
                                      :obs/success (boolean (:success result))
                                      :obs/timestamp (java.util.Date.)}
                               (seq key-facts) (assoc :obs/key-facts (set key-facts))
                               (:file opts) (assoc :obs/file (:file opts)))]
              (kg/transact! store [obs-entity])
              (log/debug "Recorded observation" {:obs-id obs-id :turn turn :tool tool}))
            (catch Exception e
              (log/warn "Failed to record observation" {:obs-id obs-id :error (.getMessage e)}))))
        obs-id))))

;; =============================================================================
;; Reasoning Recording — AGPL implementation
;; hive-knowledge overrides via try-resolve-hk when on classpath
;; =============================================================================

(defn record-reasoning!
  "Record an LLM reasoning step in the session KG.

   AGPL implementation: transacts reasoning node with intent and rationale.
   When hive-knowledge is on classpath, delegates to proprietary implementation
   (intent chain tracking, dependency graph building, compression).

   Arguments:
     store     - IKGStore instance (session KG), nil-safe
     turn      - Integer turn number
     intent    - What the LLM decided to do (string)
     rationale - Why (string)

   Returns:
     The reasoning ID string."
  [store turn intent rationale]
  (let [reason-id (str "reason-" turn)]
    ;; Try hive-knowledge override first
    (if-let [hk-fn (try-resolve-hk "record-reasoning!")]
      (hk-fn store turn intent rationale)
      ;; AGPL implementation: structural reasoning recording
      (do
        (when store
          (try
            (let [reason-entity {:reason/id reason-id
                                 :reason/turn turn
                                 :reason/intent (truncate intent 200)
                                 :reason/rationale (truncate rationale 200)
                                 :reason/timestamp (java.util.Date.)}]
              (kg/transact! store [reason-entity])
              (log/debug "Recorded reasoning" {:reason-id reason-id :turn turn}))
            (catch Exception e
              (log/warn "Failed to record reasoning" {:reason-id reason-id :error (.getMessage e)}))))
        reason-id))))

;; =============================================================================
;; Context Reconstruction — AGPL implementation
;; hive-knowledge overrides via try-resolve-hk when on classpath
;; =============================================================================

(defn- query-recent-observations
  "Query the last N observations from the session KG, ordered by turn.
   Returns vector of [obs-id turn tool summary success] tuples."
  [store max-obs]
  (try
    (let [results (kg/query store
                            '[:find ?id ?turn ?tool ?summary ?success
                              :where
                              [?e :obs/id ?id]
                              [?e :obs/turn ?turn]
                              [?e :obs/tool ?tool]
                              [?e :obs/summary ?summary]
                              [?e :obs/success ?success]])]
      (->> results
           (sort-by second >)  ; sort by turn descending
           (take max-obs)
           vec))
    (catch Exception e
      (log/debug "Failed to query observations" {:error (.getMessage e)})
      [])))

(defn- query-recent-reasoning
  "Query the last N reasoning nodes from the session KG, ordered by turn.
   Returns vector of [reason-id turn intent] tuples."
  [store max-reasons]
  (try
    (let [results (kg/query store
                            '[:find ?id ?turn ?intent
                              :where
                              [?e :reason/id ?id]
                              [?e :reason/turn ?turn]
                              [?e :reason/intent ?intent]])]
      (->> results
           (sort-by second >)
           (take max-reasons)
           vec))
    (catch Exception e
      (log/debug "Failed to query reasoning" {:error (.getMessage e)})
      [])))

(defn- query-all-key-facts
  "Query all accumulated key facts from observations.
   Returns set of fact strings."
  [store]
  (try
    (let [results (kg/query store
                            '[:find ?fact
                              :where
                              [?e :obs/key-facts ?fact]])]
      (into #{} (map first) results))
    (catch Exception e
      (log/debug "Failed to query key facts" {:error (.getMessage e)})
      #{})))

(defn- query-goals
  "Query active goals from the session KG.
   Returns vector of [goal-id description status] tuples."
  [store]
  (try
    (let [results (kg/query store
                            '[:find ?id ?desc ?status
                              :where
                              [?e :goal/id ?id]
                              [?e :goal/description ?desc]
                              [?e :goal/status ?status]])]
      (vec results))
    (catch Exception e
      (log/debug "Failed to query goals" {:error (.getMessage e)})
      [])))

(defn reconstruct-context
  "Reconstruct a compact context prompt from the session KG state.

   AGPL implementation: queries KG for observations, reasoning, key facts,
   and goals. Builds a compact prompt (~200-300 tokens) suitable for
   replacing raw message history in multi-turn drone conversations.

   When hive-knowledge is on classpath, delegates to proprietary implementation
   (relevance scoring, semantic compression, ~25x compression ratio).

   Arguments:
     store - IKGStore instance (session KG), nil-safe
     task  - Original task description
     turn  - Current turn number

   Returns:
     String - compact context for LLM prompt."
  [store task turn]
  ;; Try hive-knowledge override first
  (if-let [hk-fn (try-resolve-hk "reconstruct-context")]
    (hk-fn store task turn)
    ;; AGPL implementation: structural context reconstruction
    (if (or (nil? store) (zero? turn))
      task
      (try
        (let [;; Query session KG state
              recent-obs (query-recent-observations store 5)
              recent-reasons (query-recent-reasoning store 3)
              key-facts (query-all-key-facts store)
              goals (query-goals store)
              active-goals (filter #(= :active (nth % 2)) goals)

              ;; Build compact context
              sections (cond-> [(str "TASK: " task)]

                         ;; Key facts accumulated
                         (seq key-facts)
                         (conj (str "\nKNOWN FACTS:\n"
                                    (str/join "\n" (map #(str "- " %) (take 10 key-facts)))))

                         ;; Recent observations (most recent first)
                         (seq recent-obs)
                         (conj (str "\nRECENT OBSERVATIONS:\n"
                                    (str/join "\n"
                                              (map (fn [[_id turn _tool summary success]]
                                                     (str "T" turn ": " (if success "[OK]" "[FAIL]") " " summary))
                                                   recent-obs))))

                         ;; Recent reasoning
                         (seq recent-reasons)
                         (conj (str "\nRECENT REASONING:\n"
                                    (str/join "\n"
                                              (map (fn [[_id turn intent]]
                                                     (str "T" turn ": " intent))
                                                   recent-reasons))))

                         ;; Active goals
                         (seq active-goals)
                         (conj (str "\nACTIVE GOALS:\n"
                                    (str/join "\n"
                                              (map (fn [[_id desc _status]]
                                                     (str "- " desc))
                                                   active-goals))))

                         ;; Current turn
                         true
                         (conj (str "\nCurrent turn: " turn ". Continue working on the task.")))]
          (str/join "\n" sections))
        (catch Exception e
          (log/warn "Context reconstruction failed, falling back to raw task"
                    {:turn turn :error (.getMessage e)})
          task)))))

;; =============================================================================
;; Merge-Back (Session KG → Global KG) — AGPL implementation
;; =============================================================================

(defn- extract-session-edges
  "Extract all KG edges from a session store.
   Returns vector of edge maps without :db/id."
  [store]
  (try
    (let [edge-eids (kg/query store '[:find ?e :where [?e :kg-edge/id _]])]
      (mapv (fn [[eid]]
              (-> (kg/pull-entity store '[*] eid)
                  (dissoc :db/id)))
            edge-eids))
    (catch Exception _
      [])))

(defn merge-session-to-global!
  "Merge valuable session KG facts into the global KG.

   AGPL implementation: extracts all KG edges from the session store
   and transacts them into the global store. Uses :kg-edge/id as identity
   for upsert behavior (no duplicates).

   When hive-knowledge is on classpath, delegates to proprietary implementation
   (novelty scoring, fact filtering, confidence thresholds).

   Arguments:
     session-store - IKGStore for the drone session, nil-safe
     global-store  - IKGStore for the global/shared KG, nil-safe
     opts          - Optional {:filter-fn, :min-confidence}

   Returns:
     Map with :merged-count, :errors, or nil if stores are nil."
  [session-store global-store & [opts]]
  ;; Try hive-knowledge override first
  (if-let [f (try
               (requiring-resolve 'hive-knowledge.drone-loop/merge-session-to-global!)
               (catch Exception _ nil))]
    (f session-store global-store opts)
    ;; AGPL implementation: structural merge
    (when (and session-store global-store)
      (let [edges (extract-session-edges session-store)
            min-confidence (or (:min-confidence opts) 0.0)
            filter-fn (or (:filter-fn opts) (constantly true))
            ;; Filter edges by confidence and custom filter
            filtered (filter (fn [edge]
                               (and (>= (or (:kg-edge/confidence edge) 1.0) min-confidence)
                                    (filter-fn edge)))
                             edges)]
        (log/info "Merging session edges to global" {:total (count edges)
                                                     :after-filter (count filtered)})
        (let [result (reduce
                      (fn [acc edge]
                        (try
                          (kg/transact! global-store [edge])
                          (update acc :merged-count inc)
                          (catch Exception e
                            (update acc :errors conj {:edge-id (:kg-edge/id edge)
                                                      :error (.getMessage e)}))))
                      {:merged-count 0 :errors []}
                      filtered)]
          (log/info "Session→global merge complete" (select-keys result [:merged-count]))
          result)))))

;; =============================================================================
;; Seed Session KG from Global Context — AGPL implementation
;; =============================================================================

(defn- extract-global-edges-for-files
  "Extract edges from global store that reference the given file paths.
   Returns vector of edge maps."
  [global-store files]
  (try
    (let [;; Query edges whose :from or :to matches file-related patterns
          all-edges (kg/query global-store
                              '[:find ?e ?id ?from ?to ?rel
                                :where
                                [?e :kg-edge/id ?id]
                                [?e :kg-edge/from ?from]
                                [?e :kg-edge/to ?to]
                                [?e :kg-edge/relation ?rel]])
          file-set (set files)
          ;; Filter for edges that reference any of the target files
          relevant (filter (fn [[_e _id from to _rel]]
                             (or (contains? file-set from)
                                 (contains? file-set to)
                                 ;; Also match partial paths
                                 (some (fn [f]
                                         (or (str/includes? (str from) f)
                                             (str/includes? (str to) f)))
                                       files)))
                           all-edges)]
      (mapv (fn [[eid _id _from _to _rel]]
              (-> (kg/pull-entity global-store '[*] eid)
                  (dissoc :db/id)))
            relevant))
    (catch Exception e
      (log/debug "Failed to extract global edges for seeding" {:error (.getMessage e)})
      [])))

(defn seed-from-global!
  "Seed a session KG with relevant context from the global KG.

   AGPL implementation: queries global KG for edges referencing the
   task files and copies them into the session store as reference context.

   When hive-knowledge is on classpath, delegates to proprietary implementation
   (semantic relevance scoring, convention extraction, axiom filtering).

   Arguments:
     session-store - IKGStore for the drone session, nil-safe
     global-store  - IKGStore for the global/shared KG, nil-safe
     task-context  - Map with :task, :files, :cwd

   Returns:
     Number of reference nodes seeded."
  [session-store global-store task-context]
  ;; Try hive-knowledge override first
  (if-let [hk-fn (try-resolve-hk "seed-from-global!")]
    (hk-fn session-store global-store task-context)
    ;; AGPL implementation: structural seeding
    (if (or (nil? session-store) (nil? global-store))
      0
      (let [files (or (:files task-context) [])
            edges (when (seq files)
                    (extract-global-edges-for-files global-store files))
            seeded (when (seq edges)
                     (reduce
                      (fn [acc edge]
                        (try
                          (kg/transact! session-store [edge])
                          (inc acc)
                          (catch Exception _
                            acc)))
                      0
                      edges))
            seeded-count (or seeded 0)]
        (log/info "Seeded session KG from global" {:files (clojure.core/count files)
                                                   :edges-seeded seeded-count})
        seeded-count))))
