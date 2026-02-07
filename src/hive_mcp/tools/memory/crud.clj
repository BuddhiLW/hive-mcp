(ns hive-mcp.tools.memory.crud
  "CRUD handlers for memory operations.

   SOLID: SRP - Single responsibility for create/read/update/delete.
   CLARITY: L - Layers stay pure with clear domain separation.

   Handlers:
   - add: Create new memory entry (with optional KG edge creation)
   - query: Query entries with filtering
   - query-metadata: Query returning metadata only
   - get-full: Get full entry by ID (with optional KG edges)
   - check-duplicate: Check for existing content

   Knowledge Graph Integration:
   - add supports kg_implements, kg_supersedes, kg_depends_on, kg_refines
   - get-full returns kg_outgoing and kg_incoming edges when present"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.core :refer [mcp-json mcp-error coerce-int! coerce-vec!]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.plans :as plans]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Add Handler
;; ============================================================

;; ============================================================
;; Knowledge Graph Edge Creation
;; ============================================================

(defn- update-target-incoming!
  "Update target entry's kg-incoming field with the new edge ID.
   Appends to existing incoming edges if present."
  [target-id edge-id]
  (when-let [target-entry (chroma/get-entry-by-id target-id)]
    (let [existing-incoming (or (:kg-incoming target-entry) [])
          updated-incoming (conj existing-incoming edge-id)]
      (chroma/update-entry! target-id {:kg-incoming updated-incoming}))))

(defn- create-kg-edges!
  "Create KG edges for the given relationships.

   Arguments:
     entry-id    - The newly created memory entry ID
     kg-params   - Map with :kg_implements, :kg_supersedes, :kg_depends_on, :kg_refines
     project-id  - Project scope for edge attribution
     agent-id    - Agent creating the edges (for attribution)

   Returns:
     Vector of created edge IDs

   Side effects:
     - Creates edges in DataScript KG store
     - Updates target entries' kg-incoming field in Chroma (bidirectional lookup)"
  [entry-id {:keys [kg_implements kg_supersedes kg_depends_on kg_refines]} project-id agent-id]
  (let [created-by (when agent-id (str "agent:" agent-id))
        create-edges (fn [targets relation]
                       (when (seq targets)
                         (mapv (fn [target-id]
                                 (let [edge-id (kg-edges/add-edge!
                                                {:from entry-id
                                                 :to target-id
                                                 :relation relation
                                                 :scope project-id
                                                 :confidence 1.0
                                                 :created-by created-by})]
                                   ;; Update target's kg-incoming for bidirectional lookup
                                   (update-target-incoming! target-id edge-id)
                                   edge-id))
                               targets)))]
    (vec (concat
          (create-edges kg_implements :implements)
          (create-edges kg_supersedes :supersedes)
          (create-edges kg_depends_on :depends-on)
          (create-edges kg_refines :refines)))))

;; ============================================================
;; Abstraction Level Auto-Classification (P1.5)
;; ============================================================
;; Three-tier classification:
;;   Tier 1: Type-based default from kg-schema (16 types mapped)
;;   Tier 2: Content-keyword bumping (heuristic signals)
;;   Tier 3: L3+ similarity-based (requiring-resolve stub)

(def ^:private l4-patterns
  "Regex patterns that indicate L4 (Intent) abstraction.
   Matches axiom/decision/principle-level language."
  [#"(?i)\b(must\s+always|must\s+never|inviolable|non-negotiable)\b"
   #"(?i)^#*\s*(axiom|principle|ADR)\s*[:.]\s"
   #"(?i)\b(architectural\s+decision|design\s+rationale|strategic\s+decision)\b"
   #"(?i)\[ax\]"])  ; axiom marker convention

(def ^:private l3-patterns
  "Regex patterns that indicate L3 (Pattern) abstraction.
   Matches convention/idiom/pattern-level language."
  [#"(?i)^#*\s*(pattern|convention|idiom|recipe|guideline|workflow)\s*[:.]\s"
   #"(?i)\b(always\s+do|never\s+do|when\s+.*?,\s+(do|use)|best\s+practice)\b"
   #"(?i)\b(anti-pattern|code\s+smell|recurring\s+(pattern|structure))\b"])

(def ^:private l1-patterns
  "Regex patterns that indicate L1 (Disc/File) abstraction.
   Matches file-level, concrete code references."
  [#"(?i)\b(file|path|directory):\s*\S+"
   #"(?i)\b(git\s+commit|commit\s+hash|SHA-?256)\b"
   #"(?i)\bat\s+line\s+\d+"
   #"(?i)\bline\s+\d+[-\u2013]\d+\b"
   #"(?i)\b(kondo|lint|compilation)\s+(error|warning)\b"])

(defn- content-keyword-level
  "Analyze content text for abstraction level signals.
   Returns an integer level (1-4) if keywords suggest a level,
   or nil if no strong signal detected.

   Priority: L4 > L3 > L1 (L2 is the default, never explicitly detected).
   Only the first matching tier wins."
  [content]
  (when (and content (not (str/blank? content)))
    (let [content-str (str content)]
      (cond
        ;; Check L4 patterns first (highest priority bump)
        (some #(re-find % content-str) l4-patterns) 4

        ;; Check L3 patterns
        (some #(re-find % content-str) l3-patterns) 3

        ;; Check L1 patterns (downward bump)
        (some #(re-find % content-str) l1-patterns) 1

        ;; No strong signal - return nil (use type-based default)
        :else nil))))

(defn- tag-level-signal
  "Check tags for abstraction level signals.
   Certain tags strongly correlate with specific levels.
   Returns integer level or nil."
  [tags]
  (let [tag-set (set (or tags []))]
    (cond
      ;; Tags that indicate L4 intent
      (some tag-set ["axiom" "principle" "ADR" "strategic"]) 4

      ;; Tags that indicate L3 patterns
      (some tag-set ["convention" "pattern" "idiom" "best-practice"
                     "anti-pattern" "workflow" "recipe"]) 3

      ;; Tags that indicate L1 file-level
      (some tag-set ["file-state" "kondo" "lint" "git-state"
                     "disc" "compilation"]) 1

      :else nil)))

(defn- classify-abstraction-level
  "Auto-classify abstraction level for a memory entry.

   Three-tier classification (highest specificity wins):
     1. Type-based default from kg-schema/derive-abstraction-level
     2. Content-keyword analysis (can bump up or down)
     3. Tag-based signal (secondary content signal)
     4. L3+ similarity stub (requiring-resolve, IP boundary)

   Bumping rules:
     - Content keywords can bump UP (e.g., note with 'must always' -> L4)
     - Content keywords can bump DOWN (e.g., note with 'at line 42' -> L1)
     - Tags provide secondary signal, used when content has no signal
     - L3+ stub can override all heuristics if available

   Arguments:
     entry-type - String type (e.g., 'decision', 'note')
     content    - Entry content text
     tags       - Entry tags vector

   Returns:
     Integer abstraction level 1-4"
  [entry-type content tags]
  (let [;; Tier 1: Type-based default from comprehensive schema map
        type-level (kg-schema/derive-abstraction-level entry-type)

        ;; Tier 2: Content keyword analysis
        content-level (content-keyword-level content)

        ;; Tier 3: Tag-based signal
        tag-level (tag-level-signal tags)

        ;; Combine: content signal > tag signal > type default
        ;; Content bumps are always respected since they're most specific
        heuristic-level (or content-level tag-level type-level)

        ;; Tier 4: L3+ similarity-based classification (requiring-resolve stub)
        ;; Uses requiring-resolve so L3+ functionality is optional (IP boundary)
        l3-level (try
                   (when-let [classify-fn
                              (requiring-resolve
                               'hive-mcp.knowledge-graph.similarity/classify-abstraction-level)]
                     (classify-fn entry-type content tags heuristic-level))
                   (catch Exception _ nil))]

    ;; L3+ result overrides heuristics if available, otherwise use heuristic
    (or l3-level heuristic-level type-level)))

;; ============================================================
;; Knowledge Gap Auto-Detection (P2.8)
;; ============================================================
;; Lightweight regex/keyword analysis to identify knowledge gaps
;; in memory content. Populated automatically on memory add.
;; L3+ stub available for deeper NLP-based gap analysis.

(def ^:private max-gaps
  "Maximum number of knowledge gaps to extract per entry."
  10)

(def ^:private max-gap-length
  "Maximum character length for a single gap descriptor."
  80)

(defn- truncate-gap
  "Truncate a gap descriptor to max-gap-length, preserving word boundaries."
  [s]
  (if (<= (count s) max-gap-length)
    s
    (let [truncated (subs s 0 max-gap-length)
          last-space (.lastIndexOf truncated " ")]
      (if (pos? last-space)
        (str (subs truncated 0 last-space) "...")
        (str (subs s 0 (- max-gap-length 3)) "...")))))

(defn- extract-questions
  "Extract sentences ending with question marks from content.
   Returns vector of gap descriptors like 'question: ...'."
  [content]
  (->> (re-seq #"[^.!?\n]*\?" content)
       (map str/trim)
       (remove str/blank?)
       (remove #(< (count %) 10))  ; Skip very short questions (e.g., just "?")
       (mapv #(truncate-gap (str "question: " %)))))

(defn- extract-todo-markers
  "Extract TODO/TBD/FIXME/HACK/XXX markers and their context.
   Returns vector of gap descriptors like 'todo: ...'."
  [content]
  (->> (re-seq #"(?i)\b(TODO|TBD|FIXME|HACK|XXX)\b[:\s]*([^\n.!?]{0,60})" content)
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case marker)
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str marker-lower ": (no description)")
                    (str marker-lower ": " ctx))))))))

(def ^:private uncertainty-patterns
  "Regex patterns for uncertainty language in content."
  [#"(?i)\b(unclear|unknown|uncertain|unsure|undecided|unresolved)\b[:\s]*([^\n.!?]{0,60})"
   #"(?i)\b(not\s+sure|needs?\s+investigation|needs?\s+clarification|open\s+question)\b[:\s]*([^\n.!?]{0,60})"
   #"(?i)\b(needs?\s+to\s+be\s+determined|remains?\s+to\s+be\s+seen)\b[:\s]*([^\n.!?]{0,60})"])

(defn- extract-uncertainty
  "Extract uncertainty language from content.
   Returns vector of gap descriptors like 'unclear: ...'."
  [content]
  (->> uncertainty-patterns
       (mapcat #(re-seq % content))
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case (str/trim marker))
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str "uncertain: " marker-lower)
                    (str "uncertain: " marker-lower " " ctx))))))
       (distinct)
       (vec)))

(def ^:private missing-patterns
  "Regex patterns for missing/incomplete markers in content."
  [#"(?i)\b(missing|incomplete|placeholder|stub|not\s+implemented|needs?\s+work)\b[:\s]*([^\n.!?]{0,60})"])

(defn- extract-missing
  "Extract missing/incomplete markers from content.
   Returns vector of gap descriptors like 'missing: ...'."
  [content]
  (->> missing-patterns
       (mapcat #(re-seq % content))
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case (str/trim marker))
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str "missing: " marker-lower)
                    (str "missing: " marker-lower " " ctx))))))
       (distinct)
       (vec)))

(def ^:private assumption-patterns
  "Regex patterns for assumption markers in content."
  [#"(?i)\b(assuming|assumption)\b[:\s]*([^\n.!?]{0,60})"])

(defn- extract-assumptions
  "Extract assumption markers from content.
   Returns vector of gap descriptors like 'assumption: ...'."
  [content]
  (->> assumption-patterns
       (mapcat #(re-seq % content))
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case (str/trim marker))
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str "assumption: " marker-lower)
                    (str "assumption: " ctx))))))
       (distinct)
       (vec)))

(defn- extract-knowledge-gaps
  "Analyze content to detect knowledge gaps.

   Lightweight regex/keyword analysis (L1/L2 level) that identifies:
   1. Questions (sentences ending with ?)
   2. TODO/TBD/FIXME/HACK/XXX markers
   3. Uncertainty language (unclear, unknown, uncertain, etc.)
   4. Missing/incomplete markers (missing, stub, not implemented, etc.)
   5. Assumption markers (assuming, assumption)

   Returns vector of gap descriptor strings (max 10), each ≤80 chars.
   Returns empty vector if no gaps detected or content is nil/blank.

   L3+ stub: delegates to hive-knowledge for deeper NLP-based analysis
   when available (requiring-resolve, IP boundary)."
  [content]
  (if (or (nil? content) (str/blank? (str content)))
    []
    (let [content-str (str content)
          ;; L1/L2: Lightweight regex extraction
          questions    (extract-questions content-str)
          todos        (extract-todo-markers content-str)
          uncertainty  (extract-uncertainty content-str)
          missing      (extract-missing content-str)
          assumptions  (extract-assumptions content-str)
          ;; Combine and deduplicate
          all-gaps (->> (concat questions todos uncertainty missing assumptions)
                        (distinct)
                        (take max-gaps)
                        (vec))
          ;; L3+ stub: deeper gap analysis via requiring-resolve (IP boundary)
          l3-gaps (try
                    (when-let [analyze-fn
                               (requiring-resolve
                                'hive-mcp.knowledge-graph.similarity/extract-knowledge-gaps)]
                      (analyze-fn content-str all-gaps))
                    (catch Exception _ nil))]
      ;; L3+ result overrides L1/L2 if available
      (or l3-gaps all-gaps))))

(defn handle-add
  "Add an entry to project memory (Chroma-only storage).
   Stores full entry in Chroma with content, metadata, and embedding.

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue).

   When agent_id is provided (or CLAUDE_SWARM_SLAVE_ID env var is set),
   adds an 'agent:{id}' tag for tracking which ling created the entry.

   Knowledge Graph Integration:
   When kg_* parameters are provided, creates corresponding KG edges:
     - kg_implements: List of entry IDs this implements
     - kg_supersedes: List of entry IDs this supersedes
     - kg_depends_on: List of entry IDs this depends on
     - kg_refines: List of entry IDs this refines

   Edges are stored in DataScript with full provenance (scope, agent, timestamp).

   ELM Principle: Array parameters are coerced with helpful error messages."
  [{:keys [type content tags duration directory agent_id
           kg_implements kg_supersedes kg_depends_on kg_refines abstraction_level]}]
  (try
    (if (and abstraction_level (not (kg-schema/valid-abstraction-level? abstraction_level)))
      (mcp-error (str "Invalid abstraction_level: " abstraction_level))
      (let [;; ELM Principle: Coerce array parameters with helpful error messages
            tags-vec (coerce-vec! tags :tags [])
            kg-implements-vec (coerce-vec! kg_implements :kg_implements [])
            kg-supersedes-vec (coerce-vec! kg_supersedes :kg_supersedes [])
            kg-depends-on-vec (coerce-vec! kg_depends_on :kg_depends_on [])
            kg-refines-vec (coerce-vec! kg_refines :kg_refines [])
            directory (or directory (ctx/current-directory))
            ;; P1.5: Auto-classify abstraction level from type + content + tags
            abstraction-level (or abstraction_level
                                  (classify-abstraction-level type content tags-vec))
            ;; P2.8: Auto-detect knowledge gaps from content
            knowledge-gaps (extract-knowledge-gaps content)]
        (log/info "mcp-memory-add:" type "directory:" directory "agent_id:" agent_id)
        (with-chroma
          (let [project-id (scope/get-current-project-id directory)
                agent-id (or agent_id
                             (ctx/current-agent-id)
                             (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
                agent-tag (when agent-id (str "agent:" agent-id))
                base-tags tags-vec
                tags-with-agent (if agent-tag (conj base-tags agent-tag) base-tags)
                kg-tags (cond-> []
                          (seq kg-implements-vec) (conj "kg:has-implements")
                          (seq kg-supersedes-vec) (conj "kg:has-supersedes")
                          (seq kg-depends-on-vec) (conj "kg:has-depends-on")
                          (seq kg-refines-vec) (conj "kg:has-refines"))
                tags-with-kg (into tags-with-agent kg-tags)
                tags-with-scope (scope/inject-project-scope tags-with-kg project-id)
                content-hash (chroma/content-hash content)
                duration-str (or duration "long")
                expires (dur/calculate-expires duration-str)
                existing (chroma/find-duplicate type content-hash :project-id project-id)]
            (if existing
              (let [merged-tags (distinct (concat (:tags existing) tags-with-scope))
                    updated (chroma/update-entry! (:id existing) {:tags merged-tags})]
                (log/info "Duplicate found, merged tags:" (:id existing))
                (mcp-json (fmt/entry->json-alist updated)))
              (let [;; Route type=plan to plans collection (OpenRouter, 4096 dims)
                    ;; Regular types go to hive-mcp-memory (Ollama, 768 dims)
                    plan? (= type "plan")
                    entry-id (if plan?
                               (plans/index-plan!
                                {:type type
                                 :content content
                                 :tags tags-with-scope
                                 :content-hash content-hash
                                 :duration duration-str
                                 :expires (or expires "")
                                 :project-id project-id
                                 :abstraction-level abstraction-level
                                 :knowledge-gaps knowledge-gaps
                                 :agent-id agent-id})
                               (chroma/index-memory-entry!
                                {:type type
                                 :content content
                                 :tags tags-with-scope
                                 :content-hash content-hash
                                 :duration duration-str
                                 :expires (or expires "")
                                 :project-id project-id
                                 :abstraction-level abstraction-level
                                 :knowledge-gaps knowledge-gaps}))
                    kg-params {:kg_implements kg-implements-vec
                               :kg_supersedes kg-supersedes-vec
                               :kg_depends_on kg-depends-on-vec
                               :kg_refines kg-refines-vec}
                    edge-ids (create-kg-edges! entry-id kg-params project-id agent-id)
                    _ (when (and (seq edge-ids) (not plan?))
                        (chroma/update-entry! entry-id {:kg-outgoing edge-ids}))
                    created (if plan?
                              (plans/get-plan entry-id)
                              (chroma/get-entry-by-id entry-id))]
                (log/info "Created memory entry:" entry-id
                          (when (seq edge-ids) (str " with " (count edge-ids) " KG edges"))
                          (when (seq knowledge-gaps) (str " gaps:" (count knowledge-gaps))))
                ;; Notify Olympus Web UI of new memory entry
                ;; Uses channel/publish! so olympus.clj subscription picks it up
                (try
                  (when-let [publish-fn (requiring-resolve 'hive-mcp.channel/publish!)]
                    (publish-fn {:type :memory-added
                                 :id entry-id
                                 :memory-type type
                                 :tags tags-with-scope
                                 :project-id project-id}))
                  (catch Exception _ nil))
                (mcp-json (cond-> (fmt/entry->json-alist created)
                            (seq edge-ids) (assoc :kg_edges_created edge-ids)))))))))
    (catch clojure.lang.ExceptionInfo e
      (if (#{:coercion-error :embedding-too-long} (:type (ex-data e)))
        (mcp-error (.getMessage e))
        (throw e)))))

;; ============================================================
;; Query Handler
;; ============================================================

(defn- apply-tag-filter
  "Filter entries by required tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

(defn- apply-duration-filter
  "Filter entries by duration."
  [entries duration]
  (if duration
    (filter #(= (:duration %) duration) entries)
    entries))

(defn- record-batch-co-access!
  "Record co-access pattern for batch query results.
   Non-blocking and fail-safe (CLARITY-Y).
   Only records when 2+ entries are returned."
  [result-ids scope]
  (when (>= (count result-ids) 2)
    (future
      (try
        (kg-edges/record-co-access!
         result-ids
         {:scope scope :created-by "system:batch-recall"})
        (catch Exception e
          (log/debug "Co-access recording failed (non-fatal):" (.getMessage e)))))))

(defn- apply-auto-scope-filter
  "Filter entries for auto-scope mode (nil scope).
   Uses hierarchical scope resolution - includes entries from:
   - Current project scope
   - All ancestor scopes (via :parent-id chain in .hive-project.edn)

   SCOPE LEAK FIX: Global-scoped entries are NO LONGER included when in
   project context. Previously scope:global was always in the match set,
   causing global memories to leak into project queries.
   Global entries are only included when project-id IS 'global'.

   HCR Wave 1: Now walks the parent-id hierarchy for full ancestry visibility.
   HCR Wave 4: When include-descendants? is true, also includes entries from
   child projects (downward hierarchy traversal via full-hierarchy-scope-tags)."
  ([entries project-id]
   (apply-auto-scope-filter entries project-id false))
  ([entries project-id include-descendants?]
   (let [in-project? (and project-id (not= project-id "global"))
         ;; HCR Wave 4: Get scope tags based on direction
         ;; FIX: Exclude scope:global when in project context
         scope-tags (cond-> (if include-descendants?
                              ;; Full bidirectional: ancestors + self + descendants
                              (kg-scope/full-hierarchy-scope-tags project-id)
                              ;; Default: ancestors + self (upward only)
                              (kg-scope/visible-scope-tags project-id))
                      in-project? (disj "scope:global"))
         ;; Also include project-id metadata matches for backward compat
         ;; FIX: Exclude "global" from visible IDs when in project context
         visible-ids (cond-> (set (if include-descendants?
                                    (into (vec (kg-scope/visible-scopes project-id))
                                          (kg-scope/descendant-scopes project-id))
                                    (kg-scope/visible-scopes project-id)))
                       in-project? (disj "global"))]
     (filter (fn [entry]
               (let [tags (set (or (:tags entry) []))]
                 (or
                   ;; Match any visible scope tag
                  (some tags scope-tags)
                   ;; Also match entries with matching project-id metadata
                  (contains? visible-ids (:project-id entry)))))
             entries))))

(defn handle-query
  "Query project memory by type with scope filtering (Chroma-only).
   SCOPE controls which memories are returned:
     - nil/omitted: auto-filter by current project + global
     - \"all\": return all entries regardless of scope
     - \"global\": return only scope:global entries
     - specific scope tag: filter by that scope

   VERBOSITY controls output detail level:
     - \"full\" (default): return complete entries via entry->json-alist
     - \"metadata\": return only id, type, preview, tags, created (~10x fewer tokens).
       Follow up with get-full to fetch specific entries.

   HCR Wave 4: include_descendants parameter.
   When true, auto-scope mode also includes entries from child projects
   (downward hierarchy traversal). Useful for coordinators that need
   visibility into sub-project memories. Default: false (backward compat).

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue).

   Co-access tracking: When multiple entries are returned, records co-access
   edges between them in the Knowledge Graph (async, non-blocking).

   SCOPE ISOLATION: Global-scoped memories are only returned when in global
   context or when scope is explicitly set to 'global'. Project queries
   only see project + ancestor + (optionally) descendant entries."
  [{:keys [type tags limit duration scope directory include_descendants verbosity]}]
  (let [directory (or directory (ctx/current-directory))
        include-descendants? (boolean include_descendants)
        metadata-only? (= verbosity "metadata")]
    (log/info "mcp-memory-query:" type "scope:" scope "directory:" directory
              "include_descendants:" include-descendants? "verbosity:" (or verbosity "full"))
    (try
      (let [limit-val (coerce-int! limit :limit 20)]
        (with-chroma
          (let [project-id (scope/get-current-project-id directory)
                in-project? (and project-id (not= project-id "global"))
                ;; Compute visible project-ids for DB-level $in filtering
                ;; Pushes scope filtering to Chroma, replacing over-fetch-all strategy
                project-ids-for-db
                (cond
                  ;; Auto mode: visible scopes for current project
                  (nil? scope)
                  (let [visible (kg-scope/visible-scopes project-id)
                        ;; Exclude "global" when in project context (scope leak fix)
                        filtered (if in-project?
                                   (vec (remove #(= "global" %) visible))
                                   visible)
                        ;; Include descendants if requested (HCR Wave 4)
                        descendants (when include-descendants?
                                      (kg-scope/descendant-scopes project-id))]
                    (vec (distinct (concat filtered descendants))))

                  ;; "all" mode: no DB-level filter
                  (= scope "all")
                  nil

                  ;; "global" mode: only global entries
                  (= scope "global")
                  ["global"]

                  ;; Specific scope: visible scopes from that scope
                  :else
                  (let [visible (kg-scope/visible-scopes scope)
                        descendants (when include-descendants?
                                      (kg-scope/descendant-scopes scope))]
                    (vec (distinct (concat visible descendants)))))
                ;; Route type=plan to plans collection
                plan? (= type "plan")
                ;; Reduced over-fetch: DB-level $in filtering means less waste
                over-fetch-factor (if include-descendants? 4 3)
                entries (if plan?
                          (plans/query-plans :project-id (first project-ids-for-db)
                                             :limit (* limit-val over-fetch-factor)
                                             :tags tags)
                          (chroma/query-entries :type type
                                                :project-ids project-ids-for-db
                                                :limit (* limit-val over-fetch-factor)))
                ;; Safety net: apply in-memory scope filter (belt-and-suspenders)
                scope-filtered (cond
                                 ;; Auto mode (nil scope): include project + ancestors
                                 ;; HCR Wave 4: pass include-descendants? flag
                                 (nil? scope)
                                 (apply-auto-scope-filter entries project-id include-descendants?)

                                 ;; "all" mode: no filtering
                                 (= scope "all")
                                 entries

                                 ;; Specific scope: use hierarchical filter
                                 ;; HCR Wave 4: when include_descendants, use full hierarchy
                                 :else
                                 (let [scope-filter (if include-descendants?
                                                      (kg-scope/full-hierarchy-scope-tags scope)
                                                      (scope/derive-hierarchy-scope-filter scope))]
                                   (if scope-filter
                                     (filter #(scope/matches-hierarchy-scopes? % scope-filter) entries)
                                     entries)))
                ;; Apply tag filter
                tag-filtered (apply-tag-filter scope-filtered tags)
                ;; Apply duration filter
                dur-filtered (apply-duration-filter tag-filtered duration)
                ;; Apply limit
                results (take limit-val dur-filtered)]
            ;; Record co-access pattern asynchronously (CLARITY-Y: non-blocking)
            (record-batch-co-access! (mapv :id results) project-id)
            ;; Format based on verbosity
            (if metadata-only?
              (mcp-json (mapv fmt/entry->metadata results))
              (mcp-json (mapv fmt/entry->json-alist results))))))
      (catch clojure.lang.ExceptionInfo e
        (if (= :coercion-error (:type (ex-data e)))
          (mcp-error (.getMessage e))
          (throw e))))))

;; ============================================================
;; Query Metadata Handler
;; ============================================================

(defn handle-query-metadata
  "Backward-compatible alias: delegates to handle-query with verbosity=metadata.
   Returns id, type, preview, tags, created - ~10x fewer tokens than full query.
   Follow up with mcp_memory_get_full to fetch specific entries.

   DEPRECATED: Use query command with verbosity='metadata' instead."
  [{:keys [type tags limit scope directory] :as params}]
  (handle-query (assoc params :verbosity "metadata")))

;; ============================================================
;; Get Full Handler (with KG Edge Inclusion)
;; ============================================================

(defn- edge->json-map
  "Convert KG edge to JSON-safe map format."
  [edge]
  (cond-> {:id (:kg-edge/id edge)
           :from (:kg-edge/from edge)
           :to (:kg-edge/to edge)
           :relation (name (:kg-edge/relation edge))
           :confidence (:kg-edge/confidence edge)
           :scope (:kg-edge/scope edge)
           :created_by (:kg-edge/created-by edge)
           :created_at (str (:kg-edge/created-at edge))}
    (:kg-edge/last-verified edge) (assoc :last_verified (str (:kg-edge/last-verified edge)))
    (:kg-edge/source-type edge) (assoc :source_type (name (:kg-edge/source-type edge)))))

(defn- get-kg-edges-for-entry
  "Get KG edges for a memory entry.
   Returns map with :outgoing and :incoming edge lists."
  [entry-id]
  (let [outgoing (kg-edges/get-edges-from entry-id)
        incoming (kg-edges/get-edges-to entry-id)]
    {:outgoing (mapv edge->json-map outgoing)
     :incoming (mapv edge->json-map incoming)}))

(defn handle-get-full
  "Get full content of a memory entry by ID.
   Searches both hive-mcp-memory and hive-mcp-plans collections.
   Use after mcp_memory_query_metadata to fetch specific entries.

   Transparent fallback: If not found in memory collection, tries plans collection.
   This allows get-full to work regardless of which collection stores the entry.

   Knowledge Graph Integration:
   Automatically includes KG edges when present:
     - kg_outgoing: Edges where this entry is the source
     - kg_incoming: Edges where this entry is the target"
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (with-chroma
    ;; Try memory collection first, then fall back to plans collection
    (if-let [entry (or (chroma/get-entry-by-id id)
                       (plans/get-plan id))]
      (let [base-result (fmt/entry->json-alist entry)
            ;; Include KG edges (graceful degradation if KG backend is broken)
            {:keys [outgoing incoming]}
            (try (get-kg-edges-for-entry id)
                 (catch Exception e
                   (log/warn "KG edge lookup failed for" id ":" (.getMessage e))
                   {:outgoing [] :incoming []}))
            result (cond-> base-result
                     (seq outgoing) (assoc :kg_outgoing outgoing)
                     (seq incoming) (assoc :kg_incoming incoming))]
        (mcp-json result))
      (mcp-json {:error "Entry not found" :id id}))))

(defn handle-batch-get
  "Get full content of multiple memory entries by IDs in a single call.
   Returns all found entries with KG edges. Missing IDs reported in :missing."
  [{:keys [ids]}]
  (if (or (nil? ids) (empty? ids))
    (mcp-error "ids is required (array of memory entry ID strings)")
    (with-chroma
      (let [results (mapv (fn [id]
                            (if-let [entry (or (chroma/get-entry-by-id id)
                                               (plans/get-plan id))]
                              (let [base (fmt/entry->json-alist entry)
                                    {:keys [outgoing incoming]}
                                    (try (get-kg-edges-for-entry id)
                                         (catch Exception e
                                           (log/warn "KG edge lookup failed for" id ":" (.getMessage e))
                                           {:outgoing [] :incoming []}))]
                                (cond-> base
                                  (seq outgoing) (assoc :kg_outgoing outgoing)
                                  (seq incoming) (assoc :kg_incoming incoming)))
                              {:error "Entry not found" :id id}))
                          ids)
            found   (filterv #(not (:error %)) results)
            missing (filterv :error results)]
        (mcp-json (cond-> {:entries found :count (count found)}
                    (seq missing) (assoc :missing (mapv :id missing))))))))

;; ============================================================
;; Check Duplicate Handler
;; ============================================================

(defn handle-check-duplicate
  "Check if content already exists in memory (Chroma-only).

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer."
  [{:keys [type content directory]}]
  (log/info "mcp-memory-check-duplicate:" type "directory:" directory)
  (with-chroma
    (let [project-id (scope/get-current-project-id directory)
          hash (chroma/content-hash content)
          existing (chroma/find-duplicate type hash :project-id project-id)]
      (mcp-json {:exists (some? existing)
                 :entry (when existing (fmt/entry->json-alist existing))
                 :content_hash hash}))))

;; ============================================================
;; Update Tags Handler
;; ============================================================

(defn handle-update-tags
  "Update tags on an existing memory entry (Chroma-only).
   Replaces existing tags with the new tags list.
   Returns the updated entry or error if not found."
  [{:keys [id tags]}]
  (log/info "mcp-memory-update-tags:" id "tags:" tags)
  (with-chroma
    (if-let [_existing (chroma/get-entry-by-id id)]
      (let [updated (chroma/update-entry! id {:tags (or tags [])})]
        (log/info "Updated tags for entry:" id)
        (mcp-json (fmt/entry->json-alist updated)))
      (mcp-json {:error "Entry not found" :id id}))))
