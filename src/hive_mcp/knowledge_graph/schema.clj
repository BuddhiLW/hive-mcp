(ns hive-mcp.knowledge-graph.schema
  "Knowledge Graph schema for DataScript edge storage.

   Defines the schema for knowledge edges that connect memory entries,
   enabling graph traversal, impact analysis, and knowledge promotion.")

;; Supported relation types for edges between knowledge nodes
(def relation-types
  "Valid relation types for knowledge graph edges.

   - :implements   - Realizes a principle/pattern
   - :supersedes   - Replaces previous knowledge
   - :refines      - Improves without replacing
   - :contradicts  - Conflicts with
   - :depends-on   - Requires for correctness
   - :derived-from - Synthesized from sources
   - :applies-to   - Scope applicability"
  #{:implements :supersedes :refines :contradicts
    :depends-on :derived-from :applies-to})

(def kg-schema
  "DataScript schema for Knowledge Graph edges.

   Bounded context pattern: separate from Chroma memory storage.
   Edges connect memory entry IDs without duplicating content."
  {:kg-edge/id         {:db/unique :db.unique/identity
                        :db/doc "Unique edge identifier (UUID string)"}
   :kg-edge/from       {:db/doc "Source node ID (memory entry ID)"}
   :kg-edge/to         {:db/doc "Target node ID (memory entry ID)"}
   :kg-edge/relation   {:db/doc "Relation type keyword from relation-types"}
   :kg-edge/scope      {:db/doc "Scope where edge was discovered (e.g., project-id)"}
   :kg-edge/confidence {:db/doc "Confidence score 0.0-1.0"}
   :kg-edge/created-by {:db/doc "Agent ID that created this edge"}
   :kg-edge/created-at {:db/doc "Creation timestamp (inst)"}})

;; =============================================================================
;; Abstraction Level Tracking (per Korzybski's Structural Differential)
;; =============================================================================
;;
;; Abstraction Levels:
;;   L0: Parabola (Runtime) - Not stored, inferred from live system
;;   L1: Disc (Files)       - kondo analysis, git state, actual code
;;   L2: Semantic           - What functions DO, behavior descriptions
;;   L3: Pattern            - Conventions, idioms, recurring structures
;;   L4: Intent             - ADRs, decisions, axioms, design rationale
;;
;; Knowledge degrades as it rises through abstraction levels. These fields
;; track the abstraction level and grounding status of knowledge entries.

(def abstraction-levels
  "Valid abstraction levels for knowledge entries.
   L0 (runtime) is not stored - it's inferred from live system state."
  {:L1 {:level 1 :name "Disc"     :description "Files, kondo analysis, git state"}
   :L2 {:level 2 :name "Semantic" :description "What functions DO"}
   :L3 {:level 3 :name "Pattern"  :description "Conventions, idioms"}
   :L4 {:level 4 :name "Intent"   :description "ADRs, decisions, axioms"}})

(def knowledge-schema
  "DataScript schema for knowledge abstraction tracking.

   Tracks the abstraction level and grounding status of knowledge entries,
   enabling drift detection and re-grounding workflows."
  {:knowledge/abstraction-level {:db/doc "Abstraction level 1-4 (L1=Disc, L2=Semantic, L3=Pattern, L4=Intent)"}
   :knowledge/grounded-at       {:db/doc "Timestamp of last verification against lower level (inst)"}
   :knowledge/grounded-from     {:db/doc "Ref to disc entity (file/commit) verified against"}
   :knowledge/gaps              {:db/cardinality :db.cardinality/many
                                 :db/doc "Set of known abstraction gaps (keywords)"}
   :knowledge/source-hash       {:db/doc "Content hash of source when abstracted (for drift detection)"}})

(defn valid-relation?
  "Check if a relation type is valid."
  [relation]
  (contains? relation-types relation))

(defn valid-confidence?
  "Check if confidence score is in valid range [0.0, 1.0]."
  [confidence]
  (and (number? confidence)
       (<= 0.0 confidence 1.0)))

(defn valid-abstraction-level?
  "Check if abstraction level is valid (1-4).
   L0 (runtime) is not stored, so 0 is not valid for persistence."
  [level]
  (and (integer? level)
       (<= 1 level 4)))

(defn abstraction-level-keyword
  "Convert integer level to keyword (:L1, :L2, :L3, :L4)."
  [level]
  (when (valid-abstraction-level? level)
    (keyword (str "L" level))))

(defn abstraction-level-info
  "Get full info for an abstraction level.
   Returns {:level n :name \"Name\" :description \"...\"} or nil."
  [level]
  (when-let [kw (abstraction-level-keyword level)]
    (get abstraction-levels kw)))

(defn full-schema
  "Returns the combined KG schema (edges + knowledge abstraction)."
  []
  (merge kg-schema knowledge-schema))
