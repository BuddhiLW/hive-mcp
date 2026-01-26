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

;; =============================================================================
;; Disc Entity Schema (File State Tracking)
;; =============================================================================
;;
;; Disc entities track the actual state of files on disk, enabling grounding
;; verification without re-reading files. When a memory entry is grounded,
;; it references a disc entity as proof of verification.

(def disc-schema
  "DataScript schema for disc (file) state tracking.

   Disc entities represent the L1 abstraction level - actual files on disk.
   Used as grounding targets for higher-level knowledge entries."
  {:disc/path         {:db/unique :db.unique/identity
                       :db/doc "File path (unique identity for the disc entity)"}
   :disc/content-hash {:db/doc "SHA256 hash of file content"}
   :disc/analyzed-at  {:db/doc "Timestamp of last kondo/analysis (inst)"}
   :disc/git-commit   {:db/doc "Git commit hash when analyzed"}
   :disc/project-id   {:db/doc "Project scope (for multi-project support)"}})

;; =============================================================================
;; Abstraction Level Helpers
;; =============================================================================

(def type->abstraction-level
  "Maps memory entry types to their default abstraction levels.

   L1 (Disc):     Not stored as memory - use disc entities
   L2 (Semantic): What things DO - snippets, notes, function docs
   L3 (Pattern):  Recurring structures - conventions, idioms
   L4 (Intent):   Why - ADRs, decisions, axioms"
  {"snippet"    2  ; Code snippets describe what code does
   "note"       2  ; Notes describe semantic understanding
   "convention" 3  ; Conventions are patterns
   "decision"   4  ; Decisions are intent-level
   "axiom"      4  ; Axioms are foundational intent
   "pattern"    3  ; Explicit patterns
   "doc"        2  ; Documentation is semantic
   "todo"       2  ; TODOs are semantic notes
   "question"   2  ; Questions are semantic
   "answer"     2  ; Answers are semantic
   "warning"    2  ; Warnings are semantic
   "error"      2  ; Errors are semantic
   "lesson"     3  ; Lessons are pattern-level
   "principle"  4  ; Principles are intent-level
   "rule"       3  ; Rules are pattern-level
   "guideline"  3  ; Guidelines are pattern-level
   "workflow"   3  ; Workflows are patterns
   "recipe"     3  ; Recipes are patterns
   })

(defn derive-abstraction-level
  "Derive the default abstraction level for a memory entry type.
   Returns integer 2-4, defaulting to 2 (Semantic) for unknown types.

   Arguments:
     entry-type - String type of the memory entry (e.g., \"decision\", \"snippet\")

   Returns:
     Integer abstraction level (2-4)"
  [entry-type]
  (get type->abstraction-level entry-type 2))

(defn full-schema
  "Returns the combined KG schema (edges + knowledge abstraction + disc)."
  []
  (merge kg-schema knowledge-schema disc-schema))
