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

(defn valid-relation?
  "Check if a relation type is valid."
  [relation]
  (contains? relation-types relation))

(defn valid-confidence?
  "Check if confidence score is in valid range [0.0, 1.0]."
  [confidence]
  (and (number? confidence)
       (<= 0.0 confidence 1.0)))
