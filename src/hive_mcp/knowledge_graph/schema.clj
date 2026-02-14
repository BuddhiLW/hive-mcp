(ns hive-mcp.knowledge-graph.schema
  "Knowledge Graph schema for DataScript edge storage.

   Defines the schema for knowledge edges that connect memory entries,
   enabling graph traversal, impact analysis, and knowledge promotion."
  (:require [hive-mcp.memory.type-registry :as type-registry]))

;; Supported relation types for edges between knowledge nodes
(def relation-types
  "Valid relation types for knowledge graph edges.

   - :implements   - Realizes a principle/pattern
   - :supersedes   - Replaces previous knowledge
   - :refines      - Improves without replacing
   - :contradicts  - Conflicts with
   - :depends-on   - Requires for correctness
   - :derived-from - Synthesized from sources
   - :applies-to   - Scope applicability
   - :co-accessed  - Frequently recalled together (batch recall pattern)"
  #{:implements :supersedes :refines :contradicts
    :depends-on :derived-from :applies-to :co-accessed
    :projects-to})  ; synthetic -> entry projection

(def kg-schema
  "DataScript schema for Knowledge Graph edges.

   Bounded context pattern: separate from Chroma memory storage.
   Edges connect memory entry IDs without duplicating content."
  {:kg-edge/id            {:db/unique :db.unique/identity
                           :db/doc "Unique edge identifier (UUID string)"}
   :kg-edge/from          {:db/doc "Source node ID (memory entry ID)"}
   :kg-edge/to            {:db/doc "Target node ID (memory entry ID)"}
   :kg-edge/relation      {:db/doc "Relation type keyword from relation-types"}
   :kg-edge/scope         {:db/doc "Scope where edge was discovered (e.g., project-id)"}
   :kg-edge/confidence    {:db/doc "Confidence score 0.0-1.0"}
   :kg-edge/created-by    {:db/doc "Agent ID that created this edge"}
   :kg-edge/created-at    {:db/doc "Creation timestamp (inst)"}
   :kg-edge/last-verified {:db/doc "Timestamp of last verification that this edge is still valid (inst)"}
   :kg-edge/source-type   {:db/doc "How this edge was established: :manual, :automated, :inferred, :co-access"}})

;; =============================================================================
;; =============================================================================
;;
;; Abstraction Levels:
;;   L0: Parabola (Runtime) - Not stored, inferred from live system
;;
;; Knowledge degrades as it rises through abstraction levels. These fields
;; track the abstraction level and grounding status of knowledge entries.

(def abstraction-levels
  "Valid abstraction levels for knowledge entries.
   L0 (runtime) is not stored - it's inferred from live system state."
  {:level-1 {:level 1 :name "Disc"     :description "Files, kondo analysis, git state"}
   :level-2 {:level 2 :name "Semantic" :description "What functions DO"}
   :level-3 {:level 3 :name "Pattern"  :description "Conventions, idioms"}
   :level-4 {:level 4 :name "Intent"   :description "ADRs, decisions, axioms"}})

(def knowledge-schema
  "DataScript schema for knowledge abstraction tracking.

   Tracks the abstraction level and grounding status of knowledge entries,
   enabling drift detection and re-grounding workflows."
  {:knowledge/abstraction-level {:db/doc "Abstraction level 1-4 (1=File, 2=Semantic, 3=Pattern, 4=Intent)"}
   :knowledge/grounded-at       {:db/doc "Timestamp of last verification against lower level (inst)"}
   :knowledge/grounded-from     {:db/doc "Ref to disc entity (file/commit) verified against"}
   :knowledge/gaps              {:db/cardinality :db.cardinality/many
                                 :db/doc "Set of known abstraction gaps (keywords)"}
   :knowledge/source-hash       {:db/doc "Content hash of source when abstracted (for drift detection)"}
   :knowledge/source-type       {:db/doc "How this knowledge was created: :manual, :automated, :inferred, :co-access"}})

(def source-types
  "Valid source types for edge and knowledge provenance tracking.

   - :manual     - Explicitly created by a human or agent
   - :automated  - Created by automated analysis (kondo, git, etc.)
   - :inferred   - Derived from pattern detection or heuristics
   - :co-access  - Created from co-access pattern (batch recall)"
  #{:manual :automated :inferred :co-access})

(defn valid-relation?
  "Check if a relation type is valid."
  [relation]
  (contains? relation-types relation))

(defn valid-confidence?
  "Check if confidence score is in valid range [0.0, 1.0]."
  [confidence]
  (and (number? confidence)
       (<= 0.0 confidence 1.0)))

(defn valid-source-type?
  "Check if source type is valid."
  [source-type]
  (contains? source-types source-type))

(defn valid-abstraction-level?
  "Check if abstraction level is valid (1-4).
   L0 (runtime) is not stored, so 0 is not valid for persistence."
  [level]
  (and (integer? level)
       (<= 1 level 4)))

(defn abstraction-level-keyword
  "Convert integer level to keyword (:level-1, :level-2, :level-3, :level-4)."
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

;; Volatility classes for Bayesian certainty decay
(def volatility-classes
  "Valid volatility classes for disc certainty tracking.
   Affects how quickly certainty decays over time.

   - :stable    - Rarely changes (config, deps, infrastructure)
   - :moderate  - Changes occasionally (business logic, handlers)
   - :volatile  - Changes frequently (tests, UI, hot paths)"
  #{:stable :moderate :volatile})

(def disc-schema
  "DataScript schema for disc (file) state tracking.

   Disc entities represent the file abstraction level - actual files on disk.
   Used as grounding targets for higher-level knowledge entries.

   Bayesian Certainty Fields:
   - certainty-alpha/beta form a Beta distribution for probabilistic staleness
   - Mean certainty = alpha / (alpha + beta)
   - Higher alpha = more confident the knowledge is fresh
   - Higher beta = more observations of staleness
   - volatility-class affects decay rate between observations"
  {:disc/path              {:db/unique :db.unique/identity
                            :db/doc "File path (unique identity for the disc entity)"}
   :disc/content-hash      {:db/doc "SHA256 hash of file content"}
   :disc/analyzed-at       {:db/doc "Timestamp of last kondo/analysis (inst)"}
   :disc/git-commit        {:db/doc "Git commit hash when analyzed"}
   :disc/project-id        {:db/doc "Project scope (for multi-project support)"}
   :disc/last-read-at      {:db/doc "Timestamp of last file read by any agent (inst)"}
   :disc/read-count        {:db/doc "Number of times this file has been read by agents"}
   ;; Bayesian certainty fields
   :disc/certainty-alpha   {:db/doc "Beta distribution alpha parameter (float, default 5.0)"}
   :disc/certainty-beta    {:db/doc "Beta distribution beta parameter (float, default 2.0)"}
   :disc/volatility-class  {:db/doc "Volatility class: :stable, :moderate, or :volatile"}
   :disc/last-observation  {:db/doc "Timestamp when certainty was last updated (inst)"}})

(defn valid-volatility-class?
  "Check if volatility class is valid."
  [volatility-class]
  (contains? volatility-classes volatility-class))

(defn valid-certainty-alpha?
  "Check if certainty alpha is valid (positive number)."
  [alpha]
  (and (number? alpha) (pos? alpha)))

(defn valid-certainty-beta?
  "Check if certainty beta is valid (positive number)."
  [beta]
  (and (number? beta) (pos? beta)))

;; =============================================================================
;; Malli Specs for Disc Certainty Fields
;; =============================================================================

(def DiscCertaintyAlpha
  "Malli spec for Beta distribution alpha parameter."
  [:and :double [:> 0]])

(def DiscCertaintyBeta
  "Malli spec for Beta distribution beta parameter."
  [:and :double [:> 0]])

(def DiscVolatilityClass
  "Malli spec for volatility class enum."
  [:enum :stable :moderate :volatile])

(def DiscLastObservation
  "Malli spec for last observation timestamp."
  inst?)

(def DiscCertaintyFields
  "Malli spec for the complete set of Bayesian certainty fields."
  [:map
   [:disc/certainty-alpha {:optional true} DiscCertaintyAlpha]
   [:disc/certainty-beta {:optional true} DiscCertaintyBeta]
   [:disc/volatility-class {:optional true} DiscVolatilityClass]
   [:disc/last-observation {:optional true} DiscLastObservation]])

;; =============================================================================
;; Default Values for New Disc Entities
;; =============================================================================

(def disc-certainty-defaults
  "Default values for Bayesian certainty fields on new disc entities.

   Alpha=5, Beta=2 gives:
   - Mean certainty: 5/(5+2) = 0.714 (moderately confident)
   - Variance: relatively low, reflecting prior belief
   - Represents 'reasonably fresh but not certain' starting state"
  {:disc/certainty-alpha  5.0
   :disc/certainty-beta   2.0
   :disc/volatility-class :moderate})

(defn disc-certainty-defaults-with-timestamp
  "Returns disc certainty defaults with current timestamp for last-observation."
  []
  (assoc disc-certainty-defaults
         :disc/last-observation (java.util.Date.)))

(defn apply-disc-certainty-defaults
  "Apply default certainty values to a disc entity map.
   Only sets values for keys not already present."
  [disc-entity]
  (merge (disc-certainty-defaults-with-timestamp) disc-entity))

;; =============================================================================
;; Synthetic Node Schema (Emergent Clusters)
;; =============================================================================
;;
;; Synthetic nodes represent emergent pattern-level discovered through
;; co-access analysis, temporal proximity, or semantic similarity.
;; Per convention 20260131014506-72b6afed: Emergent knowledge is, not stored as
;; raw entries, but as synthesized clusters that project onto entries.

(def synthetic-types
  "Valid types for synthetic (emergent) knowledge nodes.

   - :co-access          - Entries frequently recalled together
   - :temporal-proximity - Entries created/accessed within time windows
   - :semantic-cluster   - Entries with high embedding similarity
   - :workflow-step      - Sequential pattern in agent workflows
   - :decision-cluster   - Related decisions forming a decision tree"
  #{:co-access :temporal-proximity :semantic-cluster
    :workflow-step :decision-cluster})

(def synthetic-schema
  "DataScript schema for synthetic emergent nodes.

   Synthetic nodes are discovered patterns that don't exist as direct
   memory entries. They aggregate multiple entries via :projects-to
   edges, enabling pattern-level queries without denormalizing content."
  {:kg-synthetic/id             {:db/unique :db.unique/identity
                                 :db/doc "Unique synthetic node ID (UUID string)"}
   :kg-synthetic/type           {:db/doc "Synthetic type from synthetic-types set"}
   :kg-synthetic/members        {:db/cardinality :db.cardinality/many
                                 :db/doc "Set of member entry IDs that form this cluster"}
   :kg-synthetic/confidence     {:db/doc "Aggregate confidence score 0.0-1.0"}
   :kg-synthetic/created-at     {:db/doc "Timestamp when pattern was first discovered (inst)"}
   :kg-synthetic/last-reinforced {:db/doc "Timestamp when pattern was last reinforced by co-access (inst)"}
   :kg-synthetic/centroid       {:db/doc "Optional embedding vector representing cluster centroid"}
   :kg-synthetic/label          {:db/doc "Human-readable label for the synthetic node"}
   :kg-synthetic/scope          {:db/doc "Project scope where this pattern was discovered"}})

(defn valid-synthetic-type?
  "Check if a synthetic type is valid."
  [synthetic-type]
  (contains? synthetic-types synthetic-type))

;; =============================================================================
;; Abstraction Level Helpers
;; =============================================================================

(def type->abstraction-level
  "Maps memory entry types to their default abstraction levels.
   Derived from type-registry (SST)."
  type-registry/type->abstraction)

(defn derive-abstraction-level
  "Derive the default abstraction level for a memory entry type.
   Returns integer 2-4, defaulting to 2 (Semantic) for unknown types."
  [entry-type]
  (type-registry/abstraction-level entry-type))

(defn full-schema
  "Returns the combined KG schema (edges + knowledge abstraction + disc + synthetic)."
  []
  (merge kg-schema knowledge-schema disc-schema synthetic-schema))
