(ns hive-mcp.graph.schema
  "Datascript schema for hive-mcp knowledge graph.
   
   Defines the schema for:
   - Friction entries (ling-reported blockers, tool gaps, workflow issues)
   - Knowledge entries (conventions, decisions, patterns derived from friction)
   - Agents (hivemind, lings, drones with their relationships)
   
   Schema follows Datascript conventions:
   - :db/valueType - Type of value (:db.type/string, :db.type/ref, etc.)
   - :db/cardinality - :db.cardinality/one or :db.cardinality/many
   - :db/unique - :db.unique/identity or :db.unique/value for uniqueness
   - :db/isComponent - true for owned refs (cascade delete)
   
   SOLID: Single responsibility - schema definition only.
   DDD: Value objects and entity definitions for graph domain.")

;;; -----------------------------------------------------------------------------
;;; Friction Types (Enum values)
;;; -----------------------------------------------------------------------------

(def friction-types
  "Valid friction types reported by lings.
   
   :tool-missing    - Required tool/capability not available
   :preset-gap      - Preset doesn't cover needed scenario  
   :workflow-blocker - Process/workflow impediment"
  #{:tool-missing :preset-gap :workflow-blocker})

;;; -----------------------------------------------------------------------------
;;; Knowledge Types (Enum values)
;;; -----------------------------------------------------------------------------

(def knowledge-types
  "Valid knowledge entry types.
   
   :convention - Agreed-upon practice or coding standard
   :decision   - Architectural or design decision with rationale
   :pattern    - Reusable solution pattern extracted from experience"
  #{:convention :decision :pattern})

;;; -----------------------------------------------------------------------------
;;; Agent Types (Enum values)
;;; -----------------------------------------------------------------------------

(def agent-types
  "Valid agent types in the swarm.
   
   :hivemind - Coordinator/orchestrator agent
   :ling     - Worker agent spawned by hivemind
   :drone    - Lightweight agent for simple tasks"
  #{:hivemind :ling :drone})

;;; -----------------------------------------------------------------------------
;;; Schema Definition
;;; -----------------------------------------------------------------------------

(def schema
  "Datascript schema for knowledge graph.
   
   Entity types:
   - Friction: Reports from lings about blockers/gaps
   - Knowledge: Derived insights from friction patterns
   - Agent: Swarm participants (hivemind, lings, drones)"

  {;;; =========================================================================
   ;;; Friction Entity
   ;;; =========================================================================
   ;;
   ;; Friction entries capture reported issues from lings during task execution.
   ;; These are the raw signals that get analyzed to derive knowledge.
   ;;
   ;; Note: Datascript is schemaless for value types. Schema only needed for:
   ;; - :db/valueType :db.type/ref - for entity references
   ;; - :db/cardinality :db.cardinality/many - for multi-value attrs
   ;; - :db/unique - for uniqueness constraints

   :friction/reported-by
   {:db/doc "Reference to the agent that reported this friction"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   ;;; =========================================================================
   ;;; Knowledge Entity
   ;;; =========================================================================
   ;;
   ;; Knowledge entries are derived from friction patterns. They represent
   ;; learnings that should persist and inform future work.

   :knowledge/derived-from
   {:db/doc "References to friction entries that led to this knowledge"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}

   :knowledge/superseded-by
   {:db/doc "Reference to newer knowledge that supersedes this entry"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :knowledge/tags
   {:db/doc "Tags for categorization and search"
    :db/cardinality :db.cardinality/many}

   ;;; =========================================================================
   ;;; Agent Entity
   ;;; =========================================================================
   ;;
   ;; Agents are the participants in the swarm. Tracking them enables
   ;; lineage queries (who reported what, who derived what).

   :agent/id
   {:db/doc "Unique identifier for the agent"
    :db/unique :db.unique/identity}

   :agent/reported-frictions
   {:db/doc "References to frictions reported by this agent"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent false}

   :agent/spawned-by
   {:db/doc "Reference to parent agent (for lings spawned by hivemind)"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}})

;;; -----------------------------------------------------------------------------
;;; Schema Helpers
;;; -----------------------------------------------------------------------------

(defn valid-friction-type?
  "Check if type is a valid friction type."
  [t]
  (contains? friction-types t))

(defn valid-knowledge-type?
  "Check if type is a valid knowledge type."
  [t]
  (contains? knowledge-types t))

(defn valid-agent-type?
  "Check if type is a valid agent type."
  [t]
  (contains? agent-types t))

;;; -----------------------------------------------------------------------------
;;; Entity Constructors
;;; -----------------------------------------------------------------------------

(defn make-friction
  "Create a friction entity map.
   
   Required: type, context, reported-by
   Optional: workaround"
  [{:keys [type context reported-by workaround]}]
  {:pre [(valid-friction-type? type)
         (string? context)
         (some? reported-by)]}
  (cond-> {:friction/type type
           :friction/context context
           :friction/reported-by reported-by
           :friction/count 1
           :friction/created-at (java.util.Date.)
           :friction/resolved? false}
    workaround (assoc :friction/workaround workaround)))

(defn make-knowledge
  "Create a knowledge entity map.
   
   Required: type, content
   Optional: derived-from (refs), confidence (default 0.5), tags"
  [{:keys [type content derived-from confidence tags]}]
  {:pre [(valid-knowledge-type? type)
         (string? content)]}
  (cond-> {:knowledge/type type
           :knowledge/content content
           :knowledge/confidence (or confidence 0.5)
           :knowledge/created-at (java.util.Date.)}
    derived-from (assoc :knowledge/derived-from derived-from)
    tags (assoc :knowledge/tags (set tags))))

(defn make-agent
  "Create an agent entity map.
   
   Required: id, type
   Optional: spawned-by (ref to parent agent)"
  [{:keys [id type spawned-by]}]
  {:pre [(string? id)
         (valid-agent-type? type)]}
  (let [now (java.util.Date.)]
    (cond-> {:agent/id id
             :agent/type type
             :agent/created-at now
             :agent/last-active now}
      spawned-by (assoc :agent/spawned-by spawned-by))))
