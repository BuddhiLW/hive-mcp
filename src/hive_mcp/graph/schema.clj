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
   
   DDD: Value objects and entity definitions for graph domain.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(def friction-types
  "Valid friction types reported by lings.
   
   :tool-missing    - Required tool/capability not available
   :preset-gap      - Preset doesn't cover needed scenario  
   :workflow-blocker - Process/workflow impediment"
  #{:tool-missing :preset-gap :workflow-blocker})


(def knowledge-types
  "Valid knowledge entry types.
   
   :convention - Agreed-upon practice or coding standard
   :decision   - Architectural or design decision with rationale
   :pattern    - Reusable solution pattern extracted from experience"
  #{:convention :decision :pattern})


(def memory-types
  "Valid memory entry types (from Emacs memory store).

   :note       - General notes and observations
   :snippet    - Code snippets or examples
   :convention - Agreed-upon practices
   :decision   - Architectural or design decisions
   :axiom      - Foundational, inviolable principles (loaded first by catchup)"
  #{:note :snippet :convention :decision :axiom})


(def duration-types
  "Valid duration/TTL categories for memory entries.
   
   :ephemeral  - 1 day TTL
   :short      - 7 days TTL
   :medium     - 30 days TTL
   :long       - 90 days TTL
   :permanent  - Never expires"
  #{:ephemeral :short :medium :long :permanent})


(def recall-contexts
  "Valid recall context types for tracking access patterns.
   
   :catchup-structural  - Mechanical catchup query (low weight)
   :wrap-structural     - Mechanical wrap query (low weight)
   :explicit-reference  - LLM explicitly cited (high weight)
   :cross-session       - Accessed from different session (high weight)
   :cross-project       - Accessed from different project (highest weight)
   :user-feedback       - User marked as helpful (highest weight)"
  #{:catchup-structural :wrap-structural :explicit-reference
    :cross-session :cross-project :user-feedback})


(def agent-types
  "Valid agent types in the swarm.

   :hivemind - Coordinator/orchestrator agent
   :ling     - Worker agent spawned by hivemind
   :drone    - Lightweight agent for simple tasks"
  #{:hivemind :ling :drone})


(def change-plan-statuses
  "Valid statuses for change plans (batch drone execution).

   :pending      - Plan created, not yet executing
   :executing    - Drones actively processing items
   :complete     - All items completed successfully
   :partial-fail - Some items failed, others completed"
  #{:pending :executing :complete :partial-fail})


(def change-item-statuses
  "Valid statuses for individual change items.

   :pending    - Not yet assigned to a drone
   :dispatched - Assigned to a drone, awaiting completion
   :completed  - Successfully completed
   :error      - Failed with error"
  #{:pending :dispatched :completed :error})


(def wave-statuses
  "Valid statuses for execution waves (batch of concurrent drones).

   :running      - Drones actively executing
   :complete     - All drones completed successfully
   :partial-fail - Some drones failed"
  #{:running :complete :partial-fail})


(def schema
  "Datascript schema for knowledge graph.
   
   Entity types:
   - Memory: Entries from Emacs memory store (notes, snippets, conventions, decisions)
   - Recall: Access tracking for memory promotion scoring
   - Friction: Reports from lings about blockers/gaps
   - Knowledge: Derived insights from friction patterns
   - Agent: Swarm participants (hivemind, lings, drones)"

  {;;; =========================================================================
   ;;; Memory Entity (from crystal module)
   ;;; =========================================================================
   ;;
   ;; Memory entries mirror the Emacs memory store, enabling graph queries
   ;; for session lineage, cross-references, and promotion candidates.

   :memory/id
   {:db/doc "Unique identifier for the memory entry (UUID string)"
    :db/unique :db.unique/identity}

   :memory/tags
   {:db/doc "Tags for categorization and search"
    :db/cardinality :db.cardinality/many}

   :memory/references
   {:db/doc "References to other memory entries (cross-references)"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}

   ;;; =========================================================================
   ;;; Recall Entity (access tracking for promotion)
   ;;; =========================================================================
   ;;
   ;; Tracks how memory entries are accessed, with context-aware weighting.
   ;; Used by crystal/core to calculate promotion scores.

   :recall/memory
   {:db/doc "Reference to the memory entry being accessed"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   ;;; =========================================================================
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
    :db/cardinality :db.cardinality/one}

   ;;; =========================================================================
   ;;; Change Plan Entity (batch drone execution)
   ;;; =========================================================================
   ;;
   ;; Change plans group related drone tasks for batch execution.
   ;; A plan can be created first, then executed as waves.

   :change-plan/id
   {:db/doc "Unique identifier for the change plan (UUID string)"
    :db/unique :db.unique/identity}

   ;; Note: :change-plan/status, :change-plan/preset, :change-plan/created-at
   ;; are schemaless (no special constraints needed)

   ;;; =========================================================================
   ;;; Change Item Entity (individual file mutation task)
   ;;; =========================================================================
   ;;
   ;; Change items are individual file mutation tasks within a plan.
   ;; Each item targets one file and is assigned to one drone.

   :change-item/id
   {:db/doc "Unique identifier for the change item (UUID string)"
    :db/unique :db.unique/identity}

   :change-item/plan
   {:db/doc "Reference to the parent change plan"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :change-item/file
   {:db/doc "File path this item will modify (indexed for conflict detection)"
    :db/index true}

   ;; Note: :change-item/task, :change-item/drone-id, :change-item/status,
   ;; :change-item/error are schemaless

   ;;; =========================================================================
   ;;; Wave Entity (execution batch)
   ;;; =========================================================================
   ;;
   ;; Waves are execution batches - a set of drones running concurrently.
   ;; A plan may have multiple waves if items exceed concurrency limits.

   :wave/id
   {:db/doc "Unique identifier for the wave (UUID string)"
    :db/unique :db.unique/identity}

   :wave/plan
   {:db/doc "Reference to the parent change plan"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   ;; Note: :wave/drone-count, :wave/completed-count, :wave/status
   ;; are schemaless (no special constraints needed)
   })


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

(defn valid-memory-type?
  "Check if type is a valid memory type."
  [t]
  (contains? memory-types t))

(defn valid-duration-type?
  "Check if type is a valid duration type."
  [t]
  (contains? duration-types t))

(defn valid-recall-context?
  "Check if context is a valid recall context."
  [ctx]
  (contains? recall-contexts ctx))

(defn valid-change-plan-status?
  "Check if status is a valid change plan status."
  [s]
  (contains? change-plan-statuses s))

(defn valid-change-item-status?
  "Check if status is a valid change item status."
  [s]
  (contains? change-item-statuses s))

(defn valid-wave-status?
  "Check if status is a valid wave status."
  [s]
  (contains? wave-statuses s))


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

(defn make-memory
  "Create a memory entity map for the graph.
   
   Required: id, type, duration
   Optional: session-id, tags, references (entity ids)"
  [{:keys [id type duration session-id tags references]}]
  {:pre [(string? id)
         (valid-memory-type? (keyword type))
         (valid-duration-type? (keyword duration))]}
  (cond-> {:memory/id id
           :memory/type (keyword type)
           :memory/duration (keyword duration)
           :memory/created-at (java.util.Date.)}
    session-id (assoc :memory/session-id session-id)
    (seq tags) (assoc :memory/tags (set tags))
    (seq references) (assoc :memory/references references)))

(defn make-recall
  "Create a recall entity map for tracking memory access.

   Required: memory (entity ref or lookup ref), context
   Optional: count (default 1)"
  [{:keys [memory context count]}]
  {:pre [(some? memory)
         (valid-recall-context? (keyword context))]}
  {:recall/memory memory
   :recall/context (keyword context)
   :recall/count (or count 1)
   :recall/accessed-at (java.util.Date.)})

(defn make-change-plan
  "Create a change plan entity map for batch drone execution.

   Required: id
   Optional: preset (drone preset to use), status (default :pending)"
  [{:keys [id preset status]}]
  {:pre [(string? id)
         (or (nil? status) (valid-change-plan-status? status))]}
  (cond-> {:change-plan/id id
           :change-plan/status (or status :pending)
           :change-plan/created-at (java.util.Date.)}
    preset (assoc :change-plan/preset preset)))

(defn make-change-item
  "Create a change item entity map for an individual file mutation task.

   Required: id, plan (ref to change-plan), file (path), task (description)
   Optional: status (default :pending)"
  [{:keys [id plan file task status]}]
  {:pre [(string? id)
         (some? plan)
         (string? file)
         (string? task)
         (or (nil? status) (valid-change-item-status? status))]}
  {:change-item/id id
   :change-item/plan plan
   :change-item/file file
   :change-item/task task
   :change-item/status (or status :pending)})

(defn make-wave
  "Create a wave entity map for an execution batch.

   Required: id, plan (ref to change-plan)
   Optional: drone-count (default 0), status (default :running)"
  [{:keys [id plan drone-count status]}]
  {:pre [(string? id)
         (some? plan)
         (or (nil? status) (valid-wave-status? status))]}
  {:wave/id id
   :wave/plan plan
   :wave/drone-count (or drone-count 0)
   :wave/completed-count 0
   :wave/status (or status :running)})
