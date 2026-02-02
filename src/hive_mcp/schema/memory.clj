(ns hive-mcp.schema.memory
  "Malli schemas for memory entries and related types.

   Core domain schemas for the memory system, defining structure and
   validation for memory entries, types, durations, and tags.

   Usage:
   ```clojure
   (require '[malli.core :as m]
            '[hive-mcp.schema.memory :as mem])

   (m/validate mem/MemoryEntry
     {:id \"20260131-abc123\"
      :type \"decision\"
      :content \"Use Malli for schema validation\"
      :tags [\"architecture\" \"validation\"]
      :duration \"long\"})
   ```

   SOLID: SRP - Schema definitions only, no behavior.
   CLARITY: R - Represented intent through explicit types."
  (:require [malli.core :as m]
            [malli.util :as mu]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Enums
;; =============================================================================

(def MemoryType
  "Valid memory entry types.

   L2 (Semantic): snippet, note, doc, todo, question, answer, warning, error
   L3 (Pattern):  convention, pattern, lesson, rule, guideline, workflow, recipe
   L4 (Intent):   decision, axiom, principle"
  [:enum
   ;; L2 - Semantic level
   "snippet" "note" "doc" "todo" "question" "answer" "warning" "error"
   ;; L3 - Pattern level
   "convention" "pattern" "lesson" "rule" "guideline" "workflow" "recipe"
   ;; L4 - Intent level
   "decision" "axiom" "principle"])

(def MemoryDuration
  "Valid duration values for memory entries.

   - session:   Expires at end of current session
   - short:     Expires in 7 days
   - medium:    Expires in 30 days
   - long:      Expires in 1 year
   - permanent: Never expires"
  [:enum "session" "short" "medium" "long" "permanent"])

;; =============================================================================
;; Tags
;; =============================================================================

(def MemoryTag
  "A single tag - non-empty string."
  [:string {:min 1}])

(def MemoryTags
  "Vector of tags - may be empty."
  [:vector MemoryTag])

;; =============================================================================
;; Scope
;; =============================================================================

(def ProjectScope
  "Project scope identifier - non-empty string like 'scope:hive-mcp'."
  [:and :string [:fn #(or (= % "scope:global")
                          (re-matches #"scope:[a-zA-Z0-9_-]+" %))]])

;; =============================================================================
;; Abstraction Level
;; =============================================================================

(def AbstractionLevel
  "Knowledge abstraction level (1-4).

   L1: Disc (files, code)
   L2: Semantic (what things DO)
   L3: Pattern (conventions, idioms)
   L4: Intent (decisions, axioms)"
  [:int {:min 1 :max 4}])

;; =============================================================================
;; Memory Entry
;; =============================================================================

(def MemoryEntryId
  "Memory entry ID - timestamp-based identifier."
  [:string {:min 1}])

(def MemoryEntry
  "Complete memory entry schema.

   Required fields:
   - id: Unique identifier
   - type: Entry type (decision, snippet, etc.)
   - content: The actual content/knowledge

   Optional fields:
   - tags: Categorization tags
   - duration: How long to retain
   - project-id: Scope/project association
   - created-at: Timestamp
   - expires: Expiration timestamp (string)
   - content-hash: SHA256 of content
   - abstraction-level: Knowledge level (1-4)
   - kg-outgoing: KG edge IDs where this is source
   - kg-incoming: KG edge IDs where this is target"
  [:map
   [:id MemoryEntryId]
   [:type MemoryType]
   [:content :string]
   [:tags {:optional true} MemoryTags]
   [:duration {:optional true} MemoryDuration]
   [:project-id {:optional true} [:maybe :string]]
   [:created-at {:optional true} [:maybe inst?]]
   [:expires {:optional true} [:maybe :string]]
   [:content-hash {:optional true} [:maybe :string]]
   [:abstraction-level {:optional true} [:maybe AbstractionLevel]]
   [:kg-outgoing {:optional true} [:maybe [:vector :string]]]
   [:kg-incoming {:optional true} [:maybe [:vector :string]]]])

(def MemoryEntryMinimal
  "Minimal memory entry for creation - only required fields."
  [:map
   [:type MemoryType]
   [:content [:string {:min 1}]]])

(def MemoryMetadata
  "Memory entry metadata (for lightweight queries).

   Returned by mcp_memory_query_metadata for token efficiency."
  [:map
   [:id MemoryEntryId]
   [:type MemoryType]
   [:preview {:optional true} [:maybe :string]]
   [:tags {:optional true} MemoryTags]
   [:created {:optional true} [:maybe :string]]])

;; =============================================================================
;; Query Results
;; =============================================================================

(def MemoryQueryResult
  "Result from memory query - vector of entries."
  [:vector MemoryEntry])

(def MemoryMetadataResult
  "Result from metadata query - vector of metadata records."
  [:vector MemoryMetadata])

;; =============================================================================
;; Validators
;; =============================================================================

(defn valid-type?
  "Check if type string is a valid MemoryType."
  [type-str]
  (m/validate MemoryType type-str))

(defn valid-duration?
  "Check if duration string is a valid MemoryDuration."
  [duration-str]
  (m/validate MemoryDuration duration-str))

(defn valid-entry?
  "Check if entry map is a valid MemoryEntry."
  [entry]
  (m/validate MemoryEntry entry))

(defn explain-entry
  "Explain validation errors for a MemoryEntry."
  [entry]
  (m/explain MemoryEntry entry))

;; =============================================================================
;; Schema Registry Entry
;; =============================================================================

(def registry
  "Schema registry entries for memory types.

   Usage with malli registry:
   ```clojure
   (mr/set-default-registry!
     (mr/composite-registry
       (m/default-schemas)
       hive-mcp.schema.memory/registry))
   ```"
  {:memory/type MemoryType
   :memory/duration MemoryDuration
   :memory/tag MemoryTag
   :memory/tags MemoryTags
   :memory/entry MemoryEntry
   :memory/entry-minimal MemoryEntryMinimal
   :memory/metadata MemoryMetadata
   :memory/abstraction-level AbstractionLevel
   :memory/project-scope ProjectScope})

(comment
  ;; Example usage

  (m/validate MemoryType "decision")
  ;; => true

  (m/validate MemoryDuration "long")
  ;; => true

  (m/validate MemoryEntry
              {:id "20260131-abc123"
               :type "decision"
               :content "Use Malli for all schema validation"
               :tags ["architecture" "tooling"]
               :duration "long"})
  ;; => true

  (m/validate MemoryEntryMinimal
              {:type "snippet"
               :content "(defn hello [] \"world\")"})
  ;; => true

  (m/explain MemoryEntry {:type "invalid" :content "test"})
  ;; => {:errors [...]}
  )
