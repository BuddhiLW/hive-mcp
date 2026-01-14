(ns hive-mcp.specs.memory
  "Clojure specs for hive-mcp memory domain.

   Defines data contracts for:
   - Memory entry types (note, snippet, convention, decision)
   - Duration categories (ephemeral, short, medium, long, permanent)
   - Scope tags (project-scoped, global)
   - Chroma storage operations"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;; ============================================================
;; Entry Type Specs
;; ============================================================

(s/def ::entry-type
  #{:note :snippet :convention :decision :conversation})

(s/def ::duration
  #{:ephemeral :short :medium :long :permanent})

;; Duration also accepted as strings from JSON
(s/def ::duration-string
  #{"ephemeral" "short" "medium" "long" "permanent"})

(s/def ::duration-any
  (s/or :keyword ::duration
        :string ::duration-string))

;; ============================================================
;; Content Specs
;; ============================================================

(s/def ::non-empty-string
  (s/and string? #(pos? (count %))))

(s/def ::content ::non-empty-string)

(s/def ::tag ::non-empty-string)
(s/def ::tags (s/coll-of ::tag :kind vector?))

;; Scope format: "scope:project:<project-id>" or "scope:global"
(defn valid-scope-tag? [s]
  (or (= s "scope:global")
      (re-matches #"scope:project:.+" s)))

(s/def ::scope-tag
  (s/and string? valid-scope-tag?))

;; ============================================================
;; Timestamp Specs
;; ============================================================

(s/def ::iso-timestamp
  (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.*" %)))

(s/def ::created ::iso-timestamp)
(s/def ::updated ::iso-timestamp)
(s/def ::expires (s/nilable ::iso-timestamp))

;; ============================================================
;; Memory Entry Spec
;; ============================================================

(s/def ::id ::non-empty-string)
(s/def ::type ::entry-type)
(s/def ::content-hash ::non-empty-string)

(s/def ::memory-entry
  (s/keys :req-un [::id ::type ::content ::tags]
          :opt-un [::duration ::created ::updated ::expires ::content-hash]))

;; Entry for creation (no id yet)
(s/def ::new-entry
  (s/keys :req-un [::type ::content]
          :opt-un [::tags ::duration]))

;; ============================================================
;; Query Specs
;; ============================================================

(s/def ::limit (s/and int? pos?))
(s/def ::scope (s/nilable string?))

(s/def ::query-params
  (s/keys :opt-un [::type ::duration ::tags ::scope ::limit]))

;; ============================================================
;; Function Specs (fdef)
;; ============================================================

;; index-memory-entry! : entry-map -> id-string
(s/fdef hive-mcp.chroma/index-memory-entry!
  :args (s/cat :entry ::new-entry)
  :ret ::id)

;; get-entry-by-id : id-string -> entry | nil
(s/fdef hive-mcp.chroma/get-entry-by-id
  :args (s/cat :id ::id)
  :ret (s/nilable ::memory-entry))

;; query-entries : query-map -> [entry...]
(s/fdef hive-mcp.chroma/query-entries
  :args (s/cat :query ::query-params)
  :ret (s/coll-of ::memory-entry))

;; cleanup-expired! : -> count
(s/fdef hive-mcp.chroma/cleanup-expired!
  :args (s/cat)
  :ret nat-int?)

;; entries-expiring-soon : days -> [entry...]
(s/fdef hive-mcp.chroma/entries-expiring-soon
  :args (s/cat :days pos-int?)
  :ret (s/coll-of ::memory-entry))

;; ============================================================
;; Helper Functions
;; ============================================================

(defn valid-entry? [entry]
  (s/valid? ::memory-entry entry))

(defn explain-entry [entry]
  (s/explain-data ::memory-entry entry))

(defn valid-new-entry? [entry]
  (s/valid? ::new-entry entry))

(defn explain-new-entry [entry]
  (s/explain-data ::new-entry entry))

;; ============================================================
;; Generators for test.check
;; ============================================================

(defn entry-type-gen []
  (gen/elements [:note :snippet :convention :decision :conversation]))

(defn duration-gen []
  (gen/elements [:ephemeral :short :medium :long :permanent]))

(defn scope-tag-gen []
  (gen/one-of [(gen/return "scope:global")
               (gen/fmap #(str "scope:project:" %)
                         (gen/string-alphanumeric))]))
