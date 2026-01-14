(ns hive-mcp.specs.hooks
  "Clojure specs for hive-mcp hooks domain.

   Defines data contracts for:
   - Hook event types (task-start, task-complete, ling-spawn, etc.)
   - Hook function signatures
   - Hook context passed to handlers
   - Hook payloads for shout events
   - Hook registry entries

   Reference: crystal/hooks.clj domain implementation"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;; ============================================================
;; Hook Event Enum
;; ============================================================

(s/def ::hook-event
  #{:task-start :task-complete :session-start :session-end
    :file-modified :error :ling-spawn :ling-terminate})

(s/def ::hook-event-string
  #{"task-start" "task-complete" "session-start" "session-end"
    "file-modified" "error" "ling-spawn" "ling-terminate"})

(s/def ::hook-event-any
  (s/or :keyword ::hook-event
        :string ::hook-event-string))

;; ============================================================
;; Basic Type Specs
;; ============================================================

(s/def ::non-empty-string
  (s/and string? #(pos? (count %))))

(s/def ::slave-id ::non-empty-string)

(s/def ::task ::non-empty-string)

(s/def ::files (s/coll-of ::non-empty-string :kind vector?))

(s/def ::error (s/nilable (s/or :string string?
                                :exception #(instance? Throwable %)
                                :map map?)))

(s/def ::data (s/nilable map?))

(s/def ::message (s/nilable string?))

;; ============================================================
;; Hook Function Spec
;; ============================================================

(s/def ::hook-fn ifn?)

;; ============================================================
;; Hook Context Spec
;; ============================================================

(s/def ::event ::hook-event-any)

(s/def ::hook-context
  (s/keys :opt-un [::event ::slave-id ::task ::files ::error ::data ::message]))

;; ============================================================
;; Hook Payload Spec (for shout events)
;; ============================================================

(s/def ::hook-type keyword?)

(s/def ::hook-payload
  (s/keys :req-un [::hook-type]
          :opt-un [::files ::message ::data]))

;; ============================================================
;; Hook Registry Entry Spec
;; ============================================================

(s/def ::priority (s/and int? #(<= 0 % 100)))

(s/def ::enabled boolean?)

(s/def ::hook-entry
  (s/keys :req-un [::hook-event ::hook-fn]
          :opt-un [::priority ::enabled]))

;; Hook ID for registry lookup
(s/def ::hook-id ::non-empty-string)

;; Registry is a map of hook-id -> hook-entry
(s/def ::hook-registry
  (s/map-of ::hook-id ::hook-entry))

;; ============================================================
;; Function Specs (fdef)
;; ============================================================

;; register-hook : registry event fn -> registry'
(s/fdef hive-mcp.hooks/register-hook
  :args (s/cat :registry any?
               :event ::hook-event
               :hook-fn ::hook-fn)
  :ret any?)

;; trigger-hooks : registry event context -> [results]
(s/fdef hive-mcp.hooks/trigger-hooks
  :args (s/cat :registry any?
               :event ::hook-event
               :context ::hook-context)
  :ret any?)

;; emit-hook-event : hook-type payload -> boolean
(s/fdef hive-mcp.hooks/emit-hook-event
  :args (s/cat :hook-type ::hook-type
               :payload map?)
  :ret boolean?)

;; ============================================================
;; Helper Functions
;; ============================================================

(defn valid-hook-event?
  "Check if event is a valid hook event keyword."
  [event]
  (s/valid? ::hook-event event))

(defn valid-hook-payload?
  "Check if payload is a valid hook payload map."
  [payload]
  (s/valid? ::hook-payload payload))

(defn explain-hook-payload
  "Explain why payload is not a valid hook payload."
  [payload]
  (s/explain-data ::hook-payload payload))

(defn valid-hook-context?
  "Check if context is a valid hook context map."
  [context]
  (s/valid? ::hook-context context))

(defn explain-hook-context
  "Explain why context is not a valid hook context."
  [context]
  (s/explain-data ::hook-context context))

(defn valid-hook-entry?
  "Check if entry is a valid hook registry entry."
  [entry]
  (s/valid? ::hook-entry entry))

(defn explain-hook-entry
  "Explain why entry is not a valid hook entry."
  [entry]
  (s/explain-data ::hook-entry entry))

;; ============================================================
;; Generators for test.check
;; ============================================================

(defn hook-event-gen
  "Generator for hook event keywords."
  []
  (gen/elements [:task-start :task-complete :session-start :session-end
                 :file-modified :error :ling-spawn :ling-terminate]))

(defn hook-type-gen
  "Generator for hook type keywords."
  []
  (gen/elements [:kanban-done :memory-accessed :task-completed :session-wrap]))

(defn slave-id-gen
  "Generator for slave identifiers."
  []
  (gen/fmap #(str "slave-" %)
            (gen/string-alphanumeric)))

(defn hook-context-gen
  "Generator for hook context maps."
  []
  (gen/hash-map
   :event (hook-event-gen)
   :slave-id (gen/fmap #(str "slave-" %) (gen/string-alphanumeric))
   :task (gen/fmap #(str "Task: " %) (gen/string-alphanumeric))))

(defn hook-payload-gen
  "Generator for hook payload maps."
  []
  (gen/hash-map
   :hook-type (hook-type-gen)
   :message (gen/fmap #(str "Message: " %) (gen/string-alphanumeric))))

(defn hook-entry-gen
  "Generator for hook registry entries."
  []
  (gen/hash-map
   :hook-event (hook-event-gen)
   :hook-fn (gen/return identity)
   :priority (gen/choose 0 100)
   :enabled (gen/boolean)))
