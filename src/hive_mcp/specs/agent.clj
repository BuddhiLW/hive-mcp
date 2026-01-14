(ns hive-mcp.specs.agent
  "Clojure specs for hive-mcp agent/swarm domain.

   Defines data contracts for:
   - Backend types (ollama, openrouter)
   - Task types (coding, arch, docs)
   - Agent delegation requests
   - Swarm events (shout, ask)
   - Hivemind coordination

   Reference: mcp-clojure-sdk/src/io/modelcontext/clojure_sdk/specs.clj"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;; ============================================================
;; Backend Enums
;; ============================================================

(s/def ::backend
  #{:ollama :openrouter})

(s/def ::backend-string
  #{"ollama" "openrouter"})

(s/def ::backend-any
  (s/or :keyword ::backend
        :string ::backend-string))

(s/def ::task-type
  #{:coding :coding-alt :arch :docs})

(s/def ::task-type-string
  #{"coding" "coding-alt" "arch" "docs"})

(s/def ::task-type-any
  (s/or :keyword ::task-type
        :string ::task-type-string))

;; Model identifier string (e.g., "mistralai/devstral-2512:free")
(s/def ::model
  (s/and string? #(pos? (count %))))

;; Preset name string (e.g., "tdd", "reviewer", "documenter")
(s/def ::preset
  (s/and string? #(pos? (count %))))

;; ============================================================
;; Common Specs
;; ============================================================

(s/def ::non-empty-string
  (s/and string? #(pos? (count %))))

(s/def ::agent-id ::non-empty-string)

(s/def ::task ::non-empty-string)

(s/def ::message (s/nilable string?))

(s/def ::tool-name (s/and string? #(re-matches #"[a-z_]+" %)))
(s/def ::tools (s/coll-of (s/or :string ::tool-name
                                :keyword keyword?)))

(s/def ::permission keyword?)
(s/def ::permissions (s/coll-of ::permission :kind set?))

(s/def ::max-steps (s/and int? pos?))

(s/def ::trace boolean?)

(s/def ::timeout-ms (s/and int? pos?))

(s/def ::host
  (s/and string? #(re-matches #"https?://.*" %)))

(s/def ::api-key (s/nilable string?))

;; ============================================================
;; Delegate Request Spec
;; ============================================================

(s/def ::delegate-request
  (s/keys :req-un [::task]
          :opt-un [::backend
                   ::model
                   ::preset
                   ::task-type
                   ::host
                   ::api-key
                   ::tools
                   ::permissions
                   ::max-steps
                   ::trace]))

;; Minimal delegate request (just task)
(s/def ::delegate-request-minimal
  (s/keys :req-un [::task]))

;; OpenRouter-specific delegate request
(s/def ::openrouter-delegate-request
  (s/and ::delegate-request
         #(= (:backend %) :openrouter)))

;; Ollama-specific delegate request
(s/def ::ollama-delegate-request
  (s/and ::delegate-request
         #(= (:backend %) :ollama)))

;; ============================================================
;; Delegate Result Spec
;; ============================================================

(s/def ::status #{:success :error :max-steps-reached})

(s/def ::result string?)

(s/def ::step
  (s/keys :req-un [::role]
          :opt-un [::content ::tool-calls ::tool-results]))

(s/def ::role #{"user" "assistant" "tool"})
(s/def ::content (s/nilable string?))
(s/def ::tool-calls (s/coll-of map?))
(s/def ::tool-results (s/coll-of map?))
(s/def ::steps (s/coll-of ::step))

(s/def ::tool_calls_made nat-int?)

(s/def ::delegate-result
  (s/keys :req-un [::status ::result ::steps ::tool_calls_made]))

;; ============================================================
;; Swarm Event Specs
;; ============================================================

(s/def ::event-type
  #{:started :progress :completed :error :blocked})

(s/def ::event-type-string
  #{"started" "progress" "completed" "error" "blocked"})

(s/def ::event-type-any
  (s/or :keyword ::event-type
        :string ::event-type-string))

;; Additional data payload (flexible map)
(s/def ::data (s/nilable map?))

(s/def ::timestamp pos-int?)

;; ============================================================
;; Shout Payload Spec
;; ============================================================

(s/def ::shout-payload
  (s/keys :req-un [::event-type]
          :opt-un [::task ::message ::data]))

;; Full shout event as stored in registry
(s/def ::shout-event
  (s/keys :req-un [::event-type ::timestamp]
          :opt-un [::task ::message ::data]))

;; ============================================================
;; Ask Payload Spec
;; ============================================================

(s/def ::question ::non-empty-string)

(s/def ::option ::non-empty-string)
(s/def ::options (s/nilable (s/coll-of ::option)))

(s/def ::ask-payload
  (s/keys :req-un [::question]
          :opt-un [::options ::timeout-ms]))

;; Ask event as broadcast
(s/def ::ask-event
  (s/keys :req-un [::ask-id ::agent-id ::question ::timestamp]
          :opt-un [::options]))

(s/def ::ask-id ::non-empty-string)

;; Ask response
(s/def ::decision string?)
(s/def ::by string?)
(s/def ::timeout boolean?)

(s/def ::ask-response
  (s/or :response (s/keys :req-un [::decision ::by])
        :timeout (s/keys :req-un [::timeout])))

;; ============================================================
;; Agent Registry Specs
;; ============================================================

(s/def ::last-seen ::timestamp)
(s/def ::current-task (s/nilable string?))
(s/def ::messages (s/coll-of ::shout-event))

(s/def ::agent-state
  (s/keys :req-un [::status ::last-seen]
          :opt-un [::current-task ::messages]))

(s/def ::agent-registry
  (s/map-of ::agent-id ::agent-state))

;; ============================================================
;; Hivemind Status Spec
;; ============================================================

(s/def ::agents ::agent-registry)
(s/def ::pending-asks (s/coll-of ::ask-event))
(s/def ::channel-connected boolean?)
(s/def ::ws-connected boolean?)
(s/def ::ws-clients nat-int?)

(s/def ::pending-swarm-prompt
  (s/keys :req-un [::slave-id ::prompt ::timestamp]
          :opt-un [::session-id ::received-at]))

(s/def ::slave-id ::non-empty-string)
(s/def ::prompt ::non-empty-string)
(s/def ::session-id (s/nilable string?))
(s/def ::received-at ::timestamp)

(s/def ::pending-swarm-prompts (s/coll-of ::pending-swarm-prompt))

(s/def ::hivemind-status
  (s/keys :req-un [::agents ::pending-asks ::pending-swarm-prompts
                   ::channel-connected ::ws-connected]
          :opt-un [::ws-clients]))

;; ============================================================
;; Function Specs (fdef)
;; ============================================================

;; delegate! : opts-map -> result-map
(s/fdef hive-mcp.agent/delegate!
  :args (s/cat :opts ::delegate-request)
  :ret ::delegate-result)

;; shout! : agent-id event-type data -> boolean
(s/fdef hive-mcp.hivemind/shout!
  :args (s/cat :agent-id ::agent-id
               :event-type ::event-type
               :data ::shout-payload)
  :ret boolean?)

;; ask! : agent-id question options & opts -> response
(s/fdef hive-mcp.hivemind/ask!
  :args (s/cat :agent-id ::agent-id
               :question ::question
               :options ::options
               :opts (s/keys* :opt-un [::timeout-ms]))
  :ret ::ask-response)

;; respond-ask! : ask-id decision & opts -> boolean
(s/fdef hive-mcp.hivemind/respond-ask!
  :args (s/cat :ask-id ::ask-id
               :decision ::decision
               :opts (s/keys* :opt-un [::by]))
  :ret boolean?)

;; get-status : -> status-map
(s/fdef hive-mcp.hivemind/get-status
  :args (s/cat)
  :ret ::hivemind-status)

;; get-agent-messages : agent-id -> [messages] | nil
(s/fdef hive-mcp.hivemind/get-agent-messages
  :args (s/cat :agent-id ::agent-id)
  :ret (s/nilable ::messages))

;; clear-agent! : agent-id -> nil
(s/fdef hive-mcp.hivemind/clear-agent!
  :args (s/cat :agent-id ::agent-id)
  :ret nil?)

;; ============================================================
;; Helper Functions
;; ============================================================

(defn valid-delegate-request?
  "Check if opts is a valid delegate request."
  [opts]
  (s/valid? ::delegate-request opts))

(defn explain-delegate-request
  "Explain why opts is not a valid delegate request."
  [opts]
  (s/explain-data ::delegate-request opts))

(defn valid-delegate-result?
  "Check if result is a valid delegate result."
  [result]
  (s/valid? ::delegate-result result))

(defn explain-delegate-result
  "Explain why result is not a valid delegate result."
  [result]
  (s/explain-data ::delegate-result result))

(defn valid-shout-payload?
  "Check if payload is a valid shout payload."
  [payload]
  (s/valid? ::shout-payload payload))

(defn explain-shout-payload
  "Explain why payload is not a valid shout payload."
  [payload]
  (s/explain-data ::shout-payload payload))

(defn valid-ask-payload?
  "Check if payload is a valid ask payload."
  [payload]
  (s/valid? ::ask-payload payload))

(defn explain-ask-payload
  "Explain why payload is not a valid ask payload."
  [payload]
  (s/explain-data ::ask-payload payload))

(defn valid-backend?
  "Check if backend is a valid backend keyword."
  [backend]
  (s/valid? ::backend backend))

(defn valid-task-type?
  "Check if task-type is a valid task type keyword."
  [task-type]
  (s/valid? ::task-type task-type))

(defn valid-event-type?
  "Check if event-type is a valid event type keyword."
  [event-type]
  (s/valid? ::event-type event-type))

(defn valid-hivemind-status?
  "Check if status is a valid hivemind status map."
  [status]
  (s/valid? ::hivemind-status status))

(defn explain-hivemind-status
  "Explain why status is not a valid hivemind status."
  [status]
  (s/explain-data ::hivemind-status status))

;; ============================================================
;; Generators for test.check
;; ============================================================

(defn backend-gen
  "Generator for backend keywords."
  []
  (gen/elements [:ollama :openrouter]))

(defn task-type-gen
  "Generator for task type keywords."
  []
  (gen/elements [:coding :coding-alt :arch :docs]))

(defn event-type-gen
  "Generator for event type keywords."
  []
  (gen/elements [:started :progress :completed :error :blocked]))

(defn model-gen
  "Generator for model identifier strings."
  []
  (gen/elements ["devstral-small:24b"
                 "mistralai/devstral-2512:free"
                 "qwen/qwen-2.5-coder-32b-instruct:free"
                 "google/gemma-3-27b-it:free"]))

(defn preset-gen
  "Generator for preset names."
  []
  (gen/elements ["tdd" "reviewer" "documenter" "clarity" "hivemind"]))

(defn agent-id-gen
  "Generator for agent identifiers."
  []
  (gen/fmap #(str "agent-" %)
            (gen/string-alphanumeric)))

(defn delegate-request-gen
  "Generator for delegate request maps."
  []
  (gen/hash-map
   :task (gen/fmap #(str "Task: " %) (gen/string-alphanumeric))
   :backend (backend-gen)
   :max-steps (gen/choose 5 50)))
