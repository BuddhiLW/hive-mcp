(ns hive-mcp.events.schemas
  "Malli schemas for event system validation.
   
   CLARITY Principle: Inputs are guarded at boundaries.
   
   This module provides validation at key system boundaries:
   - Event dispatch (validate event format)
   - Interceptor registration (validate structure)
   - Effect data (validate effect payloads)
   
   Usage:
   ```clojure
   (require '[hive-mcp.events.schemas :as schemas])
   (schemas/validate-event! [:task/complete {:id \"123\"}])
   ```"
  (:require [malli.core :as m]
            [malli.error :as me]))

;; =============================================================================
;; Event Schemas
;; =============================================================================

(def Event
  "Event schema: vector with keyword first element followed by any data.
   
   Examples:
   - [:task/complete {:id \"123\"}]
   - [:shout {:event-type :progress :message \"Working...\"}]
   - [:noop]"
  [:and
   vector?
   [:cat :keyword [:* :any]]])

;; =============================================================================
;; Interceptor Schemas
;; =============================================================================

(def Interceptor
  "Interceptor schema: map with required :id and optional :before/:after fns.
   
   The :before and :after fields should be functions, but we validate
   them loosely as fn? doesn't work with malli's registry by default."
  [:map
   [:id :keyword]
   [:before {:optional true} [:fn fn?]]
   [:after {:optional true} [:fn fn?]]
   [:comment {:optional true} :string]])

;; =============================================================================
;; Context Schemas
;; =============================================================================

(def Coeffects
  "Coeffects map: inputs to the event handler.
   
   Always contains :event and :original-event, may contain additional
   injected coeffects like :now, :db, etc."
  [:map
   [:event {:optional true} Event]
   [:original-event {:optional true} Event]])

(def Effects
  "Effects map: outputs from the event handler.
   
   Keys are effect IDs, values are effect-specific data."
  [:map-of :keyword :any])

(def Context
  "Interceptor execution context.
   
   Flows through the interceptor chain, carrying coeffects, effects,
   and the queue/stack for execution."
  [:map
   [:coeffects Coeffects]
   [:effects Effects]
   [:queue [:vector :any]]
   [:stack [:vector :any]]])

;; =============================================================================
;; Effect Data Schemas
;; =============================================================================

(def ShoutEffectData
  "Data shape for :shout effect.
   
   Used to broadcast to the hivemind coordinator."
  [:map
   [:event-type :keyword]
   [:agent-id {:optional true} :string]
   [:data {:optional true} :map]
   [:message {:optional true} :string]
   [:task {:optional true} :string]])

(def LogEffectData
  "Data shape for :log effect."
  [:or
   :string
   [:map
    [:level {:optional true} [:enum :debug :info :warn :error]]
    [:message :string]]])

(def DsTransactEffectData
  "Data shape for :ds-transact effect.
   
   Must be a valid DataScript transaction - vector of operations."
  [:vector :any])

;; =============================================================================
;; Validation Functions
;; =============================================================================

(defn validate-event!
  "Validate an event vector. Throws ex-info on invalid format.
   
   Args:
   - event: The event vector to validate
   
   Returns: event (for threading)
   Throws: ExceptionInfo with :event and :error keys"
  [event]
  (when-not (m/validate Event event)
    (throw (ex-info "Invalid event format: event must be a vector with keyword first"
                    {:event event
                     :error (me/humanize (m/explain Event event))})))
  event)

(defn validate-interceptor!
  "Validate an interceptor map. Throws ex-info on invalid format.
   
   Args:
   - interceptor: The interceptor map to validate
   
   Returns: interceptor (for threading)
   Throws: ExceptionInfo with :interceptor and :error keys"
  [interceptor]
  (when-not (m/validate Interceptor interceptor)
    (throw (ex-info "Invalid interceptor format: must be map with :id keyword"
                    {:interceptor interceptor
                     :error (me/humanize (m/explain Interceptor interceptor))})))
  interceptor)

(defn validate-context!
  "Validate an execution context. Throws ex-info on invalid format.
   
   Args:
   - context: The context map to validate
   
   Returns: context (for threading)
   Throws: ExceptionInfo with :context and :error keys"
  [context]
  (when-not (m/validate Context context)
    (throw (ex-info "Invalid context format"
                    {:context (select-keys context [:coeffects :effects])
                     :error (me/humanize (m/explain Context context))})))
  context)

(defn valid-event?
  "Check if event is valid without throwing.
   
   Returns: boolean"
  [event]
  (m/validate Event event))

(defn valid-interceptor?
  "Check if interceptor is valid without throwing.
   
   Returns: boolean"
  [interceptor]
  (m/validate Interceptor interceptor))

(defn explain-event
  "Get human-readable explanation of why event is invalid.
   
   Returns: nil if valid, humanized error otherwise"
  [event]
  (when-not (m/validate Event event)
    (me/humanize (m/explain Event event))))

(defn explain-interceptor
  "Get human-readable explanation of why interceptor is invalid.
   
   Returns: nil if valid, humanized error otherwise"
  [interceptor]
  (when-not (m/validate Interceptor interceptor)
    (me/humanize (m/explain Interceptor interceptor))))
