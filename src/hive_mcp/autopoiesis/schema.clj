(ns hive-mcp.autopoiesis.schema
  "Malli schemas for autopoiesis types.
   
   Defines the data shapes for observations, trust scores, patterns,
   feedback, and reports used by the IAutopoiesis protocol.
   
   CLARITY-I: Inputs guarded - all protocol inputs should be validated
   against these schemas at boundaries.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(require '[malli.core :as m])

;; =============================================================================
;; Base Types
;; =============================================================================

(def TrustScore
  "Trust score in range [0.0, 1.0].
   0.0 = untrustworthy, 0.5 = neutral, 1.0 = highly trusted."
  [:double {:min 0.0 :max 1.0}])

(def EventType
  "Types of observation events."
  [:enum :memory-access :pattern-match :feedback :decay
   :crystallization :grounding :query :mutation])

(def PatternType
  "Types of emergent patterns."
  [:enum :cluster :sequence :hierarchy :anomaly
   :cross-project :temporal :structural])

(def FeedbackRating
  "Feedback rating values."
  [:enum :helpful :unhelpful :correct :incorrect])

(def DurationCategory
  "Memory duration categories."
  [:enum :ephemeral :short :medium :long :permanent])

;; =============================================================================
;; Observation Events
;; =============================================================================

(def ObservationEvent
  "Schema for observation events recorded by the autopoiesis system."
  [:map
   [:event-type EventType]
   [:timestamp {:optional true} inst?]
   [:data {:optional true} :map]
   [:source {:optional true} :string]
   [:agent-id {:optional true} :string]
   [:project-id {:optional true} :string]
   [:session-id {:optional true} :string]])

(def ObservationResult
  "Schema for observation result."
  [:map
   [:observed :boolean]
   [:event-id {:optional true} :string]
   [:reason {:optional true} :string]])

;; =============================================================================
;; Pattern Detection
;; =============================================================================

(def PatternCandidate
  "Schema for emergent pattern candidates."
  [:map
   [:pattern-id :string]
   [:pattern-type PatternType]
   [:confidence TrustScore]
   [:evidence {:optional true} [:vector :string]]
   [:scope {:optional true} :string]
   [:metadata {:optional true} :map]])

(def EmergenceLevel
  "Emergence detection levels (L0-L3)."
  [:enum :l0-hash :l1-wl-kernel :l2-gnn :l3-synthesis])

;; =============================================================================
;; Feedback
;; =============================================================================

(def Feedback
  "Schema for explicit feedback on knowledge entries."
  [:map
   [:entry-id :string]
   [:rating FeedbackRating]
   [:context {:optional true} :string]
   [:agent-id {:optional true} :string]
   [:timestamp {:optional true} inst?]])

(def LearningReport
  "Schema for learning operation report."
  [:map
   [:learned :boolean]
   [:model-updates {:optional true} :int]
   [:new-patterns {:optional true} :int]
   [:timestamp {:optional true} inst?]
   [:reason {:optional true} :string]])

;; =============================================================================
;; Decay
;; =============================================================================

(def DecayReport
  "Schema for decay operation report."
  [:map
   [:decayed-count :int]
   [:promoted-count :int]
   [:expired-count :int]
   [:timestamp {:optional true} inst?]
   [:details {:optional true}
    [:map
     [:decayed-entries {:optional true} [:vector :string]]
     [:promoted-entries {:optional true} [:vector :string]]
     [:expired-entries {:optional true} [:vector :string]]]]])

;; =============================================================================
;; Trust Computation
;; =============================================================================

(def TrustFactors
  "Factors used in trust computation."
  [:map
   [:access-frequency {:optional true} :double]
   [:feedback-score {:optional true} :double]
   [:source-reliability {:optional true} :double]
   [:age-factor {:optional true} :double]
   [:grounding-bonus {:optional true} :double]])

(def TrustResult
  "Schema for trust computation result with breakdown."
  [:map
   [:score TrustScore]
   [:factors {:optional true} TrustFactors]
   [:computed-at {:optional true} inst?]])

;; =============================================================================
;; Validation Helpers
;; =============================================================================

(defn valid-observation?
  "Check if event conforms to ObservationEvent schema."
  [event]
  (m/validate ObservationEvent event))

(defn valid-feedback?
  "Check if feedback conforms to Feedback schema."
  [feedback]
  (m/validate Feedback feedback))

(defn valid-pattern?
  "Check if pattern conforms to PatternCandidate schema."
  [pattern]
  (m/validate PatternCandidate pattern))
