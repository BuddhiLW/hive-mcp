(ns hive-mcp.protocols.autopoiesis
  "Protocol definitions for self-creating knowledge systems.

   IAutopoiesis: Interface between hive-mcp core and optional extensions.
   Defines a system that creates and maintains itself - the defining
   characteristic of living systems (Maturana & Varela, 1972).

   The protocol enables systems that:
   1. Observe - query with awareness of their own state
   2. Learn - extract knowledge from exploration
   3. Emerge - detect patterns not explicitly taught
   4. Decay - apply entropy, prune what's no longer valid
   5. Cross-pollinate - transfer learning across boundaries
   6. Self-modify - adapt own parameters based on performance
   7. Track adaptation - maintain history and score of self-modifications

   IIntrospect: Companion protocol for explaining system behavior.
   Enables transparency, debuggability, and health monitoring of
   autopoietic processes.

   SOLID-I: Interface segregation - autopoietic operations only.
   SOLID-D: Depend on abstraction, not specific implementations.
   CLARITY-L: Layers stay pure - protocol is the boundary between
              core hive-mcp and knowledge enhancement implementations.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IAutopoiesis Protocol (Self-Creating Knowledge Systems)
;;; ============================================================================

(defprotocol IAutopoiesis
  "Protocol for self-creating knowledge systems.

   Autopoiesis (Greek: auto = self, poiesis = creation/production) describes
   systems that create and maintain themselves. This protocol defines the
   interface for knowledge systems that can:

   - Observe their environment with structural awareness
   - Learn from territory exploration
   - Emerge new patterns from existing knowledge
   - Decay stale knowledge via entropy
   - Cross-pollinate insights across boundaries
   - Self-modify parameters based on observed performance
   - Track and score adaptation effectiveness

   Implementations:
   - BasicAutopoiesis: No-op fallback (hive-mcp default)
   - EnhancedAutopoiesis: Full implementation (via extension)

   The protocol allows hive-mcp to function independently while enabling
   enhanced capabilities when extensions are available."

  (observe [this query opts]
    "Query with structural awareness and staleness checks.

     Arguments:
       query - Natural language or structured query
       opts  - Map with optional keys:
               :scope       - Project scope filter
               :types       - Entry types to include
               :max-staleness - Max staleness probability (0.0-1.0)
               :include-gaps? - Include entries with knowledge gaps
               :depth       - Traversal depth for related entries

     Returns:
       Map with:
       :entries    - Seq of matching entries with freshness scores
       :gaps       - Identified knowledge gaps
       :staleness  - Map of entry-id -> staleness probability
       :suggestions - Recommended follow-up queries

     The observe operation is awareness-augmented - it considers not just
     content match but the system's understanding of its own knowledge state.")

  (trust? [this entry]
    "Evaluate whether an entry should be trusted or needs re-grounding.

     Arguments:
       entry - Memory entry map

     Returns:
       Map with:
       :trustworthy? - Boolean: can we use this without verification?
       :confidence   - Confidence score (0.0-1.0)
       :reasons      - Vector of trust/distrust reasons
       :action       - Recommended action (:use, :verify, :reground, :discard)

     Trust evaluation considers:
     - Staleness probability (Bayesian Beta model)
     - Source file hash changes
     - Transitive staleness from dependencies
     - Time since last grounding
     - Knowledge gaps present")

  (emerge [this scope]
    "Detect L3 (meta-pattern) knowledge from L2 (pattern) clusters.

     Arguments:
       scope - Project scope to analyze, or :global

     Returns:
       Map with:
       :patterns    - Seq of emerged pattern maps, each with:
                      :id, :type, :content, :confidence, :source-entries
       :clusters    - L2 clusters that generated patterns
       :novel?      - Boolean: are these genuinely new insights?

     Emergence is the core autopoietic operation - creating new knowledge
     that wasn't explicitly programmed or stored.")

  (cross-pollinate [this entry-id]
    "Find isomorphic patterns across project boundaries.

     Arguments:
       entry-id - Source entry to find analogues for

     Returns:
       Map with:
       :analogues   - Seq of related entries from other projects, each with:
                      :entry, :similarity, :isomorphism-type
       :transferable? - Boolean: can learning transfer?
       :adaptations - Suggested adaptations for cross-project use

     Cross-pollination enables learning from one domain to transfer to
     another, recognizing structural similarities despite surface differences.")

  (learn [this exploration-result]
    "Extract knowledge from territory exploration.

     Arguments:
       exploration-result - Map from code/file exploration:
                           :files-read - Files examined
                           :patterns-found - Code patterns observed
                           :context - Exploration context
                           :agent-id - Exploring agent

     Returns:
       Map with:
       :entries-created - IDs of new memory entries
       :entries-updated - IDs of entries with new evidence
       :gaps-filled     - Knowledge gaps that were resolved
       :new-gaps        - Newly identified knowledge gaps

     Learning is the intake mechanism - converting raw exploration
     into structured knowledge with proper abstraction levels.")

  (decay [this]
    "Apply entropy to knowledge, pruning stale entries.

     Arguments:
       None (operates on entire knowledge base)

     Returns:
       Map with:
       :decayed     - Count of entries with increased staleness
       :pruned      - Count of entries removed (staleness > threshold)
       :promoted    - Count of entries promoted (high evidence)
       :demoted     - Count of entries demoted (low evidence)

     Decay is essential for autopoiesis - without entropy, the system
     accumulates outdated knowledge and loses adaptability.")

  (self-modify! [this modification]
    "Apply a self-modification to the system's own parameters.

     Arguments:
       modification - Map describing the change:
                      :parameter  - Keyword identifying what to change
                                    (e.g. :decay-threshold, :trust-baseline,
                                     :emergence-sensitivity, :staleness-window)
                      :old-value  - Current value (for verification)
                      :new-value  - Proposed new value
                      :reason     - Why this modification is needed
                      :evidence   - Supporting observations/metrics

     Returns:
       Map with:
       :applied?    - Boolean: was the modification accepted?
       :parameter   - The parameter that was modified
       :old-value   - Previous value
       :new-value   - New value (may differ from proposed if bounded)
       :timestamp   - When the modification was applied
       :rollback-fn - Fn to undo this modification (or nil)

     Self-modification is the reflexive capability - the system adapting
     its own operating parameters based on observed performance.
     Implementations should bound modifications to safe ranges.")

  (adaptation-history [this opts]
    "Retrieve the history of self-modifications.

     Arguments:
       opts - Map with optional keys:
              :parameter - Filter by specific parameter keyword
              :since     - java.time.Instant, only show modifications after this
              :limit     - Max number of entries to return
              :include-rollbacks? - Include rolled-back modifications (default false)

     Returns:
       Map with:
       :modifications - Seq of modification records, each with:
                        :id, :parameter, :old-value, :new-value,
                        :reason, :timestamp, :rolled-back?
       :count         - Total number of modifications matching filter
       :active-params - Map of currently active parameter values

     Adaptation history enables auditing and understanding how the system
     has evolved its own parameters over time.")

  (adaptation-score [this]
    "Evaluate the effectiveness of recent self-modifications.

     Arguments:
       None (analyzes recent adaptation history)

     Returns:
       Map with:
       :score          - Overall adaptation effectiveness (0.0-1.0)
       :trend          - :improving, :stable, or :degrading
       :recent-mods    - Count of modifications in scoring window
       :effective-mods - Count that improved performance metrics
       :ineffective-mods - Count that degraded or had no effect
       :recommendations  - Seq of suggested parameter adjustments
       :confidence     - Confidence in the score (0.0-1.0)

     The adaptation score measures whether the system's self-modifications
     are actually improving its performance, enabling meta-adaptation
     (adapting how it adapts)."))

;;; ============================================================================
;;; IIntrospect Protocol (System Transparency)
;;; ============================================================================

(defprotocol IIntrospect
  "Protocol for explaining autopoietic system behavior.

   Enables transparency and debuggability by providing introspection
   capabilities. Essential for:
   - Understanding why the system made certain decisions
   - Debugging unexpected behavior
   - Auditing knowledge evolution
   - Building trust in system outputs

   Implementations should provide human-readable explanations
   at varying levels of detail."

  (explain [this entry-id]
    "Explain how an entry came to exist and its current state.

     Arguments:
       entry-id - Memory entry to explain

     Returns:
       Map with:
       :origin      - How the entry was created (:manual, :emerged, :learned)
       :evolution   - Timeline of changes with reasons
       :influences  - Entries that influenced this one
       :influenced  - Entries this one influenced
       :trust-chain - Path from grounded source to current abstraction
       :narrative   - Human-readable explanation string")

  (trace [this query result]
    "Trace how a query produced its result.

     Arguments:
       query  - The original query
       result - The result returned by observe

     Returns:
       Map with:
       :steps       - Seq of processing steps, each with:
                      :operation, :input, :output, :reason
       :filters     - Filters applied and their effects
       :scores      - How entries were scored/ranked
       :alternatives - Entries that almost matched but were excluded
       :narrative   - Human-readable trace explanation")

  (diff [this entry-id-a entry-id-b]
    "Compare two entries and explain their differences.

     Arguments:
       entry-id-a - First entry ID
       entry-id-b - Second entry ID

     Returns:
       Map with:
       :content-diff   - Structural diff of content
       :metadata-diff  - Diff of metadata fields
       :evolution-diff - How their histories diverged
       :semantic-diff  - Conceptual/meaning differences
       :relationship   - How they relate (:supersedes, :refines, :contradicts, :independent)
       :narrative      - Human-readable comparison")

  (observe-state [this]
    "Return a snapshot of the system's current operational state.

     Arguments:
       None

     Returns:
       Map with:
       :memory-count    - Total memory entries tracked
       :kg-node-count   - Knowledge graph node count
       :kg-edge-count   - Knowledge graph edge count
       :active-scopes   - Set of project scopes currently loaded
       :uptime-ms       - Milliseconds since system initialization
       :last-decay      - Timestamp of last decay cycle (or nil)
       :last-emergence  - Timestamp of last emergence run (or nil)
       :pending-ops     - Count of queued/in-flight operations
       :enhanced?       - Whether enhanced implementation is active

     Observe-state provides a non-intrusive health snapshot without
     triggering any side effects. Useful for monitoring dashboards.")

  (diagnose [this symptom]
    "Diagnose a reported symptom or anomaly in the knowledge system.

     Arguments:
       symptom - Map describing the issue:
                 :type     - Keyword: :slow-query, :stale-results,
                             :missing-entries, :trust-failure,
                             :emergence-stall, :decay-stuck,
                             :inconsistency, :other
                 :context  - Freeform context about when/where observed
                 :severity - :low, :medium, :high, :critical

     Returns:
       Map with:
       :diagnosis     - Keyword classification of root cause
       :explanation   - Human-readable explanation
       :contributing  - Seq of contributing factors
       :recommended   - Seq of recommended actions, each with:
                        :action, :priority, :estimated-impact
       :related-entries - Entry IDs relevant to the diagnosis

     Diagnose goes beyond explain/trace by actively investigating
     a reported problem, correlating symptoms with system state.")

  (health-report [this]
    "Generate a comprehensive health report for the knowledge system.

     Arguments:
       None

     Returns:
       Map with:
       :status         - Overall status: :healthy, :degraded, :unhealthy
       :checks         - Seq of health check results, each with:
                         :name, :status (:pass/:warn/:fail), :message, :metric
       :memory-health  - Map with :total, :stale-count, :stale-pct,
                         :avg-staleness, :gap-count
       :kg-health      - Map with :nodes, :edges, :orphans,
                         :avg-connectivity, :largest-component
       :adaptation     - Map with :score, :trend, :recent-mods
       :recommendations - Prioritized list of improvement actions
       :generated-at   - Timestamp of report generation

     Health-report aggregates observe-state, adaptation-score, and
     diagnostic checks into a single comprehensive view. Intended
     for periodic monitoring and operational awareness."))

;;; ============================================================================
;;; BasicAutopoiesis (No-Op Fallback Implementation)
;;; ============================================================================

(defrecord BasicAutopoiesis []
  IAutopoiesis

  (observe [_ _query _opts]
    ;; No-op: Return empty results with indicator that enhanced search unavailable
    {:entries []
     :gaps []
     :staleness {}
     :suggestions []
     :enhanced? false
     :message "BasicAutopoiesis: Enhanced observe not available. Use memory search directly."})

  (trust? [_ _entry]
    ;; No-op: Trust everything by default (no staleness analysis)
    {:trustworthy? true
     :confidence 0.5
     :reasons ["BasicAutopoiesis: No trust analysis available"]
     :action :use
     :enhanced? false})

  (emerge [_ _scope]
    ;; No-op: No pattern emergence without enhanced implementation
    {:patterns []
     :clusters []
     :novel? false
     :enhanced? false
     :message "BasicAutopoiesis: Pattern emergence requires enhanced extension."})

  (cross-pollinate [_ _entry-id]
    ;; No-op: No cross-project analysis
    {:analogues []
     :transferable? false
     :adaptations []
     :enhanced? false
     :message "BasicAutopoiesis: Cross-pollination requires enhanced extension."})

  (learn [_ _exploration-result]
    ;; No-op: No automatic learning extraction
    {:entries-created []
     :entries-updated []
     :gaps-filled []
     :new-gaps []
     :enhanced? false
     :message "BasicAutopoiesis: Automatic learning requires enhanced extension."})

  (decay [_]
    ;; No-op: No automatic decay/pruning
    {:decayed 0
     :pruned 0
     :promoted 0
     :demoted 0
     :enhanced? false
     :message "BasicAutopoiesis: Automatic decay requires enhanced extension."})

  (self-modify! [_ _modification]
    ;; No-op: No self-modification without enhanced implementation
    {:applied? false
     :parameter nil
     :old-value nil
     :new-value nil
     :timestamp (java.time.Instant/now)
     :rollback-fn nil
     :enhanced? false
     :message "BasicAutopoiesis: Self-modification requires enhanced extension."})

  (adaptation-history [_ _opts]
    ;; No-op: No adaptation tracking without enhanced implementation
    {:modifications []
     :count 0
     :active-params {}
     :enhanced? false
     :message "BasicAutopoiesis: Adaptation history requires enhanced extension."})

  (adaptation-score [_]
    ;; No-op: Return neutral score
    {:score 0.0
     :trend :stable
     :recent-mods 0
     :effective-mods 0
     :ineffective-mods 0
     :recommendations []
     :confidence 0.0
     :enhanced? false
     :message "BasicAutopoiesis: Adaptation scoring requires enhanced extension."}))

;;; ============================================================================
;;; BasicIntrospect (No-Op Fallback Implementation)
;;; ============================================================================

(defrecord BasicIntrospect []
  IIntrospect

  (explain [_ entry-id]
    ;; No-op: Return minimal explanation
    {:origin :unknown
     :evolution []
     :influences []
     :influenced []
     :trust-chain []
     :narrative (str "BasicIntrospect: No detailed explanation available for " entry-id
                     ". Enhanced introspection requires enhanced extension.")
     :enhanced? false})

  (trace [_ _query _result]
    ;; No-op: Return minimal trace
    {:steps []
     :filters []
     :scores {}
     :alternatives []
     :narrative "BasicIntrospect: Query tracing requires enhanced extension."
     :enhanced? false})

  (diff [_ entry-id-a entry-id-b]
    ;; No-op: Return minimal diff
    {:content-diff nil
     :metadata-diff nil
     :evolution-diff nil
     :semantic-diff nil
     :relationship :unknown
     :narrative (str "BasicIntrospect: Comparison between " entry-id-a " and " entry-id-b
                     " requires enhanced extension.")
     :enhanced? false})

  (observe-state [_]
    ;; No-op: Return minimal state snapshot
    {:memory-count 0
     :kg-node-count 0
     :kg-edge-count 0
     :active-scopes #{}
     :uptime-ms 0
     :last-decay nil
     :last-emergence nil
     :pending-ops 0
     :enhanced? false
     :message "BasicIntrospect: State observation requires enhanced extension."})

  (diagnose [_ _symptom]
    ;; No-op: Return empty diagnosis
    {:diagnosis :unavailable
     :explanation "BasicIntrospect: Diagnosis requires enhanced extension."
     :contributing []
     :recommended []
     :related-entries []
     :enhanced? false})

  (health-report [_]
    ;; No-op: Return minimal health report
    {:status :unknown
     :checks []
     :memory-health {:total 0 :stale-count 0 :stale-pct 0.0
                     :avg-staleness 0.0 :gap-count 0}
     :kg-health {:nodes 0 :edges 0 :orphans 0
                 :avg-connectivity 0.0 :largest-component 0}
     :adaptation {:score 0.0 :trend :stable :recent-mods 0}
     :recommendations []
     :generated-at (java.time.Instant/now)
     :enhanced? false
     :message "BasicIntrospect: Health reporting requires enhanced extension."}))

;;; ============================================================================
;;; Active Implementation Management
;;; ============================================================================

;; Atom holding the currently active IAutopoiesis implementation.
;; Defaults to BasicAutopoiesis (no-op fallback).
(defonce ^:private active-autopoiesis (atom nil))

;; Atom holding the currently active IIntrospect implementation.
(defonce ^:private active-introspect (atom nil))

(defn set-autopoiesis!
  "Set the active autopoiesis implementation.

   Arguments:
     impl - Implementation of IAutopoiesis protocol

   Returns:
     The implementation.

   Throws:
     AssertionError if impl doesn't satisfy protocol."
  [impl]
  {:pre [(satisfies? IAutopoiesis impl)]}
  (reset! active-autopoiesis impl)
  impl)

(defn get-autopoiesis
  "Get the active autopoiesis implementation.

   Returns BasicAutopoiesis if no enhanced implementation is set.
   This ensures hive-mcp always works, with or without enhanced extensions."
  []
  (or @active-autopoiesis
      (->BasicAutopoiesis)))

(defn autopoiesis-set?
  "Check if an enhanced autopoiesis implementation is configured.

   Returns:
     true if set-autopoiesis! has been called with non-basic impl."
  []
  (some? @active-autopoiesis))

(defn set-introspect!
  "Set the active introspect implementation.

   Arguments:
     impl - Implementation of IIntrospect protocol

   Returns:
     The implementation.

   Throws:
     AssertionError if impl doesn't satisfy protocol."
  [impl]
  {:pre [(satisfies? IIntrospect impl)]}
  (reset! active-introspect impl)
  impl)

(defn get-introspect
  "Get the active introspect implementation.

   Returns BasicIntrospect if no enhanced implementation is set."
  []
  (or @active-introspect
      (->BasicIntrospect)))

(defn introspect-set?
  "Check if an enhanced introspect implementation is configured."
  []
  (some? @active-introspect))

(defn clear-implementations!
  "Clear all active implementations. Used for testing.

   Returns nil."
  []
  (reset! active-autopoiesis nil)
  (reset! active-introspect nil)
  nil)

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defn enhanced?
  "Check if enhanced autopoiesis capabilities are available.

   Returns:
     true if a non-basic IAutopoiesis implementation is active."
  []
  (and (autopoiesis-set?)
       (not (instance? BasicAutopoiesis @active-autopoiesis))))

(defn capabilities
  "Get a summary of available autopoiesis capabilities.

   Returns:
     Map describing what features are available."
  []
  (let [introspect-enhanced? (and (introspect-set?)
                                  (not (instance? BasicIntrospect @active-introspect)))]
    {:autopoiesis-enhanced? (enhanced?)
     :introspect-enhanced? introspect-enhanced?
     ;; IAutopoiesis methods
     :observe? true  ;; Always available (may be no-op)
     :trust? true
     :emerge? (enhanced?)
     :cross-pollinate? (enhanced?)
     :learn? (enhanced?)
     :decay? (enhanced?)
     :self-modify? (enhanced?)
     :adaptation-history? (enhanced?)
     :adaptation-score? (enhanced?)
     ;; IIntrospect methods
     :explain? true  ;; Always available (may be no-op)
     :trace? (introspect-set?)
     :diff? (introspect-set?)
     :observe-state? true  ;; Always available (may be no-op)
     :diagnose? introspect-enhanced?
     :health-report? true  ;; Always available (may be no-op)
     }))
