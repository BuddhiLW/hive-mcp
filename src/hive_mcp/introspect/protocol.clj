(ns hive-mcp.introspect.protocol
  "IIntrospect - Provenance and explanation protocol.

   Provides the WHY behind knowledge - provenance tracking,
   derivation chains, and diff analysis for introspectable reasoning.

   CLARITY-L: Layers stay pure - this is the port (interface).
   Implementation lives in proprietary hive-logic repo.

   CLARITY-I: Inputs are guarded at protocol boundary.
   CLARITY-R: Represented intent - each method captures a distinct
              aspect of knowledge provenance.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IIntrospect
  "Protocol for introspectable reasoning systems.

   Provides the WHY behind knowledge - provenance tracking,
   derivation chains, and diff analysis.

   DDD: This is a domain port (interface) in the hexagonal architecture.
   Implementations are adapters that can be swapped for different
   reasoning backends (Datalog, Prolog, LLM-based, etc.).

   SOLID-D: Depend on abstractions (this protocol), not concretions
   (specific reasoning engine implementations)."

  (explain [this fact]
    "Explain why a fact exists. Returns derivation chain with sources.

     Arguments:
       fact - A fact identifier (keyword, string, or map with :id)

     Returns a map with:
       :fact       - The fact being explained
       :derivation - Vector of derivation steps, each with:
                     :source - Where this knowledge came from
                     :rule   - What rule/inference produced it (if derived)
                     :timestamp - When it was derived
       :confidence - Overall confidence score (0.0-1.0)
       :grounded   - Whether fact is grounded in original source

     Example:
       (explain introspector :user-is-admin)
       => {:fact :user-is-admin
           :derivation [{:source :memory-entry-123
                         :rule :role-based-access
                         :timestamp #inst \"2026-02-03\"}]
           :confidence 0.95
           :grounded true}")

  (trace [this query]
    "Trace the execution of a query. Returns step-by-step derivation.

     Arguments:
       query - A Datalog-style query or natural language query string

     Returns a map with:
       :query  - Original query
       :steps  - Vector of execution steps, each with:
                 :operation - What happened (:match, :filter, :join, :derive)
                 :data      - Data involved at this step
                 :cost      - Computational cost estimate
       :result - Final query result
       :stats  - Execution statistics (time, memory, etc.)

     Example:
       (trace introspector '[:find ?e :where [?e :type :convention]])
       => {:query [:find ?e :where [?e :type :convention]]
           :steps [{:operation :scan, :data :memory-store}...]
           :result #{1 2 3}
           :stats {:time-ms 12, :datoms-scanned 150}}")

  (diff [this v1 v2]
    "Compare two knowledge versions. Returns semantic diff.

     Arguments:
       v1 - First version (transaction ID, timestamp, or snapshot)
       v2 - Second version (transaction ID, timestamp, or snapshot)

     Returns a map with:
       :v1      - First version identifier
       :v2      - Second version identifier
       :added   - Facts present in v2 but not v1
       :removed - Facts present in v1 but not v2
       :changed - Facts that changed (with before/after values)
       :stats   - Summary statistics

     Example:
       (diff introspector #inst \"2026-02-01\" #inst \"2026-02-03\")
       => {:v1 #inst \"2026-02-01\"
           :v2 #inst \"2026-02-03\"
           :added [{:id :new-convention, :type :convention}...]
           :removed []
           :changed [{:id :old-decision, :before {...} :after {...}}]
           :stats {:added-count 5, :removed-count 0, :changed-count 1}}"))

;; =============================================================================
;; Active Introspector Management
;; =============================================================================

(defonce ^:private active-introspector (atom nil))

(defn set-introspector!
  "Set the active introspector implementation.
   Called during system initialization."
  [introspector]
  {:pre [(satisfies? IIntrospect introspector)]}
  (reset! active-introspector introspector))

(defn get-introspector
  "Get the active introspector.
   Returns nil if no introspector has been set (introspection not available)."
  []
  @active-introspector)

(defn introspector-set?
  "Check if an introspector has been configured."
  []
  (some? @active-introspector))
