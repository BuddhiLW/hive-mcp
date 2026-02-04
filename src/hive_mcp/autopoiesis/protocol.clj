(ns hive-mcp.autopoiesis.protocol
  "Protocol definition for autopoietic (self-creating) knowledge systems.
   
   Autopoiesis = self-creation. The system observes events, evaluates trust,
   detects emergent patterns, learns from feedback, and decays stale knowledge.

   CLARITY-L: Layers stay pure - this is the port (interface).
   Implementation lives in proprietary hive-knowledge repo.
   
   CLARITY-I: Inputs guarded at protocol boundary.
   
   See also:
   - hive-mcp.autopoiesis.schema for Malli schemas
   - hive-mcp.autopoiesis.basic for no-op fallback implementation")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IAutopoiesis
  "Protocol for autopoietic (self-creating) knowledge systems.
   
   Autopoiesis (from Greek auto- 'self' + poiesis 'creation') describes systems
   that maintain and reproduce themselves. In hive-mcp, this means knowledge that:
   
   1. **Observes** - Records events from agent interactions
   2. **Trusts** - Evaluates reliability of knowledge entries
   3. **Emerges** - Detects patterns across observations
   4. **Learns** - Updates models from explicit feedback
   5. **Decays** - Applies staleness to outdated knowledge
   
   Implementations:
   - BasicAutopoiesis: No-op fallback (hive-mcp, open source)
   - AdvancedAutopoiesis: GNN-powered emergence (hive-knowledge, proprietary)"

  (observe [this event]
    "Record an observation event from agent interactions.
     
     event - Map conforming to :autopoiesis/observation-event schema:
       {:event-type   - :memory-access | :pattern-match | :feedback | :decay
        :timestamp    - Instant when event occurred
        :data         - Event-specific payload
        :source       - Origin (e.g., \"memory-query\", \"crystallization\")
        :agent-id     - Agent that triggered the event}
     
     Returns map with observation result:
       {:observed    - boolean, whether observation was recorded
        :event-id    - string, unique ID for this observation (if recorded)
        :reason      - string, explanation if not observed}")

  (trust? [this entry]
    "Compute trust score for a knowledge entry.
     
     Trust is computed from multiple signals:
     - Access patterns (frequently accessed = more trustworthy)
     - Feedback history (helpful ratings increase trust)
     - Source reliability (axioms > decisions > notes)
     - Age and staleness (recent knowledge has higher baseline trust)
     - Grounding status (grounded entries have higher trust)
     
     entry - Memory entry map with at least :id key
     
     Returns double in range [0.0, 1.0]:
       0.0 = completely untrustworthy (should be expired)
       0.5 = neutral (no signal either way)
       1.0 = highly trustworthy (core knowledge)")

  (emerge [this scope]
    "Detect emergent patterns in the given scope.
     
     Emergence detection uses multiple levels:
     - L0: Hash-based exact matching
     - L1: Weisfeiler-Leman graph kernel similarity
     - L2: GNN structural embeddings
     - L3: Cross-project pattern synthesis
     
     scope - String identifying the scope to analyze:
       - nil or \"global\" for all knowledge
       - \"project:<id>\" for project-specific
       - \"session:<id>\" for session-specific
     
     Returns vector of pattern candidates:
       [{:pattern-id   - Unique identifier
         :pattern-type - :cluster | :sequence | :hierarchy | :anomaly
         :confidence   - Trust score [0.0, 1.0]
         :evidence     - Vector of entry IDs supporting pattern
         :scope        - Scope where pattern was detected}]")

  (learn [this feedback]
    "Update internal models from explicit feedback.
     
     Learning integrates:
     - Helpfulness ratings from agents/users
     - Pattern confirmations/rejections
     - Explicit corrections to knowledge
     
     feedback - Map conforming to :autopoiesis/feedback schema:
       {:entry-id  - ID of the entry being rated
        :rating    - :helpful | :unhelpful | :correct | :incorrect
        :context   - Optional context about the feedback
        :agent-id  - Agent providing feedback}
     
     Returns learning report:
       {:learned       - boolean, whether learning occurred
        :model-updates - int, number of model parameters updated
        :new-patterns  - int, patterns discovered from this feedback
        :reason        - string, explanation if not learned}")

  (decay [this]
    "Apply staleness decay to all knowledge.
     
     Decay process:
     1. Check all entries against their TTL
     2. Demote entries with low trust scores
     3. Promote entries with high engagement
     4. Expire entries past their lifetime
     
     Should be called periodically (e.g., daily cron, session end).
     
     Returns decay report:
       {:decayed-count  - int, entries that had staleness increased
        :promoted-count - int, entries promoted to longer duration
        :expired-count  - int, entries removed due to TTL
        :timestamp      - Instant when decay was applied}"))

;; =============================================================================
;; Active Autopoiesis Management
;; =============================================================================

;; Atom holding the currently active IAutopoiesis implementation.
(defonce ^:private active-autopoiesis (atom nil))

(defn set-autopoiesis!
  "Set the active autopoiesis implementation.
   Called during system initialization.
   
   autopoiesis - Must satisfy IAutopoiesis protocol"
  [autopoiesis]
  {:pre [(satisfies? IAutopoiesis autopoiesis)]}
  (reset! active-autopoiesis autopoiesis))

(defn get-autopoiesis
  "Get the active autopoiesis implementation.
   Throws if no implementation has been set."
  []
  (or @active-autopoiesis
      (throw (ex-info "No autopoiesis engine configured. Call set-autopoiesis! first."
                      {:hint "Initialize with basic-autopoiesis or load hive-knowledge"}))))

(defn autopoiesis-set?
  "Check if an autopoiesis implementation has been configured."
  []
  (some? @active-autopoiesis))

(defn reset-autopoiesis!
  "Reset the active autopoiesis to nil.
   Used for testing and system shutdown."
  []
  (reset! active-autopoiesis nil))
