(ns hive-mcp.hivemind.state
  "Hivemind state management — atoms and their direct accessors.

   Contains all mutable state for the hivemind coordination system:
   - pending-asks: Blocking ask/respond channels
   - agent-registry: Message history ring buffer (max 10 per agent)
   - pending-swarm-prompts: Permission prompts from Emacs slaves
   - ling-results: Completion results for coordinator review

   SOLID: SRP — state management only, no messaging or query logic.
   CLARITY: L — Pure accessor functions, side-effects limited to atom swaps."
  (:require [hive-mcp.guards :as guards]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State Atoms
;; =============================================================================

(defonce ^{:doc "Map of ask-id -> {:question ... :response-chan ...}"}
  pending-asks
  (atom {}))

(defonce ^{:doc "LEGACY: Message history storage. DataScript is source of truth for slave data.

                 ADR-002 AMENDED: This atom stores ONLY message history (ring buffer).
                 Slave metadata (status, name, presets, cwd) comes from DataScript.

                 Map of agent-id -> {:messages [...] :last-seen timestamp}
                 Messages is a vector of recent shouts (max 10 per agent).

                 Migration note: This was previously the authoritative agent registry.
                 Now DataScript (swarm/datascript.clj) is the source of truth for slaves.
                 This atom persists messages which are hivemind-specific (not swarm state)."}
  agent-registry
  (atom {}))

(defonce ^{:doc "Map of slave-id -> {:prompt :timestamp :session-id}
                 Permission prompts pushed from Emacs swarm slaves.
                 These are distinct from asks (agent-initiated questions)."}
  pending-swarm-prompts
  (atom {}))

;; Ling result tracking for coordinator review
(defonce ling-results (atom {}))

;; =============================================================================
;; Ling Results
;; =============================================================================

(defn record-ling-result!
  "Record ling completion for coordinator review."
  [agent-id result]
  (swap! ling-results assoc agent-id
         {:result result
          :timestamp (System/currentTimeMillis)
          :reviewed? false}))

(defn get-pending-ling-results
  "Get ling results awaiting coordinator review."
  []
  (->> @ling-results
       (filter (fn [[_ v]] (not (:reviewed? v))))
       (into {})))

(defn mark-ling-reviewed!
  "Mark a ling result as reviewed by coordinator."
  [agent-id]
  (swap! ling-results assoc-in [agent-id :reviewed?] true))

(defn clear-ling-results!
  "Clear all ling results (e.g., at session end)."
  []
  (reset! ling-results {}))

;; =============================================================================
;; Swarm Prompts
;; =============================================================================

(defn add-swarm-prompt!
  "Add a permission prompt from a swarm slave.
   Called by sync.clj when :prompt-shown event received from Emacs."
  [slave-id prompt-text session-id timestamp]
  (let [prompt-data {:prompt prompt-text
                     :timestamp timestamp
                     :session-id session-id
                     :received-at (System/currentTimeMillis)}]
    (swap! pending-swarm-prompts assoc slave-id prompt-data)
    (log/info "Swarm prompt received from" slave-id ":"
              (subs prompt-text 0 (min 50 (count prompt-text))))
    prompt-data))

(defn remove-swarm-prompt!
  "Remove a swarm prompt (after it's been answered)."
  [slave-id]
  (swap! pending-swarm-prompts dissoc slave-id)
  (log/debug "Swarm prompt cleared:" slave-id))

(defn get-swarm-prompts
  "Get all pending swarm prompts."
  []
  @pending-swarm-prompts)

;; =============================================================================
;; Agent Registry Utilities
;; =============================================================================

(defn clear-agent-registry!
  "Clear agent registry. GUARDED - no-op if coordinator running.

   CLARITY-Y: Yield safe failure - prevents test fixtures from
   corrupting production hivemind state."
  []
  (guards/when-not-coordinator
   "clear-agent-registry! called"
   (reset! agent-registry {})))
