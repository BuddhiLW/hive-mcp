(ns hive-mcp.agent.drone.kg-session
  "KG-compressed context reconstruction for agent communication.

   Delegates to hive-knowledge (proprietary). Returns noop when not available.

   This is a thin stub that attempts to load the real implementation from
   hive-knowledge.context-reconstruction via requiring-resolve. If hive-knowledge
   is not on the classpath, all functions gracefully degrade to safe noop results
   that pass messages through uncompressed.

   Architecture (when hive-knowledge available):
   - Session KG: DataScript in-memory per agent session
   - Each turn: assistant response + tool results -> KG extraction
   - Next turn: reconstruct context from KG (not raw messages)
   - On completion: promote valuable nodes to global KG
   - ~25x compression vs raw history

   Noop fallback (when hive-knowledge NOT available):
   - create-session-kg!     -> returns nil session (no-op)
   - compress-turn!         -> returns 0 (no compression)
   - reconstruct-context    -> returns empty string
   - build-compressed-messages -> returns original messages unchanged
   - promote-to-global!     -> returns {:promoted 0}
   - close-session!         -> returns empty stats

   Integration point: hive-mcp.agent.loop/run-loop

   Decision ref: 20260206235801-2b6fb27a (In-Process Agentic Drone Loop)
   Decision ref: 20260207000937-72d57fa1 (Pass-by-Reference Communication)
   IP boundary: L3+ algorithms in hive-knowledge (proprietary)"
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Session KG Schema (Open — used by both stub and hive-knowledge)
;; =============================================================================

(def session-schema
  "DataScript schema for session KG nodes.
   Lightweight schema — no need for full KG edge schema here.
   Session nodes are ephemeral and get promoted post-completion.

   This schema is open (AGPL) as it defines the data contract.
   The algorithms that populate/query it are in hive-knowledge."
  {:node/id         {:db/unique :db.unique/identity
                     :db/doc "Unique node ID within session"}
   :node/type       {:db/doc "Node type: :observation :action :discovery :decision :goal-state"}
   :node/content    {:db/doc "Compact text summary of the node (not raw content)"}
   :node/turn       {:db/doc "Turn number when this node was created"}
   :node/timestamp  {:db/doc "Creation timestamp (epoch ms)"}
   :node/source     {:db/doc "Source tool or message type that produced this node"}
   :node/files      {:db/cardinality :db.cardinality/many
                     :db/doc "Files referenced by this node"}
   :node/superseded {:db/doc "If true, this node was superseded by a later observation"}
   :node/importance {:db/doc "Importance score 0.0-1.0 for reconstruction priority"}
   :node/tags       {:db/cardinality :db.cardinality/many
                     :db/doc "Tags for categorization and filtering"}})

;; =============================================================================
;; Node Types (Open — part of the data contract)
;; =============================================================================

(def node-types
  "Valid node types for session KG.
   - :observation  — What was seen (file contents, search results)
   - :action       — What was done (tool calls, mutations)
   - :discovery    — Key findings (patterns, bugs, architecture)
   - :decision     — Choices made (approach, tradeoffs)
   - :goal-state   — Current progress toward task goal"
  #{:observation :action :discovery :decision :goal-state})

;; =============================================================================
;; Dynamic Resolution Helpers
;; =============================================================================

(defn- try-resolve
  "Attempt to resolve a symbol from hive-knowledge.context-reconstruction.
   Returns the var if available, nil otherwise."
  [sym-name]
  (try
    (requiring-resolve (symbol "hive-knowledge.context-reconstruction" sym-name))
    (catch Exception _
      nil)))

(defn- delegate-or-noop
  "Try to delegate to hive-knowledge fn, fall back to default value."
  [fn-name default-val args]
  (if-let [f (try-resolve fn-name)]
    (apply f args)
    (do
      (log/debug "hive-knowledge not available, returning noop for" fn-name)
      default-val)))

;; =============================================================================
;; Noop Fallback Values
;; =============================================================================

(def ^:private noop-session
  "Noop session — nil-safe placeholder when hive-knowledge is not available."
  nil)

(def ^:private noop-stats
  {:turns-compressed 0
   :tokens-saved 0
   :compression-ratio "N/A (noop)"
   :nodes-created 0
   :nodes-active 0
   :nodes-superseded 0
   :raw-tokens 0
   :compressed-tokens 0})

;; =============================================================================
;; Public API — delegates to hive-knowledge or returns noop
;; =============================================================================

(defn create-session-kg!
  "Create a new session KG for an agent execution.
   Delegates to hive-knowledge (proprietary). Returns nil when not available.

   Arguments:
     agent-id — Agent identifier string
     task     — Original task description string

   Returns:
     Session KG map (opaque to caller) or nil if hive-knowledge not available."
  [agent-id task]
  (delegate-or-noop "create-session-kg!" noop-session
                    [agent-id task]))

(defn compress-turn!
  "Compress a turn's messages into session KG nodes.
   Delegates to hive-knowledge (proprietary). Returns 0 when not available.

   Called by the agent loop after each tool execution step.
   Extracts meaning from messages, stores as KG nodes, and
   supersedes outdated observations.

   Arguments:
     session  — Session KG map from create-session-kg! (nil-safe)
     messages — Vector of messages from this turn

   Returns:
     Number of nodes created this turn (0 if noop)."
  [session messages]
  (if session
    (delegate-or-noop "compress-turn!" 0 [session messages])
    0))

(defn reconstruct-context
  "Reconstruct a compact context prompt from the session KG.
   Delegates to hive-knowledge (proprietary). Returns empty string when not available.

   Called before each LLM call to replace the full message history
   with a compressed representation (~200-300 tokens vs thousands).

   Arguments:
     session — Session KG map from create-session-kg! (nil-safe)
     opts    — Optional map with :max-tokens, :recency-bias

   Returns:
     String — compact context prompt, or empty string if noop."
  [session & [opts]]
  (if session
    (delegate-or-noop "reconstruct-context" ""
                      (if opts [session opts] [session]))
    ""))

(defn build-compressed-messages
  "Build the message array for the next LLM call using KG compression.
   Delegates to hive-knowledge (proprietary).

   Noop fallback: returns original messages unchanged (no compression).

   Instead of passing the full conversation history, builds:
   1. System prompt (unchanged)
   2. KG-reconstructed context as a user message
   3. Only the most recent tool results (current turn)

   Arguments:
     session       — Session KG map (nil-safe)
     system-prompt — Original system prompt string
     all-messages  — Full message history (returned as-is if noop)
     recent-msgs   — Messages from the most recent turn only
     opts          — Optional reconstruction options

   Returns:
     Vector of messages ready for LLM call."
  [session system-prompt all-messages recent-msgs & [opts]]
  (if session
    (delegate-or-noop "build-compressed-messages" all-messages
                      (if opts
                        [session system-prompt recent-msgs opts]
                        [session system-prompt recent-msgs]))
    ;; Noop: return all messages unchanged (no compression)
    all-messages))

(defn promote-to-global!
  "Promote valuable session KG nodes to the global knowledge graph.
   Delegates to hive-knowledge (proprietary). Returns {:promoted 0} when not available.

   Called after successful task completion.

   Arguments:
     session      — Session KG map (nil-safe)
     global-store — IKGStore instance (global KG)
     opts         — Optional map with :threshold, :scope

   Returns:
     {:promoted N :edges-created N}"
  [session global-store & [opts]]
  (if session
    (delegate-or-noop "promote-to-global!" {:promoted 0 :edges-created 0}
                      (if opts [session global-store opts] [session global-store]))
    {:promoted 0 :edges-created 0}))

(defn promotable-nodes
  "Get nodes from session KG worth promoting to global KG.
   Delegates to hive-knowledge (proprietary). Returns [] when not available.

   Arguments:
     session — Session KG map (nil-safe)
     opts    — Optional map with :threshold

   Returns:
     Vector of node maps suitable for global KG storage."
  [session & [opts]]
  (if session
    (delegate-or-noop "promotable-nodes" []
                      (if opts [session opts] [session]))
    []))

(defn session-stats
  "Get compression statistics for the session.
   Delegates to hive-knowledge (proprietary). Returns noop stats when not available.

   Returns:
     Map with :turns-compressed, :tokens-saved, :compression-ratio, etc."
  [session]
  (if session
    (delegate-or-noop "session-stats" noop-stats [session])
    noop-stats))

(defn close-session!
  "Close a session KG and release resources.
   Delegates to hive-knowledge (proprietary). Returns noop stats when not available.

   Returns final stats."
  [session]
  (if session
    (delegate-or-noop "close-session!" noop-stats [session])
    noop-stats))

;; =============================================================================
;; Availability Check
;; =============================================================================

(defn compression-available?
  "Check if KG-compressed context reconstruction is available.
   Returns true if hive-knowledge.context-reconstruction is on the classpath."
  []
  (boolean (try-resolve "create-session-kg!")))
