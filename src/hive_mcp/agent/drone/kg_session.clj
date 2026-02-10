(ns hive-mcp.agent.drone.kg-session
  "Compressed context reconstruction for agent communication.

   Delegates to extension if available. Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, all functions gracefully degrade
   to safe noop results that pass messages through uncompressed.

   Noop fallback (when extension NOT available):
   - create-session-kg!     -> returns nil session (no-op)
   - compress-turn!         -> returns 0 (no compression)
   - reconstruct-context    -> returns empty string
   - build-compressed-messages -> returns original messages unchanged
   - promote-to-global!     -> returns {:promoted 0}
   - close-session!         -> returns empty stats

   Integration point: hive-mcp.agent.loop/run-loop"
  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Session Schema
;; =============================================================================

(def session-schema
  "DataScript schema for session nodes.
   Session nodes are ephemeral and may be promoted post-completion."
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
;; Node Types — part of the data contract
;; =============================================================================

(def node-types
  "Valid node types for session.
   - :observation  — What was seen (file contents, search results)
   - :action       — What was done (tool calls, mutations)
   - :discovery    — Key findings (patterns, bugs, architecture)
   - :decision     — Choices made (approach, tradeoffs)
   - :goal-state   — Current progress toward task goal"
  #{:observation :action :discovery :decision :goal-state})

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Noop Fallback Values
;; =============================================================================

(def ^:private noop-session
  "Noop session — nil-safe placeholder when extension is not available."
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
;; Public API — delegates to extension or returns noop
;; =============================================================================

(defn create-session-kg!
  "Create a new session for an agent execution.
   Delegates to extension if available. Returns nil otherwise.

   Arguments:
     agent-id — Agent identifier string
     task     — Original task description string

   Returns:
     Session map (opaque to caller) or nil if extension not available."
  [agent-id task]
  (delegate-or-noop :cr/create! noop-session
                    [agent-id task]))

(defn compress-turn!
  "Compress a turn's messages into session nodes.
   Delegates to extension if available. Returns 0 otherwise.

   Called by the agent loop after each tool execution step.

   Arguments:
     session  — Session map from create-session-kg! (nil-safe)
     messages — Vector of messages from this turn

   Returns:
     Number of nodes created this turn (0 if noop)."
  [session messages]
  (if session
    (delegate-or-noop :cr/compress! 0 [session messages])
    0))

(defn reconstruct-context
  "Reconstruct a compact context prompt from the session.
   Delegates to extension if available. Returns empty string otherwise.

   Called before each LLM call to replace the full message history
   with a compressed representation.

   Arguments:
     session — Session map from create-session-kg! (nil-safe)
     opts    — Optional map with :max-tokens, :recency-bias

   Returns:
     String — compact context prompt, or empty string if noop."
  [session & [opts]]
  (if session
    (delegate-or-noop :cr/reconstruct ""
                      (if opts [session opts] [session]))
    ""))

(defn build-compressed-messages
  "Build the message array for the next LLM call using context compression.
   Delegates to extension if available.

   Noop fallback: returns original messages unchanged (no compression).

   Arguments:
     session       — Session map (nil-safe)
     system-prompt — Original system prompt string
     all-messages  — Full message history (returned as-is if noop)
     recent-msgs   — Messages from the most recent turn only
     opts          — Optional reconstruction options

   Returns:
     Vector of messages ready for LLM call."
  [session system-prompt all-messages recent-msgs & [opts]]
  (if session
    (delegate-or-noop :cr/messages all-messages
                      (if opts
                        [session system-prompt recent-msgs opts]
                        [session system-prompt recent-msgs]))
    ;; Noop: return all messages unchanged (no compression)
    all-messages))

(defn promote-to-global!
  "Promote valuable session nodes to the global store.
   Delegates to extension if available. Returns {:promoted 0} otherwise.

   Called after successful task completion.

   Arguments:
     session      — Session map (nil-safe)
     global-store — IKGStore instance (global store)
     opts         — Optional map with :threshold, :scope

   Returns:
     {:promoted N :edges-created N}"
  [session global-store & [opts]]
  (if session
    (delegate-or-noop :cr/promote! {:promoted 0 :edges-created 0}
                      (if opts [session global-store opts] [session global-store]))
    {:promoted 0 :edges-created 0}))

(defn promotable-nodes
  "Get nodes from session worth promoting to global store.
   Delegates to extension if available. Returns [] otherwise.

   Arguments:
     session — Session map (nil-safe)
     opts    — Optional map with :threshold

   Returns:
     Vector of node maps suitable for global store storage."
  [session & [opts]]
  (if session
    (delegate-or-noop :cr/promotable []
                      (if opts [session opts] [session]))
    []))

(defn session-stats
  "Get compression statistics for the session.
   Delegates to extension if available. Returns noop stats otherwise.

   Returns:
     Map with :turns-compressed, :tokens-saved, :compression-ratio, etc."
  [session]
  (if session
    (delegate-or-noop :cr/stats noop-stats [session])
    noop-stats))

(defn close-session!
  "Close a session and release resources.
   Delegates to extension if available. Returns noop stats otherwise.

   Returns final stats."
  [session]
  (if session
    (delegate-or-noop :cr/close! noop-stats [session])
    noop-stats))

;; =============================================================================
;; Availability Check
;; =============================================================================

(defn compression-available?
  "Check if compressed context reconstruction is available.
   Returns true if the context reconstruction extension is registered."
  []
  (ext/extension-available? :cr/create!))
