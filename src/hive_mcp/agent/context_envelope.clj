(ns hive-mcp.agent.context-envelope
  "L2 Context Envelope for headless agent communication.

   Builds structured context blocks that headless agents receive instead of
   raw text blobs. The envelope contains context-store references and KG node
   IDs that the agent can hydrate on-demand via MCP tools.

   This is the bridge between the IDispatchContext protocol (coordinator-side)
   and headless agent stdin dispatch (subprocess-side).

   Key insight: headless agents connect to the same MCP server (bb-mcp → hive-mcp).
   They CAN call `session context-reconstruct`, `session context-get`, `memory get`,
   and `kg traverse`. The envelope tells them WHICH refs to hydrate.

   Two modes:
   - :inline  — Resolve RefContext immediately, embed compressed context (~750 tokens)
   - :deferred — Pass ref IDs with hydration instructions, agent resolves lazily (~200 tokens)

   Default: :inline (simpler, no extra MCP round-trips for the agent).

   SOLID-S: Single responsibility — context envelope building only.
   SOLID-O: Open for extension — new envelope formats via multimethods.
   CLARITY-Y: Yield safe failure — falls back to text on any reconstruction error."
  (:require [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.context.reconstruction :as reconstruction]
            [hive-mcp.tools.catchup.spawn :as catchup-spawn]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const max-inline-chars
  "Maximum characters for inline-resolved context envelope."
  4000)

;; =============================================================================
;; Inline Mode: Resolve at build time, embed compressed context
;; =============================================================================

(defn- build-inline-envelope
  "Build an inline L2 envelope by resolving RefContext immediately.

   Calls reconstruct-context to produce compressed markdown from refs + KG,
   then wraps it in an envelope header that identifies it as L2 context.

   Arguments:
     ctx-refs    - Map of category->ctx-id
     kg-node-ids - Vector of KG node IDs
     scope       - Project scope string

   Returns:
     Envelope string with compressed context, or nil on failure."
  [ctx-refs kg-node-ids scope]
  (try
    (let [reconstructed (reconstruction/reconstruct-context
                          ctx-refs
                          (or kg-node-ids [])
                          scope)]
      (when (and reconstructed (not (str/blank? reconstructed)))
        (let [envelope (str "<!-- L2-CONTEXT mode=inline scope=" (or scope "unknown") " -->\n"
                            reconstructed
                            "\n<!-- /L2-CONTEXT -->\n")]
          (if (> (count envelope) max-inline-chars)
            (str (subs envelope 0 (- max-inline-chars 30)) "\n...[truncated]\n<!-- /L2-CONTEXT -->\n")
            envelope))))
    (catch Exception e
      (log/debug "build-inline-envelope failed (graceful degradation):" (.getMessage e))
      nil)))

;; =============================================================================
;; Deferred Mode: Pass refs + instructions, agent hydrates lazily
;; =============================================================================

(defn- build-deferred-envelope
  "Build a deferred L2 envelope with ref IDs and hydration instructions.

   The agent receives lightweight refs (~200 tokens) and hydrates via MCP tools
   during its Silence phase. More token-efficient for large contexts but requires
   an extra MCP round-trip.

   Arguments:
     ctx-refs    - Map of category->ctx-id
     kg-node-ids - Vector of KG node IDs
     scope       - Project scope string

   Returns:
     Envelope string with refs and fetch instructions."
  [ctx-refs kg-node-ids scope]
  (let [sections (atom [])]
    ;; Header
    (swap! sections conj (str "<!-- L2-CONTEXT mode=deferred scope=" (or scope "unknown") " -->"))
    (swap! sections conj "## L2 Context (Pass-by-Reference)")
    (swap! sections conj "")
    (swap! sections conj "**Mode**: deferred (hydrate on-demand via MCP tools)")
    (swap! sections conj "")

    ;; Context refs table
    (when (seq ctx-refs)
      (swap! sections conj "### Context Store References")
      (swap! sections conj "| Category | Context ID |")
      (swap! sections conj "|----------|------------|")
      (doseq [[category ctx-id] (sort-by key ctx-refs)]
        (swap! sections conj (str "| " (name category) " | `" ctx-id "` |")))
      (swap! sections conj ""))

    ;; KG seeds
    (when (seq kg-node-ids)
      (swap! sections conj "### KG Traversal Seeds")
      (doseq [node-id kg-node-ids]
        (swap! sections conj (str "- `" node-id "`")))
      (swap! sections conj ""))

    ;; Hydration instructions
    (swap! sections conj "### How to Hydrate")
    (swap! sections conj "")
    (swap! sections conj "**Option A** (recommended): Reconstruct all at once:")
    (swap! sections conj "```json")
    (swap! sections conj (str "{\"command\": \"context-reconstruct\", "
                              "\"ctx_refs\": " (pr-str (into {} (map (fn [[k v]] [(name k) v]) ctx-refs))) ", "
                              "\"kg_node_ids\": " (pr-str (vec kg-node-ids)) ", "
                              "\"scope\": \"" (or scope "") "\"}"))
    (swap! sections conj "```")
    (swap! sections conj "")
    (swap! sections conj "**Option B**: Fetch individual categories:")
    (swap! sections conj "```json")
    (doseq [[_category ctx-id] (sort-by key ctx-refs)]
      (swap! sections conj (str "{\"command\": \"context-get\", \"ctx_id\": \"" ctx-id "\"}")))
    (swap! sections conj "```")
    (swap! sections conj "")
    (swap! sections conj "**TTL**: Refs expire ~10 minutes after creation. Run `/catchup` if expired.")
    (swap! sections conj "<!-- /L2-CONTEXT -->")

    (str/join "\n" @sections)))

;; =============================================================================
;; Public API: Build L2 Envelope
;; =============================================================================

(defn build-l2-envelope
  "Build an L2 context envelope for headless agent communication.

   Takes structured context references and produces a markdown block that
   can be prepended to a headless agent's task prompt.

   Arguments:
     ctx-refs    - Map of category->ctx-id for context-store lookups
                   e.g. {:axioms \"ctx-123\" :decisions \"ctx-456\"}
     kg-node-ids - Vector of KG node IDs for graph traversal seeds
     scope       - Project scope string (e.g. \"hive-mcp\")
     opts        - Optional map:
                   :mode - :inline (default) or :deferred
                           :inline resolves refs immediately (~750 tokens)
                           :deferred passes ref IDs for lazy hydration (~200 tokens)

   Returns:
     L2 context envelope string, or nil if no refs provided / build failed.

   CLARITY-Y: Returns nil on failure (caller falls back to text dispatch)."
  ([ctx-refs kg-node-ids scope]
   (build-l2-envelope ctx-refs kg-node-ids scope {}))
  ([ctx-refs kg-node-ids scope {:keys [mode] :or {mode :inline}}]
   (when (or (seq ctx-refs) (seq kg-node-ids))
     (case mode
       :deferred (build-deferred-envelope ctx-refs kg-node-ids scope)
       ;; :inline (default) — resolve immediately, fall back to deferred
       (or (build-inline-envelope ctx-refs kg-node-ids scope)
           (build-deferred-envelope ctx-refs kg-node-ids scope))))))

;; =============================================================================
;; Envelope from IDispatchContext
;; =============================================================================

(defn envelope-from-dispatch-context
  "Extract L2 envelope from an IDispatchContext instance.

   For RefContext: builds an L2 envelope from the stored refs + KG nodes.
   For TextContext: returns nil (no structured refs to envelope).

   This is the primary integration point — called by HeadlessStrategy
   when it receives a dispatch-context in task-opts.

   Arguments:
     dispatch-context - IDispatchContext instance (RefContext or TextContext)
     opts             - Optional map with :mode (:inline or :deferred)

   Returns:
     L2 envelope string, or nil for non-ref contexts."
  ([dispatch-context]
   (envelope-from-dispatch-context dispatch-context {}))
  ([dispatch-context opts]
   (when (and dispatch-context
              (= :ref (dispatch-ctx/context-type dispatch-context)))
     ;; RefContext — extract structured fields
     (let [{:keys [ctx-refs kg-node-ids scope]} dispatch-context]
       (build-l2-envelope ctx-refs kg-node-ids scope opts)))))

;; =============================================================================
;; Spawn Envelope: Auto-generate L2 context for headless spawn
;; =============================================================================

(defn build-spawn-envelope
  "Build an L2 context envelope for headless ling spawn.

   Generates context refs from a recent catchup (context-store lookup)
   or runs a lightweight catchup to produce them.

   This replaces the ~12K token full-text spawn-context with a ~750 token
   L2 envelope for headless agents.

   Arguments:
     directory - Working directory for project scoping
     opts      - Optional map:
                 :ctx-refs    - Pre-existing context refs (skip lookup)
                 :kg-node-ids - Pre-existing KG node IDs
                 :scope       - Pre-existing scope
                 :mode        - :inline (default) or :deferred

   Returns:
     L2 envelope string, or nil if no refs available.
     On failure, returns nil (caller should fall back to spawn-context :full).

   CLARITY-Y: Never throws — returns nil on failure for graceful fallback."
  ([directory]
   (build-spawn-envelope directory {}))
  ([directory {:keys [ctx-refs kg-node-ids scope mode] :or {mode :inline}}]
   (try
     (if (seq ctx-refs)
       ;; Pre-existing refs — build envelope directly
       (build-l2-envelope ctx-refs kg-node-ids scope {:mode mode})
       ;; No refs — try spawn-context :ref mode which does context-store lookup
       ;; This returns a markdown string already, but we want structured envelope
       (let [ref-context (catchup-spawn/spawn-context directory {:mode :ref})]
         (when ref-context
           ;; spawn-context :ref already produces a good format
           ;; Wrap it in L2 envelope markers for consistency
           (str "<!-- L2-CONTEXT mode=spawn-ref -->\n"
                ref-context
                "\n<!-- /L2-CONTEXT -->\n"))))
     (catch Exception e
       (log/debug "build-spawn-envelope failed (graceful degradation):" (.getMessage e))
       nil))))
