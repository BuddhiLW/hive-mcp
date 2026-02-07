(ns hive-mcp.protocols.dispatch
  "Polymorphic dispatch context protocol for agent communication.

   Defines the OCP boundary for how tasks are communicated to lings/drones:
   - IDispatchContext: Protocol for resolving task context
   - TextContext: OSS implementation (plain text prompts)
   - RefContext: Context-store reference-based dispatch (pass-by-reference)
   - GraphContext: Private implementation via requiring-resolve stub
     (hive-knowledge provides KG-native context traversal)

   Architecture:
   - TextContext wraps plain prompts (backward compatible, zero deps)
   - RefContext carries context-store IDs + KG node IDs, resolves lazily
     via context-store lookups and KG traversal (~25x compression vs text)
   - GraphContext traverses task subgraphs, renders compact prompts
     from L1 disc nodes + L2 semantic nodes (requires hive-knowledge)
   - ensure-context provides backward compat: string → TextContext coercion

   SOLID-O: Open for extension (new context types), closed for modification.
   SOLID-D: Consumers depend on IDispatchContext, not concretions.
   SOLID-I: Minimal interface — just resolve-context and context-type.
   CLARITY-Y: Yield safe failure — graph context degrades to text gracefully.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IDispatchContext Protocol
;;; ============================================================================

(defprotocol IDispatchContext
  "Protocol for resolving task context for agent dispatch.

   Implementations produce a consumable map that lings/drones use as their
   task specification. Minimum shape: {:prompt <string>}.
   Extended shape: {:prompt <string> :refs [<ctx-ids>] :grounding [<L1-nodes>]}."

  (resolve-context [this]
    "Returns a map that a ling/drone can consume.
     Minimum shape: {:prompt <string>}
     Extended shape: {:prompt <string> :refs [<ctx-ids>] :grounding [<L1-nodes>]}")

  (context-type [this]
    "Returns keyword identifying the context type: :text, :graph-ref, :hybrid"))

;;; ============================================================================
;;; TextContext Record (OSS Implementation)
;;; ============================================================================

(defrecord TextContext [prompt]
  IDispatchContext
  (resolve-context [_] {:prompt prompt})
  (context-type [_] :text))

;;; ============================================================================
;;; RefContext Record (Pass-by-Reference + KG Compressed)
;;; ============================================================================

(defrecord RefContext [prompt ctx-refs kg-node-ids scope reconstruct-fn]
  ;; prompt       - Base task prompt string (always present for fallback)
  ;; ctx-refs     - Map of category->ctx-id for context-store lookups
  ;;                e.g. {:axioms "ctx-123" :conventions "ctx-456"}
  ;; kg-node-ids  - Vector of KG node IDs for graph traversal
  ;;                e.g. ["20260206235801-2b6fb27a" "20260207002000-034fae2b"]
  ;; scope        - Project scope for KG traversal (e.g. "hive-mcp")
  ;; reconstruct-fn - Function (fn [ctx-refs kg-node-ids scope] -> string)
  ;;                   that reconstructs compressed context from refs + KG.
  ;;                   Injected at creation time to avoid protocol coupling.
  IDispatchContext
  (resolve-context [_]
    (let [reconstructed (when reconstruct-fn
                          (try
                            (reconstruct-fn ctx-refs kg-node-ids scope)
                            (catch Exception e
                              (str "Context reconstruction failed: " (.getMessage e)))))]
      (cond-> {:prompt prompt}
        (seq ctx-refs)      (assoc :refs (vec (vals ctx-refs)))
        (seq kg-node-ids)   (assoc :kg-nodes kg-node-ids)
        reconstructed       (assoc :reconstructed reconstructed
                                   :prompt (str reconstructed "\n\n---\n\n" prompt)))))
  (context-type [_] :ref))

;;; ============================================================================
;;; Factory Functions
;;; ============================================================================

(defn ->text-context
  "Create a TextContext wrapping a plain text prompt.

   Arguments:
     prompt - String task description

   Returns:
     TextContext record satisfying IDispatchContext."
  [prompt]
  (->TextContext prompt))

(defn ->ref-context
  "Create a RefContext for pass-by-reference dispatch with KG compression.

   Instead of passing 2000+ token text blobs, carries lightweight IDs:
   - ctx-refs: context-store entry IDs (fetched lazily)
   - kg-node-ids: KG nodes for graph traversal context

   The reconstruct-fn is called at resolve-context time to produce
   a compressed prompt from the referenced data.

   Arguments:
     prompt         - Base task description (always present as fallback)
     opts           - Map with optional keys:
                      :ctx-refs       - Map of category->ctx-id
                      :kg-node-ids    - Vector of KG node IDs
                      :scope          - Project scope string
                      :reconstruct-fn - (fn [ctx-refs kg-node-ids scope] -> string)

   Returns:
     RefContext record satisfying IDispatchContext.

   CLARITY-Y: Falls back to prompt-only if reconstruction fails."
  [prompt {:keys [ctx-refs kg-node-ids scope reconstruct-fn]}]
  (let [;; Default reconstruct-fn: resolve lazily to avoid circular deps.
        ;; Uses requiring-resolve to load hive-mcp.context.reconstruction
        ;; at call time, not at require time.
        effective-fn (or reconstruct-fn
                         (fn [refs nodes sc]
                           (when-let [f (try (requiring-resolve
                                              'hive-mcp.context.reconstruction/reconstruct-context)
                                             (catch Exception _ nil))]
                             (f refs nodes sc))))]
    (->RefContext prompt
                  (or ctx-refs {})
                  (vec (or kg-node-ids []))
                  scope
                  effective-fn)))

(defn ->graph-context
  "Create a graph-native dispatch context. Requires hive-knowledge on classpath.
   Returns TextContext fallback when not available.

   Arguments:
     task-node-id - KG node ID for the task
     kg-store     - KG store instance for graph traversal

   Returns:
     GraphContext (if hive-knowledge available) or TextContext fallback.

   CLARITY-Y: Graceful degradation when private layer unavailable."
  [task-node-id kg-store]
  (if-let [f (try (requiring-resolve 'hive-knowledge.dispatch/->graph-context)
                  (catch Exception _ nil))]
    (f task-node-id kg-store)
    (->TextContext (str "Graph context unavailable. Task node: " task-node-id))))

;;; ============================================================================
;;; Backward Compatibility
;;; ============================================================================

(defn ensure-context
  "Wraps a plain string into TextContext if needed. Passes IDispatchContext through.

   Arguments:
     prompt-or-context - Either a string/value or an IDispatchContext instance

   Returns:
     IDispatchContext instance (original if already satisfies, TextContext otherwise)."
  [prompt-or-context]
  (if (satisfies? IDispatchContext prompt-or-context)
    prompt-or-context
    (->TextContext (str prompt-or-context))))
