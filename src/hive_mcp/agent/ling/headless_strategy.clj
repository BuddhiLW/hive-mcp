(ns hive-mcp.agent.ling.headless-strategy
  "Headless spawn strategy — ProcessBuilder-based ling lifecycle.

   Delegates to hive-mcp.agent.headless for process management.
   No Emacs dependency — only needs `claude` CLI on PATH.

   L2 Context Protocol (Phase 3):
   strategy-dispatch! and strategy-spawn! are L2-aware. When dispatch-context
   is a RefContext, builds an L2 context envelope (compressed refs + KG seeds)
   and prepends it to the task. Headless agents receive structured context
   they can hydrate via MCP tools instead of raw text blobs.

   SOLID: Single Responsibility — only headless/ProcessBuilder interaction.
   SOLID-O: Open for new context types via IDispatchContext protocol.
   CLARITY: L — Pure adapter between ILingStrategy and headless module."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.context-envelope :as envelope]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; L2 Context Enrichment Helpers
;;; =============================================================================

(defn- enrich-task-with-l2
  "Enrich a task string with L2 context envelope when dispatch-context available.

   When dispatch-context is a RefContext (pass-by-reference), builds an L2
   envelope and prepends it to the task. The envelope contains compressed
   context from context-store refs + KG traversal.

   For TextContext or nil dispatch-context, returns the task unchanged.

   Arguments:
     task             - Base task string
     dispatch-context - IDispatchContext instance (may be nil)
     l2-opts          - Optional map for envelope building:
                        :mode - :inline (default) or :deferred

   Returns:
     Enriched task string (with L2 envelope prepended, if applicable)."
  [task dispatch-context l2-opts]
  (if-let [l2-envelope (envelope/envelope-from-dispatch-context dispatch-context l2-opts)]
    (do
      (log/info "L2 context envelope built for headless dispatch"
                {:envelope-chars (count l2-envelope)
                 :task-chars (count (or task ""))})
      (str l2-envelope "\n\n---\n\n" task))
    task))

(defn- enrich-spawn-task-with-l2
  "Enrich a spawn task with L2 context envelope for headless agents.

   Tries to build an L2 spawn envelope from:
   1. Explicit ctx_refs/kg_node_ids in opts (coordinator-provided)
   2. Context-store lookup (from recent catchup)
   3. Falls back to nil (caller should use hints or full text)

   Arguments:
     task - Base task string
     cwd  - Working directory for context resolution
     opts - Map with optional :ctx-refs, :kg-node-ids, :scope

   Returns:
     Enriched task string, or original task if no L2 context available."
  [task cwd opts]
  (let [{:keys [ctx-refs kg-node-ids scope]} opts]
    (if (or (seq ctx-refs) (seq kg-node-ids))
      ;; Explicit refs provided — build envelope directly
      (let [l2-envelope (envelope/build-l2-envelope ctx-refs kg-node-ids scope {})]
        (if l2-envelope
          (do
            (log/info "L2 spawn envelope built from explicit refs"
                      {:categories (count ctx-refs) :kg-nodes (count kg-node-ids)})
            (str l2-envelope "\n\n---\n\n" task))
          task))
      ;; No explicit refs — try spawn envelope (context-store lookup)
      (let [l2-envelope (envelope/build-spawn-envelope cwd {})]
        (if l2-envelope
          (do
            (log/info "L2 spawn envelope built from context-store lookup" {:cwd cwd})
            (str l2-envelope "\n\n---\n\n" task))
          (do
            (log/debug "No L2 context available for headless spawn, using plain task")
            task))))))

;;; =============================================================================
;;; Headless Strategy Implementation
;;; =============================================================================

(defrecord HeadlessStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets model]} ling-ctx
          {:keys [task buffer-capacity]} opts
          ;; L2 Phase 3: Enrich task with L2 context envelope for headless agents
          ;; This gives the spawned ling structured context refs instead of raw text
          enriched-task (when task
                          (enrich-spawn-task-with-l2 task cwd opts))
          result (headless/spawn-headless! id {:cwd cwd
                                               :task (or enriched-task task)
                                               :presets (or (:presets opts) presets)
                                               :model model
                                               :buffer-capacity (or buffer-capacity 5000)})]
      (log/info "Ling spawned headless" {:id id :pid (:pid result) :cwd cwd
                                          :model (or model "claude")
                                          :l2-enriched (some? enriched-task)})
      ;; Return the ling-id (headless always uses requested id)
      id))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task dispatch-context]} task-opts
          ;; L2 Phase 3: Enrich task with L2 context envelope when RefContext provided
          enriched-task (enrich-task-with-l2 task dispatch-context {})]
      (headless/dispatch-via-stdin! id enriched-task)
      (log/info "Task dispatched to headless ling via stdin" {:ling-id id
                                                               :l2-enriched (not= task enriched-task)})
      true))

  (strategy-status [_ ling-ctx ds-status]
    (let [{:keys [id]} ling-ctx
          headless-info (headless/headless-status id)]
      (if ds-status
        (cond-> ds-status
          headless-info (assoc :headless-alive? (:alive? headless-info)
                               :headless-pid (:pid headless-info)
                               :headless-uptime-ms (:uptime-ms headless-info)
                               :headless-stdout (:stdout headless-info)
                               :headless-stderr (:stderr headless-info)))
        ;; Fallback to headless-only status
        (when headless-info
          {:slave/id id
           :slave/status (if (:alive? headless-info) :idle :dead)
           :ling/spawn-mode :headless
           :headless-alive? (:alive? headless-info)
           :headless-pid (:pid headless-info)}))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (try
        (let [result (headless/kill-headless! id)]
          (log/info "Headless ling killed" {:id id :pid (:pid result)})
          {:killed? true :id id :pid (:pid result)})
        (catch Exception e
          ;; Process might already be dead
          (log/warn "Headless kill exception" {:id id :error (ex-message e)})
          {:killed? true :id id :reason :process-already-dead})))))

;;; =============================================================================
;;; Factory
;;; =============================================================================

(defn ->headless-strategy
  "Create a HeadlessStrategy instance."
  []
  (->HeadlessStrategy))
