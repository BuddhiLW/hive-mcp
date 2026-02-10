(ns hive-mcp.tools.agent.dispatch
  "Agent task dispatch handler.

   Handles dispatching tasks to agents with:
   - IDispatchContext abstraction (TextContext / RefContext)
   - Compressed context via pass-by-reference
   - Agent type detection for correct dispatch strategy

   SOLID-D: Depends on IDispatchContext abstraction, not string concretion.
   SOLID-O: Open for RefContext extension without modifying TextContext path.
   CLARITY-I: Validates agent exists before dispatch."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Lazy require to avoid circular dependency
;; =============================================================================

(defn- get-delegate-fn
  "Lazily resolve hive-mcp.agent.core/delegate-agentic-drone! to avoid circular dep."
  []
  (require 'hive-mcp.agent.core)
  (resolve 'hive-mcp.agent.core/delegate-agentic-drone!))

;; =============================================================================
;; Dispatch Context Builder
;; =============================================================================

(defn- build-dispatch-context
  "Build an IDispatchContext from dispatch parameters.

   When ctx_refs are provided (pass-by-reference mode), creates a RefContext
   that carries lightweight context-store IDs + node IDs instead of full
   text blobs (token-efficient compression). Otherwise wraps prompt as TextContext.

   Arguments:
     prompt      - Base task prompt string (always present as fallback)
     ctx_refs    - Map of category->ctx-id for context-store lookups (optional)
     kg_node_ids - Vector of Node IDs for context resolution (optional)
     scope       - Project scope string for KG traversal (optional)

   Returns:
     IDispatchContext instance (RefContext or TextContext).

   SOLID-O: Open for extension - new context types via new factory fns.
   CLARITY-Y: Falls back to TextContext when no refs provided."
  [prompt ctx_refs kg_node_ids scope]
  (if (seq ctx_refs)
    ;; Pass-by-reference mode: create RefContext with context-store IDs
    (let [refs-map (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} ctx_refs)]
      (dispatch-ctx/->ref-context prompt
                                  {:ctx-refs    refs-map
                                   :kg-node-ids (vec (or kg_node_ids []))
                                   :scope       scope}))
    ;; Plain text mode: wrap string as TextContext (backward compatible)
    (dispatch-ctx/ensure-context prompt)))

;; =============================================================================
;; Dispatch Handler
;; =============================================================================

(defn handle-dispatch
  "Dispatch a task to an agent.

   Parameters:
     agent_id    - Target agent ID (required)
     prompt      - Task prompt/description, or IDispatchContext (required)
     files       - Files to include (optional)
     priority    - Task priority: normal, high, low (optional)
     ctx_refs    - Map of category->ctx-id for compressed context (optional)
                   e.g. {\"axioms\": \"ctx-123\", \"decisions\": \"ctx-456\"}
                   When provided, creates RefContext (token-efficient compression vs text)
     kg_node_ids - Vector of Node IDs for context resolution seeds (optional)
     scope       - Project scope for KG traversal (optional, auto-derived)

   Accepts plain string prompts (backward compat) or IDispatchContext instances.
   When ctx_refs is provided, creates RefContext for pass-by-reference dispatch.

   SOLID-D: Depends on IDispatchContext abstraction, not string concretion.
   SOLID-O: Open for RefContext extension without modifying TextContext path.
   CLARITY: I - Validates agent exists before dispatch."
  [{:keys [agent_id prompt files priority ctx_refs kg_node_ids scope]}]
  (cond
    (empty? agent_id)
    (mcp-error "agent_id is required")

    (empty? prompt)
    (mcp-error "prompt is required")

    :else
    (try
      (if-let [agent-data (queries/get-slave agent_id)]
        (let [agent-type (if (= 1 (:slave/depth agent-data)) :ling :drone)
              agent (case agent-type
                      :ling (ling/->ling agent_id {:cwd (:slave/cwd agent-data)
                                                   :presets (:slave/presets agent-data)
                                                   :project-id (:slave/project-id agent-data)
                                                   :spawn-mode (or (:ling/spawn-mode agent-data) :vterm)})
                      :drone (drone/->drone agent_id {:cwd (:slave/cwd agent-data)
                                                      :parent-id (:slave/parent agent-data)
                                                      :project-id (:slave/project-id agent-data)}))
              ;; IDispatchContext: build RefContext when ctx_refs provided,
              ;; TextContext for plain strings (backward compatible)
              ctx (build-dispatch-context prompt ctx_refs kg_node_ids
                                          (or scope (:slave/project-id agent-data)))
              resolved-prompt (:prompt (dispatch-ctx/resolve-context ctx))
              ;; Drones need delegate-fn for execution; lings use elisp or stdin dispatch
              task-opts (cond-> {:task resolved-prompt
                                 :dispatch-context ctx
                                 :files files
                                 :priority (keyword (or priority "normal"))}
                          (= agent-type :drone)
                          (assoc :delegate-fn (get-delegate-fn)))
              task-id (proto/dispatch! agent task-opts)]
          (log/info "Dispatched task to agent" {:agent_id agent_id
                                                :task-id task-id
                                                :context-type (dispatch-ctx/context-type ctx)})
          (mcp-json {:success true
                     :agent-id agent_id
                     :task-id task-id
                     :context-type (name (dispatch-ctx/context-type ctx))
                     :files files}))
        (mcp-error (str "Agent not found: " agent_id)))
      (catch Exception e
        (log/error "Failed to dispatch to agent" {:agent_id agent_id :error (ex-message e)})
        (mcp-error (str "Failed to dispatch: " (ex-message e)))))))
