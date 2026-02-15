(ns hive-mcp.workflows.sub-fsms.context-gather
  "Context-gather sub-FSM: reusable FSM for compressed context reconstruction.

   Wraps context.reconstruction pipeline as a composable FSM that can be embedded
   as a state handler in any parent FSM needing agent context.

   States: ::fetch-refs -> ::traverse-kg -> ::compress -> ::render -> ::fsm/end

   Handler signatures: (resources, data) -> data  (pure, testable with mock resources)

   Resources (injected by parent FSM or test):
     :fetch-ref-data-fn     - (ctx-refs) -> {category -> data}
     :gather-kg-context-fn  - (kg-node-ids, scope) -> {:nodes #{} :edges []}
     :compress-kg-fn        - (kg-context) -> string|nil
     :render-context-fn     - (ref-data, kg-context) -> string

   Input data keys:
     :ctx-refs     - Map of category->ctx-id  (e.g. {:axioms \"ctx-123\"})
     :kg-node-ids  - Vector of KG node IDs for graph traversal seeds
     :scope        - Project scope string (e.g. \"hive-mcp\")

   Output data keys (assoc'd by handlers):
     :ref-data          - Fetched reference data by category
     :kg-raw            - Raw KG traversal result {:nodes :edges}
     :compressed-context - Compressed KG subgraph string
     :rendered-context   - Final rendered context string (max 3000 chars)
     :context            - Alias of :rendered-context (convenience for parent FSM)
     :token-estimate     - Approximate token count (~chars/4)

   Usage as sub-FSM:
     (let [result (fsm/run compiled-context-gather resources {:data input-data})]
       ;; result has :context and :token-estimate
       (merge parent-data result))"

  (:require [hive.events.fsm :as fsm]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const max-output-chars
  "Maximum output size for reconstructed context. ~750 tokens."
  3000)

;; =============================================================================
;; Handlers (Pure: resources, data -> data)
;; =============================================================================

(defn handle-fetch-refs
  "Fetch reference data from context-store via batch lookup.

   Input:  {:ctx-refs {category ctx-id ...}}
   Output: assoc :ref-data {category data ...}

   Graceful degradation: missing/expired refs produce empty map."
  [resources data]
  (let [ctx-refs   (or (:ctx-refs data) {})
        fetch-fn   (:fetch-ref-data-fn resources)
        ref-data   (if (and fetch-fn (seq ctx-refs))
                     (rescue {} (fetch-fn ctx-refs))
                     {})]
    (assoc data :ref-data ref-data)))

(defn handle-traverse-kg
  "Traverse KG from seed node IDs, collecting structural edges.

   Input:  {:kg-node-ids [id ...], :scope string}
   Output: assoc :kg-raw {:nodes #{} :edges [...]}

   Bounded: max 10 nodes, depth 2. Structural relations only."
  [resources data]
  (let [kg-node-ids (or (:kg-node-ids data) [])
        scope       (:scope data)
        gather-fn   (:gather-kg-context-fn resources)
        kg-raw      (if (and gather-fn (seq kg-node-ids))
                      (rescue nil (gather-fn kg-node-ids scope))
                      nil)]
    (assoc data :kg-raw kg-raw)))

(defn handle-compress
  "Compress KG subgraph into compact edge-list representation.

   Input:  {:kg-raw {:nodes #{} :edges [...]}}
   Output: assoc :compressed-context string|nil

   Structural relations only. ~200 tokens for 10 edges."
  [resources data]
  (let [kg-raw      (:kg-raw data)
        compress-fn (:compress-kg-fn resources)
        compressed  (if (and compress-fn kg-raw (seq (:edges kg-raw)))
                      (rescue nil (compress-fn kg-raw))
                      nil)]
    (assoc data :compressed-context compressed)))

(defn handle-render
  "Render compressed context as bounded markdown.

   Input:  {:ref-data {...}, :kg-raw {:nodes :edges}}
   Output: assoc :rendered-context string, :context string, :token-estimate int

   Caps output at max-output-chars (3000 chars, ~750 tokens)."
  [resources data]
  (let [ref-data    (or (:ref-data data) {})
        kg-raw      (:kg-raw data)
        render-fn   (:render-context-fn resources)
        rendered    (if render-fn
                      (rescue nil
                              (let [result (render-fn ref-data kg-raw)]
                                (if (and result (> (count result) max-output-chars))
                                  (str (subs result 0 (- max-output-chars 20)) "\n...[truncated]")
                                  result)))
                      ;; Fallback: minimal context summary
                      (str "## Context (no renderer)\n"
                           "Refs: " (count ref-data) " categories\n"
                           "KG: " (count (:nodes kg-raw)) " nodes"))
        rendered    (or rendered "## Context (empty)\nNo context data available.")]
    (assoc data
           :rendered-context rendered
           :context          rendered
           :token-estimate   (max 1 (quot (count rendered) 4)))))

;; =============================================================================
;; Dispatch Predicates
;; =============================================================================

(defn has-ref-data?
  "Data has ref-data key (even if empty map -- fetch was attempted)."
  [data]
  (contains? data :ref-data))

(defn has-kg-raw?
  "Data has kg-raw key (even if nil -- traversal was attempted)."
  [data]
  (contains? data :kg-raw))

(defn has-compressed?
  "Data has compressed-context key (even if nil -- compression was attempted)."
  [data]
  (contains? data :compressed-context))

(defn render-complete?
  "Data has rendered context and token estimate."
  [data]
  (and (contains? data :rendered-context)
       (contains? data :token-estimate)))

;; =============================================================================
;; FSM Spec
;; =============================================================================

(def context-gather-spec
  "Context-gather sub-FSM specification.

   Linear pipeline: fetch-refs -> traverse-kg -> compress -> render -> end
   Each state always progresses forward (no loops). Errors go to ::fsm/error."
  {:fsm {::fsm/start
         {:handler    handle-fetch-refs
          :dispatches [[::traverse-kg has-ref-data?]
                       [::fsm/error (constantly true)]]}

         ::traverse-kg
         {:handler    handle-traverse-kg
          :dispatches [[::compress has-kg-raw?]
                       [::fsm/error (constantly true)]]}

         ::compress
         {:handler    handle-compress
          :dispatches [[::render has-compressed?]
                       [::fsm/error (constantly true)]]}

         ::render
         {:handler    handle-render
          :dispatches [[::fsm/end render-complete?]
                       [::fsm/error (constantly true)]]}

         ::fsm/error
         {:handler (fn [_r data]
                     (log/warn "context-gather FSM error:" (:error data))
                     ;; Ensure partial context is available even on error
                     (cond-> data
                       (not (:context data))
                       (assoc :context "## Context (error)\nReconstruction failed."
                              :token-estimate 10)))}}

   :opts {:max-trace 10}})

;; Compile once, reuse
(def ^:private compiled-fsm
  (delay (fsm/compile context-gather-spec)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn run-context-gather
  "Run the context-gather sub-FSM.

   Args:
     data      - Input map with :ctx-refs, :kg-node-ids, :scope
     resources - Map with :fetch-ref-data-fn, :gather-kg-context-fn,
                 :compress-kg-fn, :render-context-fn

   Returns:
     Final data map with :context (string), :token-estimate (int),
     plus intermediate keys (:ref-data, :kg-raw, :compressed-context, :rendered-context).

   Never throws -- graceful degradation via FSM error handler."
  ([data]
   (run-context-gather data {}))
  ([data resources]
   (try
     (fsm/run @compiled-fsm resources {:data data})
     (catch Exception e
       (log/warn "run-context-gather: unexpected error:" (.getMessage e))
       (assoc data
              :context "## Context (fatal error)\nSub-FSM failed unexpectedly."
              :token-estimate 12
              :error (.getMessage e))))))

(defn make-production-resources
  "Create resources map wired to production context.reconstruction functions.

   Lazily resolves hive-mcp.context.reconstruction via requiring-resolve
   to avoid hard namespace dependency."
  []
  (let [resolve-fn (fn [sym]
                     (rescue nil (requiring-resolve sym)))]
    {:fetch-ref-data-fn    (or (resolve-fn 'hive-mcp.context.reconstruction/fetch-ref-data)
                               (constantly {}))
     :gather-kg-context-fn (or (resolve-fn 'hive-mcp.context.reconstruction/gather-kg-context)
                               (constantly nil))
     :compress-kg-fn       (or (resolve-fn 'hive-mcp.context.reconstruction/compress-kg-subgraph)
                               (constantly nil))
     :render-context-fn    (or (resolve-fn 'hive-mcp.context.reconstruction/render-compressed-context)
                               (constantly "## Context (renderer unavailable)"))}))

(defn run-sub-fsm
  "Helper for embedding context-gather in a parent FSM handler.

   Extracts :ctx-refs, :kg-node-ids, :scope from parent data,
   runs the sub-FSM, and merges :context + :token-estimate back.

   Usage in parent handler:
     (defn handle-reconstruct [resources parent-data]
       (context-gather/run-sub-fsm resources parent-data))

   Or with explicit input keys:
     (context-gather/run-sub-fsm resources parent-data
       {:ctx-refs (:my-refs parent-data)
        :scope (:project-id parent-data)})"
  ([resources parent-data]
   (run-sub-fsm resources parent-data nil))
  ([resources parent-data overrides]
   (let [input (merge (select-keys parent-data [:ctx-refs :kg-node-ids :scope])
                      overrides)
         sub-resources (or (:context-gather-resources resources)
                           (select-keys resources [:fetch-ref-data-fn
                                                   :gather-kg-context-fn
                                                   :compress-kg-fn
                                                   :render-context-fn]))
         sub-resources (if (empty? sub-resources)
                         (make-production-resources)
                         sub-resources)
         result (run-context-gather input sub-resources)]
     (merge parent-data
            (select-keys result [:context :token-estimate
                                 :ref-data :kg-raw
                                 :compressed-context :rendered-context])))))
