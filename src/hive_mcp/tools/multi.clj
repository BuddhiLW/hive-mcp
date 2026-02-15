(ns hive-mcp.tools.multi
  "Bridge layer between cross-tool batch multiplexer and MCP tool dispatch.

   Resolves consolidated tool names to handlers, executes operations with error
   isolation, and orchestrates batch execution.

   Architecture:
     consolidated/multi.clj (MCP facade)
       -> {command: 'batch', operations: [...]}
       -> tools/multi.clj (THIS — bridge layer)
       -> resolve consolidated handler per op
       -> batch execution with operation ordering

   Delegates to extensions for advanced orchestration.

   Result DSL: Internal logic uses hive-mcp.dns.result for railway-oriented
   error handling at the handler boundary. Sub-validators are pure functions
   returning error vectors. CC reduced via function decomposition.

   Design decision: 20260207194224-3b674f5d"
  (:require [hive-mcp.dns.result :as result :refer [rescue]]
            [hive-mcp.tools.core :refer [mcp-error]]
            [hive-mcp.dsl.response :as compress]
            [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; FX Effect Handlers (Step 4 of plan-20260207-multi-tool)
;; =============================================================================
;;
;; Registers :multi/wave-complete and :multi/op-error FX handlers into the
;; hive.events.fx global registry. Uses defonce for idempotent self-contained
;; registration at require time.
;;
;; These effects are emitted by the multi-op pipeline during wave execution
;; and consumed by the hive-events FX system (hive.events.fx/do-fx-seq).

(defn- resolve-agent-id
  "Resolve the current agent-id from agent context or environment.
   Returns agent-id string or nil if not in agent context."
  []
  (try
    (when-let [ctx-fn (requiring-resolve 'hive-mcp.agent.context/current-agent-id)]
      (ctx-fn))
    (catch Exception _
      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))))

(defn- handle-wave-complete
  "FX handler for :multi/wave-complete — log wave completion and optionally
   emit hivemind progress shout when inside agent context."
  [{:keys [wave-num op-count success-count failed-count total-waves]}]
  (let [msg (str "[multi] Wave " wave-num "/" total-waves " complete: "
                 success-count "/" op-count " succeeded"
                 (when (pos? (or failed-count 0))
                   (str ", " failed-count " failed")))]
    (log/info msg)
    (when-let [agent-id (resolve-agent-id)]
      (rescue nil
              (when-let [shout-fn (requiring-resolve 'hive-mcp.hivemind.core/shout!)]
                (shout-fn agent-id :progress
                          {:task "multi-op"
                           :message msg}))))))

(defn- handle-op-error
  "FX handler for :multi/op-error — log per-op errors with structured data."
  [{:keys [op-id tool command error wave-num]}]
  (log/error "[multi] Op failed"
             {:op-id   op-id
              :tool    tool
              :command command
              :wave    wave-num
              :error   error}))

(defn register-fx!
  "Explicitly register multi FX handlers. Called during server init if
   defonce registration was deferred. Safe to call multiple times."
  []
  (when-let [reg-fx (requiring-resolve 'hive.events.fx/reg-fx)]
    (reg-fx :multi/wave-complete handle-wave-complete)
    (reg-fx :multi/op-error handle-op-error)
    (log/debug "[multi] FX effects (re-)registered")
    true))

;; =============================================================================
;; Extension Delegation
;; =============================================================================

(defn- delegate-or-noop
  "Delegates to extension if available, falls back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Tool Resolution (requiring-resolve to avoid circular deps)
;; =============================================================================

(defn resolve-consolidated-handler
  "Resolve a consolidated tool name to its handler function.
   Uses requiring-resolve to avoid circular dependency with consolidated/multi.
   Returns handler fn or nil if tool not found."
  [tool-name]
  (rescue nil
          (let [get-handler (requiring-resolve 'hive-mcp.tools.consolidated.multi/get-tool-handler)]
            (get-handler tool-name))))

(defn resolve-tool-handler
  "Resolve a tool name to its handler function.
   Tries consolidated handlers first, then flat tool resolution.
   Returns handler fn or nil if tool not found."
  [tool-name]
  (or (resolve-consolidated-handler tool-name)
      (rescue nil
              (let [get-tool-fn (requiring-resolve 'hive-mcp.tools/get-tool-by-name)
                    tool-def    (get-tool-fn tool-name)]
                (:handler tool-def)))))

;; =============================================================================
;; Operation Normalization
;; =============================================================================

(defn normalize-op
  "Normalize a single operation map from MCP JSON format.
   Converts string keys to keywords. Ensures :id and :tool are present."
  [op]
  (let [normalized (into {} (map (fn [[k v]] [(keyword k) v]) op))]
    (cond-> normalized
      ;; Auto-generate ID if missing
      (str/blank? (:id normalized))
      (assoc :id (str "op-" (java.util.UUID/randomUUID)))

      ;; Normalize depends_on to vector of strings
      (:depends_on normalized)
      (update :depends_on (fn [deps]
                            (cond
                              (string? deps) [deps]
                              (sequential? deps) (vec deps)
                              :else []))))))

;; =============================================================================
;; Reference Resolution — Delegates to extension
;; =============================================================================

(def ^:private ref-not-found
  "Sentinel for unresolvable reference."
  ::ref-not-found)

(defn ref?
  "Predicate: is this value a $ref string?"
  [v]
  (and (string? v) (str/starts-with? v "$ref:")))

(defn parse-ref
  "Delegates to extension for reference parsing."
  [s]
  (delegate-or-noop :bx/a nil [s]))

(defn extract-result-data
  "Delegates to extension for result data extraction."
  [handler-result]
  (delegate-or-noop :bx/b handler-result [handler-result]))

(defn enrich-op-result
  "Enrich an execute-op result with :data (parsed handler result).
   Delegates to extension."
  [{:keys [result] :as op-result}]
  (assoc op-result :data (extract-result-data result)))

(defn resolve-ref
  "Delegates to extension for reference resolution."
  [parsed-ref results-by-id]
  (delegate-or-noop :bx/c ref-not-found [parsed-ref results-by-id]))

(defn resolve-refs-in-value
  "Delegates to extension for recursive reference resolution."
  [v results-by-id]
  (delegate-or-noop :bx/d v [v results-by-id]))

(defn resolve-op-refs
  "Delegates to extension for operation reference resolution."
  [op results-by-id]
  (delegate-or-noop :bx/e op [op results-by-id]))

(defn collect-ref-op-ids
  "Delegates to extension for reference collection."
  [op]
  (delegate-or-noop :bx/f #{} [op]))

(defn- validate-ref-deps
  "Delegates to extension for reference dependency validation."
  [ops]
  (delegate-or-noop :bx/g [] [ops]))

;; =============================================================================
;; Operation Validation — Decomposed Sub-Validators
;; =============================================================================

(defn- validate-required-fields
  "Check all ops have non-blank :id and :tool. Returns error vector."
  [ops]
  (into []
        (mapcat (fn [{:keys [id tool] :as op}]
                  (cond-> []
                    (str/blank? id)
                    (conj (str "Operation missing :id — " (pr-str (select-keys op [:tool :command]))))
                    (str/blank? tool)
                    (conj (str "Operation '" id "' missing :tool")))))
        ops))

(defn- validate-unique-ids
  "Check for duplicate operation IDs. Returns error vector."
  [ops]
  (into []
        (comp (filter (fn [[_id cnt]] (> cnt 1)))
              (map (fn [[id cnt]]
                     (str "Duplicate operation ID: '" id "' (appears " cnt " times)"))))
        (frequencies (map :id ops))))

(defn- validate-dep-references
  "Check deps reference existing ops, no self-deps. Returns error vector."
  [ops]
  (let [id-set (set (map :id ops))]
    (into []
          (mapcat (fn [{:keys [id depends_on]}]
                    (when (seq depends_on)
                      (mapcat (fn [dep]
                                (cond-> []
                                  (= dep id)
                                  (conj (str "Operation '" id "' depends on itself"))
                                  (not (contains? id-set dep))
                                  (conj (str "Operation '" id "' depends on non-existent '" dep "'"))))
                              depends_on))))
          ops)))

(defn- detect-cycles
  "Delegates to extension for cycle detection."
  [ops]
  (delegate-or-noop :bx/h [] [ops]))

(defn validate-ops
  "Validate an operations vector. Returns {:valid true} or {:valid false :errors [...]}.

   Delegates to sub-validators for each concern:
   - Required fields (:id and :tool)
   - Unique IDs
   - Dependency references exist, no self-deps
   - No circular dependencies (Kahn's algorithm)
   - $ref targets declared in depends_on"
  [ops]
  (let [basic-errors (into (validate-required-fields ops)
                           (concat (validate-unique-ids ops)
                                   (validate-dep-references ops)))]
    (if (seq basic-errors)
      {:valid false :errors basic-errors}
      ;; Only check cycles and ref deps when no basic errors
      (let [all-errors (into (detect-cycles ops)
                             (validate-ref-deps ops))]
        (if (seq all-errors)
          {:valid false :errors all-errors}
          {:valid true})))))

;; =============================================================================
;; Wave Assignment — Delegates to extension
;; =============================================================================

(defn assign-waves
  "Assign operations to execution waves. Delegates to extension.
   Noop: all ops assigned to wave 1 (sequential execution)."
  [ops]
  (delegate-or-noop :bx/i (mapv #(assoc % :wave 1) ops) [ops]))

;; =============================================================================
;; Single Operation Execution (error-isolated)
;; =============================================================================

(defn execute-op
  "Execute a single operation with error isolation.
   Resolves consolidated tool handler, merges command + params, invokes handler.

   Returns {:id op-id :success bool :result map} or {:id op-id :success false :error string}."
  [{:keys [id tool] :as op}]
  (try
    (let [handler (resolve-tool-handler tool)]
      (if-not handler
        {:id id :success false :error (str "Tool not found: " tool)}
        (let [;; Build handler args: all params except multi-meta keys
              meta-keys #{:id :tool :depends_on :wave}
              handler-args (-> (apply dissoc op meta-keys)
                               ;; Ensure command is string for CLI dispatch
                               (update :command #(if (keyword? %) (name %) %)))
              result (handler handler-args)]
          {:id id :success true :result result})))
    (catch Exception e
      (log/error {:event :op-execution-error
                  :op-id id
                  :tool  tool
                  :error (ex-message e)})
      {:id id :success false :error (ex-message e)})))

;; =============================================================================
;; Wave Execution Engine
;; =============================================================================

(defn- execute-wave
  "Execute all operations in a single wave. Delegates to extension.
   Noop: sequential execution via mapv."
  [wave-ops]
  (if-let [f (ext/get-extension :bx/j)]
    (f wave-ops execute-op)
    (mapv execute-op wave-ops)))

(defn- check-deps-satisfied
  "Check if all dependencies for an op have succeeded.
   Returns {:ok true} or {:ok false :failed-deps [ids]}."
  [{:keys [depends_on]} results-by-id]
  (if (empty? depends_on)
    {:ok true}
    (let [failed (filterv (fn [dep-id]
                            (let [r (get results-by-id dep-id)]
                              (or (nil? r) (not (:success r)))))
                          depends_on)]
      (if (empty? failed)
        {:ok true}
        {:ok false :failed-deps failed}))))

;; =============================================================================
;; FX Emission Helpers
;; =============================================================================

(defn- fire-fx!
  "Fire a single FX effect through the hive.events.fx registry.
   No-op if fx registry is not available (graceful degradation)."
  [fx-id fx-data]
  (rescue nil
          (when-let [get-fx (requiring-resolve 'hive.events.fx/get-fx)]
            (when-let [handler (get-fx fx-id)]
              (handler fx-data)))))

(defn- emit-wave-complete!
  "Emit :multi/wave-complete FX after a wave finishes execution."
  [wave-num wave-results total-waves]
  (let [op-count (count wave-results)
        success-count (count (filter :success wave-results))
        failed-count (- op-count success-count)]
    (fire-fx! :multi/wave-complete
              {:wave-num      wave-num
               :op-count      op-count
               :success-count success-count
               :failed-count  failed-count
               :total-waves   total-waves})))

(defn- emit-op-errors!
  "Emit :multi/op-error FX for each failed op in wave results."
  [wave-results wave-num]
  (doseq [{:keys [id error] :as r} wave-results
          :when (and (not (:success r)) error)]
    (fire-fx! :multi/op-error
              {:op-id    id
               :tool     (:tool r)
               :command  (:command r)
               :error    error
               :wave-num wave-num})))

;; =============================================================================
;; Multi-Operation Runner — Decomposed Pipeline
;; =============================================================================

(defn- compile-batch
  "Normalize → validate → assign-waves. Returns Result.
   Ok:  {:waved-ops [...] :wave-groups {1 [...] 2 [...]}}
   Err: :multi/validation-failed with :errors and :total."
  [ops]
  (let [normalized (mapv normalize-op ops)
        validation (validate-ops normalized)]
    (if-not (:valid validation)
      (result/err :multi/validation-failed
                  {:errors (:errors validation) :total (count ops)})
      (let [waved (assign-waves normalized)]
        (result/ok {:waved-ops   waved
                    :wave-groups (group-by :wave waved)})))))

(defn- build-dry-run-response
  "Build dry-run plan response from wave groups."
  [wave-groups total-count]
  {:success true
   :dry-run true
   :waves   (into (sorted-map)
                  (map (fn [[w ops]]
                         [w {:ops (mapv #(select-keys % [:id :tool :command :depends_on]) ops)}])
                       wave-groups))
   :summary {:total total-count :success 0 :failed 0 :waves (count wave-groups)}})

(defn- execute-and-collect-wave
  "Execute one wave, skipping ops with failed deps. Returns result vector.
   Resolves $ref strings in op params before execution.
   Enriches results with :data (parsed JSON content) for downstream refs."
  [wave-ops all-results]
  (let [{executable true skipped false}
        (group-by #(:ok (check-deps-satisfied % all-results)) wave-ops)

        skip-results (mapv (fn [op]
                             (let [{:keys [failed-deps]} (check-deps-satisfied op all-results)]
                               (enrich-op-result
                                {:id (:id op) :success false
                                 :error (str "Skipped: dependencies failed — " (str/join ", " failed-deps))})))
                           (or skipped []))

        ;; Phase 1: Resolve $ref strings in params before execution
        resolved-ops (mapv #(resolve-op-refs % all-results) (or executable []))

        exec-results (mapv enrich-op-result (execute-wave resolved-ops))]
    (into skip-results exec-results)))

(defn- execute-all-waves
  "Execute all waves sequentially, collecting results and emitting FX."
  [wave-groups total-count]
  (let [wave-count   (count wave-groups)
        all-results  (atom {})
        wave-log     (atom (sorted-map))]
    (doseq [wave-num (sort (keys wave-groups))]
      (let [wave-ops (get wave-groups wave-num)
            wave-all (execute-and-collect-wave wave-ops @all-results)]
        ;; Store results
        (doseq [r wave-all]
          (swap! all-results assoc (:id r) r))
        (swap! wave-log assoc wave-num
               {:ops     (mapv #(select-keys % [:id :tool :command]) wave-ops)
                :results wave-all})
        ;; Emit FX effects for observability
        (emit-wave-complete! wave-num wave-all wave-count)
        (emit-op-errors! wave-all wave-num)))

    (let [results     (vals @all-results)
          success-cnt (count (filter :success results))
          failed-cnt  (count (remove :success results))]
      {:success (zero? failed-cnt)
       :waves   @wave-log
       :summary {:total   total-count
                 :success success-cnt
                 :failed  failed-cnt
                 :waves   wave-count}})))

(defn run-multi
  "Execute a vector of cross-tool operations with dependency ordering.

   Pipeline: normalize → validate → assign-waves → execute-per-wave (parallel within wave)

   Options:
   - :dry-run  — validate and plan only, don't execute

   Returns:
   {:success bool
    :waves   {1 {:ops [...] :results [...]}
              2 {:ops [...] :results [...]}}
    :summary {:total N :success M :failed F :waves W}
    :errors  [...] (validation errors if any)}"
  [ops & {:keys [dry-run]}]
  (let [compiled (compile-batch ops)]
    (if (result/err? compiled)
      {:success false
       :errors  (:errors compiled)
       :summary {:total (or (:total compiled) (count ops)) :success 0 :failed 0 :waves 0}}
      (let [{:keys [wave-groups]} (:ok compiled)]
        (if dry-run
          (build-dry-run-response wave-groups (count ops))
          (execute-all-waves wave-groups (count ops)))))))

;; =============================================================================
;; Result Formatting — Decomposed Helpers
;; =============================================================================

(defn- format-op-result
  "Format a single operation result for MCP response."
  [{:keys [id success result error]}]
  (cond-> {:id id :success success}
    result (assoc :result
                  (if (string? result)
                    result
                    (try (json/write-str result)
                         (catch Exception _ (pr-str result)))))
    error  (assoc :error error)))

(defn- format-execution-waves
  "Format execution wave results for MCP response."
  [waves]
  (into {}
        (map (fn [[w {:keys [results]}]]
               [(str "wave_" w) (mapv format-op-result results)]))
        waves))

(defn- format-dry-run-plan
  "Format dry-run wave plan for MCP response."
  [waves]
  (into {}
        (map (fn [[w {:keys [ops]}]]
               [(str "wave_" w) ops]))
        waves))

(defn format-results
  "Format multi-execution results for MCP response.
   Returns a JSON-serializable map suitable for {:type \"text\" :text ...}.

   Options (keyword args):
   - :compact  — when truthy, apply batch envelope compression:
                  omit success=true, short keys, flatten single-op results.
                  Accepts true, :compact, or string \"compact\"."
  [{:keys [success waves summary errors dry-run] :as _results} & {:keys [compact]}]
  (let [output (cond-> {:success success
                        :summary summary}
                 dry-run     (assoc :dry_run true
                                    :plan (format-dry-run-plan waves))
                 (not dry-run)
                 (assoc :waves (format-execution-waves waves))
                 errors (assoc :errors errors))
        compressed (if compact
                     (compress/format-results-compact output)
                     output)]
    {:type "text"
     :text (json/write-str compressed)}))

;; =============================================================================
;; MCP Handler (called from consolidated/multi.clj)
;; =============================================================================

(defn- validate-batch-input
  "Validate batch input parameters. Returns Result."
  [{:keys [operations]}]
  (cond
    (or (nil? operations) (empty? operations))
    (result/err :multi/missing-ops
                {:message "Batch requires non-empty 'operations' array. Each op: {tool, command, ...params}"})

    (not (sequential? operations))
    (result/err :multi/invalid-ops
                {:message "operations must be an array of operation objects"})

    :else
    (result/ok operations)))

(defn handle-batch
  "Handle a batch of cross-tool operations from the MCP multi tool.

   Expects params:
   - :operations  — vector of operation maps, each with :tool, :command, + params
                    Optional: :id (auto-generated if missing), :depends_on [ids]
   - :dry_run     — (optional) validate and plan without executing
   - :compact     — (optional) apply batch envelope compression
   - :parallel    — (optional) hint, ignored since wave engine handles parallelism

   Each operation has the shape:
   {:id         'op-1'           ;; unique within batch (auto-gen if missing)
    :tool       'memory'         ;; consolidated tool name
    :command    'add'            ;; command for the tool
    :depends_on ['op-0']         ;; (optional) dependency ordering
    ...params}                   ;; tool-specific params forwarded

   Example:
   {:operations [{:id 'op-1' :tool 'memory' :command 'add' :content 'hello' :type 'note'}
                 {:id 'op-2' :tool 'kg' :command 'edge' :from 'a' :to 'b' :relation 'implements'
                  :depends_on ['op-1']}]}"
  [params]
  (let [input   (validate-batch-input params)
        compact (compress/resolve-compress-mode params)]
    (if (result/err? input)
      (mcp-error (:message input))
      (format-results (run-multi (:ok input)
                                 :dry-run (boolean (:dry_run params)))
                      :compact compact))))
