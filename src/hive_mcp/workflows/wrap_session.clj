(ns hive-mcp.workflows.wrap-session
  "hive-events FSM spec for the Wrap Session (crystallization) workflow.

   The wrap workflow crystallizes session learnings into long-term memory:
     ::fsm/start -> ::gather -> ::crystallize -> ::kg-edges -> ::notify -> ::evict -> ::end

   This is the Clojure handler implementation matching resources/fsm/wrap-session.edn.
   The EDN spec uses keyword handlers (:start, :gather, :crystallize, etc.) that are
   resolved to these functions at compile time via the handler-map.

   Design constraints (same as forge-belt):
   - Handlers are PURE functions: (resources, data) -> data'
   - Side effects flow through the resources map (L1 territory)
   - The FSM is the L2 map -- deterministic state transitions
   - Dispatch predicates are pure functions of state data

   Resources map (injected at run time):
     :harvest-fn     -- (fn [directory] -> harvested-data)
     :crystallize-fn -- (fn [harvested] -> {:summary-id str, :stats map, ...})
     :kg-edge-fn     -- (fn [summary-id source-ids project-id agent-id] -> {:created-count N})
     :notify-fn      -- (fn [agent-id session-id project-id stats] -> nil)
     :evict-fn       -- (fn [agent-id] -> {:evicted N})
     :scope-fn       -- (fn [directory] -> project-id)
     :source-ids-fn  -- (fn [harvested] -> [string])
     :directory      -- string (working directory for project scoping)
     :agent-id       -- string (ling's slave-id for attribution)

   State data shape:
     {:agent-id       string   ;; ling identity for attribution
      :directory      string   ;; working directory
      :project-id     string   ;; derived from directory via scope-fn
      :harvested      map      ;; crystal harvest result
      :crystal-result map      ;; crystallize result (:summary-id, :stats)
      :source-ids     [string] ;; memory entry IDs for KG edges
      :kg-result      map      ;; KG edge creation result
      :notify-sent?   bool     ;; wrap_notify dispatched
      :eviction       map      ;; context eviction result
      :error          any}     ;; error info if in error state

   SOLID: SRP -- FSM spec only, no side effects.
   CLARITY: L -- Pure layer, side effects via resources.
   CLARITY: R -- States represent domain intent."
  (:require [hive.events.fsm :as fsm]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Dispatch Predicates (pure functions of state data)
;; =============================================================================

(defn harvested?
  "Check if harvest/gather produced data."
  [data]
  (some? (:harvested data)))

(defn crystallized?
  "Check if crystallization succeeded (no error in result)."
  [data]
  (and (some? (:crystal-result data))
       (not (get-in data [:crystal-result :error]))))

(defn crystal-error?
  "Check if crystallization returned an error."
  [data]
  (some? (get-in data [:crystal-result :error])))

(defn always [_data] true)

;; =============================================================================
;; Handlers (pure functions: resources x data -> data')
;;
;; EDN handler-map keys: :start, :gather, :crystallize, :kg-edges,
;;                       :notify, :evict, :end, :error
;; =============================================================================

(defn handle-start
  "Initialize a wrap session.
   Resolves agent-id and directory, derives project-id.
   EDN handler key: :start"
  [resources data]
  (let [scope-fn (or (:scope-fn resources) (constantly nil))
        directory (or (:directory data) (:directory resources))
        agent-id (or (:agent-id data) (:agent-id resources))]
    (assoc data
           :agent-id agent-id
           :directory directory
           :project-id (when directory (scope-fn directory))
           :error nil)))

(defn handle-gather
  "Harvest session data for crystallization.
   EDN handler key: :gather

   Uses resources:
     :harvest-fn (fn [directory] -> harvested-data)"
  [resources data]
  (let [harvest-fn (:harvest-fn resources)
        directory (:directory data)
        harvested (harvest-fn directory)]
    (assoc data :harvested harvested)))

(defn handle-crystallize
  "Crystallize harvested session data into long-term memory.
   EDN handler key: :crystallize

   Uses resources:
     :crystallize-fn (fn [harvested] -> {:summary-id str, :stats map, ...})
     :source-ids-fn  (fn [harvested] -> [string])"
  [resources data]
  (let [crystallize-fn (:crystallize-fn resources)
        source-ids-fn (or (:source-ids-fn resources) (constantly []))
        harvested (:harvested data)
        result (crystallize-fn harvested)
        source-ids (source-ids-fn harvested)]
    (assoc data
           :crystal-result result
           :source-ids source-ids)))

(defn handle-kg-edges
  "Create :derived-from KG edges linking summary to source entries.
   EDN handler key: :kg-edges

   Uses resources:
     :kg-edge-fn (fn [summary-id source-ids project-id agent-id] -> {:created-count N})"
  [resources data]
  (let [{:keys [project-id agent-id]} data
        summary-id (get-in data [:crystal-result :summary-id])
        source-ids (:source-ids data)
        kg-edge-fn (:kg-edge-fn resources)]
    (if (and kg-edge-fn summary-id (seq source-ids))
      (let [result (kg-edge-fn summary-id source-ids project-id agent-id)]
        (assoc data :kg-result result))
      (assoc data :kg-result {:created-count 0 :skipped true}))))

(defn handle-notify
  "Emit wrap_notify event for hivemind permeation.
   EDN handler key: :notify

   Uses resources:
     :notify-fn (fn [agent-id session-id project-id stats] -> nil)"
  [resources data]
  (let [notify-fn (:notify-fn resources)
        {:keys [agent-id project-id crystal-result]} data
        session-id (:session crystal-result)
        stats (if (map? (:stats crystal-result)) (:stats crystal-result) {})]
    (when notify-fn
      (notify-fn agent-id session-id project-id stats))
    (assoc data :notify-sent? true)))

(defn handle-evict
  "Evict context-store entries for the completing agent.
   EDN handler key: :evict

   Uses resources:
     :evict-fn (fn [agent-id] -> {:evicted N})"
  [resources data]
  (let [evict-fn (:evict-fn resources)
        agent-id (:agent-id data)]
    (if evict-fn
      (let [result (evict-fn agent-id)]
        (assoc data :eviction result))
      (assoc data :eviction {:evicted 0 :skipped true}))))

(defn handle-end
  "Terminal state handler. Returns final wrap summary.
   EDN handler key: :end"
  [_resources {:keys [data]}]
  (select-keys data [:agent-id :project-id :crystal-result
                     :kg-result :notify-sent? :eviction]))

(defn handle-error
  "Error state handler. Captures error context.
   EDN handler key: :error"
  [_resources {:keys [error data] :as _fsm}]
  (throw (ex-info "Wrap session workflow error"
                  {:agent-id (:agent-id data)
                   :data (select-keys data [:crystal-result :error])
                   :error error})))

;; =============================================================================
;; Handler Map (for EDN spec registration in workflow registry)
;; =============================================================================

(def handler-map
  "Maps EDN keyword handlers to implementation functions.
   Used by registry/register-handlers! for EDN spec compilation."
  {:start       handle-start
   :gather      handle-gather
   :crystallize handle-crystallize
   :kg-edges    handle-kg-edges
   :notify      handle-notify
   :evict       handle-evict
   :end         handle-end
   :error       handle-error})

;; =============================================================================
;; In-Code FSM Spec (inline functions, no EDN needed)
;; =============================================================================

(def wrap-session-spec
  "hive-events FSM spec for the wrap session workflow.
   Uses inline functions -- no handler-map needed at compile time.

   State graph:
   ```
   ::fsm/start --> ::gather --> ::crystallize -+--> ::kg-edges --> ::notify --> ::evict --> ::end
                                               |
                                               +--> ::error (crystal error)
   ```"
  {:fsm
   {::fsm/start
    {:handler    handle-start
     :dispatches [[::gather (fn [data] (and (:agent-id data)
                                            (not (:error data))))]
                  [::fsm/error (fn [data] (some? (:error data)))]]}

    ::gather
    {:handler    handle-gather
     :dispatches [[::crystallize harvested?]
                  [::fsm/error always]]}

    ::crystallize
    {:handler    handle-crystallize
     :dispatches [[::fsm/error crystal-error?]
                  [::kg-edges always]]}

    ::kg-edges
    {:handler    handle-kg-edges
     :dispatches [[::notify always]]}

    ::notify
    {:handler    handle-notify
     :dispatches [[::evict always]]}

    ::evict
    {:handler    handle-evict
     :dispatches [[::fsm/end always]]}

    ::fsm/end
    {:handler handle-end}

    ::fsm/error
    {:handler handle-error}}

   :opts
   {:max-trace 50

    :pre
    (fn [{:keys [current-state-id] :as fsm} _resources]
      (update-in fsm [:data :trace-log] (fnil conj [])
                 {:state current-state-id
                  :at (str (java.time.Instant/now))
                  :direction :enter}))}})

;; =============================================================================
;; Compilation & Execution API
;; =============================================================================

(defn compile-wrap
  "Compile the wrap session FSM spec. Call once, reuse the compiled FSM."
  []
  (fsm/compile wrap-session-spec))

(defn run-wrap
  "Execute a compiled wrap session FSM.

   Args:
     compiled-fsm -- Result of compile-wrap
     resources    -- Map of side-effect functions and config
     opts         -- Optional initial data overrides

   Returns:
     Final data map with wrap results."
  ([compiled-fsm resources]
   (run-wrap compiled-fsm resources {}))
  ([compiled-fsm resources opts]
   (fsm/run compiled-fsm
            resources
            {:data (merge {:agent-id nil
                           :directory nil
                           :project-id nil}
                          opts)})))

(defn run-wrap-session
  "Convenience: compile and run a single wrap session.

   Example:
   ```clojure
   (run-wrap-session
     {:harvest-fn     (fn [dir] (crystal-hooks/harvest-all {:directory dir}))
      :crystallize-fn (fn [h] (crystal-hooks/crystallize-session h))
      :kg-edge-fn     create-derived-from-edges!
      :notify-fn      emit-wrap-notify!
      :evict-fn       evict-agent-context!
      :source-ids-fn  extract-source-ids
      :scope-fn       scope/get-current-project-id
      :directory      \"/home/user/project\"
      :agent-id       \"swarm-worker-123\"})
   ```"
  ([resources]
   (run-wrap-session resources {}))
  ([resources opts]
   (run-wrap (compile-wrap) resources opts)))
