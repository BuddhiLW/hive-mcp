(ns hive-mcp.workflows.catchup-session
  "hive-events FSM handlers for the Catchup workflow.

   The catchup workflow restores session context from Chroma memory,
   enriches it with KG relationships, runs maintenance tasks, and
   delivers entries via piggyback + context-store for pass-by-reference.

   This is the Clojure handler implementation matching resources/fsm/catchup.edn.
   The EDN spec uses keyword handlers (:start, :scope-resolve, :query-memory, etc.)
   that are resolved to these functions at compile time via the handler-map.

   Design constraints (same as forge-belt, wrap-session):
   - Handlers are PURE functions: (resources, data) -> data'
   - Side effects flow through the resources map (L1 territory)
   - The FSM is the L2 map -- deterministic state transitions
   - Dispatch predicates are pure functions of state data

   Resources map (injected at run time):
     :chroma-check-fn      -- () -> bool (embedding configured?)
     :scope-fn             -- (directory) -> project-id
     :project-name-fn      -- (directory) -> string
     :query-fn             -- (type, tags, project-id, limit) -> entries
     :query-axioms-fn      -- (project-id) -> entries
     :query-conventions-fn -- (project-id, axiom-ids, priority-ids) -> entries
     :query-expiring-fn    -- (project-id, limit) -> entries
     :git-fn               -- (directory) -> git-info
     :entry->meta-fns      -- {:axiom fn, :priority fn, :catchup fn}
     :kg-enrich-fn         -- (entries) -> {:entries [...] :kg-count N}
     :kg-insights-fn       -- (decisions, conventions, sessions, project-id) -> map
     :co-access-fn         -- (entry-ids, exclude-ids) -> suggestions
     :permeate-fn          -- (directory) -> {:permeated N :agents [...]}
     :tree-scan-fn         -- (directory) -> scan-result
     :disc-decay-fn        -- (project-id) -> decay-stats
     :piggyback-fn         -- (agent-id, project-id, entries, refs) -> nil
     :context-store-fn     -- (data, tags, ttl) -> ctx-id
     :build-scopes-fn      -- (project-name, project-id) -> scopes
     :build-response-fn    -- (data) -> response
     :error-response-fn    -- (error) -> response

   State data shape:
     {:directory            string    ;; working directory (input)
      :project-id           string    ;; resolved project scope
      :project-name         string    ;; display name
      :scopes               vector    ;; scope tags for display
      :chroma-configured?   bool      ;; prerequisite check

      ;; Raw Chroma entries
      :axioms               vector
      :priority-conventions vector
      :sessions             vector
      :decisions            vector
      :conventions          vector
      :snippets             vector
      :expiring             vector

      ;; Git context
      :git-info             map       ;; {:branch :uncommitted :last-commit}

      ;; Metadata transforms
      :axioms-meta          vector
      :priority-meta        vector
      :sessions-meta        vector
      :decisions-meta       vector    ;; KG-enriched
      :conventions-meta     vector    ;; KG-enriched
      :snippets-meta        vector
      :expiring-meta        vector

      ;; KG enrichment
      :kg-insights          map

      ;; Maintenance results
      :permeation           map       ;; {:permeated N :agents [...]}
      :project-tree-scan    map
      :disc-decay           map

      ;; Delivery
      :context-refs         map       ;; category->ctx-id
      :piggyback-enqueued?  bool

      ;; Error
      :error                any}

   SOLID: SRP -- FSM handlers only, no side effects.
   CLARITY: L -- Pure layer, side effects via resources.
   CLARITY: R -- States represent domain intent."
  (:require [hive.events.fsm :as fsm]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handlers (pure functions: resources x data -> data')
;;
;; EDN handler-map keys: :start, :scope-resolve, :query-memory, :gather-context,
;;                       :enrich-kg, :maintenance, :deliver, :end, :error
;; =============================================================================

(defn handle-start
  "Check prerequisites and initialize state.
   EDN handler key: :start

   Checks if Chroma is configured via :chroma-check-fn resource.
   Sets :chroma-configured? for the dispatch predicate."
  [resources data]
  (let [chroma-check-fn (or (:chroma-check-fn resources) (constantly false))
        directory (or (:directory data) (:directory resources))]
    (assoc data
           :directory directory
           :chroma-configured? (boolean (chroma-check-fn))
           :error nil)))

(defn handle-scope-resolve
  "Resolve project scope from directory.
   EDN handler key: :scope-resolve

   Uses resources:
     :scope-fn        (fn [directory] -> project-id)
     :project-name-fn (fn [directory] -> string)
     :build-scopes-fn (fn [project-name project-id] -> scopes)"
  [resources data]
  (let [{:keys [scope-fn project-name-fn build-scopes-fn]} resources
        directory (:directory data)
        project-id (when (and scope-fn directory) (scope-fn directory))
        project-name (when (and project-name-fn directory) (project-name-fn directory))
        scopes (when build-scopes-fn (build-scopes-fn project-name project-id))]
    (assoc data
           :project-id project-id
           :project-name project-name
           :scopes scopes)))

(defn handle-query-memory
  "Query all Chroma memory categories with project scoping.
   EDN handler key: :query-memory

   Uses resources:
     :query-fn             (fn [type tags project-id limit] -> entries)
     :query-axioms-fn      (fn [project-id] -> entries)
     :query-conventions-fn (fn [project-id axiom-ids priority-ids] -> entries)
     :query-expiring-fn    (fn [project-id limit] -> entries)"
  [resources data]
  (let [{:keys [query-fn query-axioms-fn query-conventions-fn query-expiring-fn]} resources
        project-id (:project-id data)]
    (try
      (let [axioms (when query-axioms-fn (query-axioms-fn project-id))
            priority-conventions (when query-fn
                                   (query-fn "convention" ["catchup-priority"] project-id 50))
            sessions (when query-fn
                       (query-fn "note" ["session-summary"] project-id 10))
            decisions (when query-fn
                        (query-fn "decision" nil project-id 50))
            conventions (when query-conventions-fn
                          (query-conventions-fn project-id
                                               (set (map :id axioms))
                                               (set (map :id priority-conventions))))
            snippets (when query-fn
                       (query-fn "snippet" nil project-id 20))
            expiring (when query-expiring-fn
                       (query-expiring-fn project-id 20))]
        (assoc data
               :axioms (or axioms [])
               :priority-conventions (or priority-conventions [])
               :sessions (or sessions [])
               :decisions (or decisions [])
               :conventions (or conventions [])
               :snippets (or snippets [])
               :expiring (or expiring [])
               :query-failed? false))
      (catch Exception e
        (assoc data
               :query-failed? true
               :error (str "Query failed: " (.getMessage e)))))))

(defn handle-gather-context
  "Gather git info and transform entries to metadata.
   EDN handler key: :gather-context

   Uses resources:
     :git-fn          (fn [directory] -> git-info)
     :entry->meta-fns {:axiom fn, :priority fn, :catchup fn}"
  [resources data]
  (let [{:keys [git-fn entry->meta-fns]} resources
        directory (:directory data)
        git-info (when (and git-fn directory) (git-fn directory))

        ;; Extract transform functions with identity fallbacks
        axiom-meta-fn (or (:axiom entry->meta-fns) identity)
        priority-meta-fn (or (:priority entry->meta-fns) identity)
        catchup-meta-fn (or (:catchup entry->meta-fns) identity)]
    (assoc data
           :git-info git-info
           :axioms-meta (mapv axiom-meta-fn (:axioms data))
           :priority-meta (mapv priority-meta-fn (:priority-conventions data))
           :sessions-meta (mapv catchup-meta-fn (:sessions data))
           :snippets-meta (mapv catchup-meta-fn (:snippets data))
           :expiring-meta (mapv catchup-meta-fn (:expiring data))
           ;; decisions and conventions will be enriched in enrich-kg
           :decisions-base (mapv catchup-meta-fn (:decisions data))
           :conventions-base (mapv catchup-meta-fn (:conventions data)))))

(defn handle-enrich-kg
  "Enrich decisions/conventions with KG relationships and gather insights.
   EDN handler key: :enrich-kg

   Uses resources:
     :kg-enrich-fn   (fn [entries] -> {:entries [...] :kg-count N})
     :kg-insights-fn (fn [decisions conventions sessions project-id] -> map)
     :co-access-fn   (fn [entry-ids exclude-ids] -> suggestions)"
  [resources data]
  (let [{:keys [kg-enrich-fn kg-insights-fn co-access-fn]} resources

        ;; KG-enrich decisions and conventions
        decisions-enriched (if kg-enrich-fn
                             (:entries (kg-enrich-fn (:decisions-base data)))
                             (:decisions-base data))
        conventions-enriched (if kg-enrich-fn
                               (:entries (kg-enrich-fn (:conventions-base data)))
                               (:conventions-base data))

        ;; Gather KG insights
        kg-insights (when kg-insights-fn
                      (kg-insights-fn decisions-enriched conventions-enriched
                                      (:sessions-meta data) (:project-id data)))

        ;; Co-access suggestions
        all-entry-ids (mapv :id (concat (:axioms data) (:priority-conventions data)
                                        (:decisions data) (:conventions data)
                                        (:sessions data)))
        co-access-suggestions (when co-access-fn
                                (co-access-fn all-entry-ids all-entry-ids))
        kg-insights (if (seq co-access-suggestions)
                      (assoc (or kg-insights {}) :co-access-suggestions co-access-suggestions)
                      kg-insights)]
    (assoc data
           :decisions-meta (or decisions-enriched [])
           :conventions-meta (or conventions-enriched [])
           :kg-insights kg-insights)))

(defn handle-maintenance
  "Run maintenance tasks: auto-permeate wraps, project tree scan, disc decay.
   EDN handler key: :maintenance

   Uses resources:
     :permeate-fn   (fn [directory] -> {:permeated N :agents [...]})
     :tree-scan-fn  (fn [directory] -> scan-result)
     :disc-decay-fn (fn [project-id] -> decay-stats)"
  [resources data]
  (let [{:keys [permeate-fn tree-scan-fn disc-decay-fn]} resources
        directory (:directory data)
        project-id (:project-id data)

        ;; Auto-permeate pending ling wraps
        permeation (when permeate-fn
                     (try (permeate-fn directory)
                          (catch Exception _e
                            {:permeated 0 :error "permeation failed"})))

        ;; HCR Wave 2: Project tree scan
        project-tree-scan (when tree-scan-fn
                            (try (tree-scan-fn (or directory "."))
                                 (catch Exception _e
                                   {:scanned false :error "tree scan failed"})))

        ;; Disc certainty time-decay
        disc-decay (when disc-decay-fn
                     (try (disc-decay-fn project-id)
                          (catch Exception _e
                            {:updated 0 :skipped 0 :errors 1})))]
    (assoc data
           :permeation (or permeation {:permeated 0})
           :project-tree-scan project-tree-scan
           :disc-decay disc-decay)))

(defn handle-deliver
  "Enqueue piggyback entries and cache in context-store.
   EDN handler key: :deliver

   Uses resources:
     :piggyback-fn      (fn [agent-id project-id entries refs] -> nil)
     :context-store-fn  (fn [data tags ttl] -> ctx-id)"
  [resources data]
  (let [{:keys [piggyback-fn context-store-fn]} resources
        project-id (:project-id data)
        catchup-ttl 600000

        ;; Build piggyback entries: axioms first, then priority conventions
        piggyback-entries (into (vec (:axioms data)) (:priority-conventions data))

        ;; Dual-write: Cache entry categories in context-store
        context-refs
        (when context-store-fn
          (try
            (let [store-category (fn [category entries-data]
                                   (when (seq entries-data)
                                     (context-store-fn
                                      entries-data
                                      #{"catchup" (name category) (or project-id "global")}
                                      catchup-ttl)))]
              (cond-> {}
                (seq (:axioms data))
                (assoc :axioms (store-category :axioms (:axioms data)))
                (seq (:priority-conventions data))
                (assoc :priority-conventions (store-category :priority-conventions (:priority-conventions data)))
                (seq (:sessions data))
                (assoc :sessions (store-category :sessions (:sessions data)))
                (seq (:decisions data))
                (assoc :decisions (store-category :decisions (:decisions data)))
                (seq (:conventions data))
                (assoc :conventions (store-category :conventions (:conventions data)))
                (seq (:snippets data))
                (assoc :snippets (store-category :snippets (:snippets data)))))
            (catch Exception _e nil)))

        ;; Piggyback enqueue
        piggyback-agent-id (if project-id
                             (str "coordinator-" project-id)
                             "coordinator")
        _ (when (and piggyback-fn (seq piggyback-entries))
            (piggyback-fn piggyback-agent-id project-id piggyback-entries context-refs))]
    (assoc data
           :context-refs context-refs
           :piggyback-enqueued? (boolean (seq piggyback-entries)))))

(defn handle-end
  "Terminal state handler. Format and return final catchup response.
   EDN handler key: :end"
  [resources {:keys [data]}]
  (let [build-response-fn (:build-response-fn resources)]
    (if build-response-fn
      (build-response-fn data)
      ;; Default: return summary data
      (select-keys data [:project-id :project-name :scopes :git-info
                         :permeation :context-refs :piggyback-enqueued?
                         :axioms-meta :priority-meta :sessions-meta
                         :decisions-meta :conventions-meta :snippets-meta
                         :expiring-meta :kg-insights
                         :project-tree-scan :disc-decay]))))

(defn handle-error
  "Error state handler. Captures error context.
   EDN handler key: :error"
  [resources {:keys [error data] :as _fsm}]
  (let [error-response-fn (:error-response-fn resources)]
    (if error-response-fn
      (error-response-fn (or error (:error data)))
      (throw (ex-info "Catchup workflow error"
                      {:data (select-keys data [:directory :project-id :error])
                       :error error})))))

;; =============================================================================
;; Handler Map (for EDN spec registration in workflow registry)
;; =============================================================================

(def handler-map
  "Maps EDN keyword handlers to implementation functions.
   Used by registry/register-handlers! for EDN spec compilation."
  {:start         handle-start
   :scope-resolve handle-scope-resolve
   :query-memory  handle-query-memory
   :gather-context handle-gather-context
   :enrich-kg     handle-enrich-kg
   :maintenance   handle-maintenance
   :deliver       handle-deliver
   :end           handle-end
   :error         handle-error})

;; =============================================================================
;; In-Code FSM Spec (inline functions, no EDN needed)
;; =============================================================================

(defn always [_data] true)

(def catchup-session-spec
  "hive-events FSM spec for the catchup session workflow.
   Uses inline functions -- no handler-map needed at compile time.

   State graph:
   ```
   ::fsm/start --> ::scope-resolve -+--> ::query-memory --> ::gather-context
                                    |        |
                                    |        +--> ::error (query failed)
                                    |
                                    +--> ::error (no chroma / no project-id)
   ::gather-context --> ::enrich-kg --> ::maintenance --> ::deliver --> ::end
   ```"
  {:fsm
   {::fsm/start
    {:handler    handle-start
     :dispatches [[::scope-resolve (fn [data] (true? (:chroma-configured? data)))]
                  [::fsm/error (fn [data] (not (:chroma-configured? data)))]]}

    ::scope-resolve
    {:handler    handle-scope-resolve
     :dispatches [[::query-memory (fn [data] (some? (:project-id data)))]
                  [::fsm/error always]]}

    ::query-memory
    {:handler    handle-query-memory
     :dispatches [[::gather-context (fn [data] (not (:query-failed? data)))]
                  [::fsm/error (fn [data] (true? (:query-failed? data)))]]}

    ::gather-context
    {:handler    handle-gather-context
     :dispatches [[::enrich-kg always]]}

    ::enrich-kg
    {:handler    handle-enrich-kg
     :dispatches [[::maintenance always]]}

    ::maintenance
    {:handler    handle-maintenance
     :dispatches [[::deliver always]]}

    ::deliver
    {:handler    handle-deliver
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

(defn compile-catchup
  "Compile the catchup session FSM spec. Call once, reuse the compiled FSM."
  []
  (fsm/compile catchup-session-spec))

(defn run-catchup
  "Execute a compiled catchup session FSM.

   Args:
     compiled-fsm -- Result of compile-catchup
     resources    -- Map of side-effect functions and config
     opts         -- Optional initial data overrides

   Returns:
     Final data map with catchup results."
  ([compiled-fsm resources]
   (run-catchup compiled-fsm resources {}))
  ([compiled-fsm resources opts]
   (fsm/run compiled-fsm
            resources
            {:data (merge {:directory nil
                           :chroma-configured? false
                           :query-failed? false}
                          opts)})))

(defn run-catchup-session
  "Convenience: compile and run a single catchup session.

   Example:
   ```clojure
   (run-catchup-session
     {:chroma-check-fn  chroma/embedding-configured?
      :scope-fn         scope/get-current-project-id
      :project-name-fn  catchup-scope/get-current-project-name
      :query-fn         catchup-scope/query-scoped-entries
      :query-axioms-fn  catchup-scope/query-axioms
      :query-conventions-fn catchup-scope/query-regular-conventions
      :query-expiring-fn catchup-scope/query-expiring-entries
      :git-fn           catchup-git/gather-git-info
      :entry->meta-fns  {:axiom fmt/entry->axiom-meta
                          :priority fmt/entry->priority-meta
                          :catchup #(fmt/entry->catchup-meta % 80)}
      :kg-enrich-fn     enrichment/enrich-entries-with-kg
      :kg-insights-fn   enrichment/gather-kg-insights
      :co-access-fn     enrichment/find-co-accessed-suggestions
      :permeate-fn      permeation/auto-permeate-wraps
      :tree-scan-fn     project-tree/maybe-scan-project-tree!
      :disc-decay-fn    #(disc/apply-time-decay-to-all-discs! :project-id %)
      :piggyback-fn     memory-piggyback/enqueue!
      :context-store-fn context-store/context-put!
      :build-scopes-fn  fmt/build-scopes
      :build-response-fn fmt/build-catchup-response
      :error-response-fn fmt/catchup-error}
     {:directory \"/home/user/project\"})
   ```"
  ([resources]
   (run-catchup-session resources {}))
  ([resources opts]
   (run-catchup (compile-catchup) resources opts)))
