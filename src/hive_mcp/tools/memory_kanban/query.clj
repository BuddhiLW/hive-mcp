(ns hive-mcp.tools.memory-kanban.query
  (:require [hive-mcp.agent.context :as ctx]
            [hive-mcp.project.tree :as tree]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.tools.kanban.list.plan :as plan]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.kanban.transitions :as kt]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.vectordb.kanban-facade :as kanban-facade]))

(declare query-kanban-entries resolve-project-ids-with-descendants resolve-visible-project-ids effective-dir stats* filter-kanban-by-tags list-slim*)

(defn query-kanban-entries
  "Fetch kanban entries from the underlying memory store.

   Routes via `kanban-facade` so reads honor the `:memory/kanban-store`
   config toggle: `:default` keeps milvus behavior, `:dual-read` merges
   :kanban + :default with kanban-first preference, `:kanban` reads
   only the dedicated qdrant collection.

   `query-tags` are pushed into the store query (server-side AND-filter)
   and `limit` is the store window the caller planned; nothing here widens
   or narrows it. Callers size it with `plan/whole-board`.

   Scope resolution honours the SAME inheritance the rest of the memory
   system uses (knowledge_graph/scope.clj): a list sees its own scope +
   ANCESTORS (UP, always on), plus DESCENDANTS (DOWN) when
   `include-descendants?` is set. See `resolve-visible-project-ids`.

   `opts` (optional) :scope \"all\" lifts the project filter entirely and
   returns the whole board across every workspace."
  ([project-id include-descendants? limit query-tags]
   (query-kanban-entries project-id include-descendants? limit query-tags nil))
  ([project-id include-descendants? limit query-tags {:keys [scope]}]
   (let [all-scopes?    (= scope "all")
         global?        (= project-id "global")
         visible-ids    (when-not (or all-scopes? global?)
                          (resolve-visible-project-ids project-id include-descendants?))
         multi-project? (boolean (or all-scopes? global? (and visible-ids (next visible-ids))))
         entries (cond
                   (or all-scopes? (and global? include-descendants?))
                   (kanban-facade/query-entries :type "note" :tags query-tags
                                                :limit limit)
                   visible-ids
                   (kanban-facade/query-entries :type "note" :tags query-tags
                                                :project-ids (vec visible-ids)
                                                :limit limit)
                   :else
                   (kanban-facade/query-entries :type "note" :tags query-tags
                                                :project-id project-id
                                                :limit limit))]
     {:entries entries :multi-project? multi-project?})))

(defn resolve-project-ids-with-descendants
  "Self + all DESCENDANT project-ids (DOWN-walk via the cached project tree).
   Returns nil for global or for a leaf with no descendants (callers fall back
   to a singular :project-id filter). Retained as the pure descendant helper;
   `resolve-visible-project-ids` composes it with the ancestor chain."
  [project-id]
  (when-let [pid (when-not (= project-id "global") project-id)]
    (when-let [desc (seq (tree/get-descendant-ids pid))]
      (vec (cons pid desc)))))

(defn resolve-visible-project-ids
  "Project-ids visible from `project-id`, honouring the documented scope
   inheritance (knowledge_graph/scope.clj):

     - self + ANCESTORS  (UP — 'child sees parent', ALWAYS) via the same
       `kg/visible-scopes` chain memory queries use; and
     - DESCENDANTS       (DOWN) only when `include-descendants?` is set.

   This is the fix for kanban HCR scope-blindness: the prior code path
   (`resolve-project-ids-with-descendants`) walked DOWN only, so listing from
   a child scope dropped every parent task. Returns nil for global (caller
   handles the no-filter / single-scope branches)."
  [project-id include-descendants?]
  (when (and project-id (not= project-id "global"))
    (let [ancestors   (scope/resolve-scope-chain project-id)   ; [self … "global"]
          descendants (when include-descendants?
                        (seq (tree/get-descendant-ids project-id)))]
      (vec (distinct (concat ancestors descendants))))))

(defn effective-dir [directory]
  (kt/effective-dir directory ctx/current-directory))

(defn stats* [{:keys [include_descendants scope]
                :or {include_descendants true}
                :as params}]
  ;; HCR: explicit :directory > :_caller_cwd (bb-mcp session pwd) >
  ;; request-ctx > server user.dir.
  (let [eff-dir    (ctx/resolve-caller-directory params)
        project-id (scope/get-current-project-id eff-dir)
        {:keys [entries multi-project?]} (query-kanban-entries
                                          project-id include_descendants
                                          plan/whole-board ["kanban"] {:scope scope})
        kanban-entries (plan/select-tagged entries ["kanban"])
        ;; Entries with a missing/invalid status are dropped from the
        ;; bucket counts rather than defaulted to :todo.
        bucket-keys    #{:todo :doing :review :done}
        stats (reduce (fn [counts entry]
                        (let [s (some-> (kt/content-val (:content entry) :status nil)
                                        keyword)]
                          (cond-> counts
                            (contains? bucket-keys s) (update s (fnil inc 0)))))
                      {:todo 0 :doing 0 :review 0 :done 0}
                      kanban-entries)
        result (if multi-project?
                 (let [by-project
                       (reduce (fn [acc entry]
                                 (let [proj (or (kt/extract-project-id-from-tags entry) "unknown")
                                       s (some-> (kt/content-val (:content entry) :status nil)
                                                 keyword)]
                                   (cond-> acc
                                     (contains? bucket-keys s)
                                     (update-in [proj s] (fnil inc 0)))))
                               {}
                               kanban-entries)]
                   (assoc stats :by-project by-project))
                 stats)]
    (mcp-json result)))

(defn filter-kanban-by-tags
  "Kanban entries carrying every tag in `required-tags`."
  [entries required-tags]
  (plan/select-tagged entries required-tags))

(defrecord FacadeBoardSource []
  src/IBoardSource
  (scoped-board [_ {:keys [project-id include-descendants? scope]} {:keys [required-tags window]}]
    (query-kanban-entries project-id include-descendants? window required-tags {:scope scope})))

(def ^:dynamic *board-source*
  "IBoardSource `list-slim*` reads at call time. Rebind to inject a board."
  (->FacadeBoardSource))

(defn list-slim*
  "List kanban tasks with optional token-budget filters.

   Filters (all optional):
   - :status               todo | inprogress | inreview | done (pushed to store)
   - :project_id           explicit project scope override (defaults to dir-resolved)
   - :include_descendants  aggregate child-project tasks (default true)
   - :scope                \"all\" lifts the project filter (whole board)
   - :query                case-insensitive substring on title + description
   - :tags                 extra tag filter beyond [kanban, status]
   - :tag_match            \"all\" (default, AND, pushed to store) or \"any\" (OR, post-filter)
   - :priority             exact: high | medium | low
   - :created_after        ISO-8601 string; entries with content :created > threshold
   - :updated_after        ISO-8601 string; checks :updated/:started/:completed
   - :limit                cap response array size
   - :offset               skip first N (after sort)
   - :fields               seq of field names to project (default = full slim shape)

   Collect (this fn) -> Promote (`plan/plan`) -> Boundary (`*board-source*`)
   -> Promote (`plan/shape`). The store window comes from the plan, so the
   caller's limit, offset and filters can never be cut by a fixed window."
  [{:keys [include_descendants project_id scope]
    :or   {include_descendants true}
    :as   params}]
  ;; HCR: explicit :directory > :_caller_cwd (bb-mcp session pwd) >
  ;; request-ctx > server user.dir.
  (let [eff-dir    (ctx/resolve-caller-directory params)
        scoped-pid (or project_id (scope/get-current-project-id eff-dir))
        fetch-plan (plan/plan params)
        {:keys [entries multi-project?]}
        (src/scoped-board *board-source*
                          {:project-id           scoped-pid
                           :include-descendants? include_descendants
                           :scope                scope}
                          fetch-plan)]
    (mcp-json (plan/shape fetch-plan entries multi-project?))))
