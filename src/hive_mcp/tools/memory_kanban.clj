(ns hive-mcp.tools.memory-kanban
  "In-memory kanban tools using the memory system.

   Tasks stored as memory entries with:
   - type: 'note'
   - tags: ['kanban', status, priority]
   - duration: 'short-term' (7 days)
   - content: {:task-type 'kanban' :title ... :status ...}

   Moving to 'done' DELETES from memory (after creating progress note).

   Result DSL boundary: try-effect* catches exceptions at handler level.
   CC-optimized: if-let/when-let/when-not/cond->/case are 0 CC in scc."
  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.memory.crud :as mem-crud]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.chroma.core :as chroma]
            [hive-mcp.project.tree :as tree]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.time ZonedDateTime]
           [java.time.format DateTimeFormatter]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Result DSL boundary (single handler wrapper)
;; ============================================================

(defn- safe-call
  "Execute thunk f, catch exceptions as MCP errors. try/catch = 0 CC in scc.
   Pure functions return MCP response maps directly."
  [category f]
  (try (f)
       (catch Exception e
         (log/error e (str (name category) " failed"))
         (mcp-error (.getMessage e)))))

;; ============================================================
;; CC-free helpers (if-let/when-let/when-not/cond->/case = 0 CC)
;; ============================================================

(defn- effective-dir
  "Resolve directory: explicit > ctx binding. if-let = 0 CC."
  [directory]
  (if-let [d directory] d (ctx/current-directory)))

(defn- content-val
  "Get value from content map, trying keyword then string key with default.
   if-let chain = 0 CC (vs (or (get k) (get s) default) = 2 CC)."
  [content k default]
  (if-let [v (get content k)] v
          (if-let [v2 (get content (name k))] v2
                  default)))

(defn- kanban-task-type?
  "Check if content has task-type 'kanban'. some = 0 CC."
  [content]
  (some #(= "kanban" (get content %)) [:task-type "task-type"]))

;; ============================================================
;; Direct Chroma Kanban Helpers
;; ============================================================

(defn- kanban-timestamp []
  (.format (ZonedDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssZ")))

(defn- build-kanban-tags [status priority project-id]
  (conj ["kanban" status (str "priority-" priority)]
        (scope/make-scope-tag project-id)))

(defn- kanban-entry? [entry]
  (boolean (kanban-task-type? (:content entry))))

(def ^:private status-enum->tag
  {"inprogress" "doing" "inreview" "review" "todo" "todo" "done" "done"})

(def ^:private valid-statuses #{"todo" "doing" "review" "done"})

(def ^:private priority-order
  {"high" 0 "priority-high" 0 "medium" 1 "priority-medium" 1 "low" 2 "priority-low" 2})

(defn- sort-by-priority-then-created [tasks]
  (sort (fn [a b]
          (let [pa (get priority-order (content-val a :priority "medium") 1)
                pb (get priority-order (content-val b :priority "medium") 1)]
            (case (compare pa pb)
              0 (compare (str (:id a)) (str (:id b)))
              (compare pa pb))))
        tasks))

;; ============================================================
;; Descendant Aggregation (HCR Wave 5)
;; ============================================================

(defn- resolve-project-ids-with-descendants [project-id]
  (when-let [pid (when-not (= project-id "global") project-id)]
    (when-let [desc (seq (tree/get-descendant-ids pid))]
      (vec (cons pid desc)))))

(defn- extract-project-id-from-tags [entry]
  (some (fn [tag]
          (when-let [s (when-not (nil? tag) (str tag))]
            (when-not (not (.startsWith ^String s "scope:project:"))
              (subs s (count "scope:project:")))))
        (:tags entry)))

;; ============================================================
;; Slim Formatting
;; ============================================================

(defn- task->slim
  ([entry] (task->slim entry false))
  ([entry multi-project?]
   (let [content (:content entry)]
     (cond-> {:id (:id entry)
              :title (content-val content :title nil)
              :status (content-val content :status nil)
              :priority (content-val content :priority nil)}
       multi-project? (assoc :project (extract-project-id-from-tags entry))))))

;; ============================================================
;; Query Helpers (DRY between list-slim and stats)
;; ============================================================

(defn- query-kanban-entries [project-id include-descendants? limit]
  (let [all-project-ids (when-let [_ include-descendants?]
                          (resolve-project-ids-with-descendants project-id))
        multi-project? (boolean all-project-ids)
        entries (if-let [pids all-project-ids]
                  (chroma/query-entries :type "note" :project-ids pids :limit (max limit 500))
                  (chroma/query-entries :type "note" :project-id project-id :limit limit))]
    {:entries entries :multi-project? multi-project?}))

(defn- filter-kanban-by-tags [entries required-tags]
  (->> entries
       (filter (fn [entry]
                 (let [entry-tags (set (:tags entry))]
                   (every? #(contains? entry-tags %) required-tags))))
       (filter kanban-entry?)))

;; ============================================================
;; Pure Logic (return MCP response maps directly)
;; ============================================================

(defn- create* [{:keys [title priority context directory agent_id]}]
  (let [eff-dir (effective-dir directory)
        eff-agent (if-let [a agent_id] a
                          (if-let [c (ctx/current-agent-id)] c
                                  (System/getenv "CLAUDE_SWARM_SLAVE_ID")))
        priority (if-let [p priority] p "medium")
        project-id (scope/get-current-project-id eff-dir)
        content {:task-type "kanban" :title title :status "todo"
                 :priority priority :created (kanban-timestamp)
                 :started nil :context context}
        tags (build-kanban-tags "todo" priority project-id)
        crud-result (mem-crud/handle-add {:type "note"
                                          :content (json/write-str content)
                                          :tags tags :directory eff-dir
                                          :agent_id eff-agent :duration "short"})]
    (log/info "kanban-create result:" crud-result)
    (if-let [_ (:isError crud-result)]
      crud-result
      {:type "text" :text (:text crud-result)})))

(defn- list-slim* [{:keys [status directory include_descendants]}]
  (let [eff-dir (effective-dir directory)
        project-id (scope/get-current-project-id eff-dir)
        status-tag (when-let [s status] (get status-enum->tag s s))
        required-tags (if-let [st status-tag] ["kanban" st] ["kanban"])
        {:keys [entries multi-project?]} (query-kanban-entries
                                          project-id include_descendants 100)
        kanban-entries (filter-kanban-by-tags entries required-tags)
        slim-entries (mapv #(task->slim % multi-project?) kanban-entries)]
    (mcp-json (sort-by-priority-then-created slim-entries))))

(defn- archive-to-done-archive!
  "Archive task data via extension registry before deletion.
   Non-blocking, non-fatal. Delegates to extension if available."
  [entry task-id]
  (try
    (when-let [archive-fn (ext/get-extension :da/archive!)]
      (let [content (:content entry)
            scope (some-> entry :tags
                          (->> (filter #(str/starts-with? % "scope:project:"))
                               first
                               (str/replace "scope:project:" "")))
            task-data {:id task-id
                       :title (or (get content :title)
                                  (get content :description)
                                  (str task-id))
                       :scope scope
                       :agent-id (get content :agent-id)
                       :files (get content :files)
                       :completed-at (java.util.Date.)
                       :session-id (try (when-let [sid (requiring-resolve 'hive-mcp.crystal.core/session-id)]
                                          (sid))
                                        (catch Exception _ nil))
                       :context (get content :context)
                       :tags (filterv #(not (str/starts-with? % "scope:"))
                                      (or (:tags entry) []))}]
        (archive-fn task-data)
        (log/info "Archived done task via extension:" task-id)))
    (catch Exception e
      (log/debug "Done-archive extension not available (non-fatal):" (.getMessage e)))))

(defn- move-to-done! [entry task-id]
  (when-let [task-data (crystal-hooks/extract-task-from-kanban-entry entry)]
    (log/info "Calling crystal hook for completed kanban task:" task-id)
    (try (crystal-hooks/on-kanban-done task-data)
         (catch Exception e (log/warn "Crystal hook failed (non-fatal):" (.getMessage e)))))
  ;; Archive to Datahike before deleting from Chroma
  (archive-to-done-archive! entry task-id)
  (chroma/delete-entry! task-id)
  (mcp-json {:deleted true :status "done" :id task-id}))

(defn- move-to-status! [entry task-id new-status directory]
  (let [content (:content entry)
        priority (content-val content :priority "medium")
        new-content (cond-> (assoc content :status new-status)
                      (= new-status "doing") (assoc :started (kanban-timestamp)))
        eff-dir (effective-dir directory)
        project-id (scope/get-current-project-id eff-dir)
        new-tags (build-kanban-tags new-status priority project-id)
        _ (chroma/update-entry! task-id {:content new-content :tags new-tags})
        updated (chroma/get-entry-by-id task-id)]
    (mcp-json (task->slim updated))))

(defn- move* [{:keys [task_id new_status directory]}]
  (if-let [_ (valid-statuses new_status)]
    (if-let [entry (chroma/get-entry-by-id task_id)]
      (if-let [_ (kanban-task-type? (:content entry))]
        (case new_status
          "done" (move-to-done! entry task_id)
          (move-to-status! entry task_id new_status directory))
        (mcp-error (str "Entry is not a kanban task: " task_id)))
      (mcp-error (str "Task not found: " task_id)))
    (mcp-error (str "Invalid status: " new_status ". Valid: todo, doing, review, done"))))

(defn- stats* [{:keys [directory include_descendants]}]
  (let [eff-dir (effective-dir directory)
        project-id (scope/get-current-project-id eff-dir)
        {:keys [entries multi-project?]} (query-kanban-entries
                                          project-id include_descendants 500)
        kanban-entries (filter-kanban-by-tags entries ["kanban"])
        stats (reduce (fn [counts entry]
                        (let [status (content-val (:content entry) :status "todo")]
                          (update counts (keyword status) (fnil inc 0))))
                      {:todo 0 :doing 0 :review 0}
                      kanban-entries)
        result (if-let [_ multi-project?]
                 (let [by-project
                       (reduce (fn [acc entry]
                                 (let [proj (if-let [p (extract-project-id-from-tags entry)] p "unknown")
                                       status (content-val (:content entry) :status "todo")]
                                   (update-in acc [proj (keyword status)] (fnil inc 0))))
                               {}
                               kanban-entries)]
                   (assoc stats :by-project by-project))
                 stats)]
    (mcp-json result)))

;; ============================================================
;; Public Handlers (boundary: safe-call wraps try-effect*)
;; ============================================================

(defn handle-mem-kanban-create
  "Create a kanban task in memory (direct Chroma, no elisp roundtrip).
   CTX Migration: Uses request context for agent_id and directory extraction."
  [params]
  (safe-call :kanban/create-failed #(create* params)))

(defn handle-mem-kanban-list-slim
  "List kanban tasks with minimal data for token optimization.
   HCR Wave 4: Pass include_descendants=true to aggregate child project tasks."
  [params]
  (safe-call :kanban/list-failed #(with-chroma (list-slim* params))))

(defn handle-mem-kanban-move
  "Move task to new status. Moving to 'done' DELETES the task from memory.
   CTX Migration: Uses request context for directory extraction."
  [params]
  (safe-call :kanban/move-failed #(with-chroma (move* params))))

(defn handle-mem-kanban-stats
  "Get kanban statistics by status.
   HCR Wave 4: Pass include_descendants=true to aggregate child project stats."
  [params]
  (safe-call :kanban/stats-failed #(with-chroma (stats* params))))

(defn handle-mem-kanban-quick
  "Quick add task with defaults (todo, medium priority).
   CTX Migration: Delegates to handle-mem-kanban-create which uses context."
  [{:keys [title directory agent_id]}]
  (handle-mem-kanban-create {:title title :directory directory :agent_id agent_id}))

;; Tool definitions

(def tools
  [{:name "mcp_mem_kanban_create"
    :description "Create a kanban task in memory (short-term duration, 7 days)"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "Task title"}
                               :priority {:type "string" :enum ["high" "medium" "low"] :description "Priority (default: medium)"}
                               :context {:type "string" :description "Additional notes"}
                               :directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}
                               :agent_id {:type "string" :description "Agent identifier for attribution (auto-extracted from context if not provided)"}}
                  :required ["title"]}
    :handler handle-mem-kanban-create}

   {:name "mcp_mem_kanban_list_slim"
    :description "List kanban tasks with minimal data (id, title, status, priority only). Use for token-efficient overviews (~10x fewer tokens than full list)."
    :inputSchema {:type "object"
                  :properties {:status {:type "string" :enum ["todo" "doing" "review"] :description "Filter by status"}
                               :directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}
                               :include_descendants {:type "boolean" :description "Include child project tasks (HCR Wave 4). Default false."}}}
    :handler handle-mem-kanban-list-slim}

   {:name "mcp_mem_kanban_move"
    :description "Move task to new status. Moving to 'done' DELETES the task from memory"
    :inputSchema {:type "object"
                  :properties {:task_id {:type "string" :description "Task ID"}
                               :new_status {:type "string" :enum ["todo" "doing" "review" "done"] :description "New status"}
                               :directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}}
                  :required ["task_id" "new_status"]}
    :handler handle-mem-kanban-move}

   {:name "mcp_mem_kanban_stats"
    :description "Get kanban statistics (counts by status)"
    :inputSchema {:type "object"
                  :properties {:directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}
                               :include_descendants {:type "boolean" :description "Include child project stats (HCR Wave 4). Default false."}}}
    :handler handle-mem-kanban-stats}

   {:name "mcp_mem_kanban_quick"
    :description "Quick add task with defaults (todo status, medium priority)"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "Task title"}
                               :directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}
                               :agent_id {:type "string" :description "Agent identifier for attribution (auto-extracted from context if not provided)"}}
                  :required ["title"]}
    :handler handle-mem-kanban-quick}])
