(ns hive-mcp.tools.memory-kanban
  "In-memory kanban tools using the memory system.

   Tasks stored as memory entries with:
   - type: 'note'
   - tags: ['kanban', status, priority]
   - duration: 'short-term' (7 days)
   - content: {:task-type 'kanban' :title ... :status ...}

   Moving to 'done' DELETES from memory (after creating progress note)."
  (:require [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.tools.memory.crud :as mem-crud]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.project.tree :as tree]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log])
  (:import [java.time ZonedDateTime]
           [java.time.format DateTimeFormatter]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Direct Chroma Kanban (bypasses elisp roundtrip)
;; ============================================================

(defn- kanban-timestamp
  "Generate ISO timestamp for kanban task."
  []
  (.format (ZonedDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssZ")))

(defn- build-kanban-tags
  "Build tags for kanban task: ['kanban', status, priority, scope].
   Always includes a scope tag using scope/make-scope-tag for consistency.
   This ensures 'global' project-id produces 'scope:global' not 'scope:project:global'."
  [status priority project-id]
  (conj ["kanban" status (str "priority-" priority)]
        (scope/make-scope-tag project-id)))

(defn handle-mem-kanban-create
  "Create a kanban task in memory (direct Chroma, no elisp roundtrip).
   When directory is provided, scopes task to that project.

   CTX Migration: Uses request context for agent_id and directory extraction.
   Fallback chain: explicit param → ctx binding → env var → default."
  [{:keys [title priority context directory agent_id]}]
  (try
    ;; CTX: Apply fallback chain for directory and agent_id
    (let [effective-dir (or directory (ctx/current-directory))
          effective-agent (or agent_id
                              (ctx/current-agent-id)
                              (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
          priority (or priority "medium")
          status "todo"
          ;; Use scope/get-current-project-id for consistent project-id derivation
          ;; Returns "global" when effective-dir is nil, preventing scope leakage
          project-id (scope/get-current-project-id effective-dir)
          content {:task-type "kanban"
                   :title title
                   :status status
                   :priority priority
                   :created (kanban-timestamp)
                   :started nil
                   :context context}
          tags (build-kanban-tags status priority project-id)
          ;; Call memory CRUD directly - no elisp roundtrip
          ;; Pass effective-agent for agent tag attribution
          result (mem-crud/handle-add {:type "note"
                                       :content (json/write-str content)
                                       :tags tags
                                       :directory effective-dir
                                       :agent_id effective-agent
                                       :duration "short"})]
      (log/info "kanban-create result:" result)
      (if (:isError result)
        result
        {:type "text" :text (:text result)}))
    (catch Exception e
      (log/error e "kanban-create failed")
      {:type "text" :text (str "Error: " (.getMessage e)) :isError true})))

;; ============================================================
;; Descendant Aggregation Helpers (HCR Wave 5)
;; ============================================================

(defn- resolve-project-ids-with-descendants
  "Resolve project-id to a vector including all descendant project-ids.
   When project has children in the hierarchy, automatically includes them.
   Returns nil when no descendants (caller should use singular :project-id)."
  [project-id]
  (when (and project-id (not= project-id "global"))
    (let [descendant-ids (tree/get-descendant-ids project-id)]
      (when (seq descendant-ids)
        (vec (cons project-id descendant-ids))))))

(defn- extract-project-id-from-tags
  "Extract project-id from entry tags.
   Looks for scope:project:X tag and extracts X."
  [entry]
  (some (fn [tag]
          (when (and (string? tag) (.startsWith ^String tag "scope:project:"))
            (subs tag (count "scope:project:"))))
        (:tags entry)))

;; ============================================================
;; Slim/Metadata Formatting (Token Optimization)
;; ============================================================

(defn- task->slim
  "Convert kanban task to slim format.
   Returns only id, title, status, priority - strips context bloat.
   When multi-project is true, includes :project field for disambiguation.
   ~10x fewer tokens than full entry."
  ([entry] (task->slim entry false))
  ([entry multi-project?]
   (let [content (:content entry)]
     (cond-> {:id (:id entry)
              :title (get content :title (get content "title"))
              :status (get content :status (get content "status"))
              :priority (get content :priority (get content "priority"))}
       multi-project? (assoc :project (extract-project-id-from-tags entry))))))

(defn- kanban-entry?
  "Check if entry is a kanban task."
  [entry]
  (let [content (:content entry)]
    (= "kanban" (or (get content :task-type) (get content "task-type")))))

(def ^:private status-enum->tag
  "Map API enum values to actual tag values.
   API uses 'inprogress'/'inreview' but tags are 'doing'/'review'."
  {"inprogress" "doing"
   "inreview"   "review"
   "todo"       "todo"
   "done"       "done"})

(defn handle-mem-kanban-list-slim
  "List kanban tasks with minimal data for token optimization.
   Returns only id, title, status, priority per entry (~10x fewer tokens).
   When directory is provided, scopes query to that project.

   HCR Wave 5: Automatically aggregates descendant project tasks when
   querying from a parent project. Adds :project field to entries when
   results span multiple projects.

   CTX Migration: Uses request context for directory extraction.
   Fallback chain: explicit param → ctx binding.

   Queries Chroma directly (bypasses elisp roundtrip) to be consistent
   with handle-mem-kanban-create which stores directly in Chroma."
  [{:keys [status directory]}]
  (try
    (let [effective-dir (or directory (ctx/current-directory))]
      (with-chroma
        (let [project-id (scope/get-current-project-id effective-dir)
              ;; HCR Wave 5: Check for descendant projects
              all-project-ids (resolve-project-ids-with-descendants project-id)
              multi-project? (boolean all-project-ids)
              ;; Normalize status enum to actual tag value
              status-tag (when status (get status-enum->tag status status))
              ;; Build tag filter - always include "kanban", optionally status
              required-tags (if status-tag ["kanban" status-tag] ["kanban"])
              ;; Query from Chroma - use :project-ids when aggregating descendants
              entries (if all-project-ids
                        (chroma/query-entries :type "note"
                                              :project-ids all-project-ids
                                              :limit 500)
                        (chroma/query-entries :type "note"
                                              :project-id project-id
                                              :limit 100))
              ;; Filter by required tags
              tag-filtered (filter (fn [entry]
                                     (let [entry-tags (set (:tags entry))]
                                       (every? #(contains? entry-tags %) required-tags)))
                                   entries)
              ;; Filter to only kanban entries
              kanban-entries (filter kanban-entry? tag-filtered)
              ;; Convert to slim format (with project label when multi-project)
              slim-entries (mapv #(task->slim % multi-project?) kanban-entries)]
          {:type "text" :text (json/write-str slim-entries)})))
    (catch Exception e
      (log/error e "kanban-list-slim failed")
      {:type "text" :text (str "Error: " (.getMessage e)) :isError true})))

(defn handle-mem-kanban-move
  "Move task to new status. When moving to 'done':
   1. Fetches the task entry first
   2. Calls crystal hook to create progress note
   3. Then deletes the kanban entry
   When directory is provided, scopes operation to that project.

   CTX Migration: Uses request context for directory extraction.
   Fallback chain: explicit param → ctx binding.

   Queries/updates Chroma directly (bypasses elisp roundtrip) to be consistent
   with handle-mem-kanban-create which stores directly in Chroma."
  [{:keys [task_id new_status directory]}]
  (let [valid-statuses #{"todo" "doing" "review" "done"}]
    (if-not (valid-statuses new_status)
      {:type "text" :text (str "Invalid status: " new_status ". Valid: todo, doing, review, done") :isError true}
      (try
        (with-chroma
          (if-let [entry (chroma/get-entry-by-id task_id)]
            (if-not (kanban-entry? entry)
              {:type "text" :text (str "Entry is not a kanban task: " task_id) :isError true}
              (if (= new_status "done")
                ;; Special handling for DONE: crystal hook then delete
                (let [task-data (crystal-hooks/extract-task-from-kanban-entry entry)]
                  ;; Call crystal hook to create progress note
                  (when task-data
                    (log/info "Calling crystal hook for completed kanban task:" task_id)
                    (try
                      (crystal-hooks/on-kanban-done task-data)
                      (catch Exception e
                        (log/warn "Crystal hook failed (non-fatal):" (.getMessage e)))))
                  ;; Delete the entry
                  (chroma/delete-entry! task_id)
                  {:type "text" :text (json/write-str {:deleted true :status "done" :id task_id})})
                ;; Normal status change
                (let [content (:content entry)
                      priority (or (get content :priority) (get content "priority") "medium")
                      ;; Update content with new status
                      new-content (-> content
                                      (assoc :status new_status)
                                      ;; Set :started timestamp when moving to "doing"
                                      (cond-> (= new_status "doing")
                                        (assoc :started (kanban-timestamp))))
                      ;; Rebuild tags with new status
                      effective-dir (or directory (ctx/current-directory))
                      project-id (scope/get-current-project-id effective-dir)
                      new-tags (build-kanban-tags new_status priority project-id)
                      ;; Update the entry
                      _ (chroma/update-entry! task_id {:content new-content :tags new-tags})
                      updated (chroma/get-entry-by-id task_id)]
                  {:type "text" :text (json/write-str (task->slim updated))})))
            {:type "text" :text (str "Task not found: " task_id) :isError true}))
        (catch Exception e
          (log/error e "kanban-move failed")
          {:type "text" :text (str "Error: " (.getMessage e)) :isError true})))))

(defn handle-mem-kanban-stats
  "Get kanban statistics by status.
   When directory is provided, scopes query to that project.

   HCR Wave 5: Automatically aggregates descendant project stats when
   querying from a parent project. Returns per-project breakdown in
   :by-project field when results span multiple projects.

   CTX Migration: Uses request context for directory extraction.
   Fallback chain: explicit param → ctx binding.

   Queries Chroma directly (bypasses elisp roundtrip) to be consistent
   with handle-mem-kanban-create which stores directly in Chroma."
  [{:keys [directory]}]
  (try
    (let [effective-dir (or directory (ctx/current-directory))]
      (with-chroma
        (let [project-id (scope/get-current-project-id effective-dir)
              ;; HCR Wave 5: Check for descendant projects
              all-project-ids (resolve-project-ids-with-descendants project-id)
              multi-project? (boolean all-project-ids)
              ;; Query all kanban tasks - use :project-ids when aggregating
              entries (if all-project-ids
                        (chroma/query-entries :type "note"
                                              :project-ids all-project-ids
                                              :limit 500)
                        (chroma/query-entries :type "note"
                                              :project-id project-id
                                              :limit 500))
              ;; Filter to kanban entries with "kanban" tag
              kanban-entries (->> entries
                                  (filter #(contains? (set (:tags %)) "kanban"))
                                  (filter kanban-entry?))
              ;; Count by status (aggregate totals)
              stats (reduce (fn [counts entry]
                              (let [content (:content entry)
                                    status (or (get content :status) (get content "status") "todo")]
                                (update counts (keyword status) (fnil inc 0))))
                            {:todo 0 :doing 0 :review 0}
                            kanban-entries)
              ;; HCR Wave 5: Per-project breakdown when multi-project
              result (if multi-project?
                       (let [by-project
                             (reduce (fn [acc entry]
                                       (let [proj (or (extract-project-id-from-tags entry) "unknown")
                                             content (:content entry)
                                             status (or (get content :status) (get content "status") "todo")]
                                         (update-in acc [proj (keyword status)] (fnil inc 0))))
                                     {}
                                     kanban-entries)]
                         (assoc stats :by-project by-project))
                       stats)]
          {:type "text" :text (json/write-str result)})))
    (catch Exception e
      (log/error e "kanban-stats failed")
      {:type "text" :text (str "Error: " (.getMessage e)) :isError true})))

(defn handle-mem-kanban-quick
  "Quick add task with defaults (todo, medium priority).
   When directory is provided, scopes task to that project.

   CTX Migration: Delegates to handle-mem-kanban-create which uses context.
   Passes through directory and agent_id for explicit param support."
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
                               :directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}}}
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
                  :properties {:directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}}}
    :handler handle-mem-kanban-stats}

   {:name "mcp_mem_kanban_quick"
    :description "Quick add task with defaults (todo status, medium priority)"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "Task title"}
                               :directory {:type "string" :description "Working directory to determine project scope (auto-extracted from context if not provided)"}
                               :agent_id {:type "string" :description "Agent identifier for attribution (auto-extracted from context if not provided)"}}
                  :required ["title"]}
    :handler handle-mem-kanban-quick}])
