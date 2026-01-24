(ns hive-mcp.tools.memory-kanban
  "In-memory kanban tools using the memory system.
   
   Tasks stored as memory entries with:
   - type: 'note'
   - tags: ['kanban', status, priority]
   - duration: 'short-term' (7 days)
   - content: {:task-type 'kanban' :title ... :status ...}
   
   Moving to 'done' DELETES from memory (after creating progress note)."
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Tool handlers

(defn handle-mem-kanban-create
  "Create a kanban task in memory.
   When directory is provided, scopes task to that project."
  [{:keys [title priority context directory]}]
  (let [priority (or priority "medium")
        elisp (format "(json-encode (hive-mcp-api-kanban-create %s %s %s %s))"
                      (str "\"" (v/escape-elisp-string title) "\"")
                      (str "\"" (v/escape-elisp-string priority) "\"")
                      (if context
                        (str "\"" (v/escape-elisp-string context) "\"")
                        "nil")
                      (if directory
                        (str "\"" (v/escape-elisp-string directory) "\"")
                        "nil"))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

;; ============================================================
;; Slim/Metadata Formatting (Token Optimization)
;; ============================================================

(defn- task->slim
  "Convert kanban task to slim format.
   Returns only id, title, status, priority - strips context bloat.
   ~10x fewer tokens than full entry."
  [entry]
  (let [content (:content entry)]
    {:id (:id entry)
     :title (get content :title (get content "title"))
     :status (get content :status (get content "status"))
     :priority (get content :priority (get content "priority"))}))

(defn handle-mem-kanban-list-slim
  "List kanban tasks with minimal data for token optimization.
   Returns only id, title, status, priority per entry (~10x fewer tokens).
   When directory is provided, scopes query to that project."
  [{:keys [status directory]}]
  (let [elisp (cond
                (and status directory)
                (format "(json-encode (hive-mcp-api-kanban-list %s %s))"
                        (str "\"" (v/escape-elisp-string status) "\"")
                        (str "\"" (v/escape-elisp-string directory) "\""))
                status
                (format "(json-encode (hive-mcp-api-kanban-list %s))"
                        (str "\"" (v/escape-elisp-string status) "\""))
                directory
                (format "(json-encode (hive-mcp-api-kanban-list nil %s))"
                        (str "\"" (v/escape-elisp-string directory) "\""))
                :else
                "(json-encode (hive-mcp-api-kanban-list))")
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (let [entries (try (json/read-str result :key-fn keyword) (catch Exception _ []))
            slim-entries (mapv task->slim entries)]
        {:type "text" :text (json/write-str slim-entries)})
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mem-kanban-move
  "Move task to new status. When moving to 'done':
   1. Fetches the task entry first
   2. Calls crystal hook to create progress note
   3. Then deletes the kanban entry
   When directory is provided, scopes operation to that project."
  [{:keys [task_id new_status directory]}]
  (let [valid-statuses #{"todo" "doing" "review" "done"}
        dir-arg (if directory
                  (str "\"" (v/escape-elisp-string directory) "\"")
                  "nil")]
    (if-not (valid-statuses new_status)
      {:type "text" :text (str "Invalid status: " new_status ". Valid: todo, doing, review, done") :isError true}
      (if (= new_status "done")
        ;; Special handling for DONE: call crystal hook before deletion
        (let [;; 1. Fetch the entry before deletion
              get-elisp (format "(json-encode (hive-mcp-memory-get %s %s))"
                                (str "\"" (v/escape-elisp-string task_id) "\"")
                                dir-arg)
              {:keys [success result error]} (ec/eval-elisp get-elisp)]
          (if-not success
            {:type "text" :text (str "Error fetching task: " error) :isError true}
            (let [;; 2. Parse the entry and extract task data
                  entry (try (json/read-str result :key-fn keyword) (catch Exception _ nil))
                  task-data (when entry (crystal-hooks/extract-task-from-kanban-entry entry))]
              ;; 3. Call crystal hook to create progress note
              (when task-data
                (log/info "Calling crystal hook for completed kanban task:" task_id)
                (try
                  (crystal-hooks/on-kanban-done task-data)
                  (catch Exception e
                    (log/warn "Crystal hook failed (non-fatal):" (.getMessage e)))))
              ;; 4. Proceed with move (which deletes for 'done')
              (let [move-elisp (format "(json-encode (hive-mcp-api-kanban-move %s %s %s))"
                                       (str "\"" (v/escape-elisp-string task_id) "\"")
                                       (str "\"" (v/escape-elisp-string new_status) "\"")
                                       dir-arg)
                    {:keys [success result error]} (ec/eval-elisp move-elisp)]
                (if success
                  {:type "text" :text result}
                  {:type "text" :text (str "Error: " error) :isError true})))))
        ;; Normal status change (not done)
        (let [elisp (format "(json-encode (hive-mcp-api-kanban-move %s %s %s))"
                            (str "\"" (v/escape-elisp-string task_id) "\"")
                            (str "\"" (v/escape-elisp-string new_status) "\"")
                            dir-arg)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            {:type "text" :text result}
            {:type "text" :text (str "Error: " error) :isError true}))))))

(defn handle-mem-kanban-stats
  "Get kanban statistics by status.
   When directory is provided, scopes query to that project."
  [{:keys [directory]}]
  (let [elisp (if directory
                (format "(json-encode (hive-mcp-api-kanban-stats %s))"
                        (str "\"" (v/escape-elisp-string directory) "\""))
                "(json-encode (hive-mcp-api-kanban-stats))")
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mem-kanban-quick
  "Quick add task with defaults (todo, medium priority).
   When directory is provided, scopes task to that project."
  [{:keys [title directory]}]
  (handle-mem-kanban-create {:title title :directory directory}))

;; Tool definitions

(def tools
  [{:name "mcp_mem_kanban_create"
    :description "Create a kanban task in memory (short-term duration, 7 days)"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "Task title"}
                               :priority {:type "string" :enum ["high" "medium" "low"] :description "Priority (default: medium)"}
                               :context {:type "string" :description "Additional notes"}
                               :directory {:type "string" :description "Working directory to determine project scope (pass your cwd to ensure correct scoping)"}}
                  :required ["title"]}
    :handler handle-mem-kanban-create}

   {:name "mcp_mem_kanban_list_slim"
    :description "List kanban tasks with minimal data (id, title, status, priority only). Use for token-efficient overviews (~10x fewer tokens than full list)."
    :inputSchema {:type "object"
                  :properties {:status {:type "string" :enum ["todo" "doing" "review"] :description "Filter by status"}
                               :directory {:type "string" :description "Working directory to determine project scope (pass your cwd to ensure correct scoping)"}}}
    :handler handle-mem-kanban-list-slim}

   {:name "mcp_mem_kanban_move"
    :description "Move task to new status. Moving to 'done' DELETES the task from memory"
    :inputSchema {:type "object"
                  :properties {:task_id {:type "string" :description "Task ID"}
                               :new_status {:type "string" :enum ["todo" "doing" "review" "done"] :description "New status"}
                               :directory {:type "string" :description "Working directory to determine project scope (pass your cwd to ensure correct scoping)"}}
                  :required ["task_id" "new_status"]}
    :handler handle-mem-kanban-move}

   {:name "mcp_mem_kanban_stats"
    :description "Get kanban statistics (counts by status)"
    :inputSchema {:type "object"
                  :properties {:directory {:type "string" :description "Working directory to determine project scope (pass your cwd to ensure correct scoping)"}}}
    :handler handle-mem-kanban-stats}

   {:name "mcp_mem_kanban_quick"
    :description "Quick add task with defaults (todo status, medium priority)"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "Task title"}
                               :directory {:type "string" :description "Working directory to determine project scope (pass your cwd to ensure correct scoping)"}}
                  :required ["title"]}
    :handler handle-mem-kanban-quick}])
