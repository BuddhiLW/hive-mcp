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
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Tool handlers

(defn handle-mem-kanban-create
  "Create a kanban task in memory."
  [{:keys [title priority context]}]
  (let [priority (or priority "medium")
        elisp (format "(json-encode (hive-mcp-api-kanban-create %s %s %s))"
                      (str "\"" (v/escape-elisp-string title) "\"")
                      (str "\"" (v/escape-elisp-string priority) "\"")
                      (if context
                        (str "\"" (v/escape-elisp-string context) "\"")
                        "nil"))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mem-kanban-list
  "List kanban tasks, optionally by status."
  [{:keys [status]}]
  (let [elisp (if status
                (format "(json-encode (hive-mcp-api-kanban-list %s))"
                        (str "\"" (v/escape-elisp-string status) "\""))
                "(json-encode (hive-mcp-api-kanban-list))")
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mem-kanban-move
  "Move task to new status. When moving to 'done':
   1. Fetches the task entry first
   2. Calls crystal hook to create progress note
   3. Then deletes the kanban entry"
  [{:keys [task_id new_status]}]
  (let [valid-statuses #{"todo" "doing" "review" "done"}]
    (if-not (valid-statuses new_status)
      {:type "text" :text (str "Invalid status: " new_status ". Valid: todo, doing, review, done") :isError true}
      (if (= new_status "done")
        ;; Special handling for DONE: call crystal hook before deletion
        (let [;; 1. Fetch the entry before deletion
              get-elisp (format "(json-encode (hive-mcp-memory-get %s))"
                                (str "\"" (v/escape-elisp-string task_id) "\""))
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
              (let [move-elisp (format "(json-encode (hive-mcp-api-kanban-move %s %s))"
                                       (str "\"" (v/escape-elisp-string task_id) "\"")
                                       (str "\"" (v/escape-elisp-string new_status) "\""))
                    {:keys [success result error]} (ec/eval-elisp move-elisp)]
                (if success
                  {:type "text" :text result}
                  {:type "text" :text (str "Error: " error) :isError true})))))
        ;; Normal status change (not done)
        (let [elisp (format "(json-encode (hive-mcp-api-kanban-move %s %s))"
                            (str "\"" (v/escape-elisp-string task_id) "\"")
                            (str "\"" (v/escape-elisp-string new_status) "\""))
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            {:type "text" :text result}
            {:type "text" :text (str "Error: " error) :isError true}))))))

(defn handle-mem-kanban-stats
  "Get kanban statistics by status."
  [_]
  (let [elisp "(json-encode (hive-mcp-api-kanban-stats))"
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mem-kanban-quick
  "Quick add task with defaults (todo, medium priority)."
  [{:keys [title]}]
  (handle-mem-kanban-create {:title title}))

;; Tool definitions

(def tools
  [{:name "mcp_mem_kanban_create"
    :description "Create a kanban task in memory (short-term duration, 7 days)"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "Task title"}
                               :priority {:type "string" :enum ["high" "medium" "low"] :description "Priority (default: medium)"}
                               :context {:type "string" :description "Additional notes"}}
                  :required ["title"]}
    :handler handle-mem-kanban-create}

   {:name "mcp_mem_kanban_list"
    :description "List kanban tasks from memory, optionally filtered by status"
    :inputSchema {:type "object"
                  :properties {:status {:type "string" :enum ["todo" "doing" "review"] :description "Filter by status"}}}
    :handler handle-mem-kanban-list}

   {:name "mcp_mem_kanban_move"
    :description "Move task to new status. Moving to 'done' DELETES the task from memory"
    :inputSchema {:type "object"
                  :properties {:task_id {:type "string" :description "Task ID"}
                               :new_status {:type "string" :enum ["todo" "doing" "review" "done"] :description "New status"}}
                  :required ["task_id" "new_status"]}
    :handler handle-mem-kanban-move}

   {:name "mcp_mem_kanban_stats"
    :description "Get kanban statistics (counts by status)"
    :inputSchema {:type "object" :properties {}}
    :handler handle-mem-kanban-stats}

   {:name "mcp_mem_kanban_quick"
    :description "Quick add task with defaults (todo status, medium priority)"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "Task title"}}
                  :required ["title"]}
    :handler handle-mem-kanban-quick}])
