(ns emacs-mcp.tools.kanban
  "Kanban integration handlers for MCP.

   Provides org-kanban operations via the emacs-mcp-org-kanban addon:
   - Status, task listing, and progress tracking
   - Task creation, updates, and status moves
   - Roadmap and personal task views
   - Backend synchronization"
  (:require [emacs-mcp.tools.core :refer [mcp-success mcp-error]]
            [emacs-mcp.emacsclient :as ec]
            [emacs-mcp.validation :as v]))

;; ============================================================
;; Kanban Tools (org-kanban integration)
;; ============================================================

(defn kanban-addon-available?
  "Check if emacs-mcp-org-kanban addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'emacs-mcp-org-kanban)")]
    (and success (= result "t"))))

(defn handle-mcp-kanban-status
  "Get kanban status including tasks by status, progress, and backend info."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (emacs-mcp-kanban-api-status))")]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded. Run (emacs-mcp-addon-load 'org-kanban)")))

(defn handle-mcp-kanban-list-tasks
  "List kanban tasks, optionally filtered by status."
  [{:keys [status]}]
  (if (kanban-addon-available?)
    (let [elisp (if status
                  (format "(json-encode (emacs-mcp-kanban-list-tasks nil \"%s\"))" status)
                  "(json-encode (emacs-mcp-kanban-list-tasks))")
          result (ec/eval-elisp elisp)]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-create-task
  "Create a new kanban task."
  [{:keys [title description]}]
  (if (kanban-addon-available?)
    (let [elisp (if description
                  (format "(json-encode (emacs-mcp-kanban-create-task \"%s\" \"%s\"))"
                          (v/escape-elisp-string title)
                          (v/escape-elisp-string description))
                  (format "(json-encode (emacs-mcp-kanban-create-task \"%s\"))"
                          (v/escape-elisp-string title)))
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Created task: " result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-update-task
  "Update a kanban task's status or title."
  [{:keys [task_id status title]}]
  (if (kanban-addon-available?)
    (let [props (cond-> ""
                  status (str (format ":status \"%s\" " status))
                  title (str (format ":title \"%s\" " (v/escape-elisp-string title))))
          elisp (format "(emacs-mcp-kanban-update-task \"%s\" %s)" task_id props)
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Updated task: " result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-move-task
  "Move a task to a new status column."
  [{:keys [task_id new_status]}]
  (if (kanban-addon-available?)
    (let [elisp (format "(emacs-mcp-kanban-move-task \"%s\" \"%s\")" task_id new_status)
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Moved task to " new_status)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-roadmap
  "Get roadmap view with milestones and progress."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (emacs-mcp-kanban-api-roadmap))")]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-my-tasks
  "Get tasks assigned to or modified by the current agent."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (emacs-mcp-kanban-api-my-tasks))")]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-sync
  "Sync tasks between vibe-kanban and standalone backends."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(emacs-mcp-kanban-sync-all)")]
      (mcp-success (str "Sync complete: " result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  [{:name "mcp_kanban_status"
    :description "Get kanban status including tasks by status, progress percentage, backend info, and roadmap. Use at session start."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-status}

   {:name "mcp_kanban_list_tasks"
    :description "List kanban tasks, optionally filtered by status (todo, inprogress, inreview, done)."
    :inputSchema {:type "object"
                  :properties {"status" {:type "string"
                                         :enum ["todo" "inprogress" "inreview" "done"]
                                         :description "Filter by status (optional)"}}
                  :required []}
    :handler handle-mcp-kanban-list-tasks}

   {:name "mcp_kanban_create_task"
    :description "Create a new kanban task with title and optional description."
    :inputSchema {:type "object"
                  :properties {"title" {:type "string"
                                        :description "Task title"}
                               "description" {:type "string"
                                              :description "Task description (optional)"}}
                  :required ["title"]}
    :handler handle-mcp-kanban-create-task}

   {:name "mcp_kanban_update_task"
    :description "Update a kanban task's status or title."
    :inputSchema {:type "object"
                  :properties {"task_id" {:type "string"
                                          :description "Task ID to update"}
                               "status" {:type "string"
                                         :enum ["todo" "inprogress" "inreview" "done"]
                                         :description "New status (optional)"}
                               "title" {:type "string"
                                        :description "New title (optional)"}}
                  :required ["task_id"]}
    :handler handle-mcp-kanban-update-task}

   {:name "mcp_kanban_move_task"
    :description "Move a task to a new status column. Shorthand for update with status change."
    :inputSchema {:type "object"
                  :properties {"task_id" {:type "string"
                                          :description "Task ID to move"}
                               "new_status" {:type "string"
                                             :enum ["todo" "inprogress" "inreview" "done"]
                                             :description "Target status column"}}
                  :required ["task_id" "new_status"]}
    :handler handle-mcp-kanban-move-task}

   {:name "mcp_kanban_roadmap"
    :description "Get roadmap view with milestones, phases, and progress indicators."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-roadmap}

   {:name "mcp_kanban_my_tasks"
    :description "Get tasks assigned to or modified by the current agent session."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-my-tasks}

   {:name "mcp_kanban_sync"
    :description "Sync tasks between vibe-kanban cloud and standalone org-file backends."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-sync}])
