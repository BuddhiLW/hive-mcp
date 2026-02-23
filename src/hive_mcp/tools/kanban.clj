(ns hive-mcp.tools.kanban
  "Kanban integration handlers for MCP.

   Provides org-kanban operations via the hive-mcp-org-kanban addon:
   - Status, task listing, and progress tracking
   - Task creation, updates, and status moves
   - Roadmap and personal task views
   - Backend synchronization"
  (:require [hive-mcp.tools.core :refer [mcp-success mcp-error]]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.dns.validation :as v]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Kanban Tools (org-kanban integration)
;; ============================================================

(defn kanban-addon-available?
  "Check if hive-mcp-org-kanban addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-org-kanban)")]
    (and success (= result "t"))))

(defn handle-mcp-kanban-status
  "Get kanban status including tasks by status, progress, and backend info."
  [_]
  (if (kanban-addon-available?)
    (let [_result (ec/eval-elisp "(json-encode (hive-mcp-kanban-api-status))")]
      (mcp-success (str _result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded. Run (hive-mcp-addon-load 'org-kanban)")))

(defn handle-mcp-kanban-list-tasks
  "List kanban tasks, optionally filtered by status."
  [{:keys [status]}]
  (if (kanban-addon-available?)
    (let [elisp (if status
                  (format "(json-encode (hive-mcp-kanban-list-tasks nil \"%s\"))" status)
                  "(json-encode (hive-mcp-kanban-list-tasks))")
          result (ec/eval-elisp elisp)]
      (mcp-success (str result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-create-task
  "Create a new kanban task."
  [{:keys [title description]}]
  (if (kanban-addon-available?)
    (let [elisp (if description
                  (format "(json-encode (hive-mcp-kanban-create-task \"%s\" \"%s\"))"
                          (v/escape-elisp-string title)
                          (v/escape-elisp-string description))
                  (format "(json-encode (hive-mcp-kanban-create-task \"%s\"))"
                          (v/escape-elisp-string title)))
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Created task: " result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-update-task
  "Update a kanban task's status or title."
  [{:keys [task_id status title]}]
  (if (kanban-addon-available?)
    (let [props (cond-> ""
                  status (str (format ":status \"%s\" " status))
                  title (str (format ":title \"%s\" " (v/escape-elisp-string title))))
          elisp (format "(hive-mcp-kanban-update-task \"%s\" %s)" task_id props)
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Updated task: " result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-move-task
  "Move a task to a new status column."
  [{:keys [task_id new_status]}]
  (if (kanban-addon-available?)
    (let [elisp (format "(hive-mcp-kanban-move-task \"%s\" \"%s\")" task_id new_status)
          _result (ec/eval-elisp elisp)]
      (mcp-success (str "Moved task to " new_status)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-roadmap
  "Get roadmap view with milestones and progress."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (hive-mcp-kanban-api-roadmap))")]
      (mcp-success (str result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-my-tasks
  "Get tasks assigned to or modified by the current agent."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (hive-mcp-kanban-api-my-tasks))")]
      (mcp-success (str result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-sync
  "Sync tasks between vibe-kanban and standalone backends."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(hive-mcp-kanban-sync-all)")]
      (mcp-success (str "Sync complete: " result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  "REMOVED: Flat kanban tools no longer exposed. Use consolidated `kanban` tool."
  [])
