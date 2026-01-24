(ns hive-mcp.tools.session-complete
  "MCP tool for completing ling sessions with full lifecycle.

   CLARITY Framework:
   - C: Composition - orchestrates git, kanban, crystal, hivemind
   - L: Layers pure - handler validates, event system executes effects
   - A: Architectural performance - single tool call instead of 4
   - R: Represented intent - clear session completion semantics
   - I: Inputs guarded - validates commit_msg required
   - T: Telemetry first - shouts completion for coordinator
   - Y: Yield safe failure - individual effects can fail gracefully

   Session complete sequence:
   1. Git commit staged changes
   2. Move kanban tasks to done
   3. Run wrap/crystallize for memory persistence
   4. Shout completion to hivemind coordinator

   Usage by lings:
   ```
   session_complete({
     commit_msg: \"feat: implement feature X\",
     task_ids: [\"kanban-task-1\", \"kanban-task-2\"],
     agent_id: \"<CLAUDE_SWARM_SLAVE_ID>\"
   })
   ```"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.swarm.datascript :as ds]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Validation (CLARITY: Inputs are guarded)
;; =============================================================================

(defn- validate-params
  "Validate session_complete parameters.
   Returns nil if valid, error map if invalid."
  [{:keys [commit_msg]}]
  (cond
    (nil? commit_msg)
    {:error "Missing required field: commit_msg"}

    (str/blank? commit_msg)
    {:error "commit_msg cannot be empty"}

    :else nil))

;; =============================================================================
;; Event Handler (dispatched via hive-events)
;; =============================================================================

(defn- handle-ling-session-complete
  "Handler for :ling/session-complete events.

   Orchestrates the full session completion sequence via effects:
   1. :git-commit     - Commit staged changes with message
   2. :kanban-move-done - Move specified tasks to done status
   3. :wrap-crystallize - Persist session learnings to memory
   4. :shout          - Broadcast completion to hivemind

   Expects event data:
   {:commit-msg \"feat: message\"
    :task-ids   [\"task-1\" \"task-2\"]  ; optional
    :agent-id   \"ling-123\"
    :cwd        \"/project/path\"}       ; optional

   Produces effects:
   - :git-commit       - Stage all and commit
   - :kanban-move-done - Move tasks to done (if task-ids provided)
   - :wrap-crystallize - Persist session to memory
   - :shout            - Broadcast completion"
  [coeffects [_ {:keys [commit-msg task-ids agent-id cwd]}]]
  ;; Fallback chain: explicit param → request-ctx → coeffect → env var → unknown
  (let [effective-agent (or agent-id
                            (ctx/current-agent-id)
                            (get-in coeffects [:agent-context :agent-id])
                            (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                            "unknown-ling")
        effective-cwd (or cwd
                          (ctx/current-directory)
                          (get-in coeffects [:agent-context :cwd])
                          (System/getProperty "user.dir"))

        ;; Base effects that always execute
        base-effects {:git-commit {:message commit-msg
                                   :files ["all"]  ; Stage all changes
                                   :cwd effective-cwd}

                      :wrap-crystallize {:agent-id effective-agent}

                      :shout {:agent-id effective-agent
                              :event-type :completed
                              :message (str "Session complete: " commit-msg)}}

        ;; Conditionally add kanban effect if tasks provided
        effects (if (seq task-ids)
                  (assoc base-effects
                         :kanban-move-done {:task-ids (vec task-ids)
                                            :directory effective-cwd})
                  base-effects)]

    (log/info "ling/session-complete:" effective-agent
              "tasks:" (count (or task-ids [])))
    effects))

;; =============================================================================
;; Handler Registration
;; =============================================================================

(defonce ^:private *handler-registered (atom false))

(defn register-handler!
  "Register the :ling/session-complete event handler.
   Safe to call multiple times."
  []
  (when-not @*handler-registered
    (ev/reg-event :ling/session-complete
                  [interceptors/debug]
                  handle-ling-session-complete)
    (reset! *handler-registered true)
    (log/info "[session-complete] Handler registered: :ling/session-complete")
    true))

(defn reset-registration!
  "Reset registration state. For testing."
  []
  (reset! *handler-registered false))

;; =============================================================================
;; MCP Tool Handler
;; =============================================================================

(defn- get-ling-kanban-task-id
  "Look up the kanban-task-id for a ling from DataScript.
   Returns nil if ling not found or no kanban-task-id is set."
  [agent-id]
  (when agent-id
    (when-let [slave (ds/get-slave agent-id)]
      (:slave/kanban-task-id slave))))

(defn- merge-kanban-task-ids
  "Merge explicit task_ids with ling's kanban-task-id if not already included.
   Ensures the ling's linked kanban task is auto-completed on session_complete."
  [explicit-task-ids ling-kanban-task-id]
  (let [explicit-set (set (or explicit-task-ids []))]
    (if (and ling-kanban-task-id
             (not (contains? explicit-set ling-kanban-task-id)))
      (conj (vec (or explicit-task-ids [])) ling-kanban-task-id)
      (vec (or explicit-task-ids [])))))

(defn handle-session-complete
  "Complete a ling session: commit changes, move tasks to done, crystallize wrap.

   Required:
   - commit_msg: Git commit message

   Optional:
   - task_ids: Array of kanban task IDs to mark done
   - agent_id: Ling's slave ID (auto-detected from env if not provided)
   - directory: Working directory for git/kanban scoping

   Note: If the ling was spawned with a kanban_task_id, it will be
   automatically added to the task_ids for completion (if not already present).

   Returns:
   - {:status \"ok\", :agent_id \"...\", :tasks_completed N}
   - {:error \"...\"}  on validation failure"
  [{:keys [commit_msg task_ids agent_id directory] :as params}]
  (log/info "session-complete:" commit_msg "tasks:" (count (or task_ids [])))

  ;; Validate input
  (if-let [validation-error (validate-params params)]
    {:type "text"
     :text (json/write-str validation-error)}

    ;; Dispatch event for execution
    ;; Fallback chain: explicit param → request-ctx → env var → unknown
    (let [agent-id (or agent_id
                       (ctx/current-agent-id)
                       (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                       "unknown-ling")
          effective-dir (or directory
                            (ctx/current-directory))
          ;; Look up ling's linked kanban-task-id and merge it
          ling-kanban-task-id (get-ling-kanban-task-id agent-id)
          merged-task-ids (merge-kanban-task-ids task_ids ling-kanban-task-id)
          event-data {:commit-msg commit_msg
                      :task-ids (when (seq merged-task-ids) merged-task-ids)
                      :agent-id agent-id
                      :cwd effective-dir}]

      (when ling-kanban-task-id
        (log/info "session-complete: auto-adding ling's kanban-task-id:" ling-kanban-task-id))

      ;; Ensure handler is registered
      (register-handler!)

      ;; Dispatch the event
      (ev/dispatch [:ling/session-complete event-data])

      ;; Return success response
      {:type "text"
       :text (json/write-str {:status "ok"
                              :agent_id agent-id
                              :tasks_completed (count merged-task-ids)
                              :linked_kanban_task ling-kanban-task-id
                              :commit_msg commit_msg})})))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tools
  "Tool definitions for session completion."
  [{:name "session_complete"
    :description "Complete ling session: commit changes, move kanban tasks to done, crystallize wrap. Use this at session end instead of calling git/kanban/wrap separately. IMPORTANT: Pass your CLAUDE_SWARM_SLAVE_ID as agent_id for proper attribution."
    :inputSchema {:type "object"
                  :properties {"commit_msg" {:type "string"
                                             :description "Git commit message summarizing session work"}
                               "task_ids" {:type "array"
                                           :items {:type "string"}
                                           :description "Kanban task IDs to mark as done (optional)"}
                               "agent_id" {:type "string"
                                           :description "Ling's slave-id (CLAUDE_SWARM_SLAVE_ID). Required for proper attribution."}
                               "directory" {:type "string"
                                            :description "Working directory for git/kanban scoping (pass your cwd)"}}
                  :required ["commit_msg"]}
    :handler handle-session-complete}])
