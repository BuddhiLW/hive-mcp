(ns hive-mcp.crystal.hooks
  "Event hooks for progressive crystallization.

   Handles:
   - Kanban DONE → session-progress note
   - Memory access → recall tracking
   - Session boundaries → cross-session detection
   - Session end → auto-wrap crystallization

   SOLID: Single responsibility - event handling only.
   DDD: Application service layer."
  (:require [hive-mcp.crystal.core :as crystal]
            [hive-mcp.crystal.recall :as recall]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.channel :as channel]
            [hive-mcp.hooks :as hooks]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.events.core :as ev]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Telemetry Helper (Phase 1)
;; =============================================================================

(defn- emit-harvest-error!
  "Emit a structured error for harvest failures.

   Provides structured error handling for catastrophic failures:
   1. Logs with structured format for searchability
   2. Dispatches :system/error event for telemetry pipeline

   CLARITY: Telemetry first - observable system behavior."
  [fn-name message context]
  (let [error-data {:error-type :harvest-failed
                    :source (str "hooks/" fn-name)
                    :message message
                    :context (merge {:fn fn-name} context)}]
    ;; Dispatch to event system (handler in effects.clj will do the rest)
    (try
      (ev/dispatch [:system/error error-data])
      (catch Exception e
        ;; Fallback: at minimum log the error if event system fails
        (log/error "[TELEMETRY-FALLBACK] Failed to dispatch system error:" (.getMessage e))))))

;; =============================================================================
;; Kanban DONE Hook
;; =============================================================================

(defn on-kanban-done
  "Hook called when a kanban task moves to DONE.

   Creates an ephemeral progress note capturing the completion,
   registers the task in DataScript for wrap harvesting,
   then the kanban entry can be safely deleted.

   task: {:id :title :context :priority :started :status}

   Returns: {:success bool :progress-note-id string}"
  [{:keys [id title _context _priority _started] :as task}]
  (log/info "Kanban DONE hook triggered for task:" id title)
  ;; Register in DataScript for wrap harvesting (CLARITY-T: telemetry)
  (try
    (ds/register-completed-task! id {:title title})
    (log/debug "Registered completed task in DataScript:" id)
    (catch Exception e
      (log/warn "Failed to register completed task in DataScript:" (.getMessage e))))
  (let [;; Generate progress note
        progress-note (crystal/task-to-progress-note
                       (assoc task :completed-at (.toString (java.time.Instant/now))))
        ;; Convert tags vector to elisp list format
        tags-elisp (str "(" (str/join " " (map pr-str (:tags progress-note))) ")")
        ;; Store via Emacs memory
        elisp (format "(hive-mcp-memory-add 'note %s '%s nil 'ephemeral)"
                      (pr-str (:content progress-note))
                      tags-elisp)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (do
        (log/info "Created progress note for completed task:" id)
        ;; Broadcast event for swarm awareness
        (when (channel/server-connected?)
          (channel/broadcast! {:type "task-completed"
                               :task-id id
                               :title title
                               :progress-note-id result}))
        {:success true
         :progress-note-id result
         :task task})
      (do
        (log/error "Failed to create progress note:" error)
        {:success false
         :error error
         :task task}))))

(defn extract-task-from-kanban-entry
  "Extract task data from a kanban memory entry.
   
   entry: {:id :content {:task-type :title :context :priority :status :started}}"
  [entry]
  (let [content (:content entry)]
    (if (map? content)
      {:id (:id entry)
       :title (:title content)
       :context (:context content)
       :priority (or (:priority content) "medium")
       :started (:started content)
       :status (:status content)}
      ;; Handle string content (legacy format)
      {:id (:id entry)
       :title (str content)
       :context nil
       :priority "medium"
       :started nil
       :status "done"})))

;; =============================================================================
;; Memory Access Hook
;; =============================================================================

(defn on-memory-accessed
  "Hook called when memory entries are accessed.
   
   Tracks recall context for smart promotion.
   
   params: {:entry-ids [string]
            :source string      ; 'catchup', 'wrap', 'agent', 'user', 'query'
            :session string
            :project string
            :query-type string  ; 'note', 'decision', etc.
            :tags [string]}"
  [{:keys [entry-ids source session project] :as _params}]
  (let [current-session (or session (crystal/session-id))]
    (doseq [entry-id entry-ids]
      ;; For each entry, we'd ideally look up its session/project
      ;; For now, create the recall event with what we know
      (let [event (recall/create-recall-event
                   {:source source
                    :session current-session
                    :project project
                    :explicit? (not (contains? #{"catchup" "wrap"} source))})]
        (recall/buffer-recall! entry-id event)))
    {:tracked (count entry-ids)
     :source source}))

;; =============================================================================
;; Wrap Harvest Hook
;; =============================================================================

(defn harvest-session-progress
  "Harvest session progress notes for wrap workflow.

   opts: {:directory string} - Working directory for project scoping.
         Falls back to ctx/current-directory from request context.

   Queries ephemeral notes from current project - no specific tags required.
   This captures all ephemeral notes LLMs created during the session,
   regardless of what tags they used.

   Returns: {:notes [...] :count int :error? {...} on failure}

   CLARITY: Telemetry first - emits :system/error on failure."
  ([] (harvest-session-progress nil))
  ([{:keys [directory]}]
   (try
     (let [;; Priority: explicit param > request context
           dir (or directory (ctx/current-directory))
           ;; Derive project-id for scoped memory query
           project-id (when dir (scope/get-current-project-id dir))
           session-tag (crystal/session-tag)
           ;; Query ephemeral notes with explicit project scope
           ;; Args: type, tags, project-id, limit, duration, scope-filter
           elisp (if project-id
                   (format "(json-encode (hive-mcp-memory-query 'note nil %s 50 'ephemeral nil))"
                           (pr-str project-id))
                   "(json-encode (hive-mcp-memory-query 'note nil nil 50 'ephemeral nil))")
           {:keys [success result error]} (ec/eval-elisp elisp)]
       (if success
         (let [raw-notes (try (json/read-str result :key-fn keyword)
                              (catch Exception _ []))
               ;; Guard: filter to maps only to prevent "Key must be integer" error
               ;; when accessing (:tags item) on non-map items (vectors, strings, etc.)
               notes (filterv map? (if (sequential? raw-notes) raw-notes []))]
           {:notes notes
            :count (count notes)
            :session session-tag
            :project-id project-id})
         (do
           (log/error "harvest-session-progress: Emacs query failed:" error)
           {:notes []
            :count 0
            :error {:type :harvest-failed
                    :fn "harvest-session-progress"
                    :msg error}})))
     (catch Exception e
       (let [error-data {:type :harvest-failed
                         :fn "harvest-session-progress"
                         :msg (.getMessage e)}]
         (log/error e "harvest-session-progress failed")
         ;; Emit structured error for telemetry
         (emit-harvest-error! "harvest-session-progress" (.getMessage e) {})
         {:notes []
          :count 0
          :error error-data})))))

(defn harvest-completed-tasks
  "Harvest completed task progress notes for wrap.

   opts: {:directory string} - Working directory for project scoping.
         Falls back to ctx/current-directory from request context.

   Queries from TWO sources:
   1. DataScript registry (primary, reliable)
   2. Emacs kanban-tagged notes (fallback, may be unreliable)

   Returns: {:tasks [...] :count int :ds-count int :emacs-count int :error? {...} on failure}

   CLARITY: Telemetry first - emits :system/error on failure."
  ([] (harvest-completed-tasks nil))
  ([{:keys [directory]}]
   (try
     ;; Priority: explicit param > request context
     (let [dir (or directory (ctx/current-directory))
           ;; Derive project-id for scoped memory query
           project-id (when dir (scope/get-current-project-id dir))
           ;; Primary source: DataScript registry (added by on-kanban-done)
           ds-tasks (try
                      (->> (ds/get-completed-tasks-this-session)
                           (map (fn [t]
                                  {:id (:completed-task/id t)
                                   :title (:completed-task/title t)
                                   :completed-at (:completed-task/completed-at t)
                                   :agent-id (:completed-task/agent-id t)
                                   :source :datascript})))
                      (catch Exception e
                        (log/warn "Failed to query DataScript completed tasks:" (.getMessage e))
                        []))
           ;; Fallback source: Emacs memory (kanban-tagged notes)
           ;; Use project-id for scoped queries when available
           elisp-ephemeral (if project-id
                             (format "(hive-mcp-memory-query 'note '(\"kanban\") %s 50 'ephemeral nil)"
                                     (pr-str project-id))
                             "(hive-mcp-memory-query 'note '(\"kanban\") nil 50 'ephemeral nil)")
           elisp-short (if project-id
                         (format "(hive-mcp-memory-query 'note '(\"kanban\") %s 50 'short-term nil)"
                                 (pr-str project-id))
                         "(hive-mcp-memory-query 'note '(\"kanban\") nil 50 'short-term nil)")
           elisp (format "(json-encode (append %s %s))" elisp-ephemeral elisp-short)
           {:keys [success result]} (ec/eval-elisp elisp)
           emacs-tasks (if success
                         (try
                           (->> (json/read-str result :key-fn keyword)
                                (filter map?)  ;; Guard: prevent "Key must be integer" if non-map items
                                (map #(assoc % :source :emacs)))
                           (catch Exception _ []))
                         [])
           ;; Combine sources, prefer DataScript (has task IDs)
           all-tasks (concat ds-tasks emacs-tasks)]
       {:tasks all-tasks
        :count (count all-tasks)
        :ds-count (count ds-tasks)
        :emacs-count (count emacs-tasks)
        :project-id project-id})
     (catch Exception e
       (let [error-data {:type :harvest-failed
                         :fn "harvest-completed-tasks"
                         :msg (.getMessage e)}]
         (log/error e "harvest-completed-tasks failed")
        ;; Emit structured error for telemetry
         (emit-harvest-error! "harvest-completed-tasks" (.getMessage e) {})
         {:tasks []
          :count 0
          :ds-count 0
          :emacs-count 0
          :error error-data})))))

(defn harvest-git-commits
  "Harvest git commits since session start.

   opts: {:directory string} - Working directory for git operations.
         Falls back to ctx/current-directory from request context.
         If neither available, uses Emacs default-directory (may be incorrect).

   Returns: {:commits [string] :count int :error? {...} on failure}

   CLARITY: Telemetry first - emits :system/error on failure."
  ([] (harvest-git-commits nil))
  ([{:keys [directory]}]
   (try
     (let [;; Priority: explicit param > request context > Emacs default
           dir (or directory (ctx/current-directory))
           ;; Get commits from today (session approximation)
           ;; Use let-binding to set default-directory when directory is provided
           elisp (if dir
                   (format "(let ((default-directory %s)) (shell-command-to-string \"git log --since='midnight' --oneline 2>/dev/null\"))"
                           (pr-str dir))
                   "(shell-command-to-string \"git log --since='midnight' --oneline 2>/dev/null\")")
           {:keys [success result error]} (ec/eval-elisp elisp)]
       (if success
         (let [commits (when (and result (not (str/blank? result)))
                         (str/split-lines (str/trim result)))]
           {:commits (or commits [])
            :count (count (or commits []))
            :directory dir})
         (do
           (log/error "harvest-git-commits: Emacs command failed:" error)
           {:commits []
            :count 0
            :error {:type :harvest-failed
                    :fn "harvest-git-commits"
                    :msg error}})))
     (catch Exception e
       (let [error-data {:type :harvest-failed
                         :fn "harvest-git-commits"
                         :msg (.getMessage e)}]
         (log/error e "harvest-git-commits failed")
        ;; Emit structured error for telemetry
         (emit-harvest-error! "harvest-git-commits" (.getMessage e) {})
         {:commits []
          :count 0
          :error error-data})))))

(defn harvest-all
  "Harvest all session data for wrap crystallization.

   opts: {:directory string} - Working directory for git/memory operations.
         Falls back to ctx/current-directory from request context.
         Pass caller's cwd to ensure correct project scoping.

   Returns: {:progress-notes [...]
             :completed-tasks [...]
             :git-commits [...]
             :recalls {...}
             :session string
             :directory string
             :errors [...]}  ; aggregated errors from sub-harvests

   CLARITY: Telemetry first - aggregates errors for visibility."
  ([] (harvest-all nil))
  ([{:keys [directory] :as _opts}]
   (try
     ;; Priority: explicit param > request context
     (let [dir (or directory (ctx/current-directory))
           progress (harvest-session-progress {:directory dir})
           tasks (harvest-completed-tasks {:directory dir})
           commits (harvest-git-commits {:directory dir})
           recalls (try
                     (recall/get-buffered-recalls)
                     (catch Exception e
                       (log/warn "Failed to get buffered recalls:" (.getMessage e))
                       {}))
           ;; Aggregate errors from sub-harvests
           errors (filterv some? [(:error progress)
                                  (:error tasks)
                                  (:error commits)])]
       {:progress-notes (:notes progress)
        :completed-tasks (:tasks tasks)
        :git-commits (:commits commits)
        :recalls recalls
        :session (crystal/session-id)
        :directory dir
        :summary {:progress-count (:count progress)
                  :task-count (:count tasks)
                  :commit-count (:count commits)
                  :recall-count (count recalls)}
        :errors (when (seq errors) errors)})
     (catch Exception e
       (let [error-data {:type :harvest-failed
                         :fn "harvest-all"
                         :msg (.getMessage e)}]
         (log/error e "harvest-all failed catastrophically")
        ;; Emit structured error for telemetry
         (emit-harvest-error! "harvest-all" (.getMessage e) {})
         {:progress-notes []
          :completed-tasks []
          :git-commits []
          :recalls {}
          :session (try (crystal/session-id) (catch Exception _ "unknown"))
          :summary {:progress-count 0
                    :task-count 0
                    :commit-count 0
                    :recall-count 0}
          :errors [error-data]})))))

;; =============================================================================
;; Crystallization Hook
;; =============================================================================

(defn crystallize-session
  "Crystallize session data into long-term memory (Chroma storage).

   Takes harvested data (including :directory for project scoping) and:
   1. Creates session summary (short-term, scoped to project) in Chroma
   2. Promotes entries that meet threshold
   3. Clears ephemeral progress notes

   FIX: Now stores directly to Chroma instead of elisp memory layer.
   This ensures wrap data persists in the vector database.

   Returns: {:summary-id string :promoted [ids] :cleared [ids] :project-id string}"
  [{:keys [progress-notes completed-tasks git-commits directory _recalls] :as harvested}]
  (log/info "Crystallizing session:" (crystal/session-id) (when directory (str "directory:" directory)))

  ;; Derive project-id for scoped memory creation
  (let [project-id (or (when directory (scope/get-current-project-id directory)) "global")
        ;; 1. Create session summary
        summary (crystal/summarize-session-progress
                 (concat progress-notes completed-tasks)
                 git-commits)
        content (:content summary)
        tags (scope/inject-project-scope (or (:tags summary) []) project-id)
        ;; Calculate expiration (short-term = 7 days)
        expires (dur/calculate-expires "short")]
    (try
      ;; Store directly to Chroma (FIX: was using elisp before)
      (let [entry-id (chroma/index-memory-entry!
                      {:type "note"
                       :content content
                       :tags tags
                       :duration "short"
                       :expires (or expires "")
                       :project-id project-id
                       :content-hash (chroma/content-hash content)})]
        (log/info "Created session summary in Chroma:" entry-id "project:" project-id)
        ;; 2. Check for entries to promote (based on recall scores)
        ;; TODO: Integrate with recall tracking when we have access patterns
        {:summary-id entry-id
         :session (crystal/session-id)
         :project-id project-id
         :stats (:summary harvested)})
      (catch Exception e
        (log/error e "Failed to crystallize session to Chroma")
        {:error (.getMessage e)
         :session (crystal/session-id)}))))

;; =============================================================================
;; Auto-Wrap Session-End Handler
;; =============================================================================

(defn- on-session-end
  "Handler for session-end event.

   Automatically crystallizes session data (auto-wrap).
   Called by JVM shutdown hook via hooks/trigger-hooks.

   ctx: {:reason string :session string :directory string}

   Tries to get directory from ctx or request context for proper scoping.

   Returns: {:success bool :summary-id string :project-id string}"
  [event-ctx]
  (log/info "Auto-wrap triggered on session-end:" (:reason event-ctx "shutdown"))
  (try
    ;; Try to get directory from event context or request context
    (let [dir (or (:directory event-ctx) (ctx/current-directory))
          harvested (harvest-all {:directory dir})
          result (crystallize-session harvested)]
      ;; Broadcast wrap completion if channel available
      (when (channel/server-connected?)
        (channel/broadcast! {:type "session-ended"
                             :wrap-completed true
                             :session (:session result)
                             :project-id (:project-id result)
                             :stats (:stats result)}))
      (log/info "Auto-wrap completed:" (:summary-id result) "project:" (:project-id result))
      {:success true
       :summary-id (:summary-id result)
       :project-id (:project-id result)
       :stats (:stats result)})
    (catch Exception e
      (log/error e "Auto-wrap failed on session-end")
      {:success false
       :error (.getMessage e)})))

;; =============================================================================
;; Hook Registration
;; =============================================================================

(defonce ^:private hooks-registered? (atom false))

(defn register-hooks!
  "Register crystal hooks with the event system.

   Hooks into:
   - :session-end for auto-wrap crystallization
   - channel events for task completion
   - memory query events for recall tracking

   registry: Hook registry atom from hive-mcp.hooks/create-registry"
  [registry]
  (when-not @hooks-registered?
    (log/info "Registering crystal hooks")
    ;; Register auto-wrap handler for session-end
    (hooks/register-hook registry :session-end on-session-end)
    (log/info "Registered auto-wrap handler for :session-end")
    ;; Subscribe to task-completed events if channel is available
    (when (channel/server-connected?)
      (try
        ;; The channel subscription would go here
        ;; For now, hooks are called directly from handlers
        (log/debug "Channel hooks registered")
        (catch Exception e
          (log/warn "Could not register channel hooks:" (.getMessage e)))))
    (reset! hooks-registered? true)
    {:registered true}))

(comment
  ;; Example usage

  ;; When a kanban task completes
  (on-kanban-done {:id "task-123"
                   :title "Implement crystal module"
                   :context "Part of progressive crystallization feature"
                   :priority "high"
                   :started "2026-01-04T10:00:00"})

  ;; Harvest session data for wrap
  (harvest-all)

  ;; Crystallize the session
  (crystallize-session (harvest-all)))
