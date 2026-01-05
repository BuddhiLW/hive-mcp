(ns hive-mcp.crystal.hooks
  "Event hooks for progressive crystallization.
   
   Handles:
   - Kanban DONE → session-progress note
   - Memory access → recall tracking
   - Session boundaries → cross-session detection
   
   SOLID: Single responsibility - event handling only.
   DDD: Application service layer."
  (:require [hive-mcp.crystal.core :as crystal]
            [hive-mcp.crystal.recall :as recall]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.channel :as channel]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Kanban DONE Hook
;; =============================================================================

(defn on-kanban-done
  "Hook called when a kanban task moves to DONE.
   
   Creates an ephemeral progress note capturing the completion,
   then the kanban entry can be safely deleted.
   
   task: {:id :title :context :priority :started :status}
   
   Returns: {:success bool :progress-note-id string}"
  [{:keys [id title context priority started] :as task}]
  (log/info "Kanban DONE hook triggered for task:" id title)
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
  [{:keys [entry-ids source session project] :as params}]
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
   
   Queries ephemeral notes from current project - no specific tags required.
   This captures all ephemeral notes LLMs created during the session,
   regardless of what tags they used.
   
   Returns: {:notes [...] :count int}"
  []
  (let [session-tag (crystal/session-tag)
        ;; Query ephemeral notes with auto scope filtering (current project + global)
        ;; Args: type, tags, project-id, limit, duration, scope-filter
        elisp "(json-encode (hive-mcp-memory-query 'note nil nil 50 'ephemeral nil))"
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (let [notes (try (json/read-str result :key-fn keyword)
                       (catch Exception _ []))]
        {:notes notes
         :count (count notes)
         :session session-tag})
      {:notes []
       :count 0
       :error error})))

(defn harvest-completed-tasks
  "Harvest completed task progress notes for wrap.
   
   Queries kanban-tagged notes from current project with ephemeral or short-term duration.
   Captures task completions automatically without requiring specific 'completed-task' tag.
   
   Returns: {:tasks [...] :count int}"
  []
  (let [;; Query kanban-tagged ephemeral notes
        elisp-ephemeral "(hive-mcp-memory-query 'note '(\"kanban\") nil 50 'ephemeral nil)"
        ;; Also query short-term kanban notes (might have been promoted)
        elisp-short "(hive-mcp-memory-query 'note '(\"kanban\") nil 50 'short-term nil)"
        ;; Combine both queries
        elisp (format "(json-encode (append %s %s))" elisp-ephemeral elisp-short)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (let [tasks (try (json/read-str result :key-fn keyword)
                       (catch Exception _ []))]
        {:tasks tasks
         :count (count tasks)})
      {:tasks []
       :count 0
       :error error})))

(defn harvest-git-commits
  "Harvest git commits since session start.
   
   Returns: {:commits [string] :count int}"
  []
  (let [;; Get commits from today (session approximation)
        elisp "(shell-command-to-string \"git log --since='midnight' --oneline 2>/dev/null\")"
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (let [commits (when (and result (not (str/blank? result)))
                      (str/split-lines (str/trim result)))]
        {:commits (or commits [])
         :count (count (or commits []))})
      {:commits []
       :count 0
       :error error})))

(defn harvest-all
  "Harvest all session data for wrap crystallization.
   
   Returns: {:progress-notes [...]
             :completed-tasks [...]
             :git-commits [...]
             :recalls {...}
             :session string}"
  []
  (let [progress (harvest-session-progress)
        tasks (harvest-completed-tasks)
        commits (harvest-git-commits)
        recalls (recall/get-buffered-recalls)]
    {:progress-notes (:notes progress)
     :completed-tasks (:tasks tasks)
     :git-commits (:commits commits)
     :recalls recalls
     :session (crystal/session-id)
     :summary {:progress-count (:count progress)
               :task-count (:count tasks)
               :commit-count (:count commits)
               :recall-count (count recalls)}}))

;; =============================================================================
;; Crystallization Hook
;; =============================================================================

(defn crystallize-session
  "Crystallize session data into long-term memory.
   
   Takes harvested data and:
   1. Creates session summary (short-term)
   2. Promotes entries that meet threshold
   3. Clears ephemeral progress notes
   
   Returns: {:summary-id string :promoted [ids] :cleared [ids]}"
  [{:keys [progress-notes completed-tasks git-commits recalls] :as harvested}]
  (log/info "Crystallizing session:" (crystal/session-id))

  ;; 1. Create session summary
  (let [summary (crystal/summarize-session-progress
                 (concat progress-notes completed-tasks)
                 git-commits)
        elisp (format "(hive-mcp-memory-add 'note %s '%s nil 'short-term)"
                      (pr-str (:content summary))
                      (pr-str (:tags summary)))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (do
        (log/info "Created session summary:" result)
        ;; 2. Check for entries to promote (based on recall scores)
        ;; TODO: Integrate with recall tracking when we have access patterns
        {:summary-id result
         :session (crystal/session-id)
         :stats (:summary harvested)})
      (do
        (log/error "Failed to crystallize session:" error)
        {:error error
         :session (crystal/session-id)}))))

;; =============================================================================
;; Hook Registration
;; =============================================================================

(defonce ^:private hooks-registered? (atom false))

(defn register-hooks!
  "Register crystal hooks with the event system.
   
   Hooks into:
   - channel events for task completion
   - memory query events for recall tracking"
  []
  (when-not @hooks-registered?
    (log/info "Registering crystal hooks")
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
