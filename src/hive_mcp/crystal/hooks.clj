(ns hive-mcp.crystal.hooks
  "Event hooks for progressive crystallization.

   Shared helpers (safe-effect, eval-elisp-safe, parse-json-safe) reduce
   try-catch repetition. Lifecycle operations extracted into run-lifecycle-ops!
   to eliminate duplication between crystallize-session branches.

   DDD: Domain service layer for crystal/wrap events."
  (:require [hive-mcp.crystal.core :as crystal]
            [hive-mcp.crystal.recall :as recall]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.hooks.core :as hooks]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.events.core :as ev]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.chroma.core :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Shared Helpers (CC-reducing, try-catch consolidation)
;; =============================================================================

(defn- emit-harvest-error!
  "Emit a structured error for harvest failures."
  [fn-name message context]
  (let [error-data {:error-type :harvest-failed
                    :source (str "hooks/" fn-name)
                    :message message
                    :context (merge {:fn fn-name} context)}]
    (try
      (ev/dispatch [:system/error error-data])
      (catch Exception e
        (log/error "[TELEMETRY-FALLBACK] Failed to dispatch system error:" (.getMessage e))))))

(defn- safe-effect
  "Run non-blocking side effect. Returns result on success.
   On exception: logs warning, returns fallback (with :error merged if map)."
  [label f fallback]
  (try (f)
       (catch Exception e
         (log/warn (str label " failed (non-blocking): " (.getMessage e)))
         (if (map? fallback)
           (assoc fallback :error (.getMessage e))
           fallback))))

(defn- eval-elisp-safe
  "Eval elisp with timeout. Returns {:success :result :error :timed-out}.
   Catches exceptions, returning {:success false :error message}."
  [elisp timeout-ms]
  (try
    (let [r (ec/eval-elisp-with-timeout elisp timeout-ms)]
      (when (:timed-out r)
        (log/warn "eval-elisp-safe: timed out"))
      r)
    (catch Exception e
      (log/warn "eval-elisp-safe: failed:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn- parse-json-safe
  "Parse JSON string, returning nil on failure."
  [s]
  (try (json/read-str s :key-fn keyword)
       (catch Exception _ nil)))

(defn- harvest-error-result
  "Build error result for harvest failure with telemetry emission."
  [fn-name e defaults]
  (let [error-data {:type :harvest-failed :fn fn-name :msg (.getMessage e)}]
    (log/error e (str fn-name " failed"))
    (emit-harvest-error! fn-name (.getMessage e) {})
    (merge defaults {:error error-data})))

;; =============================================================================
;; Kanban DONE Hook
;; =============================================================================

(defn on-kanban-done
  "Hook called when a kanban task moves to DONE."
  [{:keys [id title _context _priority _started] :as task}]
  (log/info "Kanban DONE hook triggered for task:" id title)
  ;; Register in DataScript for wrap harvesting (non-blocking)
  (safe-effect "register-completed-task"
               #(do (ds/register-completed-task! id {:title title})
                    (log/debug "Registered completed task in DataScript:" id))
               nil)
  (let [;; Generate progress note
        progress-note (crystal/task-to-progress-note
                       (assoc task :completed-at (.toString (java.time.Instant/now))))
        ;; Convert tags vector to elisp list format
        tags-elisp (str "(" (str/join " " (map pr-str (:tags progress-note))) ")")
        ;; Store via Emacs memory
        elisp (format "(hive-mcp-memory-add 'note %s '%s nil 'ephemeral)"
                      (pr-str (:content progress-note))
                      tags-elisp)
        {:keys [success result error timed-out]}
        (try
          (ec/eval-elisp-with-timeout elisp 10000)
          (catch Exception e
            (log/warn "on-kanban-done: elisp eval failed gracefully:" (.getMessage e))
            {:success false :error (.getMessage e)}))]
    (when timed-out
      (log/warn "on-kanban-done: elisp eval timed out for task:" id))
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
  "Extract task data from a kanban memory entry."
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
  "Hook called when memory entries are accessed."
  [{:keys [entry-ids source session project] :as _params}]
  (let [current-session (or session (crystal/session-id))]
    (doseq [entry-id entry-ids]
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
  "Harvest session progress notes for wrap workflow."
  ([] (harvest-session-progress nil))
  ([{:keys [directory]}]
   (try
     (let [dir (or directory (ctx/current-directory))
           project-id (when dir (scope/get-current-project-id dir))
           session-tag (crystal/session-tag)
           elisp (if project-id
                   (format "(json-encode (hive-mcp-memory-query 'note nil %s 50 'ephemeral nil))"
                           (pr-str project-id))
                   "(json-encode (hive-mcp-memory-query 'note nil nil 50 'ephemeral nil))")
           {:keys [success result error]} (eval-elisp-safe elisp 12000)]
       (if success
         (let [raw-notes (parse-json-safe result)
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
       (harvest-error-result "harvest-session-progress" e
                             {:notes [] :count 0})))))

(defn harvest-completed-tasks
  "Harvest completed task progress notes for wrap."
  ([] (harvest-completed-tasks nil))
  ([{:keys [directory]}]
   (try
     (let [dir (or directory (ctx/current-directory))
           project-id (when dir (scope/get-current-project-id dir))
           ;; Primary source: DataScript registry (non-blocking fallback to [])
           ds-tasks (safe-effect "DataScript completed tasks"
                                 #(->> (ds/get-completed-tasks-this-session)
                                       (mapv (fn [t]
                                               {:id (:completed-task/id t)
                                                :title (:completed-task/title t)
                                                :completed-at (:completed-task/completed-at t)
                                                :agent-id (:completed-task/agent-id t)
                                                :source :datascript})))
                                 [])
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
           {:keys [success result]} (eval-elisp-safe elisp 15000)
           emacs-tasks (if success
                         (let [parsed (parse-json-safe result)]
                           (->> (if (sequential? parsed) parsed [])
                                (filter map?)
                                (mapv #(assoc % :source :emacs))))
                         [])
           all-tasks (concat ds-tasks emacs-tasks)]
       {:tasks all-tasks
        :count (count all-tasks)
        :ds-count (count ds-tasks)
        :emacs-count (count emacs-tasks)
        :project-id project-id})
     (catch Exception e
       (harvest-error-result "harvest-completed-tasks" e
                             {:tasks [] :count 0 :ds-count 0 :emacs-count 0})))))

(defn harvest-git-commits
  "Harvest git commits since session start (or midnight if no session-start recorded)."
  ([] (harvest-git-commits nil))
  ([{:keys [directory agent-id]}]
   (try
     (let [dir (or directory (ctx/current-directory))
           since (if-let [start (crystal/get-session-start (or agent-id (ctx/current-agent-id)))]
                   (.toString start)
                   "midnight")
           elisp (if dir
                   (format "(let ((default-directory %s)) (shell-command-to-string \"git log --since='%s' --oneline 2>/dev/null\"))"
                           (pr-str dir) since)
                   (format "(shell-command-to-string \"git log --since='%s' --oneline 2>/dev/null\")" since))
           {:keys [success result error]} (eval-elisp-safe elisp 10000)]
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
       (harvest-error-result "harvest-git-commits" e
                             {:commits [] :count 0})))))

(defn harvest-all
  "Harvest all session data for wrap crystallization.
   Returns map with :progress-notes, :completed-tasks, :git-commits, :recalls,
   :session-timing, :session-temporal (alias), :memory-ids-created (flushed),
   :memory-ids-accessed (recall buffer keys), :session, :directory, :agent-id, :summary, :errors.

   Opts:
     :directory  -- working directory for project scoping
     :agent-id   -- agent identity for per-agent session timing"
  ([] (harvest-all nil))
  ([{:keys [directory agent-id] :as _opts}]
   (try
     (let [dir (or directory (ctx/current-directory))
           effective-agent (or agent-id (ctx/current-agent-id))
           progress (harvest-session-progress {:directory dir})
           tasks (harvest-completed-tasks {:directory dir})
           commits (harvest-git-commits {:directory dir :agent-id effective-agent})
           recalls (safe-effect "buffered recalls"
                                #(recall/get-buffered-recalls) {})
           session-timing (safe-effect "session timing"
                                       #(crystal/session-timing-metadata
                                         (crystal/get-session-start effective-agent)
                                         (java.time.Instant/now))
                                       {:session-start nil :session-end nil :duration-minutes 0})
           ;; Auto-KG: memory IDs created this session (flushed — destructive read)
           memory-ids-created (safe-effect "memory-ids-created"
                                           #(recall/flush-created-ids!) [])
           ;; Auto-KG: memory IDs accessed this session (from recall buffer keys)
           memory-ids-accessed (vec (keys recalls))
           ;; Aggregate errors from sub-harvests
           errors (filterv some? [(:error progress)
                                  (:error tasks)
                                  (:error commits)])]
       {:progress-notes (:notes progress)
        :completed-tasks (:tasks tasks)
        :git-commits (:commits commits)
        :recalls recalls
        :session-timing session-timing
        :session-temporal session-timing
        :memory-ids-created memory-ids-created
        :memory-ids-accessed memory-ids-accessed
        :session (crystal/session-id)
        :directory dir
        :agent-id effective-agent
        :summary {:progress-count (:count progress)
                  :task-count (:count tasks)
                  :commit-count (:count commits)
                  :recall-count (count recalls)
                  :created-count (count memory-ids-created)
                  :accessed-count (count memory-ids-accessed)}
        :errors (when (seq errors) errors)})
     (catch Exception e
       (let [error-data {:type :harvest-failed
                         :fn "harvest-all"
                         :msg (.getMessage e)}]
         (log/error e "harvest-all failed catastrophically")
         (emit-harvest-error! "harvest-all" (.getMessage e) {})
         {:progress-notes []
          :completed-tasks []
          :git-commits []
          :recalls {}
          :session-temporal {:session-start nil :session-end nil :duration-minutes 0}
          :memory-ids-created []
          :memory-ids-accessed []
          :session (safe-effect "session-id" #(crystal/session-id) "unknown")
          :summary {:progress-count 0
                    :task-count 0
                    :commit-count 0
                    :recall-count 0
                    :created-count 0
                    :accessed-count 0}
          :errors [error-data]})))))

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Noop Defaults (lifecycle operations)
;; =============================================================================

(def ^:private empty-promotion-stats
  {:promoted 0 :skipped 0 :below 0 :evaluated 0})

(def ^:private empty-decay-stats
  {:decayed 0 :pruned 0 :fresh 0 :evaluated 0})

(def ^:private empty-xpoll-stats
  {:promoted 0 :candidates 0 :total-scanned 0})

(def ^:private empty-memory-decay-stats
  {:decayed 0 :expired 0 :total-scanned 0})

;; =============================================================================
;; Lifecycle Operations — delegates to extensions
;; =============================================================================

(defn- run-lifecycle-ops!
  "Run post-crystallization lifecycle operations. Delegates to extensions.
   All non-blocking. Returns map with :promotion-stats :decay-stats
   :xpoll-stats :memory-decay-stats."
  [project-id directory]
  (let [promo   (safe-effect "lifecycle-a"
                             #(delegate-or-noop :ch/a empty-promotion-stats
                                                [{:scope project-id :created-by "crystallize-session"}])
                             empty-promotion-stats)
        decay   (safe-effect "lifecycle-b"
                             #(delegate-or-noop :ch/b empty-decay-stats
                                                [{:scope project-id :created-by "crystallize-session"}])
                             empty-decay-stats)
        xpoll   (safe-effect "lifecycle-c"
                             #(delegate-or-noop :ch/c empty-xpoll-stats
                                                [{:directory directory :limit 100}])
                             empty-xpoll-stats)
        mdecay  (safe-effect "lifecycle-d"
                             #(delegate-or-noop :ch/d empty-memory-decay-stats
                                                [{:directory directory :limit 50}])
                             empty-memory-decay-stats)]
    {:promotion-stats    (select-keys promo  [:promoted :skipped :below :evaluated :error])
     :decay-stats        (select-keys decay  [:decayed :pruned :fresh :evaluated :error])
     :xpoll-stats        (select-keys xpoll  [:promoted :candidates :total-scanned :error])
     :memory-decay-stats (select-keys mdecay [:decayed :expired :total-scanned :error])}))

;; =============================================================================
;; Temporal Block Formatting (Step-5: Auto-KG wrap metadata)
;; =============================================================================

(defn- format-temporal-block
  "Format session timing and memory-ID counts as a markdown block.
   Appended to session summary content for temporal traceability.
   Nil-safe: handles missing memory-ids gracefully (steps 2-4 may not be merged yet)."
  [{:keys [session-start session-end duration-minutes]}
   {:keys [memory-ids-created memory-ids-accessed]}]
  (let [lines (cond-> ["\n\n### Temporal Metadata"
                       (str "- Session start: " (or session-start "unknown"))
                       (str "- Session end: " (or session-end "unknown"))
                       (str "- Duration: " (or duration-minutes 0) " minutes")]
                (seq memory-ids-created)
                (conj (str "- Memory entries created: " (count memory-ids-created)))
                (seq memory-ids-accessed)
                (conj (str "- Memory entries accessed: " (count memory-ids-accessed))))]
    (str/join "\n" lines)))

;; =============================================================================
;; Crystallization Hook
;; =============================================================================

(defn crystallize-session
  "Crystallize session data into long-term memory.
   Tries progress/task/commit summary first, falls back to memory-activity
   summary for coordinator sessions."
  [{:keys [progress-notes completed-tasks git-commits directory _recalls] :as harvested}]
  (log/info "Crystallizing session:" (crystal/session-id) (when directory (str "directory:" directory)))

  (let [project-id (or (when directory (scope/get-current-project-id directory)) "global")
        session-timing (or (:session-timing harvested)
                           (crystal/session-timing-metadata nil (java.time.Instant/now)))
        summary (or (crystal/summarize-session-progress
                     (concat progress-notes completed-tasks)
                     git-commits)
                    (crystal/summarize-memory-activity
                     {:created  (count (or (:memory-ids-created harvested) []))
                      :accessed (count (or (:memory-ids-accessed harvested) []))}))]
    (if (nil? summary)
      ;; No content — still run lifecycle ops for maintenance
      (let [lifecycle (run-lifecycle-ops! project-id directory)]
        (log/info "No content to crystallize for session:" (crystal/session-id))
        (merge {:skipped true
                :reason "no-content"
                :session (crystal/session-id)
                :project-id project-id
                :session-timing session-timing
                :stats (:summary harvested)}
               lifecycle))
      ;; Content exists — store summary with temporal block then run lifecycle ops
      (let [temporal-block (format-temporal-block session-timing harvested)
            content (str (:content summary) temporal-block)
            base-tags (into (or (:tags summary) []) ["auto-kg" "session-wrap" "temporal"])
            tags (scope/inject-project-scope base-tags project-id)
            expires (dur/calculate-expires "short")]
        (try
          (let [entry-id (chroma/index-memory-entry!
                          {:type "note"
                           :content content
                           :tags tags
                           :duration "short"
                           :expires (or expires "")
                           :project-id project-id
                           :content-hash (chroma/content-hash content)})
                lifecycle (run-lifecycle-ops! project-id directory)]
            (log/info "Created session summary in Chroma:" entry-id "project:" project-id)
            (merge {:summary-id entry-id
                    :session (crystal/session-id)
                    :project-id project-id
                    :session-timing session-timing
                    :stats (:summary harvested)}
                   lifecycle))
          (catch Exception e
            (log/error e "Failed to crystallize session to Chroma")
            {:error (.getMessage e)
             :session (crystal/session-id)}))))))

;; =============================================================================
;; Auto-Wrap Session-End Handler
;; =============================================================================

(defn- on-session-end
  "Handler for session-end event."
  [event-ctx]
  (log/info "Auto-wrap triggered on session-end:" (:reason event-ctx "shutdown"))
  (try
    ;; Try to get directory from event context or request context
    (let [dir (or (:directory event-ctx) (ctx/current-directory))
          agent-id (or (:agent-id event-ctx) (ctx/current-agent-id))
          harvested (harvest-all {:directory dir :agent-id agent-id})
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
  "Register crystal hooks with the event system."
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
