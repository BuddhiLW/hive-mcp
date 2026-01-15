(ns hive-mcp.tools.crystal
  "Crystal/wrap workflow tools for session crystallization.

   Handles progressive crystallization of session data into long-term memory.

   SOLID: Single Responsibility - crystal/wrap logic in dedicated module.
   DDD: Application service layer exposing crystal domain functionality."
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

(defn- hive-mcp-el-available?
  "Check if hive-mcp.el is loaded in Emacs."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp)")]
    (and success (= result "t"))))

(defn handle-wrap-gather
  "Gather session data for wrap workflow without storing.
   
   Uses progressive crystallization to harvest:
   - Session progress notes (from kanban completions)
   - Completed tasks (ephemeral notes tagged session-progress)
   - Git commits since session start
   - Recall patterns (for smart promotion)
   
   Returns gathered data for confirmation before crystallization."
  [_params]
  (log/info "wrap-gather with crystal harvesting")
  (try
    ;; Use crystal hooks for comprehensive harvesting
    (let [harvested (crystal-hooks/harvest-all)
          ;; Also get elisp-side data for completeness
          elisp-result (when (hive-mcp-el-available?)
                         (let [{:keys [success result]}
                               (ec/eval-elisp "(json-encode (hive-mcp-api-wrap-gather))")]
                           (when success
                             (try (json/read-str result :key-fn keyword)
                                  (catch Exception _ nil)))))
          ;; Merge both sources
          combined {:crystal harvested
                    :elisp elisp-result
                    :session (:session harvested)
                    :summary (merge (:summary harvested)
                                    {:has-elisp-data (some? elisp-result)})}]
      {:type "text"
       :text (json/write-str combined)})
    (catch Exception e
      (log/error e "wrap-gather failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)
                              :fallback "Use elisp-only gather"})
       :isError true})))

(defn handle-wrap-crystallize
  "Crystallize session data into long-term memory.

   Takes harvested data (from wrap-gather) and:
   1. Creates session summary (short-term duration)
   2. Promotes entries that meet score threshold
   3. Flushes recall buffer
   4. Emits wrap_notify event for hivemind permeation

   Call after wrap-gather when ready to persist."
  [_params]
  (log/info "wrap-crystallize")
  (try
    (let [harvested (crystal-hooks/harvest-all)
          result (crystal-hooks/crystallize-session harvested)
          agent-id (or (System/getenv "CLAUDE_SWARM_SLAVE_ID") "coordinator")]
      ;; Emit wrap_notify via hive-events for Crystal Convergence
      (try
        (ev/dispatch [:crystal/wrap-notify
                      {:agent-id agent-id
                       :session-id (:session result)
                       :created-ids (when-let [sid (:summary-id result)] [sid])
                       :stats (:stats result)}])
        (log/info "wrap-crystallize: emitted wrap_notify for" agent-id)
        (catch Exception e
          (log/warn "wrap-crystallize: failed to emit wrap_notify:" (.getMessage e))))
      {:type "text"
       :text (json/write-str result)})
    (catch Exception e
      (log/error e "wrap-crystallize failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})
       :isError true})))

(defn handle-permeate-crystals
  "Process wrap queue entries from ling sessions.

   Coordinator calls this to process wrap notifications from lings:
   1. Gets unprocessed wraps from DataScript queue
   2. Marks each as processed
   3. Returns stats about what was permeated

   Part of Crystal Convergence - hivemind permeating ling session data."
  [_params]
  (log/info "permeate-crystals: processing wrap queue")
  (try
    (let [queue-items (ds/get-unprocessed-wraps)
          processed-count (count queue-items)
          agent-ids (map :wrap-queue/agent-id queue-items)]
      ;; Mark each as processed
      (doseq [item queue-items]
        (ds/mark-wrap-processed! (:wrap-queue/id item)))
      ;; Return stats
      {:type "text"
       :text (json/write-str {:processed processed-count
                              :agent-ids (vec agent-ids)
                              :status "ok"})})
    (catch Exception e
      (log/error e "permeate-crystals failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})
       :isError true})))

;; Tool definitions
(def tools
  [{:name "wrap_gather"
    :description "Gather session data for wrap workflow. Returns recent notes, git commits, kanban activity without storing. Use before wrap to preview/confirm data."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-wrap-gather}

   {:name "wrap_crystallize"
    :description "Crystallize session data into long-term memory. Creates session summary, promotes entries meeting score threshold, and flushes recall buffer. Call after wrap_gather to persist."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-wrap-crystallize}

   {:name "mcp_permeate_crystals"
    :description "Process wrap queue from ling sessions. Coordinator calls this to permeate ling session data. Returns stats about processed wraps including agent IDs and counts."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-permeate-crystals}])
