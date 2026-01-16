(ns hive-mcp.events.handlers.claim
  "File claim event handlers.

   Handles events related to file claim lifecycle:
   - :claim/file-released - When a file claim is released, notify waiting lings
   - :claim/notify-waiting - Send targeted shout to a ling that file is available

   Event Cascade:
   release-claim! → :claim/file-released → check wait-queue → :claim/notify-waiting → targeted shout

   SOLID: SRP - File claim lifecycle only
   CLARITY: R - Represented intent through claim domain"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [datascript.core :as d]
            [hive-mcp.swarm.datascript :as ds]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- query-waiting-lings
  "Query DataScript for lings with queued tasks waiting on a specific file.

   Returns a sequence of maps with :slave-id and :task-id for each waiting ling.

   Uses the task :files attribute to find tasks that include the specified file,
   then filters for :queued status tasks."
  [db file-path]
  (when (and db file-path)
    (let [results (d/q '[:find ?slave-id ?task-id
                         :in $ ?file
                         :where
                         [?t :task/files ?file]
                         [?t :task/status :queued]
                         [?t :task/id ?task-id]
                         [?t :task/slave ?s]
                         [?s :slave/id ?slave-id]]
                       db file-path)]
      (mapv (fn [[slave-id task-id]]
              {:slave-id slave-id
               :task-id task-id})
            results))))

;; =============================================================================
;; Handler: :claim/file-released
;; =============================================================================

(defn handle-claim-file-released
  "Handler for :claim/file-released events.

   When a file claim is released, this handler:
   1. Queries for lings with queued tasks waiting on this file
   2. Dispatches :claim/notify-waiting for each waiting ling
   3. Logs the release activity

   Expects event data:
   {:file        \"src/core.clj\"  ; The released file path
    :released-by \"ling-123\"}     ; Ling that released the claim (optional)

   Produces effects:
   - :log          - Log the release
   - :dispatch-n   - Dispatch notify events for all waiting lings"
  [coeffects [_ {:keys [file released-by]}]]
  (let [db (or (:db-snapshot coeffects) @(ds/get-conn))
        waiting-lings (query-waiting-lings db file)
        waiting-count (count waiting-lings)]

    (log/debug "File released:" file "by:" released-by
               "- waiting lings:" waiting-count)

    (if (seq waiting-lings)
      ;; Dispatch notification to each waiting ling
      {:log {:level :info
             :message (str "File " file " released by " released-by
                           " - notifying " waiting-count " waiting ling(s)")}
       :dispatch-n (mapv (fn [{:keys [slave-id task-id]}]
                           [:claim/notify-waiting {:target-agent-id slave-id
                                                   :file file
                                                   :task-id task-id}])
                         waiting-lings)}
      ;; No waiting lings - just log
      {:log {:level :debug
             :message (str "File " file " released by " released-by
                           " - no waiting lings")}})))

;; =============================================================================
;; Handler: :claim/notify-waiting
;; =============================================================================

(defn handle-claim-notify-waiting
  "Handler for :claim/notify-waiting events.

   Sends a targeted shout to a specific ling informing them that
   a file they were waiting for is now available.

   Expects event data:
   {:target-agent-id \"ling-worker-1\"  ; Ling to notify
    :file            \"src/core.clj\"   ; File that became available
    :task-id         \"task-123\"}      ; Task that was waiting (optional)

   Produces effects:
   - :targeted-shout - Send shout to specific ling"
  [_coeffects [_ {:keys [target-agent-id file task-id]}]]
  (log/debug "Notifying ling" target-agent-id "that file" file "is available")

  {:targeted-shout {:target-agent-id target-agent-id
                    :event-type :file-available
                    :data {:file file
                           :task-id task-id
                           :message (str "File " file " is now available")}}})

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register file claim event handlers."
  []
  (ev/reg-event :claim/file-released
                [(ev/inject-cofx :db-snapshot)
                 interceptors/debug]
                handle-claim-file-released)

  (ev/reg-event :claim/notify-waiting
                [interceptors/debug]
                handle-claim-notify-waiting)

  (log/info "[hive-events] Claim handlers registered: :claim/file-released :claim/notify-waiting"))
