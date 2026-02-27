(ns hive-mcp.events.handlers.ling
  "Ling spawn/kill/status event handlers.

   Handles events related to ling lifecycle:
   - :ling/started       - Ling spawned and initialized
   - :ling/completed     - Ling finished all work
   - :ling/ready-for-wrap - Auto-wrap hook on ling completion"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :ling/started (EVENTS-03)
;; =============================================================================

(defn handle-ling-started
  "Handler for :ling/started events.

   Called when a ling is spawned and initialized. Registers the ling
   in DataScript for swarm coordination and logs the spawn.

   Expects event data:
   {:slave-id  \"swarm-worker-123\"
    :name      \"task-name\"
    :presets   [\"tdd\" \"clarity\"]
    :cwd       \"/path/to/project\"
    :depth     1}

   Produces effects:
   - :log         - Log spawn message
   - :ds-transact - Register ling in DataScript"
  [_coeffects [_ {:keys [slave-id name presets cwd depth]}]]
  (let [effective-id (or slave-id name "unknown-ling")
        effective-depth (or depth 1)]
    {:log {:level :info
           :message (str "Ling started: " effective-id
                         (when (seq presets) (str " presets=" presets)))}
     :ds-transact [{:slave/id effective-id
                    :slave/name (or name effective-id)
                    :slave/status :starting
                    :slave/depth effective-depth
                    :slave/tasks-completed 0
                    :slave/created-at (java.util.Date.)
                    :slave/presets (vec (or presets []))
                    :slave/cwd cwd}]}))

;; =============================================================================
;; Handler: :ling/completed (EVENTS-03)
;; =============================================================================

(defn handle-ling-completed
  "Handler for :ling/completed events.

   Called when a ling finishes all work. Broadcasts completion to hivemind
   and dispatches :session/end for auto-wrap behavior.

   Expects event data:
   {:slave-id \"swarm-worker-123\"
    :result   \"success\" | \"failure\" | any
    :reason   \"task completed\" | \"error\" | etc}

   Produces effects:
   - :log      - Log completion message
   - :dispatch - Dispatch :session/end for auto-wrap
   NOTE: Does NOT re-shout. The caller (sync.clj, session-complete) is
   responsible for shouting. Re-shouting here would create a feedback loop:
   shout! → :ling/completed → handle-ling-completed → :shout → shout!"
  [_coeffects [_ {:keys [slave-id agent-id result reason]}]]
  (let [effective-id (or slave-id agent-id)]
    (if effective-id
      {:log {:level :info
             :message (str "Ling completed: " effective-id
                           (when result (str " result=" result)))}
       :dispatch [:session/end {:slave-id effective-id
                                :reason (or reason "ling-completed")}]}
      {:log {:level :warn
             :message "Ling completed event with no slave-id or agent-id — dropped"}})))

;; =============================================================================
;; Handler: :ling/ready-for-wrap
;; =============================================================================

(defn handle-ling-ready-for-wrap
  "Handler for :ling/ready-for-wrap events.

   Called when auto-wrap hook detects a ling has completed work and
   is ready for session crystallization. Dispatches :session/wrap
   to trigger the wrap workflow.

   Expects event data:
   {:slave-id   \"swarm-worker-123\"
    :reason     \"task-completed\" | \"idle-detected\" | \"manual\"
    :session-id \"session:2026-01-14:worker-123\"}

   Produces effects:
   - :log      - Log auto-wrap trigger
   - :dispatch - Dispatch :session/wrap to run wrap workflow"
  [_coeffects [_ {:keys [slave-id agent-id reason session-id]}]]
  (let [effective-id (or slave-id agent-id)]
    (if effective-id
      {:log {:level :info
             :message (str "Auto-wrap triggered for " effective-id
                           " (reason: " (or reason "task-completed") ")")}
       :dispatch [:session/wrap {:session-id session-id
                                 :slave-id effective-id
                                 :triggered-by "auto-wrap"
                                 :reason (or reason "task-completed")}]}
      {:log {:level :warn
             :message "Ready-for-wrap event with no slave-id or agent-id — dropped"}})))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register ling-related event handlers."
  []
  (ev/reg-event :ling/started
                [interceptors/debug]
                handle-ling-started)

  (ev/reg-event :ling/completed
                [interceptors/debug]
                handle-ling-completed)

  (ev/reg-event :ling/ready-for-wrap
                [interceptors/debug]
                handle-ling-ready-for-wrap))
