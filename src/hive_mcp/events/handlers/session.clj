(ns hive-mcp.events.handlers.session
  "Session start/end event handlers.

   Handles events related to session lifecycle:
   - :session/end  - Session ending, trigger auto-wrap
   - :session/wrap - Trigger wrap workflow

   SOLID: SRP - Session lifecycle only
   CLARITY: R - Represented intent through session domain"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :session/end (EVENTS-06)
;; =============================================================================

(defn handle-session-end
  "Handler for :session/end events.

   Emits :wrap-notify effect for coordinator permeation. This enables
   the auto-wrap convergence pattern where lings' session learnings
   are automatically queued for coordinator to permeate.

   Expects event data:
   {:slave-id \"swarm-worker-123\"
    :reason   \"auto-wrap\" | \"ling-completed\" | etc}

   Produces effects:
   - :log         - Log session end
   - :wrap-notify - Queue wrap for coordinator permeation"
  [coeffects [_ {:keys [slave-id reason]}]]
  (let [agent-id (or slave-id
                     (get-in coeffects [:agent-context :agent-id])
                     (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                     "unknown-agent")
        session-id (or (get-in coeffects [:agent-context :session-id])
                       (str "session:" (java.time.LocalDate/now) ":" agent-id))]
    {:log {:level :info
           :message (str "Session ending: " agent-id)}
     :wrap-notify {:agent-id agent-id
                   :session-id session-id
                   :stats {:triggered-by "auto-wrap"
                           :reason (or reason "session-end")}}}))

;; =============================================================================
;; Handler: :session/wrap (P5-3)
;; =============================================================================

(defn handle-session-wrap
  "Handler for :session/wrap events.

   Triggers wrap crystallization at session end to persist session learnings.

   Expects event data:
   {:session-id   \"session-abc\"
    :slave-id     \"swarm-ling-123\"
    :project      \"hive-mcp\"
    :triggered-by \"auto-wrap\" | \"manual\"
    :reason       \"task-completed\" | etc}

   Produces effects:
   - :log              - Log wrap trigger
   - :wrap-crystallize - Persist session learnings to memory

   FIX: GAP-1 - Previously used :run-workflow which was never registered.
   Now directly emits :wrap-crystallize to ensure auto-wrap actually persists."
  [coeffects [_ {:keys [session-id slave-id project triggered-by reason]}]]
  (let [agent-id (or slave-id
                     (get-in coeffects [:agent-context :agent-id])
                     (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                     "unknown-agent")
        effective-session-id (or session-id
                                 (str "session:" (java.time.LocalDate/now) ":" agent-id))]
    {:log {:level :info
           :message (str "Auto-wrap crystallizing session: " effective-session-id
                         " (triggered-by: " (or triggered-by "unknown")
                         ", reason: " (or reason "session-wrap") ")")}
     :wrap-crystallize {:agent-id agent-id
                        :session-id effective-session-id
                        :project project}}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register session-related event handlers."
  []
  (ev/reg-event :session/end
                [interceptors/debug]
                handle-session-end)

  (ev/reg-event :session/wrap
                [interceptors/debug]
                handle-session-wrap))
