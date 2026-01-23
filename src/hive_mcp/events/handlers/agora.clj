(ns hive-mcp.events.handlers.agora
  "Agora dialogue event handlers.

   Handles events related to Agora multi-ling dialogues:
   - :agora/turn-dispatched - Relay turn to target ling's terminal

   SOLID: SRP - Agora dialogue lifecycle only
   CLARITY: R - Represented intent through agora domain"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.agora.dialogue :as dialogue]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const max-turns
  "Maximum turns before dialogue times out to prevent infinite loops."
  50)

(def ^:private agora-context-template
  "Template for injecting Agora context into prompts.
   Format args: dialogue-id, from, topic, dialogue-id, from, message"
  "---
AGORA DIALOGUE CONTEXT
Dialogue ID: %s
From: %s
Topic: %s

You are in a Nash Equilibrium dialogue. Respond via agora_dispatch:
- dialogue_id: \"%s\"
- to: \"%s\" (or another participant)  
- signal: [propose|counter|approve|no-change|defer]

Signals: propose/counter=reset equilibrium, approve/no-change=toward consensus
---

%s")

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- format-agora-prompt
  "Inject Agora context into message for target ling."
  [{:keys [dialogue-id from topic message]}]
  (format agora-context-template
          dialogue-id
          from
          (or topic "Unspecified")
          dialogue-id
          from
          message))

;; =============================================================================
;; Handler: :agora/turn-dispatched
;; =============================================================================

(defn handle-agora-turn-dispatched
  "Handler for :agora/turn-dispatched events.

   Called after a turn is dispatched within an Agora dialogue.
   Relays the message to the target ling's terminal with Agora context.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :from        \"sender-slave-id\"
    :to          \"target-slave-id\"
    :turn-num    5
    :signal      :propose
    :message     \"The actual message content\"
    :topic       \"Dialogue topic\"}

   Produces effects:
   - :log              - Log relay status
   - :swarm-send-prompt - Send enhanced prompt to target ling
   - :dispatch         - Chain to timeout event if max turns exceeded"
  [_coeffects [_ {:keys [dialogue-id from to turn-num message topic]}]]
  (let [dialogue (dialogue/get-dialogue dialogue-id)]
    (cond
      ;; Dialogue not found
      (nil? dialogue)
      {:log {:level :warn
             :message (str "Agora turn relay skipped - dialogue not found: " dialogue-id)}}

      ;; Dialogue already reached consensus - no need to relay
      (= :consensus (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " already at consensus, not relaying")}}

      ;; Dialogue aborted or timed out
      (#{:aborted :timeout} (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " is " (name (:status dialogue)) ", not relaying")}}

      ;; Max turns exceeded - trigger timeout
      (>= turn-num max-turns)
      {:log {:level :warn
             :message (str "Agora " dialogue-id " exceeded max turns (" max-turns ")")}
       :dispatch [:agora/timeout {:dialogue-id dialogue-id
                                  :reason :max-turns
                                  :turn-count turn-num}]}

      ;; Normal case: relay to target ling
      :else
      {:log {:level :debug
             :message (str "Relaying Agora turn " turn-num " to " to " in dialogue " dialogue-id)}
       :swarm-send-prompt {:slave-id to
                           :prompt (format-agora-prompt
                                    {:dialogue-id dialogue-id
                                     :from from
                                     :topic topic
                                     :message message})}})))

;; =============================================================================
;; Handler: :agora/timeout
;; =============================================================================

(defn handle-agora-timeout
  "Handler for :agora/timeout events.

   Called when a dialogue exceeds max turns or times out.
   Updates dialogue status to :timeout.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :reason      :max-turns | :inactivity
    :turn-count  50}

   Produces effects:
   - :log - Log timeout message"
  [_coeffects [_ {:keys [dialogue-id reason turn-count]}]]
  {:log {:level :warn
         :message (str "Agora dialogue " dialogue-id " timed out"
                       " (reason: " (name (or reason :unknown)) ")"
                       (when turn-count (str ", turns: " turn-count)))}})

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register agora-related event handlers."
  []
  (ev/reg-event :agora/turn-dispatched
                [interceptors/debug]
                handle-agora-turn-dispatched)

  (ev/reg-event :agora/timeout
                [interceptors/debug]
                handle-agora-timeout))