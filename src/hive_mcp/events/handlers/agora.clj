(ns hive-mcp.events.handlers.agora
  "Agora dialogue event handlers.

   Handles events related to Agora multi-ling dialogues:
   - :agora/turn-dispatched  - Relay turn to target ling's terminal
   - :agora/turn-completed   - Unified turn completion (renamed from turn-response)
   - :agora/dispatch-next    - Participant-type aware next turn dispatch
   - :agora/execute-drone    - Execute drone turn via delegate-drone!
   - :agora/debate-started   - Auto-kick first turn after create-debate!
   - :agora/consensus        - Crystallize debate result to memory
   - :agora/timeout          - Handle dialogue timeout

   Event chain: :agora/turn-completed -> :agora/dispatch-next
                -> :agora/execute-drone (drones) or :swarm-send-prompt (lings)"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.schema :as schema]
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
;; Handler: :agora/turn-completed (Unified Turn Completion)
;; =============================================================================

(defn handle-turn-completed
  "Handler for :agora/turn-completed events (unified turn completion).

   Called after ANY turn completes (drone or ling). Routes to
   :agora/dispatch-next which handles participant-type aware dispatch.

   Renamed from :agora/turn-response for clarity in the unified event chain:
   :agora/turn-completed -> :agora/dispatch-next -> :agora/execute-drone | :swarm-send-prompt

   Expects event data:
   {:dialogue-id      \"dialogue-uuid\"
    :participant-id   \"drone-123\"
    :participant-type  :drone | :ling
    :signal           :propose | :counter | :approve | :no-change | :defer
    :message          \"The argument text\"
    :confidence       0.8
    :turn-num         5}

   Produces effects:
   - :log      - Log turn completion
   - :dispatch - Chain to :agora/dispatch-next if debate continues"
  [_coeffects [_ {:keys [dialogue-id participant-id signal turn-num] :as _data}]]
  (let [dialogue (dialogue/get-dialogue dialogue-id)]
    (cond
      ;; Dialogue not found
      (nil? dialogue)
      {:log {:level :warn
             :message (str "Turn completed for unknown dialogue: " dialogue-id)}}

      ;; Consensus already reached - no action needed (consensus handler takes over)
      (= :consensus (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " at consensus after turn " turn-num
                           " (signal: " (name signal) ")")}}

      ;; Dialogue ended (timeout/aborted)
      (#{:aborted :timeout} (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " is " (name (:status dialogue))
                           ", not continuing")}}

      ;; Dialogue still active - dispatch next turn
      (= :active (:status dialogue))
      {:log {:level :debug
             :message (str "Turn " turn-num " completed by " participant-id
                           " (signal: " (name signal) "), dispatching next")}
       :dispatch [:agora/dispatch-next {:dialogue-id dialogue-id}]}

      ;; Unknown status
      :else
      {:log {:level :warn
             :message (str "Agora " dialogue-id " in unexpected status: "
                           (:status dialogue))}})))

;; =============================================================================
;; Handler: :agora/dispatch-next (Participant-Type Aware Dispatch)
;; =============================================================================

(defn handle-dispatch-next
  "Handler for :agora/dispatch-next events.

   Determines the next participant and dispatches based on type:
   - :drone -> :agora/execute-drone (via effect)
   - :ling  -> :swarm-send-prompt (relay to terminal)

   Uses require/resolve to access debate state without circular deps.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"}

   Produces effects:
   - :log             - Log dispatch decision
   - :agora/execute-drone - For drone participants
   - :swarm-send-prompt   - For ling participants
   - :dispatch            - Fallback to :agora/continue for legacy"
  [_coeffects [_ {:keys [dialogue-id]}]]
  ;; Resolve debate state at runtime to avoid circular dependency
  (try
    (require 'hive-mcp.agora.debate)
    (let [get-debate-fn (resolve 'hive-mcp.agora.debate/get-debate-status)
          debate-status (when get-debate-fn (get-debate-fn dialogue-id))]
      (if debate-status
        ;; Debate exists - use :agora/continue effect (handles drone execution)
        {:log {:level :info
               :message (str "Dispatching next turn for debate: " dialogue-id
                             " (participants: " (count (:participants debate-status)) ")")}
         :agora/continue {:dialogue-id dialogue-id}}
        ;; No debate state - this is a ling-based dialogue, relay via prompt
        (let [dialogue (dialogue/get-dialogue dialogue-id)
              turns (dialogue/get-dialogue-turns dialogue-id)
              last-turn (last turns)
              next-participant (when last-turn
                                 (first (disj (:participants dialogue) (:sender last-turn))))]
          (if next-participant
            {:log {:level :info
                   :message (str "Relaying to ling participant: " next-participant)}
             :swarm-send-prompt {:slave-id next-participant
                                 :prompt (format-agora-prompt
                                          {:dialogue-id dialogue-id
                                           :from (:sender last-turn)
                                           :topic (:topic dialogue)
                                           :message (:message last-turn)})}}
            {:log {:level :warn
                   :message (str "No next participant found for dialogue: " dialogue-id)}}))))
    (catch Exception e
      {:log {:level :error
             :message (str "dispatch-next failed for " dialogue-id ": " (.getMessage e))}})))

;; =============================================================================
;; Handler: :agora/execute-drone (Drone Turn Execution)
;; =============================================================================

(defn handle-execute-drone
  "Handler for :agora/execute-drone events.

   Wraps delegate-drone! in a future, parses response via signal.clj,
   records turn, and emits :agora/turn-completed.

   This is the drone-specific execution path. The actual execution
   happens via the :agora/continue effect.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"}

   Produces effects:
   - :log            - Log execution intent
   - :agora/continue - Trigger async debate continuation"
  [_coeffects [_ {:keys [dialogue-id]}]]
  {:log {:level :info
         :message (str "Executing drone turn for debate: " dialogue-id)}
   :agora/continue {:dialogue-id dialogue-id}})

;; =============================================================================
;; Handler: :agora/debate-started (Auto-Kick First Turn)
;; =============================================================================

(defn handle-debate-started
  "Handler for :agora/debate-started events.

   Auto-kicks the first turn after agora_create_debate.
   No manual intervention needed - the debate starts immediately.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :topic       \"debate topic\"
    :participants [{:id ... :type ... :role ...}]}

   Produces effects:
   - :log      - Log debate start
   - :dispatch - Chain to :agora/dispatch-next to kick first turn"
  [_coeffects [_ {:keys [dialogue-id topic participants]}]]
  {:log {:level :info
         :message (str "Debate started: " dialogue-id
                       " topic: " (or topic "unspecified")
                       " participants: " (count participants))}
   :dispatch [:agora/dispatch-next {:dialogue-id dialogue-id}]})

;; =============================================================================
;; Handler: :agora/stage-transition (Two-Stage Agora)
;; =============================================================================

(defn handle-stage-transition
  "Handler for :agora/stage-transition events.

   Called when research stage completes and debate stage begins.
   Logs the transition with evidence count.

   Expects event data:
   {:dialogue-id    \"dialogue-uuid\"
    :from-stage     :research
    :to-stage       :debate
    :evidence-pool  [{:source ... :content ... :confidence ...}]
    :topic          \"debate topic\"}

   Produces effects:
   - :log             - Log stage transition
   - :channel-publish - Notify Emacs of stage change"
  [_coeffects [_ {:keys [dialogue-id from-stage to-stage evidence-pool topic]}]]
  {:log {:level :info
         :message (str "Stage transition for " dialogue-id ": "
                       (name (or from-stage :unknown)) " -> " (name (or to-stage :unknown))
                       " with " (count evidence-pool) " evidence items")}
   :channel-publish {:event :agora-stage-transition
                     :data {:dialogue-id dialogue-id
                            :from-stage from-stage
                            :to-stage to-stage
                            :evidence-count (count evidence-pool)
                            :topic topic}}})

;; =============================================================================
;; Handler: :agora/consensus (P0: Crystallize Result)
;; =============================================================================

(defn- generate-debate-summary
  "Generate a summary of the debate for crystallization.

   Extracts key information:
   - Topic and methodology
   - Final positions from participants
   - Turn count and consensus signal
   - Key arguments from turn log"
  [dialogue turns]
  (let [topic (:topic (:config dialogue))
        methodology (get-in dialogue [:config :methodology] :opinion)
        final-turns (take-last 3 (sort-by :turn-number turns))
        participant-summaries (->> turns
                                   (group-by :sender)
                                   (map (fn [[sender sender-turns]]
                                          (str "- " sender ": "
                                               (:message (last (sort-by :turn-number sender-turns))))))
                                   (str/join "\n"))]
    (str "# Agora Debate Consensus: " (or topic "Untitled") "\n\n"
         "**Methodology:** " (name methodology) "\n"
         "**Turns:** " (count turns) "\n"
         "**Status:** Consensus reached\n\n"
         "## Final Positions\n"
         participant-summaries "\n\n"
         "## Key Points\n"
         (->> final-turns
              (map (fn [t] (str "- [" (name (:signal t)) "] " (:message t))))
              (str/join "\n")))))

(defn handle-consensus
  "Handler for :agora/consensus events (P0 Crystallization).

   Called when a debate reaches consensus. Crystallizes the result
   to long-term memory for future reference.

   Expects event data:
   {:dialogue-id \"dialogue-uuid\"
    :turns       5}

   Produces effects:
   - :log          - Log consensus achievement
   - :memory-write - Crystallize debate summary to memory
   - :channel-publish - Notify Emacs of consensus"
  [_coeffects [_ {:keys [dialogue-id turns]}]]
  (let [dialogue (schema/get-dialogue dialogue-id)
        turn-log (schema/get-turns dialogue-id)
        summary (generate-debate-summary dialogue turn-log)
        topic (or (get-in dialogue [:config :topic])
                  (:name dialogue)
                  "Untitled debate")]
    {:log {:level :info
           :message (str "Debate " dialogue-id " reached consensus after "
                         turns " turns, crystallizing result")}
     :memory-write {:type "decision"
                    :content summary
                    :tags ["agora" "debate-result" "consensus"
                           (str "dialogue:" dialogue-id)]
                    :duration "long"}
     :channel-publish {:event :agora-consensus-crystallized
                       :data {:dialogue-id dialogue-id
                              :topic topic
                              :turns turns}}}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register agora-related event handlers.

   Handlers registered:
   - :agora/turn-dispatched  - Relay turn to target ling (existing)
   - :agora/timeout          - Handle dialogue timeout (existing)
   - :agora/turn-completed   - Unified turn completion (renamed from turn-response)
   - :agora/dispatch-next    - Participant-type aware next turn dispatch
   - :agora/execute-drone    - Execute drone turn
   - :agora/debate-started   - Auto-kick first turn
   - :agora/consensus        - Crystallize debate result

   Event chain: turn-completed -> dispatch-next -> execute-drone | swarm-send-prompt"
  []
  ;; Existing handlers
  (ev/reg-event :agora/turn-dispatched
                [interceptors/debug]
                handle-agora-turn-dispatched)

  (ev/reg-event :agora/timeout
                [interceptors/debug]
                handle-agora-timeout)

  ;; Unified event chain handlers
  (ev/reg-event :agora/turn-completed
                [interceptors/debug]
                handle-turn-completed)

  ;; Backward compat: :agora/turn-response -> :agora/turn-completed
  (ev/reg-event :agora/turn-response
                [interceptors/debug]
                handle-turn-completed)

  (ev/reg-event :agora/dispatch-next
                [interceptors/debug]
                handle-dispatch-next)

  (ev/reg-event :agora/execute-drone
                [interceptors/debug]
                handle-execute-drone)

  (ev/reg-event :agora/debate-started
                [interceptors/debug]
                handle-debate-started)

  ;; Stage transition (Two-Stage Agora)
  (ev/reg-event :agora/stage-transition
                [interceptors/debug]
                handle-stage-transition)

  ;; Consensus crystallization
  (ev/reg-event :agora/consensus
                [interceptors/debug]
                handle-consensus))