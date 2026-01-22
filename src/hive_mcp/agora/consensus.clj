(ns hive-mcp.agora.consensus
  "Agora Consensus: Nash Equilibrium detection for multi-ling dialogues.

   Core principle: A dialogue reaches **consensus** when it achieves
   **Nash equilibrium** - no participant would unilaterally change their
   position given the others' positions.

   SOLID: Single responsibility - consensus detection only.
   DDD: Pure domain functions, no side effects.

   Depends on: hive-mcp.agora.schema (dialogue state queries)"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [hive-mcp.agora.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Turn Signal Constants
;; =============================================================================

(def equilibrium-signals
  "Signals that indicate participant has reached equilibrium state.
   These signals count toward Nash equilibrium detection."
  #{:no-change :approve :lgtm})

(def reset-signals
  "Signals that reset equilibrium - participant proposes a change."
  #{:propose :counter})

(def neutral-signals
  "Signals that neither block nor contribute to equilibrium."
  #{:defer})

(def all-valid-signals
  "All valid turn signals."
  (set/union equilibrium-signals reset-signals neutral-signals))

;; =============================================================================
;; Default Configuration
;; =============================================================================

(def default-config
  "Default consensus detection configuration."
  {:threshold 1.0            ; 100% agreement for simple dialogues
   :minimum-turns 2          ; Require at least 2 turns before consensus valid
   :stuck-threshold 10       ; Turns without progress = stuck
   :timeout-turns 50         ; Max turns before forced timeout
   :require-all-active true  ; All participants must have made a turn
   })

;; =============================================================================
;; Schema Adapters (wrap schema API for consensus-specific queries)
;; =============================================================================

(defn get-participants
  "Get all participants in a dialogue.
   Wraps schema/get-dialogue to extract participants."
  [dialogue-id]
  (or (:participants (schema/get-dialogue dialogue-id)) []))

(defn get-dialogue-config
  "Get configuration for a dialogue, with defaults.
   Wraps schema/get-dialogue to extract config."
  [dialogue-id]
  (or (:config (schema/get-dialogue dialogue-id)) default-config))

(defn last-turn-for
  "Get the most recent turn for a specific participant.
   Filters turns by sender and returns the last one."
  [dialogue-id participant]
  (let [turns (schema/get-turns dialogue-id)
        participant-turns (filter #(= participant (:sender %)) turns)]
    (last (sort-by :turn-number participant-turns))))

;; =============================================================================
;; Nash Equilibrium Detection
;; =============================================================================

(defn nash-equilibrium?
  "Check if all participants have signaled :no-change/:approve/:lgtm in their last turn.

   Returns true when dialogue has reached Nash equilibrium - no participant
   would unilaterally change their position given the others' positions.

   dialogue-id: identifier for the dialogue to check

   Returns: boolean"
  [dialogue-id]
  (let [participants (get-participants dialogue-id)
        last-turns (map #(last-turn-for dialogue-id %) participants)
        signals (map :signal last-turns)]
    (and (seq participants)                          ; Has participants
         (every? some? last-turns)                   ; All have made a turn
         (every? equilibrium-signals signals))))     ; All in equilibrium state

;; =============================================================================
;; Threshold-based Consensus (N-party)
;; =============================================================================

(defn count-approvals
  "Count participants whose last turn signal is an equilibrium signal.

   dialogue-id: identifier for the dialogue

   Returns: integer count"
  [dialogue-id]
  (let [participants (get-participants dialogue-id)
        last-turns (map #(last-turn-for dialogue-id %) participants)
        signals (map :signal last-turns)]
    (count (filter equilibrium-signals signals))))

(defn count-participants
  "Count total active participants in a dialogue.

   dialogue-id: identifier for the dialogue

   Returns: integer count"
  [dialogue-id]
  (count (get-participants dialogue-id)))

(defn approval-ratio
  "Calculate the ratio of approving participants to total participants.

   dialogue-id: identifier for the dialogue

   Returns: float between 0.0 and 1.0"
  [dialogue-id]
  (let [total (count-participants dialogue-id)]
    (if (zero? total)
      0.0
      (/ (count-approvals dialogue-id) (float total)))))

(defn threshold-consensus?
  "For N-party dialogues with configurable threshold.

   Returns true when the approval ratio meets or exceeds the configured
   threshold (default 1.0 = unanimous).

   dialogue-id: identifier for the dialogue

   Returns: boolean"
  [dialogue-id]
  (let [config (or (get-dialogue-config dialogue-id) default-config)
        threshold (get config :threshold 1.0)]
    (>= (approval-ratio dialogue-id) threshold)))

;; =============================================================================
;; Consensus State Check
;; =============================================================================

(defn turn-count
  "Get total number of turns in a dialogue."
  [dialogue-id]
  (count (schema/get-turns dialogue-id)))

(defn turns-since-last-proposal
  "Count turns since last :propose or :counter signal."
  [dialogue-id]
  (let [turns (reverse (schema/get-turns dialogue-id))]
    (count (take-while #(not (reset-signals (:signal %))) turns))))

(defn check-consensus
  "Comprehensive consensus check for a dialogue.

   Returns one of:
   - :consensus     - Dialogue has reached Nash equilibrium or threshold
   - :continue      - Dialogue should continue (active discussion)
   - :timeout       - Dialogue has exceeded maximum turns
   - :stuck         - Dialogue appears stuck (no progress)
   - :insufficient  - Not enough turns yet for valid consensus

   dialogue-id: identifier for the dialogue

   Returns: keyword status"
  [dialogue-id]
  (let [config (or (get-dialogue-config dialogue-id) default-config)
        {:keys [minimum-turns stuck-threshold timeout-turns]} config
        total-turns (turn-count dialogue-id)
        turns-no-change (turns-since-last-proposal dialogue-id)]
    (cond
      ;; Check timeout first
      (>= total-turns timeout-turns)
      :timeout

      ;; Check if stuck (many turns with no proposal)
      (and (>= turns-no-change stuck-threshold)
           (not (threshold-consensus? dialogue-id)))
      :stuck

      ;; Check if enough turns for valid consensus
      (< total-turns minimum-turns)
      :insufficient

      ;; Check for Nash equilibrium (strict) or threshold consensus
      (or (nash-equilibrium? dialogue-id)
          (threshold-consensus? dialogue-id))
      :consensus

      ;; Otherwise, dialogue continues
      :else
      :continue)))

;; =============================================================================
;; Stuck Detection
;; =============================================================================

(defn stuck?
  "Detect infinite loops - N turns without progress.

   A dialogue is stuck when:
   - More than stuck-threshold turns have passed without a proposal
   - But consensus has NOT been reached (people are just deferring/silent)

   dialogue-id: identifier for the dialogue

   Returns: boolean"
  [dialogue-id]
  (let [config (or (get-dialogue-config dialogue-id) default-config)
        {:keys [stuck-threshold]} config
        turns-no-change (turns-since-last-proposal dialogue-id)]
    (and (>= turns-no-change stuck-threshold)
         (not (threshold-consensus? dialogue-id)))))

(defn calculate-progress-score
  "Calculate a progress score for the dialogue.

   Higher scores indicate more active, productive discussion.

   Returns: {:score float
             :factors {:approval-momentum float
                       :participation float
                       :recent-activity float}}"
  [dialogue-id]
  (let [total-turns (turn-count dialogue-id)
        approval-pct (approval-ratio dialogue-id)
        participant-count (count-participants dialogue-id)
        turns-active (- total-turns (turns-since-last-proposal dialogue-id))

        ;; Momentum: are we getting closer to consensus?
        approval-momentum (* approval-pct 40.0)

        ;; Participation: is everyone engaged?
        participation (if (> participant-count 0)
                        (* (/ turns-active (max 1 total-turns)) 30.0)
                        0.0)

        ;; Recent activity: has there been recent proposals?
        recent-activity (if (< (turns-since-last-proposal dialogue-id) 5)
                          30.0
                          (max 0.0 (- 30.0 (* 3 (turns-since-last-proposal dialogue-id)))))]
    {:score (+ approval-momentum participation recent-activity)
     :factors {:approval-momentum approval-momentum
               :participation participation
               :recent-activity recent-activity}}))

;; =============================================================================
;; Mediator Recruitment
;; =============================================================================

(defn deadlocked?
  "Check if dialogue is in a deadlock state.

   Deadlock: mutual :counter signals between the same participants
   without any :approve/:no-change in between.

   dialogue-id: identifier for the dialogue

   Returns: boolean"
  [dialogue-id]
  (let [turns (schema/get-turns dialogue-id)
        recent-turns (take-last 6 turns)  ; Look at last 6 turns
        recent-signals (map :signal recent-turns)]
    (and (>= (count recent-turns) 4)                        ; Need some history
         (every? #{:counter :propose} recent-signals)       ; All contentious
         (> (count (filter #{:counter} recent-signals)) 2)))) ; Multiple counters

(defn should-recruit-mediator?
  "Determine when to bring in a fresh perspective.

   Recruitment triggers:
   1. Deadlock detected (mutual :counter without progress)
   2. Stuck for too long (threshold exceeded)
   3. Progress score critically low

   dialogue-id: identifier for the dialogue

   Returns: {:recruit? boolean
             :reason keyword or nil
             :urgency :high/:medium/:low or nil}"
  [dialogue-id]
  (let [{:keys [score]} (calculate-progress-score dialogue-id)
        is-stuck (stuck? dialogue-id)
        is-deadlocked (deadlocked? dialogue-id)]
    (cond
      is-deadlocked
      {:recruit? true
       :reason :deadlock
       :urgency :high}

      is-stuck
      {:recruit? true
       :reason :stuck
       :urgency :medium}

      (< score 20.0)
      {:recruit? true
       :reason :low-progress
       :urgency :low}

      :else
      {:recruit? false
       :reason nil
       :urgency nil})))

;; =============================================================================
;; Consensus Result
;; =============================================================================

(defn consensus-result
  "Get comprehensive consensus analysis for a dialogue.

   dialogue-id: identifier for the dialogue

   Returns: {:status keyword
             :nash-equilibrium? boolean
             :approval-ratio float
             :participants int
             :turn-count int
             :progress-score float
             :mediator-needed? boolean
             :mediator-reason keyword or nil}"
  [dialogue-id]
  (let [status (check-consensus dialogue-id)
        {:keys [recruit? reason]} (should-recruit-mediator? dialogue-id)
        {:keys [score]} (calculate-progress-score dialogue-id)]
    {:status status
     :nash-equilibrium? (nash-equilibrium? dialogue-id)
     :approval-ratio (approval-ratio dialogue-id)
     :participants (count-participants dialogue-id)
     :turn-count (turn-count dialogue-id)
     :progress-score score
     :mediator-needed? recruit?
     :mediator-reason reason}))

;; =============================================================================
;; Signal Parsing (from ling dispatches)
;; =============================================================================

(def signal-pattern
  "Regex pattern for extracting signals from ling messages.
   Matches [SIGNAL: <signal>] at start of message."
  #"^\[SIGNAL:\s*(\w+(?:-\w+)*)\]")

(defn parse-signal
  "Extract Nash signal from a ling dispatch message.

   message: string from swarm_dispatch prompt

   Returns: keyword signal or :propose (default)"
  [message]
  (if-let [[_ signal-str] (re-find signal-pattern (or message ""))]
    (let [signal (keyword (str/lower-case signal-str))]
      (if (all-valid-signals signal)
        signal
        :propose))  ; Unknown signal defaults to propose
    :propose))      ; No signal defaults to propose

(defn signal->equilibrium-contribution
  "Determine if a signal contributes to equilibrium.

   signal: keyword

   Returns: :positive, :negative, or :neutral"
  [signal]
  (cond
    (equilibrium-signals signal) :positive
    (reset-signals signal) :negative
    (neutral-signals signal) :neutral
    :else :negative))  ; Unknown signals are disruptive

(comment
  ;; Example usage

  ;; Parse signals from ling messages
  (parse-signal "[SIGNAL: no-change] LGTM. No further improvements.")
  ;; => :no-change

  (parse-signal "[SIGNAL: approve] The implementation looks correct.")
  ;; => :approve

  (parse-signal "[SIGNAL: counter] I disagree with the approach.")
  ;; => :counter

  (parse-signal "Regular message without signal")
  ;; => :propose (default)

  ;; Check signal contribution
  (signal->equilibrium-contribution :no-change)
  ;; => :positive

  (signal->equilibrium-contribution :counter)
  ;; => :negative
  )
