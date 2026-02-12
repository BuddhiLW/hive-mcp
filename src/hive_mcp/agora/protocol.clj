(ns hive-mcp.agora.protocol
  "Unified coordination protocols for Agora multi-agent systems.

   Implements ADR 20260126125019-3e7cb6f5: Agora Unified Coordination.

   PROTOCOLS
   =========
   - ICoordination: Core coordination lifecycle (participants, signals, turns)
   - ISignalable: Event emission and subscription
   - IConsensus: Threshold evaluation and Nash equilibrium

   IMPLEMENTATIONS
   ===============
   | Type       | Turn Model    | Consensus Criteria              | Use Case            |
   |------------|---------------|--------------------------------|---------------------|
   | Dialogue   | Free-form     | Nash equilibrium (threshold)   | Open discussion     |
   | Debate     | Round-robin   | Nash + methodology-weighted    | Structured argument |
   | Pipeline   | Event-driven  | Step completion + validation   | Workflow automation |

   SOLID Compliance:
   - S: Protocols are single-purpose
   - O: New coordination types extend, don't modify
   - L: All implementations satisfy protocol contracts uniformly
   - I: Three small protocols compose as needed
   - D: Core depends on protocols, implementations are injected

   Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
   SPDX-License-Identifier: AGPL-3.0-or-later"
  (:require [hive-mcp.agora.schema :as schema]
            [hive-mcp.agora.consensus :as consensus]
            [hive-mcp.channel.websocket :as ws]
            [datascript.core]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Protocol Definitions
;; =============================================================================

(defprotocol ICoordination
  "Core coordination protocol for multi-agent collaboration.

   All coordination types (Dialogue, Debate, Pipeline) implement this.
   Provides participant management, signaling, consensus checks, and turn control."

  (add-participant [this agent role]
    "Add an agent to the coordination with specified role.
     Role is a keyword describing the agent's function (e.g., :reviewer, :advocate).
     Returns true if added, false if already present or coordination closed.")

  (remove-participant [this agent]
    "Remove an agent from the coordination.
     Returns true if removed, false if not found.")

  (get-participants [this]
    "Return set of current participant agent IDs.")

  (signal [this signal-type data]
    "Send a signal within the coordination.
     signal-type: keyword (:propose :counter :approve :no-change :defer)
     data: map with :from :to :message keys
     Returns map with :signal :turn :dialogue-id")

  (check-consensus [this]
    "Check current consensus status.
     Returns one of: :consensus :continue :timeout :stuck :insufficient")

  (next-turn [this]
    "Advance to next turn and return new turn number.")

  (get-turns [this]
    "Return sequence of all turns in order.")

  (get-id [this]
    "Return the coordination's unique identifier."))

(defprotocol ISignalable
  "Event emission and subscription for coordination events.

   Enables reactive patterns where handlers respond to coordination events
   like turns, consensus, errors, etc."

  (emit [this event-data]
    "Emit an event to all subscribers of the event type.
     event-data: map with :type and event-specific keys
     Returns nil.")

  (subscribe [this event-type handler]
    "Register a handler for an event type.
     event-type: keyword (:turn :consensus :error :participant-joined etc.)
     handler: (fn [event-data] ...) called when event fires
     Returns subscription id for unsubscribe."))

(defprotocol IConsensus
  "Consensus evaluation for coordination systems.

   Provides threshold-based and Nash equilibrium consensus detection."

  (threshold [this]
    "Return the approval threshold (0.0 to 1.0) for this coordination.")

  (evaluate [this votes]
    "Evaluate votes against consensus criteria.
     votes: seq of {:participant id :signal keyword}
     Returns :consensus or :continue"))

;; =============================================================================
;; Signal Definitions (shared across implementations)
;; =============================================================================

(def signals
  "Valid coordination signal types."
  #{:propose :counter :no-change :approve :defer})

(def equilibrium-signals
  "Signals that contribute toward Nash equilibrium."
  #{:no-change :approve})

(def disruption-signals
  "Signals that reset/disrupt equilibrium."
  #{:propose :counter})

;; =============================================================================
;; DialogueCoordination Implementation
;; =============================================================================

(defrecord DialogueCoordination [dialogue-id state-atom]
  ICoordination

  (add-participant [_ agent role]
    (if-let [_dialogue (schema/get-dialogue dialogue-id)]
      (let [c (schema/get-conn)
            db @c
            eid (:db/id (datascript.core/entity db [:agora.dialogue/id dialogue-id]))]
        (when eid
          (datascript.core/transact! c [{:db/id eid
                                         :agora.dialogue/participants agent}])
          (swap! state-atom update :roles assoc agent role)
          (log/info "Added participant" agent "with role" role "to dialogue" dialogue-id)
          true))
      (do
        (log/warn "Cannot add participant - dialogue not found:" dialogue-id)
        false)))

  (remove-participant [_ agent]
    (if-let [_dialogue (schema/get-dialogue dialogue-id)]
      (let [c (schema/get-conn)
            db @c
            eid (:db/id (datascript.core/entity db [:agora.dialogue/id dialogue-id]))]
        (when eid
          (datascript.core/transact! c [[:db/retract eid :agora.dialogue/participants agent]])
          (swap! state-atom update :roles dissoc agent)
          (log/info "Removed participant" agent "from dialogue" dialogue-id)
          true))
      false))

  (get-participants [_]
    (set (:participants (schema/get-dialogue dialogue-id))))

  (signal [this signal-type data]
    (let [{:keys [from to message]} data
          sender (or from (System/getenv "CLAUDE_SWARM_SLAVE_ID") "unknown")]
      ;; Record turn
      (schema/add-turn! dialogue-id
                        {:sender sender
                         :receiver to
                         :message message
                         :signal signal-type})
      ;; Advance turn counter
      (swap! state-atom update :current-turn inc)
      ;; Emit turn event
      (emit this {:type :turn
                  :dialogue-id dialogue-id
                  :turn-num (:current-turn @state-atom)
                  :from sender
                  :to to
                  :signal signal-type})
      ;; Return result
      {:signal signal-type
       :turn (:current-turn @state-atom)
       :dialogue-id dialogue-id}))

  (check-consensus [_]
    (consensus/check-consensus dialogue-id))

  (next-turn [_]
    (swap! state-atom update :current-turn inc)
    (:current-turn @state-atom))

  (get-turns [_]
    (schema/get-turns dialogue-id))

  (get-id [_]
    dialogue-id)

  ISignalable

  (emit [_ event-data]
    (let [event-type (:type event-data)
          handlers (get-in @state-atom [:handlers event-type] [])]
      ;; Call local handlers
      (doseq [h handlers]
        (try
          (h event-data)
          (catch Exception e
            (log/error e "Handler error for event:" event-type))))
      ;; Also emit to WebSocket for Emacs UI
      (ws/emit! (keyword "agora" (name event-type)) event-data)
      nil))

  (subscribe [_ event-type handler]
    (let [sub-id (str (java.util.UUID/randomUUID))]
      (swap! state-atom update-in [:handlers event-type] (fnil conj []) handler)
      (swap! state-atom update :subscriptions assoc sub-id {:type event-type :handler handler})
      sub-id))

  IConsensus

  (threshold [_]
    (or (get-in (schema/get-dialogue dialogue-id) [:config :threshold])
        0.8))

  (evaluate [this votes]
    (let [thresh (threshold this)
          approval-count (count (filter #(equilibrium-signals (:signal %)) votes))
          total (count votes)
          ratio (if (pos? total) (/ approval-count total) 0)]
      (if (>= ratio thresh)
        :consensus
        :continue))))

;; =============================================================================
;; DebateCoordination Implementation
;; =============================================================================

(defrecord DebateCoordination [dialogue-id state-atom]
  ICoordination

  (add-participant [_this agent role]
    ;; Delegate to dialogue coordination logic
    (when-let [_dialogue (schema/get-dialogue dialogue-id)]
      (let [c (schema/get-conn)
            db @c
            eid (:db/id (datascript.core/entity db [:agora.dialogue/id dialogue-id]))]
        (when eid
          (datascript.core/transact! c [{:db/id eid
                                         :agora.dialogue/participants agent}])
          (swap! state-atom update :roles assoc agent role)
          (swap! state-atom update :turn-order conj agent)
          true))))

  (remove-participant [_ agent]
    (when-let [_dialogue (schema/get-dialogue dialogue-id)]
      (let [c (schema/get-conn)
            db @c
            eid (:db/id (datascript.core/entity db [:agora.dialogue/id dialogue-id]))]
        (when eid
          (datascript.core/transact! c [[:db/retract eid :agora.dialogue/participants agent]])
          (swap! state-atom update :roles dissoc agent)
          (swap! state-atom update :turn-order (fn [order] (vec (remove #{agent} order))))
          true))))

  (get-participants [_]
    (set (:participants (schema/get-dialogue dialogue-id))))

  (signal [this signal-type data]
    (let [{:keys [from to message]} data
          turn-order (:turn-order @state-atom)
          current-turn (:current-turn @state-atom)
          sender (or from (nth turn-order (mod current-turn (count turn-order)) "unknown"))]
      ;; Record turn
      (schema/add-turn! dialogue-id
                        {:sender sender
                         :receiver (or to "all")
                         :message message
                         :signal signal-type})
      ;; Advance turn
      (swap! state-atom update :current-turn inc)
      ;; Emit turn event
      (emit this {:type :turn
                  :dialogue-id dialogue-id
                  :turn-num (:current-turn @state-atom)
                  :from sender
                  :to to
                  :signal signal-type
                  :methodology (:methodology @state-atom)})
      {:signal signal-type
       :turn (:current-turn @state-atom)
       :dialogue-id dialogue-id}))

  (check-consensus [_]
    (consensus/check-consensus dialogue-id))

  (next-turn [_]
    (swap! state-atom update :current-turn inc)
    (:current-turn @state-atom))

  (get-turns [_]
    (schema/get-turns dialogue-id))

  (get-id [_]
    dialogue-id)

  ISignalable

  (emit [_ event-data]
    (let [event-type (:type event-data)
          handlers (get-in @state-atom [:handlers event-type] [])]
      (doseq [h handlers]
        (try
          (h event-data)
          (catch Exception e
            (log/error e "Debate handler error for:" event-type))))
      (ws/emit! (keyword "agora" (name event-type)) event-data)
      nil))

  (subscribe [_ event-type handler]
    (let [sub-id (str (java.util.UUID/randomUUID))]
      (swap! state-atom update-in [:handlers event-type] (fnil conj []) handler)
      (swap! state-atom update :subscriptions assoc sub-id {:type event-type :handler handler})
      sub-id))

  IConsensus

  (threshold [_]
    (or (get-in (schema/get-dialogue dialogue-id) [:config :threshold])
        0.8))

  (evaluate [this votes]
    (let [thresh (threshold this)
          methodology (:methodology @state-atom)
          ;; Apply methodology weighting
          weighted-votes (case methodology
                           :fact-based
                           ;; Fact-based debates weight counter signals higher
                           (map (fn [v]
                                  (if (= :counter (:signal v))
                                    (assoc v :weight 1.5)
                                    (assoc v :weight 1.0)))
                                votes)
                           ;; Default: equal weights
                           (map #(assoc % :weight 1.0) votes))
          approval-weight (reduce + (map :weight (filter #(equilibrium-signals (:signal %)) weighted-votes)))
          total-weight (reduce + (map :weight weighted-votes))
          ratio (if (pos? total-weight) (/ approval-weight total-weight) 0)]
      (if (>= ratio thresh)
        :consensus
        :continue))))

;; =============================================================================
;; Extended Protocol for Debate-specific Methods
;; =============================================================================

(defprotocol IDebateExtension
  "Debate-specific extensions beyond core ICoordination."

  (get-methodology [this]
    "Return the debate methodology (:opinion :fact-based :mixed).")

  (get-turn-order [this]
    "Return the participant turn order as vector."))

(extend-protocol IDebateExtension
  DebateCoordination
  (get-methodology [this]
    (:methodology @(.state-atom this)))
  (get-turn-order [this]
    (:turn-order @(.state-atom this)))

  DialogueCoordination
  (get-methodology [_]
    :opinion)  ; Dialogues use opinion methodology by default
  (get-turn-order [this]
    (vec (get-participants this))))

;; =============================================================================
;; Factory Functions
;; =============================================================================

(defn create-dialogue-coordination!
  "Create a DialogueCoordination backed by schema.

   Arguments:
   - opts: map with:
     - :participants - vector of agent IDs (required)
     - :topic - dialogue topic/name
     - :config - optional {:threshold N :timeout-ms N}

   Returns: DialogueCoordination record"
  [{:keys [participants topic config]}]
  {:pre [(vector? participants) (>= (count participants) 1)]}
  (let [{:keys [id]} (schema/create-dialogue!
                      {:participants participants
                       :name (or topic "Unspecified")
                       :config (merge {:threshold 0.8 :timeout-ms 300000} config)})
        state (atom {:current-turn 0
                     :roles (zipmap participants (repeat :participant))
                     :handlers {}
                     :subscriptions {}})]
    (log/info "Created DialogueCoordination:" id)
    (DialogueCoordination. id state)))

(defn create-debate-coordination!
  "Create a DebateCoordination for structured debates.

   Arguments:
   - opts: map with:
     - :topic - debate topic (required)
     - :roles - vector of {:role 'name' :position 'stance'} (required, min 2)
     - :methodology - :opinion | :fact-based | :mixed (default :opinion)
     - :config - optional {:threshold N :timeout-ms N}

   Returns: DebateCoordination record"
  [{:keys [topic roles methodology config]}]
  {:pre [(string? topic) (>= (count roles) 2)]}
  (let [participant-ids (mapv #(str "debate-" (:role %) "-" (System/currentTimeMillis)) roles)
        {:keys [id]} (schema/create-dialogue!
                      {:participants participant-ids
                       :name topic
                       :config (merge {:threshold 0.8
                                       :timeout-ms 600000
                                       :methodology (or methodology :opinion)}
                                      config)})
        roles-map (zipmap participant-ids (map :role roles))
        positions-map (zipmap participant-ids (map :position roles))
        state (atom {:current-turn 0
                     :methodology (or methodology :opinion)
                     :turn-order participant-ids
                     :roles roles-map
                     :positions positions-map
                     :handlers {}
                     :subscriptions {}})]
    (log/info "Created DebateCoordination:" id "methodology:" (or methodology :opinion))
    (DebateCoordination. id state)))

;; =============================================================================
;; Convenience Constructors (wrap existing dialogue)
;; =============================================================================

(defn wrap-dialogue-coordination
  "Wrap an existing dialogue-id as DialogueCoordination.

   Use create-dialogue-coordination! for new dialogues."
  [dialogue-id]
  (let [dialogue (schema/get-dialogue dialogue-id)
        current-turns (count (schema/get-turns dialogue-id))
        state (atom {:current-turn current-turns
                     :roles (zipmap (:participants dialogue) (repeat :participant))
                     :handlers {}
                     :subscriptions {}})]
    (DialogueCoordination. dialogue-id state)))

(defn wrap-debate-coordination
  "Wrap an existing dialogue-id as DebateCoordination.

   Use create-debate-coordination! for new debates."
  [dialogue-id]
  (let [dialogue (schema/get-dialogue dialogue-id)
        current-turns (count (schema/get-turns dialogue-id))
        methodology (get-in dialogue [:config :methodology] :opinion)
        state (atom {:current-turn current-turns
                     :methodology methodology
                     :turn-order (vec (:participants dialogue))
                     :roles (zipmap (:participants dialogue) (repeat :debater))
                     :handlers {}
                     :subscriptions {}})]
    (DebateCoordination. dialogue-id state)))

;; =============================================================================
;; Protocol Predicates
;; =============================================================================

(defn coordination?
  "Check if x satisfies ICoordination."
  [x]
  (satisfies? ICoordination x))

(defn signalable?
  "Check if x satisfies ISignalable."
  [x]
  (satisfies? ISignalable x))

(defn consensus?
  "Check if x satisfies IConsensus."
  [x]
  (satisfies? IConsensus x))

;; =============================================================================
;; Protocol-based Utilities
;; =============================================================================

(defn consensus-reached?
  "Check if coordination has reached consensus."
  [coord]
  (= :consensus (check-consensus coord)))

(defn active?
  "Check if coordination is still active (not consensus/timeout/aborted)."
  [coord]
  (let [dialogue (schema/get-dialogue (get-id coord))]
    (= :active (:status dialogue))))

(defn signal-and-check!
  "Signal and check consensus in one operation.

   Returns: {:signal ... :turn ... :consensus? bool :status keyword}"
  [coord signal-type data]
  (let [result (signal coord signal-type data)
        status (check-consensus coord)]
    (when (= :consensus status)
      (schema/update-dialogue-status! (get-id coord) :consensus))
    (assoc result
           :consensus? (= :consensus status)
           :status status)))
