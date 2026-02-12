(ns hive-mcp.agora.schema
  "DataScript schema and CRUD for Agora multi-ling dialogue system.

   Agora enables structured dialogue between lings with:
   - Dialogue sessions (conversation threads)
   - Dialogue turns (individual messages with signals)
   - Consensus tracking via signal types

   Signal types follow Greek democratic tradition:
   - :propose   - Introduce a new idea/approach
   - :counter   - Disagree with reasoning
   - :no-change - Acknowledge without modifying position
   - :approve   - Agree and support
   - :defer     - Yield to another's judgment

   DDD: Aggregate Root pattern (dialogue owns turns)."
  (:require [datascript.core :as d]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(def dialogue-statuses
  "Valid dialogue status values.

   :active    - Dialogue in progress
   :consensus - Reached agreement (approval threshold met)
   :timeout   - Timed out without consensus
   :aborted   - Cancelled by participant or coordinator"
  #{:active :consensus :timeout :aborted})

(def turn-signals
  "Valid turn signal values for ling-to-ling dialogue.

   :propose   - Introduce a new idea or approach
   :counter   - Disagree with reasoning, offer alternative
   :no-change - Acknowledge message without modifying position
   :approve   - Agree and support the proposal
   :defer     - Yield to another's judgment on this matter"
  #{:propose :counter :no-change :approve :defer})


(def schema
  "DataScript schema for Agora dialogue system.

   Design notes:
   - Dialogues are aggregate roots owning turns
   - Turns reference dialogues via :agora.turn/dialogue-id
   - :db/index true for frequent dialogue-scoped queries
   - Task refs enable linking dialogue turns to swarm tasks"

  {;;; =========================================================================
   ;;; Dialogue Session Entity (Aggregate Root)
   ;;; =========================================================================

   :agora.dialogue/id
   {:db/doc "Unique identifier for the dialogue (UUID string)"
    :db/unique :db.unique/identity}

   :agora.dialogue/name
   {:db/doc "Human-readable name for the dialogue (e.g., 'auth-design-discussion')"}

   :agora.dialogue/participants
   {:db/doc "Set of ling slave-ids participating in this dialogue"
    :db/cardinality :db.cardinality/many}

   :agora.dialogue/status
   {:db/doc "Current status: :active :consensus :timeout :aborted"}

   :agora.dialogue/config
   {:db/doc "Configuration map (EDN string): {:threshold 0.8 :timeout-ms 300000}"}

   :agora.dialogue/created
   {:db/doc "Timestamp when dialogue was created"}

   ;;; =========================================================================
   ;;; Dialogue Turn Entity
   ;;; =========================================================================

   :agora.turn/id
   {:db/doc "Unique identifier for the turn (UUID string)"
    :db/unique :db.unique/identity}

   :agora.turn/dialogue-id
   {:db/doc "ID of the parent dialogue (indexed for efficient queries)"
    :db/index true}

   :agora.turn/sender
   {:db/doc "Slave-id of the ling sending this turn"}

   :agora.turn/receiver
   {:db/doc "Slave-id of the target ling (or 'all' for broadcast)"}

   :agora.turn/message
   {:db/doc "Message content (the prompt/response text)"}

   :agora.turn/signal
   {:db/doc "Signal type: :propose :counter :no-change :approve :defer"}

   :agora.turn/timestamp
   {:db/doc "When this turn was created"}

   :agora.turn/task-ref
   {:db/doc "Reference to resulting task entity (if dispatched)"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :agora.turn/in-reply-to
   {:db/doc "Reference to turn being replied to (conversation threading)"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :agora.turn/turn-number
   {:db/doc "Sequence number within the dialogue (1-indexed)"}

   ;;; =========================================================================
   ;;; Stage Support (Two-Stage Agora)
   ;;; =========================================================================

   :agora.dialogue/stage
   {:db/doc "Current stage: :research or :debate (default :debate for non-staged)"}

   :agora.dialogue/stage-config
   {:db/doc "Stage configuration (EDN string): {:research {:focus-areas [...]} :debate {:use-evidence true}}"}

   :agora.dialogue/evidence-pool
   {:db/doc "Accumulated evidence from research stage (EDN string): [{:source :content :confidence}]"}})


(defonce ^:private conn (atom nil))

(defn create-conn
  "Create a new DataScript connection with Agora schema.
   Can be merged with swarm schema if needed for cross-entity refs."
  []
  (d/create-conn schema))

(defn get-conn
  "Get the global Agora connection, creating if needed."
  []
  (or @conn
      (do
        (reset! conn (create-conn))
        (log/info "Created Agora DataScript connection")
        @conn)))

(defn reset-conn!
  "Reset the Agora connection to empty state. Use for testing."
  []
  (reset! conn (create-conn))
  (log/debug "Agora DataScript connection reset"))

(defn ensure-conn
  "Ensure connection exists, return it."
  []
  (get-conn))


(defn now
  "Current timestamp as epoch millis."
  []
  (System/currentTimeMillis))

(defn gen-id
  "Generate a unique ID string."
  []
  (str (java.util.UUID/randomUUID)))

(defn config->edn
  "Serialize config map to EDN string for storage."
  [config]
  (pr-str config))

(defn edn->config
  "Deserialize config EDN string back to map."
  [edn-str]
  (when edn-str
    (read-string edn-str)))


(defn create-dialogue!
  "Create a new dialogue session.

   Arguments:
     opts - Map with keys:
            :id           - Dialogue ID (auto-generated if nil)
            :name         - Human-readable name
            :participants - Collection of ling slave-ids
            :config       - Config map {:threshold N :timeout-ms N}

   Returns:
     Map with :id and :tx-report"
  [{:keys [id name participants config]
    :or {config {:threshold 0.8 :timeout-ms 300000}}}]
  {:pre [(seq participants)]}
  (let [c (ensure-conn)
        dialogue-id (or id (gen-id))
        tx-data {:agora.dialogue/id dialogue-id
                 :agora.dialogue/name (or name (str "dialogue-" (subs dialogue-id 0 8)))
                 :agora.dialogue/participants (vec participants)
                 :agora.dialogue/status :active
                 :agora.dialogue/config (config->edn config)
                 :agora.dialogue/created (now)}]
    (log/info "Creating dialogue:" dialogue-id "with participants:" participants)
    {:id dialogue-id
     :tx-report (d/transact! c [tx-data])}))

(defn get-dialogue
  "Get a dialogue by ID.

   Arguments:
     dialogue-id - Dialogue identifier

   Returns:
     Map with dialogue attributes, or nil if not found"
  [dialogue-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:agora.dialogue/id dialogue-id])]
      {:id (:agora.dialogue/id e)
       :name (:agora.dialogue/name e)
       :participants (vec (:agora.dialogue/participants e))
       :status (:agora.dialogue/status e)
       :config (edn->config (:agora.dialogue/config e))
       :created (:agora.dialogue/created e)})))

(defn update-dialogue-status!
  "Update a dialogue's status.

   Arguments:
     dialogue-id - Dialogue to update
     new-status  - New status (must be in dialogue-statuses)

   Returns:
     Transaction report or nil if dialogue not found"
  [dialogue-id new-status]
  {:pre [(contains? dialogue-statuses new-status)]}
  (let [c (ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:agora.dialogue/id dialogue-id]))]
      (log/info "Dialogue" dialogue-id "status changed to:" new-status)
      (d/transact! c [{:db/id eid
                       :agora.dialogue/status new-status}]))))

(defn list-dialogues
  "List all dialogues, optionally filtered by status.

   Arguments:
     status - Optional filter by dialogue status

   Returns:
     Sequence of dialogue maps"
  ([]
   (list-dialogues nil))
  ([status]
   (let [c (ensure-conn)
         db @c
         query (if status
                 '[:find ?id ?name ?status ?created
                   :in $ ?filter-status
                   :where
                   [?e :agora.dialogue/id ?id]
                   [?e :agora.dialogue/name ?name]
                   [?e :agora.dialogue/status ?status]
                   [?e :agora.dialogue/created ?created]
                   [(= ?status ?filter-status)]]
                 '[:find ?id ?name ?status ?created
                   :where
                   [?e :agora.dialogue/id ?id]
                   [?e :agora.dialogue/name ?name]
                   [?e :agora.dialogue/status ?status]
                   [?e :agora.dialogue/created ?created]])
         results (if status
                   (d/q query db status)
                   (d/q query db))]
     (->> results
          (map (fn [[id name status created]]
                 {:id id :name name :status status :created created}))
          (sort-by :created)))))


(defn add-turn!
  "Add a turn to a dialogue.

   Arguments:
     dialogue-id - Parent dialogue ID
     opts        - Map with keys:
                   :sender      - Slave-id of sender (required)
                   :receiver    - Slave-id of receiver (required)
                   :message     - Message content (required)
                   :signal      - Signal type (default :propose)
                   :in-reply-to - Turn ID being replied to
                   :task-ref    - Task entity ID (if task dispatched)

   Returns:
     Map with :id, :turn-number, and :tx-report, or nil if dialogue not found"
  [dialogue-id {:keys [sender receiver message signal in-reply-to task-ref]
                :or {signal :propose}}]
  {:pre [(string? sender)
         (string? receiver)
         (string? message)
         (contains? turn-signals signal)]}
  (let [c (ensure-conn)
        db @c]
    ;; Verify dialogue exists
    (when-let [_dialogue (d/entity db [:agora.dialogue/id dialogue-id])]
      (let [turn-id (gen-id)
            ;; Count existing turns to get next number
            existing-count (count (d/q '[:find ?t
                                         :in $ ?did
                                         :where
                                         [?t :agora.turn/dialogue-id ?did]]
                                       db dialogue-id))
            turn-number (inc existing-count)
            tx-data (cond-> {:agora.turn/id turn-id
                             :agora.turn/dialogue-id dialogue-id
                             :agora.turn/sender sender
                             :agora.turn/receiver receiver
                             :agora.turn/message message
                             :agora.turn/signal signal
                             :agora.turn/timestamp (now)
                             :agora.turn/turn-number turn-number}
                      in-reply-to (assoc :agora.turn/in-reply-to
                                         [:agora.turn/id in-reply-to])
                      task-ref (assoc :agora.turn/task-ref
                                      [:task/id task-ref]))]
        (log/debug "Adding turn" turn-number "to dialogue" dialogue-id
                   ":" sender "->" receiver "signal:" signal)
        {:id turn-id
         :turn-number turn-number
         :tx-report (d/transact! c [tx-data])}))))

(defn get-turn
  "Get a turn by ID.

   Arguments:
     turn-id - Turn identifier

   Returns:
     Map with turn attributes, or nil if not found"
  [turn-id]
  (let [c (ensure-conn)
        db @c]
    (when-let [e (d/entity db [:agora.turn/id turn-id])]
      {:id (:agora.turn/id e)
       :dialogue-id (:agora.turn/dialogue-id e)
       :sender (:agora.turn/sender e)
       :receiver (:agora.turn/receiver e)
       :message (:agora.turn/message e)
       :signal (:agora.turn/signal e)
       :timestamp (:agora.turn/timestamp e)
       :turn-number (:agora.turn/turn-number e)
       :in-reply-to (when-let [ref (:agora.turn/in-reply-to e)]
                      (:agora.turn/id ref))
       :task-ref (when-let [ref (:agora.turn/task-ref e)]
                   (:task/id ref))})))

(defn get-turns
  "Get all turns for a dialogue, ordered by turn number.

   Arguments:
     dialogue-id - Dialogue to get turns for

   Returns:
     Sequence of turn maps ordered by turn-number"
  [dialogue-id]
  (let [c (ensure-conn)
        db @c
        results (d/q '[:find ?id ?sender ?receiver ?message ?signal ?ts ?turn-num
                       :in $ ?did
                       :where
                       [?e :agora.turn/id ?id]
                       [?e :agora.turn/dialogue-id ?did]
                       [?e :agora.turn/sender ?sender]
                       [?e :agora.turn/receiver ?receiver]
                       [?e :agora.turn/message ?message]
                       [?e :agora.turn/signal ?signal]
                       [?e :agora.turn/timestamp ?ts]
                       [?e :agora.turn/turn-number ?turn-num]]
                     db dialogue-id)]
    (->> results
         (map (fn [[id sender receiver message signal ts turn-num]]
                {:id id
                 :dialogue-id dialogue-id
                 :sender sender
                 :receiver receiver
                 :message message
                 :signal signal
                 :timestamp ts
                 :turn-number turn-num}))
         (sort-by :turn-number))))

(defn get-turns-by-sender
  "Get all turns from a specific sender in a dialogue.

   Arguments:
     dialogue-id - Dialogue to search
     sender-id   - Slave-id of sender

   Returns:
     Sequence of turn maps from that sender"
  [dialogue-id sender-id]
  (let [c (ensure-conn)
        db @c
        results (d/q '[:find ?id ?receiver ?message ?signal ?ts ?turn-num
                       :in $ ?did ?sender
                       :where
                       [?e :agora.turn/id ?id]
                       [?e :agora.turn/dialogue-id ?did]
                       [?e :agora.turn/sender ?sender]
                       [?e :agora.turn/receiver ?receiver]
                       [?e :agora.turn/message ?message]
                       [?e :agora.turn/signal ?signal]
                       [?e :agora.turn/timestamp ?ts]
                       [?e :agora.turn/turn-number ?turn-num]]
                     db dialogue-id sender-id)]
    (->> results
         (map (fn [[id receiver message signal ts turn-num]]
                {:id id
                 :dialogue-id dialogue-id
                 :sender sender-id
                 :receiver receiver
                 :message message
                 :signal signal
                 :timestamp ts
                 :turn-number turn-num}))
         (sort-by :turn-number))))


(defn count-signals
  "Count signal types for a dialogue.

   Arguments:
     dialogue-id - Dialogue to analyze

   Returns:
     Map of {:propose N :counter N :approve N :no-change N :defer N}"
  [dialogue-id]
  (let [turns (get-turns dialogue-id)]
    (reduce (fn [acc {:keys [signal]}]
              (update acc signal (fnil inc 0)))
            {:propose 0 :counter 0 :approve 0 :no-change 0 :defer 0}
            turns)))

(defn check-consensus
  "Check if dialogue has reached consensus based on approval threshold.

   Arguments:
     dialogue-id - Dialogue to check

   Returns:
     Map {:reached? bool :approval-ratio float :threshold float}"
  [dialogue-id]
  (let [{:keys [config]} (get-dialogue dialogue-id)
        threshold (or (:threshold config) 0.8)
        signals (count-signals dialogue-id)
        total (reduce + (vals signals))
        approvals (+ (:approve signals) (:defer signals))
        ratio (if (pos? total) (/ approvals total) 0)]
    {:reached? (>= ratio threshold)
     :approval-ratio (double ratio)
     :threshold threshold
     :signal-counts signals}))
