(ns hive-mcp.tools.agora
  "MCP tools for Agora multi-ling dialogue system.

   Exposes Nash Equilibrium dialogue infrastructure as MCP tools:
   - agora_create_dialogue: Create a new dialogue session
   - agora_dispatch: Send message within dialogue (with signal parsing)
   - agora_check_consensus: Check Nash equilibrium status
   - agora_list_dialogues: List all dialogues
   - agora_join_dialogue: Add participant to dialogue

   SOLID: SRP - MCP tool interface for Agora domain.
   CLARITY: L - Layer separation from domain logic (agora.*)."
  (:require [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.consensus :as consensus]
            [hive-mcp.agora.schema :as schema]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Response Helpers
;; ============================================================

(defn- mcp-success
  "Build successful MCP response with JSON data."
  [data]
  {:type "text" :text (json/write-str data)})

(defn- mcp-error
  "Build error MCP response."
  [message]
  {:type "text" :text (json/write-str {:error message}) :isError true})

;; ============================================================
;; Tool Handlers
;; ============================================================

(defn handle-agora-create-dialogue
  "Create a new Agora dialogue session.

   Participants must be slave-ids of lings in the swarm.
   Config supports :threshold (0.0-1.0) and :timeout-ms.

   Returns: {:dialogue-id \"...\"}."
  [{:keys [participants topic _config]}]
  (try
    (let [participants-vec (if (vector? participants)
                             participants
                             (vec participants))
          dialogue-id (dialogue/create-dialogue
                       {:participants participants-vec
                        :topic (or topic "Unspecified dialogue")})]
      (log/info "Created Agora dialogue:" dialogue-id
                "with participants:" participants-vec)
      (mcp-success {:dialogue-id dialogue-id
                    :participants participants-vec
                    :topic (or topic "Unspecified dialogue")
                    :status "active"}))
    (catch Exception e
      (log/error e "Failed to create dialogue")
      (mcp-error (str "Failed to create dialogue: " (.getMessage e))))))

(defn handle-agora-dispatch
  "Dispatch a message within an Agora dialogue.

   Signal can be provided explicitly via :signal parameter, or parsed from
   [SIGNAL: X] prefix in message. Explicit signal takes priority.

   Valid signals (Nash equilibrium tracking):
   - propose   - Introduce change (resets equilibrium)
   - counter   - Disagree with reasoning (resets equilibrium)
   - approve   - Accept current state (+1 equilibrium)
   - no-change - No changes needed (+1 equilibrium)
   - defer     - Yield to another's judgment (neutral)

   If neither explicit signal nor prefix provided, defaults to :propose.

   Auto-checks Nash equilibrium after recording turn.

   Returns: {:dialogue-id, :turn, :signal, :consensus-status}."
  [{:keys [dialogue_id to message from timeout_ms files signal]}]
  (try
    (let [signal-kw (when signal (keyword signal))
          result (dialogue/dialogue-dispatch
                  {:dialogue-id dialogue_id
                   :from from
                   :to to
                   :message message
                   :signal signal-kw
                   :timeout_ms timeout_ms
                   :files files})
          ;; Check consensus status after dispatch
          consensus-status (consensus/check-consensus dialogue_id)
          ;; Safe name conversion with nil guards
          signal-name (if-let [s (:signal result)] (name s) "unknown")
          consensus-name (if consensus-status (name consensus-status) "unknown")]
      (log/info "Agora dispatch to" to "in dialogue" dialogue_id
                "signal:" (:signal result) "detection:" (:signal-detection result)
                "consensus:" consensus-status)
      (mcp-success {:dialogue-id dialogue_id
                    :turn (:turn result)
                    :signal signal-name
                    :signal-detection (when-let [d (:signal-detection result)] (name d))
                    :consensus-status consensus-name
                    :dispatch-result (:dispatch-result result)}))
    (catch clojure.lang.ExceptionInfo e
      (log/warn "Agora dispatch failed:" (ex-message e) (ex-data e))
      (mcp-error (str "Dispatch failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Agora dispatch error")
      (mcp-error (str "Dispatch error: " (.getMessage e))))))

(defn handle-agora-check-consensus
  "Check Nash equilibrium status for a dialogue.

   Returns comprehensive analysis:
   - :status - :consensus, :continue, :timeout, :stuck, :insufficient
   - :nash-equilibrium? - True if all participants signaled approval
   - :approval-ratio - Ratio of approving participants (0.0-1.0)
   - :participants - Number of participants
   - :turn-count - Total turns in dialogue
   - :mediator-needed? - True if dialogue is stuck/deadlocked
   - :participant-status - Per-participant signal status for debugging."
  [{:keys [dialogue_id]}]
  (try
    ;; Validate dialogue_id is provided
    (when-not dialogue_id
      (throw (ex-info "Missing required parameter: dialogue_id" {:type :validation})))
    ;; Check if dialogue exists first to provide clear error
    (let [dialogue (schema/get-dialogue dialogue_id)]
      (when-not dialogue
        (throw (ex-info (str "Dialogue not found: " dialogue_id)
                        {:type :not-found :dialogue-id dialogue_id})))
      (let [result (consensus/consensus-result dialogue_id)
            ;; Get last turn for preview
            turns (schema/get-turns dialogue_id)
            last-turn (last (sort-by :turn-number turns))
            ;; Truncate message for preview (first 100 chars)
            msg-preview (when-let [msg (:message last-turn)]
                          (if (> (count msg) 100)
                            (str (subs msg 0 100) "...")
                            msg))
            ;; Build per-participant status for debugging
            participants (consensus/get-participants dialogue_id)
            participant-status (mapv (fn [p]
                                       (let [turn (consensus/last-turn-for dialogue_id p)
                                             signal (:signal turn)]
                                         {:participant p
                                          :short-name (consensus/extract-short-name p)
                                          :last-signal (when signal (name signal))
                                          :in-equilibrium? (boolean
                                                            (when signal
                                                              (contains? consensus/equilibrium-signals signal)))}))
                                     participants)]
        (log/debug "Consensus check for" dialogue_id ":" result)
        (mcp-success {:dialogue-id dialogue_id
                      :status (if-let [s (:status result)]
                                (name s)
                                "unknown")
                      :nash-equilibrium? (boolean (:nash-equilibrium? result))
                      :approval-ratio (or (:approval-ratio result) 0.0)
                      :participants (or (:participants result) 0)
                      :turn-count (or (:turn-count result) 0)
                      :progress-score (or (:progress-score result) 0.0)
                      :mediator-needed? (boolean (:mediator-needed? result))
                      :mediator-reason (when-let [r (:mediator-reason result)]
                                         (name r))
                      ;; Per-participant status for debugging signal issues
                      :participant-status participant-status
                      ;; Last turn preview for quick context
                      :last-turn (when last-turn
                                   {:from (:sender last-turn)
                                    :signal (when-let [s (:signal last-turn)] (name s))
                                    :preview msg-preview})})))
    (catch clojure.lang.ExceptionInfo e
      (log/warn "Consensus check failed:" (ex-message e) (ex-data e))
      (mcp-error (str "Consensus check failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Consensus check failed")
      (mcp-error (str "Consensus check failed: " (.getMessage e))))))

(defn handle-agora-list-dialogues
  "List all Agora dialogues, optionally filtered by status.

   Status filter: :active, :consensus, :timeout, :aborted.

   Returns vector of {:id, :topic, :status, :participants, :turn-count}."
  [{:keys [status]}]
  (try
    (let [status-kw (when status (keyword status))
          dialogues (if status-kw
                      (schema/list-dialogues status-kw)
                      (schema/list-dialogues))
          ;; Enrich with turn counts
          enriched (mapv (fn [d]
                           (let [turns (schema/get-turns (:id d))
                                 dialogue (schema/get-dialogue (:id d))]
                             {:id (:id d)
                              :topic (:name d)
                              :status (if-let [s (:status d)] (name s) "unknown")
                              :participants (vec (or (:participants dialogue) []))
                              :turn-count (count turns)
                              :created (:created d)}))
                         dialogues)]
      (log/debug "Listed" (count enriched) "dialogues"
                 (when status-kw (str "with status " status-kw)))
      (mcp-success {:dialogues enriched
                    :count (count enriched)
                    :filter (when status-kw (name status-kw))}))
    (catch Exception e
      (log/error e "Failed to list dialogues")
      (mcp-error (str "Failed to list dialogues: " (.getMessage e))))))

(defn handle-agora-join-dialogue
  "Add a participant to an existing dialogue.

   Used for dynamic role assignment when lings recruit new voices.

   Returns: {:success true/false, :dialogue-id, :participant}."
  [{:keys [dialogue_id slave_id]}]
  (try
    (let [joined? (dialogue/join-dialogue dialogue_id slave_id)]
      (if joined?
        (do
          (log/info "Participant" slave_id "joined dialogue" dialogue_id)
          (mcp-success {:success true
                        :dialogue-id dialogue_id
                        :participant slave_id}))
        (do
          (log/warn "Failed to join dialogue" dialogue_id "- not found")
          (mcp-error (str "Dialogue not found: " dialogue_id)))))
    (catch Exception e
      (log/error e "Failed to join dialogue")
      (mcp-error (str "Failed to join dialogue: " (.getMessage e))))))

(defn handle-agora-get-history
  "Retrieve dialogue transcript with all turns.

   Returns the full dialogue history including all messages,
   signals, and metadata. Useful for agents to catch up on
   dialogue state or review the conversation.

   Returns: {:dialogue-id, :topic, :status, :participants, :turns}."
  [{:keys [dialogue_id limit]}]
  (try
    (when-not dialogue_id
      (throw (ex-info "Missing required parameter: dialogue_id" {:type :validation})))
    (let [dialogue (dialogue/get-dialogue dialogue_id)]
      (when-not dialogue
        (throw (ex-info (str "Dialogue not found: " dialogue_id)
                        {:type :not-found :dialogue-id dialogue_id})))
      (let [all-turns (dialogue/get-dialogue-turns dialogue_id)
            ;; Apply limit if provided
            turns (if (and limit (pos? limit))
                    (take-last limit all-turns)
                    all-turns)
            ;; Format turns for output
            formatted-turns (mapv (fn [t]
                                    {:turn-num (:turn-num t)
                                     :from (:sender t)
                                     :to (:receiver t)
                                     :signal (when-let [s (:signal t)] (name s))
                                     :message (:message t)
                                     :timestamp (when-let [ts (:timestamp t)]
                                                  (.getTime ts))})
                                  turns)]
        (log/debug "Retrieved" (count formatted-turns) "turns for dialogue" dialogue_id)
        (mcp-success {:dialogue-id dialogue_id
                      :topic (:topic dialogue)
                      :status (when-let [s (:status dialogue)] (name s))
                      :participants (vec (:participants dialogue))
                      :turn-count (count all-turns)
                      :turns formatted-turns})))
    (catch clojure.lang.ExceptionInfo e
      (log/warn "Get history failed:" (ex-message e) (ex-data e))
      (mcp-error (str "Get history failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Failed to get dialogue history")
      (mcp-error (str "Failed to get history: " (.getMessage e))))))

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  [{:name "agora_create_dialogue"
    :description "Create a new Agora dialogue session for multi-ling Nash Equilibrium consensus. Requires at least 2 participants (ling slave-ids)."
    :inputSchema {:type "object"
                  :properties {:participants {:type "array"
                                              :items {:type "string"}
                                              :description "Vector of ling slave-ids participating in the dialogue (minimum 2)"}
                               :topic {:type "string"
                                       :description "Human-readable description of what the dialogue is about"}
                               :config {:type "object"
                                        :description "Optional config: {threshold: 0.8, timeout-ms: 300000}"
                                        :properties {:threshold {:type "number"
                                                                 :description "Approval threshold (0.0-1.0, default 0.8)"}
                                                     :timeout-ms {:type "number"
                                                                  :description "Timeout in milliseconds (default 300000)"}}}}
                  :required ["participants"]}
    :handler handle-agora-create-dialogue}

   {:name "agora_dispatch"
    :description "Dispatch a message within an Agora dialogue. Signal can be provided via `signal` parameter (recommended) or parsed from [SIGNAL: X] prefix in message (backward compatible). Signals: propose (reset), counter (reset), approve (+1), no-change (+1), defer (neutral). Auto-checks consensus after turn."
    :inputSchema {:type "object"
                  :properties {:dialogue_id {:type "string"
                                             :description "ID of the dialogue"}
                               :to {:type "string"
                                    :description "Target ling slave-id to receive the message"}
                               :message {:type "string"
                                         :description "Message content. Signal detection priority: 1) explicit `signal` param, 2) [SIGNAL: X] prefix, 3) natural language ('I accept', 'LGTM', 'looks good' -> approve; 'I disagree', 'have concerns' -> counter), 4) default: propose."}
                               :signal {:type "string"
                                        :enum ["propose" "counter" "approve" "no-change" "defer"]
                                        :description "Optional signal. Takes priority over message prefix. If omitted, parsed from message prefix or defaults to 'propose'."}
                               :from {:type "string"
                                      :description "Sender slave-id (defaults to CLAUDE_SWARM_SLAVE_ID env)"}
                               :timeout_ms {:type "number"
                                            :description "Optional dispatch timeout in milliseconds"}
                               :files {:type "array"
                                       :items {:type "string"}
                                       :description "Optional list of files this message relates to"}}
                  :required ["dialogue_id" "to" "message"]}
    :handler handle-agora-dispatch}

   {:name "agora_check_consensus"
    :description "Check Nash equilibrium and consensus status for a dialogue. Returns comprehensive analysis including approval ratio, participant count, and mediator recruitment signals."
    :inputSchema {:type "object"
                  :properties {:dialogue_id {:type "string"
                                             :description "ID of the dialogue to check"}}
                  :required ["dialogue_id"]}
    :handler handle-agora-check-consensus}

   {:name "agora_list_dialogues"
    :description "List all Agora dialogues with optional status filter. Returns dialogue summaries with turn counts and participant lists."
    :inputSchema {:type "object"
                  :properties {:status {:type "string"
                                        :enum ["active" "consensus" "timeout" "aborted"]
                                        :description "Optional filter by dialogue status"}}}
    :handler handle-agora-list-dialogues}

   {:name "agora_join_dialogue"
    :description "Add a participant to an existing dialogue. Used for dynamic role assignment when recruiting new voices to the discussion."
    :inputSchema {:type "object"
                  :properties {:dialogue_id {:type "string"
                                             :description "ID of the dialogue to join"}
                               :slave_id {:type "string"
                                          :description "Slave-id of the ling joining the dialogue"}}
                  :required ["dialogue_id" "slave_id"]}
    :handler handle-agora-join-dialogue}

   {:name "agora_get_history"
    :description "Retrieve dialogue transcript with all turns. Returns messages, signals, and timestamps for catching up on dialogue state or reviewing the conversation."
    :inputSchema {:type "object"
                  :properties {:dialogue_id {:type "string"
                                             :description "ID of the dialogue to retrieve history for"}
                               :limit {:type "integer"
                                       :description "Optional: limit to last N turns (default: all)"}}
                  :required ["dialogue_id"]}
    :handler handle-agora-get-history}])
