(ns hive-mcp.tools.agora
  "MCP tools for Agora multi-ling dialogue system.

   Exposes Nash Equilibrium dialogue infrastructure as MCP tools:
   - agora_create_dialogue: Create a new dialogue session (ling-based)
   - agora_create_debate: Create auto-orchestrated drone debate (new!)
   - agora_dispatch: Send message within dialogue (with signal parsing)
   - agora_check_consensus: Check Nash equilibrium status
   - agora_list_dialogues: List all dialogues
   - agora_join_dialogue: Add participant to dialogue
   - agora_debate_status: Get drone debate status (new!)

  (:require [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.debate :as debate]
            [hive-mcp.agora.consensus :as consensus]
            [hive-mcp.agora.schema :as schema]
            [hive-mcp.agora.stages :as stages]
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
;; Drone Debate Handlers (ADR 20260124224722-51e4fad6)
;; ============================================================

(defn handle-agora-create-debate
  "Create an auto-orchestrated drone debate.

   Spawns drones as debate participants and runs to consensus.
   No manual dispatch needed - fully automated.

   Arguments:
   - topic: What the debate is about
   - roles: Array of {role: string, position: string}
   - methodology: 'opinion' | 'fact-based' | 'mixed' (default: opinion)
   - blocking: If true, runs debate to completion (default: false)

   Returns: {:dialogue-id :participants :status}"
  [{:keys [topic roles methodology blocking]}]
  (try
    (when-not topic
      (throw (ex-info "Missing required parameter: topic" {:type :validation})))
    (when (or (nil? roles) (< (count roles) 2))
      (throw (ex-info "Requires at least 2 roles" {:type :validation})))
    (let [role-maps (mapv (fn [r]
                            {:role (or (:role r) (get r "role"))
                             :position (or (:position r) (get r "position"))})
                          roles)
          methodology-kw (when methodology (keyword methodology))
          result (if blocking
                   ;; Blocking: run to completion
                   (debate/start-debate! topic role-maps {:methodology methodology-kw})
                   ;; Non-blocking: create and start first turn
                   (debate/start-debate-async! topic role-maps {:methodology methodology-kw}))]
      (log/info "Created drone debate:" (:dialogue-id result)
                "blocking:" blocking "methodology:" (or methodology-kw :opinion))
      (mcp-success result))
    (catch clojure.lang.ExceptionInfo e
      (log/warn "Create debate failed:" (ex-message e))
      (mcp-error (str "Create debate failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Create debate error")
      (mcp-error (str "Create debate error: " (.getMessage e))))))

(defn handle-agora-debate-status
  "Get status of a drone debate.

   Returns: {:dialogue-id :status :turn-count :participants :methodology}"
  [{:keys [dialogue_id]}]
  (try
    (when-not dialogue_id
      (throw (ex-info "Missing required parameter: dialogue_id" {:type :validation})))
    (if-let [status (debate/get-debate-status dialogue_id)]
      (mcp-success status)
      (mcp-error (str "Debate not found: " dialogue_id)))
    (catch clojure.lang.ExceptionInfo e
      (mcp-error (str "Status check failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Debate status error")
      (mcp-error (str "Status error: " (.getMessage e))))))

(defn handle-agora-continue-debate
  "Continue an async drone debate by executing next turn.

   Use this after start_debate_async to advance turns manually.

   Returns: turn result or consensus status"
  [{:keys [dialogue_id]}]
  (try
    (when-not dialogue_id
      (throw (ex-info "Missing required parameter: dialogue_id" {:type :validation})))
    (if-let [result (debate/continue-debate! dialogue_id)]
      (mcp-success result)
      (mcp-error (str "Debate not active or not found: " dialogue_id)))
    (catch clojure.lang.ExceptionInfo e
      (mcp-error (str "Continue failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Continue debate error")
      (mcp-error (str "Continue error: " (.getMessage e))))))

(defn handle-agora-list-debates
  "List all active drone debates."
  [_]
  (try
    (let [debates (debate/list-active-debates)]
      (mcp-success {:debates debates :count (count debates)}))
    (catch Exception e
      (log/error e "List debates error")
      (mcp-error (str "List error: " (.getMessage e))))))

;; ============================================================
;; Staged Debate Handlers (Two-Stage Agora)
;; ============================================================

(defn handle-agora-create-staged-debate
  "Create a two-stage debate with research + debate phases.

   Stage 1: Research drones gather evidence
   Stage 2: Debate drones argue with evidence

   Arguments:
   - topic: What the debate is about
   - research_roles: Array of {role, focus_area} for research drones
   - debate_roles: Array of {role, position} for debate drones
   - methodology: opinion | fact-based | mixed

   Returns: {:dialogue-id :stage :research-participants :debate-participants}"
  [{:keys [topic research_roles debate_roles methodology]}]
  (try
    (when-not topic
      (throw (ex-info "Missing required parameter: topic" {:type :validation})))
    (when (or (nil? research_roles) (empty? research_roles))
      (throw (ex-info "Requires at least 1 research role" {:type :validation})))
    (when (or (nil? debate_roles) (< (count debate_roles) 2))
      (throw (ex-info "Requires at least 2 debate roles" {:type :validation})))
    (let [research-maps (mapv (fn [r]
                                {:role (or (:role r) (get r "role"))
                                 :position (or (:focus_area r) (get r "focus_area")
                                               (:role r) (get r "role"))})
                              research_roles)
          debate-maps (mapv (fn [r]
                              {:role (or (:role r) (get r "role"))
                               :position (or (:position r) (get r "position"))})
                            debate_roles)
          result (stages/create-staged-debate!
                  topic research-maps debate-maps
                  {:methodology (when methodology (keyword methodology))})]
      (log/info "Created staged debate:" (:dialogue-id result)
                "research:" (count research-maps) "debate:" (count debate-maps))
      (mcp-success result))
    (catch clojure.lang.ExceptionInfo e
      (log/warn "Create staged debate failed:" (ex-message e))
      (mcp-error (str "Create staged debate failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Create staged debate error")
      (mcp-error (str "Create staged debate error: " (.getMessage e))))))

(defn handle-agora-stage-status
  "Get stage status for a staged debate.

   Returns: {:dialogue-id :current-stage :evidence-count :all-research-done?}"
  [{:keys [dialogue_id]}]
  (try
    (when-not dialogue_id
      (throw (ex-info "Missing required parameter: dialogue_id" {:type :validation})))
    (if-let [status (stages/get-stage-status dialogue_id)]
      (mcp-success status)
      (mcp-error (str "Staged debate not found: " dialogue_id)))
    (catch clojure.lang.ExceptionInfo e
      (mcp-error (str "Stage status failed: " (ex-message e))))
    (catch Exception e
      (log/error e "Stage status error")
      (mcp-error (str "Stage status error: " (.getMessage e))))))

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
    :handler handle-agora-get-history}

   ;; ============================================================
   ;; Drone Debate Tools (ADR 20260124224722-51e4fad6)
   ;; ============================================================

   {:name "agora_create_debate"
    :description "Create an auto-orchestrated drone debate. Spawns free-tier drones as participants and runs to Nash equilibrium consensus. No manual dispatch needed - fully automated per [Arch>Bh]p principle."
    :inputSchema {:type "object"
                  :properties {:topic {:type "string"
                                       :description "What the debate is about (e.g., 'Should we use Strategy vs Decorator pattern?')"}
                               :roles {:type "array"
                                       :items {:type "object"
                                               :properties {:role {:type "string"
                                                                   :description "Role name (e.g., 'advocate', 'skeptic')"}
                                                            :position {:type "string"
                                                                       :description "Position to argue (e.g., 'For Strategy pattern')"}}
                                               :required ["role" "position"]}
                                       :description "Array of debate roles (minimum 2)"}
                               :methodology {:type "string"
                                             :enum ["opinion" "fact-based" "mixed"]
                                             :description "Debate methodology: opinion (quick), fact-based (evidence required), mixed (both)"}
                               :blocking {:type "boolean"
                                          :description "If true, runs debate to completion synchronously (default: false, returns after first turn)"}}
                  :required ["topic" "roles"]}
    :handler handle-agora-create-debate}

   {:name "agora_debate_status"
    :description "Get status of a drone debate including turn count, participants, and consensus state."
    :inputSchema {:type "object"
                  :properties {:dialogue_id {:type "string"
                                             :description "ID of the debate to check"}}
                  :required ["dialogue_id"]}
    :handler handle-agora-debate-status}

   {:name "agora_continue_debate"
    :description "Continue an async drone debate by executing the next turn. Use after agora_create_debate with blocking=false to manually advance turns."
    :inputSchema {:type "object"
                  :properties {:dialogue_id {:type "string"
                                             :description "ID of the debate to continue"}}
                  :required ["dialogue_id"]}
    :handler handle-agora-continue-debate}

   {:name "agora_list_debates"
    :description "List all active drone debates with their status and participant info."
    :inputSchema {:type "object"
                  :properties {}}
    :handler handle-agora-list-debates}

   ;; ============================================================
   ;; Two-Stage Agora Tools
   ;; ============================================================

   {:name "agora_create_staged_debate"
    :description "Create a two-stage debate. Stage 1: research drones gather evidence. Stage 2: debate drones argue with evidence from Stage 1. Fully automated per [Arch>Bh]p principle."
    :inputSchema {:type "object"
                  :properties {:topic {:type "string"
                                       :description "What the debate is about"}
                               :research_roles {:type "array"
                                                :items {:type "object"
                                                        :properties {:role {:type "string"
                                                                            :description "Research role name (e.g., 'codebase-analyst')"}
                                                                     :focus_area {:type "string"
                                                                                  :description "What to investigate (e.g., 'Current dispatch patterns')"}}
                                                        :required ["role"]}
                                                :description "Array of research roles for Stage 1 (minimum 1)"}
                               :debate_roles {:type "array"
                                              :items {:type "object"
                                                      :properties {:role {:type "string"
                                                                          :description "Debate role name (e.g., 'advocate')"}
                                                                   :position {:type "string"
                                                                              :description "Position to argue (e.g., 'For Strategy pattern')"}}
                                                      :required ["role" "position"]}
                                              :description "Array of debate roles for Stage 2 (minimum 2)"}
                               :methodology {:type "string"
                                             :enum ["opinion" "fact-based" "mixed"]
                                             :description "Debate methodology (default: fact-based for staged)"}}
                  :required ["topic" "research_roles" "debate_roles"]}
    :handler handle-agora-create-staged-debate}

   {:name "agora_stage_status"
    :description "Get status of a staged debate including current stage, evidence count, and research completion."
    :inputSchema {:type "object"
                  :properties {:dialogue_id {:type "string"
                                             :description "ID of the staged debate to check"}}
                  :required ["dialogue_id"]}
    :handler handle-agora-stage-status}])
