(ns hive-mcp.agent.saa.orchestrator
  "SAA (Silence-Abstract-Act) orchestrator implementing ISAAOrchestrator protocol.

   Manages the three-phase exploration strategy for agent-driven work:
   1. Silence: Observe with read-only tools, collect context
   2. Abstract: Synthesize observations into a structured plan
   3. Act: Execute the plan with full tool access

   Architecture:
   - Uses IAgentSession.query! for phase execution (protocol-based, not SDK-private)
   - Tracks per-agent state (phase, observations, plan, timestamps)
   - Integrates with hivemind shouts for phase progress reporting
   - Uses requiring-resolve stubs for hive-knowledge integration (L3+)
   - References headless-sdk for SAA phase definitions and scoring (public API only)

   Philosophy:
   Korzybski's structural differential — ground in territory (Silence),
   build the map (Abstract), then navigate by the map (Act).

   SOLID-S: Single responsibility — SAA phase orchestration only.
   SOLID-D: Depends on ISAAOrchestrator/IAgentSession abstractions, not concretions.
   CLARITY-L: Pure orchestration layer, delegates execution to session query!.
   CLARITY-T: Telemetry via hivemind shouts at every phase transition.
   CLARITY-Y: Returns error maps on failure, never throws."
  (:require [clojure.core.async :as async :refer [go go-loop chan >! <! >!! <!! close! put!]]
            [clojure.string :as str]
            [hive-mcp.protocols.agent-bridge :as bridge]
            [hive-mcp.agent.headless-sdk :as sdk]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Per-Agent Phase State
;;; =============================================================================

;; Tracks SAA state per agent-id.
;; Key: agent-id (string)
;; Value: {:phase        keyword (:idle :silence :abstract :act :complete :error)
;;         :task         string  (original task description)
;;         :observations vector  (collected during silence)
;;         :plan         string  (produced during abstract)
;;         :result       map     (produced during act)
;;         :phase-history [{:phase kw :started-at ms :ended-at ms}]
;;         :started-at   long
;;         :error        string  (if :error phase)}
(defonce ^:private agent-states (atom {}))

(defn- init-agent-state!
  "Initialize SAA state for an agent."
  [agent-id task]
  (swap! agent-states assoc agent-id
         {:phase :idle
          :task task
          :observations []
          :plan nil
          :result nil
          :phase-history []
          :started-at (System/currentTimeMillis)
          :phase-started-at (System/currentTimeMillis)
          :error nil}))

(defn- transition-phase!
  "Transition an agent to a new SAA phase.
   Records the transition in phase-history."
  [agent-id new-phase]
  (swap! agent-states update agent-id
         (fn [state]
           (when state
             (let [now (System/currentTimeMillis)
                   history-entry {:phase (:phase state)
                                  :started-at (or (:phase-started-at state)
                                                  (:started-at state))
                                  :ended-at now}]
               (-> state
                   (assoc :phase new-phase
                          :phase-started-at now)
                   (update :phase-history conj history-entry)))))))

(defn- update-agent-state!
  "Update specific fields in an agent's SAA state."
  [agent-id updates]
  (swap! agent-states update agent-id merge updates))

(defn- get-agent-state
  "Get current SAA state for an agent."
  [agent-id]
  (get @agent-states agent-id))

(defn- clear-agent-state!
  "Remove SAA state for an agent. For cleanup."
  [agent-id]
  (swap! agent-states dissoc agent-id))

;;; =============================================================================
;;; Hivemind Integration
;;; =============================================================================

(defn- shout-phase!
  "Broadcast phase transition to hivemind.
   Uses requiring-resolve to avoid circular dependency."
  [agent-id phase message]
  (try
    (when-let [shout-fn (requiring-resolve 'hive-mcp.hivemind/shout!)]
      (shout-fn agent-id
                :progress
                {:task (:task (get-agent-state agent-id))
                 :message (str "[SAA:" (name phase) "] " message)
                 :saa-phase phase}))
    (catch Exception e
      (log/debug "[saa] Hivemind shout failed (non-critical)" {:error (ex-message e)}))))

(defn- maybe-shout!
  "Broadcast phase transition only if :shout? is enabled in config.
   Respects the SAAOrchestrator configuration."
  [config agent-id phase message]
  (when (:shout? config)
    (shout-phase! agent-id phase message)))

;;; =============================================================================
;;; Requiring-Resolve Stubs (hive-knowledge L3+ integration)
;;; =============================================================================

(defn- score-observations-l3
  "Score observations using hive-knowledge L3+ scoring.
   Falls back to headless-sdk heuristic scoring if unavailable.

   L3+ provides: structural similarity, GNN scoring, emergence detection.
   L1/L2 provides: keyword-based heuristic scoring."
  [observations]
  (if-let [score-fn (try (requiring-resolve 'hive-knowledge.scoring/score-observations)
                         (catch Exception _ nil))]
    (score-fn observations)
    (sdk/score-observations observations)))

(defn- plan-from-observations-l3
  "Generate a plan using hive-knowledge L3+ planning.
   Falls back to nil (let Abstract phase LLM do planning).

   L3+ provides: graph-aware planning, priority weighting by KG centrality.
   L1/L2 provides: raw observations forwarded to LLM for synthesis."
  [observations task]
  (if-let [plan-fn (try (requiring-resolve 'hive-knowledge.planning/generate-plan)
                        (catch Exception _ nil))]
    (plan-fn observations task)
    nil))

(defn- enrich-silence-context-l3
  "Enrich Silence phase with hive-knowledge context.
   Falls back to nil (no enrichment).

   L3+ provides: relevant KG subgraph, cross-project pattern matches.
   L1/L2 provides: nothing (the Silence phase explores on its own)."
  [task]
  (if-let [enrich-fn (try (requiring-resolve 'hive-knowledge.context/enrich-task-context)
                          (catch Exception _ nil))]
    (enrich-fn task)
    nil))

;;; =============================================================================
;;; Phase Execution via IAgentSession.query!
;;; =============================================================================

(defn- build-phase-prompt
  "Build the full prompt for a given SAA phase."
  [phase task-or-content extra-context]
  (let [phase-config (get sdk/saa-phases phase)
        suffix (:system-prompt-suffix phase-config)]
    (case phase
      :silence
      (str "TASK: " task-or-content
           "\n\nExplore the codebase and collect context. "
           "List all relevant files, patterns, and observations."
           (when extra-context
             (str "\n\nPrior knowledge context:\n" (pr-str extra-context))))

      :abstract
      (str "Based on these observations from the Silence phase:\n"
           (pr-str task-or-content)
           "\n\nSynthesize these into a concrete action plan."
           (when extra-context
             (str "\n\nOriginal task: " extra-context))
           "\n\nProduce a structured plan with specific steps. "
           "Each step should name the file, the change, and the rationale.")

      :act
      (str "Execute the following plan:\n" (or task-or-content "Use best judgment.")
           (when extra-context
             (str "\n\nOriginal task: " extra-context))
           "\n\nFollow the plan precisely. Make changes file by file. "
           "Verify each change before moving to the next."))))

(defn- build-phase-opts
  "Build query options for a phase."
  [phase user-opts]
  (let [phase-config (get sdk/saa-phases phase)]
    (cond-> {:allowed-tools (:allowed-tools phase-config)
             :permission-mode (keyword (:permission-mode phase-config))}
      (:system-prompt user-opts)
      (assoc :system-prompt (str (:system-prompt user-opts)
                                 "\n\n" (:system-prompt-suffix phase-config)))

      (nil? (:system-prompt user-opts))
      (assoc :system-prompt (:system-prompt-suffix phase-config))

      (:max-turns user-opts)
      (assoc :max-turns (:max-turns user-opts)))))

(defn- execute-phase-via-session!
  "Execute a SAA phase by querying the agent session.
   Returns a channel that yields phase messages then closes.

   This is the core execution path — delegates to IAgentSession.query!
   which is backend-agnostic (works with ClaudeSDKBackend, Noop, etc.)."
  [session prompt phase-opts]
  (bridge/query! session prompt phase-opts))

(defn- collect-phase-messages!
  "Drain a phase channel, collecting messages and forwarding to output channel.
   Returns a core.async channel that yields the collected messages vector.
   Uses parking ops (<!, >!) — safe to call from go blocks without thread starvation."
  [phase-ch out-ch saa-phase]
  (go-loop [messages []]
    (if-let [msg (<! phase-ch)]
      (do
        (when out-ch
          (>! out-ch (assoc msg :saa-phase saa-phase)))
        (recur (conj messages msg)))
      messages)))

(defn- extract-content
  "Extract textual content from phase messages."
  [messages]
  (->> messages
       (filter #(contains? #{:message :complete :result} (:type %)))
       (mapv #(or (:content %) (:data %) (str %)))))

;;; =============================================================================
;;; ISAAOrchestrator Implementation
;;; =============================================================================

(defrecord SAAOrchestrator [config]
  bridge/ISAAOrchestrator

  (run-silence! [_ session task opts]
    (let [agent-id (bridge/session-id session)
          out-ch (chan 1024)]
      (init-agent-state! agent-id task)
      (transition-phase! agent-id :silence)
      (maybe-shout! config agent-id :silence "Starting observation phase")
      (go
        (try
          (let [;; L3+ enrichment (nil fallback)
                enrichment (enrich-silence-context-l3 task)
                ;; Build prompt and opts
                prompt (build-phase-prompt :silence task enrichment)
                phase-opts (build-phase-opts :silence opts)
                ;; Execute via session query
                phase-ch (execute-phase-via-session! session prompt phase-opts)
                ;; Collect all messages (parking ops — safe in go block)
                messages (<! (collect-phase-messages! phase-ch out-ch :silence))
                observations (extract-content messages)]
            ;; Store observations in agent state
            (update-agent-state! agent-id {:observations observations})
            (maybe-shout! config agent-id :silence
                          (str "Completed. Collected " (count observations) " observations"))
            ;; Final phase-complete message
            (>! out-ch {:type :phase-complete
                        :saa-phase :silence
                        :observations observations
                        :observation-count (count observations)}))
          (catch Exception e
            (log/error "[saa] Silence phase failed" {:agent-id agent-id :error (ex-message e)})
            (transition-phase! agent-id :error)
            (update-agent-state! agent-id {:error (ex-message e)})
            (maybe-shout! config agent-id :silence (str "FAILED: " (ex-message e)))
            (>! out-ch {:type :error :saa-phase :silence :error (ex-message e)}))
          (finally
            (close! out-ch))))
      out-ch))

  (run-abstract! [_ session observations opts]
    (let [agent-id (bridge/session-id session)
          out-ch (chan 1024)]
      (transition-phase! agent-id :abstract)
      (maybe-shout! config agent-id :abstract
                    (str "Starting synthesis with " (count observations) " observations"))
      (go
        (try
          (let [;; L3+ scoring (heuristic fallback)
                scored (score-observations-l3 observations)
                ;; L3+ pre-planning (nil fallback)
                task (:task (get-agent-state agent-id))
                l3-plan (plan-from-observations-l3 scored task)
                ;; Build prompt — pass scored observations + task as context
                prompt (build-phase-prompt :abstract scored task)
                phase-opts (build-phase-opts :abstract opts)
                ;; Execute via session query
                phase-ch (execute-phase-via-session! session prompt phase-opts)
                ;; Collect all messages (parking ops — safe in go block)
                messages (<! (collect-phase-messages! phase-ch out-ch :abstract))
                plan-content (extract-content messages)
                ;; Prefer L3+ plan if available, else use LLM-generated plan
                final-plan (or l3-plan (str/join "\n" plan-content))]
            ;; Store plan in agent state
            (update-agent-state! agent-id {:plan final-plan})
            (maybe-shout! config agent-id :abstract "Completed. Plan ready for execution.")
            ;; Final phase-complete message
            (>! out-ch {:type :phase-complete
                        :saa-phase :abstract
                        :plan final-plan}))
          (catch Exception e
            (log/error "[saa] Abstract phase failed" {:agent-id agent-id :error (ex-message e)})
            (transition-phase! agent-id :error)
            (update-agent-state! agent-id {:error (ex-message e)})
            (maybe-shout! config agent-id :abstract (str "FAILED: " (ex-message e)))
            (>! out-ch {:type :error :saa-phase :abstract :error (ex-message e)}))
          (finally
            (close! out-ch))))
      out-ch))

  (run-act! [_ session plan opts]
    (let [agent-id (bridge/session-id session)
          out-ch (chan 1024)]
      (transition-phase! agent-id :act)
      (maybe-shout! config agent-id :act "Starting execution phase")
      (go
        (try
          (let [task (:task (get-agent-state agent-id))
                ;; Build prompt — pass plan + task as context
                prompt (build-phase-prompt :act plan task)
                phase-opts (build-phase-opts :act opts)
                ;; Execute via session query
                phase-ch (execute-phase-via-session! session prompt phase-opts)
                ;; Collect all messages (parking ops — safe in go block)
                messages (<! (collect-phase-messages! phase-ch out-ch :act))
                result-content (extract-content messages)]
            ;; Store result in agent state
            (update-agent-state! agent-id {:result {:messages result-content
                                                    :message-count (count messages)}})
            (transition-phase! agent-id :complete)
            (maybe-shout! config agent-id :act
                          (str "Completed. " (count messages) " messages processed."))
            ;; Final phase-complete message
            (>! out-ch {:type :phase-complete
                        :saa-phase :act
                        :result {:messages result-content
                                 :message-count (count messages)}}))
          (catch Exception e
            (log/error "[saa] Act phase failed" {:agent-id agent-id :error (ex-message e)})
            (transition-phase! agent-id :error)
            (update-agent-state! agent-id {:error (ex-message e)})
            (maybe-shout! config agent-id :act (str "FAILED: " (ex-message e)))
            (>! out-ch {:type :error :saa-phase :act :error (ex-message e)}))
          (finally
            (close! out-ch))))
      out-ch))

  (run-full-saa! [this session task opts]
    (let [agent-id (bridge/session-id session)
          out-ch (chan 4096)
          {:keys [skip-silence? skip-abstract? phase-opts]} opts
          silence-opts (get phase-opts :silence {})
          abstract-opts (get phase-opts :abstract {})
          act-opts (get phase-opts :act {})]
      (init-agent-state! agent-id task)
      (maybe-shout! config agent-id :silence
                    (str "Starting full SAA cycle"
                         (when skip-silence? " (skipping Silence)")
                         (when skip-abstract? " (skipping Abstract)")))
      (go
        (try
          ;; === SILENCE PHASE ===
          (let [observations
                (if-not skip-silence?
                  (let [silence-ch (bridge/run-silence! this session task silence-opts)]
                    ;; Forward messages to out-ch, collect internally
                    (loop []
                      (when-let [msg (<! silence-ch)]
                        (>! out-ch msg)
                        (recur)))
                    ;; Get observations from state (populated by run-silence!)
                    (:observations (get-agent-state agent-id)))
                  ;; Skip silence: empty observations
                  [])]

            ;; === ABSTRACT PHASE ===
            (let [plan
                  (if-not skip-abstract?
                    (let [abstract-ch (bridge/run-abstract! this session observations abstract-opts)]
                      (loop []
                        (when-let [msg (<! abstract-ch)]
                          (>! out-ch msg)
                          (recur)))
                      (:plan (get-agent-state agent-id)))
                    ;; Skip abstract: no plan
                    nil)]

              ;; === ACT PHASE ===
              (let [act-ch (bridge/run-act! this session (or plan task) act-opts)]
                (loop []
                  (when-let [msg (<! act-ch)]
                    (>! out-ch msg)
                    (recur)))

                ;; SAA cycle complete
                (let [final-state (get-agent-state agent-id)]
                  (maybe-shout! config agent-id :complete
                                (str "SAA cycle complete. "
                                     (count (:observations final-state)) " observations, "
                                     (count (:phase-history final-state)) " phases"))
                  (>! out-ch {:type :saa-complete
                              :agent-id agent-id
                              :observations-count (count (:observations final-state))
                              :plan (:plan final-state)
                              :result (:result final-state)
                              :phase-history (:phase-history final-state)
                              :elapsed-ms (- (System/currentTimeMillis)
                                             (:started-at final-state))})))))
          (catch Exception e
            (log/error "[saa] Full SAA cycle failed"
                       {:agent-id agent-id :error (ex-message e)})
            (transition-phase! agent-id :error)
            (update-agent-state! agent-id {:error (ex-message e)})
            (maybe-shout! config agent-id :error (str "SAA cycle FAILED: " (ex-message e)))
            (>! out-ch {:type :error :error (ex-message e)}))
          (finally
            (close! out-ch))))
      out-ch)))

;;; =============================================================================
;;; Factory & Public API
;;; =============================================================================

(defn ->saa-orchestrator
  "Create an SAAOrchestrator instance.

   Arguments:
     config - Optional configuration map:
              :shout?             - Enable hivemind shouts (default: true)
              :score-threshold    - Min score for observation inclusion (default: 0.0)
              :max-silence-turns  - Max turns in Silence phase (default: 50)
              :max-abstract-turns - Max turns in Abstract phase (default: 20)
              :max-act-turns      - Max turns in Act phase (default: 100)

   Returns:
     SAAOrchestrator implementing ISAAOrchestrator protocol."
  ([] (->saa-orchestrator {}))
  ([config]
   (->SAAOrchestrator (merge {:shout? true
                              :score-threshold 0.0
                              :max-silence-turns 50
                              :max-abstract-turns 20
                              :max-act-turns 100}
                             config))))

;;; =============================================================================
;;; State Inspection (Read-Only)
;;; =============================================================================

(defn agent-saa-state
  "Get the current SAA state for an agent. Read-only inspection.

   Returns:
     Map with :phase :task :observations :plan :result :phase-history :error
     or nil if agent has no SAA state."
  [agent-id]
  (get-agent-state agent-id))

(defn agent-saa-phase
  "Get just the current SAA phase for an agent.

   Returns:
     Keyword (:idle :silence :abstract :act :complete :error)
     or nil if agent has no SAA state."
  [agent-id]
  (:phase (get-agent-state agent-id)))

(defn list-active-saa
  "List all agents currently in SAA phases (not :idle or :complete).

   Returns:
     Vector of {:agent-id :phase :task :started-at :elapsed-ms} maps."
  []
  (->> @agent-states
       (filter (fn [[_ state]]
                 (#{:silence :abstract :act} (:phase state))))
       (mapv (fn [[agent-id state]]
               {:agent-id agent-id
                :phase (:phase state)
                :task (:task state)
                :started-at (:started-at state)
                :elapsed-ms (- (System/currentTimeMillis) (:started-at state))}))))

(defn clear-completed-states!
  "Remove SAA state for all completed or errored agents. Cleanup."
  []
  (let [to-clear (->> @agent-states
                      (filter (fn [[_ state]]
                                (#{:complete :error} (:phase state))))
                      (map first))]
    (doseq [agent-id to-clear]
      (clear-agent-state! agent-id))
    {:cleared (count to-clear)}))

(defn clear-all-states!
  "Remove all SAA states. For testing."
  []
  (reset! agent-states {})
  nil)

(comment
  ;; Usage examples

  ;; Create orchestrator
  ;; (def orch (->saa-orchestrator))

  ;; Run individual phases via IAgentSession
  ;; (let [session (bridge/->noop-session "test-1")
  ;;       ch (bridge/run-silence! orch session "Fix auth bug" {})]
  ;;   (go-loop []
  ;;     (when-let [msg (<! ch)]
  ;;       (println "SAA:" (:type msg) (:saa-phase msg))
  ;;       (recur))))

  ;; Run full SAA cycle
  ;; (let [session (bridge/->noop-session "test-2")
  ;;       ch (bridge/run-full-saa! orch session "Fix auth bug" {})]
  ;;   (go-loop []
  ;;     (when-let [msg (<! ch)]
  ;;       (println "SAA:" (:type msg) (:saa-phase msg))
  ;;       (recur))))

  ;; Check state
  ;; (agent-saa-state "test-1")
  ;; (agent-saa-phase "test-1")
  ;; (list-active-saa)

  ;; Cleanup
  ;; (clear-completed-states!)
  ;; (clear-all-states!)
  )
