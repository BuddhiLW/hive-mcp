(ns hive-mcp.agent.ling
  "Ling agent implementation - Claude Code instances with tool chaining.

   Lings are persistent agents that:
   - Run as Claude Code subprocesses
   - Can chain multiple tool calls
   - Maintain session context
   - Coordinate via hivemind
   - Delegate to drones for file mutations

   Supports four spawn modes via Strategy Protocol:
   - :vterm      (default for interactive) - Spawned inside Emacs vterm buffer (visual)
   - :headless   (alias for :agent-sdk)    - Auto-mapped to :agent-sdk since 0.12.0
   - :openrouter                           - Direct OpenRouter API calls (multi-model)
   - :agent-sdk  (default for headless)    - Claude Agent SDK via libpython-clj

   Architecture (SOLID Open-Closed via Strategy Pattern):
   - ILingStrategy protocol defines mode-specific ops (spawn/dispatch/status/kill)
   - VtermStrategy implements via emacsclient/elisp
   - HeadlessStrategy implements via ProcessBuilder
   - OpenRouterStrategy implements via OpenRouter HTTP API streaming
   - AgentSDKStrategy implements via libpython-clj + Claude Agent SDK
   - This file is the thin facade — delegates mode ops to strategy

   Implements IAgent protocol for unified lifecycle management."
  (:require [hive-mcp.agent.protocol :refer [IAgent]]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.vterm :as vterm]
            [hive-mcp.agent.ling.headless-strategy :as headless-strat]
            [hive-mcp.agent.ling.openrouter-strategy :as openrouter-strat]
            [hive-mcp.agent.ling.agent-sdk-strategy :as sdk-strat]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.swarm.datascript.claims :as ds-claims]
            [hive-mcp.swarm.datascript.schema :as schema]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Strategy Resolution
;;; =============================================================================

(defn- resolve-strategy
  "Get the ILingStrategy implementation for a spawn mode.

   Arguments:
     mode - :vterm, :headless, :openrouter, or :agent-sdk

   Returns:
     ILingStrategy instance"
  [mode]
  (case mode
    :headless (headless-strat/->headless-strategy)
    :openrouter (openrouter-strat/->openrouter-strategy)
    :agent-sdk (sdk-strat/->agent-sdk-strategy)
    ;; default: vterm
    (vterm/->vterm-strategy)))

(defn- ling-ctx
  "Build a context map from a Ling record for strategy calls.

   Extracts fields strategies need without coupling them to the Ling record."
  [ling]
  (cond-> {:id (:id ling)
           :cwd (:cwd ling)
           :presets (:presets ling)
           :project-id (:project-id ling)
           :spawn-mode (:spawn-mode ling)
           :model (:model ling)}
    ;; Only include agents when present (avoids nil noise in logging)
    (:agents ling) (assoc :agents (:agents ling))))

;;; =============================================================================
;;; Forward Declarations
;;; =============================================================================

(declare ->ling)

;;; =============================================================================
;;; Ling Record - IAgent Implementation (Strategy-Delegating Facade)
;;; =============================================================================

(defrecord Ling [id cwd presets project-id spawn-mode model agents]
  IAgent

  (spawn! [this opts]
    "Spawn a ling in the configured spawn-mode.

     Delegates mode-specific spawn to ILingStrategy.
     Handles common concerns: model resolution, DataScript registration, initial task.

     Hint injection flow (Tier 1 activation):
     When kanban-task-id is provided, generates KG-driven memory hints
     and prepends them to the task BEFORE passing to strategy-spawn!.
     This ensures hints are embedded in the initial CLI arg (headless)
     or elisp dispatch (vterm) — not sent as a separate stdin dispatch
     which would fail for claude -p mode."
    (let [;; Non-claude models spawn via OpenRouter API (no CLI needed)
          effective-model (or (:model opts) model)
          non-claude? (and effective-model
                           (not (schema/claude-model? effective-model)))
          ;; :headless maps to :agent-sdk (default headless mechanism since 0.12.0)
          raw-mode (if non-claude?
                     :openrouter
                     (or (:spawn-mode opts) spawn-mode :vterm))
          mode (if (= raw-mode :headless) :agent-sdk raw-mode)
          strat (resolve-strategy mode)
          ctx (assoc (ling-ctx this) :model effective-model)
          {:keys [depth parent kanban-task-id]
           :or {depth 1}} opts

          ;; Generate KG-driven hints BEFORE spawn when kanban-task-id is provided
          ;; This must happen before strategy-spawn! so hints are embedded in the
          ;; initial task (CLI arg for headless, elisp dispatch for vterm).
          task-hints-str (when (and kanban-task-id (:task opts))
                           (try
                             (let [task-hints (hints/generate-task-hints {:task-id kanban-task-id :depth 2})
                                   hint-data (hints/generate-hints (or project-id "global")
                                                                   {:task (:task opts)
                                                                    :extra-ids (:l1-ids task-hints)
                                                                    :extra-queries (:l2-queries task-hints)})
                                   kg-seeds (mapv :id (or (:l3-seeds task-hints) []))]
                               (hints/serialize-hints
                                (cond-> hint-data
                                  (seq kg-seeds) (assoc-in [:memory-hints :kg-seeds] kg-seeds)
                                  (seq kg-seeds) (assoc-in [:memory-hints :kg-depth] 2))
                                :project-name (or project-id "global")))
                             (catch Exception e
                               (log/debug "Task hint generation in spawn failed (non-fatal):" (.getMessage e))
                               nil)))

          ;; Enrich task with hints (prepend hint block before task prompt)
          enriched-task (when (:task opts)
                          (if task-hints-str
                            (str task-hints-str "\n\n---\n\n" (:task opts))
                            (:task opts)))

          ;; Pass enriched task to strategy-spawn! so hints are baked into
          ;; the initial CLI arg (headless) or elisp dispatch (vterm)
          spawn-opts (if enriched-task
                       (assoc opts :task enriched-task)
                       opts)

          ;; Delegate spawn to strategy (with hints already in task)
          slave-id (strategy/strategy-spawn! strat ctx spawn-opts)]

      ;; Common: Register in DataScript
      (ds-lings/add-slave! slave-id {:status :idle
                                     :depth depth
                                     :parent parent
                                     :presets (or (:presets opts) presets)
                                     :cwd cwd
                                     :project-id project-id
                                     :kanban-task-id kanban-task-id
                                     :requested-id (when (not= slave-id id) id)})
      ;; Store spawn mode + model
      (ds-lings/update-slave! slave-id (cond-> {:ling/spawn-mode mode
                                                :ling/model (or effective-model "claude")}
                                         ;; Headless mode: track OS process PID
                                         (and (= mode :headless)
                                              (headless/headless-status slave-id))
                                         (assoc :ling/process-pid
                                                (:pid (headless/headless-status slave-id))
                                                :ling/process-alive? true)
                                         ;; OpenRouter mode: mark as alive (HTTP-based, no PID)
                                         (= mode :openrouter)
                                         (assoc :ling/process-alive? true)
                                         ;; Agent SDK mode: mark as alive (in-process, no PID)
                                         (= mode :agent-sdk)
                                         (assoc :ling/process-alive? true)))

      ;; For vterm mode, the task is NOT embedded in strategy-spawn! (spawn only
      ;; creates the buffer). We need a separate dispatch to send the task.
      ;; For headless mode, the task is already baked into the CLI arg by
      ;; strategy-spawn!, so no separate dispatch needed.
      ;; For openrouter mode, the task is dispatched in strategy-spawn! directly.
      (when (and enriched-task (not (#{:headless :openrouter :agent-sdk} mode)))
        (let [task-ling (->ling slave-id {:cwd cwd
                                          :presets (or (:presets opts) presets)
                                          :project-id project-id
                                          :spawn-mode mode
                                          :model effective-model})]
          (.dispatch! task-ling {:task enriched-task})))
      slave-id))

  (dispatch! [this task-opts]
    "Dispatch a task to this ling.

     Common: update status, register task, claim files.
     Mode-specific: delegate to strategy.

     Accepts :task as plain string or IDispatchContext.
     Also accepts :dispatch-context for pre-resolved context (from consolidated handler).
     Uses ensure-context + resolve-context for normalization.

     L2 Phase 3: Passes dispatch-context through to strategy so L2-aware
     strategies (HeadlessStrategy) can build L2 context envelopes instead
     of losing structured refs by resolving to text prematurely.

     SOLID-D: Depends on IDispatchContext abstraction."
    (let [{:keys [task files timeout-ms dispatch-context]
           :or {timeout-ms 60000}} task-opts
          ;; IDispatchContext support: resolve context for task registration
          ctx (or dispatch-context
                  (when task (dispatch-ctx/ensure-context task)))
          resolved-task (if ctx
                          (:prompt (dispatch-ctx/resolve-context ctx))
                          task)
          task-id (str "task-" (System/currentTimeMillis) "-" (subs id 0 (min 8 (count id))))
          mode (or spawn-mode
                   (when-let [slave (ds-queries/get-slave id)]
                     (:ling/spawn-mode slave))
                   :vterm)
          strat (resolve-strategy mode)]
      ;; Common: Update status and register task
      (ds-lings/update-slave! id {:slave/status :working})
      (ds-lings/add-task! task-id id {:status :dispatched
                                      :prompt resolved-task
                                      :files files})
      (when (seq files)
        (.claim-files! this files task-id))

      ;; L2 Phase 3: Pass dispatch-context through to strategy
      ;; HeadlessStrategy uses it to build L2 envelope instead of losing refs.
      ;; VtermStrategy and others ignore it and use :task (resolved text).
      (let [resolved-opts (cond-> (assoc task-opts :task resolved-task)
                            ;; Preserve dispatch-context for L2-aware strategies
                            ctx (assoc :dispatch-context ctx))]
        (try
          (strategy/strategy-dispatch! strat (ling-ctx this) resolved-opts)
          (log/info "Task dispatched to ling" {:ling-id id :task-id task-id
                                               :mode mode :files files
                                               :context-type (when ctx
                                                               (dispatch-ctx/context-type ctx))})
          task-id
          (catch Exception e
            (log/error "Failed to dispatch to ling"
                       {:ling-id id :task-id task-id :mode mode :error (ex-message e)})
            (ds-lings/update-task! task-id {:status :failed
                                            :error (ex-message e)})
            (throw (ex-info "Failed to dispatch to ling"
                            {:ling-id id :task-id task-id :error (ex-message e)}
                            e)))))))

  (status [this]
    "Get current ling status from DataScript with mode-appropriate liveness check.

     Delegates mode-specific liveness to strategy."
    (let [ds-status (ds-queries/get-slave id)
          mode (or spawn-mode
                   (:ling/spawn-mode ds-status)
                   :vterm)
          strat (resolve-strategy mode)]
      (strategy/strategy-status strat (ling-ctx this) ds-status)))

  (kill! [this]
    "Terminate the ling and release resources.

     Common: check critical ops, release claims, remove from DataScript.
     Mode-specific: delegate kill to strategy."
    (let [{:keys [can-kill? blocking-ops]} (ds-lings/can-kill? id)
          mode (or spawn-mode
                   (when-let [slave (ds-queries/get-slave id)]
                     (:ling/spawn-mode slave))
                   :vterm)]
      (if can-kill?
        (do
          ;; Common: Release claims first
          (.release-claims! this)
          ;; Delegate kill to strategy
          (let [strat (resolve-strategy mode)
                result (strategy/strategy-kill! strat (ling-ctx this))]
            ;; Common: Remove from DataScript if kill succeeded
            (when (:killed? result)
              (ds-lings/remove-slave! id))
            result))
        (do
          (log/warn "Cannot kill ling - critical ops in progress"
                    {:id id :blocking-ops blocking-ops})
          {:killed? false
           :reason :critical-ops-blocking
           :blocking-ops blocking-ops}))))

  (agent-type [_]
    :ling)

  (can-chain-tools? [_]
    "Lings can chain multiple tool calls in a single turn."
    true)

  (claims [_this]
    "Get list of files currently claimed by this ling."
    (let [all-claims (ds-queries/get-all-claims)]
      (->> all-claims
           (filter #(= id (:slave-id %)))
           (map :file)
           vec)))

  (claim-files! [_this files task-id]
    "Claim files for exclusive access during task."
    (when (seq files)
      (doseq [f files]
        (let [{:keys [conflict? held-by]} (ds-queries/has-conflict? f id)]
          (if conflict?
            (do
              (log/warn "File already claimed by another agent"
                        {:file f :held-by held-by :requesting id})
              (ds-claims/add-to-wait-queue! id f))
            (ds-lings/claim-file! f id task-id))))
      (log/info "Files claimed" {:ling-id id :count (count files)})))

  (release-claims! [_this]
    "Release all file claims held by this ling."
    (let [released-count (ds-lings/release-claims-for-slave! id)]
      (log/info "Released claims" {:ling-id id :count released-count})
      released-count))

  (upgrade! [_]
    "No-op for lings - they already have full capabilities."
    nil))

;;; =============================================================================
;;; Factory Functions
;;; =============================================================================

(defn ->ling
  "Create a new Ling agent instance.

   Arguments:
     id   - Unique identifier for this ling
     opts - Map with optional keys:
            :cwd        - Working directory
            :presets    - Collection of preset names
            :project-id - Project ID for scoping
            :spawn-mode - :vterm (default), :headless, :openrouter, or :agent-sdk
                          NOTE: :headless is accepted but maps to :agent-sdk
                          (agent-sdk is the default headless mechanism since 0.12.0)
            :model      - Model identifier (default: 'claude' = Claude Code CLI)
                          Non-claude models automatically use :openrouter spawn-mode.
            :agents     - Subagent definitions map (optional, agent-sdk mode only)
                          Map of name -> {:description :prompt :tools :model}
                          Passed to ClaudeAgentOptions.agents for custom
                          subagent definitions in the Claude SDK session.

   Returns:
     Ling record implementing IAgent protocol"
  [id opts]
  (let [model-val (:model opts)
        ;; Non-claude models use OpenRouter API directly
        ;; :headless maps to :agent-sdk (default headless mechanism since 0.12.0)
        raw-spawn-mode (:spawn-mode opts :vterm)
        effective-spawn-mode (if (and model-val (not (schema/claude-model? model-val)))
                               :openrouter
                               (if (= raw-spawn-mode :headless)
                                 :agent-sdk
                                 raw-spawn-mode))]
    (map->Ling (cond-> {:id id
                        :cwd (:cwd opts)
                        :presets (:presets opts [])
                        :project-id (:project-id opts)
                        :spawn-mode effective-spawn-mode
                        :model model-val}
                 (:agents opts) (assoc :agents (:agents opts))))))

(defn create-ling!
  "Create and spawn a new ling agent.

   Convenience function that creates the Ling record and spawns it.

   Arguments:
     id   - Unique identifier
     opts - Spawn options (see spawn! and ->ling)

   Returns:
     The ling ID on success, throws on failure"
  [id opts]
  (let [ling (->ling id opts)]
    (.spawn! ling opts)))

;;; =============================================================================
;;; Ling Query Functions
;;; =============================================================================

(defn get-ling
  "Get a ling by ID as a Ling record.

   Reconstitutes the Ling record from DataScript state,
   including spawn-mode and model for proper dispatch routing.

   Returns:
     Ling record or nil if not found"
  [id]
  (when-let [slave (ds-queries/get-slave id)]
    (->ling id {:cwd (:slave/cwd slave)
                :presets (:slave/presets slave)
                :project-id (:slave/project-id slave)
                :spawn-mode (or (:ling/spawn-mode slave) :vterm)
                :model (:ling/model slave)})))

(defn list-lings
  "List all lings, optionally filtered by project-id.

   Arguments:
     project-id - Optional project ID filter

   Returns:
     Seq of Ling records"
  [& [project-id]]
  (let [slaves (if project-id
                 (ds-queries/get-slaves-by-project project-id)
                 (ds-queries/get-all-slaves))]
    (->> slaves
         (filter #(= 1 (:slave/depth %)))
         (map (fn [s]
                (->ling (:slave/id s)
                        {:cwd (:slave/cwd s)
                         :presets (:slave/presets s)
                         :project-id (:slave/project-id s)
                         :spawn-mode (or (:ling/spawn-mode s) :vterm)
                         :model (:ling/model s)}))))))

(defn get-ling-for-task
  "Get the ling assigned to a kanban task.

   Arguments:
     kanban-task-id - Kanban task ID

   Returns:
     Ling record or nil"
  [kanban-task-id]
  (when-let [slave (ds-queries/get-slave-by-kanban-task kanban-task-id)]
    (->ling (:slave/id slave)
            {:cwd (:slave/cwd slave)
             :presets (:slave/presets slave)
             :project-id (:slave/project-id slave)
             :spawn-mode (or (:ling/spawn-mode slave) :vterm)
             :model (:ling/model slave)})))

;;; =============================================================================
;;; Critical Operations Guard
;;; =============================================================================

(defn with-critical-op
  "Execute body while holding a critical operation guard.

   Prevents swarm_kill from terminating the ling during critical ops.
   Wraps ds-lings/with-critical-op.

   Usage:
     (with-critical-op ling-id :wrap
       (do-wrap-stuff))"
  [ling-id op-type body-fn]
  (ds-lings/with-critical-op ling-id op-type
    (body-fn)))

(comment
  ;; Usage examples

  ;; === Vterm mode (default, requires Emacs) ===
  (def my-ling (->ling "ling-001" {:cwd "/home/user/project"
                                   :presets ["coordinator"]
                                   :project-id "hive-mcp"}))

  ;; === Headless mode (no Emacs required) ===
  (def headless-ling (->ling "ling-002" {:cwd "/home/user/project"
                                         :presets ["worker"]
                                         :project-id "hive-mcp"
                                         :spawn-mode :headless}))

  ;; === Multi-model mode (OpenRouter API direct) ===
  (def deepseek-ling (->ling "ling-003" {:cwd "/home/user/project"
                                         :presets ["worker"]
                                         :project-id "hive-mcp"
                                         :model "deepseek/deepseek-chat"}))
  ;; spawn-mode will be :openrouter automatically

  ;; === Agent SDK mode (in-process via libpython-clj) ===
  (def sdk-ling (->ling "ling-004" {:cwd "/home/user/project"
                                    :presets ["worker"]
                                    :project-id "hive-mcp"
                                    :spawn-mode :agent-sdk}))

  ;; Check status (works for all modes — strategy handles it)
  ;; (.status my-ling)
  ;; (.status headless-ling)

  ;; Kill when done (mode-appropriate cleanup via strategy)
  ;; (.kill! my-ling)
  ;; (.kill! headless-ling)
  )
