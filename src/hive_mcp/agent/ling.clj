(ns hive-mcp.agent.ling
  "Ling agent implementation - Claude Code instances with tool chaining and multi-mode spawn."
  (:require [hive-mcp.agent.protocol :refer [IAgent]]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.terminal-registry :as terminal-reg]
            [hive-mcp.agent.ling.headless-strategy :as headless-strat]
            [hive-mcp.agent.ling.openrouter-strategy :as openrouter-strat]
            [hive-mcp.agent.ling.agent-sdk-strategy :as sdk-strat]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.workflows.catchup-ling :as catchup-ling]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.swarm.datascript.schema :as schema]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn resolve-effective-mode
  "Pure function: raw spawn inputs -> effective spawn mode keyword.
   Handles OpenRouter model detection and Agent SDK availability.
   Public: also used by spawn.clj for mode queries."
  [{:keys [model spawn-mode]}]
  (let [non-claude? (and model (not (schema/claude-model? model)))
        raw-mode (if non-claude?
                   :openrouter
                   (or spawn-mode :vterm))]
    (if (= raw-mode :headless)
      (if (sdk-strat/sdk-available?)
        :agent-sdk
        (do (log/warn "Agent SDK unavailable, falling back to raw headless"
                      {:sdk-status (sdk-strat/sdk-status)})
            :headless))
      raw-mode)))

(defn- resolve-strategy
  "Get the ILingStrategy implementation for a spawn mode.
   Checks terminal registry first (addon-contributed backends),
   then falls back to built-in strategies."
  [mode]
  (or (terminal-reg/resolve-terminal-strategy mode)
      (case mode
        :headless (headless-strat/->headless-strategy)
        :openrouter (openrouter-strat/->openrouter-strategy)
        :agent-sdk (sdk-strat/->agent-sdk-strategy)
        (throw (ex-info (str "No strategy registered for mode: " mode)
                        {:mode mode
                         :registered (terminal-reg/registered-terminals)})))))

(defn- ling-ctx
  "Build a context map from a Ling record for strategy calls."
  [ling]
  (cond-> {:id (:id ling)
           :cwd (:cwd ling)
           :presets (:presets ling)
           :project-id (:project-id ling)
           :spawn-mode (:spawn-mode ling)
           :model (:model ling)}
    (:agents ling) (assoc :agents (:agents ling))))

(defn- slave->ling-opts
  "Extract ling construction opts from a DataScript slave entity."
  [slave]
  {:cwd (:slave/cwd slave)
   :presets (:slave/presets slave)
   :project-id (:slave/project-id slave)
   :spawn-mode (or (:ling/spawn-mode slave) :vterm)
   :model (:ling/model slave)})

(declare ->ling)

(defn- compute-spawn-plan
  "Pure computation: derive all spawn decisions from ling state and opts.
   Returns a plan map with resolved mode, model, strategy, and context."
  [ling opts]
  (let [effective-model (or (:model opts) (:model ling))
        mode (resolve-effective-mode {:model effective-model
                                      :spawn-mode (or (:spawn-mode opts) (:spawn-mode ling))})
        {:keys [depth parent kanban-task-id]
         :or {depth 1}} opts]
    {:effective-model effective-model
     :mode mode
     :strat (resolve-strategy mode)
     :ctx (assoc (ling-ctx ling) :model effective-model)
     :depth depth
     :parent parent
     :kanban-task-id kanban-task-id
     :presets (or (:presets opts) (:presets ling))
     :cwd (:cwd ling)
     :project-id (:project-id ling)
     :ling-id (:id ling)
     :max-budget-usd (or (:max-budget-usd opts) (:max-budget-usd ling))
     :task (:task opts)}))

(defn- execute-spawn-plan!
  "Execute spawn effects: catchup enrichment, strategy spawn, DS writes,
   budget registration, and vterm dispatch."
  [plan opts]
  (let [{:keys [mode strat ctx cwd presets project-id ling-id
                effective-model depth parent kanban-task-id
                max-budget-usd task]} plan
        ling-context-str (when task
                           (rescue nil
                                   (catchup-ling/ling-catchup
                                    {:directory cwd
                                     :task task
                                     :kanban-task-id kanban-task-id})))
        enriched-task (when task
                        (if ling-context-str
                          (str ling-context-str "\n\n---\n\n" task)
                          task))
        spawn-opts (if enriched-task
                     (assoc opts :task enriched-task)
                     opts)
        slave-id (strategy/strategy-spawn! strat ctx spawn-opts)]

    ;; Set initial status based on whether a task will be dispatched.
    ;; When a task is provided, set :working immediately to prevent
    ;; the Emacs sync :slave-ready event from resetting to :idle
    ;; before the dispatch! call updates status (race condition fix).
    (ds-lings/add-slave! slave-id {:status (if enriched-task :working :idle)
                                   :depth depth
                                   :parent parent
                                   :presets presets
                                   :cwd cwd
                                   :project-id project-id
                                   :kanban-task-id kanban-task-id
                                   :requested-id (when (not= slave-id ling-id) ling-id)})
    (ds-lings/update-slave! slave-id (cond-> {:ling/spawn-mode mode
                                              :ling/model (or effective-model "claude")}
                                       (and (= mode :headless)
                                            (headless/headless-status slave-id))
                                       (assoc :ling/process-pid
                                              (:pid (headless/headless-status slave-id))
                                              :ling/process-alive? true)
                                       (= mode :openrouter)
                                       (assoc :ling/process-alive? true)
                                       (= mode :agent-sdk)
                                       (assoc :ling/process-alive? true)))

    (when (and max-budget-usd (pos? max-budget-usd))
      (rescue nil
              (when-let [register-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/register-budget!)]
                (register-fn slave-id max-budget-usd {:model (or effective-model "claude")})
                (log/info "Budget guardrail registered for ling"
                          {:ling-id slave-id :max-budget-usd max-budget-usd}))))

    (when (and enriched-task (not (#{:headless :openrouter :agent-sdk} mode)))
      (let [task-ling (->ling slave-id {:cwd cwd
                                        :presets presets
                                        :project-id project-id
                                        :spawn-mode mode
                                        :model effective-model})]
        (.dispatch! task-ling {:task enriched-task})))

    slave-id))

(defrecord Ling [id cwd presets project-id spawn-mode model agents max-budget-usd]
  IAgent

  (spawn! [this opts]
    (let [plan (compute-spawn-plan this opts)]
      (execute-spawn-plan! plan opts)))

  (dispatch! [this task-opts]
    (let [{:keys [task files _timeout-ms dispatch-context]} task-opts
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
      (ds-lings/update-slave! id {:slave/status :working})
      (ds-lings/add-task! task-id id {:status :dispatched
                                      :prompt resolved-task
                                      :files files})
      (when (seq files)
        (.claim-files! this files task-id))

      (let [resolved-opts (cond-> (assoc task-opts :task resolved-task)
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
    (let [ds-status (ds-queries/get-slave id)
          mode (or spawn-mode
                   (:ling/spawn-mode ds-status)
                   :vterm)
          strat (resolve-strategy mode)]
      (strategy/strategy-status strat (ling-ctx this) ds-status)))

  (kill! [this]
    (let [{:keys [can-kill? blocking-ops]} (ds-lings/can-kill? id)
          mode (or spawn-mode
                   (when-let [slave (ds-queries/get-slave id)]
                     (:ling/spawn-mode slave))
                   :vterm)]
      (if can-kill?
        (do
          (.release-claims! this)
          (rescue nil
                  (when-let [deregister-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/deregister-budget!)]
                    (deregister-fn id)))
          (let [strat (resolve-strategy mode)
                result (strategy/strategy-kill! strat (ling-ctx this))]
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
    true)

  (claims [_this]
    (let [all-claims (ds-queries/get-all-claims)]
      (->> all-claims
           (filter #(= id (:slave-id %)))
           (map :file)
           vec)))

  (claim-files! [_this files task-id]
    (when (seq files)
      (doseq [f files]
        (let [{:keys [conflict? held-by]} (ds-queries/has-conflict? f id)]
          (if conflict?
            (do
              (log/warn "File already claimed by another agent"
                        {:file f :held-by held-by :requesting id})
              (ds-lings/add-to-wait-queue! id f))
            (ds-lings/claim-file! f id task-id))))
      (log/info "Files claimed" {:ling-id id :count (count files)})))

  (release-claims! [_this]
    (let [released-count (ds-lings/release-claims-for-slave! id)]
      (log/info "Released claims" {:ling-id id :count released-count})
      released-count))

  (upgrade! [_]
    nil))

(defn ->ling
  "Create a new Ling agent instance."
  [id opts]
  (let [model-val (:model opts)
        effective-spawn-mode (resolve-effective-mode {:model model-val
                                                      :spawn-mode (:spawn-mode opts :vterm)})]
    (map->Ling (cond-> {:id id
                        :cwd (:cwd opts)
                        :presets (:presets opts [])
                        :project-id (:project-id opts)
                        :spawn-mode effective-spawn-mode
                        :model model-val}
                 (:agents opts)         (assoc :agents (:agents opts))
                 (:max-budget-usd opts) (assoc :max-budget-usd (:max-budget-usd opts))))))

(defn create-ling!
  "Create and spawn a new ling agent."
  [id opts]
  (let [ling (->ling id opts)]
    (.spawn! ling opts)))

(defn get-ling
  "Get a ling by ID as a Ling record from DataScript."
  [id]
  (when-let [slave (ds-queries/get-slave id)]
    (->ling id (slave->ling-opts slave))))

(defn list-lings
  "List all lings, optionally filtered by project-id."
  [& [project-id]]
  (let [slaves (if project-id
                 (ds-queries/get-slaves-by-project project-id)
                 (ds-queries/get-all-slaves))]
    (->> slaves
         (filter #(= 1 (:slave/depth %)))
         (map #(->ling (:slave/id %) (slave->ling-opts %))))))

(defn get-ling-for-task
  "Get the ling assigned to a kanban task."
  [kanban-task-id]
  (when-let [slave (ds-queries/get-slave-by-kanban-task kanban-task-id)]
    (->ling (:slave/id slave) (slave->ling-opts slave))))

(defn interrupt-ling!
  "Interrupt the current query/task of a running ling."
  [ling-id]
  (if-let [ling (get-ling ling-id)]
    (let [mode (or (:spawn-mode ling)
                   (when-let [slave (ds-queries/get-slave ling-id)]
                     (:ling/spawn-mode slave))
                   :vterm)
          strat (resolve-strategy mode)]
      (strategy/strategy-interrupt! strat (ling-ctx ling)))
    {:success? false
     :ling-id ling-id
     :errors [(str "Ling not found: " ling-id)]}))

(defn with-critical-op
  "Execute body while holding a critical operation guard."
  [ling-id op-type body-fn]
  (ds-lings/with-critical-op ling-id op-type
    (body-fn)))
