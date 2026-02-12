(ns hive-mcp.agent.ling.headless-strategy
  "Headless spawn strategy using ProcessBuilder-based ling lifecycle."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.context-envelope :as envelope]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- enrich-task-with-l2
  "Enrich a task string with context envelope when dispatch-context is available."
  [task dispatch-context l2-opts]
  (if-let [l2-envelope (envelope/envelope-from-dispatch-context dispatch-context l2-opts)]
    (do
      (log/info "context envelope built for headless dispatch"
                {:envelope-chars (count l2-envelope)
                 :task-chars (count (or task ""))})
      (str l2-envelope "\n\n---\n\n" task))
    task))

(defn- build-spawn-l2-system-prompt
  "Build context envelope string for headless spawn system prompt."
  [cwd opts]
  (let [{:keys [ctx-refs kg-node-ids scope]} opts]
    (if (or (seq ctx-refs) (seq kg-node-ids))
      (let [l2-envelope (envelope/enrich-context ctx-refs kg-node-ids scope {})]
        (when l2-envelope
          (log/info "context spawn system-prompt built from explicit refs"
                    {:categories (count ctx-refs) :kg-nodes (count kg-node-ids)})
          l2-envelope))
      (let [l2-envelope (envelope/build-spawn-envelope cwd {})]
        (if l2-envelope
          (do
            (log/info "context spawn system-prompt built from context-store lookup" {:cwd cwd})
            l2-envelope)
          (do
            (log/debug "No context available for headless spawn system-prompt")
            nil))))))

(defrecord HeadlessStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets model]} ling-ctx
          {:keys [task buffer-capacity env-extra]} opts
          l2-system-prompt (build-spawn-l2-system-prompt cwd opts)
          result (headless/spawn-headless! id (cond-> {:cwd cwd
                                                       :task task
                                                       :presets (or (:presets opts) presets)
                                                       :model model
                                                       :buffer-capacity (or buffer-capacity 5000)}
                                                l2-system-prompt (assoc :system-prompt l2-system-prompt)
                                                env-extra (assoc :env-extra env-extra)))]
      (log/info "Ling spawned headless" {:id id :pid (:pid result) :cwd cwd
                                         :model (or model "claude")
                                         :l2-system-prompt? (some? l2-system-prompt)})
      id))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task dispatch-context]} task-opts
          enriched-task (enrich-task-with-l2 task dispatch-context {})]
      (headless/dispatch-via-stdin! id enriched-task)
      (log/info "Task dispatched to headless ling via stdin" {:ling-id id
                                                              :l2-enriched (not= task enriched-task)})
      true))

  (strategy-status [_ ling-ctx ds-status]
    (let [{:keys [id]} ling-ctx
          headless-info (headless/headless-status id)]
      (if ds-status
        (cond-> ds-status
          headless-info (assoc :headless-alive? (:alive? headless-info)
                               :headless-pid (:pid headless-info)
                               :headless-uptime-ms (:uptime-ms headless-info)
                               :headless-stdout (:stdout headless-info)
                               :headless-stderr (:stderr headless-info)))
        (when headless-info
          {:slave/id id
           :slave/status (if (:alive? headless-info) :idle :dead)
           :ling/spawn-mode :headless
           :headless-alive? (:alive? headless-info)
           :headless-pid (:pid headless-info)}))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (try
        (let [result (headless/kill-headless! id)]
          (log/info "Headless ling killed" {:id id :pid (:pid result)})
          {:killed? true :id id :pid (:pid result)})
        (catch Exception e
          (log/warn "Headless kill exception" {:id id :error (ex-message e)})
          {:killed? true :id id :reason :process-already-dead}))))

  (strategy-interrupt! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      {:success? false
       :ling-id id
       :errors ["Interrupt not supported for headless (ProcessBuilder) spawn mode"]})))

(defn ->headless-strategy
  "Create a HeadlessStrategy instance."
  []
  (->HeadlessStrategy))
