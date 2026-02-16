(ns hive-mcp.tools.swarm.lifecycle
  "Swarm lifecycle handlers - spawn and kill operations with ownership and kill guards."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hivemind.core :as hivemind]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.agent.context :as ctx]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.telemetry.prometheus :as prom]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-swarm-spawn
  "DEPRECATED: Spawn a new Claude slave instance with context injection.
   Compat shims redirect swarm_spawn → agent spawn → ling.clj → terminal-registry.
   This handler is dead code kept for backward compatibility. Prefer `agent spawn`."
  [{:keys [name presets cwd _role terminal kanban_task_id]}]
  (core/with-swarm
    (let [effective-cwd (or cwd (ctx/current-directory))
          validated-cwd (when (and effective-cwd (string? effective-cwd) (not (str/blank? effective-cwd)))
                          effective-cwd)
          project-id (when validated-cwd (scope/get-current-project-id validated-cwd))]
      (try
        (let [slave-id (ling/create-ling! (or name "slave")
                                          {:cwd validated-cwd
                                           :presets (or presets [])
                                           :project-id project-id
                                           :spawn-mode (keyword (or terminal "claude"))
                                           :kanban-task-id kanban_task_id})]
          (let [current-count (count (registry/get-available-lings))]
            (prom/set-lings-active! (inc current-count)))
          (core/mcp-success {:slave_id slave-id
                             :status "spawned"
                             :cwd validated-cwd
                             :project-id project-id}))
        (catch Exception e
          (core/mcp-error (str "Error: " (ex-message e))))))))

(defn- format-blocking-ops
  "Format blocking operations for error message."
  [ops]
  (str/join ", " (map clojure.core/name ops)))

(defn- can-kill-ownership?
  "Check if caller can kill target ling based on project ownership."
  ([caller-project-id target-slave-id]
   (can-kill-ownership? caller-project-id target-slave-id false))
  ([caller-project-id target-slave-id force-cross-project?]
   (let [target-slave (ds/get-slave target-slave-id)
         target-project-id (:slave/project-id target-slave)]
     (cond
       force-cross-project?
       {:can-kill? true :reason "force-cross-project"}

       (nil? caller-project-id)
       {:can-kill? true :reason "coordinator-context"}

       (nil? target-project-id)
       {:can-kill? true :reason "legacy-ling"}

       (= caller-project-id target-project-id)
       {:can-kill? true :reason "same-project"}

       :else
       {:can-kill? false
        :reason "cross-project"
        :caller-project caller-project-id
        :target-project target-project-id}))))

(defn- check-any-critical-ops
  "Check if any slaves have critical operations blocking kill-all."
  ([]
   (check-any-critical-ops nil))
  ([slave-ids]
   (let [ids-to-check (or slave-ids (keys (registry/get-available-lings)))
         blocked (->> ids-to-check
                      (map (fn [slave-id]
                             (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
                               (when-not can-kill?
                                 {:id slave-id :ops blocking-ops}))))
                      (filter some?))]
     (if (seq blocked)
       {:can-kill? false :blocked-slaves blocked}
       {:can-kill? true :blocked-slaves []}))))

(defn- kill-single-slave!
  "Kill a single slave by ID with ownership and critical ops checks.
   Delegates to ling.clj protocol for terminal-agnostic kill."
  ([slave_id]
   (kill-single-slave! slave_id nil false))
  ([slave_id caller-project-id]
   (kill-single-slave! slave_id caller-project-id false))
  ([slave_id caller-project-id force-cross-project?]
   (let [{:keys [can-kill? reason caller-project target-project]} (can-kill-ownership? caller-project-id slave_id force-cross-project?)]
     (if-not can-kill?
       {:success false
        :error (format "cross-project kill denied: caller=%s target=%s" caller-project target-project)
        :slave-id slave_id
        :reason reason}
       (let [{crit-can-kill? :can-kill? :keys [blocking-ops]} (ds/can-kill? slave_id)]
         (if crit-can-kill?
           (if-let [ling-record (ling/get-ling slave_id)]
             (let [result (proto/kill! ling-record)]
               (if (:killed? result)
                 (do
                   (hivemind/clear-agent! slave_id)
                   {:success true :result result :slave-id slave_id})
                 (do
                   (log/warn "Kill blocked by ling protocol" {:slave-id slave_id
                                                              :reason (:reason result)
                                                              :blocking-ops (:blocking-ops result)})
                   {:success false
                    :error (if (= :critical-ops-blocking (:reason result))
                             (format "critical ops: %s" (format-blocking-ops (:blocking-ops result)))
                             "kill-blocked-safety")
                    :slave-id slave_id
                    :reason (:reason result)})))
             {:success false :error "agent not found" :slave-id slave_id})
           {:success false
            :error (format "critical ops: %s" (format-blocking-ops blocking-ops))
            :slave-id slave_id}))))))

(defn handle-swarm-kill
  "DEPRECATED: Kill a slave or all slaves with kill guard and ownership checks.
   Compat shims redirect swarm_kill → agent kill → ling.clj → terminal-registry.
   This handler is dead code kept for backward compatibility. Prefer `agent kill`."
  [{:keys [slave_id directory force_cross_project]}]
  (core/with-swarm
    (let [effective-dir (or directory (ctx/current-directory))
          caller-project-id (when effective-dir (scope/get-current-project-id effective-dir))
          force? (boolean force_cross_project)]
      (if (= slave_id "all")
        (let [slave-ids (if caller-project-id
                          (ds/get-slave-ids-by-project caller-project-id)
                          (keys (registry/get-available-lings)))
              {:keys [can-kill? blocked-slaves]} (check-any-critical-ops slave-ids)]
          (cond
            (empty? slave-ids)
            (core/mcp-success {:killed 0
                               :project-id caller-project-id
                               :message (if caller-project-id
                                          (format "No lings found for project '%s'" caller-project-id)
                                          "No lings to kill")})

            (not can-kill?)
            (core/mcp-error
             (format "KILL BLOCKED: Cannot kill slaves with critical operations in progress. Blocked slaves: %s"
                     (str/join ", " (map (fn [{:keys [id ops]}]
                                           (format "%s (%s)" id (format-blocking-ops ops)))
                                         blocked-slaves))))

            caller-project-id
            (let [results (mapv #(kill-single-slave! % caller-project-id force?) slave-ids)
                  killed (filter :success results)
                  failed (remove :success results)]
              (let [current-count (count (registry/get-available-lings))]
                (prom/set-lings-active! (max 0 (- current-count (count killed)))))
              (core/mcp-success {:killed (count killed)
                                 :failed (count failed)
                                 :project-id caller-project-id
                                 :details {:killed (mapv :slave-id killed)
                                           :failed (mapv #(select-keys % [:slave-id :error]) failed)}}))

            :else
            (let [results (mapv #(kill-single-slave! % nil false) slave-ids)
                  killed (filter :success results)
                  failed (remove :success results)]
              (when (every? :success results)
                (reset! hivemind/agent-registry {}))
              (prom/set-lings-active! (max 0 (- (count slave-ids) (count killed))))
              (core/mcp-success {:killed (count killed)
                                 :failed (count failed)
                                 :details {:killed (mapv :slave-id killed)
                                           :failed (mapv #(select-keys % [:slave-id :error]) failed)}}))))
        (let [{:keys [success error] :as result} (kill-single-slave! slave_id caller-project-id force?)]
          (if success
            (do
              (let [current-count (count (registry/get-available-lings))]
                (prom/set-lings-active! (max 0 (dec current-count))))
              (core/mcp-success (:result result)))
            (core/mcp-error
             (format "KILL BLOCKED: Cannot kill slave '%s' - %s"
                     slave_id error))))))))
