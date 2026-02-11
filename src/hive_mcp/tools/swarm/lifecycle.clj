(ns hive-mcp.tools.swarm.lifecycle
  "Swarm lifecycle handlers - spawn and kill operations with ownership and kill guards."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.agent.context :as ctx]
            [clojure.string :as str]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [hive-mcp.telemetry.prometheus :as prom]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- write-spawn-context-file
  "Write spawn context to a temporary file for elisp consumption."
  [context-str]
  (when context-str
    (try
      (let [tmp-file (java.io.File/createTempFile "hive-spawn-ctx-" ".md")]
        (spit tmp-file context-str)
        (.getAbsolutePath tmp-file))
      (catch Exception e
        (log/warn "Failed to write spawn context file:" (.getMessage e))
        nil))))

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance with context injection."
  [{:keys [name presets cwd _role terminal kanban_task_id]}]
  (core/with-swarm
    (let [effective-cwd (or cwd (ctx/current-directory))
          validated-cwd (when (and effective-cwd (string? effective-cwd) (not (str/blank? effective-cwd)))
                          effective-cwd)
          spawn-ctx (try
                      (catchup/spawn-context validated-cwd)
                      (catch Exception e
                        (log/warn "spawn-context generation failed (non-fatal):" (.getMessage e))
                        nil))
          ctx-file (write-spawn-context-file spawn-ctx)
          presets-str (when (seq presets)
                        (format "'(%s)" (str/join " " (map #(format "\"%s\"" %) presets))))
          elisp (format "(json-encode (hive-mcp-swarm-api-spawn \"%s\" %s %s %s %s %s))"
                        (v/escape-elisp-string (or name "slave"))
                        (or presets-str "nil")
                        (if validated-cwd (format "\"%s\"" (v/escape-elisp-string validated-cwd)) "nil")
                        (if terminal (format "\"%s\"" terminal) "nil")
                        (if kanban_task_id (format "\"%s\"" (v/escape-elisp-string kanban_task_id)) "nil")
                        (if ctx-file (format "\"%s\"" (v/escape-elisp-string ctx-file)) "nil"))
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
      (when spawn-ctx
        (log/info "spawn-context injected for ling" name
                  {:chars (count spawn-ctx) :file ctx-file}))
      (cond
        timed-out
        (core/mcp-timeout-error "Spawn operation" :extra-data {:slave_name name})

        success
        (do
          (let [current-count (count (registry/get-available-lings))]
            (prom/set-lings-active! (inc current-count)))
          (core/mcp-success result))

        :else
        (core/mcp-error (str "Error: " error))))))

(defn- format-blocking-ops
  "Format blocking operations for error message."
  [ops]
  (str/join ", " (map name ops)))

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
  "Kill a single slave by ID with ownership and critical ops checks."
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
           (let [elisp (format "(json-encode (hive-mcp-swarm-api-kill \"%s\"))"
                               (v/escape-elisp-string slave_id))
                 {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
             (cond
               timed-out
               {:success false :error "timeout" :slave-id slave_id}

               success
               (let [parsed-result (if (string? result)
                                     (try (json/parse-string result true)
                                          (catch Exception _ result))
                                     result)
                     result-error (get parsed-result :error)]
                 (if (= result-error "kill-blocked")
                   (do
                     (log/warn "Kill blocked by safety validation" {:slave-id slave_id
                                                                    :reason (get parsed-result :reason)
                                                                    :message (get parsed-result :message)})
                     {:success false
                      :error "kill-blocked-safety"
                      :slave-id slave_id
                      :reason (get parsed-result :reason "buffer-safety-validation-failed")
                      :message (get parsed-result :message)})
                   (do
                     (hivemind/clear-agent! slave_id)
                     {:success true :result parsed-result :slave-id slave_id})))

               :else
               {:success false :error error :slave-id slave_id}))
           {:success false
            :error (format "critical ops: %s" (format-blocking-ops blocking-ops))
            :slave-id slave_id}))))))

(defn handle-swarm-kill
  "Kill a slave or all slaves with kill guard and ownership checks."
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
            (let [elisp "(json-encode (hive-mcp-swarm-api-kill-all))"
                  {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
              (cond
                timed-out
                (core/mcp-timeout-error "Kill operation" :extra-data {:slave_id slave_id})

                success
                (do
                  (registry/clear-registry!)
                  (reset! hivemind/agent-registry {})
                  (prom/set-lings-active! 0)
                  (core/mcp-success result))

                :else
                (core/mcp-error (str "Error: " error))))))
        (let [{:keys [success error] :as result} (kill-single-slave! slave_id caller-project-id force?)]
          (if success
            (do
              (let [current-count (count (registry/get-available-lings))]
                (prom/set-lings-active! (max 0 (dec current-count))))
              (core/mcp-success (:result result)))
            (core/mcp-error
             (format "KILL BLOCKED: Cannot kill slave '%s' - %s"
                     slave_id error))))))))
