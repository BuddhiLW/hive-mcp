(ns hive-mcp.tools.consolidated.workflow
  "Consolidated Workflow CLI tool for Forja Belt automation.

   Thin facade that routes MCP commands to sub-namespace implementations:
   - workflow.forge-ops: smite, survey, forge-status
   - workflow.forge-cycle: build-fsm-resources, forge-strike logic
   - workflow.spawn: spawn, spark, drone dispatch
   - workflow.readiness: agent readiness checks

   Includes defense-in-depth guard: child lings (spawned agents) are
   denied from executing forge-strike to prevent recursive self-call
   chains."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.consolidated.session :as c-session]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as forge-ops]
            [hive-mcp.tools.consolidated.workflow.forge-cycle :as forge-cycle]
            [hive-mcp.config.core :as config]
            [hive-mcp.server.guards :as guards]
            [hive-mcp.dns.result :as result]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ── Forge State ─────────────────────────────────────────────────────────────

(defonce ^:private forge-state
  (atom {:quenched?       false
         :last-strike     nil
         :last-fsm-result nil
         :total-smited    0
         :total-sparked   0
         :total-strikes   0}))

;; ── Result Helpers ──────────────────────────────────────────────────────────

(defn- try-result
  "Execute f in try/catch, returning Result. f must return a Result ({:ok ...}).
   Exceptions -> (result/err category {:message ...})."
  [category f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (result/err category {:message (ex-message e) :data (ex-data e)}))
       (catch Exception e
         (result/err category {:message (ex-message e) :class (str (class e))}))))

(defn- result->mcp
  "Convert a Result to MCP response: ok -> mcp-json, err -> mcp-error."
  [r]
  (if (result/ok? r)
    (mcp-json (:ok r))
    (mcp-error (or (:message r) (str (:error r))))))

;; ── Forge Strike Handlers ───────────────────────────────────────────────────

(defn handle-forge-strike-legacy
  "DEPRECATED: Execute ONE forge cycle via imperative smite!/survey/spark!."
  [params]
  (log/warn "DEPRECATED: forge strike-legacy called. Use 'forge strike' (FSM) instead."
            {:directory (:directory params)})
  (if (:quenched? @forge-state)
    (mcp-json {:success false
               :message "Forge is quenched. Use forge-status to check or restart."
               :quenched? true})
    (result->mcp (try-result :forge/strike-legacy-failed
                             #(forge-cycle/forge-strike-legacy* params forge-state)))))

(defn- do-forge-strike-fsm
  "FSM-driven forge strike."
  [params]
  (result->mcp (try-result :forge/strike-failed
                           #(forge-cycle/fsm-forge-strike* params forge-state))))

(defn handle-forge-strike
  "Execute ONE forge cycle, FSM-driven by default with legacy config gate.

   Defense-in-depth: denies forge-strike when called from a child ling process
   (HIVE_MCP_ROLE=child-ling). Forge-strike spawns agents, which must be
   restricted to the coordinator to prevent recursive self-call chains."
  [params]
  ;; Layer 3: Defense-in-depth forge-strike guard
  (if (guards/child-ling?)
    (do
      (log/warn "Forge-strike denied: child ling attempted forge-strike"
                {:role (guards/get-role) :depth (guards/ling-depth)})
      (mcp-error (str "FORGE DENIED: Child lings cannot execute forge strikes.\n\n"
                      "You are running as a child ling (HIVE_MCP_ROLE=child-ling, depth="
                      (guards/ling-depth) ").\n"
                      "Forge-strike spawns agents, which is restricted to the coordinator\n"
                      "to prevent recursive self-call chains (Ling->forge->spawn->Ling->inf).\n\n"
                      "If you need forge work, use hivemind_shout to request the coordinator\n"
                      "to execute forge-strike on your behalf.")))
    (if (:quenched? @forge-state)
      (mcp-json {:success false
                 :message "Forge is quenched. Use forge-status to check or restart."
                 :quenched? true})
      (if (config/get-service-value :forge :legacy :default false)
        (do
          (log/warn "FORGE STRIKE: legacy mode enabled via config. Using imperative path.")
          (handle-forge-strike-legacy params))
        (do-forge-strike-fsm params)))))

;; ── Forge Status ────────────────────────────────────────────────────────────

(defn handle-forge-status
  "Belt dashboard: show forge state, active lings, kanban summary."
  [params]
  (result->mcp (try-result :forge/status-failed
                           #(forge-ops/forge-status* params forge-state))))

;; ── Forge Quench ────────────────────────────────────────────────────────────

(defn handle-forge-quench
  "Gracefully stop or restart the forge belt."
  [{:keys [restart]}]
  (if restart
    (do
      (swap! forge-state assoc :quenched? false)
      (log/info "FORGE RESTART: Belt restarted. forge-strike is available again.")
      (mcp-json {:success true
                 :message "Forge restarted. Ready for forge-strike."
                 :state @forge-state}))
    (do
      (swap! forge-state assoc :quenched? true)
      (log/info "FORGE QUENCH: Belt stopped. Active lings will continue to completion.")
      (mcp-json {:success true
                 :message "Forge quenched. Active lings will finish. No new spawns."
                 :state @forge-state}))))

;; ── Deprecated Aliases ──────────────────────────────────────────────────────

(def handle-forge-strike-fsm
  "DEPRECATED alias: FSM is now the default path via handle-forge-strike."
  handle-forge-strike)

(def handle-forge-strike-imperative
  "DEPRECATED alias: renamed to handle-forge-strike-legacy."
  handle-forge-strike-legacy)

;; ── CLI Handler + Tool Definition ───────────────────────────────────────────

(def canonical-handlers
  {:catchup  c-session/handle-catchup
   :wrap     c-session/handle-wrap
   :complete (fn [params] (c-session/handle-session (assoc params :command "complete")))
   :forge    {:strike             handle-forge-strike
              :strike-imperative  handle-forge-strike-imperative
              :status             handle-forge-status
              :quench             handle-forge-quench
              :_handler           handle-forge-status}})

(def handlers canonical-handlers)

(def handle-workflow
  (make-cli-handler handlers))

(def tool-def
  {:name "workflow"
   :consolidated true
   :description "Forja Belt workflow: catchup (restore context), wrap (crystallize), complete (full lifecycle), forge-strike (FSM-driven smite->survey->spark cycle), forge-strike-imperative (DEPRECATED legacy path), forge-status (belt dashboard), forge-quench (graceful stop). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["catchup" "wrap" "complete"
                                                "forge strike"
                                                "forge strike-imperative"
                                                "forge status" "forge quench"
                                                "help"]
                                         :description "Workflow operation to perform"}
                              "commit_msg" {:type "string"
                                            :description "Git commit message (for complete)"}
                              "task_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Kanban task IDs. For complete: marks done. For forge-strike: survey whitelist."}
                              "task_filter" {:type "string"
                                             :description "Title prefix filter for survey. E.g. 'result-dsl:' matches all tasks starting with that prefix. More LLM-friendly than exact task_ids."}
                              "agent_id" {:type "string"
                                          :description "Agent ID for session attribution"}
                              "directory" {:type "string"
                                           :description "Working directory for project scoping"}}
                 :required ["command"]}
   :handler handle-workflow})

(def tools [tool-def])
