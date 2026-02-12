(ns hive-mcp.events.effects.coeffect
  "Coeffect handlers for the hive-mcp event system.

   Coeffects inject contextual data into event handlers (read-only inputs).

   Coeffects implemented:
   - :now             - Current timestamp in milliseconds
   - :agent-context   - Agent ID and current working directory
   - :db-snapshot     - DataScript database snapshot
   - :waiting-lings   - Query lings waiting on a specific file (File Claim Cascade)
   - :request-ctx     - Current request context from tool execution

   Usage:
   ```clojure
   (require '[hive-mcp.events.effects.coeffect :as cofx-effects])
   (cofx-effects/register-coeffects!)
   ```"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.agent.context :as ctx]
            [datascript.core :as d]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Coeffect: :now (POC-08)
;; =============================================================================

(defn- handle-now
  "Inject current timestamp in milliseconds.

   Returns coeffects map with :now key added."
  [coeffects]
  (assoc coeffects :now (System/currentTimeMillis)))

;; =============================================================================
;; Coeffect: :agent-context (POC-09)
;; =============================================================================

(defn- handle-agent-context
  "Inject agent environment info (agent-id and working directory).

   Returns coeffects map with :agent-context key added.
   Agent ID comes from CLAUDE_SWARM_SLAVE_ID env var (may be nil).
   CWD comes from user.dir system property (always available)."
  [coeffects]
  (assoc coeffects :agent-context
         {:agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")
          :cwd (System/getProperty "user.dir")}))

;; =============================================================================
;; Coeffect: :db-snapshot (POC-10)
;; =============================================================================

(defn- handle-db-snapshot
  "Inject DataScript database snapshot (dereferenced connection).

   Returns coeffects map with :db-snapshot key added."
  [coeffects]
  (assoc coeffects :db-snapshot @(ds/get-conn)))

;; =============================================================================
;; Coeffect: :waiting-lings (POC-11)
;; =============================================================================

(defn- handle-waiting-lings
  "Query lings waiting on a specific file (File Claim Cascade).

   Accepts an optional file-path parameter to filter by.
   Returns coeffects map with :waiting-lings key added as a vector
   of {:slave-id \"...\" :task-id \"...\"} maps."
  [coeffects file-path]
  (let [db @(ds/get-conn)
        waiting (when (and db file-path)
                  (d/q '[:find ?slave-id ?task-id
                         :in $ ?file
                         :where
                         [?t :task/files ?file]
                         [?t :task/status :queued]
                         [?t :task/id ?task-id]
                         [?t :task/slave ?s]
                         [?s :slave/id ?slave-id]]
                       db file-path))]
    (assoc coeffects :waiting-lings
           (mapv (fn [[slave-id task-id]]
                   {:slave-id slave-id
                    :task-id task-id})
                 (or waiting [])))))

;; =============================================================================
;; Coeffect: :request-ctx
;; =============================================================================

(defn- handle-request-ctx
  "Inject current request context from tool execution.

   Returns coeffects map with :request-ctx key added."
  [coeffects]
  (assoc coeffects :request-ctx (ctx/request-ctx)))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-coeffects!
  "Register all coeffect handlers.

   Coeffects registered:
   - :now             - Current timestamp in milliseconds (POC-08)
   - :agent-context   - Agent ID and current working directory (POC-09)
   - :db-snapshot     - DataScript database snapshot (POC-10)
   - :waiting-lings   - Query lings waiting on a file (POC-11)
   - :request-ctx     - Current request context from tool execution

   Called from hive-mcp.events.effects/register-effects!"
  []
  (ev/reg-cofx :now handle-now)
  (ev/reg-cofx :agent-context handle-agent-context)
  (ev/reg-cofx :db-snapshot handle-db-snapshot)
  (ev/reg-cofx :waiting-lings handle-waiting-lings)
  (ev/reg-cofx :request-ctx handle-request-ctx)
  (log/info "[hive-events.coeffect] Coeffects registered: :now :agent-context :db-snapshot :waiting-lings :request-ctx"))
