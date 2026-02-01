(ns hive-mcp.tools.swarm.status
  "Swarm status handlers - status, lings-available, broadcast.

   Provides visibility into swarm state with hivemind integration.
   Includes elisp fallback for lings when registry is empty.

   SOLID: SRP - Single responsibility for status queries.
   CLARITY: Y - Yield safe failure with graceful fallbacks."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.tools.swarm.state :as state]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Elisp Fallback for Lings (ADR-001 Phase 1 Fix)
;; ============================================================

(defn query-elisp-lings
  "Query elisp for list of lings when Clojure registry is empty.
   Returns parsed lings data or nil on failure.

   This function exists because:
   - Lings spawned directly via elisp won't be in Clojure registry
   - Registration at spawn time may fail (JSON parse error)
   - Provides fallback to ensure lings are always discoverable

   CLARITY: Y - Yield safe failure (returns nil on error)"
  []
  (when (core/swarm-addon-available?)
    (let [{:keys [success result timed-out]}
          (ec/eval-elisp-with-timeout
           "(json-encode (hive-mcp-swarm-list-lings))" 3000)]
      (when (and success (not timed-out))
        (try
          (let [parsed (json/read-str result :key-fn keyword)]
            (when (sequential? parsed)
              parsed))
          (catch Exception e
            (log/debug "Failed to parse elisp lings:" (ex-message e))
            nil))))))

(defn format-lings-for-response
  "Format lings data for MCP response.
   Accepts either registry map or elisp vector format.
   Includes project-id for project-scoped operations.

   CLARITY: R - Clear transformation of both data formats"
  [lings-data]
  (cond
    ;; Registry format: {slave-id {:name, :presets, :cwd, :project-id, :spawned-at}}
    (map? lings-data)
    (into {}
          (map (fn [[id info]]
                 [id (assoc info
                            :age-minutes (quot (- (System/currentTimeMillis)
                                                  (or (:spawned-at info) 0))
                                               60000))])
               lings-data))

    ;; Elisp format: [{:slave-id, :name, :presets, :cwd, :status}]
    (sequential? lings-data)
    (into {}
          (map (fn [ling]
                 [(or (:slave-id ling) (:slave_id ling))
                  {:name (:name ling)
                   :presets (:presets ling)
                   :cwd (:cwd ling)
                   :project-id (:project-id ling)  ;; May be nil for elisp-sourced
                   :status (:status ling)
                   :age-minutes nil}])  ;; Unknown age for elisp-sourced
               lings-data))

    :else {}))

;; ============================================================
;; Status Handler
;; ============================================================

(defn handle-swarm-status
  "Get swarm status including all slaves and their states.
   Uses timeout to prevent MCP blocking.

   TODO: SMELLY CODE - Needs further SOLID/DDD/CLARITY refactoring
   Issues:
   - Violates SRP: parsing, merging, error handling mixed together
   - Violates DIP: directly calls elisp instead of using abstraction
   - Dual state: elisp hash table + Clojure hivemind registry

   Parameters:
   - slave_id: Optional specific slave to query"
  [{:keys [slave_id]}]
  (core/with-swarm
    (let [elisp (if slave_id
                  (format "(json-encode (hive-mcp-swarm-status \"%s\"))" slave_id)
                  "(json-encode (hive-mcp-swarm-api-status))")
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Status check")

        success
        (try
          (let [parsed (json/read-str result :key-fn keyword)
                slaves-detail (:slaves-detail parsed)
                ;; Merge hivemind state into each slave's status
                merged-slaves (state/merge-hivemind-into-slaves slaves-detail)
                merged-parsed (if merged-slaves
                                (assoc parsed :slaves-detail merged-slaves)
                                parsed)]
            (core/mcp-success merged-parsed))
          (catch Exception e
            (log/warn "Failed to merge hivemind status:" (.getMessage e))
            (core/mcp-success result)))

        :else
        (core/mcp-error (str "Error: " error))))))

;; ============================================================
;; Lings Available Handler
;; ============================================================

(defn handle-lings-available
  "List all available lings (spawned slaves) with their metadata.

   Strategy (ADR-001 Phase 1):
   1. First check Clojure registry (fast path)
   2. If empty, fallback to elisp query (catches direct elisp spawns)
   3. Return merged/formatted response

   CLARITY: Y - Yield safe failure with graceful fallback"
  [_]
  (let [registry-lings (registry/get-available-lings)
        ;; ADR-001: Fallback to elisp when registry empty
        lings-data (if (empty? registry-lings)
                     (or (query-elisp-lings) {})
                     registry-lings)
        formatted (format-lings-for-response lings-data)
        source (if (empty? registry-lings)
                 (if (seq lings-data) "elisp-fallback" "empty")
                 "registry")]
    (core/mcp-success {:count (count formatted)
                       :lings formatted
                       :source source})))

;; ============================================================
;; Broadcast Handler
;; ============================================================

(defn handle-swarm-broadcast
  "Broadcast a prompt to all slaves.
   Uses timeout to prevent MCP blocking.

   FIX: Returns delivery count and errors if no targets found.
   Previously returned success even with empty slave list.

   Parameters:
   - prompt: The prompt to broadcast to all slaves

   Returns:
   - :delivered-count - Number of slaves that received the broadcast
   - :task-ids - List of task IDs created for each slave
   - :error - Only if no slaves are available to broadcast to"
  [{:keys [prompt]}]
  (core/with-swarm
    (let [elisp (format "(json-encode (hive-mcp-swarm-broadcast \"%s\"))"
                        (v/escape-elisp-string prompt))
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Broadcast operation")

        success
        (try
          (let [task-ids (json/read-str result :key-fn keyword)
                delivered-count (count task-ids)]
            (if (zero? delivered-count)
              ;; BUG FIX: Return error when no targets, not silent success
              (core/mcp-error-json
               {:error "no-targets"
                :message "No slaves available to broadcast to. Spawn slaves first with swarm_spawn."
                :delivered-count 0
                :task-ids []})
              ;; Success with delivery confirmation
              (core/mcp-success
               {:delivered-count delivered-count
                :task-ids task-ids
                :message (format "Broadcast delivered to %d slave(s)" delivered-count)})))
          (catch Exception e
            ;; Fallback to raw result if JSON parsing fails
            (log/warn "Failed to parse broadcast result:" (.getMessage e))
            (core/mcp-success result)))

        :else
        (core/mcp-error (str "Error: " error))))))

;; ============================================================
;; List Presets Handler
;; ============================================================

(defn handle-swarm-list-presets
  "List available swarm presets.
   Uses timeout to prevent MCP blocking."
  [_]
  (core/with-swarm
    (let [{:keys [success result error timed-out]}
          (ec/eval-elisp-with-timeout "(json-encode (hive-mcp-swarm-api-list-presets))" 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "List presets")

        success
        (core/mcp-success result)

        :else
        (core/mcp-error (str "Error: " error))))))
