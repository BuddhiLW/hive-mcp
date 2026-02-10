(ns hive-mcp.tools.agent.helpers
  "Shared helper functions for agent tool handlers.

   Contains formatting, ID generation, and elisp integration utilities
   used across multiple agent handler modules.

   SOLID-S: Single responsibility - shared utilities only.
   CLARITY-Y: Safe failure on elisp queries."
  (:require [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.emacs.client :as ec]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; ID Generation
;; =============================================================================

(defn generate-agent-id
  "Generate unique agent ID with type prefix."
  [agent-type]
  (str (name agent-type) "-" (java.util.UUID/randomUUID)))

;; =============================================================================
;; Agent Formatting
;; =============================================================================

(defn format-agent
  "Format agent data for response."
  [agent-data]
  (when agent-data
    (let [base {:id (:slave/id agent-data)
                :status (:slave/status agent-data)
                :type (case (:slave/depth agent-data)
                        0 :coordinator
                        1 :ling
                        :drone)  ;; depth 2+ = drone
                :cwd (:slave/cwd agent-data)
                :project-id (:slave/project-id agent-data)}]
      (cond-> base
        (:slave/parent agent-data) (assoc :parent (:slave/parent agent-data))
        (:slave/presets agent-data) (assoc :presets (:slave/presets agent-data))
        (:slave/created-at agent-data) (assoc :created-at (:slave/created-at agent-data))))))

(defn format-agents
  "Format a list of agents for response."
  [agents]
  (let [formatted (->> agents
                       (map format-agent)
                       (remove nil?)
                       vec)]
    {:agents formatted
     :count (count formatted)
     :by-type (frequencies (map :type formatted))
     :by-status (frequencies (map :status formatted))}))

;; =============================================================================
;; Elisp Fallback for Lings (FIX: swarm_status only returning coordinator)
;; =============================================================================

(defn query-elisp-lings
  "Query elisp for list of lings that may not be in DataScript.
   Lings spawned directly via elisp won't be in DataScript.

   Returns a seq of slave maps in DataScript format, or empty seq on failure."
  []
  (when (swarm-core/swarm-addon-available?)
    (let [{:keys [success result timed-out]}
          (ec/eval-elisp-with-timeout
           "(json-encode (hive-mcp-swarm-list-lings))" 3000)]
      (when (and success (not timed-out))
        (try
          (let [parsed (json/read-str result :key-fn keyword)]
            (when (sequential? parsed)
              ;; Convert elisp format to DataScript format
              (->> parsed
                   (map (fn [ling]
                          {:slave/id (or (:slave-id ling) (:slave_id ling))
                           :slave/name (:name ling)
                           :slave/status (keyword (or (:status ling) "idle"))
                           :slave/depth 1  ;; lings are depth 1
                           :slave/cwd (:cwd ling)
                           :slave/project-id (:project-id ling)
                           :slave/presets (:presets ling)}))
                   (filter :slave/id))))  ;; filter out invalid entries
          (catch Exception e
            (log/debug "Failed to parse elisp lings:" (ex-message e))
            []))))))

(defn merge-with-elisp-lings
  "Merge DataScript agents with elisp lings.
   DataScript entries take precedence for duplicates (by ID).

   CLARITY: Y - Yield safe failure - returns at least DataScript data on error."
  [ds-agents]
  (try
    (let [;; Get elisp lings
          elisp-lings (or (query-elisp-lings) [])
          ;; Create set of IDs already in DataScript
          ds-ids (set (map :slave/id ds-agents))
          ;; Filter elisp lings to only those NOT in DataScript
          new-lings (remove #(ds-ids (:slave/id %)) elisp-lings)]
      (log/debug "Merging agents: DataScript=" (count ds-agents)
                 "elisp-only=" (count new-lings))
      (concat ds-agents new-lings))
    (catch Exception e
      (log/warn "Failed to merge elisp lings (returning DataScript only):" (ex-message e))
      ds-agents)))
