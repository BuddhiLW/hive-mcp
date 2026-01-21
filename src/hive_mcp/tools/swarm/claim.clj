(ns hive-mcp.tools.swarm.claim
  "File claim management MCP tools.

   Provides visibility and manual control over file claims:
   - claim_list: Show all active claims with staleness warnings
   - claim_clear: Release a claim by file path
   - claim_cleanup: Release all stale claims (auto-expiration)

   SOLID: SRP - File claim tools only
   CLARITY: I - Inputs validated at boundary, Y - Safe failure for missing claims"
  (:require [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.events.core :as events]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handlers
;; =============================================================================

(defn- format-claim
  "Format a claim with timestamp and staleness info."
  [{:keys [file slave-id created-at]} now]
  (let [age-ms (when created-at (- now created-at))
        age-min (when age-ms (/ age-ms 60000.0))
        stale? (and age-ms (> age-ms lings/default-stale-threshold-ms))]
    (cond-> {:file file
             :owner slave-id}
      created-at (assoc :claimed-at created-at
                        :age-minutes (when age-min (Math/round ^double age-min)))
      stale? (assoc :stale true
                    :warning (str "Claim is " (Math/round ^double age-min) " minutes old (>10 min)")))))

(defn handle-claim-list
  "Handler for claim_list tool.
   Lists all active file claims with timestamps and staleness warnings."
  [_params]
  (let [claims (lings/get-all-claims)
        now (System/currentTimeMillis)
        formatted (mapv #(format-claim % now) claims)
        stale-count (count (filter :stale formatted))]
    (log/info "claim_list: " (count claims) " claims, " stale-count " stale")
    {:type "text"
     :text (json/write-str
            {:claims formatted
             :total (count claims)
             :stale-count stale-count
             :stale-threshold-minutes 10})}))

(defn handle-claim-clear
  "Handler for claim_clear tool.
   Releases a claim by file path."
  [{:keys [file_path force]}]
  (if (empty? file_path)
    {:type "text"
     :text (json/write-str {:error "file_path is required"})}
    (let [claim (lings/get-claim-info file_path)]
      (if claim
        (let [now (System/currentTimeMillis)
              created-at (:created-at claim)
              age-ms (when created-at (- now created-at))
              age-min (when age-ms (/ age-ms 60000.0))
              stale? (and age-ms (> age-ms lings/default-stale-threshold-ms))]
          ;; Require force flag if claim is not stale (safety measure)
          (if (or force stale?)
            (do
              (lings/release-claim! file_path)
              ;; Emit event for claim release (enables queue processing)
              (events/dispatch [:claim/file-released {:file file_path
                                                      :released-by "manual-clear"}])
              (log/info "claim_clear: Released claim for" file_path "from" (:slave-id claim))
              {:type "text"
               :text (json/write-str
                      {:success true
                       :released {:file file_path
                                  :owner (:slave-id claim)
                                  :was-stale stale?
                                  :age-minutes (when age-min (Math/round ^double age-min))}})})
            {:type "text"
             :text (json/write-str
                    {:error "Claim is not stale. Use force=true to override."
                     :claim {:file file_path
                             :owner (:slave-id claim)
                             :age-minutes (when age-min (Math/round ^double age-min))}
                     :hint "Active claims may be in use. Only force-clear if you're sure the owner is stuck."})}))
        {:type "text"
         :text (json/write-str
                {:success false
                 :message (str "No claim exists for file: " file_path)})}))))

(defn handle-claim-cleanup
  "Handler for claim_cleanup tool.
   Releases all stale claims (auto-expiration)."
  [{:keys [threshold_minutes dry_run]}]
  (let [threshold-ms (if threshold_minutes
                       (* threshold_minutes 60 1000)
                       lings/default-stale-threshold-ms)
        stale-claims (lings/get-stale-claims threshold-ms)]
    (if (empty? stale-claims)
      {:type "text"
       :text (json/write-str
              {:success true
               :message "No stale claims found"
               :threshold-minutes (/ threshold-ms 60000)})}
      (if dry_run
        ;; Dry run - just report what would be cleaned
        {:type "text"
         :text (json/write-str
                {:dry-run true
                 :would-release (count stale-claims)
                 :stale-claims (mapv (fn [{:keys [file slave-id age-minutes]}]
                                       {:file file
                                        :owner slave-id
                                        :age-minutes age-minutes})
                                     stale-claims)
                 :hint "Run with dry_run=false to actually release these claims"})}
        ;; Actual cleanup
        (let [result (lings/cleanup-stale-claims! threshold-ms)]
          ;; Emit events for each released claim
          (doseq [file (:released-files result)]
            (events/dispatch [:claim/file-released {:file file
                                                    :released-by "auto-cleanup"}]))
          (log/info "claim_cleanup: Released" (:released-count result) "stale claims")
          {:type "text"
           :text (json/write-str
                  {:success true
                   :released-count (:released-count result)
                   :released-files (:released-files result)
                   :threshold-minutes (/ threshold-ms 60000)})})))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  "Claim management MCP tool definitions."
  [{:name "claim_list"
    :description "List all active file claims with owner, timestamp, and staleness warnings. Claims older than 10 minutes are marked as stale. Use this to diagnose blocked drones."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-claim-list}

   {:name "claim_clear"
    :description "Release a file claim by path. Requires force=true for non-stale claims (safety). Use when a ling/drone is stuck and holding claims."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute or relative file path to release"}
                               "force" {:type "boolean"
                                        :description "Force release even if claim is not stale (default: false)"}}
                  :required ["file_path"]}
    :handler handle-claim-clear}

   {:name "claim_cleanup"
    :description "Release all stale claims (auto-expiration). Claims older than threshold are automatically released. Use dry_run=true to preview what would be cleaned."
    :inputSchema {:type "object"
                  :properties {"threshold_minutes" {:type "number"
                                                    :description "Staleness threshold in minutes (default: 10)"}
                               "dry_run" {:type "boolean"
                                          :description "If true, only report stale claims without releasing (default: false)"}}
                  :required []}
    :handler handle-claim-cleanup}])
