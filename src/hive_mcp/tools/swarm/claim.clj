(ns hive-mcp.tools.swarm.claim
  "File claim management MCP tools.

   Provides visibility and manual control over file claims:
   - claim_list: Show all active claims with staleness warnings
   - claim_clear: Release a claim by file path
   - claim_cleanup: Release all stale claims (auto-expiration)

   DUAL STORAGE NOTE:
   Claims are stored in both core.logic (for conflict detection) and
   DataScript (for staleness tracking). This module provides visibility
   into both systems to help diagnose ghost claims.

   SOLID: SRP - File claim tools only
   CLARITY: I - Inputs validated at boundary, Y - Safe failure for missing claims"
  (:require [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.events.core :as events]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Timestamp Recording (for coordinator dual-storage pattern)
;; =============================================================================

(defn record-claim-timestamp!
  "Record a claim timestamp in DataScript for staleness tracking.
   Called by coordinator after adding claim to logic DB.

   The coordinator uses dual storage:
   - logic/add-claim! → core.logic pldb (in-memory, for conflict resolution)
   - record-claim-timestamp! → DataScript (persistent, for staleness detection)

   Arguments:
     file-path - File being claimed
     slave-id  - Slave making the claim (optional, defaults to unknown)"
  ([file-path]
   (record-claim-timestamp! file-path "unknown"))
  ([file-path slave-id]
   (lings/claim-file! file-path slave-id)))

(defn remove-claim-timestamp!
  "Remove a claim from DataScript when task completes.
   Called by coordinator when releasing task claims.

   Arguments:
     file-path - File to release"
  [file-path]
  (lings/release-claim! file-path))

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
   Lists all active file claims from BOTH storage systems with timestamps and staleness warnings.

   DUAL STORAGE VISIBILITY:
   - datascript-claims: Claims in DataScript (with timestamps for staleness)
   - logic-claims: Claims in core.logic (used for conflict detection)
   - ghost-claims: Claims in logic but not DataScript (the 'ghost claim' bug symptom)

   If ghost-claims is non-empty, there's a storage mismatch that may cause
   'file already claimed' errors when claim_list shows 0 claims."
  [_params]
  (let [;; Get claims from both storage systems
        ds-claims (lings/get-all-claims)
        logic-claims (logic/get-all-claims)
        now (System/currentTimeMillis)

        ;; Format DataScript claims with timestamps
        formatted-ds (mapv #(format-claim % now) ds-claims)
        stale-count (count (filter :stale formatted-ds))

        ;; Find ghost claims (in logic but not in DataScript)
        ds-files (set (map :file ds-claims))
        ghost-claims (->> logic-claims
                          (remove #(contains? ds-files (:file %)))
                          (mapv (fn [{:keys [file slave-id]}]
                                  {:file file
                                   :owner slave-id
                                   :ghost true
                                   :warning "Claim exists in logic but not DataScript - may cause false conflicts"})))]
    (log/info "claim_list:" (count ds-claims) "DataScript claims,"
              (count logic-claims) "logic claims,"
              (count ghost-claims) "ghost claims,"
              stale-count "stale")
    {:type "text"
     :text (json/write-str
            {:claims formatted-ds
             :total (count ds-claims)
             :stale-count stale-count
             :stale-threshold-minutes 10
             ;; GHOST CLAIMS DIAGNOSTIC
             :logic-claims-count (count logic-claims)
             :ghost-claims ghost-claims
             :ghost-count (count ghost-claims)
             :storage-in-sync (empty? ghost-claims)})}))

(defn handle-claim-clear
  "Handler for claim_clear tool.
   Releases a claim by file path from BOTH storage systems.

   GHOST CLAIMS FIX: Also checks core.logic for claims that may not
   be in DataScript (ghost claims). If a claim exists only in logic,
   it's released from there too."
  [{:keys [file_path force]}]
  (if (empty? file_path)
    {:type "text"
     :text (json/write-str {:error "file_path is required"})}
    (let [ds-claim (lings/get-claim-info file_path)
          logic-claim (logic/get-claim-for-file file_path)
          ;; Claim exists if in either storage
          has-claim? (or ds-claim logic-claim)]
      (if has-claim?
        (let [now (System/currentTimeMillis)
              ;; Use DataScript timestamp if available
              created-at (:created-at ds-claim)
              age-ms (when created-at (- now created-at))
              age-min (when age-ms (/ age-ms 60000.0))
              stale? (and age-ms (> age-ms lings/default-stale-threshold-ms))
              ;; Get owner from whichever storage has it
              owner (or (:slave-id ds-claim) (:slave-id logic-claim))
              ;; Ghost claim = in logic but not DataScript
              ghost? (and logic-claim (not ds-claim))]
          ;; Require force flag if claim is not stale AND not a ghost (safety measure)
          ;; Ghost claims can always be cleared (they're the bug symptom)
          (if (or force stale? ghost?)
            (do
              ;; Release from BOTH storage systems
              (when ds-claim
                (lings/release-claim! file_path))
              (when logic-claim
                (logic/remove-claim! file_path (:slave-id logic-claim)))
              ;; Emit event for claim release (enables queue processing)
              (events/dispatch [:claim/file-released {:file file_path
                                                      :released-by "manual-clear"}])
              (log/info "claim_clear: Released claim for" file_path "from" owner
                        (when ghost? "(was ghost claim)"))
              {:type "text"
               :text (json/write-str
                      {:success true
                       :released {:file file_path
                                  :owner owner
                                  :was-stale stale?
                                  :was-ghost ghost?
                                  :age-minutes (when age-min (Math/round ^double age-min))}})})
            {:type "text"
             :text (json/write-str
                    {:error "Claim is not stale. Use force=true to override."
                     :claim {:file file_path
                             :owner owner
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
