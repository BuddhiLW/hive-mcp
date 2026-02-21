(ns hive-mcp.tools.swarm.claim
  "File claim management MCP tools for visibility and manual control over claims."
  (:require [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.events.core :as events]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn record-claim-timestamp!
  "Record a claim timestamp in DataScript for staleness tracking."
  ([file-path]
   (record-claim-timestamp! file-path "unknown"))
  ([file-path slave-id]
   (lings/claim-file! file-path slave-id)))

(defn remove-claim-timestamp!
  "Remove a claim from DataScript when task completes."
  [file-path]
  (lings/release-claim! file-path))

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
  "List all active file claims from both storage systems with staleness warnings."
  [_params]
  (let [ds-claims (lings/get-all-claims)
        logic-claims (logic/get-all-claims)
        now (System/currentTimeMillis)

        formatted-ds (mapv #(format-claim % now) ds-claims)
        stale-count (count (filter :stale formatted-ds))

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
             :logic-claims-count (count logic-claims)
             :ghost-claims ghost-claims
             :ghost-count (count ghost-claims)
             :storage-in-sync (empty? ghost-claims)})}))

(defn handle-claim-clear
  "Release a claim by file path from both storage systems."
  [{:keys [file_path force]}]
  (if (empty? file_path)
    {:type "text"
     :text (json/write-str {:error "file_path is required"})}
    (let [ds-claim (lings/get-claim-info file_path)
          logic-claim (logic/get-claim-for-file file_path)
          has-claim? (or ds-claim logic-claim)]
      (if has-claim?
        (let [now (System/currentTimeMillis)
              created-at (:created-at ds-claim)
              age-ms (when created-at (- now created-at))
              age-min (when age-ms (/ age-ms 60000.0))
              stale? (and age-ms (> age-ms lings/default-stale-threshold-ms))
              owner (or (:slave-id ds-claim) (:slave-id logic-claim))
              ghost? (and logic-claim (not ds-claim))]
          (if (or force stale? ghost?)
            (do
              (when ds-claim
                (lings/release-claim! file_path))
              (when logic-claim
                (logic/remove-claim! file_path (:slave-id logic-claim)))
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
  "Release all stale claims and clean ghost claims."
  [{:keys [threshold_minutes dry_run include_ghosts]}]
  (let [threshold-ms (if threshold_minutes
                       (* threshold_minutes 60 1000)
                       lings/default-stale-threshold-ms)
        stale-claims (lings/get-stale-claims threshold-ms)
        ds-claims (lings/get-all-claims)
        ds-files (set (map :file ds-claims))
        logic-claims (logic/get-all-claims)
        ghost-claims (->> logic-claims
                          (remove #(contains? ds-files (:file %)))
                          vec)
        clean-ghosts? (if (false? include_ghosts) false true)
        total-to-clean (+ (count stale-claims)
                          (if clean-ghosts? (count ghost-claims) 0))]
    (if (zero? total-to-clean)
      {:type "text"
       :text (json/write-str
              {:success true
               :message "No stale or ghost claims found"
               :threshold-minutes (/ threshold-ms 60000)
               :ghost-count 0})}
      (if dry_run
        {:type "text"
         :text (json/write-str
                {:dry-run true
                 :would-release-stale (count stale-claims)
                 :would-release-ghosts (if clean-ghosts? (count ghost-claims) 0)
                 :stale-claims (mapv (fn [{:keys [file slave-id age-minutes]}]
                                       {:file file
                                        :owner slave-id
                                        :age-minutes age-minutes})
                                     stale-claims)
                 :ghost-claims (when clean-ghosts?
                                 (mapv (fn [{:keys [file slave-id]}]
                                         {:file file :owner slave-id})
                                       ghost-claims))
                 :hint "Run with dry_run=false to actually release these claims"})}
        (let [stale-result (lings/cleanup-stale-claims! threshold-ms)
              ghost-result (when clean-ghosts?
                             (doseq [{:keys [file slave-id]} ghost-claims]
                               (logic/remove-claim! file slave-id))
                             {:ghost-cleared (count ghost-claims)})]
          (doseq [file (:released-files stale-result)]
            (events/dispatch [:claim/file-released {:file file
                                                    :released-by "auto-cleanup"}]))
          (doseq [{:keys [file]} ghost-claims]
            (events/dispatch [:claim/file-released {:file file
                                                    :released-by "ghost-cleanup"}]))
          (log/info "claim_cleanup: Released" (:released-count stale-result) "stale claims,"
                    (or (:ghost-cleared ghost-result) 0) "ghost claims")
          {:type "text"
           :text (json/write-str
                  {:success true
                   :released-stale (:released-count stale-result)
                   :released-ghosts (or (:ghost-cleared ghost-result) 0)
                   :released-files (:released-files stale-result)
                   :ghost-files (mapv :file ghost-claims)
                   :threshold-minutes (/ threshold-ms 60000)})})))))

(def tools
  "REMOVED: Flat claim tools no longer exposed. Use consolidated `agent` tool with `claims` command."
  [])
