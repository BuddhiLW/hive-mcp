(ns hive-mcp.tools.memory.promotion
  "Cross-pollination promotion handlers for memory entries.
   Extracted from lifecycle.clj for CC reduction."
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.chroma.core :as chroma]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn walk-tiers
  "Walk duration tiers upward by N steps. Pure function.
   Returns {:new-duration :tiers-walked :changed?}."
  [current-duration tiers]
  (loop [dur current-duration
         remaining tiers
         walked 0]
    (if (or (zero? remaining) (= dur "permanent"))
      {:new-duration dur :tiers-walked walked :changed? (pos? walked)}
      (let [{:keys [new-duration changed?]} (dur/shift-duration dur +1)]
        (if changed?
          (recur new-duration (dec remaining) (inc walked))
          {:new-duration dur :tiers-walked walked :changed? (pos? walked)})))))

(defn build-xpoll-plan
  "Build a cross-pollination plan for a single entry. Pure function."
  [entry]
  (let [xpoll-projects (crystal/extract-xpoll-projects entry)
        tiers (crystal/scope-tiers entry)]
    {:id (:id entry)
     :type (:type entry)
     :duration (:duration entry)
     :cross_projects (vec xpoll-projects)
     :cross_project_count (count xpoll-projects)
     :tiers_to_promote tiers
     :next_duration (name (reduce (fn [d _] (crystal/current-duration->next d))
                                  (keyword (:duration entry))
                                  (range tiers)))}))

(defn- persist-promotion!
  "Persist a tier promotion to Chroma. Side-effecting."
  [id original-duration {:keys [new-duration tiers-walked]}]
  (let [expires (dur/calculate-expires new-duration)]
    (chroma/update-entry! id {:duration new-duration :expires (or expires "")})
    (log/info "Cross-pollination auto-promote:" id original-duration "->" new-duration
              "(tiers:" tiers-walked ")")
    {:id id :promoted true :old_duration original-duration
     :new_duration new-duration :tiers_promoted tiers-walked}))

(defn promote-entry-by-tiers!
  "Promote an entry by N duration tiers."
  [entry tiers]
  (let [{:keys [changed?] :as result} (walk-tiers (:duration entry) tiers)]
    (if changed?
      (persist-promotion! (:id entry) (:duration entry) result)
      {:id (:id entry) :promoted false :duration (:duration entry)})))

(defn handle-xpoll-promote
  "Scan and auto-promote entries accessed across multiple projects."
  [{:keys [_directory min_projects limit dry_run]}]
  (log/info "mcp-memory-xpoll-promote: scanning for cross-project knowledge")
  (with-chroma
    (let [limit-val (or (some-> limit int) 500)
          opts {:min-projects (or (some-> min_projects int) 2)}
          all-entries (chroma/query-entries :limit limit-val :include-expired? false)
          candidates (filter #(crystal/scope-eligible? % opts) all-entries)
          plans (mapv build-xpoll-plan candidates)
          results (if dry_run
                    (vec plans)
                    (vec (for [plan plans
                               :let [entry (chroma/get-entry-by-id (:id plan))]
                               :when entry]
                           (promote-entry-by-tiers! entry (:tiers_to_promote plan)))))
          promoted-count (count (filter #(or (:promoted %) (not dry_run)) results))
          summary {:promoted (if dry_run 0 promoted-count)
                   :candidates (count candidates)
                   :total_scanned (count all-entries)
                   :dry_run (boolean dry_run)
                   :entries (vec (if dry_run
                                   (mapv #(select-keys % [:id :duration :cross_projects
                                                          :cross_project_count :next_duration])
                                         plans)
                                   (mapv #(select-keys % [:id :promoted :old_duration
                                                          :new_duration :tiers_promoted])
                                         results)))}]
      (log/info "mcp-memory-xpoll-promote:"
                (:promoted summary) "promoted of" (:candidates summary)
                "candidates from" (:total_scanned summary) "entries"
                (when dry_run "(dry run)"))
      (mcp-json summary))))

(defn run-xpoll-cycle!
  "Run bounded xpoll auto-promotion cycle for crystallize-session hooks."
  [{:keys [_directory limit]}]
  (try
    (if-not (chroma/embedding-configured?)
      {:promoted 0 :candidates 0 :total-scanned 0 :error "chroma-not-configured"}
      (let [limit-val (or (some-> limit int) 100)
            all-entries (chroma/query-entries :limit limit-val :include-expired? false)
            candidates (filter #(crystal/scope-eligible? %) all-entries)
            results (vec (for [entry candidates
                               :let [tiers (crystal/scope-tiers entry)]
                               :when (pos? tiers)]
                           (try
                             (promote-entry-by-tiers! entry tiers)
                             (catch Exception e
                               (log/warn "run-xpoll-cycle!: failed to promote" (:id entry)
                                         ":" (.getMessage e))
                               {:id (:id entry) :promoted false :error (.getMessage e)}))))]
        {:promoted (count (filter :promoted results))
         :candidates (count candidates)
         :total-scanned (count all-entries)
         :entries (mapv #(select-keys % [:id :promoted :old_duration :new_duration :tiers_promoted])
                        results)}))
    (catch Exception e
      (log/warn "run-xpoll-cycle! failed (non-blocking):" (.getMessage e))
      {:error (.getMessage e) :promoted 0 :candidates 0 :total-scanned 0})))
