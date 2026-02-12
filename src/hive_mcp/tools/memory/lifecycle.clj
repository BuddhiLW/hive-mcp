(ns hive-mcp.tools.memory.lifecycle
  "Lifecycle handlers for memory entry duration management and decay."
  (:require [hive-mcp.tools.memory.core :refer [with-chroma with-entry]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.core :refer [mcp-error mcp-json coerce-int!]]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.chroma.core :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log])
  (:import [java.time ZonedDateTime]
           [java.time.temporal ChronoUnit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-set-duration
  "Set duration category for a memory entry."
  [{:keys [id duration]}]
  (log/info "mcp-memory-set-duration:" id duration)
  (with-chroma
    (let [expires (dur/calculate-expires duration)
          updated (chroma/update-entry! id {:duration duration
                                            :expires (or expires "")})]
      (if updated
        {:type "text" :text (json/write-str (fmt/entry->json-alist updated))}
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true}))))

(defn- shift-entry-duration
  "Shift entry duration by delta steps."
  [id delta boundary-msg]
  (with-entry [entry id]
    (let [{:keys [new-duration changed?]} (dur/shift-duration (:duration entry) delta)]
      (if-not changed?
        {:type "text" :text (json/write-str {:message boundary-msg
                                             :duration new-duration})}
        (let [expires (dur/calculate-expires new-duration)
              updated (chroma/update-entry! id {:duration new-duration
                                                :expires (or expires "")})]
          {:type "text" :text (json/write-str (fmt/entry->json-alist updated))})))))

(defn handle-promote
  "Promote memory entry to longer duration."
  [{:keys [id]}]
  (log/info "mcp-memory-promote:" id)
  (shift-entry-duration id +1 "Already at maximum duration"))

(defn handle-demote
  "Demote memory entry to shorter duration."
  [{:keys [id]}]
  (log/info "mcp-memory-demote:" id)
  (shift-entry-duration id -1 "Already at minimum duration"))

(defn handle-cleanup-expired
  "Remove all expired memory entries and clean up their KG edges."
  [_]
  (log/info "mcp-memory-cleanup-expired")
  (with-chroma
    (let [{:keys [count deleted-ids repaired]} (chroma/cleanup-expired!)
          edges-removed (when (seq deleted-ids)
                          (reduce (fn [total id]
                                    (+ total (kg-edges/remove-edges-for-node! id)))
                                  0 deleted-ids))]
      (when (pos? (or edges-removed 0))
        (log/info "Cleaned up" edges-removed "KG edges for" count "deleted entries"))
      {:type "text" :text (json/write-str {:deleted count
                                           :kg_edges_removed (or edges-removed 0)
                                           :repaired (or repaired 0)})})))

(defn handle-expire
  "Force-expire (delete) a memory entry by ID and clean up its KG edges."
  [{:keys [id]}]
  (log/info "mcp-memory-expire:" id)
  (with-entry [_entry id]
    (let [edges-removed (kg-edges/remove-edges-for-node! id)]
      (chroma/delete-entry! id)
      (when (pos? edges-removed)
        (log/info "Cleaned up" edges-removed "KG edges for expired entry" id))
      {:type "text" :text (json/write-str {:expired id
                                           :kg_edges_removed edges-removed})})))

(defn- worth-promoting?
  "Filter for entries worth alerting about expiration."
  [entry]
  (let [duration (:duration entry)
        entry-type (:type entry)]
    (or (= entry-type "axiom")
        (= entry-type "decision")
        (contains? #{"medium" "long" "permanent"} duration))))

(defn- entry->expiring-meta
  "Convert entry to expiring-alert format with duration/expires info."
  [entry]
  (let [base (fmt/entry->metadata entry 150)]
    (assoc base
           :duration (:duration entry)
           :expires (:expires entry))))

(defn handle-expiring-soon
  "List memory entries expiring within N days, filtered by project scope."
  [{:keys [days directory limit include-short]}]
  (try
    (let [days-val (coerce-int! days :days 3)
          limit-val (coerce-int! limit :limit 20)
          directory (or directory (ctx/current-directory))]
      (log/info "mcp-memory-expiring-soon:" days-val "limit:" limit-val "directory:" directory)
      (with-chroma
        (let [project-id (scope/get-current-project-id directory)
              all-entries (chroma/entries-expiring-soon days-val)
              scope-filter (scope/make-scope-tag project-id)
              filtered (->> all-entries
                            (filter #(scope/matches-scope? % scope-filter))
                            (filter #(or include-short (worth-promoting? %)))
                            (take limit-val))]
          {:type "text" :text (json/write-str (mapv entry->expiring-meta filtered))})))
    (catch clojure.lang.ExceptionInfo e
      (if (= :coercion-error (:type (ex-data e)))
        (mcp-error (.getMessage e))
        (throw e)))))

(defn- apply-decay!
  "Apply staleness decay to a single entry."
  [entry opts]
  (when (crystal/decay-candidate? entry opts)
    (let [delta (crystal/calculate-decay-delta entry opts)]
      (when (> delta 0.0)
        (let [old-beta (or (:staleness-beta entry) 1)
              new-beta (+ old-beta delta)]
          (chroma/update-staleness! (:id entry)
                                    {:beta new-beta
                                     :source :time-decay})
          {:id (:id entry)
           :delta delta
           :old-beta old-beta
           :new-beta new-beta})))))

(defn handle-decay
  "Run scheduled staleness decay on memory entries."
  [{:keys [directory access_threshold recency_days limit dry_run]}]
  (log/info "mcp-memory-decay: starting scheduled decay cycle")
  (with-chroma
    (let [directory (or directory (ctx/current-directory))
          limit-val (or (some-> limit int) 200)
          opts {:access-threshold (or (some-> access_threshold int) 3)
                :recency-days (or (some-> recency_days int) 7)}
          all-entries (chroma/query-entries :limit limit-val
                                            :include-expired? false)
          project-id (scope/get-current-project-id directory)
          scope-filter (scope/make-scope-tag project-id)
          scoped-entries (filter #(scope/matches-scope? % scope-filter) all-entries)
          candidates (filter #(crystal/decay-candidate? % opts) scoped-entries)
          decay-plans (for [entry candidates
                            :let [delta (crystal/calculate-decay-delta entry opts)]
                            :when (> delta 0.0)]
                        {:id (:id entry)
                         :type (:type entry)
                         :duration (:duration entry)
                         :access-count (:access-count entry)
                         :old-beta (or (:staleness-beta entry) 1)
                         :delta delta
                         :new-beta (+ (or (:staleness-beta entry) 1) delta)})
          applied (if dry_run
                    (vec decay-plans)
                    (vec (keep #(apply-decay! (chroma/get-entry-by-id (:id %)) opts)
                               decay-plans)))
          summary {:decayed (count applied)
                   :skipped (- (count scoped-entries) (count decay-plans))
                   :total_scanned (count scoped-entries)
                   :dry_run (boolean dry_run)
                   :entries (mapv #(select-keys % [:id :delta :new-beta]) applied)}]
      (log/info "mcp-memory-decay: decayed" (:decayed summary)
                "of" (:total_scanned summary) "entries"
                (when dry_run "(dry run)"))
      (mcp-json summary))))

(defn run-decay-cycle!
  "Bounded, idempotent decay cycle for crystallize-session hooks."
  [{:keys [directory limit]}]
  (try
    (let [directory (or directory (ctx/current-directory))
          limit-val (or (some-> limit int) 50)
          cleanup-result (try
                           (chroma/cleanup-expired!)
                           (catch Exception e
                             {:count 0 :deleted-ids []
                              :error (.getMessage e)}))
          expired-count (or (:count cleanup-result) 0)
          cleanup-error (:error cleanup-result)
          all-entries (chroma/query-entries :limit limit-val
                                            :include-expired? false)
          project-id (scope/get-current-project-id directory)
          scope-filter (scope/make-scope-tag project-id)
          scoped-entries (filter #(scope/matches-scope? % scope-filter) all-entries)
          opts {:access-threshold 3 :recency-days 7}
          applied (vec (keep #(apply-decay! % opts) scoped-entries))
          result (cond-> {:decayed (count applied)
                          :expired expired-count
                          :total-scanned (count scoped-entries)
                          :error nil}
                   cleanup-error (assoc :cleanup-error cleanup-error))]
      result)
    (catch Exception e
      {:decayed 0 :expired 0 :total-scanned 0
       :error (.getMessage e)})))

(defn- promote-entry-by-tiers!
  "Promote an entry by N duration tiers."
  [entry tiers]
  (let [id (:id entry)
        original-duration (:duration entry)]
    (loop [current-duration original-duration
           remaining tiers]
      (if (or (zero? remaining) (= current-duration "permanent"))
        (if (= current-duration original-duration)
          {:id id :promoted false :duration current-duration}
          (let [expires (dur/calculate-expires current-duration)
                _updated (chroma/update-entry! id {:duration current-duration
                                                   :expires (or expires "")})]
            (log/info "Cross-pollination auto-promote:" id
                      original-duration "->" current-duration
                      "(tiers:" tiers ")")
            {:id id
             :promoted true
             :old_duration original-duration
             :new_duration current-duration
             :tiers_promoted (- tiers remaining)}))
        (let [{:keys [new-duration changed?]} (dur/shift-duration current-duration +1)]
          (if changed?
            (recur new-duration (dec remaining))
            (if (= current-duration original-duration)
              {:id id :promoted false :duration current-duration}
              (let [expires (dur/calculate-expires current-duration)
                    updated (chroma/update-entry! id {:duration current-duration
                                                      :expires (or expires "")})]
                (log/info "Cross-pollination auto-promote:" id
                          original-duration "->" current-duration
                          "(tiers:" (- tiers remaining) "hit ceiling)")
                {:id id
                 :promoted true
                 :old_duration original-duration
                 :new_duration current-duration
                 :tiers_promoted (- tiers remaining)}))))))))

(defn handle-xpoll-promote
  "Scan and auto-promote entries accessed across multiple projects."
  [{:keys [_directory min_projects limit dry_run]}]
  (log/info "mcp-memory-xpoll-promote: scanning for cross-project knowledge")
  (with-chroma
    (let [limit-val (or (some-> limit int) 500)
          opts {:min-projects (or (some-> min_projects int) 2)}
          all-entries (chroma/query-entries :limit limit-val
                                            :include-expired? false)
          candidates (filter #(crystal/scope-eligible? % opts) all-entries)
          plans (for [entry candidates]
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
  [{:keys [directory limit]}]
  (try
    (if-not (chroma/embedding-configured?)
      {:promoted 0 :candidates 0 :total-scanned 0 :error "chroma-not-configured"}
      (let [limit-val (or (some-> limit int) 100)
            all-entries (chroma/query-entries :limit limit-val
                                              :include-expired? false)
            candidates (filter #(crystal/scope-eligible? %) all-entries)
            results (vec (for [entry candidates
                               :let [tiers (crystal/scope-tiers entry)]
                               :when (pos? tiers)]
                           (try
                             (promote-entry-by-tiers! entry tiers)
                             (catch Exception e
                               (log/warn "run-xpoll-cycle!: failed to promote" (:id entry)
                                         ":" (.getMessage e))
                               {:id (:id entry) :promoted false :error (.getMessage e)}))))
            promoted-count (count (filter :promoted results))]
        (when (pos? promoted-count)
          (log/info "Xpoll auto-promote:" promoted-count
                    "of" (count candidates) "candidates promoted"))
        {:promoted promoted-count
         :candidates (count candidates)
         :total-scanned (count all-entries)
         :entries (mapv #(select-keys % [:id :promoted :old_duration :new_duration :tiers_promoted])
                        results)}))
    (catch Exception e
      (log/warn "run-xpoll-cycle! failed (non-blocking):" (.getMessage e))
      {:error (.getMessage e) :promoted 0 :candidates 0 :total-scanned 0})))
