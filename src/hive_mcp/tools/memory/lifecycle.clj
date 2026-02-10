(ns hive-mcp.tools.memory.lifecycle
  "Lifecycle handlers for memory entry duration management.

   SOLID: SRP - Single responsibility for entry lifecycle.
   CLARITY: Y - Yield safe failure with boundary handling.

   Handlers:
   - set-duration: Explicitly set duration category
   - promote: Move to longer duration
   - demote: Move to shorter duration
   - cleanup-expired: Remove expired entries
   - expiring-soon: List entries expiring within N days
   - decay: Scheduled time-based staleness decay (W2)
   - cross-pollination-promote: Auto-promote entries with cross-project access (W5)"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma with-entry]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.core :refer [mcp-error mcp-json coerce-int!]]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log])
  (:import [java.time ZonedDateTime]
           [java.time.temporal ChronoUnit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Set Duration Handler
;; ============================================================

(defn handle-set-duration
  "Set duration category for a memory entry (Chroma-only)."
  [{:keys [id duration]}]
  (log/info "mcp-memory-set-duration:" id duration)
  (with-chroma
    (let [expires (dur/calculate-expires duration)
          updated (chroma/update-entry! id {:duration duration
                                            :expires (or expires "")})]
      (if updated
        {:type "text" :text (json/write-str (fmt/entry->json-alist updated))}
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true}))))

;; ============================================================
;; Promote/Demote Handlers
;; ============================================================

(defn- shift-entry-duration
  "Shift entry duration by delta steps. Returns MCP response.
   SOLID: DRY - Unified promote/demote logic."
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
  "Promote memory entry to longer duration (Chroma-only)."
  [{:keys [id]}]
  (log/info "mcp-memory-promote:" id)
  (shift-entry-duration id +1 "Already at maximum duration"))

(defn handle-demote
  "Demote memory entry to shorter duration (Chroma-only)."
  [{:keys [id]}]
  (log/info "mcp-memory-demote:" id)
  (shift-entry-duration id -1 "Already at minimum duration"))

;; ============================================================
;; Cleanup Handler
;; ============================================================

(defn handle-cleanup-expired
  "Remove all expired memory entries (Chroma-only).
   Also cleans up KG edges for deleted entries to maintain referential integrity.
   Additionally repairs entries with invalid/missing duration or expires fields."
  [_]
  (log/info "mcp-memory-cleanup-expired")
  (with-chroma
    (let [{:keys [count deleted-ids repaired]} (chroma/cleanup-expired!)
          ;; Clean up KG edges for all deleted entries
          edges-removed (when (seq deleted-ids)
                          (reduce (fn [total id]
                                    (+ total (kg-edges/remove-edges-for-node! id)))
                                  0 deleted-ids))]
      (when (pos? (or edges-removed 0))
        (log/info "Cleaned up" edges-removed "KG edges for" count "deleted entries"))
      {:type "text" :text (json/write-str {:deleted count
                                           :kg_edges_removed (or edges-removed 0)
                                           :repaired (or repaired 0)})})))

;; ============================================================
;; Expire (Delete) Handler
;; ============================================================

(defn handle-expire
  "Force-expire (delete) a memory entry by ID.
   Also cleans up KG edges for the deleted entry."
  [{:keys [id]}]
  (log/info "mcp-memory-expire:" id)
  (with-entry [_entry id]
    (let [edges-removed (kg-edges/remove-edges-for-node! id)]
      (chroma/delete-entry! id)
      (when (pos? edges-removed)
        (log/info "Cleaned up" edges-removed "KG edges for expired entry" id))
      {:type "text" :text (json/write-str {:expired id
                                           :kg_edges_removed edges-removed})})))

;; ============================================================
;; Expiring Soon Handler
;; ============================================================

(defn- worth-promoting?
  "Filter for entries worth alerting about expiration.
   Short-duration memories are expected to expire - only alert on medium+ or axioms."
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
  "List memory entries expiring within N days (Chroma-only).
   ALWAYS filters by project scope (convention: every memory query must filter).
   Uses scope tag matching: returns entries matching current project OR global.
   By default, filters out short-duration memories (they're expected to expire)."
  [{:keys [days directory limit include-short]}]
  (try
    (let [days-val (coerce-int! days :days 3)
          limit-val (coerce-int! limit :limit 20)
          directory (or directory (ctx/current-directory))]
      (log/info "mcp-memory-expiring-soon:" days-val "limit:" limit-val "directory:" directory)
      (with-chroma
        (let [project-id (scope/get-current-project-id directory)
              ;; Fetch all expiring entries (don't filter by project-id in query
              ;; because we need scope tag filtering, not metadata field matching)
              all-entries (chroma/entries-expiring-soon days-val)
              ;; ALWAYS apply scope filter - convention: every memory query filters by scope
              ;; This matches the pattern in handle-query (crud.clj)
              scope-filter (scope/make-scope-tag project-id)
              filtered (->> all-entries
                            (filter #(scope/matches-scope? % scope-filter))
                            (filter #(or include-short (worth-promoting? %)))
                            (take limit-val))]
          ;; Use metadata-only format for expiring alerts (~10x fewer tokens)
          ;; User can fetch full content with memory:get if needed
          {:type "text" :text (json/write-str (mapv entry->expiring-meta filtered))})))
    (catch clojure.lang.ExceptionInfo e
      (if (= :coercion-error (:type (ex-data e)))
        (mcp-error (.getMessage e))
        (throw e)))))

;; ============================================================
;; Scheduled Decay Handler (W2)
;; ============================================================

(defn- apply-decay!
  "Apply staleness decay to a single entry.
   Returns {:id :delta :new-beta} or nil if no decay needed."
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
  "Run scheduled staleness decay on memory entries (W2 lifecycle hook).

   Scans all non-permanent, non-axiom entries and increases staleness-beta
   for entries with low access count and no recent recalls.

   Uses Bayesian Beta model: staleness = beta / (alpha + beta).
   Higher beta → entry is considered more stale.

   Parameters:
   - directory: working directory for project scope (optional, uses current)
   - access_threshold: max access-count to be a decay candidate (default: 3)
   - recency_days: grace period in days since last access (default: 7)
   - limit: max entries to process per cycle (default: 200)
   - dry_run: if true, compute but don't apply decay (default: false)

   Returns summary: {decayed: N, skipped: N, total_scanned: N, entries: [...]}"
  [{:keys [directory access_threshold recency_days limit dry_run]}]
  (log/info "mcp-memory-decay: starting scheduled decay cycle")
  (with-chroma
    (let [directory (or directory (ctx/current-directory))
          limit-val (or (some-> limit int) 200)
          opts {:access-threshold (or (some-> access_threshold int) 3)
                :recency-days (or (some-> recency_days int) 7)}
          ;; Query all entries (we filter in-process for decay candidates)
          all-entries (chroma/query-entries :limit limit-val
                                            :include-expired? false)
          ;; Apply project scope filter
          project-id (scope/get-current-project-id directory)
          scope-filter (scope/make-scope-tag project-id)
          scoped-entries (filter #(scope/matches-scope? % scope-filter) all-entries)
          ;; Split into candidates and skipped
          candidates (filter #(crystal/decay-candidate? % opts) scoped-entries)
          ;; Compute deltas
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
          ;; Apply if not dry run
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

;; ============================================================
;; Bounded Decay Cycle for Hooks (P0.3)
;; ============================================================

(defn run-decay-cycle!
  "Bounded, idempotent decay cycle for crystallize-session hooks.
   Returns plain Clojure map (NOT MCP response).

   Steps:
   1. Cleanup expired entries
   2. Query + scope-filter entries
   3. Apply staleness decay to candidates

   Parameters:
   - :directory - working directory for project scope
   - :limit - max entries to scan (default: 50)"
  [{:keys [directory limit]}]
  (try
    (let [directory (or directory (ctx/current-directory))
          limit-val (or (some-> limit int) 50)
          ;; Phase 1: cleanup expired
          cleanup-result (try
                           (chroma/cleanup-expired!)
                           (catch Exception e
                             {:count 0 :deleted-ids []
                              :error (.getMessage e)}))
          expired-count (or (:count cleanup-result) 0)
          cleanup-error (:error cleanup-result)
          ;; Phase 2: query + scope filter
          all-entries (chroma/query-entries :limit limit-val
                                            :include-expired? false)
          project-id (scope/get-current-project-id directory)
          scope-filter (scope/make-scope-tag project-id)
          scoped-entries (filter #(scope/matches-scope? % scope-filter) all-entries)
          ;; Phase 3: decay candidates
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

;; ============================================================
;; Cross-Pollination Auto-Promote Handler (W5)
;; ============================================================

(defn- promote-entry-by-tiers!
  "Promote an entry by N duration tiers. Returns promotion result map.
   Applies successive tier shifts, each with new expiry calculation."
  [entry tiers]
  (let [id (:id entry)
        original-duration (:duration entry)]
    (loop [current-duration original-duration
           remaining tiers]
      (if (or (zero? remaining) (= current-duration "permanent"))
        ;; Done - return result
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
        ;; Shift one tier up
        (let [{:keys [new-duration changed?]} (dur/shift-duration current-duration +1)]
          (if changed?
            (recur new-duration (dec remaining))
            ;; Hit ceiling
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

(defn handle-cross-pollination-promote
  "Scan and auto-promote entries that have been accessed across multiple projects (W5).

   Cross-pollination detection uses xpoll:project:<id> tags added by log-access.
   When an entry is accessed from 2+ distinct projects, it's a strong signal that
   the knowledge has universal value and should be promoted to a longer duration.

   Promotion tiers by breadth:
   - 2 projects: +1 tier (e.g., short → medium)
   - 3 projects: +2 tiers (e.g., short → long)
   - 4+ projects: promote to permanent

   Parameters:
   - directory: working directory for project scope (optional)
   - min_projects: minimum cross-project access count to trigger (default: 2)
   - limit: max entries to scan (default: 500)
   - dry_run: if true, show what would be promoted without acting (default: false)

   Returns summary: {promoted: N, candidates: N, total_scanned: N, entries: [...]}"
  [{:keys [_directory min_projects limit dry_run]}]
  (log/info "mcp-memory-cross-pollination-promote: scanning for cross-project knowledge")
  (with-chroma
    (let [limit-val (or (some-> limit int) 500)
          opts {:min-projects (or (some-> min_projects int) 2)}
          ;; Query all non-expired entries
          all-entries (chroma/query-entries :limit limit-val
                                            :include-expired? false)
          ;; Find cross-pollination candidates
          candidates (filter #(crystal/cross-pollination-candidate? % opts) all-entries)
          ;; Compute promotion plans
          plans (for [entry candidates]
                  (let [xpoll-projects (crystal/extract-xpoll-projects entry)
                        tiers (crystal/cross-pollination-promotion-tiers entry)]
                    {:id (:id entry)
                     :type (:type entry)
                     :duration (:duration entry)
                     :cross_projects (vec xpoll-projects)
                     :cross_project_count (count xpoll-projects)
                     :tiers_to_promote tiers
                     :next_duration (name (reduce (fn [d _] (crystal/current-duration->next d))
                                                  (keyword (:duration entry))
                                                  (range tiers)))}))
          ;; Apply promotions if not dry run
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
      (log/info "mcp-memory-cross-pollination-promote:"
                (:promoted summary) "promoted of" (:candidates summary)
                "candidates from" (:total_scanned summary) "entries"
                (when dry_run "(dry run)"))
      (mcp-json summary))))

;; ============================================================
;; Bounded Cross-Pollination Cycle (for crystallize-session)
;; ============================================================

(defn run-cross-pollination-cycle!
  "Run bounded cross-pollination auto-promotion cycle.

   Designed for wiring into crystallize-session (wrap/session-complete hooks).
   Scans entries with xpoll:project: tags, promotes candidates by tier count.

   Unlike handle-cross-pollination-promote (MCP tool), this:
   - Returns plain Clojure map (not MCP response)
   - Has lower default limit (100 vs 500 — wrap should be fast)
   - Catches all errors (non-blocking)
   - Is idempotent (already-promoted entries won't re-promote)

   Takes: {:directory string :limit int (default 100)}
   Returns: {:promoted N :candidates N :total-scanned N}
            or {:error msg :promoted 0 :candidates 0 :total-scanned 0}"
  [{:keys [directory limit]}]
  (try
    (if-not (chroma/embedding-configured?)
      {:promoted 0 :candidates 0 :total-scanned 0 :error "chroma-not-configured"}
      (let [limit-val (or (some-> limit int) 100)
            ;; Query all non-expired entries
            all-entries (chroma/query-entries :limit limit-val
                                              :include-expired? false)
            ;; Find cross-pollination candidates (2+ projects, not permanent, not axiom)
            candidates (filter #(crystal/cross-pollination-candidate? %) all-entries)
            ;; Apply promotions
            results (vec (for [entry candidates
                               :let [tiers (crystal/cross-pollination-promotion-tiers entry)]
                               :when (pos? tiers)]
                           (try
                             (promote-entry-by-tiers! entry tiers)
                             (catch Exception e
                               (log/warn "run-cross-pollination-cycle!: failed to promote" (:id entry)
                                         ":" (.getMessage e))
                               {:id (:id entry) :promoted false :error (.getMessage e)}))))
            promoted-count (count (filter :promoted results))]
        (when (pos? promoted-count)
          (log/info "Cross-pollination auto-promote:" promoted-count
                    "of" (count candidates) "candidates promoted"))
        {:promoted promoted-count
         :candidates (count candidates)
         :total-scanned (count all-entries)
         :entries (mapv #(select-keys % [:id :promoted :old_duration :new_duration :tiers_promoted])
                        results)}))
    (catch Exception e
      (log/warn "run-cross-pollination-cycle! failed (non-blocking):" (.getMessage e))
      {:error (.getMessage e) :promoted 0 :candidates 0 :total-scanned 0})))
