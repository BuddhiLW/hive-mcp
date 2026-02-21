(ns hive-mcp.tools.catchup.enrichment
  "KG enrichment functions for the catchup workflow."
  (:require [hive-mcp.chroma.core :as chroma]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn find-related-via-session-summaries
  "Find entries related to session summaries via :derived-from traversal."
  [session-ids _project-id]
  (when (seq session-ids)
    (try
      (->> session-ids
           (pmap (fn [session-id]
                   (try
                     (let [results (kg-queries/traverse
                                    session-id
                                    {:direction :incoming
                                     :relations #{:derived-from}
                                     :max-depth 2})]
                       (mapv :node-id results))
                     (catch Exception _ []))))
           (apply concat)
           (distinct)
           (vec))
      (catch Exception e
        (log/debug "Session traversal failed:" (.getMessage e))
        []))))

(defn find-related-decisions-via-kg
  "Find decisions connected via :implements, :refines, or :depends-on relationships."
  [decision-ids _project-id]
  (when (seq decision-ids)
    (try
      (->> decision-ids
           (pmap (fn [decision-id]
                   (try
                     (let [results (kg-queries/traverse
                                    decision-id
                                    {:direction :both
                                     :relations #{:implements :refines :depends-on}
                                     :max-depth 2})]
                       (mapv :node-id results))
                     (catch Exception _ []))))
           (apply concat)
           (distinct)
           (remove (set decision-ids))
           (vec))
      (catch Exception e
        (log/debug "Decision traversal failed:" (.getMessage e))
        []))))

(defn entry-grounding-age-days
  "Compute age in days since entry was last grounded."
  [entry]
  (let [grounded-at (or (get-in entry [:metadata :grounded-at])
                        (:grounded-at entry))]
    (when (and grounded-at (not (str/blank? (str grounded-at))))
      (try
        (let [grounded-inst (cond
                              (instance? java.util.Date grounded-at)
                              (.toInstant grounded-at)

                              (string? grounded-at)
                              (java.time.Instant/parse grounded-at)

                              :else nil)]
          (when grounded-inst
            (.toDays (java.time.Duration/between grounded-inst (java.time.Instant/now)))))
        (catch Exception _ nil)))))

(defn entry->grounding-warning
  "Return a warning map if entry needs regrounding, nil otherwise."
  [entry max-age-days]
  (let [entry-id (or (:id entry) "unknown")
        entry-type (name (or (:type entry) "unknown"))
        grounded-at (or (get-in entry [:metadata :grounded-at])
                        (:grounded-at entry))
        age-days (entry-grounding-age-days entry)
        never-grounded? (or (nil? grounded-at)
                            (str/blank? (str grounded-at)))
        stale? (or never-grounded?
                   (and age-days (> age-days max-age-days)))]
    (when stale?
      {:id entry-id
       :type entry-type
       :grounded-at (when-not never-grounded? (str grounded-at))
       :age-days age-days
       :never-grounded? never-grounded?
       :preview (subs (str (:content entry ""))
                      0 (min (count (str (:content entry ""))) 60))})))

(defn check-grounding-freshness
  "Check grounding freshness of top project entries."
  [project-id & [{:keys [max-age-days limit timeout-ms]
                  :or {max-age-days 7 limit 20 timeout-ms 5000}}]]
  (let [result-future
        (future
          (try
            (let [decisions (chroma/query-entries :type "decision"
                                                  :project-id project-id
                                                  :limit limit)
                  conventions (chroma/query-entries :type "convention"
                                                    :project-id project-id
                                                    :limit limit)
                  all-entries (take (* 2 limit) (concat decisions conventions))
                  warnings (->> all-entries
                                (keep #(entry->grounding-warning % max-age-days))
                                (vec))]
              {:total-checked (count all-entries)
               :stale-count (count warnings)
               :stale-entries warnings
               :timed-out? false})
            (catch Exception e
              (log/debug "Grounding freshness check failed:" (.getMessage e))
              {:total-checked 0
               :stale-count 0
               :stale-entries []
               :error (.getMessage e)
               :timed-out? false})))
        result (deref result-future timeout-ms ::timeout)]
    (if (= result ::timeout)
      (do
        (future-cancel result-future)
        (log/warn "Grounding freshness check timed out after" timeout-ms "ms")
        {:total-checked 0
         :stale-count 0
         :stale-entries []
         :timed-out? true})
      result)))

(defn find-co-accessed-suggestions
  "Find memory entries frequently co-accessed with the given entries."
  [entry-ids exclude-ids]
  (when (seq entry-ids)
    (try
      (let [excluded (set exclude-ids)
            co-accessed (->> entry-ids
                             (pmap (fn [eid]
                                     (try (kg-edges/get-co-accessed eid)
                                          (catch Exception _ []))))
                             (apply concat)
                             (remove #(contains? excluded (:entry-id %)))
                             (group-by :entry-id)
                             (map (fn [[eid entries]]
                                    {:entry-id eid
                                     :confidence (apply max (map :confidence entries))
                                     :co-access-count (count entries)}))
                             (sort-by (fn [{:keys [confidence co-access-count]}]
                                        (* confidence co-access-count))
                                      >)
                             (take 5)
                             vec)]
        (when (seq co-accessed)
          (log/debug "Found" (count co-accessed) "co-accessed suggestions"))
        co-accessed)
      (catch Exception e
        (log/debug "Co-access suggestions failed:" (.getMessage e))
        []))))

(defn extract-kg-relations
  "Extract meaningful KG relationships from node context."
  [{:keys [incoming outgoing]}]
  (let [extract-by-rel (fn [edges rel from?]
                         (->> (:edges edges)
                              (filter #(= (:kg-edge/relation %) rel))
                              (map #(if from?
                                      (:kg-edge/from %)
                                      (:kg-edge/to %)))
                              (vec)))
        supersedes (extract-by-rel outgoing :supersedes false)
        depends-on (extract-by-rel outgoing :depends-on false)
        derived-from (extract-by-rel outgoing :derived-from false)
        superseded-by (extract-by-rel incoming :supersedes true)
        depended-by (extract-by-rel incoming :depends-on true)
        contradicts-out (extract-by-rel outgoing :contradicts false)
        contradicts-in (extract-by-rel incoming :contradicts true)
        contradicts (vec (distinct (concat contradicts-out contradicts-in)))]
    (cond-> {}
      (seq supersedes) (assoc :supersedes supersedes)
      (seq superseded-by) (assoc :superseded-by superseded-by)
      (seq depends-on) (assoc :depends-on depends-on)
      (seq depended-by) (assoc :depended-by depended-by)
      (seq derived-from) (assoc :derived-from derived-from)
      (seq contradicts) (assoc :contradicts contradicts))))

(defn enrich-entry-with-kg
  "Enrich a single entry with its KG relationships."
  [entry]
  (try
    (when-let [entry-id (:id entry)]
      (let [context (kg-queries/get-node-context entry-id)
            relations (extract-kg-relations context)]
        (if (seq relations)
          (assoc entry :kg relations)
          entry)))
    (catch Exception e
      (log/debug "KG enrichment failed for" (:id entry) ":" (.getMessage e))
      entry)))

(defn enrich-entries-with-kg
  "Enrich a collection of entries with KG context."
  [entries]
  (try
    (let [enriched (vec (pmap enrich-entry-with-kg entries))
          kg-count (count (filter :kg enriched))]
      (when (pos? kg-count)
        (log/debug "Enriched" kg-count "entries with KG context"))
      {:entries enriched
       :kg-count kg-count})
    (catch Exception e
      (log/warn "KG enrichment failed:" (.getMessage e))
      {:entries entries
       :kg-count 0
       :warnings [(.getMessage e)]})))

(defn gather-kg-insights
  "Gather high-level KG insights for the catchup summary."
  [decisions-meta conventions-meta sessions-meta project-id]
  (try
    (let [kg-stats (kg-edges/edge-stats)
          edge-count (:total-edges kg-stats)
          by-relation (:by-relation kg-stats)

          session-ids (mapv :id sessions-meta)
          decision-ids (mapv :id decisions-meta)

          ;; Fire independent KG queries in parallel
          f-sessions-related (future (find-related-via-session-summaries session-ids project-id))
          f-decisions-related (future (find-related-decisions-via-kg decision-ids project-id))
          f-grounding (future (check-grounding-freshness project-id
                                                         {:max-age-days 7
                                                          :limit 20
                                                          :timeout-ms 5000}))
          f-stale-files (future
                          (try
                            (kg-disc/top-stale-files :n 10 :project-id project-id :threshold 0.5)
                            (catch Exception e
                              (log/debug "KG insights stale-files query failed:" (.getMessage e))
                              [])))
          related-from-sessions (deref f-sessions-related 10000 [])
          related-decisions (deref f-decisions-related 10000 [])
          grounding-check (deref f-grounding 10000 {:stale-count 0})

          all-entries (concat decisions-meta conventions-meta)
          contradictions (->> all-entries
                              (filter #(seq (get-in % [:kg :contradicts])))
                              (mapv #(select-keys % [:id :preview :kg])))
          superseded (->> all-entries
                          (filter #(seq (get-in % [:kg :superseded-by])))
                          (mapv #(select-keys % [:id :preview :kg])))
          with-deps (->> all-entries
                         (filter #(or (seq (get-in % [:kg :depends-on]))
                                      (seq (get-in % [:kg :depended-by]))))
                         (count))

          stale-files (deref f-stale-files 10000 [])]

      (cond-> {:edge-count edge-count}
        (seq by-relation) (assoc :by-relation by-relation)
        (seq contradictions) (assoc :contradictions contradictions)
        (seq superseded) (assoc :superseded superseded)
        (pos? with-deps) (assoc :dependency-chains with-deps)
        (seq related-from-sessions) (assoc :session-derived related-from-sessions)
        (seq related-decisions) (assoc :related-decisions related-decisions)
        (pos? (:stale-count grounding-check)) (assoc :grounding-warnings grounding-check)
        (seq stale-files) (assoc :stale-files stale-files)))
    (catch Exception e
      (log/warn "KG insights gathering failed:" (.getMessage e))
      {:edge-count 0 :error (.getMessage e)})))
