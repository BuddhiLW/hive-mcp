(ns hive-mcp.knowledge-graph.disc.propagation
  "Time decay and transitive staleness propagation for disc entities."
  (:require [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.chroma.core :as chroma]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [hive-dsl.result :as r]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn apply-time-decay-to-all-discs!
  "Apply time-based certainty decay to all disc entities."
  [& {:keys [project-id]}]
  (let [discs (crud/get-all-discs :project-id project-id)
        results (reduce
                 (fn [acc disc]
                   (let [v (r/guard Exception nil
                                    (let [path (:disc/path disc)
                                          decayed (vol/apply-time-decay disc)
                                          updates {:disc/certainty-beta (:disc/certainty-beta decayed)
                                                   :disc/last-observation (:disc/last-observation decayed)}]
                                      (crud/update-disc! path updates)
                                      :ok))]
                     (if (= :ok v)
                       (update acc :updated inc)
                       (do (when-let [err (::r/error (meta v))]
                             (log/warn "Failed to apply decay to disc"
                                       {:path (:disc/path disc) :error (:message err)}))
                           (update acc :errors inc)))))
                 {:updated 0 :skipped 0 :errors 0}
                 discs)]
    (log/info "Time decay applied to discs" results)
    results))

(defn- apply-transitive-staleness!
  "Apply staleness to a single Chroma entry with depth-based decay."
  [entry-id base-staleness depth staleness-source]
  (let [beta-increment (* base-staleness
                          (Math/pow vol/staleness-decay-factor depth))]
    (if (< beta-increment vol/staleness-min-threshold)
      {:status :skipped :entry-id entry-id :beta-increment 0}
      (let [result (r/try-effect* :disc/staleness-propagation
                                  (let [current-entry (chroma/get-entry-by-id entry-id)
                                        current-beta (or (:staleness-beta current-entry) 1.0)
                                        new-beta (+ current-beta beta-increment)]
                                    (chroma/update-entry! entry-id
                                                          {:staleness-beta new-beta
                                                           :staleness-depth depth
                                                           :staleness-source (name staleness-source)})
                                    (log/debug "Applied transitive staleness"
                                               {:entry-id entry-id :depth depth :beta-increment beta-increment})
                                    {:status :updated :entry-id entry-id :beta-increment beta-increment}))]
        (if (r/ok? result)
          (:ok result)
          (do (log/warn "Failed to apply staleness to entry"
                        {:entry-id entry-id :error (:message result)})
              {:status :error :entry-id entry-id :error (:message result)}))))))

(defn- propagate-impact!
  "Propagate staleness through impact analysis for a single entry."
  [entry-id base-staleness staleness-source results]
  (let [impact (r/guard Exception nil
                        (queries/impact-analysis entry-id
                                                 {:max-depth vol/staleness-max-depth}))]
    (if (nil? impact)
      (do (log/warn "Impact analysis failed for entry" {:entry-id entry-id})
          (swap! results update :errors inc))
      (let [{:keys [direct transitive]} impact]
        (doseq [dep-id direct]
          (let [result (apply-transitive-staleness! dep-id base-staleness 1 staleness-source)]
            (case (:status result)
              :updated (swap! results update :propagated inc)
              :skipped (swap! results update :skipped inc)
              :error (swap! results update :errors inc))))
        (doseq [trans-id transitive]
          (let [result (apply-transitive-staleness! trans-id base-staleness 2 staleness-source)]
            (case (:status result)
              :updated (swap! results update :propagated inc)
              :skipped (swap! results update :skipped inc)
              :error (swap! results update :errors inc))))))))

(defn propagate-staleness!
  "Propagate staleness from a disc to dependent entries via KG edges."
  [disc-path base-staleness staleness-source]
  (let [outer (r/guard Exception {:propagated 0 :skipped 0 :errors 1 :grounded 0}
                       (let [grounded-entries (chroma/query-grounded-from disc-path)
                             results (atom {:propagated 0 :skipped 0 :errors 0 :grounded 0})]
                         (doseq [entry grounded-entries]
                           (let [entry-id (:id entry)
                                 result (apply-transitive-staleness! entry-id base-staleness 0 staleness-source)]
                             (case (:status result)
                               :updated (swap! results update :grounded inc)
                               :skipped (swap! results update :skipped inc)
                               :error (swap! results update :errors inc))
                             (propagate-impact! entry-id base-staleness staleness-source results)))
                         (log/info "Staleness propagation complete"
                                   {:disc-path disc-path :results @results})
                         @results))]
    (when-let [err (::r/error (meta outer))]
      (log/warn "Failed to propagate staleness"
                {:disc-path disc-path :error (:message err)}))
    outer))
