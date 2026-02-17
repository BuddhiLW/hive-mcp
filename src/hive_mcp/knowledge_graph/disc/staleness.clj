(ns hive-mcp.knowledge-graph.disc.staleness
  "Staleness surfacing and context classification for disc entities."
  (:require [hive-mcp.knowledge-graph.disc.hash :as hash]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [hive-dsl.result :as r]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn staleness-score
  "Compute staleness score for a disc entity (0.0-1.0)."
  [disc]
  (let [path (:disc/path disc)
        hash-result (when path (hash/file-content-hash path))]
    (vol/staleness-score disc hash-result)))

(defn staleness-report
  "Compute staleness score and diagnostic info for a disc entity."
  [disc]
  (let [path (:disc/path disc)
        hash-result (when path (hash/file-content-hash path))]
    (vol/staleness-report disc hash-result)))

(defn staleness-warnings
  "Generate staleness warnings for stale files from a collection of paths."
  [paths]
  (when (seq paths)
    (->> paths
         (keep (fn [path]
                 (when-let [disc (crud/get-disc path)]
                   (let [{:keys [score days-since-read hash-mismatch?]}
                         (staleness-report disc)]
                     (when (> score 0.3)
                       {:path path
                        :staleness score
                        :message (format "NOTE: file %s is stale (staleness: %.1f%s%s). Re-read carefully."
                                         path
                                         (float score)
                                         (if days-since-read
                                           (format ", last read %dd ago" days-since-read)
                                           ", never read")
                                         (if hash-mismatch?
                                           ", hash mismatch"
                                           ""))})))))
         vec)))

(defn top-stale-files
  "Query top-N most stale disc entities sorted by staleness score."
  [& {:keys [n project-id threshold] :or {n 5 threshold 0.5}}]
  (let [discs (crud/get-all-discs :project-id project-id)]
    (->> discs
         (map (fn [disc]
                (let [report (staleness-report disc)]
                  (assoc report :path (:disc/path disc)))))
         (filter #(> (:score %) threshold))
         (sort-by :score >)
         (take n)
         vec)))

(def ^:private missing-disc-entry
  "Default entry for a path with no disc data."
  {:disc nil
   :status :missing
   :staleness-score 1.0
   :days-since-read nil
   :hash-mismatch? false
   :never-analyzed? true
   :read-count 0
   :last-read-at nil})

(defn- classify-disc
  "Classify a single file path's KG status as fresh, stale, or missing."
  [path staleness-threshold]
  (let [result (r/guard Exception (assoc missing-disc-entry :path path)
                        (if-let [disc (crud/get-disc path)]
                          (let [{:keys [score days-since-read hash-mismatch? never-analyzed?]}
                                (staleness-report disc)]
                            {:path path
                             :disc disc
                             :status (if (<= score staleness-threshold) :fresh :stale)
                             :staleness-score score
                             :days-since-read days-since-read
                             :hash-mismatch? hash-mismatch?
                             :never-analyzed? never-analyzed?
                             :read-count (or (:disc/read-count disc) 0)
                             :last-read-at (:disc/last-read-at disc)})
                          (assoc missing-disc-entry :path path)))]
    (when-let [err (::r/error (meta result))]
      (log/warn "KG classify-disc failed, treating as missing"
                {:path path :error (:message err)}))
    result))

(defn kg-first-context
  "Classify file paths by KG freshness to determine which need reading."
  [paths & [{:keys [staleness-threshold] :or {staleness-threshold 0.3}}]]
  (let [unique-paths (distinct (filter (every-pred string? seq) paths))
        classified (mapv #(classify-disc % staleness-threshold) unique-paths)
        grouped (group-by :status classified)
        kg-known (->> (:fresh grouped)
                      (reduce (fn [m entry]
                                (assoc m (:path entry)
                                       (dissoc entry :path :status)))
                              {}))
        needs-read (mapv :path (:missing grouped))
        stale (mapv :path (:stale grouped))]
    {:kg-known kg-known
     :needs-read needs-read
     :stale stale
     :summary {:total (count unique-paths)
               :known (count kg-known)
               :needs-read (count needs-read)
               :stale (count stale)}}))
