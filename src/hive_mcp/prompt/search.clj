(ns hive-mcp.prompt.search
  "Search and query operations for prompt database.

   CLARITY-L: Layers stay pure - search logic separated from I/O.

   Provides:
   - Filtered listing (by category, quality, tags)
   - Full-text keyword search
   - ID-based lookup
   - Statistics aggregation"
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; ============================================================
;;; Core Search Functions
;;; ============================================================

(defn list-prompts
  "List captured prompts with optional filtering.

   Arguments:
   - entries: Vector of prompt entries to search
   - opts: Map with optional filters

   Options:
   - :category - Filter by category keyword
   - :quality - Filter by quality rating
   - :tags - Filter by tags (any match)
   - :limit - Max results (default 50)"
  [entries & [{:keys [category quality tags limit]
               :or {limit 50}}]]
  (let [filtered (cond->> entries
                   category (filter #(= (:category %) category))
                   quality (filter #(= (:quality %) quality))
                   (seq tags) (filter #(some (set (:tags %)) tags))
                   true (take limit))]
    {:count (count filtered)
     :total (count entries)
     :entries (vec filtered)}))

(defn search-prompts
  "Search prompts by keyword in prompt text or accomplishes.

   Arguments:
   - entries: Vector of prompt entries to search
   - query: Search string
   - opts: Map with optional settings

   Options:
   - :limit - Max results (default 20)"
  [entries query & [{:keys [limit] :or {limit 20}}]]
  (let [query-lower (str/lower-case query)
        matches (filter (fn [e]
                          (or (str/includes? (str/lower-case (:prompt e "")) query-lower)
                              (str/includes? (str/lower-case (:accomplishes e "")) query-lower)))
                        entries)]
    {:count (count matches)
     :query query
     :entries (vec (take limit matches))}))

(defn get-prompt
  "Get a specific prompt by ID.

   Arguments:
   - entries: Vector of prompt entries to search
   - id: The prompt ID to find

   Returns the matching entry or nil."
  [entries id]
  (first (filter #(= (:id %) id) entries)))

(defn get-statistics
  "Get statistics about captured prompts.

   Arguments:
   - entries: Vector of prompt entries

   Returns map with:
   - :total - Total count
   - :by-category - Frequency map by category
   - :by-quality - Frequency map by quality
   - :by-source - Frequency map by source
   - :recent - 5 most recent entries"
  [entries]
  {:total (count entries)
   :by-category (frequencies (map :category entries))
   :by-quality (frequencies (map :quality entries))
   :by-source (frequencies (map :source entries))
   :recent (take 5 (reverse (sort-by :created entries)))})
