(ns hive-mcp.chroma.search
  "Semantic search operations for Chroma memory entries.

   Provides vector-based similarity search using the configured
   embedding provider."
  (:require [clojure-chroma-client.api :as chroma]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.embeddings :as emb]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn search-similar
  "Search for memory entries similar to the query text.
   Options:
     :limit - Max results to return (default: 10)
     :type - Filter by memory type (note, snippet, convention, decision)
     :project-ids - Filter by multiple projects using Chroma $in operator

   When :project-ids is provided, builds {:project-id {:$in ids}} for
   DB-level filtering, reducing over-fetch needs for scoped searches.

   Returns seq of {:id, :document, :metadata, :distance}"
  [query-text & {:keys [limit type project-ids] :or {limit 10}}]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        query-embedding (emb/embed-text (emb/get-embedding-provider) query-text)
        where-clause (cond-> {}
                       type (assoc :type type)
                       project-ids (assoc :project-id {:$in (vec project-ids)}))
        where-clause (when (seq where-clause) where-clause)
        results @(chroma/query coll query-embedding
                               :num-results limit
                               :where where-clause
                               :include #{:documents :metadatas :distances})]
    (log/debug "Semantic search for:" (subs query-text 0 (min 50 (count query-text))) "..."
               "found:" (count results))
    results))

(defn search-by-id
  "Get a specific entry by ID from Chroma."
  [id]
  (let [coll (conn/get-or-create-collection)
        results @(chroma/get coll :ids [id] :include #{:documents :metadatas})]
    (first results)))
