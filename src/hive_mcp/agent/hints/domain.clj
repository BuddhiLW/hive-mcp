(ns hive-mcp.agent.hints.domain
  "Pure domain calculations for hint generation.
  Extracted from hints.clj to reduce cyclomatic complexity.
  All functions are pure calculations (no I/O, no side effects)."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ---------------------------------------------------------------------------
;; Utility: CC-free nil-coalescing
;; ---------------------------------------------------------------------------

(defn or-val
  "CC-free nil-coalescing. Returns val when non-nil/non-false, otherwise default.
  Replaces (or val default) with zero cyclomatic complexity cost (if-let is FREE)."
  [val default]
  (if-let [v val] v default))

;; ---------------------------------------------------------------------------
;; Tag Domain
;; ---------------------------------------------------------------------------

(defn meaningful-tags
  "Filter out meta-tags (scope:*, agent:*, catchup-priority) from a tag collection.
  Returns a vector of domain-meaningful tags."
  [tags]
  (into []
        (remove (fn [tag]
                  (or (str/starts-with? tag "scope:")
                      (str/starts-with? tag "agent:")
                      (= tag "catchup-priority"))))
        (or-val tags [])))

(defn scope-visible?
  "Check if an entry is visible given scope tags and visible project IDs."
  [scope-tags visible-ids entry]
  (let [entry-tags (set (or-val (:tags entry) []))]
    (or (some entry-tags scope-tags)
        (contains? visible-ids (:project-id entry)))))

;; ---------------------------------------------------------------------------
;; Serialization Sections — each returns a vector of strings, or nil if empty
;; All use when-let (CC=0) instead of when (+1 CC)
;; ---------------------------------------------------------------------------

(defn axiom-section
  "Render axiom IDs section as string vector, or nil if empty."
  [axiom-ids]
  (when-let [ids (seq axiom-ids)]
    ["### Axiom IDs (INVIOLABLE — fetch ALL)"
     "```"
     (str "memory get " (str/join " " (take 10 ids)))
     "```"
     (str "Count: " (count axiom-ids))
     ""]))

(defn read-id-section
  "Render priority memory IDs section as string vector, or nil if empty."
  [read-ids]
  (when-let [ids (seq read-ids)]
    (let [batches (mapv #(str "memory get " (str/join " " %)) (partition-all 5 ids))]
      (into ["### Priority Memory IDs (fetch during Silence)"
             "```"]
            (concat batches
                    ["```"
                     (str "Count: " (count read-ids))
                     ""])))))

(defn query-section
  "Render semantic queries section as string vector, or nil if empty."
  [queries]
  (when-let [qs (seq queries)]
    (into ["### Semantic Queries (search during Silence)"]
          (concat (mapv #(str "- `memory search \"" % "\"`") qs)
                  [""]))))

(defn tag-section
  "Render tag queries section as string vector, or nil if empty."
  [tag-sets]
  (when-let [ts (seq tag-sets)]
    (into ["### Tag Queries"]
          (concat (mapv #(str "- `memory query --tags " (str/join "," %) "`") ts)
                  [""]))))

(defn kg-section
  "Render KG seeds section as string vector, or nil if empty."
  [kg-seeds kg-depth]
  (when-let [seeds (seq kg-seeds)]
    (let [depth (or-val kg-depth 2)]
      (into [(str "### Knowledge Graph Seeds (traverse depth " depth ")")]
            (concat (mapv #(str "- `kg traverse --start " % " --depth " depth "`") seeds)
                    [""])))))

(defn git-section
  "Render git status section as string vector, or nil if empty."
  [git-info]
  (when-let [gi git-info]
    (cond-> ["### Git Status"
             (str "- **Branch**: " (or-val (:branch gi) "unknown"))]
      (:uncommitted gi) (conj "- **Uncommitted changes**: yes")
      :always (conj (str "- **Last commit**: " (or-val (:last-commit gi) "unknown"))
                    ""))))

;; ---------------------------------------------------------------------------
;; Parse Helpers — each extracts data from hint section text (CC=0 via when-let)
;; ---------------------------------------------------------------------------

(def ^:private id-pattern #"memory get\s+([^\n`]+)")

(defn parse-memory-ids
  "Extract memory IDs from hint section text."
  [text]
  (when-let [matches (seq (re-seq id-pattern (or-val text "")))]
    (->> matches
         (mapcat (fn [[_ ids-str]] (str/split (str/trim ids-str) #"\s+")))
         (remove str/blank?)
         vec)))

(defn parse-search-queries
  "Extract semantic search queries from hint section text."
  [text]
  (when-let [matches (seq (re-seq #"memory search \"([^\"]+)\"" (or-val text "")))]
    (mapv second matches)))

(defn parse-tag-queries
  "Extract tag query sets from hint section text."
  [text]
  (when-let [matches (seq (re-seq #"memory query --tags\s+([^\n`]+)" (or-val text "")))]
    (mapv (fn [[_ tags-str]]
            (str/split (str/trim tags-str) #","))
          matches)))

(defn parse-kg-traversals
  "Extract KG traversal seeds and depth from hint section text.
  Returns {:seeds [...] :depth int} or nil."
  [text]
  (when-let [matches (seq (re-seq #"kg traverse --start\s+(\S+)\s+--depth\s+(\d+)" (or-val text "")))]
    {:seeds (mapv second matches)
     :depth (Integer/parseInt (nth (first matches) 2))}))

(defn parse-axiom-ids
  "Extract axiom IDs from the Axiom IDs section of hint text."
  [text]
  (when-let [section (second (re-find #"(?s)Axiom IDs.*?```\n(.*?)```" (or-val text "")))]
    (when-let [matches (seq (re-seq id-pattern section))]
      (->> matches
           (mapcat (fn [[_ ids-str]] (str/split (str/trim ids-str) #"\s+")))
           (remove str/blank?)
           vec))))
