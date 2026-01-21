(ns hive-mcp.tools.memory.format
  "JSON formatting utilities for memory entries.

   SOLID: SRP - Single responsibility for entry formatting.
   CLARITY: R - Represented intent with clear format transformations.

   Handles:
   - Converting entries to JSON-serializable format
   - Creating metadata-only representations
   - Preview generation for efficient browsing"
  (:require [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Full Entry Formatting
;; ============================================================

(defn entry->json-alist
  "Convert entry map to JSON-serializable format.
   Removes internal fields like :document.
   Converts kg-outgoing/kg-incoming to snake_case for JSON consistency."
  [entry]
  (let [base (-> entry
                 (update :tags #(or % []))
                 (dissoc :document)) ; Remove internal field
        ;; Convert KG edge fields from kebab-case to snake_case
        kg-outgoing (:kg-outgoing base)
        kg-incoming (:kg-incoming base)]
    (cond-> (dissoc base :kg-outgoing :kg-incoming)
      (seq kg-outgoing) (assoc :kg_outgoing_ids kg-outgoing)
      (seq kg-incoming) (assoc :kg_incoming_ids kg-incoming))))

;; ============================================================
;; Metadata-Only Formatting
;; ============================================================

(defn- truncate-string
  "Truncate string to max-len, adding ellipsis if truncated."
  [s max-len]
  (if (> (count s) max-len)
    (str (subs s 0 (- max-len 3)) "...")
    s))

(defn- content->preview
  "Extract preview from entry content.
   Handles string, map, and other content types."
  [content max-len]
  (cond
    (string? content)
    (subs content 0 (min max-len (count content)))

    (map? content)
    (or (:description content)
        (:title content)
        (:name content)
        (subs (json/write-str content) 0 (min max-len (count (json/write-str content)))))

    :else
    (str content)))

(defn entry->metadata
  "Convert entry to metadata-only format.
   Returns id, type, preview, tags, created.
   ~10x fewer tokens than full entry."
  ([entry]
   (entry->metadata entry 100))
  ([entry max-preview-len]
   (let [preview (content->preview (:content entry) max-preview-len)]
     {:id (:id entry)
      :type (:type entry)
      :preview (truncate-string (str preview) (- max-preview-len 3))
      :tags (or (:tags entry) [])
      :created (:created entry)})))

;; ============================================================
;; Collection Formatting
;; ============================================================

(defn entries->json
  "Convert collection of entries to JSON string."
  [entries]
  (json/write-str (mapv entry->json-alist entries)))

(defn entries->metadata-json
  "Convert collection of entries to metadata-only JSON string."
  [entries]
  (json/write-str (mapv entry->metadata entries)))
