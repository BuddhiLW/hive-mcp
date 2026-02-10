(ns hive-mcp.tools.memory.gaps
  "Knowledge Gap Auto-Detection (P2.8).

   Lightweight regex/keyword analysis to identify knowledge gaps
   in memory content. Populated automatically on memory add.

   Detects:
   1. Questions (sentences ending with ?)
   2. TODO/TBD/FIXME/HACK/XXX markers
   3. Uncertainty language (unclear, unknown, uncertain, etc.)
   4. Missing/incomplete markers (missing, stub, not implemented, etc.)
   5. Assumption markers (assuming, assumption)

   SOLID-S: Single responsibility - knowledge gap extraction only.
   CLARITY-Y: Enhanced NLP-based analysis via extension (noop fallback)."
  (:require [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Constants
;; ============================================================

(def ^:private max-gaps
  "Maximum number of knowledge gaps to extract per entry."
  10)

(def ^:private max-gap-length
  "Maximum character length for a single gap descriptor."
  80)

;; ============================================================
;; Utility
;; ============================================================

(defn- truncate-gap
  "Truncate a gap descriptor to max-gap-length, preserving word boundaries."
  [s]
  (if (<= (count s) max-gap-length)
    s
    (let [truncated (subs s 0 max-gap-length)
          last-space (.lastIndexOf truncated " ")]
      (if (pos? last-space)
        (str (subs truncated 0 last-space) "...")
        (str (subs s 0 (- max-gap-length 3)) "...")))))

;; ============================================================
;; Extractors
;; ============================================================

(defn extract-questions
  "Extract sentences ending with question marks from content.
   Returns vector of gap descriptors like 'question: ...'."
  [content]
  (->> (re-seq #"[^.!?\n]*\?" content)
       (map str/trim)
       (remove str/blank?)
       (remove #(< (count %) 10))  ; Skip very short questions (e.g., just "?")
       (mapv #(truncate-gap (str "question: " %)))))

(defn extract-todo-markers
  "Extract TODO/TBD/FIXME/HACK/XXX markers and their context.
   Returns vector of gap descriptors like 'todo: ...'."
  [content]
  (->> (re-seq #"(?i)\b(TODO|TBD|FIXME|HACK|XXX)\b[:\s]*([^\n.!?]{0,60})" content)
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case marker)
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str marker-lower ": (no description)")
                    (str marker-lower ": " ctx))))))))

(def ^:private uncertainty-patterns
  "Regex patterns for uncertainty language in content."
  [#"(?i)\b(unclear|unknown|uncertain|unsure|undecided|unresolved)\b[:\s]*([^\n.!?]{0,60})"
   #"(?i)\b(not\s+sure|needs?\s+investigation|needs?\s+clarification|open\s+question)\b[:\s]*([^\n.!?]{0,60})"
   #"(?i)\b(needs?\s+to\s+be\s+determined|remains?\s+to\s+be\s+seen)\b[:\s]*([^\n.!?]{0,60})"])

(defn extract-uncertainty
  "Extract uncertainty language from content.
   Returns vector of gap descriptors like 'unclear: ...'."
  [content]
  (->> uncertainty-patterns
       (mapcat #(re-seq % content))
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case (str/trim marker))
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str "uncertain: " marker-lower)
                    (str "uncertain: " marker-lower " " ctx))))))
       (distinct)
       (vec)))

(def ^:private missing-patterns
  "Regex patterns for missing/incomplete markers in content."
  [#"(?i)\b(missing|incomplete|placeholder|stub|not\s+implemented|needs?\s+work)\b[:\s]*([^\n.!?]{0,60})"])

(defn extract-missing
  "Extract missing/incomplete markers from content.
   Returns vector of gap descriptors like 'missing: ...'."
  [content]
  (->> missing-patterns
       (mapcat #(re-seq % content))
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case (str/trim marker))
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str "missing: " marker-lower)
                    (str "missing: " marker-lower " " ctx))))))
       (distinct)
       (vec)))

(def ^:private assumption-patterns
  "Regex patterns for assumption markers in content."
  [#"(?i)\b(assuming|assumption)\b[:\s]*([^\n.!?]{0,60})"])

(defn extract-assumptions
  "Extract assumption markers from content.
   Returns vector of gap descriptors like 'assumption: ...'."
  [content]
  (->> assumption-patterns
       (mapcat #(re-seq % content))
       (mapv (fn [[_ marker context]]
               (let [marker-lower (str/lower-case (str/trim marker))
                     ctx (str/trim (or context ""))]
                 (truncate-gap
                  (if (str/blank? ctx)
                    (str "assumption: " marker-lower)
                    (str "assumption: " ctx))))))
       (distinct)
       (vec)))

;; ============================================================
;; Main Entry Point
;; ============================================================

(defn extract-knowledge-gaps
  "Analyze content to detect knowledge gaps.

   Lightweight regex/keyword analysis that identifies:
   1. Questions (sentences ending with ?)
   2. TODO/TBD/FIXME/HACK/XXX markers
   3. Uncertainty language (unclear, unknown, uncertain, etc.)
   4. Missing/incomplete markers (missing, stub, not implemented, etc.)
   5. Assumption markers (assuming, assumption)

   Returns vector of gap descriptor strings (max 10), each <=80 chars.
   Returns empty vector if no gaps detected or content is nil/blank.

   Delegates to enhanced extension for deeper NLP-based analysis
   when available, falls back to heuristic extraction."
  [content]
  (if (or (nil? content) (str/blank? (str content)))
    []
    (let [content-str (str content)
          ;; L1/L2: Lightweight regex extraction
          questions    (extract-questions content-str)
          todos        (extract-todo-markers content-str)
          uncertainty  (extract-uncertainty content-str)
          missing      (extract-missing content-str)
          assumptions  (extract-assumptions content-str)
          ;; Combine and deduplicate
          all-gaps (->> (concat questions todos uncertainty missing assumptions)
                        (distinct)
                        (take max-gaps)
                        (vec))
          ;; Enhanced gap analysis via extension
          enhanced-gaps (when-let [analyze-fn (ext/get-extension :gs/detect-gaps)]
                          (try
                            (analyze-fn content-str all-gaps)
                            (catch Exception _ nil)))]
      ;; Enhanced result overrides heuristic if available
      (or enhanced-gaps all-gaps))))
