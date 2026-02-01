(ns hive-mcp.plan.parser
  "Parser for extracting structured plan data from memory entries.

   Supports two parsing modes:
   1. EDN mode - Extract ```edn blocks from memory content
   2. Markdown mode - Parse ## headers as steps with [depends: ...] syntax

   Primary functions:
   - parse-plan: Auto-detect mode and parse content
   - parse-edn-plan: Parse EDN blocks specifically
   - parse-markdown-plan: Parse markdown headers as steps

   CLARITY: Facade pattern - coordinates EDN and markdown parsing strategies."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [hive-mcp.plan.schema :as schema]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; EDN Block Extraction
;; =============================================================================

(def ^:private edn-block-pattern
  "Regex pattern to match ```edn ... ``` code blocks"
  #"(?s)```edn\s*\n(.*?)\n```")

(defn- extract-edn-blocks
  "Extract all EDN code blocks from content.

   Returns: vector of EDN strings found in ```edn ... ``` blocks"
  [content]
  (->> (re-seq edn-block-pattern content)
       (mapv second)))

(defn- looks-like-edn-plan?
  "Check if content looks like raw EDN with plan structure.

   Detects patterns like {:plan/steps or {:steps at the start of content."
  [content]
  (boolean
   (and (string? content)
        (let [trimmed (str/trim content)]
          (and (str/starts-with? trimmed "{")
               (or (str/includes? trimmed ":plan/steps")
                   (str/includes? trimmed ":steps")))))))

(defn contains-edn-block?
  "Check if content contains any ```edn ... ``` blocks."
  [content]
  (boolean (re-find edn-block-pattern content)))

(defn contains-edn-plan?
  "Check if content contains EDN plan (raw or in blocks).

   Detects both:
   - ```edn ... ``` code blocks
   - Raw EDN content with :plan/steps or :steps"
  [content]
  (or (contains-edn-block? content)
      (looks-like-edn-plan? content)))

(defn- try-parse-edn
  "Safely attempt to parse EDN string.

   Returns: {:success true :data ...} or {:success false :error ...}"
  [edn-str]
  (try
    {:success true :data (edn/read-string edn-str)}
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn- get-steps-key
  "Get the steps from EDN data, checking both namespaced and non-namespaced keys."
  [data]
  (or (:steps data)
      (:plan/steps data)))

(defn- is-plan-edn?
  "Check if parsed EDN looks like a plan (has :steps or :plan/steps key)."
  [data]
  (and (map? data)
       (let [steps (get-steps-key data)]
         (and steps (vector? steps)))))

(defn- strip-namespace
  "Remove namespace from a keyword if present."
  [k]
  (if (keyword? k)
    (keyword (name k))
    k))

(defn- normalize-edn-step
  "Normalize an EDN step map, converting namespaced keys to non-namespaced.

   :step/id -> :id, :step/title -> :title, etc."
  [step]
  (reduce-kv (fn [m k v]
               (assoc m (strip-namespace k) v))
             {}
             step))

(defn- normalize-edn-plan
  "Normalize an EDN plan map, converting namespaced keys to non-namespaced.

   :plan/title -> :title, :plan/steps -> :steps, etc.
   Also normalizes nested step maps."
  [data]
  (let [base-map (reduce-kv (fn [m k v]
                              (assoc m (strip-namespace k) v))
                            {}
                            data)
        steps (get-steps-key data)]
    (if steps
      (assoc base-map :steps (mapv normalize-edn-step steps))
      base-map)))

;; =============================================================================
;; EDN Plan Parsing
;; =============================================================================

(defn- finalize-edn-plan
  "Normalize and validate parsed EDN plan data.

   Applies defaults, normalization, and schema validation.
   Returns {:success true :plan ...} or {:success false :error ...}"
  [plan-data]
  (let [;; Normalize namespaced keys to non-namespaced
        normalized-edn (normalize-edn-plan plan-data)
        ;; Generate ID and title if not provided
        plan-with-defaults (cond-> normalized-edn
                             (not (:id normalized-edn))
                             (assoc :id (str "plan-" (System/currentTimeMillis)))

                             (not (:title normalized-edn))
                             (assoc :title "Untitled Plan"))
        normalized (schema/normalize-plan
                    (assoc plan-with-defaults :source-format :edn))]
    (if (schema/valid-plan? normalized)
      {:success true :plan normalized}
      {:success false
       :error "Plan failed schema validation"
       :details (schema/explain-plan normalized)})))

(defn parse-edn-plan
  "Parse plan from EDN content or EDN blocks.

   Tries two strategies:
   1. Parse content directly as EDN (for raw EDN plans)
   2. Find ```edn ... ``` blocks and parse those

   Supports both namespaced (:plan/steps, :step/id) and plain keys.

   Args:
   - content: String containing EDN (raw or in code blocks)

   Returns:
   - {:success true :plan ...} with normalized plan
   - {:success false :error ...} if no valid plan found"
  [content]
  ;; Strategy 1: Try parsing content directly as EDN
  (let [direct-parse (try-parse-edn content)]
    (if (and (:success direct-parse) (is-plan-edn? (:data direct-parse)))
      (finalize-edn-plan (:data direct-parse))
      ;; Strategy 2: Extract from ```edn blocks
      (let [blocks (extract-edn-blocks content)]
        (if (empty? blocks)
          {:success false :error "No EDN plan found (tried direct parse and ```edn blocks)"}
          (let [results (for [block blocks
                              :let [parsed (try-parse-edn block)]
                              :when (:success parsed)
                              :when (is-plan-edn? (:data parsed))]
                          (:data parsed))
                plan-data (first results)]
            (if plan-data
              (finalize-edn-plan plan-data)
              {:success false :error "No valid plan structure found in EDN blocks"})))))))

;; =============================================================================
;; Markdown Plan Parsing
;; =============================================================================

(def ^:private header-pattern
  "Regex pattern to match markdown headers"
  #"^(#{1,6})\s+(.+)$")

(def ^:private depends-pattern
  "Regex pattern to extract [depends: step-1, step-2] syntax"
  #"\[depends:\s*([^\]]+)\]")

(def ^:private priority-pattern
  "Regex pattern to extract [priority: high] syntax"
  #"\[priority:\s*(high|medium|low)\]")

(def ^:private id-pattern
  "Regex pattern to extract [id: step-1] syntax"
  #"\[id:\s*([^\]]+)\]")

(def ^:private estimate-pattern
  "Regex pattern to extract [estimate: small] syntax"
  #"\[estimate:\s*(small|medium|large)\]")

(def ^:private files-pattern
  "Regex pattern to extract [files: path1, path2] syntax"
  #"\[files:\s*([^\]]+)\]")

(defn- parse-header-line
  "Parse a markdown header line into level and text.

   Returns: {:level n :text \"...\"} or nil if not a header"
  [line]
  (when-let [match (re-matches header-pattern (str/trim line))]
    {:level (count (second match))
     :text (nth match 2)}))

(defn- extract-depends
  "Extract dependency list from text.

   [depends: step-1, step-2] -> [\"step-1\" \"step-2\"]"
  [text]
  (when-let [match (re-find depends-pattern text)]
    (->> (str/split (second match) #",")
         (mapv str/trim)
         (filterv (complement str/blank?)))))

(defn- extract-priority
  "Extract priority from text.

   [priority: high] -> :high"
  [text]
  (when-let [match (re-find priority-pattern text)]
    (keyword (second match))))

(defn- extract-id
  "Extract explicit ID from text.

   [id: step-1] -> \"step-1\""
  [text]
  (when-let [match (re-find id-pattern text)]
    (str/trim (second match))))

(defn- extract-estimate
  "Extract estimate from text.

   [estimate: small] -> :small"
  [text]
  (when-let [match (re-find estimate-pattern text)]
    (keyword (second match))))

(defn- extract-files
  "Extract file paths from text.

   [files: src/a.clj, src/b.clj] -> [\"src/a.clj\" \"src/b.clj\"]"
  [text]
  (when-let [match (re-find files-pattern text)]
    (->> (str/split (second match) #",")
         (mapv str/trim)
         (filterv (complement str/blank?)))))

(defn- clean-title
  "Remove metadata annotations from title text."
  [text]
  (-> text
      (str/replace depends-pattern "")
      (str/replace priority-pattern "")
      (str/replace id-pattern "")
      (str/replace estimate-pattern "")
      (str/replace files-pattern "")
      str/trim))

(defn- generate-step-id
  "Generate a step ID from title if not explicitly provided."
  [title index]
  (let [slug (-> title
                 str/lower-case
                 (str/replace #"[^a-z0-9]+" "-")
                 (str/replace #"^-|-$" ""))]
    (if (str/blank? slug)
      (str "step-" (inc index))
      (str slug "-" (inc index)))))

(defn- lines->sections
  "Split lines into sections by header level.

   Returns vector of {:header ... :content-lines [...] :level n}"
  [lines target-level]
  (loop [remaining lines
         current nil
         sections []]
    (if (empty? remaining)
      (if current
        (conj sections current)
        sections)
      (let [line (first remaining)
            header (parse-header-line line)]
        (cond
          ;; Found a header at target level - start new section
          (and header (= (:level header) target-level))
          (recur (rest remaining)
                 {:header (:text header) :content-lines [] :level target-level}
                 (if current (conj sections current) sections))

          ;; Inside a section - accumulate content
          current
          (recur (rest remaining)
                 (update current :content-lines conj line)
                 sections)

          ;; Not in a section yet - skip
          :else
          (recur (rest remaining) nil sections))))))

(defn- section->step
  "Convert a markdown section to a plan step."
  [section index]
  (let [title-text (:header section)
        content-lines (:content-lines section)
        content-str (str/join "\n" content-lines)]
    (schema/normalize-step
     {:id (or (extract-id title-text)
              (generate-step-id title-text index))
      :title (clean-title title-text)
      :description (when-not (str/blank? content-str) content-str)
      :depends-on (or (extract-depends title-text) [])
      :priority (or (extract-priority title-text) :medium)
      :estimate (or (extract-estimate title-text) :medium)
      :files (or (extract-files title-text) [])})))

(defn- extract-plan-title
  "Extract plan title from first # header or return default."
  [lines]
  (some (fn [line]
          (when-let [header (parse-header-line line)]
            (when (= 1 (:level header))
              (:text header))))
        lines))

(defn parse-markdown-plan
  "Parse plan from markdown content.

   ## headers become steps (level 2)
   ### headers become substeps (level 3) - nested under parent
   [depends: step-1] syntax for dependencies
   [priority: high] syntax for priority
   [id: custom-id] syntax for explicit step IDs

   Args:
   - content: Markdown string

   Returns:
   - {:success true :plan ...} with normalized plan
   - {:success false :error ...} if no steps found"
  [content]
  (let [lines (str/split-lines content)
        title (or (extract-plan-title lines) "Untitled Plan")
        sections (lines->sections lines 2)]
    (if (empty? sections)
      {:success false :error "No ## headers found in content"}
      (let [steps (vec (map-indexed (fn [idx section]
                                      (section->step section idx))
                                    sections))
            plan (schema/normalize-plan
                  {:id (str "plan-" (System/currentTimeMillis))
                   :title (clean-title title)
                   :steps steps
                   :source-format :markdown})]
        (if (schema/valid-plan? plan)
          {:success true :plan plan}
          {:success false
           :error "Plan failed schema validation"
           :details (schema/explain-plan plan)})))))

;; =============================================================================
;; Unified Plan Parsing
;; =============================================================================

(defn parse-plan
  "Parse plan from memory content, auto-detecting format.

   If content contains ```edn blocks, attempts EDN parsing first.
   Falls back to markdown parsing if EDN parsing fails or no EDN blocks.

   Args:
   - content: Memory entry content (string)
   - opts: Optional map with:
     - :prefer-format - :edn or :markdown to force format
     - :memory-id - ID to attach to resulting plan

   Returns:
   - {:success true :plan ...} with normalized, validated plan
   - {:success false :error ...} with error details"
  ([content] (parse-plan content {}))
  ([content {:keys [prefer-format memory-id]}]
   (let [result (cond
                  ;; Forced format
                  (= prefer-format :edn)
                  (parse-edn-plan content)

                  (= prefer-format :markdown)
                  (parse-markdown-plan content)

                  ;; Auto-detect: try EDN first if EDN structure present
                  (contains-edn-plan? content)
                  (let [edn-result (parse-edn-plan content)]
                    (if (:success edn-result)
                      edn-result
                      (parse-markdown-plan content)))

                  ;; Default to markdown
                  :else
                  (parse-markdown-plan content))]
     ;; Attach memory-id if provided
     (if (and (:success result) memory-id)
       (update result :plan assoc :memory-id memory-id)
       result))))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn plan->task-specs
  "Convert a parsed plan to kanban task specifications.

   Useful for feeding into mcp_mem_kanban_create.

   Returns: vector of task spec maps ready for kanban creation"
  [plan]
  (mapv (fn [step]
          {:title (:title step)
           :description (:description step)
           :priority (name (:priority step))
           :tags (:tags step)
           :depends-on (:depends-on step)
           :plan-step-id (:id step)})
        (:steps plan)))

(defn validate-dependencies
  "Validate that all depends-on references exist as step IDs.

   Returns: {:valid true} or {:valid false :missing [...] :step ...}"
  [plan]
  (let [step-ids (set (map :id (:steps plan)))]
    (reduce (fn [acc step]
              (if (:valid acc)
                (let [missing (remove step-ids (:depends-on step))]
                  (if (empty? missing)
                    acc
                    {:valid false
                     :step (:id step)
                     :missing (vec missing)}))
                acc))
            {:valid true}
            (:steps plan))))
