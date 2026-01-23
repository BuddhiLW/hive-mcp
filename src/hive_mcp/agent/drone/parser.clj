(ns hive-mcp.agent.drone.parser
  "Robust output parsing for drone agents.

   Free-tier models (OpenRouter) often return messy, inconsistent output.
   This module provides multi-format extraction and confidence scoring
   to reliably extract code changes from varied response formats.

   Key functions:
   - extract-code-blocks: Multi-format code extraction
   - extract-diff: Unified/ad-hoc diff parsing
   - extraction-confidence: Quality scoring
   - recovery-action: Suggest next steps based on confidence

   SOLID-S: Single responsibility - parsing only.
   CLARITY-Y: Yield safe failure with confidence-based recovery."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defn looks-like-code?
  "Check if string appears to be Clojure code.

   Looks for common Clojure patterns:
   - Balanced parens with Clojure forms (defn, ns, let, etc)
   - Keywords, vectors, maps
   - High paren density

   Returns true if string likely contains code."
  [s]
  (when (and s (not (str/blank? s)))
    (let [s-lower (str/lower-case s)
          ;; Clojure keywords/forms
          clj-patterns [#"\(ns\s" #"\(defn?\s" #"\(let\s" #"\(fn\s"
                        #"\(if\s" #"\(when\s" #"\(cond\s" #"\(case\s"
                        #"\(require\s" #"\(import\s" #"\(def\s"
                        #":\w+" #"\[\s*\w" #"\{\s*:"]
          matches (count (filter #(re-find % s-lower) clj-patterns))
          ;; Paren density check
          paren-count (count (re-seq #"[\(\)\[\]\{\}]" s))
          char-count (count s)
          paren-density (if (pos? char-count)
                          (/ paren-count (double char-count))
                          0)]
      (or (>= matches 2)
          (and (pos? matches) (> paren-density 0.05))))))

(defn valid-clojure?
  "Check if string is valid Clojure syntax.

   Attempts to read the string as Clojure code.
   Returns true if parsing succeeds, false otherwise."
  [s]
  (when (and s (not (str/blank? s)))
    (try
      (binding [*read-eval* false]
        (read-string (str "(do " s ")")))
      true
      (catch Exception _
        false))))

(defn extract-indented-code
  "Extract code blocks that are indented with 4+ spaces.

   Common in plain-text model responses where code is
   indented but not fenced.

   Returns vector of extracted code strings."
  [s]
  (when (and s (not (str/blank? s)))
    (let [lines (str/split-lines s)
          ;; Group consecutive indented lines
          groups (reduce
                  (fn [acc line]
                    (if (re-matches #"^[ ]{4,}.*" line)
                       ;; Indented line - add to current group
                      (let [current (or (peek acc) [])
                            trimmed (str/replace line #"^[ ]{4}" "")]
                        (conj (pop (if (empty? acc) [[]] acc))
                              (conj current trimmed)))
                       ;; Non-indented - start new group if current has content
                      (if (seq (peek acc))
                        (conj acc [])
                        acc)))
                  [[]]
                  lines)]
      (->> groups
           (map #(str/join "\n" %))
           (filter #(not (str/blank? %)))
           (filter looks-like-code?)
           vec))))

;;; ============================================================
;;; Code Block Extraction
;;; ============================================================

(defn- extract-markdown-blocks
  "Extract code from markdown fenced blocks (```clojure or ```clj)."
  [response]
  (let [;; Match ```clojure, ```clj, or plain ``` blocks
        pattern #"```(?:clojure|clj)?\s*\n([\s\S]*?)```"
        matches (re-seq pattern response)]
    (when (seq matches)
      (->> matches
           (map second)
           (map str/trim)
           (filter #(not (str/blank? %)))
           vec))))

(defn- extract-xml-blocks
  "Extract code from XML-style <code>...</code> tags."
  [response]
  (let [pattern #"<code[^>]*>([\s\S]*?)</code>"
        matches (re-seq pattern response)]
    (when (seq matches)
      (->> matches
           (map second)
           (map str/trim)
           (filter #(not (str/blank? %)))
           vec))))

(defn- extract-source-blocks
  "Extract code from <source>...</source> tags (some models use this)."
  [response]
  (let [pattern #"<source[^>]*>([\s\S]*?)</source>"
        matches (re-seq pattern response)]
    (when (seq matches)
      (->> matches
           (map second)
           (map str/trim)
           (filter #(not (str/blank? %)))
           vec))))

(defn extract-code-blocks
  "Extract code blocks from model response using multiple strategies.

   Tries in order:
   1. Markdown fenced blocks (```clojure)
   2. XML-style <code>...</code>
   3. <source>...</source> tags
   4. Indented blocks (4+ spaces)
   5. Fallback: entire response if it looks like code

   Returns vector of extracted code strings, or nil if none found."
  [response]
  (when (and response (not (str/blank? response)))
    (or
     ;; Try markdown code blocks first (most common)
     (extract-markdown-blocks response)
     ;; Try XML-style tags
     (extract-xml-blocks response)
     ;; Try source tags
     (extract-source-blocks response)
     ;; Try indented blocks
     (let [indented (extract-indented-code response)]
       (when (seq indented) indented))
     ;; Fallback: entire response if it looks like code
     (when (looks-like-code? response)
       [(str/trim response)]))))

;;; ============================================================
;;; Diff Extraction
;;; ============================================================

(defn- extract-unified-diff
  "Extract unified diff format (--- +++ @@ style)."
  [response]
  (let [;; Look for unified diff pattern
        diff-start (str/index-of response "---")
        has-plus-plus (str/includes? response "+++")
        has-hunk (re-find #"@@.*@@" response)]
    (when (and diff-start has-plus-plus has-hunk)
      (let [diff-section (subs response diff-start)
            ;; Extract old lines (starting with -)
            old-lines (->> (str/split-lines diff-section)
                           (filter #(str/starts-with? % "-"))
                           (filter #(not (str/starts-with? % "---")))
                           (map #(subs % 1))
                           (str/join "\n"))
            ;; Extract new lines (starting with +)
            new-lines (->> (str/split-lines diff-section)
                           (filter #(str/starts-with? % "+"))
                           (filter #(not (str/starts-with? % "+++")))
                           (map #(subs % 1))
                           (str/join "\n"))]
        {:old-content old-lines
         :new-content new-lines
         :format :unified}))))

(defn- extract-adhoc-diff
  "Extract ad-hoc diff markers models sometimes use.

   Handles patterns like:
   - [OLD] ... [NEW] ...
   - BEFORE: ... AFTER: ...
   - Original: ... Modified: ..."
  [response]
  (let [patterns [;; [OLD] ... [NEW] pattern
                  [#"(?s)\[OLD\]\s*(.*?)\[NEW\]\s*(.*?)(?:\[|$)"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :old-new})]
                  ;; BEFORE: ... AFTER: pattern
                  [#"(?s)BEFORE:\s*(.*?)AFTER:\s*(.*?)(?:BEFORE:|$)"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :before-after})]
                  ;; Original: ... Modified: pattern
                  [#"(?si)Original:\s*(.*?)Modified:\s*(.*?)(?:Original:|$)"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :original-modified})]
                  ;; old_content: ... new_content: pattern (from propose_diff)
                  [#"(?s)old_content:\s*```[^\n]*\n(.*?)```.*?new_content:\s*```[^\n]*\n(.*?)```"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :propose-diff-style})]]]
    (some (fn [[pattern extractor]]
            (when-let [match (re-find pattern response)]
              (extractor match)))
          patterns)))

(defn extract-diff
  "Extract diff information from model response.

   Handles multiple diff formats:
   - Unified diff (--- +++ @@ style)
   - Ad-hoc markers ([OLD]/[NEW], BEFORE:/AFTER:, etc)

   Returns {:old-content :new-content :format} or nil if no diff found."
  [response]
  (when (and response (not (str/blank? response)))
    (or
     (extract-unified-diff response)
     (extract-adhoc-diff response))))

;;; ============================================================
;;; Confidence Scoring
;;; ============================================================

(defn- task-mentions-symbols?
  "Check if extracted code contains symbols mentioned in task.

   Extracts potential identifiers from task and checks if they
   appear in the extracted code."
  [task extracted]
  (when (and task (seq extracted))
    (let [;; Extract potential symbols from task (words, kebab-case, etc)
          task-words (set (re-seq #"[a-zA-Z][-a-zA-Z0-9_!?*]+" task))
          ;; Combine all extracted code
          all-code (str/join "\n" extracted)
          ;; Extract symbols from code
          code-words (set (re-seq #"[a-zA-Z][-a-zA-Z0-9_!?*]+" all-code))
          ;; Find overlap
          overlap (set/intersection task-words code-words)]
      (and (seq overlap)
           (> (count overlap) 0)))))

(defn- calculate-confidence-score
  "Calculate numeric confidence score (0.0-1.0)."
  [has-code? matches-task? syntax-valid? block-count]
  (let [base-score (cond
                     (not has-code?) 0.0
                     (and syntax-valid? matches-task?) 0.9
                     syntax-valid? 0.7
                     matches-task? 0.5
                     :else 0.3)
        ;; Bonus for multiple blocks (might be more complete)
        multi-block-bonus (if (> block-count 1) 0.05 0)
        ;; Penalty for too many blocks (might be noisy)
        noise-penalty (if (> block-count 5) -0.1 0)]
    (max 0.0 (min 1.0 (+ base-score multi-block-bonus noise-penalty)))))

(defn extraction-confidence
  "Calculate confidence metrics for extracted code.

   Arguments:
     extracted     - Vector of extracted code strings
     original-task - Original task description

   Returns map with:
     :has-code?     - Boolean, whether any code was extracted
     :matches-task? - Boolean, whether code mentions task symbols
     :syntax-valid? - Boolean, whether code parses as valid Clojure
     :block-count   - Number of code blocks extracted
     :confidence    - Float 0.0-1.0, overall confidence score"
  [extracted original-task]
  (let [has-code? (boolean (seq extracted))
        matches-task? (boolean (task-mentions-symbols? original-task extracted))
        ;; Check if at least one block is valid
        syntax-valid? (boolean (and has-code?
                                    (some valid-clojure? extracted)))
        block-count (count (or extracted []))
        confidence (calculate-confidence-score has-code? matches-task?
                                               syntax-valid? block-count)]
    {:has-code? has-code?
     :matches-task? matches-task?
     :syntax-valid? syntax-valid?
     :block-count block-count
     :confidence confidence}))

;;; ============================================================
;;; Recovery Strategies
;;; ============================================================

(defn recovery-action
  "Suggest recovery action based on confidence result.

   Arguments:
     confidence-result - Map from extraction-confidence

   Returns keyword indicating suggested action:
     :accept           - Confidence high enough to proceed
     :pick-best        - Multiple options, pick best match
     :warn-low         - Proceed with warning
     :ask-clarify      - Ask model for clarification
     :report-failure   - No code found, report failure"
  [{:keys [has-code? confidence block-count] :as _result}]
  (cond
    (not has-code?)
    :report-failure

    (>= confidence 0.7)
    :accept

    (and (>= confidence 0.5) (> block-count 1))
    :pick-best

    (>= confidence 0.3)
    :warn-low

    :else
    :ask-clarify))

(defn select-best-block
  "Select the best code block when multiple are extracted.

   Prioritizes:
   1. Syntactically valid blocks
   2. Blocks mentioning task symbols
   3. Longer blocks (likely more complete)"
  [blocks task]
  (when (seq blocks)
    (let [scored (for [block blocks]
                   {:block block
                    :valid? (valid-clojure? block)
                    :matches? (task-mentions-symbols? task [block])
                    :length (count block)})
          sorted (->> scored
                      (sort-by (juxt :valid? :matches? :length))
                      reverse)]
      (:block (first sorted)))))

;;; ============================================================
;;; High-Level API
;;; ============================================================

(defn parse-drone-response
  "Parse drone response and extract structured results.

   Arguments:
     response - Raw model response string
     task     - Original task description

   Returns map with:
     :code-blocks   - Vector of extracted code strings
     :diff          - Extracted diff info or nil
     :confidence    - Confidence metrics map
     :action        - Suggested recovery action
     :best-block    - Best code block if multiple found"
  [response task]
  (let [code-blocks (extract-code-blocks response)
        diff-info (extract-diff response)
        confidence (extraction-confidence code-blocks task)
        action (recovery-action confidence)
        best-block (when (> (:block-count confidence) 1)
                     (select-best-block code-blocks task))]
    (log/debug "Parsed drone response"
               {:blocks (count (or code-blocks []))
                :has-diff (boolean diff-info)
                :confidence (:confidence confidence)
                :action action})
    {:code-blocks code-blocks
     :diff diff-info
     :confidence confidence
     :action action
     :best-block best-block
     :raw-response response}))
