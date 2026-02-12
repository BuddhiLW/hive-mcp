(ns hive-mcp.agent.drone.parser
  "Robust output parsing for drone agents with multi-format extraction and confidence scoring."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn looks-like-code?
  "Check if string appears to be Clojure code."
  [s]
  (when (and s (not (str/blank? s)))
    (let [s-lower (str/lower-case s)
          clj-patterns [#"\(ns\s" #"\(defn?\s" #"\(let\s" #"\(fn\s"
                        #"\(if\s" #"\(when\s" #"\(cond\s" #"\(case\s"
                        #"\(require\s" #"\(import\s" #"\(def\s"
                        #":\w+" #"\[\s*\w" #"\{\s*:"]
          matches (count (filter #(re-find % s-lower) clj-patterns))
          paren-count (count (re-seq #"[\(\)\[\]\{\}]" s))
          char-count (count s)
          paren-density (if (pos? char-count)
                          (/ paren-count (double char-count))
                          0)]
      (or (>= matches 2)
          (and (pos? matches) (> paren-density 0.05))))))

(defn valid-clojure?
  "Check if string is valid Clojure syntax."
  [s]
  (when (and s (not (str/blank? s)))
    (try
      (binding [*read-eval* false]
        (read-string (str "(do " s ")")))
      true
      (catch Exception _
        false))))

(defn extract-indented-code
  "Extract code blocks that are indented with 4+ spaces."
  [s]
  (when (and s (not (str/blank? s)))
    (let [lines (str/split-lines s)
          groups (reduce
                  (fn [acc line]
                    (if (re-matches #"^[ ]{4,}.*" line)
                      (let [current (or (peek acc) [])
                            trimmed (str/replace line #"^[ ]{4}" "")]
                        (conj (pop (if (empty? acc) [[]] acc))
                              (conj current trimmed)))
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

(defn- extract-markdown-blocks
  "Extract code from markdown fenced blocks."
  [response]
  (let [pattern #"```(?:clojure|clj)?\s*\n([\s\S]*?)```"
        matches (re-seq pattern response)]
    (when (seq matches)
      (->> matches
           (map second)
           (map str/trim)
           (filter #(not (str/blank? %)))
           vec))))

(defn- extract-xml-blocks
  "Extract code from XML-style code tags."
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
  "Extract code from source tags."
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
  "Extract code blocks from model response using multiple strategies."
  [response]
  (when (and response (not (str/blank? response)))
    (or
     (extract-markdown-blocks response)
     (extract-xml-blocks response)
     (extract-source-blocks response)
     (let [indented (extract-indented-code response)]
       (when (seq indented) indented))
     (when (looks-like-code? response)
       [(str/trim response)]))))

(defn- extract-unified-diff
  "Extract unified diff format."
  [response]
  (let [diff-start (str/index-of response "---")
        has-plus-plus (str/includes? response "+++")
        has-hunk (re-find #"@@.*@@" response)]
    (when (and diff-start has-plus-plus has-hunk)
      (let [diff-section (subs response diff-start)
            old-lines (->> (str/split-lines diff-section)
                           (filter #(str/starts-with? % "-"))
                           (filter #(not (str/starts-with? % "---")))
                           (map #(subs % 1))
                           (str/join "\n"))
            new-lines (->> (str/split-lines diff-section)
                           (filter #(str/starts-with? % "+"))
                           (filter #(not (str/starts-with? % "+++")))
                           (map #(subs % 1))
                           (str/join "\n"))]
        {:old-content old-lines
         :new-content new-lines
         :format :unified}))))

(defn- extract-adhoc-diff
  "Extract ad-hoc diff markers models sometimes use."
  [response]
  (let [patterns [[#"(?s)\[OLD\]\s*(.*?)\[NEW\]\s*(.*?)(?:\[|$)"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :old-new})]
                  [#"(?s)BEFORE:\s*(.*?)AFTER:\s*(.*?)(?:BEFORE:|$)"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :before-after})]
                  [#"(?si)Original:\s*(.*?)Modified:\s*(.*?)(?:Original:|$)"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :original-modified})]
                  [#"(?s)old_content:\s*```[^\n]*\n(.*?)```.*?new_content:\s*```[^\n]*\n(.*?)```"
                   (fn [[_ old new]] {:old-content (str/trim old)
                                      :new-content (str/trim new)
                                      :format :propose-diff-style})]]]
    (some (fn [[pattern extractor]]
            (when-let [match (re-find pattern response)]
              (extractor match)))
          patterns)))

(defn extract-diff
  "Extract diff information from model response."
  [response]
  (when (and response (not (str/blank? response)))
    (or
     (extract-unified-diff response)
     (extract-adhoc-diff response))))

(defn- task-mentions-symbols?
  "Check if extracted code contains symbols mentioned in task."
  [task extracted]
  (when (and task (seq extracted))
    (let [task-words (set (re-seq #"[a-zA-Z][-a-zA-Z0-9_!?*]+" task))
          all-code (str/join "\n" extracted)
          code-words (set (re-seq #"[a-zA-Z][-a-zA-Z0-9_!?*]+" all-code))
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
        multi-block-bonus (if (> block-count 1) 0.05 0)
        noise-penalty (if (> block-count 5) -0.1 0)]
    (max 0.0 (min 1.0 (+ base-score multi-block-bonus noise-penalty)))))

(defn extraction-confidence
  "Calculate confidence metrics for extracted code."
  [extracted original-task]
  (let [has-code? (boolean (seq extracted))
        matches-task? (boolean (task-mentions-symbols? original-task extracted))
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

(defn recovery-action
  "Suggest recovery action based on confidence result."
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
  "Select the best code block when multiple are extracted."
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

(defn parse-drone-response
  "Parse drone response and extract structured results."
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
