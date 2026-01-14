(ns hive-mcp.org-clj.tokenizer
  "Line-level tokenization for org-mode files.
   
   Parses individual lines into structured tokens:
   - Headlines (* TODO [#A] Title :tags:)
   - Property lines (:KEY: value)
   - Planning lines (CLOSED: [...] SCHEDULED: <...>)
   - File properties (#+KEY: value)
   
   CLARITY: Single Responsibility - only line tokenization, no tree building."
  (:require [clojure.string :as str]))

;; =============================================================================
;; TODO Keywords
;; =============================================================================

(def todo-keywords
  "Set of recognized TODO keywords"
  #{"TODO" "DONE" "IN-PROGRESS" "IN-REVIEW" "CANCELLED"})

;; =============================================================================
;; Headline Parsing
;; =============================================================================

(defn headline?
  "Check if a line is an org headline (starts with one or more stars).
   
   Examples:
     (headline? \"* Heading\")     => true
     (headline? \"** TODO Task\")  => true
     (headline? \"Not a heading\") => false"
  [line]
  (and line (re-matches #"^\*+\s+.*" (str/trim line))))

(defn headline-level
  "Get the level of a headline (number of stars).
   
   Examples:
     (headline-level \"* H1\")      => 1
     (headline-level \"*** H3\")    => 3
     (headline-level \"Not a HL\")  => nil"
  [line]
  (when (headline? line)
    (count (re-find #"^\*+" (str/trim line)))))

(defn parse-headline-text
  "Parse an org headline text into its components.
   
   Input: string like '** TODO [#A] Task title :tag1:tag2:'
   Output: map with :type, :level, :keyword, :priority, :title, :tags
   
   Example:
     (parse-headline-text \"** TODO [#A] My task :work:urgent:\")
     => {:type :headline
         :level 2
         :keyword \"TODO\"
         :priority \"A\"
         :title \"My task\"
         :tags [\"work\" \"urgent\"]}"
  [line]
  (when (and line (str/starts-with? (str/trim line) "*"))
    (let [trimmed (str/trim line)
          ;; Count stars for level
          stars (re-find #"^\*+" trimmed)
          level (count stars)
          rest-line (str/trim (subs trimmed level))

          ;; Extract TODO keyword
          first-word (first (str/split rest-line #"\s+" 2))
          has-keyword? (contains? todo-keywords first-word)
          keyword (when has-keyword? first-word)
          after-keyword (if has-keyword?
                          (str/trim (subs rest-line (count first-word)))
                          rest-line)

          ;; Extract priority [#A]
          priority-match (re-find #"^\[#([A-C])\]\s*" after-keyword)
          priority (when priority-match (second priority-match))
          after-priority (if priority-match
                           (str/trim (subs after-keyword (count (first priority-match))))
                           after-keyword)

          ;; Extract tags :tag1:tag2: at end of line
          tags-match (re-find #"\s+:([\w:]+):$" after-priority)
          tags (when tags-match
                 (vec (filter (complement str/blank?)
                              (str/split (second tags-match) #":"))))
          title (if tags-match
                  (str/trim (subs after-priority 0 (- (count after-priority)
                                                      (count (first tags-match)))))
                  (str/trim after-priority))]

      {:type :headline
       :level level
       :keyword keyword
       :priority priority
       :title title
       :tags (or tags [])})))

;; =============================================================================
;; Property Drawer Parsing
;; =============================================================================

(defn parse-property-line
  "Parse a single property line like ':KEY: value'.
   
   Returns [key value] tuple or nil if line doesn't match.
   
   Examples:
     (parse-property-line \":ID: abc-123\")    => [:ID \"abc-123\"]
     (parse-property-line \"not a property\")  => nil"
  [line]
  (when-let [[_ key value] (re-matches #"^\s*:([^:]+):\s*(.*)$" (str/trim line))]
    [(keyword key) (str/trim value)]))

(defn parse-property-drawer
  "Parse property drawer lines (between :PROPERTIES: and :END:).
   
   Input: vector of strings representing lines inside the drawer
   Output: map of keyword keys to string values
   
   Example:
     (parse-property-drawer [\":ID: abc-123\" \":CREATED: 2025-01-01\"])
     => {:ID \"abc-123\", :CREATED \"2025-01-01\"}"
  [lines]
  (if (or (nil? lines) (empty? lines))
    {}
    (->> lines
         (map str/trim)
         (filter (complement str/blank?))
         (map parse-property-line)
         (filter some?)
         (into {}))))

;; =============================================================================
;; Planning Line Parsing
;; =============================================================================

(defn- extract-timestamp
  "Extract timestamp from org timestamp format.
   Handles both active <...> and inactive [...] timestamps.
   Returns the inner content without brackets."
  [s]
  (when s
    (let [s (str/trim s)]
      (cond
        (and (str/starts-with? s "[") (str/ends-with? s "]"))
        (subs s 1 (dec (count s)))

        (and (str/starts-with? s "<") (str/ends-with? s ">"))
        (subs s 1 (dec (count s)))

        :else s))))

(defn- parse-planning-keyword
  "Parse a single planning keyword and its timestamp.
   Returns [keyword timestamp-string] or nil."
  [text keyword-name]
  (let [pattern (re-pattern (str "(?i)" keyword-name ":\\s*([<\\[][^>\\]]+[>\\]])"))
        match (re-find pattern text)]
    (when match
      [(keyword (str/lower-case keyword-name))
       (extract-timestamp (second match))])))

(defn parse-planning-line
  "Parse org-mode planning line containing CLOSED, SCHEDULED, DEADLINE.
   
   Input: string like 'CLOSED: [2025-01-01 Wed] SCHEDULED: <2025-01-02>'
   Output: map with :closed, :scheduled, :deadline keys (only present if found)
   
   Example:
     (parse-planning-line \"CLOSED: [2025-01-01 Wed 14:30] SCHEDULED: <2025-01-02 Thu>\")
     => {:closed \"2025-01-01 Wed 14:30\", :scheduled \"2025-01-02 Thu\"}"
  [line]
  (if (or (nil? line) (str/blank? line))
    {}
    (let [keywords ["CLOSED" "SCHEDULED" "DEADLINE"]
          parsed (keep #(parse-planning-keyword line %) keywords)]
      (into {} parsed))))

;; =============================================================================
;; File Property Parsing
;; =============================================================================

(defn parse-file-property
  "Parse a file-level property like #+TITLE: value.
   
   Returns [KEY value] tuple or nil if line doesn't match.
   
   Examples:
     (parse-file-property \"#+TITLE: My Document\") => [:TITLE \"My Document\"]
     (parse-file-property \"Not a property\")        => nil"
  [line]
  (when-let [[_ key value] (re-matches #"^\s*#\+([A-Za-z_]+):\s*(.*)$" line)]
    [(keyword (str/upper-case key)) (str/trim value)]))
