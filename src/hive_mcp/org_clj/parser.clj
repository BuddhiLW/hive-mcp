(ns hive-mcp.org-clj.parser
  "Parser for org-mode files including headlines, property drawers, and planning lines.
   
   Property drawers look like:
   :PROPERTIES:
   :ID: some-uuid-value
   :CREATED: 2025-01-01
   :END:
   
   Planning lines look like:
   CLOSED: [2025-01-01 Wed 14:30] SCHEDULED: <2025-01-02 Thu>
   
   Headlines look like:
   ** TODO [#A] Task title :tag1:tag2:"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing run-tests]]
            [malli.core :as m]
            [malli.error :as me]))

;; =============================================================================
;; Data Schemas (Malli)
;; =============================================================================

(def TodoKeyword
  "Valid TODO keywords"
  [:enum "TODO" "DONE" "IN-PROGRESS" "IN-REVIEW" "CANCELLED" nil])

(def Priority
  "Priority markers"
  [:enum "A" "B" "C" nil])

(def Headline
  "Schema for an org headline"
  [:map
   [:type [:= :headline]]
   [:level :int]
   [:keyword {:optional true} [:maybe :string]]
   [:priority {:optional true} [:maybe :string]]
   [:title :string]
   [:tags {:optional true} [:vector :string]]
   [:properties {:optional true} [:map-of :keyword :string]]
   [:planning {:optional true} [:map
                                [:closed {:optional true} [:maybe :string]]
                                [:scheduled {:optional true} [:maybe :string]]
                                [:deadline {:optional true} [:maybe :string]]]]
   [:content {:optional true} [:vector :any]]
   [:children {:optional true} [:vector [:ref #'Headline]]]])

(def Document
  "Schema for an org document"
  [:map
   [:type [:= :document]]
   [:properties {:optional true} [:map-of :keyword :string]]
   [:headlines [:vector Headline]]])

(defn validate-headline
  "Validate a headline map against the schema"
  [headline]
  (if (m/validate Headline headline)
    {:valid true :data headline}
    {:valid false :errors (me/humanize (m/explain Headline headline))}))

(defn validate-document
  "Validate a document map against the schema"
  [doc]
  (if (m/validate Document doc)
    {:valid true :data doc}
    {:valid false :errors (me/humanize (m/explain Document doc))}))

;; =============================================================================
;; Headline Text Parsing
;; =============================================================================

(def ^:private todo-keywords
  "Set of recognized TODO keywords"
  #{"TODO" "DONE" "IN-PROGRESS" "IN-REVIEW" "CANCELLED"})

(defn parse-headline-text
  "Parse an org headline text into its components.
   
   Input: string like '** TODO [#A] Task title :tag1:tag2:'
   Output: map with :level, :keyword, :priority, :title, :tags
   
   Example:
     (parse-headline-text \"** TODO [#A] My task :work:urgent:\")
     => {:level 2, :keyword \"TODO\", :priority \"A\", 
         :title \"My task\", :tags [\"work\" \"urgent\"]}"
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

(defn headline?
  "Check if a line is a headline"
  [line]
  (and line (re-matches #"^\*+\s+.*" (str/trim line))))

(defn headline-level
  "Get the level of a headline (number of stars)"
  [line]
  (when (headline? line)
    (count (re-find #"^\*+" (str/trim line)))))

;; =============================================================================
;; Property Drawer Parsing
;; =============================================================================

(defn- parse-property-line
  "Parse a single property line like ':KEY: value'.
   Returns [key value] or nil if the line doesn't match."
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
;; Utility Functions
;; =============================================================================

(defn extract-property-drawer
  "Extract property drawer from the beginning of org lines.
   Property drawers must appear immediately after a headline (per org-mode spec).
   Returns {:properties map, :remaining-lines lines-after-drawer}
   or {:properties nil, :remaining-lines all-lines} if no drawer at start."
  [lines]
  (let [lines-vec (vec lines)
        ;; Skip leading blank lines to find where content starts
        first-non-blank-idx (some (fn [[idx line]]
                                    (when-not (str/blank? line) idx))
                                  (map-indexed vector lines-vec))]
    (if (and first-non-blank-idx
             (re-matches #"^\s*:PROPERTIES:\s*$" (str/trim (nth lines-vec first-non-blank-idx))))
      ;; Found :PROPERTIES: at start - look for :END: (but stop at first headline)
      (let [start-idx first-non-blank-idx
            end-idx (some (fn [[idx line]]
                            (when (> idx start-idx)
                              (cond
                                ;; Found :END: - success
                                (re-matches #"^\s*:END:\s*$" (str/trim line)) idx
                                ;; Hit a headline before :END: - malformed, stop
                                (headline? line) nil
                                :else nil)))
                          (map-indexed vector lines-vec))]
        (if end-idx
          {:properties (parse-property-drawer (subvec lines-vec (inc start-idx) end-idx))
           :remaining-lines (vec (concat (subvec lines-vec 0 start-idx)
                                         (subvec lines-vec (inc end-idx))))}
          ;; No :END: found or hit a headline - no valid drawer
          {:properties nil
           :remaining-lines lines-vec}))
      ;; No :PROPERTIES: at start
      {:properties nil
       :remaining-lines lines-vec})))

(defn parse-headline-metadata
  "Parse metadata from lines immediately following a headline.
   Extracts planning info and property drawer.
   
   Input: vector of lines after the headline
   Output: {:planning {...}, :properties {...}, :remaining-lines [...]}"
  [lines]
  (if (empty? lines)
    {:planning {} :properties {} :remaining-lines []}
    (let [first-line (str/trim (first lines))
          ;; Check if first line is a planning line
          has-planning? (re-find #"(?i)^(CLOSED|SCHEDULED|DEADLINE):" first-line)
          planning (if has-planning?
                     (parse-planning-line first-line)
                     {})
          remaining-after-planning (if has-planning?
                                     (rest lines)
                                     lines)
          ;; Now check for property drawer
          {:keys [properties remaining-lines]} (extract-property-drawer remaining-after-planning)]
      {:planning planning
       :properties (or properties {})
       :remaining-lines remaining-lines})))

;; =============================================================================
;; Document-Level Parsing
;; =============================================================================

(defn- parse-file-property
  "Parse a file-level property like #+TITLE: value"
  [line]
  (when-let [[_ key value] (re-matches #"^\s*#\+([A-Za-z_]+):\s*(.*)$" line)]
    [(keyword (str/upper-case key)) (str/trim value)]))

(defn parse-file-properties
  "Extract file-level properties (#+KEY: value) from org lines.
   Returns {:properties map, :remaining-lines lines-without-properties}"
  [lines]
  (loop [remaining lines
         props {}]
    (if (empty? remaining)
      {:properties props :remaining-lines []}
      (let [line (first remaining)
            trimmed (str/trim line)]
        (if (or (str/blank? trimmed)
                (str/starts-with? trimmed "#"))
          (if-let [[k v] (parse-file-property trimmed)]
            (recur (rest remaining) (assoc props k v))
            (if (str/blank? trimmed)
              (recur (rest remaining) props)
              {:properties props :remaining-lines remaining}))
          {:properties props :remaining-lines remaining})))))

(defn- collect-headline-content
  "Collect content lines until the next headline or end of input.
   Returns {:content lines, :rest remaining-lines}"
  [lines]
  (loop [content []
         remaining lines]
    (if (empty? remaining)
      {:content content :rest []}
      (let [line (first remaining)]
        (if (headline? line)
          {:content content :rest remaining}
          (recur (conj content line) (rest remaining)))))))

(defn- parse-single-headline
  "Parse a single headline and its immediate content (not children).
   Returns headline map with :content but empty :children"
  [headline-line following-lines]
  (let [base (parse-headline-text headline-line)
        {:keys [planning properties remaining-lines]} (parse-headline-metadata following-lines)
        {:keys [content rest]} (collect-headline-content remaining-lines)]
    (-> base
        (assoc :planning planning)
        (assoc :properties properties)
        (assoc :content (vec (filter (complement str/blank?) content)))
        (assoc :children [])
        (with-meta {:remaining-lines rest}))))

(defn- build-headline-tree
  "Build a tree of headlines from a flat sequence.
   Headlines are nested based on their level."
  [headlines]
  (when (seq headlines)
    (let [first-hl (first headlines)
          first-level (:level first-hl)
          rest-hls (rest headlines)]
      (loop [result [(dissoc first-hl :children)]
             remaining rest-hls]
        (if (empty? remaining)
          result
          (let [current (first remaining)
                current-level (:level current)]
            (cond
              ;; Same level - add as sibling
              (= current-level first-level)
              (recur (conj result (dissoc current :children))
                     (rest remaining))

              ;; Higher level (more stars) - this is a child of the last item
              (> current-level first-level)
              (let [;; Find all consecutive items at higher levels (children subtree)
                    child-items (take-while #(> (:level %) first-level) remaining)
                    rest-after-children (drop (count child-items) remaining)
                    ;; Recursively build children
                    children (build-headline-tree child-items)
                    ;; Update last item with children
                    updated-last (update (peek result) :children
                                         (fnil into []) children)]
                (recur (conj (pop result) updated-last)
                       rest-after-children))

              ;; Lower level - we're done with this subtree
              :else
              result)))))))

(defn parse-headlines
  "Parse all headlines from org lines into a flat sequence.
   Each headline includes its metadata and content."
  [lines]
  (loop [remaining lines
         headlines []]
    (if (empty? remaining)
      headlines
      (let [line (first remaining)]
        (if (headline? line)
          (let [hl (parse-single-headline line (rest remaining))
                rest-lines (:remaining-lines (meta hl))]
            (recur rest-lines (conj headlines (vary-meta hl dissoc :remaining-lines))))
          (recur (rest remaining) headlines))))))

(defn parse-document
  "Parse a complete org document from text or lines.
   
   Input: string (full file content) or vector of lines
   Output: {:type :document, :properties {...}, :headlines [...]}
   
   Example:
     (parse-document \"#+TITLE: My Doc\\n* Heading 1\\n** TODO Task\")
     => {:type :document
         :properties {:TITLE \"My Doc\"}
         :headlines [{:type :headline :level 1 :title \"Heading 1\"
                      :children [{:type :headline :level 2 :keyword \"TODO\" ...}]}]}"
  [input]
  (let [lines (if (string? input)
                (str/split-lines input)
                (vec input))
        {:keys [properties remaining-lines]} (parse-file-properties lines)
        flat-headlines (parse-headlines remaining-lines)
        nested-headlines (build-headline-tree flat-headlines)]
    {:type :document
     :properties properties
     :headlines (or nested-headlines [])}))

(defn parse-file
  "Parse an org file directly from a file path.
   
   Input: file-path (string path to .org file)
   Output: Parsed document {:type :document, :properties {...}, :headlines [...]}
   
   Example:
     (parse-file \"/path/to/tasks.org\")
     => {:type :document
         :properties {:TITLE \"Tasks\"}
         :headlines [...]}
   
   Throws: Exception if file doesn't exist or can't be read"
  [file-path]
  (-> file-path
      slurp
      parse-document))

;; =============================================================================
;; Tests
;; =============================================================================

(deftest test-parse-property-line
  (testing "Basic property parsing"
    (is (= [:ID "abc-123"] (parse-property-line ":ID: abc-123")))
    (is (= [:CREATED "2025-01-01"] (parse-property-line ":CREATED: 2025-01-01"))))

  (testing "Property with spaces in value"
    (is (= [:TITLE "My Important Task"] (parse-property-line ":TITLE: My Important Task"))))

  (testing "Property with special characters"
    (is (= [:URL "https://example.com/path?q=1&r=2"]
           (parse-property-line ":URL: https://example.com/path?q=1&r=2"))))

  (testing "Property with leading/trailing whitespace"
    (is (= [:ID "trimmed"] (parse-property-line "  :ID:   trimmed  "))))

  (testing "Empty value"
    (is (= [:EMPTY ""] (parse-property-line ":EMPTY:"))))

  (testing "Invalid lines return nil"
    (is (nil? (parse-property-line "not a property")))
    (is (nil? (parse-property-line "")))))

(deftest test-parse-property-drawer
  (testing "Normal property drawer"
    (is (= {:ID "abc-123" :CREATED "2025-01-01"}
           (parse-property-drawer [":ID: abc-123" ":CREATED: 2025-01-01"]))))

  (testing "Empty drawer"
    (is (= {} (parse-property-drawer [])))
    (is (= {} (parse-property-drawer nil))))

  (testing "Drawer with blank lines"
    (is (= {:ID "abc"} (parse-property-drawer [":ID: abc" "" "  "]))))

  (testing "Multi-word values preserved"
    (is (= {:DESCRIPTION "This is a long description with spaces"}
           (parse-property-drawer [":DESCRIPTION: This is a long description with spaces"]))))

  (testing "Special characters in values"
    (is (= {:EXPR "(+ 1 2)" :PATH "/home/user/file.org"}
           (parse-property-drawer [":EXPR: (+ 1 2)" ":PATH: /home/user/file.org"])))))

(deftest test-parse-planning-line
  (testing "CLOSED only"
    (is (= {:closed "2025-01-01 Wed"}
           (parse-planning-line "CLOSED: [2025-01-01 Wed]"))))

  (testing "SCHEDULED only"
    (is (= {:scheduled "2025-01-02 Thu"}
           (parse-planning-line "SCHEDULED: <2025-01-02 Thu>"))))

  (testing "DEADLINE only"
    (is (= {:deadline "2025-01-03 Fri 10:00"}
           (parse-planning-line "DEADLINE: <2025-01-03 Fri 10:00>"))))

  (testing "Multiple planning keywords"
    (is (= {:closed "2025-01-01 Wed 14:30" :scheduled "2025-01-02 Thu"}
           (parse-planning-line "CLOSED: [2025-01-01 Wed 14:30] SCHEDULED: <2025-01-02 Thu>"))))

  (testing "All three keywords"
    (is (= {:closed "2025-01-01" :scheduled "2025-01-02" :deadline "2025-01-03"}
           (parse-planning-line "CLOSED: [2025-01-01] SCHEDULED: <2025-01-02> DEADLINE: <2025-01-03>"))))

  (testing "Empty and nil input"
    (is (= {} (parse-planning-line "")))
    (is (= {} (parse-planning-line nil)))
    (is (= {} (parse-planning-line "   ")))))

(deftest test-extract-property-drawer
  (testing "Extract drawer from lines"
    (let [lines [":PROPERTIES:" ":ID: test-id" ":END:" "Content here"]
          result (extract-property-drawer lines)]
      (is (= {:ID "test-id"} (:properties result)))
      (is (= ["Content here"] (:remaining-lines result)))))

  (testing "No drawer present"
    (let [lines ["Just some content" "More content"]
          result (extract-property-drawer lines)]
      (is (nil? (:properties result)))
      (is (= lines (:remaining-lines result)))))

  (testing "Unclosed drawer"
    (let [lines [":PROPERTIES:" ":ID: test-id" "Missing END"]
          result (extract-property-drawer lines)]
      (is (nil? (:properties result))))))

(deftest test-parse-headline-metadata
  (testing "Planning and properties"
    (let [lines ["CLOSED: [2025-01-01]"
                 ":PROPERTIES:"
                 ":ID: headline-1"
                 ":END:"
                 "Body content"]
          result (parse-headline-metadata lines)]
      (is (= {:closed "2025-01-01"} (:planning result)))
      (is (= {:ID "headline-1"} (:properties result)))
      (is (= ["Body content"] (:remaining-lines result)))))

  (testing "Properties only (no planning)"
    (let [lines [":PROPERTIES:"
                 ":ID: headline-2"
                 ":END:"
                 "Body"]
          result (parse-headline-metadata lines)]
      (is (= {} (:planning result)))
      (is (= {:ID "headline-2"} (:properties result)))))

  (testing "Planning only (no properties)"
    (let [lines ["SCHEDULED: <2025-01-02>"
                 "Body content"]
          result (parse-headline-metadata lines)]
      (is (= {:scheduled "2025-01-02"} (:planning result)))
      (is (= {} (:properties result)))))

  (testing "Empty input"
    (let [result (parse-headline-metadata [])]
      (is (= {} (:planning result)))
      (is (= {} (:properties result)))
      (is (= [] (:remaining-lines result))))))

(deftest test-parse-headline-text
  (testing "Basic headline with TODO"
    (is (= {:type :headline
            :level 2
            :keyword "TODO"
            :priority nil
            :title "Task title"
            :tags []}
           (parse-headline-text "** TODO Task title"))))

  (testing "Headline with priority"
    (is (= {:type :headline
            :level 1
            :keyword "TODO"
            :priority "A"
            :title "Important task"
            :tags []}
           (parse-headline-text "* TODO [#A] Important task"))))

  (testing "Headline with tags"
    (is (= {:type :headline
            :level 2
            :keyword "DONE"
            :priority nil
            :title "Completed task"
            :tags ["work" "urgent"]}
           (parse-headline-text "** DONE Completed task :work:urgent:"))))

  (testing "Full headline with everything"
    (is (= {:type :headline
            :level 3
            :keyword "IN-PROGRESS"
            :priority "B"
            :title "Complex task"
            :tags ["project" "dev"]}
           (parse-headline-text "*** IN-PROGRESS [#B] Complex task :project:dev:"))))

  (testing "Headline without TODO keyword"
    (is (= {:type :headline
            :level 1
            :keyword nil
            :priority nil
            :title "Just a heading"
            :tags []}
           (parse-headline-text "* Just a heading"))))

  (testing "Headline with only tags"
    (is (= {:type :headline
            :level 2
            :keyword nil
            :priority nil
            :title "Tagged heading"
            :tags ["tag1" "tag2" "tag3"]}
           (parse-headline-text "** Tagged heading :tag1:tag2:tag3:"))))

  (testing "Non-headline returns nil"
    (is (nil? (parse-headline-text "Not a headline")))
    (is (nil? (parse-headline-text "")))
    (is (nil? (parse-headline-text nil)))))

(comment
  ;; Run tests
  (run-tests)

  ;; Example usage
  (parse-property-drawer [":ID: abc-123" ":CREATED: 2025-01-01"])
  ;; => {:ID "abc-123", :CREATED "2025-01-01"}

  (parse-planning-line "CLOSED: [2025-01-01 Wed 14:30] SCHEDULED: <2025-01-02 Thu>")
  ;; => {:closed "2025-01-01 Wed 14:30", :scheduled "2025-01-02 Thu"}

  (parse-headline-metadata ["CLOSED: [2025-01-01]"
                            ":PROPERTIES:"
                            ":ID: task-1"
                            ":CATEGORY: work"
                            ":END:"
                            "This is the body"])
  ;; => {:planning {:closed "2025-01-01"},
  ;;     :properties {:ID "task-1", :CATEGORY "work"},
  ;;     :remaining-lines ["This is the body"]}
  )
