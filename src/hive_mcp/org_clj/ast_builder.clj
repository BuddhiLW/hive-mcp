(ns hive-mcp.org-clj.ast-builder
  "AST construction for org-mode documents.
   
   Builds hierarchical document structures from tokenized lines:
   - Extracts property drawers from line sequences
   - Parses headline metadata (planning, properties)
   - Constructs headline trees from flat headline sequences
   
   CLARITY: Single Responsibility - only AST construction, no tokenization."
  (:require [clojure.string :as str]
            [hive-mcp.org-clj.tokenizer :as tok]))

;; =============================================================================
;; Property Drawer Extraction
;; =============================================================================

(defn extract-property-drawer
  "Extract property drawer from the beginning of org lines.
   Property drawers must appear immediately after a headline (per org-mode spec).
   
   Returns {:properties map, :remaining-lines lines-after-drawer}
   or {:properties nil, :remaining-lines all-lines} if no drawer at start.
   
   Example:
     (extract-property-drawer [\":PROPERTIES:\" \":ID: 123\" \":END:\" \"content\"])
     => {:properties {:ID \"123\"}, :remaining-lines [\"content\"]}"
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
                                (tok/headline? line) nil
                                :else nil)))
                          (map-indexed vector lines-vec))]
        (if end-idx
          {:properties (tok/parse-property-drawer (subvec lines-vec (inc start-idx) end-idx))
           :remaining-lines (vec (concat (subvec lines-vec 0 start-idx)
                                         (subvec lines-vec (inc end-idx))))}
          ;; No :END: found or hit a headline - no valid drawer
          {:properties nil
           :remaining-lines lines-vec}))
      ;; No :PROPERTIES: at start
      {:properties nil
       :remaining-lines lines-vec})))

;; =============================================================================
;; Headline Metadata Parsing
;; =============================================================================

(defn parse-headline-metadata
  "Parse metadata from lines immediately following a headline.
   Extracts planning info and property drawer.
   
   Input: vector of lines after the headline
   Output: {:planning {...}, :properties {...}, :remaining-lines [...]}
   
   Example:
     (parse-headline-metadata [\"CLOSED: [2025-01-01]\"
                               \":PROPERTIES:\" \":ID: task-1\" \":END:\"
                               \"Body content\"])
     => {:planning {:closed \"2025-01-01\"}
         :properties {:ID \"task-1\"}
         :remaining-lines [\"Body content\"]}"
  [lines]
  (if (empty? lines)
    {:planning {} :properties {} :remaining-lines []}
    (let [first-line (str/trim (first lines))
          ;; Check if first line is a planning line
          has-planning? (re-find #"(?i)^(CLOSED|SCHEDULED|DEADLINE):" first-line)
          planning (if has-planning?
                     (tok/parse-planning-line first-line)
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
;; File Properties Parsing
;; =============================================================================

(defn parse-file-properties
  "Extract file-level properties (#+KEY: value) from org lines.
   
   Returns {:properties map, :remaining-lines lines-without-properties}
   
   Example:
     (parse-file-properties [\"#+TITLE: My Doc\" \"#+AUTHOR: Me\" \"* Heading\"])
     => {:properties {:TITLE \"My Doc\" :AUTHOR \"Me\"}
         :remaining-lines [\"* Heading\"]}"
  [lines]
  (loop [remaining lines
         props {}]
    (if (empty? remaining)
      {:properties props :remaining-lines []}
      (let [line (first remaining)
            trimmed (str/trim line)]
        (if (or (str/blank? trimmed)
                (str/starts-with? trimmed "#"))
          (if-let [[k v] (tok/parse-file-property trimmed)]
            (recur (rest remaining) (assoc props k v))
            (if (str/blank? trimmed)
              (recur (rest remaining) props)
              {:properties props :remaining-lines remaining}))
          {:properties props :remaining-lines remaining})))))

;; =============================================================================
;; Content Collection
;; =============================================================================

(defn- collect-headline-content
  "Collect content lines until the next headline or end of input.
   Returns {:content lines, :rest remaining-lines}"
  [lines]
  (loop [content []
         remaining lines]
    (if (empty? remaining)
      {:content content :rest []}
      (let [line (first remaining)]
        (if (tok/headline? line)
          {:content content :rest remaining}
          (recur (conj content line) (rest remaining)))))))

;; =============================================================================
;; Single Headline Parsing
;; =============================================================================

(defn- parse-single-headline
  "Parse a single headline and its immediate content (not children).
   Returns headline map with :content but empty :children.
   
   Attaches remaining lines as metadata for the caller to continue processing."
  [headline-line following-lines]
  (let [base (tok/parse-headline-text headline-line)
        {:keys [planning properties remaining-lines]} (parse-headline-metadata following-lines)
        {:keys [content rest]} (collect-headline-content remaining-lines)]
    (-> base
        (assoc :planning planning)
        (assoc :properties properties)
        (assoc :content (vec (filter (complement str/blank?) content)))
        (assoc :children [])
        (with-meta {:remaining-lines rest}))))

;; =============================================================================
;; Headline Tree Building
;; =============================================================================

(defn- build-headline-tree
  "Build a tree of headlines from a flat sequence.
   Headlines are nested based on their level.
   
   Example:
     Level 1, Level 2, Level 2, Level 1
     =>
     [{:level 1 :children [{:level 2} {:level 2}]}
      {:level 1 :children []}]"
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

;; =============================================================================
;; Headline Sequence Parsing
;; =============================================================================

(defn parse-headlines
  "Parse all headlines from org lines into a flat sequence.
   Each headline includes its metadata and content."
  [lines]
  (loop [remaining lines
         headlines []]
    (if (empty? remaining)
      headlines
      (let [line (first remaining)]
        (if (tok/headline? line)
          (let [hl (parse-single-headline line (rest remaining))
                rest-lines (:remaining-lines (meta hl))]
            (recur rest-lines (conj headlines (vary-meta hl dissoc :remaining-lines))))
          (recur (rest remaining) headlines))))))

(defn build-document-tree
  "Build a complete document tree from file properties and flat headlines.
   
   Input: {:properties map} and flat-headlines sequence
   Output: Document map with nested headline tree"
  [file-properties flat-headlines]
  {:type :document
   :properties file-properties
   :headlines (or (build-headline-tree flat-headlines) [])})
