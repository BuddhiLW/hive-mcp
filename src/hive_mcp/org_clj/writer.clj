(ns hive-mcp.org-clj.writer
  "Serializer for org-mode documents - converts EDN back to org text.
   
   Handles:
   - File properties (#+TITLE, etc)
   - Headlines with TODO keywords, priorities, tags
   - Property drawers
   - Planning lines (CLOSED, SCHEDULED, DEADLINE)
   - Nested headline structure"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Property Drawer Writing
;; =============================================================================

(defn write-property-drawer
  "Convert a properties map to org property drawer lines.
   
   Input: {:ID \"abc-123\", :CREATED \"2025-01-01\"}
   Output: [\":PROPERTIES:\" \":ID: abc-123\" \":CREATED: 2025-01-01\" \":END:\"]"
  [properties]
  (when (and properties (seq properties))
    (concat [":PROPERTIES:"]
            (map (fn [[k v]] (str ":" (name k) ": " v)) properties)
            [":END:"])))

;; =============================================================================
;; Planning Line Writing
;; =============================================================================

(defn write-planning-line
  "Convert planning map to org planning line.
   
   Input: {:closed \"2025-01-01\", :scheduled \"2025-01-02\"}
   Output: \"CLOSED: [2025-01-01] SCHEDULED: <2025-01-02>\""
  [planning]
  (when (and planning (seq planning))
    (let [parts (keep (fn [[k v]]
                        (when v
                          (case k
                            :closed (str "CLOSED: [" v "]")
                            :scheduled (str "SCHEDULED: <" v ">")
                            :deadline (str "DEADLINE: <" v ">")
                            nil)))
                      planning)]
      (when (seq parts)
        (str/join " " parts)))))

;; =============================================================================
;; Headline Writing
;; =============================================================================

(defn write-headline-text
  "Convert headline map to headline text line.
   
   Input: {:level 2, :keyword \"TODO\", :priority \"A\", :title \"Task\", :tags [\"work\"]}
   Output: \"** TODO [#A] Task :work:\""
  [{:keys [level keyword priority title tags]}]
  (let [stars (apply str (repeat level "*"))
        parts (filter some?
                      [stars
                       keyword
                       (when priority (str "[#" priority "]"))
                       title
                       (when (seq tags)
                         (str ":" (str/join ":" tags) ":"))])]
    (str/join " " parts)))

(defn write-headline
  "Convert a headline map to org lines (including properties, planning, content).
   Does NOT include children - use write-headline-tree for nested output."
  [{:keys [planning properties content] :as headline}]
  (let [headline-line (write-headline-text headline)
        planning-line (write-planning-line planning)
        drawer-lines (write-property-drawer properties)
        content-lines (or content [])]
    (filter some?
            (concat [headline-line]
                    (when planning-line [planning-line])
                    drawer-lines
                    content-lines))))

(defn write-headline-tree
  "Recursively write a headline and all its children to org lines."
  [{:keys [children] :as headline}]
  (concat (write-headline headline)
          (when (seq children)
            (mapcat write-headline-tree children))))

;; =============================================================================
;; File Properties Writing
;; =============================================================================

(defn write-file-properties
  "Convert file properties map to org file property lines.
   
   Input: {:TITLE \"My Doc\", :STARTUP \"overview\"}
   Output: [\"#+TITLE: My Doc\" \"#+STARTUP: overview\"]"
  [properties]
  (when (and properties (seq properties))
    (map (fn [[k v]] (str "#+" (name k) ": " v)) properties)))

;; =============================================================================
;; Document Writing
;; =============================================================================

(defn write-document
  "Convert a document map to org text.
   
   Input: {:type :document, :properties {...}, :headlines [...]}
   Output: string of complete org file content"
  [{:keys [properties headlines]}]
  (let [file-props (write-file-properties properties)
        headline-lines (mapcat write-headline-tree headlines)
        all-lines (concat file-props
                          (when (seq file-props) [""]) ; blank line after props
                          headline-lines)]
    (str/join "\n" all-lines)))

(defn write-document-to-file
  "Write a document to a file."
  [doc file-path]
  (spit file-path (write-document doc)))

;; =============================================================================
;; Tests
;; =============================================================================

(deftest test-write-property-drawer
  (testing "Basic property drawer"
    (is (= [":PROPERTIES:" ":ID: abc-123" ":END:"]
           (write-property-drawer {:ID "abc-123"}))))

  (testing "Multiple properties"
    (is (= [":PROPERTIES:" ":ID: abc" ":CREATED: 2025" ":END:"]
           (write-property-drawer {:ID "abc" :CREATED "2025"}))))

  (testing "Empty properties"
    (is (nil? (write-property-drawer {})))
    (is (nil? (write-property-drawer nil)))))

(deftest test-write-planning-line
  (testing "CLOSED only"
    (is (= "CLOSED: [2025-01-01]"
           (write-planning-line {:closed "2025-01-01"}))))

  (testing "Multiple planning keywords"
    (is (= "CLOSED: [2025-01-01] SCHEDULED: <2025-01-02>"
           (write-planning-line {:closed "2025-01-01" :scheduled "2025-01-02"}))))

  (testing "Empty planning"
    (is (nil? (write-planning-line {})))
    (is (nil? (write-planning-line nil)))))

(deftest test-write-headline-text
  (testing "Simple headline"
    (is (= "* Heading"
           (write-headline-text {:level 1 :title "Heading" :tags []}))))

  (testing "With TODO keyword"
    (is (= "** TODO Task"
           (write-headline-text {:level 2 :keyword "TODO" :title "Task" :tags []}))))

  (testing "With priority"
    (is (= "** TODO [#A] Important"
           (write-headline-text {:level 2 :keyword "TODO" :priority "A" :title "Important" :tags []}))))

  (testing "With tags"
    (is (= "** DONE Task :work:urgent:"
           (write-headline-text {:level 2 :keyword "DONE" :title "Task" :tags ["work" "urgent"]}))))

  (testing "Full headline"
    (is (= "*** IN-PROGRESS [#B] Complex :project:dev:"
           (write-headline-text {:level 3 :keyword "IN-PROGRESS" :priority "B"
                                 :title "Complex" :tags ["project" "dev"]})))))

(deftest test-write-document
  (testing "Simple document"
    (let [doc {:type :document
               :properties {:TITLE "Test"}
               :headlines [{:type :headline
                            :level 1
                            :keyword nil
                            :priority nil
                            :title "Heading"
                            :tags []
                            :properties {}
                            :planning {}
                            :content []
                            :children []}]}
          result (write-document doc)]
      (is (str/includes? result "#+TITLE: Test"))
      (is (str/includes? result "* Heading")))))

(comment
  ;; Run tests
  (require '[clojure.test :refer [run-tests]])
  (run-tests)

  ;; Example usage
  (write-headline-text {:level 2 :keyword "TODO" :priority "A" :title "My task" :tags ["work"]})
  ;; => "** TODO [#A] My task :work:"

  (write-property-drawer {:ID "abc-123" :CREATED "2025-01-01"})
  ;; => [":PROPERTIES:" ":ID: abc-123" ":CREATED: 2025-01-01" ":END:"]
  )
