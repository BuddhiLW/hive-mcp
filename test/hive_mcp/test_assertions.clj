(ns hive-mcp.test-assertions
  "Test assertion helpers for hive-mcp tests.

   DRY: Consolidates repeated assertion patterns.
   Reference: docs/DRY-AUDIT-REPORT.md - A4 JSON Response Parsing, A8 String Assertions"
  (:require [clojure.data.json :as json]
            [clojure.test :refer [is]]
            [clojure.string :as str]))

;; =============================================================================
;; JSON Response Assertions (A4)
;; =============================================================================

(defn parse-json-response
  "Parse JSON from an MCP tool response.

   DRY: Replaces 20+ instances of:
     (json/read-str (:text result) :key-fn keyword)

   Usage:
     (let [data (parse-json-response result)]
       (is (= \"expected\" (:field data))))"
  [result]
  (json/read-str (:text result) :key-fn keyword))

(defn assert-success
  "Assert that an MCP response is successful (not an error).

   Usage:
     (assert-success result)"
  [result]
  (is (not (:isError result)) "Expected successful response"))

(defn assert-error
  "Assert that an MCP response is an error.

   Usage:
     (assert-error result)"
  [result]
  (is (:isError result) "Expected error response"))

(defn assert-json-field
  "Assert a field value in a JSON response.

   Usage:
     (assert-json-field result :status \"ok\")"
  [result field expected]
  (let [data (parse-json-response result)]
    (is (= expected (get data field))
        (str "Expected " field " to be " expected))))

;; =============================================================================
;; String Assertions (A8)
;; =============================================================================

(defn assert-includes
  "Assert that a string includes a substring.

   DRY: Replaces (is (str/includes? s substr)) pattern.

   Usage:
     (assert-includes output \"expected text\")"
  [s substr]
  (is (str/includes? (str s) substr)
      (str "Expected string to include: " substr)))

(defn assert-not-includes
  "Assert that a string does NOT include a substring.

   Usage:
     (assert-not-includes output \"unexpected text\")"
  [s substr]
  (is (not (str/includes? (str s) substr))
      (str "Expected string to NOT include: " substr)))

(defn assert-valid-elisp
  "Assert that a string looks like valid elisp (starts with paren).

   Usage:
     (assert-valid-elisp generated-code)"
  [s]
  (is (str/starts-with? (str s) "(")
      "Expected elisp to start with (")
  (is (str/ends-with? (str/trim (str s)) ")")
      "Expected elisp to end with )"))

(defn assert-elisp-includes
  "Assert that generated elisp contains expected fragments.

   Usage:
     (assert-elisp-includes elisp \"hive-mcp-memory-add\" \"note\")"
  [elisp & fragments]
  (assert-valid-elisp elisp)
  (doseq [fragment fragments]
    (assert-includes elisp fragment)))

;; =============================================================================
;; Collection Assertions
;; =============================================================================

(defn assert-contains-key
  "Assert that a map contains a key.

   Usage:
     (assert-contains-key result :id)"
  [m k]
  (is (contains? m k)
      (str "Expected map to contain key: " k)))

(defn assert-count
  "Assert the count of a collection.

   Usage:
     (assert-count results 3)"
  [coll expected]
  (is (= expected (count coll))
      (str "Expected count " expected ", got " (count coll))))
