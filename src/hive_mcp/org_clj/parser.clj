(ns hive-mcp.org-clj.parser
  "Facade for org-mode file parsing.
   
   This is the main entry point for parsing org-mode files. It orchestrates:
   - tokenizer.clj: Line-level parsing
   - ast-builder.clj: AST construction
   - schemas.clj: Validation
   
   Primary functions:
   - parse-document: Parse org text/lines into document structure
   - parse-file: Parse org file from path
   
   CLARITY: Facade pattern - thin coordination layer over specialized modules."
  (:require [clojure.string :as str]
            [hive-mcp.org-clj.tokenizer :as tok]
            [hive-mcp.org-clj.ast-builder :as ast]
            [hive-mcp.org-clj.schemas :as schemas]))

;; =============================================================================
;; Re-exports for Backward Compatibility
;; =============================================================================

;; Schema definitions
(def TodoKeyword schemas/TodoKeyword)
(def Priority schemas/Priority)
(def Headline schemas/Headline)
(def Document schemas/Document)

;; Validation functions
(def validate-headline schemas/validate-headline)
(def validate-document schemas/validate-document)

;; Tokenizer functions (commonly used directly)
(def parse-headline-text tok/parse-headline-text)
(def headline? tok/headline?)
(def headline-level tok/headline-level)
(def parse-property-drawer tok/parse-property-drawer)
(def parse-planning-line tok/parse-planning-line)

;; AST builder functions
(def extract-property-drawer ast/extract-property-drawer)
(def parse-headline-metadata ast/parse-headline-metadata)
(def parse-file-properties ast/parse-file-properties)
(def parse-headlines ast/parse-headlines)

;; =============================================================================
;; Main Entry Points
;; =============================================================================

(defn parse-document
  "Parse a complete org document from text or lines.
   
   Input: string (full file content) or vector of lines
   Output: {:type :document, :properties {...}, :headlines [...]}
   
   The document contains:
   - :type :document
   - :properties - file-level properties (#+TITLE:, etc.)
   - :headlines - nested tree of headlines
   
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
        {:keys [properties remaining-lines]} (ast/parse-file-properties lines)
        flat-headlines (ast/parse-headlines remaining-lines)]
    (ast/build-document-tree properties flat-headlines)))

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

(defn parse-and-validate
  "Parse a document and validate against schema.
   
   Returns {:valid true :data doc} or {:valid false :errors [...]}
   
   Use when you need validation guarantees on the parsed output."
  [input]
  (let [doc (parse-document input)]
    (schemas/validate-document doc)))
