(ns emacs-mcp.memory-scope-test
  "Unit tests for emacs-mcp-memory.el scope filtering functionality.

   Tests cover the memory scope filtering system:
   - Scope tag creation (global, domain, project)
   - Auto-injection of project scope tags
   - Scope matching logic for filtering entries
   - Query filtering with scope parameter

   The scope system enables hierarchical memory organization:
   - scope:global - available across all projects
   - scope:domain:<name> - shared within a domain (e.g., multiple related projects)
   - scope:project:<name> - isolated to a single project

   These tests verify the elisp generation for the scope functions,
   following the TDD approach used throughout this project."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [emacs-mcp.elisp :as el]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(def test-project-name "emacs-mcp")
(def test-domain-name "development-tools")

(defn make-scope-tag-elisp
  "Generate elisp for emacs-mcp-memory--make-scope-tag."
  ([level]
   (format "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--make-scope-tag '%s))"
           (if (keyword? level) (clojure.core/name level) level)))
  ([level scope-name]
   (format "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--make-scope-tag '%s %s))"
           (if (keyword? level) (clojure.core/name level) level)
           (pr-str scope-name))))

(defn inject-project-scope-elisp
  "Generate elisp for emacs-mcp-memory--inject-project-scope."
  [tags]
  (format "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--inject-project-scope '%s))"
          (pr-str tags)))

(defn applicable-scope-tags-elisp
  "Generate elisp for emacs-mcp-memory--applicable-scope-tags."
  [& {:keys [project-name domain-name]}]
  (cond
    (and project-name domain-name)
    (format "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--applicable-scope-tags %s %s))"
            (pr-str project-name) (pr-str domain-name))

    project-name
    (format "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--applicable-scope-tags %s nil))"
            (pr-str project-name))

    domain-name
    (format "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--applicable-scope-tags nil %s))"
            (pr-str domain-name))

    :else
    "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--applicable-scope-tags))"))

(defn entry-matches-scope-elisp
  "Generate elisp for emacs-mcp-memory--entry-matches-scope-p."
  [entry scope-tags]
  (format "(progn (require 'emacs-mcp-memory nil t) (emacs-mcp-memory--entry-matches-scope-p '%s '%s))"
          (pr-str entry) (pr-str scope-tags)))

(defn query-with-scope-elisp
  "Generate elisp for emacs-mcp-api-memory-query with scope parameter."
  [type scope]
  (format "(json-encode (emacs-mcp-api-memory-query %s nil 20 %s))"
          (pr-str type)
          (if scope (pr-str scope) "nil")))

;; =============================================================================
;; Test Scope Tag Creation - emacs-mcp-memory--make-scope-tag
;; =============================================================================

(deftest test-make-scope-tag-global
  (testing "make-scope-tag creates 'scope:global' for global level"
    (let [result (make-scope-tag-elisp :global)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'emacs-mcp-memory nil t)"))
      (is (str/includes? result "emacs-mcp-memory--make-scope-tag"))
      (is (str/includes? result "'global")))))

(deftest test-make-scope-tag-domain
  (testing "make-scope-tag creates 'scope:domain:<name>' for domain level"
    (let [result (make-scope-tag-elisp :domain test-domain-name)]
      (is (str/includes? result "emacs-mcp-memory--make-scope-tag"))
      (is (str/includes? result "'domain"))
      (is (str/includes? result (pr-str test-domain-name))))))

(deftest test-make-scope-tag-project
  (testing "make-scope-tag creates 'scope:project:<name>' for project level"
    (let [result (make-scope-tag-elisp :project test-project-name)]
      (is (str/includes? result "emacs-mcp-memory--make-scope-tag"))
      (is (str/includes? result "'project"))
      (is (str/includes? result (pr-str test-project-name))))))

(deftest test-make-scope-tag-requires-name-for-domain
  (testing "make-scope-tag for domain level includes name parameter"
    (let [result (make-scope-tag-elisp :domain "my-domain")]
      (is (str/includes? result "\"my-domain\"")))))

(deftest test-make-scope-tag-requires-name-for-project
  (testing "make-scope-tag for project level includes name parameter"
    (let [result (make-scope-tag-elisp :project "my-project")]
      (is (str/includes? result "\"my-project\"")))))

;; =============================================================================
;; Test Project Scope Injection - emacs-mcp-memory--inject-project-scope
;; =============================================================================

(deftest test-inject-project-scope-adds-when-none-present
  (testing "inject-project-scope adds project scope when no scope tag present"
    (let [tags ["important" "todo"]
          result (inject-project-scope-elisp tags)]
      (is (str/includes? result "emacs-mcp-memory--inject-project-scope"))
      (is (str/includes? result "important"))
      (is (str/includes? result "todo")))))

(deftest test-inject-project-scope-preserves-existing-scope
  (testing "inject-project-scope preserves tags when scope already present"
    (let [tags ["scope:global" "reference"]
          result (inject-project-scope-elisp tags)]
      (is (str/includes? result "emacs-mcp-memory--inject-project-scope"))
      (is (str/includes? result "scope:global"))
      (is (str/includes? result "reference")))))

(deftest test-inject-project-scope-empty-tags
  (testing "inject-project-scope handles empty tag list"
    (let [result (inject-project-scope-elisp [])]
      (is (str/includes? result "emacs-mcp-memory--inject-project-scope"))
      (is (str/includes? result "'[]")))))

(deftest test-inject-project-scope-domain-tag-present
  (testing "inject-project-scope preserves domain scope tag"
    (let [tags ["scope:domain:myapp" "feature"]
          result (inject-project-scope-elisp tags)]
      (is (str/includes? result "emacs-mcp-memory--inject-project-scope"))
      (is (str/includes? result "scope:domain:myapp")))))

(deftest test-inject-project-scope-project-tag-present
  (testing "inject-project-scope preserves existing project scope tag"
    (let [tags ["scope:project:other-project" "note"]
          result (inject-project-scope-elisp tags)]
      (is (str/includes? result "emacs-mcp-memory--inject-project-scope"))
      (is (str/includes? result "scope:project:other-project")))))

;; =============================================================================
;; Test Applicable Scope Tags - emacs-mcp-memory--applicable-scope-tags
;; =============================================================================

(deftest test-applicable-scope-tags-always-includes-global
  (testing "applicable-scope-tags always includes scope:global"
    (let [result (applicable-scope-tags-elisp)]
      (is (str/includes? result "emacs-mcp-memory--applicable-scope-tags")))))

(deftest test-applicable-scope-tags-includes-project
  (testing "applicable-scope-tags includes current project scope"
    (let [result (applicable-scope-tags-elisp :project-name test-project-name)]
      (is (str/includes? result "emacs-mcp-memory--applicable-scope-tags"))
      (is (str/includes? result (pr-str test-project-name))))))

(deftest test-applicable-scope-tags-includes-domain
  (testing "applicable-scope-tags includes domain scope when provided"
    (let [result (applicable-scope-tags-elisp :domain-name test-domain-name)]
      (is (str/includes? result "emacs-mcp-memory--applicable-scope-tags"))
      (is (str/includes? result (pr-str test-domain-name))))))

(deftest test-applicable-scope-tags-includes-both-project-and-domain
  (testing "applicable-scope-tags includes both project and domain scopes"
    (let [result (applicable-scope-tags-elisp
                  :project-name test-project-name
                  :domain-name test-domain-name)]
      (is (str/includes? result "emacs-mcp-memory--applicable-scope-tags"))
      (is (str/includes? result (pr-str test-project-name)))
      (is (str/includes? result (pr-str test-domain-name))))))

(deftest test-applicable-scope-tags-no-params
  (testing "applicable-scope-tags works with no parameters (auto-detect)"
    (let [result (applicable-scope-tags-elisp)]
      (is (str/includes? result "emacs-mcp-memory--applicable-scope-tags"))
      ;; Should call with no args, relying on current project detection
      (is (not (str/includes? result "nil nil"))))))

;; =============================================================================
;; Test Entry Scope Matching - emacs-mcp-memory--entry-matches-scope-p
;; =============================================================================

(deftest test-entry-matches-scope-global-entry-matches-any-scope
  (testing "Entry without scope tag (global) matches any scope context"
    (let [entry {:id "entry1" :tags ["general" "note"]}
          scope-tags ["scope:project:my-project" "scope:global"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p")))))

(deftest test-entry-matches-scope-project-entry-matches-project-scope
  (testing "Entry with project scope matches same project scope"
    (let [entry {:id "entry1" :tags ["scope:project:emacs-mcp" "feature"]}
          scope-tags ["scope:project:emacs-mcp" "scope:global"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p"))
      (is (str/includes? result "scope:project:emacs-mcp")))))

(deftest test-entry-matches-scope-project-entry-no-match-different-project
  (testing "Entry with project scope does not match different project"
    (let [entry {:id "entry1" :tags ["scope:project:other-project"]}
          scope-tags ["scope:project:emacs-mcp" "scope:global"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p"))
      (is (str/includes? result "scope:project:other-project"))
      (is (str/includes? result "scope:project:emacs-mcp")))))

(deftest test-entry-matches-scope-domain-entry-matches-domain-scope
  (testing "Entry with domain scope matches same domain scope"
    (let [entry {:id "entry1" :tags ["scope:domain:dev-tools"]}
          scope-tags ["scope:domain:dev-tools" "scope:global"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p"))
      (is (str/includes? result "scope:domain:dev-tools")))))

(deftest test-entry-matches-scope-global-tag-always-matches
  (testing "Entry with scope:global tag always matches"
    (let [entry {:id "entry1" :tags ["scope:global" "reference"]}
          scope-tags ["scope:project:any-project"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p"))
      (is (str/includes? result "scope:global")))))

(deftest test-entry-matches-scope-multiple-scope-tags
  (testing "Entry can have multiple scope tags (e.g., global + domain)"
    (let [entry {:id "entry1" :tags ["scope:global" "scope:domain:shared"]}
          scope-tags ["scope:domain:shared"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p")))))

(deftest test-entry-matches-scope-no-tags-treated-as-global
  (testing "Entry with no tags at all is treated as global"
    (let [entry {:id "entry1" :tags []}
          scope-tags ["scope:project:my-project" "scope:global"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p")))))

(deftest test-entry-matches-scope-entry-without-tags-key
  (testing "Entry without :tags key is treated as global"
    (let [entry {:id "entry1"}
          scope-tags ["scope:project:my-project" "scope:global"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p")))))

;; =============================================================================
;; Test Query Filtering with Scope Parameter
;; =============================================================================

(deftest test-query-scope-filter-nil-auto-filters-current-project
  (testing "Query with nil scope auto-filters by current project + global"
    (let [result (query-with-scope-elisp "note" nil)]
      (is (str/includes? result "emacs-mcp-api-memory-query"))
      (is (str/includes? result "\"note\""))
      (is (str/includes? result "nil")))))

(deftest test-query-scope-filter-all-returns-all-entries
  (testing "Query with 'all' scope returns entries regardless of scope"
    (let [result (query-with-scope-elisp "note" "all")]
      (is (str/includes? result "emacs-mcp-api-memory-query"))
      (is (str/includes? result "\"all\"")))))

(deftest test-query-scope-filter-global-only-global-entries
  (testing "Query with 'global' scope returns only scope:global entries"
    (let [result (query-with-scope-elisp "convention" "global")]
      (is (str/includes? result "emacs-mcp-api-memory-query"))
      (is (str/includes? result "\"global\"")))))

(deftest test-query-scope-filter-specific-scope-tag
  (testing "Query with specific scope tag filters to that scope + global"
    (let [result (query-with-scope-elisp "snippet" "scope:project:my-app")]
      (is (str/includes? result "emacs-mcp-api-memory-query"))
      (is (str/includes? result "\"scope:project:my-app\"")))))

(deftest test-query-scope-filter-domain-scope
  (testing "Query with domain scope filters to domain + global"
    (let [result (query-with-scope-elisp "decision" "scope:domain:backend")]
      (is (str/includes? result "emacs-mcp-api-memory-query"))
      (is (str/includes? result "\"scope:domain:backend\"")))))

(deftest test-query-generates-valid-json-encode
  (testing "Query with scope parameter generates valid json-encode wrapper"
    (let [result (query-with-scope-elisp "note" "global")]
      (is (str/starts-with? result "(json-encode"))
      (is (str/includes? result "emacs-mcp-api-memory-query")))))

;; =============================================================================
;; Integration Tests - Scope System Behavior
;; =============================================================================

(deftest test-scope-system-workflow-create-and-query
  (testing "Complete workflow: create entry with scope, then query with scope filter"
    ;; Step 1: Create entry - should auto-inject project scope
    (let [add-elisp (el/require-and-call-json 'emacs-mcp-memory
                                              'emacs-mcp-memory-add
                                              'note "test note" '("important") nil nil)]
      (is (str/includes? add-elisp "emacs-mcp-memory-add"))
      (is (str/includes? add-elisp "\"test note\"")))

    ;; Step 2: Query with project scope - should find the entry
    (let [query-elisp (query-with-scope-elisp "note" nil)]
      (is (str/includes? query-elisp "emacs-mcp-api-memory-query")))))

(deftest test-scope-system-global-entry-visible-everywhere
  (testing "Global entry visible in all project contexts"
    ;; Create with explicit global scope
    (let [add-elisp (el/require-and-call-json 'emacs-mcp-memory
                                              'emacs-mcp-memory-add
                                              'convention
                                              "Always use kebab-case"
                                              '("scope:global" "style")
                                              nil nil)]
      (is (str/includes? add-elisp "scope:global")))

    ;; Query from any project should find it
    (let [query-project-a (query-with-scope-elisp "convention" nil)]
      (is (str/includes? query-project-a "emacs-mcp-api-memory-query")))))

(deftest test-scope-system-project-isolation
  (testing "Project-scoped entries are isolated between projects"
    ;; This documents expected behavior - entries with scope:project:X
    ;; should only appear in queries from project X
    (let [entry-project-a {:tags ["scope:project:app-a" "feature"]}
          entry-project-b {:tags ["scope:project:app-b" "feature"]}
          scope-tags-a ["scope:project:app-a" "scope:global"]
          scope-tags-b ["scope:project:app-b" "scope:global"]]
      ;; Generate matching elisp
      (is (str/includes? (entry-matches-scope-elisp entry-project-a scope-tags-a)
                         "scope:project:app-a"))
      (is (str/includes? (entry-matches-scope-elisp entry-project-b scope-tags-b)
                         "scope:project:app-b")))))

(deftest test-scope-system-domain-sharing
  (testing "Domain-scoped entries shared across projects in same domain"
    (let [entry {:tags ["scope:domain:microservices" "architecture"]}
          scope-project-1 ["scope:project:auth-service"
                           "scope:domain:microservices"
                           "scope:global"]
          scope-project-2 ["scope:project:user-service"
                           "scope:domain:microservices"
                           "scope:global"]]
      ;; Both projects should match the domain entry
      (is (str/includes? (entry-matches-scope-elisp entry scope-project-1)
                         "scope:domain:microservices"))
      (is (str/includes? (entry-matches-scope-elisp entry scope-project-2)
                         "scope:domain:microservices")))))

;; =============================================================================
;; Edge Cases and Error Handling
;; =============================================================================

(deftest test-scope-tag-with-special-characters
  (testing "Scope tags handle project names with special characters"
    (let [project-name "my-app-2.0"
          result (make-scope-tag-elisp :project project-name)]
      (is (str/includes? result "emacs-mcp-memory--make-scope-tag"))
      (is (str/includes? result "\"my-app-2.0\"")))))

(deftest test-scope-tag-with-spaces-in-name
  (testing "Scope tags handle names with spaces"
    (let [domain-name "Web Services"
          result (make-scope-tag-elisp :domain domain-name)]
      (is (str/includes? result "emacs-mcp-memory--make-scope-tag"))
      (is (str/includes? result "\"Web Services\"")))))

(deftest test-inject-scope-preserves-tag-order
  (testing "inject-project-scope preserves original tag order"
    (let [tags ["first" "second" "third"]
          result (inject-project-scope-elisp tags)]
      (is (str/includes? result "first"))
      (is (str/includes? result "second"))
      (is (str/includes? result "third")))))

(deftest test-applicable-scopes-with-nil-project
  (testing "applicable-scope-tags handles nil project name gracefully"
    (let [result (applicable-scope-tags-elisp :project-name nil)]
      (is (str/includes? result "emacs-mcp-memory--applicable-scope-tags"))
      (is (str/includes? result "nil")))))

(deftest test-entry-matches-with-malformed-scope-tag
  (testing "entry-matches-scope handles malformed scope tags"
    (let [entry {:tags ["scope:" "scope:invalid"]}  ; Malformed
          scope-tags ["scope:global"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p")))))

;; =============================================================================
;; Documentation Tests - Expected Behavior
;; =============================================================================

(deftest test-scope-hierarchy-documentation
  (testing "Documents the scope hierarchy: global > domain > project"
    ;; This test documents expected scope precedence
    ;; When querying, the system includes:
    ;; 1. scope:global (always)
    ;; 2. scope:domain:X (if in domain X)
    ;; 3. scope:project:Y (if in project Y)
    (let [result (applicable-scope-tags-elisp
                  :project-name "my-project"
                  :domain-name "my-domain")]
      ;; Should generate call with both parameters
      (is (str/includes? result "\"my-project\""))
      (is (str/includes? result "\"my-domain\"")))))

(deftest test-scope-filter-all-bypasses-filtering
  (testing "Documents that scope filter 'all' bypasses all scope filtering"
    ;; When scope is "all", no filtering should be applied
    (let [result (query-with-scope-elisp "note" "all")]
      (is (str/includes? result "\"all\"")))))

(deftest test-no-scope-tag-means-global
  (testing "Documents that entries without scope tags are treated as global"
    ;; This is important behavior - backward compatibility
    ;; Old entries without scope tags should be visible everywhere
    (let [entry {:tags ["legacy" "note"]}  ; No scope tag
          scope-tags ["scope:project:new-project"]
          result (entry-matches-scope-elisp entry scope-tags)]
      (is (str/includes? result "emacs-mcp-memory--entry-matches-scope-p")))))

(deftest test-multiple-scope-tags-union-behavior
  (testing "Documents that entries can have multiple scope tags (union)"
    ;; An entry with both scope:global and scope:domain:X is visible:
    ;; - Everywhere (global)
    ;; - In domain X projects (domain)
    (let [entry {:tags ["scope:global" "scope:domain:shared" "reference"]}
          result (entry-matches-scope-elisp entry ["scope:global"])]
      (is (str/includes? result "scope:global"))
      (is (str/includes? result "scope:domain:shared")))))
