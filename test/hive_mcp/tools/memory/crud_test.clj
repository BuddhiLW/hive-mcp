(ns hive-mcp.tools.memory.crud-test
  "Unit tests for memory CRUD intelligence modules.

   Tests cover:
   - classify.clj: Abstraction level classification (P1.5)
     - content-keyword-level, tag-level-signal, classify-abstraction-level
   - gaps.clj: Knowledge gap auto-detection (P2.8)
     - extract-knowledge-gaps, extract-questions, extract-todo-markers
     - extract-uncertainty, extract-missing, extract-assumptions
   - Schema integration: type->abstraction-level map coverage

   Test strategy:
   - All functions are pure (public in their modules) - tested directly
   - No Chroma or DataScript dependencies needed"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [hive-mcp.tools.memory.classify :as classify]
            [hive-mcp.tools.memory.gaps :as gaps]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Aliases for cleaner test code (now public functions in extracted modules)
;; =============================================================================

(def ^:private content-keyword-level classify/content-keyword-level)
(def ^:private tag-level-signal classify/tag-level-signal)
(def ^:private classify-abstraction-level classify/classify-abstraction-level)

(def ^:private extract-knowledge-gaps gaps/extract-knowledge-gaps)
(def ^:private extract-questions gaps/extract-questions)
(def ^:private extract-todo-markers gaps/extract-todo-markers)
(def ^:private extract-uncertainty gaps/extract-uncertainty)
(def ^:private extract-missing gaps/extract-missing)
(def ^:private extract-assumptions gaps/extract-assumptions)

;; =============================================================================
;; content-keyword-level Tests (Pure Function)
;; =============================================================================

(deftest content-keyword-level-detects-l4-axiom-marker
  (testing "[ax] marker triggers L4"
    (is (= 4 (content-keyword-level "[ax] TDD Trust Bridge: Drones think, TDD validates")))
    (is (= 4 (content-keyword-level "Rule [ax] must be followed")))))

(deftest content-keyword-level-detects-l4-must-always
  (testing "'must always' and 'must never' trigger L4"
    (is (= 4 (content-keyword-level "You must always validate input before processing")))
    (is (= 4 (content-keyword-level "Agents must never modify production data directly")))))

(deftest content-keyword-level-detects-l4-inviolable
  (testing "'inviolable' and 'non-negotiable' trigger L4"
    (is (= 4 (content-keyword-level "This is an inviolable constraint on the system")))
    (is (= 4 (content-keyword-level "Non-negotiable requirement: all data must be encrypted")))))

(deftest content-keyword-level-detects-l4-architectural-decision
  (testing "Architectural decision language triggers L4"
    (is (= 4 (content-keyword-level "This architectural decision affects all services")))
    (is (= 4 (content-keyword-level "The design rationale for choosing Clojure was...")))
    (is (= 4 (content-keyword-level "Strategic decision: migrate to event-driven architecture")))))

(deftest content-keyword-level-detects-l4-header-patterns
  (testing "Markdown headers with axiom/principle/ADR trigger L4"
    (is (= 4 (content-keyword-level "# Axiom: Never skip catchup")))
    (is (= 4 (content-keyword-level "## Principle: Fail fast, recover gracefully")))
    (is (= 4 (content-keyword-level "### ADR: Use DataScript for KG storage")))))

(deftest content-keyword-level-detects-l3-convention-headers
  (testing "Convention/pattern/workflow headers trigger L3"
    (is (= 3 (content-keyword-level "# Convention: always use kebab-case")))
    (is (= 3 (content-keyword-level "## Pattern: Facade + Domain decomposition")))
    (is (= 3 (content-keyword-level "# Workflow: SAA (Silence-Abstract-Act)")))
    (is (= 3 (content-keyword-level "## Recipe: TDD loop for drone tasks")))
    (is (= 3 (content-keyword-level "# Guideline: Keep functions under 20 lines")))))

(deftest content-keyword-level-detects-l3-behavior-patterns
  (testing "'always do', 'never do', 'best practice' trigger L3"
    (is (= 3 (content-keyword-level "Always do input validation before DB queries")))
    (is (= 3 (content-keyword-level "Never do direct file writes from lings")))
    (is (= 3 (content-keyword-level "Best practice is to use with-redefs for mocking")))))

(deftest content-keyword-level-detects-l3-anti-patterns
  (testing "'anti-pattern', 'code smell', 'recurring pattern' trigger L3"
    (is (= 3 (content-keyword-level "This is a known anti-pattern in concurrent systems")))
    (is (= 3 (content-keyword-level "Code smell: function does too many things")))
    (is (= 3 (content-keyword-level "Recurring pattern across all handler modules")))))

(deftest content-keyword-level-detects-l1-file-references
  (testing "File/path references trigger L1"
    (is (= 1 (content-keyword-level "file: src/hive_mcp/tools/memory/crud.clj")))
    (is (= 1 (content-keyword-level "path: /home/user/project/foo.clj")))
    (is (= 1 (content-keyword-level "directory: test/hive_mcp/")))))

(deftest content-keyword-level-detects-l1-git-references
  (testing "Git commit/hash references trigger L1"
    (is (= 1 (content-keyword-level "Fixed in git commit abc123")))
    (is (= 1 (content-keyword-level "See commit hash 5e7ee59 for details")))))

(deftest content-keyword-level-detects-l1-line-references
  (testing "Line number references trigger L1"
    (is (= 1 (content-keyword-level "Bug found at line 42 in the handler")))
    (is (= 1 (content-keyword-level "Check line 100-150 for the logic")))))

(deftest content-keyword-level-detects-l1-lint-references
  (testing "Kondo/lint/compilation errors trigger L1"
    (is (= 1 (content-keyword-level "kondo error found in namespace declaration")))
    (is (= 1 (content-keyword-level "lint warning about unused import")))
    (is (= 1 (content-keyword-level "compilation error in tools/kg.clj")))))

(deftest content-keyword-level-returns-nil-for-neutral
  (testing "Returns nil when no strong signal detected"
    (is (nil? (content-keyword-level "Just a regular note about the project")))
    (is (nil? (content-keyword-level "Implemented feature X with approach Y")))
    (is (nil? (content-keyword-level "The system processes requests efficiently")))))

(deftest content-keyword-level-handles-edge-cases
  (testing "Handles nil, empty, and blank content"
    (is (nil? (content-keyword-level nil)))
    (is (nil? (content-keyword-level "")))
    (is (nil? (content-keyword-level "   ")))))

(deftest content-keyword-level-priority-l4-over-l3
  (testing "L4 signals win over L3 when both present"
    ;; Contains both L4 ("must always") and L3 ("convention") keywords
    (is (= 4 (content-keyword-level "Must always follow the convention for naming")))))

(deftest content-keyword-level-priority-l3-over-l1
  (testing "L3 signals win over L1 when both present"
    ;; Contains both L3 ("best practice") and L1 ("at line 42")
    (is (= 3 (content-keyword-level "Best practice found at line 42 in the codebase")))))

;; =============================================================================
;; tag-level-signal Tests (Pure Function)
;; =============================================================================

(deftest tag-level-signal-detects-l4-tags
  (testing "L4 tags: axiom, principle, ADR, strategic"
    (is (= 4 (tag-level-signal ["axiom" "architecture"])))
    (is (= 4 (tag-level-signal ["principle" "testing"])))
    (is (= 4 (tag-level-signal ["ADR" "database"])))
    (is (= 4 (tag-level-signal ["strategic" "migration"])))))

(deftest tag-level-signal-detects-l3-tags
  (testing "L3 tags: convention, pattern, idiom, best-practice, etc."
    (is (= 3 (tag-level-signal ["convention" "naming"])))
    (is (= 3 (tag-level-signal ["pattern" "facade"])))
    (is (= 3 (tag-level-signal ["idiom" "clojure"])))
    (is (= 3 (tag-level-signal ["best-practice" "testing"])))
    (is (= 3 (tag-level-signal ["anti-pattern" "concurrency"])))
    (is (= 3 (tag-level-signal ["workflow" "SAA"])))
    (is (= 3 (tag-level-signal ["recipe" "TDD"])))))

(deftest tag-level-signal-detects-l1-tags
  (testing "L1 tags: file-state, kondo, lint, git-state, disc, compilation"
    (is (= 1 (tag-level-signal ["file-state" "crud.clj"])))
    (is (= 1 (tag-level-signal ["kondo" "analysis"])))
    (is (= 1 (tag-level-signal ["lint" "errors"])))
    (is (= 1 (tag-level-signal ["git-state" "branch"])))
    (is (= 1 (tag-level-signal ["disc" "hash"])))
    (is (= 1 (tag-level-signal ["compilation" "jvm"])))))

(deftest tag-level-signal-returns-nil-for-neutral
  (testing "Returns nil for tags with no level signal"
    (is (nil? (tag-level-signal ["misc" "other"])))
    (is (nil? (tag-level-signal ["architecture" "design"])))
    (is (nil? (tag-level-signal ["testing" "refactoring"])))))

(deftest tag-level-signal-handles-edge-cases
  (testing "Handles nil and empty tags"
    (is (nil? (tag-level-signal nil)))
    (is (nil? (tag-level-signal [])))
    (is (nil? (tag-level-signal ["scope:global" "agent:foo"])))))

(deftest tag-level-signal-l4-priority-over-l3
  (testing "L4 tags take priority when both L4 and L3 tags present"
    (is (= 4 (tag-level-signal ["axiom" "convention" "testing"])))))

;; =============================================================================
;; classify-abstraction-level Tests (Integration)
;; =============================================================================

(deftest classify-type-defaults-from-schema
  (testing "Type-based defaults match schema map when no content/tag signals"
    ;; L4 types
    (is (= 4 (classify-abstraction-level "decision" "Some decision" [])))
    (is (= 4 (classify-abstraction-level "axiom" "Some axiom" [])))
    (is (= 4 (classify-abstraction-level "principle" "A principle" [])))
    ;; L3 types
    (is (= 3 (classify-abstraction-level "convention" "A convention" [])))
    (is (= 3 (classify-abstraction-level "lesson" "A lesson learned" [])))
    (is (= 3 (classify-abstraction-level "rule" "A rule" [])))
    (is (= 3 (classify-abstraction-level "guideline" "A guideline" [])))
    (is (= 3 (classify-abstraction-level "workflow" "A workflow" [])))
    (is (= 3 (classify-abstraction-level "recipe" "A recipe" [])))
    ;; L2 types
    (is (= 2 (classify-abstraction-level "note" "A note" [])))
    (is (= 2 (classify-abstraction-level "snippet" "Some code" [])))
    (is (= 2 (classify-abstraction-level "doc" "Documentation" [])))
    (is (= 2 (classify-abstraction-level "todo" "A todo item" [])))
    ;; Unknown type defaults to L2
    (is (= 2 (classify-abstraction-level "unknown-type" "Something" [])))))

(deftest classify-content-bumps-up-note-to-l4
  (testing "Note with L4 content signals gets bumped to L4"
    (is (= 4 (classify-abstraction-level "note" "[ax] Must always validate" [])))
    (is (= 4 (classify-abstraction-level "note" "This is an inviolable rule" [])))
    (is (= 4 (classify-abstraction-level "note" "Strategic decision to use Clojure" [])))))

(deftest classify-content-bumps-up-note-to-l3
  (testing "Note with L3 content signals gets bumped to L3"
    (is (= 3 (classify-abstraction-level "note" "# Convention: use kebab-case" [])))
    (is (= 3 (classify-abstraction-level "note" "Best practice for testing" [])))
    (is (= 3 (classify-abstraction-level "note" "This is an anti-pattern" [])))))

(deftest classify-content-bumps-down-note-to-l1
  (testing "Note with L1 content signals gets bumped down to L1"
    (is (= 1 (classify-abstraction-level "note" "kondo error at line 42" [])))
    (is (= 1 (classify-abstraction-level "note" "file: src/foo.clj needs fixing" [])))
    (is (= 1 (classify-abstraction-level "note" "git commit abc123 broke tests" [])))))

(deftest classify-content-bumps-down-convention-to-l1
  (testing "Even L3 types can be bumped down to L1 by content"
    (is (= 1 (classify-abstraction-level "convention" "kondo error in the linter" [])))))

(deftest classify-tags-provide-secondary-signal
  (testing "Tags bump level when content has no signal"
    ;; Note (L2 default) + L4 tag → L4
    (is (= 4 (classify-abstraction-level "note" "Some note" ["axiom" "architecture"])))
    ;; Note (L2 default) + L3 tag → L3
    (is (= 3 (classify-abstraction-level "note" "Some note" ["convention" "testing"])))
    ;; Note (L2 default) + L1 tag → L1
    (is (= 1 (classify-abstraction-level "note" "Some note" ["kondo" "analysis"])))))

(deftest classify-content-overrides-tags
  (testing "Content signal takes priority over tag signal"
    ;; L4 content overrides L3 tags
    (is (= 4 (classify-abstraction-level "note" "Must always validate" ["convention"])))
    ;; L1 content overrides L3 tags
    (is (= 1 (classify-abstraction-level "note" "kondo error found" ["convention"])))))

(deftest classify-explicit-level-preserved-by-handle-add
  (testing "Schema's derive-abstraction-level covers all standard types"
    ;; Verify schema map has entries for all standard types
    (doseq [t ["snippet" "note" "convention" "decision" "axiom"
               "pattern" "doc" "todo" "question" "answer"
               "warning" "error" "lesson" "principle" "rule"
               "guideline" "workflow" "recipe"]]
      (is (integer? (kg-schema/derive-abstraction-level t))
          (str "Schema should have mapping for type: " t)))))

(deftest classify-handles-nil-content
  (testing "Classification works with nil content"
    (is (= 2 (classify-abstraction-level "note" nil [])))
    (is (= 4 (classify-abstraction-level "decision" nil [])))
    (is (= 3 (classify-abstraction-level "convention" nil [])))))

(deftest classify-handles-nil-tags
  (testing "Classification works with nil tags"
    (is (= 2 (classify-abstraction-level "note" "Some note" nil)))
    (is (= 4 (classify-abstraction-level "note" "Must always do X" nil)))))

(deftest classify-l3-stub-graceful-fallback
  (testing "Extension requiring-resolve stub falls back gracefully"
    ;; The similarity namespace doesn't exist, so requiring-resolve
    ;; should fail silently and fall back to heuristic level
    (is (= 2 (classify-abstraction-level "note" "Some note" [])))
    (is (= 4 (classify-abstraction-level "axiom" "An axiom" [])))))

;; =============================================================================
;; Knowledge Gap Detection Tests (P2.8)
;; =============================================================================

;; --- extract-questions ---

(deftest extract-questions-finds-question-marks
  (testing "Extracts sentences ending with question marks"
    (let [gaps (extract-questions "How does the auth flow work? It connects via OAuth.")]
      (is (= 1 (count gaps)))
      (is (str/starts-with? (first gaps) "question:")))))

(deftest extract-questions-skips-short-questions
  (testing "Skips very short question fragments"
    (let [gaps (extract-questions "Is it? Yes.")]
      (is (= 0 (count gaps))))))

(deftest extract-questions-multiple
  (testing "Extracts multiple questions"
    (let [gaps (extract-questions "What is the timeout value? How do we handle retries? The system is stable.")]
      (is (= 2 (count gaps)))
      (is (every? #(str/starts-with? % "question:") gaps)))))

;; --- extract-todo-markers ---

(deftest extract-todo-markers-finds-todo
  (testing "Finds TODO markers"
    (let [gaps (extract-todo-markers "TODO: implement validation for edge cases")]
      (is (= 1 (count gaps)))
      (is (str/starts-with? (first gaps) "todo:")))))

(deftest extract-todo-markers-finds-fixme
  (testing "Finds FIXME markers"
    (let [gaps (extract-todo-markers "FIXME: this is a workaround")]
      (is (= 1 (count gaps)))
      (is (str/starts-with? (first gaps) "fixme:")))))

(deftest extract-todo-markers-finds-tbd
  (testing "Finds TBD markers"
    (let [gaps (extract-todo-markers "The approach is TBD based on requirements")]
      (is (= 1 (count gaps)))
      (is (str/starts-with? (first gaps) "tbd:")))))

(deftest extract-todo-markers-finds-hack-and-xxx
  (testing "Finds HACK and XXX markers"
    (let [gaps (extract-todo-markers "HACK: temporary workaround. XXX: needs review")]
      (is (= 2 (count gaps))))))

(deftest extract-todo-markers-case-insensitive
  (testing "Case-insensitive matching"
    (let [gaps (extract-todo-markers "todo implement this later")]
      (is (= 1 (count gaps))))))

;; --- extract-uncertainty ---

(deftest extract-uncertainty-finds-unclear
  (testing "Finds 'unclear' language"
    (let [gaps (extract-uncertainty "The behavior is unclear when connections drop")]
      (is (= 1 (count gaps)))
      (is (some #(str/includes? % "unclear") gaps)))))

(deftest extract-uncertainty-finds-unknown
  (testing "Finds 'unknown' language"
    (let [gaps (extract-uncertainty "The root cause is unknown at this point")]
      (is (= 1 (count gaps))))))

(deftest extract-uncertainty-finds-needs-investigation
  (testing "Finds 'needs investigation' language"
    (let [gaps (extract-uncertainty "This failure mode needs investigation")]
      (is (= 1 (count gaps))))))

(deftest extract-uncertainty-finds-open-question
  (testing "Finds 'open question' language"
    (let [gaps (extract-uncertainty "Open question about the API contract")]
      (is (= 1 (count gaps))))))

;; --- extract-missing ---

(deftest extract-missing-finds-missing
  (testing "Finds 'missing' markers"
    (let [gaps (extract-missing "Missing error handling in the retry logic")]
      (is (= 1 (count gaps)))
      (is (some #(str/includes? % "missing") gaps)))))

(deftest extract-missing-finds-not-implemented
  (testing "Finds 'not implemented' markers"
    (let [gaps (extract-missing "Feature X is not implemented yet")]
      (is (= 1 (count gaps))))))

(deftest extract-missing-finds-stub
  (testing "Finds 'stub' markers"
    (let [gaps (extract-missing "This is a stub for the payment processor")]
      (is (= 1 (count gaps))))))

(deftest extract-missing-finds-placeholder
  (testing "Finds 'placeholder' markers"
    (let [gaps (extract-missing "Using a placeholder value for the API key")]
      (is (= 1 (count gaps))))))

;; --- extract-assumptions ---

(deftest extract-assumptions-finds-assuming
  (testing "Finds 'assuming' markers"
    (let [gaps (extract-assumptions "Assuming the database is already migrated")]
      (is (= 1 (count gaps)))
      (is (some #(str/includes? % "assumption") gaps)))))

(deftest extract-assumptions-finds-assumption
  (testing "Finds 'assumption' markers"
    (let [gaps (extract-assumptions "This assumption may not hold under load")]
      (is (= 1 (count gaps))))))

;; --- extract-knowledge-gaps (Integration) ---

(deftest extract-knowledge-gaps-returns-empty-for-nil
  (testing "Returns empty vector for nil content"
    (is (= [] (extract-knowledge-gaps nil)))))

(deftest extract-knowledge-gaps-returns-empty-for-blank
  (testing "Returns empty vector for blank content"
    (is (= [] (extract-knowledge-gaps "")))
    (is (= [] (extract-knowledge-gaps "   ")))))

(deftest extract-knowledge-gaps-returns-empty-for-clean-content
  (testing "Returns empty vector for content with no gaps"
    (is (= [] (extract-knowledge-gaps "The system processes requests efficiently. All tests pass.")))))

(deftest extract-knowledge-gaps-detects-questions
  (testing "Detects questions in content"
    (let [gaps (extract-knowledge-gaps "Should we use Redis or Memcached for caching?")]
      (is (pos? (count gaps)))
      (is (some #(str/starts-with? % "question:") gaps)))))

(deftest extract-knowledge-gaps-detects-todos
  (testing "Detects TODO markers in content"
    (let [gaps (extract-knowledge-gaps "TODO: add input validation for the API endpoint")]
      (is (pos? (count gaps)))
      (is (some #(str/starts-with? % "todo:") gaps)))))

(deftest extract-knowledge-gaps-detects-uncertainty
  (testing "Detects uncertainty language in content"
    (let [gaps (extract-knowledge-gaps "The timeout behavior is unclear when the server is under load")]
      (is (pos? (count gaps)))
      (is (some #(str/includes? % "unclear") gaps)))))

(deftest extract-knowledge-gaps-detects-missing
  (testing "Detects missing markers in content"
    (let [gaps (extract-knowledge-gaps "Missing error handling for the edge case where connection drops")]
      (is (pos? (count gaps)))
      (is (some #(str/includes? % "missing") gaps)))))

(deftest extract-knowledge-gaps-detects-assumptions
  (testing "Detects assumption markers in content"
    (let [gaps (extract-knowledge-gaps "Assuming all entries have valid project-id. This assumption may break.")]
      (is (pos? (count gaps))))))

(deftest extract-knowledge-gaps-combines-multiple-types
  (testing "Detects multiple gap types in same content"
    (let [gaps (extract-knowledge-gaps
                (str "TODO: implement retry logic. "
                     "How should we handle timeouts? "
                     "The error behavior is unclear. "
                     "Missing validation for edge cases."))]
      (is (>= (count gaps) 3) "Should detect at least 3 gap types"))))

(deftest extract-knowledge-gaps-respects-max-limit
  (testing "Limits to max 10 gaps"
    (let [content (str/join ". "
                            (map #(str "TODO: item " %) (range 20)))
          gaps (extract-knowledge-gaps content)]
      (is (<= (count gaps) 10)))))

(deftest extract-knowledge-gaps-truncates-long-gaps
  (testing "Truncates gap descriptors to max length"
    (let [long-content (str "TODO: " (apply str (repeat 200 "x")))
          gaps (extract-knowledge-gaps long-content)]
      (when (seq gaps)
        (is (every? #(<= (count %) 80) gaps))))))

(deftest extract-knowledge-gaps-l3-stub-fallback
  (testing "Extension requiring-resolve stub falls back to L1/L2 result"
    ;; The similarity namespace doesn't exist, so requiring-resolve
    ;; should fail silently and fall back to regex-based detection
    (let [gaps (extract-knowledge-gaps "TODO: implement feature X")]
      (is (vector? gaps))
      (is (pos? (count gaps))))))
