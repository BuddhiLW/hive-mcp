# Sprint 2: A+ Compliance Plan

**Target:** A- (8.35) → A+ (9.5+)
**Duration:** 1 session (parallel execution)
**Approach:** TDD-first, drone-heavy, ling-coordinated

---

## Metrics Targets

| Metric | Current | Target | Delta |
|--------|---------|--------|-------|
| Overall Grade | A- (8.35) | A+ (9.5+) | +1.15 |
| TDD Coverage | 50% | 65%+ | +15% |
| Kondo Warnings | 90 | <50 | -40+ |
| Hotspots (>40) | 5 | 0 | -5 |
| DDD | 8.5/10 | 9.5/10 | +1.0 |
| CLARITY | 9.5/10 | 10/10 | +0.5 |

---

## Learnings Applied (Anti-Patterns Avoided)

| Last Sprint Pitfall | This Sprint Solution |
|---------------------|---------------------|
| Lings edited files directly | ALL file changes via `delegate_drone` or `dispatch_drone_wave` |
| TDD regressed (-7%) | Tests written FIRST or IN PARALLEL with code |
| Drone API mismatches | Lings READ existing API, provide EXACT signatures to drones |
| Hotspots deferred | Decomposition is PRIORITY, not optional |
| Sequential bottlenecks | 6 parallel streams, minimal dependencies |

---

## Stream Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     PARALLEL EXECUTION                          │
├─────────────────────────────────────────────────────────────────┤
│ Stream A: Critical Fix (1 task)           │ 5 min   │ BLOCKING │
├─────────────────────────────────────────────────────────────────┤
│ Stream B: Test Coverage (3 tasks)         │ 20 min  │ TDD      │
│ Stream C: Kondo Cleanup (4 waves)         │ 15 min  │ Cleanup  │
│ Stream D: Decompose prompt_capture (4)    │ 30 min  │ Refactor │
│ Stream E: Decompose chroma (4)            │ 30 min  │ Refactor │
│ Stream F: Decompose sync (3)              │ 25 min  │ Refactor │
├─────────────────────────────────────────────────────────────────┤
│ Total parallel time: ~35 min (with 5 lings)                     │
│ Sequential time: ~125 min                                       │
│ Speedup: ~3.5x                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## STREAM A: Critical Fix (BLOCKING)

**Ling Assignment:** Any ling, first priority
**Dependencies:** None (blocks all other work)

### Task A1: Fix Unresolved Var

```
TASK: Fix agent/ollama-backend reference
FILE: src/hive_mcp/agent/cider.clj
LINE: 195
METHOD: delegate_drone (single file edit)

CURRENT CODE (line 195):
  (case type
    :ollama (agent/ollama-backend opts)  ; <- WRONG
    :cider (cider-backend opts)
    :openrouter (openrouter/openrouter-backend opts))

FIX:
  Replace: agent/ollama-backend
  With: ollama/ollama-backend

CONTEXT: The ns already has [hive-mcp.agent.ollama :as ollama] imported.
         Line 173 correctly uses ollama/ollama-backend.

VERIFY: kondo_lint(path: "src/hive_mcp/agent/cider.clj") returns 0 unresolved-var
```

---

## STREAM B: Test Coverage Recovery (TDD-First)

**Ling Assignment:** 1 dedicated TDD ling
**Dependencies:** Stream A complete
**Presets:** ling, tdd, clarity

### Task B1: Create parser_test.clj

```
TASK: Create parser module test file
FILE: test/hive_mcp/tools/swarm/jvm/parser_test.clj
METHOD: file_write (new file)

CONTENT SPEC:
─────────────────────────────────────────────────────
(ns hive-mcp.tools.swarm.jvm.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.jvm.parser :as parser]))

;; Test 1: parse-process-line for Linux format
(deftest parse-process-line-linux-test
  (testing "Parses standard ps output line"
    (let [line "  12345   1.5   2.3   05:32 /usr/bin/java -jar app.jar"
          result (parser/parse-process-line line)]
      (is (= "12345" (:pid result)))
      (is (= "1.5" (:cpu result)))
      (is (= "2.3" (:mem result)))
      (is (= "05:32" (:etime result)))
      (is (= "/usr/bin/java -jar app.jar" (:cmd result)))))

  (testing "Returns nil for insufficient parts"
    (is (nil? (parser/parse-process-line "12345 1.5")))
    (is (nil? (parser/parse-process-line "")))
    (is (nil? (parser/parse-process-line "   ")))))

;; Test 2: parse-etime-to-minutes
(deftest parse-etime-to-minutes-test
  (testing "MM:SS format"
    (is (= 5 (parser/parse-etime-to-minutes "05:30")))
    (is (= 0 (parser/parse-etime-to-minutes "00:45"))))

  (testing "HH:MM:SS format"
    (is (= 90 (parser/parse-etime-to-minutes "01:30:00")))
    (is (= 150 (parser/parse-etime-to-minutes "02:30:00"))))

  (testing "DD-HH:MM:SS format"
    (is (= 1440 (parser/parse-etime-to-minutes "1-00:00:00")))  ; 1 day
    (is (= 3030 (parser/parse-etime-to-minutes "2-02:30:00")))) ; 2d + 2h + 30m

  (testing "Invalid input returns 0"
    (is (= 0 (parser/parse-etime-to-minutes "invalid")))
    (is (= 0 (parser/parse-etime-to-minutes nil)))))

;; Test 3: parse-process-line-extended (with PPID)
(deftest parse-process-line-extended-test
  (testing "Parses ps output with PPID field"
    (let [line "  1234   1001   0.5   1.2   05:30 java -jar shadow-cljs.jar"
          result (parser/parse-process-line-extended line)]
      (is (= "1234" (:pid result)))
      (is (= "1001" (:ppid result)))
      (is (= "0.5" (:cpu result)))
      (is (= "1.2" (:mem result)))
      (is (= "05:30" (:etime result)))
      (is (= "java -jar shadow-cljs.jar" (:cmd result)))))

  (testing "Returns nil for insufficient parts"
    (is (nil? (parser/parse-process-line-extended "1234 1001 0.5 1.2 05:30")))))
─────────────────────────────────────────────────────

VERIFY:
  1. Read file, confirm ns form present
  2. Run: clojure -M:dev:test --namespace hive-mcp.tools.swarm.jvm.parser-test
  3. All tests pass (0 failures)
```

### Task B2: Create orphan_test.clj

```
TASK: Create orphan detector test file
FILE: test/hive_mcp/tools/swarm/jvm/orphan_test.clj
METHOD: file_write (new file)

CONTENT SPEC:
─────────────────────────────────────────────────────
(ns hive-mcp.tools.swarm.jvm.orphan-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.jvm.orphan :as orphan]
            [hive-mcp.tools.swarm.jvm.parser :as parser]))

;; Test 1: enrich-with-parent-info
(deftest enrich-with-parent-info-test
  (testing "Adds parent info when parent alive"
    (let [proc {:ppid "1001"}
          parents {"1001" {:ppid "1000" :comm "bash"}}
          result (orphan/enrich-with-parent-info proc parents)]
      (is (true? (:parent-alive result)))
      (is (= "bash" (:parent-comm result)))
      (is (false? (:parent-is-claude result)))
      (is (false? (:truly-orphaned result)))))

  (testing "Marks as orphaned when parent dead"
    (let [proc {:ppid "9999"}
          parents {}
          result (orphan/enrich-with-parent-info proc parents)]
      (is (false? (:parent-alive result)))
      (is (nil? (:parent-comm result)))
      (is (true? (:truly-orphaned result)))))

  (testing "Marks as orphaned when reparented to init (ppid=1)"
    (let [proc {:ppid "1"}
          parents {"1" {:ppid "0" :comm "init"}}
          result (orphan/enrich-with-parent-info proc parents)]
      (is (true? (:truly-orphaned result)))))

  (testing "Detects Claude as parent"
    (let [proc {:ppid "1001"}
          parents {"1001" {:ppid "1000" :comm "claude"}}
          result (orphan/enrich-with-parent-info proc parents)]
      (is (true? (:parent-is-claude result)))
      (is (false? (:truly-orphaned result))))))

;; Test 2: protected-type?
(deftest protected-type-test
  (testing "Returns true for protected types"
    (is (true? (orphan/protected-type? {:type :shadow-cljs} #{"shadow-cljs"})))
    (is (true? (orphan/protected-type? {:type :leiningen} #{"leiningen"}))))

  (testing "Returns false for unprotected types"
    (is (false? (orphan/protected-type? {:type :nrepl} #{"shadow-cljs"})))))

;; Test 3: orphan-detector (higher-order function)
(deftest orphan-detector-test
  (testing "Returns a function"
    (is (fn? (orphan/orphan-detector))))

  (testing "Protected types are never orphans"
    (let [detector (orphan/orphan-detector :protected-types #{"shadow-cljs"})]
      (is (false? (detector {:type :shadow-cljs :truly-orphaned true})))))

  (testing "Truly orphaned non-protected types are orphans"
    (let [detector (orphan/orphan-detector :protected-types #{"shadow-cljs"})]
      (is (true? (detector {:type :nrepl :truly-orphaned true})))))

  (testing "Non-orphaned processes are not detected"
    (let [detector (orphan/orphan-detector)]
      (is (false? (detector {:type :nrepl :truly-orphaned false}))))))

;; Test 4: identify-orphan
(deftest identify-orphan-test
  (testing "Adds orphan status and reason"
    (let [proc {:pid "123" :type :nrepl :etime "05:32"
                :truly-orphaned true :parent-comm nil}
          result (orphan/identify-orphan proc)]
      (is (true? (:orphan result)))
      (is (= 5 (:age-minutes result)))
      (is (string? (:reason result))))))

;; Test 5: identify-orphans (batch)
(deftest identify-orphans-test
  (testing "Processes multiple procs"
    (let [procs [{:pid "123" :type :nrepl :etime "05:32" :truly-orphaned true}
                 {:pid "456" :type :shadow-cljs :etime "10:00" :truly-orphaned true}]
          results (orphan/identify-orphans procs :protected-types #{"shadow-cljs"})]
      (is (= 2 (count results)))
      (is (true? (:orphan (first results))))
      (is (false? (:orphan (second results)))))))
─────────────────────────────────────────────────────

VERIFY:
  1. Read file, confirm ns form present
  2. Run: clojure -M:dev:test --namespace hive-mcp.tools.swarm.jvm.orphan-test
  3. All tests pass (0 failures)
```

### Task B3: Create init_test.clj

```
TASK: Create server init test file
FILE: test/hive_mcp/server/init_test.clj
METHOD: file_write (new file)

CONTENT SPEC:
─────────────────────────────────────────────────────
(ns hive-mcp.server.init-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.init :as init]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.tools.memory.crud :as memory-crud]))

;; Fixture: Reset initialization state before each test
(defn reset-init-fixture [f]
  (init/reset-initialization!)
  (f))

(use-fixtures :each reset-init-fixture)

;; Test 1: init-effects! wires handler
(deftest init-effects-wires-handler-test
  (testing "First call wires memory handler"
    (let [handler-set (atom nil)]
      (with-redefs [effects/set-memory-write-handler!
                    (fn [f] (reset! handler-set f))]
        (init/init-effects!)
        (is (= memory-crud/handle-add @handler-set))))))

;; Test 2: init-effects! is idempotent
(deftest init-effects-idempotent-test
  (testing "Second call does not re-wire"
    (let [call-count (atom 0)]
      (with-redefs [effects/set-memory-write-handler!
                    (fn [_] (swap! call-count inc))]
        (init/init-effects!)
        (init/init-effects!)
        (init/init-effects!)
        (is (= 1 @call-count))))))

;; Test 3: reset-initialization! allows re-init
(deftest reset-initialization-test
  (testing "Reset allows re-initialization"
    (let [call-count (atom 0)]
      (with-redefs [effects/set-memory-write-handler!
                    (fn [_] (swap! call-count inc))]
        (init/init-effects!)
        (is (= 1 @call-count))
        (init/reset-initialization!)
        (init/init-effects!)
        (is (= 2 @call-count))))))

;; Test 4: init-effects! returns true
(deftest init-effects-returns-true-test
  (testing "Returns true on success"
    (with-redefs [effects/set-memory-write-handler! (fn [_] nil)]
      (is (true? (init/init-effects!))))))
─────────────────────────────────────────────────────

VERIFY:
  1. Read file, confirm ns form present
  2. Run: clojure -M:dev:test --namespace hive-mcp.server.init-test
  3. All tests pass (0 failures)
```

---

## STREAM C: Kondo Cleanup Waves

**Ling Assignment:** 1 cleanup ling
**Dependencies:** None (parallel with B, D, E, F)
**Approach:** dispatch_drone_wave for batch operations

### Task C1: Wave 1 - Protocol `this` Bindings (24 fixes)

```
TASK: Prefix unused 'this' bindings in protocol implementations
METHOD: dispatch_drone_wave

DRONE TASKS:
─────────────────────────────────────────────────────
[
  {
    "file": "src/hive_mcp/transport.clj",
    "task": "Prefix unused 'this' with underscore in protocol methods at lines 118, 133, 143, 162, 176, 191, 201, 220, 270, 273, 288, 291, 400, 403, 414, 417. Change '[this' to '[_this' where 'this' is not used in the method body."
  },
  {
    "file": "src/hive_mcp/graph/datascript.clj",
    "task": "Prefix unused 'this' with underscore in protocol methods at lines 91, 104, 107, 110, 114, 136, 140, 152. Change '[this' to '[_this' where 'this' is not used in the method body."
  },
  {
    "file": "src/hive_mcp/evaluator.clj",
    "task": "Prefix unused 'this' with underscore in protocol methods at lines 168, 233. Change '[this' to '[_this' where 'this' is not used in the method body."
  }
]
─────────────────────────────────────────────────────

VERIFY: kondo_lint returns 0 unused-binding warnings for 'this'
```

### Task C2: Wave 2 - Exception Handler Bindings (2 fixes)

```
TASK: Prefix unused exception bindings
METHOD: dispatch_drone_wave

DRONE TASKS:
─────────────────────────────────────────────────────
[
  {
    "file": "src/hive_mcp/channel/websocket.clj",
    "task": "Line 54: Change '(catch Exception e' to '(catch Exception _e' - the exception is not used in the catch body."
  },
  {
    "file": "src/hive_mcp/embeddings/ollama.clj",
    "task": "Line 135: Change '(catch Exception e' to '(catch Exception _e' - the exception is not used in the catch body."
  }
]
─────────────────────────────────────────────────────

VERIFY: kondo_lint returns 0 unused-binding warnings for exception handlers
```

### Task C3: Wave 3 - Unused Namespace/Refer Cleanup (22 fixes)

```
TASK: Remove unused namespace requires and referred vars
METHOD: dispatch_drone_wave

DRONE TASKS:
─────────────────────────────────────────────────────
[
  {
    "file": "src/hive_mcp/swarm/sync.clj",
    "task": "Line 22: Remove '[hive-mcp.swarm.logic :as logic]' from the ns :require vector - namespace is required but never used."
  },
  {
    "file": "src/hive_mcp/tools/memory_kanban.clj",
    "task": "Line 15: Remove '[clojure.string :as str]' from the ns :require vector - namespace is required but never used."
  },
  {
    "file": "src/hive_mcp/tools/presets.clj",
    "task": "Line 6: Remove the entire '[hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]' from the ns :require vector - namespace and all refers are unused."
  },
  {
    "file": "src/hive_mcp/tools/projectile.clj",
    "task": "Line 8: Remove the entire '[hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]' from the ns :require vector - namespace and all refers are unused."
  },
  {
    "file": "src/hive_mcp/tools/prompt.clj",
    "task": "Line 5: Remove the entire '[hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]' from the ns :require vector - namespace and all refers are unused."
  },
  {
    "file": "src/hive_mcp/tools/diff.clj",
    "task": "Line 17: In the require '[hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]', remove 'mcp-success' and 'mcp-error' from the :refer vector, keeping only mcp-json."
  },
  {
    "file": "src/hive_mcp/tools/magit.clj",
    "task": "Line 8: In the require '[hive-mcp.tools.core :refer [...mcp-json]]', remove 'mcp-json' from the :refer vector - it is unused."
  },
  {
    "file": "src/hive_mcp/tools/swarm/channel.clj",
    "task": "Line 11: In '[clojure.core.async :refer [...]]', remove 'go' from the :refer vector - it is unused."
  },
  {
    "file": "src/hive_mcp/tools/swarm/lifecycle.clj",
    "task": "Line 14: Remove '[clojure.data.json :as json]' from the ns :require vector - namespace is required but never used."
  },
  {
    "file": "src/hive_mcp/transport/websocket.clj",
    "task": "Line 6: In '[clojure.core.async :refer [chan >!]]', remove 'chan' and '>!' from the :refer vector - they are unused."
  },
  {
    "file": "src/hive_mcp/workflows/router.clj",
    "task": "Line 10: Remove '[taoensso.timbre :as log]' from the ns :require vector - namespace is required but never used."
  }
]
─────────────────────────────────────────────────────

VERIFY: kondo_lint returns 0 unused-namespace and 0 unused-referred-var warnings for these files
```

### Task C4: Wave 4 - Structural Cleanup (4 fixes)

```
TASK: Fix redundant let, duplicate require, and dynamic declaration
METHOD: dispatch_drone_wave

DRONE TASKS:
─────────────────────────────────────────────────────
[
  {
    "file": "src/hive_mcp/chroma.clj",
    "task": "Lines 335-337: Flatten nested let expressions. Merge the outer let bindings with the inner let bindings into a single let form."
  },
  {
    "file": "src/hive_mcp/server.clj",
    "task": "Lines 278-282: Flatten nested let expressions. Merge 'server-opts' binding with 'server' binding into a single let form."
  },
  {
    "file": "src/hive_mcp/org_clj/writer.clj",
    "task": "Line 198: Remove the dynamic require '(require '[clojure.test :refer [run-tests]])' - clojure.test is already required in the ns form at line 11."
  },
  {
    "file": "src/hive_mcp/events/effects.clj",
    "task": "Line 38: Add ^:dynamic metadata to the var. Change 'defonce ^:private *memory-write-handler*' to 'defonce ^:private ^:dynamic *memory-write-handler*'."
  }
]
─────────────────────────────────────────────────────

VERIFY: kondo_lint returns 0 redundant-let, duplicate-require, and earmuffed-var-not-dynamic warnings
```

---

## STREAM D: Decompose prompt_capture.clj (51 → <30)

**Ling Assignment:** 1 decomposition ling
**Dependencies:** None (parallel)
**Presets:** ling, clarity, tdd

### Task D1: Create schema.clj (TDD-first)

```
TASK: Extract schema definitions and validation
FILE: src/hive_mcp/prompt_capture/schema.clj
METHOD: file_write (new file)

CONTENT SPEC (extract from prompt_capture.clj lines 28-70):
─────────────────────────────────────────────────────
(ns hive-mcp.prompt-capture.schema
  (:require [malli.core :as m]
            [malli.error :as me]))

;; Category enum
(def Category
  [:enum :coding :debug :planning :meta :research :config :workflow :architecture])

;; Quality rating enum
(def QualityRating
  [:enum :success :partial :failure :untested])

;; Prompt entry schema
(def PromptEntry
  [:map
   [:id :string]
   [:prompt :string]
   [:accomplishes :string]
   [:well-structured :string]
   [:category Category]
   [:quality QualityRating]
   [:tags [:vector :string]]
   [:created :string]
   [:model {:optional true} :string]
   [:source {:optional true} :string]
   [:improvements {:optional true} :string]
   [:context {:optional true} :string]])

;; Database schema
(def PromptDatabase
  [:map
   [:version :int]
   [:entries [:vector PromptEntry]]])

;; Validation function
(defn validate-entry
  "Validates a prompt entry against schema. Returns {:valid true} or {:valid false :errors ...}"
  [entry]
  (if (m/validate PromptEntry entry)
    {:valid true}
    {:valid false
     :errors (me/humanize (m/explain PromptEntry entry))}))
─────────────────────────────────────────────────────

TEST FILE: test/hive_mcp/prompt_capture/schema_test.clj
─────────────────────────────────────────────────────
(ns hive-mcp.prompt-capture.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.prompt-capture.schema :as schema]))

(deftest validate-entry-valid-test
  (testing "Valid entry passes validation"
    (let [entry {:id "abc123"
                 :prompt "Test prompt"
                 :accomplishes "Does X"
                 :well-structured "Clear structure"
                 :category :coding
                 :quality :untested
                 :tags ["test"]
                 :created "2024-01-01"}]
      (is (true? (:valid (schema/validate-entry entry)))))))

(deftest validate-entry-invalid-test
  (testing "Invalid entry fails validation"
    (let [entry {:id "abc123"}]  ; Missing required fields
      (is (false? (:valid (schema/validate-entry entry)))))))
─────────────────────────────────────────────────────

VERIFY: Both files created, tests pass
```

### Task D2: Create analysis.clj (TDD-first)

```
TASK: Extract prompt analysis logic (pure functions, no I/O)
FILE: src/hive_mcp/prompt_capture/analysis.clj
METHOD: file_write (new file)

CONTENT SPEC (extract from prompt_capture.clj lines 73-192):
─────────────────────────────────────────────────────
(ns hive-mcp.prompt-capture.analysis
  (:require [clojure.string :as str]))

;; Category descriptions (for inference)
(def category-descriptions
  {:coding "Code writing, implementation, refactoring"
   :debug "Debugging, error analysis, troubleshooting"
   :planning "Architecture, design, planning"
   :meta "Prompt engineering, meta-prompts"
   :research "Research, exploration, learning"
   :config "Configuration, setup, environment"
   :workflow "Process, workflow, automation"
   :architecture "System design, architecture decisions"})

;; Keyword patterns for category inference
(def ^:private category-patterns
  {:coding #{"implement" "write" "code" "function" "refactor"}
   :debug #{"debug" "error" "fix" "bug" "issue" "trace"}
   :planning #{"plan" "design" "architect" "strategy"}
   :meta #{"prompt" "claude" "llm" "ai"}
   :research #{"research" "explore" "understand" "learn"}
   :config #{"config" "setup" "install" "environment"}
   :workflow #{"workflow" "automate" "process" "pipeline"}
   :architecture #{"architecture" "system" "design" "pattern"}})

(defn infer-category
  "Infers category from prompt text based on keyword patterns"
  [prompt-text]
  (let [lower-text (str/lower-case (or prompt-text ""))
        scores (for [[cat patterns] category-patterns]
                 [cat (count (filter #(str/includes? lower-text %) patterns))])]
    (or (first (apply max-key second (filter #(pos? (second %)) scores)))
        :coding)))

(defn assess-prompt-quality
  "Scores prompt quality 0-100 based on structure indicators"
  [prompt-text]
  (let [text (or prompt-text "")
        has-context? (or (str/includes? text "context")
                        (str/includes? text "background"))
        has-constraint? (re-find #"(?i)(must|should|don't|avoid)" text)
        has-example? (re-find #"(?i)(example|e\.g\.|for instance)" text)
        has-output-spec? (re-find #"(?i)(return|output|format|respond)" text)
        length-score (min 30 (/ (count text) 10))]
    (+ length-score
       (if has-context? 20 0)
       (if has-constraint? 15 0)
       (if has-example? 20 0)
       (if has-output-spec? 15 0))))

(defn suggest-improvements
  "Suggests improvements for a prompt based on missing elements"
  [prompt-text]
  (let [text (or prompt-text "")
        suggestions (cond-> []
                      (not (str/includes? text "context"))
                      (conj "Add context about the current situation")

                      (not (re-find #"(?i)(must|should)" text))
                      (conj "Add explicit constraints or requirements")

                      (not (re-find #"(?i)example" text))
                      (conj "Include an example of desired output")

                      (< (count text) 100)
                      (conj "Expand with more specific details"))]
    (if (empty? suggestions)
      "Prompt appears well-structured"
      (str/join "; " suggestions))))
─────────────────────────────────────────────────────

TEST FILE: test/hive_mcp/prompt_capture/analysis_test.clj
─────────────────────────────────────────────────────
(ns hive-mcp.prompt-capture.analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.prompt-capture.analysis :as analysis]))

(deftest infer-category-test
  (testing "Infers coding category"
    (is (= :coding (analysis/infer-category "implement a function"))))
  (testing "Infers debug category"
    (is (= :debug (analysis/infer-category "debug this error"))))
  (testing "Defaults to coding for unknown"
    (is (= :coding (analysis/infer-category "hello world")))))

(deftest assess-prompt-quality-test
  (testing "High quality prompt scores well"
    (let [prompt "Context: working on X. Must return JSON. Example: {\"a\": 1}"]
      (is (> (analysis/assess-prompt-quality prompt) 50))))
  (testing "Low quality prompt scores poorly"
    (is (< (analysis/assess-prompt-quality "do stuff") 30))))

(deftest suggest-improvements-test
  (testing "Suggests adding context when missing"
    (let [result (analysis/suggest-improvements "do something")]
      (is (str/includes? result "context")))))
─────────────────────────────────────────────────────

VERIFY: Both files created, tests pass, no I/O dependencies
```

### Task D3: Create storage.clj

```
TASK: Extract org-mode serialization (I/O layer)
FILE: src/hive_mcp/prompt_capture/storage.clj
METHOD: file_write (new file)

CONTENT SPEC (extract from prompt_capture.clj lines 195-308):
─────────────────────────────────────────────────────
(ns hive-mcp.prompt-capture.storage
  (:require [hive-mcp.prompt-capture.schema :as schema]
            [hive-mcp.org-clj.parser :as parser]
            [hive-mcp.org-clj.writer :as writer]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn entry->headline
  "Converts a prompt entry to org-mode headline structure"
  [entry]
  {:level 2
   :title (:accomplishes entry)
   :properties {:ID (:id entry)
                :CATEGORY (name (:category entry))
                :QUALITY (name (:quality entry))
                :CREATED (:created entry)
                :TAGS (str/join "," (:tags entry))}
   :content (str "** Prompt\n" (:prompt entry)
                "\n\n** Well-Structured\n" (:well-structured entry)
                (when (:improvements entry)
                  (str "\n\n** Improvements\n" (:improvements entry))))})

(defn headline->entry
  "Converts org-mode headline to prompt entry"
  [headline]
  (let [props (:properties headline)]
    {:id (get props :ID)
     :accomplishes (:title headline)
     :category (keyword (get props :CATEGORY "coding"))
     :quality (keyword (get props :QUALITY "untested"))
     :created (get props :CREATED)
     :tags (if-let [t (get props :TAGS)]
             (str/split t #",")
             [])
     :prompt (extract-section (:content headline) "Prompt")
     :well-structured (extract-section (:content headline) "Well-Structured")
     :improvements (extract-section (:content headline) "Improvements")}))

(defn- extract-section [content section-name]
  ;; Helper to extract content between section headers
  ...)

(defn load-prompts-file
  "Loads prompts from org file. Returns {:entries [...]} or {:error ...}"
  [file-path]
  (if (.exists (io/file file-path))
    (try
      {:entries (->> (parser/parse-file file-path)
                     :children
                     (filter #(= 2 (:level %)))
                     (map headline->entry))}
      (catch Exception e
        {:error (.getMessage e)}))
    {:entries []}))

(defn save-prompts-file
  "Saves prompts to org file"
  [file-path entries]
  (let [doc {:title "Prompt Capture Database"
             :children (map entry->headline entries)}]
    (spit file-path (writer/write-document doc))))
─────────────────────────────────────────────────────

VERIFY: File created, integrates with org-clj modules
```

### Task D4: Update prompt_capture.clj facade

```
TASK: Reduce prompt_capture.clj to facade (delegate to submodules)
FILE: src/hive_mcp/prompt_capture.clj
METHOD: delegate_drone (rewrite)

NEW STRUCTURE (target: <100 lines, complexity <20):
─────────────────────────────────────────────────────
(ns hive-mcp.prompt-capture
  (:require [hive-mcp.prompt-capture.schema :as schema]
            [hive-mcp.prompt-capture.analysis :as analysis]
            [hive-mcp.prompt-capture.storage :as storage]))

;; Re-export schemas
(def Category schema/Category)
(def PromptEntry schema/PromptEntry)
(def validate-entry schema/validate-entry)

;; Re-export analysis
(def infer-category analysis/infer-category)
(def assess-prompt-quality analysis/assess-prompt-quality)
(def suggest-improvements analysis/suggest-improvements)

;; Core API (compose submodules)
(defn capture-prompt [opts]
  (let [category (or (:category opts) (analysis/infer-category (:prompt opts)))
        entry (-> opts
                  (assoc :id (generate-id))
                  (assoc :category category)
                  (assoc :created (now-iso)))]
    (if-let [errors (:errors (schema/validate-entry entry))]
      {:error errors}
      (do (storage/append-entry! entry)
          {:success true :id (:id entry)}))))

;; ... remaining API functions delegate to submodules
─────────────────────────────────────────────────────

VERIFY:
  1. scc_file returns complexity <20
  2. All existing tests still pass
  3. MCP tools still work
```

---

## STREAM E: Decompose chroma.clj (46 → <25)

**Ling Assignment:** 1 decomposition ling
**Dependencies:** None (parallel)
**Presets:** ling, clarity

### Task E1: Create chroma/serialization.clj

```
TASK: Extract metadata serialization (DRY - used 4x currently)
FILE: src/hive_mcp/chroma/serialization.clj
METHOD: file_write (new file)

CONTENT SPEC:
─────────────────────────────────────────────────────
(ns hive-mcp.chroma.serialization
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(defn serialize-tags
  "Converts tag vector to comma-separated string"
  [tags]
  (when (seq tags)
    (str/join "," (map name tags))))

(defn deserialize-tags
  "Converts comma-separated string to tag vector"
  [tags-str]
  (when (and tags-str (not (str/blank? tags-str)))
    (mapv keyword (str/split tags-str #","))))

(defn serialize-content
  "JSON-encodes content map for storage"
  [content]
  (json/write-str content))

(defn deserialize-content
  "JSON-decodes content from storage"
  [content-str]
  (when content-str
    (json/read-str content-str :key-fn keyword)))

(defn entry->document
  "Converts memory entry to Chroma document format"
  [entry]
  {:id (:id entry)
   :document (or (:content entry) "")
   :metadata (-> {}
                 (assoc "type" (name (:type entry)))
                 (assoc "tags" (serialize-tags (:tags entry)))
                 (assoc "created" (:created entry))
                 (assoc "scope" (:scope entry))
                 (cond-> (:duration entry) (assoc "duration" (name (:duration entry)))))})

(defn document->entry
  "Converts Chroma document to memory entry format"
  [doc]
  (let [meta (:metadata doc)]
    {:id (:id doc)
     :content (:document doc)
     :type (keyword (get meta "type"))
     :tags (deserialize-tags (get meta "tags"))
     :created (get meta "created")
     :scope (get meta "scope")
     :duration (when-let [d (get meta "duration")] (keyword d))}))
─────────────────────────────────────────────────────

TEST FILE: test/hive_mcp/chroma/serialization_test.clj
─────────────────────────────────────────────────────
(ns hive-mcp.chroma.serialization-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.chroma.serialization :as ser]))

(deftest serialize-tags-test
  (testing "Serializes tag vector"
    (is (= "foo,bar" (ser/serialize-tags [:foo :bar]))))
  (testing "Returns nil for empty"
    (is (nil? (ser/serialize-tags [])))))

(deftest deserialize-tags-test
  (testing "Deserializes tag string"
    (is (= [:foo :bar] (ser/deserialize-tags "foo,bar"))))
  (testing "Returns nil for blank"
    (is (nil? (ser/deserialize-tags "")))))

(deftest roundtrip-test
  (testing "Entry survives roundtrip"
    (let [entry {:id "123" :content "test" :type :note
                 :tags [:a :b] :created "2024-01-01" :scope "global"}
          doc (ser/entry->document entry)
          result (ser/document->entry doc)]
      (is (= (:id entry) (:id result)))
      (is (= (:type entry) (:type result)))
      (is (= (:tags entry) (:tags result))))))
─────────────────────────────────────────────────────

VERIFY: Both files created, roundtrip test passes
```

### Task E2: Create chroma/provider.clj

```
TASK: Extract embedding provider protocol and management
FILE: src/hive_mcp/chroma/provider.clj
METHOD: file_write (new file)

CONTENT SPEC (extract from chroma.clj lines 57-89):
─────────────────────────────────────────────────────
(ns hive-mcp.chroma.provider
  (:require [taoensso.timbre :as log]))

(defprotocol EmbeddingProvider
  "Protocol for embedding generation"
  (embed [this text] "Generate embedding vector for text")
  (model-name [this] "Return model name string"))

(defonce ^:private *provider (atom nil))

(defn set-provider!
  "Sets the global embedding provider"
  [provider]
  (log/info "[chroma/provider] Setting embedding provider:" (model-name provider))
  (reset! *provider provider))

(defn get-provider
  "Gets the current embedding provider, or nil"
  []
  @*provider)

(defn provider-available?
  "Returns true if a provider is configured"
  []
  (some? @*provider))

(defn generate-embedding
  "Generates embedding using current provider. Returns nil if no provider."
  [text]
  (when-let [p @*provider]
    (try
      (embed p text)
      (catch Exception e
        (log/warn "[chroma/provider] Embedding failed:" (.getMessage e))
        nil))))
─────────────────────────────────────────────────────

VERIFY: File created, protocol extracted cleanly
```

### Task E3: Create chroma/maintenance.clj

```
TASK: Extract maintenance and health operations
FILE: src/hive_mcp/chroma/maintenance.clj
METHOD: file_write (new file)

CONTENT SPEC (extract from chroma.clj lines 339-487):
─────────────────────────────────────────────────────
(ns hive-mcp.chroma.maintenance
  (:require [hive-mcp.chroma.provider :as provider]
            [clojure-chroma-client.api :as chroma]
            [taoensso.timbre :as log]))

(defn cleanup-expired!
  "Removes entries past their expiration date"
  [collection]
  ...)

(defn entries-expiring-soon
  "Returns entries expiring within n days"
  [collection days]
  ...)

(defn collection-stats
  "Returns statistics for a collection"
  [collection]
  ...)

(defn chroma-available?
  "Checks if Chroma server is reachable"
  []
  (try
    (chroma/heartbeat)
    true
    (catch Exception _
      false)))

(defn status
  "Returns full status including provider and collections"
  []
  {:chroma-available (chroma-available?)
   :provider-configured (provider/provider-available?)
   :provider-model (when (provider/provider-available?)
                     (provider/model-name (provider/get-provider)))})
─────────────────────────────────────────────────────

VERIFY: File created, health checks centralized
```

### Task E4: Update chroma.clj facade

```
TASK: Reduce chroma.clj to facade
FILE: src/hive_mcp/chroma.clj
METHOD: delegate_drone (rewrite)

NEW STRUCTURE (target: <150 lines, complexity <25):
- Re-export from submodules
- Keep core CRUD (index, query, get, update)
- Delegate serialization, provider management, maintenance

VERIFY:
  1. scc_file returns complexity <25
  2. All existing memory tests pass
  3. MCP memory tools still work
```

---

## STREAM F: Decompose sync.clj (43 → <25)

**Ling Assignment:** 1 decomposition ling
**Dependencies:** None (parallel)
**Presets:** ling, clarity

### Task F1: Create sync/handlers.clj

```
TASK: Extract event handlers by entity (slave/task/prompt)
FILE: src/hive_mcp/swarm/sync/handlers.clj
METHOD: file_write (new file)

CONTENT SPEC:
─────────────────────────────────────────────────────
(ns hive-mcp.swarm.sync.handlers
  (:require [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel :as channel]
            [taoensso.timbre :as log]))

;; Slave lifecycle handlers
(defn handle-slave-spawned [event]
  (let [{:keys [slave-id name presets]} event]
    (ds/register-ling! slave-id name presets)
    (log/info "[sync] Slave spawned:" slave-id)))

(defn handle-slave-status [event]
  (let [{:keys [slave-id status]} event]
    (ds/update-ling-status! slave-id status)))

(defn handle-slave-killed [event]
  (let [{:keys [slave-id]} event]
    (ds/unregister-ling! slave-id)
    (log/info "[sync] Slave killed:" slave-id)))

;; Task lifecycle handlers
(defn handle-task-dispatched [event]
  (let [{:keys [task-id slave-id files]} event]
    (ds/claim-files! slave-id files)
    (ds/record-task! task-id slave-id)))

(defn handle-task-completed [event]
  (let [{:keys [task-id slave-id]} event]
    (ds/release-claims! slave-id)
    (ds/mark-task-complete! task-id)
    ;; Layer 4 guarantee: emit shout
    (hivemind/emit-shout! slave-id :completed {:task-id task-id})))

(defn handle-task-failed [event]
  (let [{:keys [task-id slave-id error]} event]
    (ds/release-claims! slave-id)
    (ds/mark-task-failed! task-id error)))

;; Prompt handlers
(defn handle-prompt-shown [event]
  (let [{:keys [slave-id prompt]} event]
    (channel/emit! :prompt-shown {:slave-id slave-id :prompt prompt})))

(defn handle-prompt-stall [event]
  (let [{:keys [slave-id duration]} event]
    (log/warn "[sync] Prompt stall detected:" slave-id "duration:" duration)))

;; Handler dispatch map
(def handlers
  {:slave-spawned handle-slave-spawned
   :slave-status handle-slave-status
   :slave-killed handle-slave-killed
   :task-dispatched handle-task-dispatched
   :task-completed handle-task-completed
   :task-failed handle-task-failed
   :prompt-shown handle-prompt-shown
   :prompt-stall handle-prompt-stall})
─────────────────────────────────────────────────────

VERIFY: File created, handlers isolated by entity
```

### Task F2: Create sync/subscription.clj

```
TASK: Extract reusable event subscription pattern
FILE: src/hive_mcp/swarm/sync/subscription.clj
METHOD: file_write (new file)

CONTENT SPEC:
─────────────────────────────────────────────────────
(ns hive-mcp.swarm.sync.subscription
  (:require [hive-mcp.channel :as channel]
            [clojure.core.async :refer [go-loop <!]]
            [taoensso.timbre :as log]))

(defn subscribe-handler!
  "Creates a go-loop that processes events of given type with handler.
   Returns the subscription channel for cleanup."
  [event-type handler]
  (let [sub-ch (channel/subscribe! event-type)]
    (go-loop []
      (when-let [event (<! sub-ch)]
        (try
          (handler event)
          (catch Exception e
            (log/error "[subscription] Handler error for" event-type ":" (.getMessage e))))
        (recur)))
    sub-ch))

(defn subscribe-all!
  "Subscribes handlers map {event-type -> handler-fn}.
   Returns map of {event-type -> subscription-channel}."
  [handlers]
  (into {}
    (for [[event-type handler] handlers]
      [event-type (subscribe-handler! event-type handler)])))

(defn unsubscribe-all!
  "Closes all subscription channels"
  [subscriptions]
  (doseq [[_ ch] subscriptions]
    (channel/close! ch)))
─────────────────────────────────────────────────────

VERIFY: File created, pattern is reusable
```

### Task F3: Update sync.clj facade

```
TASK: Reduce sync.clj to facade + public API
FILE: src/hive_mcp/swarm/sync.clj
METHOD: delegate_drone (rewrite)

NEW STRUCTURE (target: <80 lines, complexity <20):
─────────────────────────────────────────────────────
(ns hive-mcp.swarm.sync
  (:require [hive-mcp.swarm.sync.handlers :as handlers]
            [hive-mcp.swarm.sync.subscription :as sub]
            [taoensso.timbre :as log]))

(defonce ^:private *subscriptions (atom nil))

(defn start-sync!
  "Starts event sync subscriptions"
  []
  (when-not @*subscriptions
    (log/info "[sync] Starting sync subscriptions")
    (reset! *subscriptions (sub/subscribe-all! handlers/handlers))))

(defn stop-sync!
  "Stops event sync subscriptions"
  []
  (when @*subscriptions
    (log/info "[sync] Stopping sync subscriptions")
    (sub/unsubscribe-all! @*subscriptions)
    (reset! *subscriptions nil)))

(defn sync-status
  "Returns current sync status"
  []
  {:active (some? @*subscriptions)
   :subscriptions (when @*subscriptions (keys @*subscriptions))})
─────────────────────────────────────────────────────

VERIFY:
  1. scc_file returns complexity <20
  2. Swarm sync tests pass
  3. Event flow still works
```

---

## Execution Timeline

```
T+0:00  Start all streams in parallel (6 lings)
        ├── Stream A: Fix cider.clj (5 min)
        ├── Stream B: Write tests (20 min parallel)
        ├── Stream C: Kondo waves (15 min parallel)
        ├── Stream D: Decompose prompt_capture (30 min)
        ├── Stream E: Decompose chroma (30 min)
        └── Stream F: Decompose sync (25 min)

T+5:00  Stream A COMPLETE → unblocks nothing (was parallel anyway)

T+15:00 Stream C COMPLETE (all 4 waves)
        → Run kondo_lint verification
        → Expected: <50 warnings

T+20:00 Stream B COMPLETE (all 3 test files)
        → Run test suite
        → Expected: 3 new test namespaces pass

T+25:00 Stream F COMPLETE
        → Run scc_hotspots
        → Expected: sync.clj complexity <25

T+30:00 Streams D, E COMPLETE
        → Run scc_hotspots
        → Expected: prompt_capture <30, chroma <25
        → Run full test suite
        → Expected: all tests pass

T+35:00 Final verification
        → kondo_analyze: <50 warnings
        → scc_hotspots (threshold 40): 0 files
        → Test coverage: 65%+
        → Grade calculation: A+
```

---

## Verification Checklist

### After Each Stream

| Stream | Verification Command | Expected Result |
|--------|---------------------|-----------------|
| A | `kondo_lint(path: "src/hive_mcp/agent/cider.clj")` | 0 unresolved-var |
| B | `clojure -M:dev:test` | 3 new test ns pass |
| C | `kondo_lint(path: "src/hive_mcp")` | <50 warnings |
| D | `scc_file(path: "src/hive_mcp/prompt_capture.clj")` | complexity <30 |
| E | `scc_file(path: "src/hive_mcp/chroma.clj")` | complexity <25 |
| F | `scc_file(path: "src/hive_mcp/swarm/sync.clj")` | complexity <20 |

### Final Verification

```bash
# Run all tests
clojure -M:dev:test

# Verify metrics
scc_analyze(path: "src/hive_mcp")
scc_hotspots(path: "src/hive_mcp", threshold: 40)
kondo_analyze(path: "src/hive_mcp")

# Calculate grade
# (manual assessment based on metrics)
```

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Drone writes wrong API | Lings READ source first, provide EXACT signatures |
| Tests fail after decomposition | Run tests after EACH submodule creation |
| Circular dependencies | Lings verify dependency graph before committing |
| Complexity not reduced | Lings verify scc_file after EACH extraction |
| Feature regression | Run full MCP tool suite after decomposition |

---

## Success Criteria

- [ ] Kondo warnings < 50
- [ ] Zero hotspots > 40 complexity
- [ ] TDD coverage > 65%
- [ ] All existing tests pass
- [ ] All MCP tools functional
- [ ] Grade: A+ (9.5+)

---

*Plan generated by hivemind sprint planner*
