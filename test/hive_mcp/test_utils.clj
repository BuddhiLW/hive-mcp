(ns hive-mcp.test-utils
  "Test utilities and fixture factories for hive-mcp tests.

   DRY: Consolidates repeated fixture patterns from multiple test files.
   Reference: docs/DRY-AUDIT-REPORT.md - A1 Fixture Boilerplate")

;; =============================================================================
;; Fixture Factories (A1)
;; =============================================================================

(defn make-reset-fixture
  "Create a fixture that resets state before and after test.

   DRY: Replaces 6+ duplicated reset-state-fixture functions.

   Usage:
     (use-fixtures :each (make-reset-fixture #(reset! my-atom {})))

   Multiple reset functions:
     (use-fixtures :each (make-reset-fixture
                           #(reset! atom1 {})
                           #(reset! atom2 nil)))"
  [& reset-fns]
  (fn [f]
    (doseq [reset-fn reset-fns]
      (reset-fn))
    (f)
    (doseq [reset-fn reset-fns]
      (reset-fn))))

(defn make-with-atom-fixture
  "Create a fixture that sets and restores an atom's value.

   Usage:
     (use-fixtures :each (make-with-atom-fixture my-atom initial-value))"
  [atom-var initial-value]
  (fn [f]
    (let [original @atom-var]
      (reset! atom-var initial-value)
      (try
        (f)
        (finally
          (reset! atom-var original))))))

(defn make-capture-fixture
  "Create a fixture that captures function calls for testing.

   Returns a fixture and an atom containing captured calls.

   Usage:
     (let [[fixture calls-atom] (make-capture-fixture)]
       (use-fixtures :each fixture)
       ;; In test:
       @calls-atom => [{:fn 'foo :args [...]} ...])"
  []
  (let [calls (atom [])]
    [(fn [f]
       (reset! calls [])
       (f))
     calls]))

;; =============================================================================
;; Mock Helpers
;; =============================================================================

(defn record-call!
  "Record a function call to an atom. For use in mock implementations.

   Usage:
     (with-redefs [some-fn (fn [& args]
                             (record-call! calls-atom 'some-fn args)
                             mock-result)]
       ...)"
  [calls-atom fn-name args]
  (swap! calls-atom conj {:fn fn-name :args (vec args) :timestamp (System/currentTimeMillis)}))

(defn calls-to
  "Filter captured calls to a specific function.

   Usage:
     (calls-to @calls-atom 'my-fn) => [{:fn 'my-fn :args [...]}]"
  [calls fn-name]
  (filter #(= fn-name (:fn %)) calls))

(defn call-count
  "Count calls to a specific function.

   Usage:
     (call-count @calls-atom 'my-fn) => 3"
  [calls fn-name]
  (count (calls-to calls fn-name)))
