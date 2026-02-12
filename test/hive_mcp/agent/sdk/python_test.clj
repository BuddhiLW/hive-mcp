(ns hive-mcp.agent.sdk.python-test
  "Unit tests for python.clj bridge layer.

   Strategy: Mock `requiring-resolve` to verify correct namespace/var
   resolution for each bridge function. Tests verify:
   - Correct libpython-clj2 namespace resolution
   - Argument passing and return value delegation
   - Graceful degradation when libpython-clj2 is absent
   - Error handling behavior (throw vs return nil)"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.sdk.python :as py]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Infrastructure
;; =============================================================================

(defn- mock-requiring-resolve
  "Create a mock requiring-resolve that returns `ret-fn` when the expected
   `expected-sym` is resolved, and throws for anything else.
   Captures the resolved symbol in `captured-atom`."
  [expected-sym ret-fn captured-atom]
  (fn [sym]
    (reset! captured-atom sym)
    (if (= sym expected-sym)
      ret-fn
      (throw (ex-info "Unexpected resolve" {:sym sym})))))

(defn- mock-requiring-resolve-multi
  "Create a mock requiring-resolve that handles multiple symbols.
   `sym-fn-map` is {symbol -> function}.
   Captures all resolved symbols in `captured-atom` (vector)."
  [sym-fn-map captured-atom]
  (fn [sym]
    (swap! captured-atom conj sym)
    (if-let [f (get sym-fn-map sym)]
      f
      (throw (ex-info "Unexpected resolve" {:sym sym :expected (keys sym-fn-map)})))))

(defn- mock-requiring-resolve-nil
  "Create a mock requiring-resolve that always returns nil (simulates missing lib)."
  []
  (fn [_sym] nil))

;; =============================================================================
;; py-import tests
;; =============================================================================

(deftest py-import-resolves-correct-namespace
  (testing "py-import resolves libpython-clj2.python/import-module"
    (let [captured (atom nil)
          mock-fn (fn [mod-name] {:module mod-name})]
      (with-redefs [requiring-resolve (mock-requiring-resolve
                                       'libpython-clj2.python/import-module
                                       mock-fn captured)]
        (let [result (py/py-import "os")]
          (is (= 'libpython-clj2.python/import-module @captured)
              "Should resolve import-module from libpython-clj2.python")
          (is (= {:module "os"} result)
              "Should pass module name and return result"))))))

(deftest py-import-returns-nil-on-failure
  (testing "py-import returns nil when import fails"
    (let [mock-fn (fn [_] (throw (Exception. "No such module")))]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (is (nil? (py/py-import "nonexistent.module"))
            "Should return nil on import failure")))))

(deftest py-import-returns-nil-when-lib-absent
  (testing "py-import returns nil when libpython-clj2 is not available"
    (with-redefs [requiring-resolve (fn [_] (throw (Exception. "Could not resolve")))]
      (is (nil? (py/py-import "os"))
          "Should return nil when requiring-resolve fails"))))

;; =============================================================================
;; py-call tests
;; =============================================================================

(deftest py-call-resolves-correct-namespace
  (testing "py-call resolves libpython-clj2.python.fn/call-attr"
    (let [captured (atom nil)
          mock-fn (fn [obj method arglist] {:obj obj :method method :args arglist})]
      (with-redefs [requiring-resolve (mock-requiring-resolve
                                       'libpython-clj2.python.fn/call-attr
                                       mock-fn captured)]
        (let [result (py/py-call :fake-obj "some_method" "arg1" "arg2")]
          (is (= 'libpython-clj2.python.fn/call-attr @captured)
              "Should resolve call-attr from libpython-clj2.python.fn")
          (is (= {:obj :fake-obj :method "some_method" :args ["arg1" "arg2"]}
                 result)
              "Should pass obj, method, and args collected into vector"))))))

(deftest py-call-no-extra-args
  (testing "py-call works with no extra arguments"
    (let [mock-fn (fn [obj method arglist] {:obj obj :method method :args arglist})]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (let [result (py/py-call :obj "method")]
          (is (= {:obj :obj :method "method" :args []}
                 result)))))))

(deftest py-call-throws-on-failure
  (testing "py-call throws ex-info on failure"
    (let [mock-fn (fn [& _] (throw (Exception. "Python error")))]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Python call failed"
                              (py/py-call :obj "bad_method")))))))

;; =============================================================================
;; py-attr tests
;; =============================================================================

(deftest py-attr-resolves-correct-namespace
  (testing "py-attr resolves libpython-clj2.python/get-attr"
    (let [captured (atom nil)
          mock-fn (fn [obj attr] {:obj obj :attr attr})]
      (with-redefs [requiring-resolve (mock-requiring-resolve
                                       'libpython-clj2.python/get-attr
                                       mock-fn captured)]
        (let [result (py/py-attr :fake-obj "name")]
          (is (= 'libpython-clj2.python/get-attr @captured)
              "Should resolve get-attr from libpython-clj2.python")
          (is (= {:obj :fake-obj :attr "name"} result)
              "Should pass obj and attr name"))))))

(deftest py-attr-returns-nil-on-failure
  (testing "py-attr returns nil when attribute access fails"
    (let [mock-fn (fn [_ _] (throw (Exception. "No attribute")))]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (is (nil? (py/py-attr :obj "missing_attr"))
            "Should return nil on attribute access failure")))))

;; =============================================================================
;; py-call-kw tests (THE BUG: was resolving wrong namespace)
;; =============================================================================

(deftest py-call-kw-resolves-correct-namespace
  (testing "py-call-kw resolves libpython-clj2.python.fn/call-kw (NOT libpython-clj2.python/call-kw)"
    (let [captured (atom nil)
          mock-fn (fn [callable args kw] {:callable callable :args args :kw kw})]
      (with-redefs [requiring-resolve (mock-requiring-resolve
                                       'libpython-clj2.python.fn/call-kw
                                       mock-fn captured)]
        (let [result (py/py-call-kw :some-class ["pos1" "pos2"] {:key "val"})]
          (is (= 'libpython-clj2.python.fn/call-kw @captured)
              "MUST resolve from libpython-clj2.python.fn, not libpython-clj2.python")
          (is (= {:callable :some-class
                  :args ["pos1" "pos2"]
                  :kw {:key "val"}}
                 result)
              "Should pass callable, positional args as vec, and kw-args map"))))))

(deftest py-call-kw-empty-positional-args
  (testing "py-call-kw works with empty positional args (common case for constructors)"
    (let [mock-fn (fn [callable args kw] {:callable callable :args args :kw kw})]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (let [result (py/py-call-kw :MyClass [] {:name "test"})]
          (is (= [] (:args result))
              "Empty positional args should be passed as empty vector")
          (is (= {:name "test"} (:kw result))
              "Keyword args should be passed through"))))))

(deftest py-call-kw-coerces-positional-to-vec
  (testing "py-call-kw coerces positional args to vector"
    (let [mock-fn (fn [callable args kw] {:args args})]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (let [result (py/py-call-kw :cls '("a" "b") {:k "v"})]
          (is (vector? (:args result))
              "Positional args should be coerced to vector"))))))

(deftest py-call-kw-throws-on-failure
  (testing "py-call-kw throws ex-info on failure"
    (let [mock-fn (fn [& _] (throw (Exception. "Call failed")))]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Python keyword call failed"
                              (py/py-call-kw :cls [] {:k "v"})))))))

;; =============================================================================
;; py->clj tests
;; =============================================================================

(deftest py->clj-resolves-correct-namespace
  (testing "py->clj resolves libpython-clj2.python/->jvm"
    (let [captured (atom nil)
          mock-fn (fn [obj] {:converted obj})]
      (with-redefs [requiring-resolve (mock-requiring-resolve
                                       'libpython-clj2.python/->jvm
                                       mock-fn captured)]
        (let [result (py/py->clj :py-obj)]
          (is (= 'libpython-clj2.python/->jvm @captured)
              "Should resolve ->jvm from libpython-clj2.python")
          (is (= {:converted :py-obj} result)))))))

(deftest py->clj-returns-input-on-failure
  (testing "py->clj returns the original object when conversion fails"
    (let [mock-fn (fn [_] (throw (Exception. "Cannot convert")))]
      (with-redefs [requiring-resolve (constantly mock-fn)]
        (is (= :unconvertible (py/py->clj :unconvertible))
            "Should return the input object as fallback")))))

;; =============================================================================
;; py-run tests
;; =============================================================================

(deftest py-run-resolves-correct-namespace
  (testing "py-run resolves libpython-clj2.python/run-simple-string"
    (let [captured (atom nil)
          mock-fn (fn [code] {:ran code})]
      (with-redefs [requiring-resolve (mock-requiring-resolve
                                       'libpython-clj2.python/run-simple-string
                                       mock-fn captured)]
        (let [result (py/py-run "print('hello')")]
          (is (= 'libpython-clj2.python/run-simple-string @captured)
              "Should resolve run-simple-string from libpython-clj2.python")
          (is (= {:ran "print('hello')"} result)))))))

;; =============================================================================
;; py-set-global! tests
;; =============================================================================

(deftest py-set-global!-resolves-correct-namespaces
  (testing "py-set-global! resolves set-attr! and import-module"
    (let [captured (atom [])
          mock-main-mod :mock-main
          sym-fn-map {'libpython-clj2.python/set-attr!
                      (fn [obj var-name val] {:set-on obj :var var-name :val val})

                      'libpython-clj2.python/import-module
                      (fn [mod-name]
                        (when (= mod-name "__main__")
                          mock-main-mod))}]
      (with-redefs [requiring-resolve (mock-requiring-resolve-multi sym-fn-map captured)]
        (let [result (py/py-set-global! "my_var" 42)]
          (is (= {:set-on mock-main-mod :var "my_var" :val 42} result)
              "Should call set-attr! on __main__ module")
          (is (some #(= 'libpython-clj2.python/set-attr! %) @captured)
              "Should resolve set-attr!")
          (is (some #(= 'libpython-clj2.python/import-module %) @captured)
              "Should resolve import-module for __main__"))))))

(deftest py-set-global!-throws-on-failure
  (testing "py-set-global! throws ex-info on failure"
    (with-redefs [requiring-resolve (fn [_] (throw (Exception. "No python")))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Failed to set Python global"
                            (py/py-set-global! "var" "val"))))))

;; =============================================================================
;; py-get-global tests
;; =============================================================================

(deftest py-get-global-resolves-correct-namespaces
  (testing "py-get-global uses py-import + py-attr on __main__"
    (let [captured (atom [])
          mock-main-mod :mock-main
          sym-fn-map {'libpython-clj2.python/import-module
                      (fn [mod-name]
                        (when (= mod-name "__main__")
                          mock-main-mod))

                      'libpython-clj2.python/get-attr
                      (fn [obj attr-name]
                        (when (and (= obj mock-main-mod) (= attr-name "my_var"))
                          "the-value"))}]
      (with-redefs [requiring-resolve (mock-requiring-resolve-multi sym-fn-map captured)]
        (let [result (py/py-get-global "my_var")]
          (is (= "the-value" result)
              "Should return the global variable value"))))))

(deftest py-get-global-returns-nil-on-failure
  (testing "py-get-global returns nil when variable doesn't exist"
    (with-redefs [requiring-resolve (fn [_] (throw (Exception. "No python")))]
      (is (nil? (py/py-get-global "nonexistent"))
          "Should return nil on failure"))))
