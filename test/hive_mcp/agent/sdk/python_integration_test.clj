(ns hive-mcp.agent.sdk.python-integration-test
  "Integration tests for python.clj bridge with real Python runtime.

   These tests require libpython-clj2 and a working Python installation.
   They exercise the actual Python interop path end-to-end.

   Run via nREPL:
     (require 'hive-mcp.agent.sdk.python-integration-test :reload)
     (clojure.test/run-tests 'hive-mcp.agent.sdk.python-integration-test)

   Tagged :integration — excluded from CI unit test runs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.sdk.python :as py]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn python-available?
  "Check if libpython-clj2 is on the classpath and Python is functional."
  []
  (try
    (let [init-fn (requiring-resolve 'libpython-clj2.python/initialize!)]
      (init-fn)
      true)
    (catch Exception _
      false)))

(def ^:dynamic *python-ok* false)

(defn python-fixture
  "Once fixture: initialize Python, skip all tests if unavailable."
  [f]
  (if (python-available?)
    (binding [*python-ok* true]
      (f))
    (println "SKIP: Python/libpython-clj2 not available, skipping integration tests")))

(use-fixtures :once python-fixture)

;; =============================================================================
;; Tests
;; =============================================================================

(deftest ^:integration py-import-real-module
  (testing "Import a real Python module (sys)"
    (when *python-ok*
      (let [sys-mod (py/py-import "sys")]
        (is (some? sys-mod)
            "Should successfully import sys module")))))

(deftest ^:integration py-import-nonexistent-module
  (testing "Import a nonexistent module returns nil"
    (when *python-ok*
      (is (nil? (py/py-import "nonexistent_module_xyz_12345"))
          "Should return nil for nonexistent module"))))

(deftest ^:integration py-run-simple-string
  (testing "Execute simple Python code"
    (when *python-ok*
      (let [result (py/py-run "x = 1 + 2")]
        (is (some? result)
            "Should return a result from run-simple-string")))))

(deftest ^:integration py-set-get-global-roundtrip
  (testing "Set and get a global variable in Python __main__"
    (when *python-ok*
      (py/py-set-global! "test_var_42" 42)
      (py/py-run "assert test_var_42 == 42")
      (let [val (py/py-get-global "test_var_42")]
        (is (= 42 (long val))
            "Should retrieve the value set as global")))))

(deftest ^:integration py-call-kw-with-class-constructor
  (testing "Call a Python class constructor with keyword arguments"
    (when *python-ok*
      ;; Define a simple Python class
      (py/py-run "
class Point:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y
")
      (let [point-class (py/py-get-global "Point")
            point (py/py-call-kw point-class [] {:x 10 :y 20})
            x-val (py/py-attr point "x")
            y-val (py/py-attr point "y")]
        (is (some? point) "Should create a Point instance")
        (is (= 10 (long x-val)) "Should have x=10")
        (is (= 20 (long y-val)) "Should have y=20")))))

(deftest ^:integration py-call-kw-empty-args
  (testing "Call a Python callable with empty positional args and kwargs"
    (when *python-ok*
      (py/py-run "
class Config:
    def __init__(self, name='default', debug=False):
        self.name = name
        self.debug = debug
")
      (let [config-class (py/py-get-global "Config")
            config (py/py-call-kw config-class [] {:name "test" :debug true})
            name-val (py/py-attr config "name")]
        (is (= "test" (str name-val))
            "Should pass keyword arguments correctly")))))

(deftest ^:integration py-attr-access
  (testing "Access Python object attributes"
    (when *python-ok*
      (let [sys-mod (py/py-import "sys")
            version (py/py-attr sys-mod "version")]
        (is (string? (str version))
            "sys.version should be a string")))))

(deftest ^:integration py->clj-conversion
  (testing "Convert Python objects to Clojure data"
    (when *python-ok*
      (py/py-run "test_list = [1, 2, 3]")
      (let [py-list (py/py-get-global "test_list")
            clj-val (py/py->clj py-list)]
        (is (sequential? clj-val)
            "Python list should convert to sequential Clojure data")
        (is (= [1 2 3] (mapv long clj-val))
            "Values should be preserved")))))

(deftest ^:integration py-call-method
  (testing "Call a Python method on an object"
    (when *python-ok*
      (py/py-run "
class Calculator:
    def add(self, a, b):
        return a + b
calc = Calculator()
")
      (let [calc (py/py-get-global "calc")
            result (py/py-call calc "add" 3 4)]
        (is (= 7 (long result))
            "Should call method with positional args")))))
