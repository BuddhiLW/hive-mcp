(ns hive-mcp.agent.sdk.python
  "Python bridge helpers for libpython-clj interop.

   Provides safe wrappers around libpython-clj2 functions for:
   - Module import (py-import)
   - Method calls (py-call)
   - Attribute access (py-attr)
   - Keyword argument calls (py-call-kw)
   - Type conversion (py->clj)
   - Script execution (py-run)
   - Global variable management (py-set-global!, py-get-global)

   All functions use requiring-resolve for graceful degradation
   when libpython-clj is not on the classpath."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn py-import
  "Safely import a Python module via libpython-clj.
   Returns the module object or nil if unavailable."
  [module-name]
  (try
    (let [import-fn (requiring-resolve 'libpython-clj2.python/import-module)]
      (import-fn module-name))
    (catch Exception e
      (log/warn "[sdk.python] Failed to import Python module"
                {:module module-name :error (ex-message e)})
      nil)))

(defn py-call
  "Call a Python method/attribute with args.
   Wraps libpython-clj2.python.fn/call-attr [item att-name arglist]."
  [obj method & args]
  (try
    (let [call-fn (requiring-resolve 'libpython-clj2.python.fn/call-attr)]
      (call-fn obj method (vec args)))
    (catch Exception e
      (log/error "[sdk.python] Python call failed"
                 {:method method :error (ex-message e)})
      (throw (ex-info "Python call failed"
                      {:method method :error (ex-message e)}
                      e)))))

(defn py-attr
  "Get a Python object attribute.
   Wraps libpython-clj2.python/get-attr [obj attr-name]."
  [obj attr]
  (try
    (let [attr-fn (requiring-resolve 'libpython-clj2.python/get-attr)]
      (attr-fn obj attr))
    (catch Exception e
      (log/warn "[sdk.python] Failed to get Python attribute"
                {:attr attr :error (ex-message e)})
      nil)))

(defn py-call-kw
  "Call a Python callable with positional and keyword arguments.
   Wraps libpython-clj2.python.fn/call-kw [callable arglist kw-args]."
  [callable positional-args kw-args]
  (try
    (let [call-kw-fn (requiring-resolve 'libpython-clj2.python.fn/call-kw)]
      (call-kw-fn callable (vec positional-args) kw-args))
    (catch Exception e
      (log/error "[sdk.python] Python keyword call failed"
                 {:error (ex-message e)})
      (throw (ex-info "Python keyword call failed"
                      {:error (ex-message e)} e)))))

(defn py->clj
  "Convert a Python object to Clojure data."
  [py-obj]
  (try
    (let [convert-fn (requiring-resolve 'libpython-clj2.python/->jvm)]
      (convert-fn py-obj))
    (catch Exception _
      py-obj)))

(defn py-run
  "Run a Python string and return the last value.
   Wraps libpython-clj2.python/run-simple-string."
  [code]
  (let [run-fn (requiring-resolve 'libpython-clj2.python/run-simple-string)]
    (run-fn code)))

(defn py-set-global!
  "Set a variable in Python's __main__ namespace.
   Used to inject Clojure-constructed Python objects into scripts."
  [var-name value]
  (try
    (let [set-fn (requiring-resolve 'libpython-clj2.python/set-attr!)
          main-mod (py-import "__main__")]
      (set-fn main-mod var-name value))
    (catch Exception e
      (log/error "[sdk.python] Failed to set Python global"
                 {:var-name var-name :error (ex-message e)})
      (throw (ex-info "Failed to set Python global"
                      {:var-name var-name :error (ex-message e)} e)))))

(defn py-get-global
  "Get a variable from Python's __main__ namespace.
   Used to extract results from async bridge scripts."
  [var-name]
  (try
    (let [main-mod (py-import "__main__")]
      (py-attr main-mod var-name))
    (catch Exception e
      (log/warn "[sdk.python] Failed to get Python global"
                {:var-name var-name :error (ex-message e)})
      nil)))
