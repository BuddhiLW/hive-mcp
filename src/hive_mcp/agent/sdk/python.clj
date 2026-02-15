(ns hive-mcp.agent.sdk.python
  "Python bridge helpers for libpython-clj interop."
  (:require [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn py-import
  "Safely import a Python module via libpython-clj."
  [module-name]
  (rescue nil
          (let [import-fn (requiring-resolve 'libpython-clj2.python/import-module)]
            (import-fn module-name))))

(defn py-call
  "Call a Python method/attribute with args."
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
  "Get a Python object attribute."
  [obj attr]
  (rescue nil
          (let [attr-fn (requiring-resolve 'libpython-clj2.python/get-attr)]
            (attr-fn obj attr))))

(defn py-call-kw
  "Call a Python callable with positional and keyword arguments."
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
  "Run a Python string and return the last value."
  [code]
  (let [run-fn (requiring-resolve 'libpython-clj2.python/run-simple-string)]
    (run-fn code)))

(defn py-set-global!
  "Set a variable in Python's __main__ namespace."
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
  "Get a variable from Python's __main__ namespace."
  [var-name]
  (rescue nil
          (let [main-mod (py-import "__main__")]
            (py-attr main-mod var-name))))
