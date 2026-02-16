(ns hive-mcp.dns.result
  "Re-exports hive-dsl.result — single source of truth for Result monad.

   Canonical implementation lives in hive-dsl. This namespace re-exports
   all public vars so existing hive-mcp code continues to work unchanged.

   Usage (both equivalent):
     (require '[hive-mcp.dns.result :as result])    ;; legacy
     (require '[hive-dsl.result :as result])         ;; canonical"
  (:require [hive-dsl.result]))

;; --- Re-export all public vars from hive-dsl.result --------------------------
;; Macros must be re-defined (potemkin not on classpath), fns use def aliases.

;; Functions — simple var aliases
(def ok       hive-dsl.result/ok)
(def err      hive-dsl.result/err)
(def ok?      hive-dsl.result/ok?)
(def err?     hive-dsl.result/err?)
(def bind     hive-dsl.result/bind)
(def map-ok   hive-dsl.result/map-ok)
(def map-err  hive-dsl.result/map-err)
(def rescue-fn hive-dsl.result/rescue-fn)
(def guard-fn  hive-dsl.result/guard-fn)

;; Macros — must re-define (def doesn't preserve macro metadata)
(defmacro let-ok
  "Re-export of hive-dsl.result/let-ok. See canonical docstring."
  [bindings & body]
  `(hive-dsl.result/let-ok ~bindings ~@body))

(defmacro try-effect
  "Re-export of hive-dsl.result/try-effect. See canonical docstring."
  [& body]
  `(hive-dsl.result/try-effect ~@body))

(defmacro try-effect*
  "Re-export of hive-dsl.result/try-effect*. See canonical docstring."
  [category & body]
  `(hive-dsl.result/try-effect* ~category ~@body))

(defmacro rescue
  "Re-export of hive-dsl.result/rescue. See canonical docstring."
  [fallback & body]
  `(hive-dsl.result/rescue ~fallback ~@body))

(defmacro guard
  "Re-export of hive-dsl.result/guard. See canonical docstring."
  [catch-class fallback & body]
  `(hive-dsl.result/guard ~catch-class ~fallback ~@body))
