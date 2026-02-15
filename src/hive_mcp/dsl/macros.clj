(ns hive-mcp.dsl.macros
  "DSL macro expansion layer.

   Registry infrastructure for composable multi-step macros.
   Built-in macro implementations delegate to extension.

   Sentence format: [verb-string params-map]
   Macro output:    vector of [verb params] with :id and :depends_on in params.

   Convention: macro names are descriptive (forge!, catchup!) or use `>`
   for pipeline composition (plan>kb). Short verb-table entries (m+, k!, a!)
   are NOT macros — registration in the macro-registry is authoritative.

   Architecture:
     [macro-verb params] -> expand-macro -> [[verb params]...]
     expand-all handles recursive expansion with cycle guard.
     Output is ready for dsl/verbs.clj parse-dsl or compile-paragraph.

   Design decision: 20260211213929-2be37853"
  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helper
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Registry
;; =============================================================================

;; Maps macro-name (string) -> expansion-fn.
;; expansion-fn: (fn [params-map] -> vector of [verb params] sentences)
(defonce ^:private macro-registry (atom {}))

(defn registered-macros
  "Return set of all registered macro names."
  []
  (set (keys @macro-registry)))

(defn register-macro!
  "Register a macro expansion function.
   expansion-fn: (fn [params] -> [[verb params]...])
   Returns the macro name."
  [macro-name expansion-fn]
  {:pre [(string? macro-name) (fn? expansion-fn)]}
  (swap! macro-registry assoc macro-name expansion-fn)
  macro-name)

(defn unregister-macro!
  "Remove a macro from the registry. Returns the macro name."
  [macro-name]
  (swap! macro-registry dissoc macro-name)
  macro-name)

(defmacro defmacro-dsl
  "Define and register a DSL macro.

   (defmacro-dsl forge! [params]
     [(step \"b?\" {:status \"todo\"} \"forge-survey\")
      (step \"a+\" {:type \"ling\"} \"forge-spawn\" [\"forge-survey\"])])

   Body should return a vector of [verb params] tuples."
  [macro-name params-binding & body]
  `(register-macro! ~(str macro-name)
                    (fn ~params-binding ~@body)))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn macro?
  "Predicate: is this verb a registered DSL macro?
   Convention: macros end with `!` (descriptive names, >= 4 chars)
   or use `>` for pipeline composition. But registration is authoritative."
  [verb]
  (and (string? verb)
       (contains? @macro-registry verb)))

;; =============================================================================
;; Step Construction Helper
;; =============================================================================

(defn step
  "Convenience: build a [verb params] sentence with :id and optional :depends_on.

   (step \"m+\" {:content \"x\"} \"step-1\")
   => [\"m+\" {:content \"x\" :id \"step-1\"}]

   (step \"k>\" {:from \"a\"} \"step-2\" [\"step-1\"])
   => [\"k>\" {:from \"a\" :id \"step-2\" :depends_on [\"step-1\"]}]"
  ([verb params id]
   [verb (assoc (or params {}) :id id)])
  ([verb params id depends-on]
   [verb (assoc (or params {}) :id id :depends_on (vec depends-on))]))

;; =============================================================================
;; Expansion Engine
;; =============================================================================

(defn expand-macro
  "Expand a macro invocation into a vector of [verb params] sentences.

   Returns: [[verb params]...] with :id and :depends_on in params for $ref wiring.
   Throws:  ex-info if macro is not registered."
  [macro-name params]
  (if-let [expansion-fn (get @macro-registry macro-name)]
    (let [result (expansion-fn (or params {}))]
      (when-not (sequential? result)
        (throw (ex-info (str "Macro '" macro-name "' must return a sequential collection")
                        {:macro macro-name :got (type result)})))
      (vec result))
    (throw (ex-info (str "Unknown DSL macro: " macro-name)
                    {:macro     macro-name
                     :available (vec (sort (keys @macro-registry)))}))))

(def ^:private max-expansion-depth
  "Maximum recursion depth for nested macro expansion (cycle guard)."
  10)

(defn expand-all
  "Recursively expand all macros in a DSL paragraph.

   Input:  vector of [verb params] sentences (may contain macro verbs)
   Output: flat vector of [verb params] sentences (all macros expanded)

   Handles nested macros (a macro expansion can itself contain macros)
   with cycle detection via depth limit."
  ([sentences]
   (expand-all sentences 0))
  ([sentences depth]
   (when (>= depth max-expansion-depth)
     (throw (ex-info "Maximum macro expansion depth exceeded (possible cycle)"
                     {:depth depth :max max-expansion-depth})))
   (let [expanded (into []
                        (mapcat (fn [[verb params :as sentence]]
                                  (if (macro? verb)
                                    (expand-macro verb params)
                                    [sentence])))
                        sentences)
         has-nested? (some (fn [[verb _]] (macro? verb)) expanded)]
     (if-not has-nested?
       expanded
       (recur expanded (inc depth))))))

;; =============================================================================
;; Built-in Macros — delegate to extension
;; =============================================================================

(defn- dm-a
  "Delegates to extension."
  [params]
  (delegate-or-noop :dm/a [] [params]))

(defn- dm-b
  "Delegates to extension."
  [params]
  (delegate-or-noop :dm/b [] [params]))

(defn- dm-c
  "Delegates to extension."
  [params]
  (delegate-or-noop :dm/c [] [params]))

(defn- dm-d
  "Delegates to extension."
  [params]
  (delegate-or-noop :dm/d [] [params]))

;; =============================================================================
;; Register Built-in Macros
;; =============================================================================

(register-macro! "forge!"          dm-a)
(register-macro! "catchup!"        dm-b)
(register-macro! "plan>kb"         dm-c)
(register-macro! "plan>kb>forge!"  dm-d)
