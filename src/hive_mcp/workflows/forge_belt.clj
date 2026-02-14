(ns hive-mcp.workflows.forge-belt
  "Forge belt workflow stubs.

   Delegates to extension if available. Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, all functions gracefully degrade
   to empty/noop results."

  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helpers
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
;; Noop Defaults
;; =============================================================================

(def ^:private noop-result
  {:success       false
   :message       "Extension not available"
   :strike-count  0
   :total-smited  0
   :total-sparked 0
   :last-strike   nil
   :quenched?     false
   :continuous?   false
   :smite-result  {:smited [] :failed [] :count 0}
   :survey-result {:tasks [] :count 0}
   :spark-result  {:spawned [] :failed [] :count 0}})

;; =============================================================================
;; Dispatch Predicates — delegates to extension or returns noop
;; =============================================================================

(defn quenched?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q1 false [data]))

(defn has-tasks?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q2 false [data]))

(defn no-tasks?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q3 true [data]))

(defn continuous?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q4 false [data]))

(defn single-shot?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q5 true [data]))

(defn- spark-all-failed?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q6 false [data]))

(defn always
  "Trivial predicate. Returns true for any input."
  [_data]
  true)

;; =============================================================================
;; Handlers — delegates to extension or returns noop
;; =============================================================================

(defn handle-start
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h1 data [resources data]))

(defn handle-smite
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h2
                    (assoc data :smite-result {:smited [] :failed [] :count 0})
                    [resources data]))

(defn handle-survey
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h3
                    (assoc data :survey-result {:tasks [] :count 0})
                    [resources data]))

(defn handle-spark
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h4
                    (assoc data :spark-result {:spawned [] :failed [] :count 0})
                    [resources data]))

(defn handle-end
  "Delegates to extension if available."
  [_resources fsm]
  (delegate-or-noop :fb/h5 {} [_resources fsm]))

(defn handle-halt
  "Delegates to extension if available."
  [_resources fsm]
  (delegate-or-noop :fb/h6 fsm [_resources fsm]))

(defn handle-error
  "Delegates to extension if available."
  [_resources fsm]
  (delegate-or-noop :fb/h7 nil [_resources fsm]))

;; =============================================================================
;; Subscription Handlers — delegates to extension or returns noop
;; =============================================================================

(defn on-smite-count-change
  "Delegates to extension if available."
  [path old-value new-value]
  (delegate-or-noop :fb/s1 nil [path old-value new-value]))

(defn on-spark-count-change
  "Delegates to extension if available."
  [path old-value new-value]
  (delegate-or-noop :fb/s2 nil [path old-value new-value]))

;; =============================================================================
;; FSM Spec — delegates to extension or returns nil
;; =============================================================================

(def forge-belt-spec
  "Delegates to extension if available."
  nil)

;; =============================================================================
;; Compilation & Execution API — delegates to extension or returns noop
;; =============================================================================

(defn compile-belt
  "Delegates to extension if available."
  []
  (delegate-or-noop :fb/compile nil []))

(defn run-belt
  "Delegates to extension if available."
  ([compiled-fsm resources]
   (run-belt compiled-fsm resources {}))
  ([compiled-fsm resources opts]
   (delegate-or-noop :fb/run noop-result [compiled-fsm resources opts])))

(defn run-single-strike
  "Delegates to extension if available."
  [resources]
  (delegate-or-noop :fb/strike noop-result [resources]))

(defn run-continuous-belt
  "Delegates to extension if available."
  [resources]
  (delegate-or-noop :fb/cont noop-result [resources]))
