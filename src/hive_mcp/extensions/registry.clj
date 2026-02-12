(ns hive-mcp.extensions.registry
  "Opaque extension registry for optional capabilities.

   Provides a thread-safe registry where external projects can register
   implementations at startup. Consumers look up extensions by opaque
   keyword keys without knowing which project provides them.

   Usage:
     ;; Registration (at startup, by extension project)
     (register! :gs/struct-cmp my-cmp-fn)

     ;; Consumption (anywhere in hive-mcp)
     (if-let [f (get-extension :gs/struct-cmp)]
       (f node-a node-b)
       default-value)

   Thread safety: All operations are atomic via atom + swap!.
   Idempotent: Re-registering the same key replaces silently.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registry State
;; =============================================================================

(defonce ^:private registry
  (atom {}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn register!
  "Register an extension function under an opaque keyword key.
   Thread-safe, idempotent. Re-registration replaces the previous value."
  [k f]
  {:pre [(keyword? k) (ifn? f)]}
  (swap! registry assoc k f)
  k)

(defn register-many!
  "Register multiple extensions at once from a map of {keyword fn}.
   Thread-safe, atomic."
  [m]
  {:pre [(map? m)]}
  (swap! registry merge m)
  (keys m))

(defn get-extension
  "Look up a registered extension by keyword key.
   Returns the function if registered, or default (nil if not provided)."
  ([k]
   (get @registry k))
  ([k default]
   (get @registry k default)))

(defn extension-available?
  "Check if an extension is registered under the given key."
  [k]
  (contains? @registry k))

(defn registered-keys
  "Return the set of all registered extension keys."
  []
  (set (keys @registry)))

(defn deregister!
  "Remove an extension registration. Returns the key."
  [k]
  (swap! registry dissoc k)
  k)

(defn clear-all!
  "Remove all registrations. Intended for testing only."
  []
  (reset! registry {})
  nil)
