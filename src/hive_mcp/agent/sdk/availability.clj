(ns hive-mcp.agent.sdk.availability
  "SDK availability detection with graceful degradation."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private sdk-available? (atom nil))

(defn- check-libpython-available?
  "Check if libpython-clj2 is on the classpath."
  []
  (try
    (require 'libpython-clj2.python)
    true
    (catch Exception _
      (log/debug "[sdk.availability] libpython-clj2 not on classpath")
      false)))

(defn- check-sdk-available?
  "Check if claude-code-sdk Python package is importable."
  []
  (try
    (let [py-fn (requiring-resolve 'libpython-clj2.python/run-simple-string)]
      (py-fn "import claude_code_sdk")
      true)
    (catch Exception _
      (log/debug "[sdk.availability] claude-code-sdk Python package not found")
      false)))

(defn sdk-status
  "Return SDK availability as a keyword status."
  []
  (if-let [cached @sdk-available?]
    cached
    (let [status (cond
                   (not (check-libpython-available?))
                   :no-libpython

                   :else
                   (try
                     (let [init! (requiring-resolve 'libpython-clj2.python/initialize!)]
                       (init!)
                       (if (check-sdk-available?)
                         :available
                         :no-sdk))
                     (catch Exception e
                       (log/warn "[sdk.availability] Failed to initialize Python" (ex-message e))
                       :not-initialized)))]
      (reset! sdk-available? status)
      status)))

(defn available?
  "Returns true if the SDK backend is fully available."
  []
  (= :available (sdk-status)))

(defn reset-availability!
  "Reset cached availability check."
  []
  (reset! sdk-available? nil))
