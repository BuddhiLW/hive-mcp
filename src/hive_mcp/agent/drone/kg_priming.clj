(ns hive-mcp.agent.drone.kg-priming
  "Domain context priming for drone task augmentation via extension delegation."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.protocols.kg :as kg-proto]
            [hive-dsl.result :as r]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- ensure-kg-store!
  "Ensure KG store is initialized, auto-initializing via connection if needed."
  []
  (let [result (r/guard Exception false
                        (if (kg-proto/store-set?)
                          true
                          (do
                            (log/info "KG store not initialized, auto-initializing for priming")
                            (kg-conn/get-conn)
                            (kg-proto/store-set?))))]
    (when-let [err (::r/error (meta result))]
      (log/debug "KG store auto-initialization failed (non-fatal)"
                 {:error (:message err)}))
    result))

(defn kg-store-available?
  "Check if the KG store is currently initialized."
  []
  (kg-proto/store-set?))

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (if (ensure-kg-store!)
      (let [result (r/guard Exception default-val
                            (apply f args))]
        (when-let [err (::r/error (meta result))]
          (log/debug "Extension delegation failed, returning noop"
                     {:ext-key ext-key :error (:message err)}))
        result)
      (do
        (log/debug "KG store unavailable after auto-init attempt, noop for" ext-key)
        default-val))
    (do
      (log/debug "Extension not available, noop for" ext-key)
      default-val)))

(defn prime-context
  "Returns a markdown block with domain knowledge relevant to the task, or empty string."
  [{:keys [seeds] :as opts}]
  (if (seq seeds)
    (delegate-or-noop :pm/prime "" [opts])
    ""))

(defn resolve-seeds
  "Resolve seed topic strings into concrete references."
  [opts]
  (delegate-or-noop :pm/seeds [] [opts]))

(defn priming-available?
  "Check if the priming extension is available and KG store is accessible."
  []
  (and (ext/extension-available? :pm/prime)
       (kg-store-available?)))
