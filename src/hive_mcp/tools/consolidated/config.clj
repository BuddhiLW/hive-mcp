(ns hive-mcp.tools.consolidated.config
  "Consolidated Config CLI tool for managing hive-mcp configuration."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.config.core :as config]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-get
  "Read a config value by dotted key path."
  [{:keys [key]}]
  (if (or (nil? key) (= "" key))
    (mcp-error "Missing required parameter: key. Example: config get {\"key\": \"embeddings.ollama.host\"}")
    (let [value (config/get-config-value key)]
      (mcp-json {:key key :value value}))))

(defn handle-set
  "Set a config value at a dotted key path and persist to disk."
  [{:keys [key value]}]
  (cond
    (or (nil? key) (= "" key))
    (mcp-error "Missing required parameter: key. Example: config set {\"key\": \"embeddings.ollama.host\", \"value\": \"http://new:11434\"}")

    ;; value can be nil (to clear a key), so we don't validate it
    :else
    (try
      (config/set-config-value! key value)
      (log/info "Config set:" key "=" value)
      (mcp-json {:key key :value value :status "updated"})
      (catch Exception e
        (mcp-error (str "Failed to set config: " (.getMessage e)))))))

(defn handle-list
  "List all configuration values."
  [_params]
  (mcp-json {:config (config/get-global-config)}))

(defn handle-reload
  "Reload config from disk, merging with defaults."
  [_params]
  (try
    (let [loaded (config/load-global-config!)]
      (mcp-json {:status "reloaded"
                 :keys (vec (keys loaded))}))
    (catch Exception e
      (mcp-error (str "Failed to reload config: " (.getMessage e))))))

(def handlers
  {:get    handle-get
   :set    handle-set
   :list   handle-list
   :reload handle-reload})

(def handle-config
  (make-cli-handler handlers))

(def tool-def
  {:name "config"
   :consolidated true
   :description "Manage ~/.config/hive-mcp/config.edn: get (read value at key path), set (write value at key path), list (show all config), reload (re-read from disk). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["get" "set" "list" "reload" "help"]
                                         :description "Config operation to perform"}
                              "key" {:type "string"
                                     :description "Dotted key path (e.g. \"embeddings.ollama.host\")"}
                              "value" {:description "Value to set (string, number, boolean, or object)"}}
                 :required ["command"]}
   :handler handle-config})

(def tools [tool-def])
