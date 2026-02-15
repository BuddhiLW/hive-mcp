(ns hive-mcp.tools.consolidated.config
  "Consolidated Config CLI tool for managing hive-mcp configuration."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dns.result :as result]
            [hive-mcp.config.core :as config]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ── Pure Result-returning functions ───────────────────────────────────────────

(defn- get*
  [{:keys [key]}]
  (if (or (nil? key) (= "" key))
    (result/err :config/get {:message "Missing required parameter: key. Example: config get {\"key\": \"embeddings.ollama.host\"}"})
    (result/ok {:key key :value (config/get-config-value key)})))

(defn- set*
  [{:keys [key value]}]
  (if (or (nil? key) (= "" key))
    (result/err :config/set {:message "Missing required parameter: key. Example: config set {\"key\": \"embeddings.ollama.host\", \"value\": \"http://new:11434\"}"})
    (do
      (config/set-config-value! key value)
      (log/info "Config set:" key "=" value)
      (result/ok {:key key :value value :status "updated"}))))

(defn- list-config*
  [_params]
  (result/ok {:config (config/get-global-config)}))

(defn- reload*
  [_params]
  (let [loaded (config/load-global-config!)]
    (result/ok {:status "reloaded"
                :keys (vec (keys loaded))})))

;; ── Public handlers (MCP boundary) ────────────────────────────────────────────

(defn handle-get
  "Read a config value by dotted key path."
  [params]
  (rb/result->mcp (rb/try-result :config/get-failed #(get* params))))

(defn handle-set
  "Set a config value at a dotted key path and persist to disk."
  [params]
  (rb/result->mcp (rb/try-result :config/set-failed #(set* params))))

(defn handle-list
  "List all configuration values."
  [params]
  (rb/result->mcp (rb/try-result :config/list-failed #(list-config* params))))

(defn handle-reload
  "Reload config from disk, merging with defaults."
  [params]
  (rb/result->mcp (rb/try-result :config/reload-failed #(reload* params))))

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
