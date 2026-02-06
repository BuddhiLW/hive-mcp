(ns hive-mcp.tools.consolidated.config
  "Consolidated Config CLI tool.

   Subcommands: get, set, list, reload

   Usage via MCP: config {\"command\": \"get\", \"key\": \"embeddings.ollama.host\"}
                  config {\"command\": \"set\", \"key\": \"embeddings.ollama.host\", \"value\": \"http://new:11434\"}
                  config {\"command\": \"list\"}
                  config {\"command\": \"reload\"}

   SOLID: Facade pattern - single tool entry point for config management.
   CLARITY: L - Thin adapter delegating to config domain module."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.config :as config]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Get Handler - Read value at dotted key path
;; =============================================================================

;; Read a config value by dotted key path.
;; Requires :key param (e.g. "embeddings.ollama.host").
;; Returns the value as JSON, or error if key missing.
(defn handle-get
  [{:keys [key]}]
  (if (or (nil? key) (= "" key))
    (mcp-error "Missing required parameter: key. Example: config get {\"key\": \"embeddings.ollama.host\"}")
    (let [value (config/get-config-value key)]
      (mcp-json {:key key :value value}))))

;; =============================================================================
;; Set Handler - Write value at dotted key path
;; =============================================================================

;; Set a config value at a dotted key path and persist to disk.
;; Requires :key and :value params.
;; Returns the updated value as confirmation.
(defn handle-set
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

;; =============================================================================
;; List Handler - Show all config
;; =============================================================================

;; List all configuration values currently in the cached config.
;; Returns the full config map as JSON.
(defn handle-list
  [_params]
  (mcp-json {:config (config/get-global-config)}))

;; =============================================================================
;; Reload Handler - Re-read from disk
;; =============================================================================

;; Reload config from disk, merging with defaults.
;; Useful after manual edits to config.edn.
(defn handle-reload
  [_params]
  (try
    (let [loaded (config/load-global-config!)]
      (mcp-json {:status "reloaded"
                 :keys (vec (keys loaded))}))
    (catch Exception e
      (mcp-error (str "Failed to reload config: " (.getMessage e))))))

;; =============================================================================
;; Handlers Map
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:get    handle-get
   :set    handle-set
   :list   handle-list
   :reload handle-reload})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-config
  "Unified CLI handler for config management."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated config command."
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

(def tools
  "Tool definitions for registration."
  [tool-def])
