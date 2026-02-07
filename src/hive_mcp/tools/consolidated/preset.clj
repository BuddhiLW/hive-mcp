(ns hive-mcp.tools.consolidated.preset
  "Consolidated Preset CLI tool.

   Subcommands: list, get, header, search, add, delete, status, migrate
   Deprecated aliases: list_slim → list (verbosity=slim), core → get (verbosity=core)

   Verbosity:
   - list supports verbosity: full (default) | slim (names+categories only)
   - get supports verbosity: full (default) | core (~200 token summary)
   - header already has lazy:bool (same pattern)

   Usage via MCP: preset {\"command\": \"list\", \"verbosity\": \"slim\"}

   SOLID: Facade pattern - single tool entry point for preset operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.presets :as preset-handlers]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Verbosity-aware wrappers
;; =============================================================================

(defn- handle-list
  "List presets with verbosity control.
   verbosity: 'full' (default) - all fields, 'slim' - names+categories only."
  [params]
  (if (= (:verbosity params) "slim")
    (preset-handlers/handle-preset-list-slim params)
    (preset-handlers/handle-preset-list params)))

(defn- handle-get
  "Get preset with verbosity control.
   verbosity: 'full' (default) - complete content, 'core' - minimal summary."
  [params]
  (if (= (:verbosity params) "core")
    (preset-handlers/handle-preset-core params)
    (preset-handlers/handle-preset-get params)))

;; =============================================================================
;; Deprecated Alias Support
;; =============================================================================

(def ^:private deprecated-aliases
  "Map of deprecated command keywords to their canonical replacements."
  {:list_slim :list
   :core      :get})

(defn- wrap-deprecated
  "Wrap a handler fn to emit a deprecation warning before delegating."
  [alias-kw canonical-kw handler-fn]
  (fn [params]
    (log/warn (str "DEPRECATED: command '" (name alias-kw)
                   "' is deprecated, use '" (name canonical-kw) "' instead."))
    (handler-fn params)))

;; =============================================================================
;; Handlers Map
;; =============================================================================

(def canonical-handlers
  "Map of canonical command keywords to handler functions."
  {:list      handle-list
   :get       handle-get
   :header    preset-handlers/handle-preset-header
   :search    preset-handlers/handle-preset-search
   :add       preset-handlers/handle-preset-add
   :delete    preset-handlers/handle-preset-delete
   :status    preset-handlers/handle-preset-status
   :migrate   preset-handlers/handle-preset-migrate})

(def handlers
  "Canonical handlers merged with deprecated aliases (with log warnings)."
  (merge canonical-handlers
         (reduce-kv (fn [m alias-kw canonical-kw]
                      (assoc m alias-kw
                             (wrap-deprecated alias-kw canonical-kw
                                              (get canonical-handlers canonical-kw))))
                    {} deprecated-aliases)))

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-preset
  "Unified CLI handler for preset operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated preset command."
  {:name "preset"
   :consolidated true
   :description "Swarm preset management: list (all presets, verbosity: full|slim), get (by name, verbosity: full|core), header (generate system prompt header), search (semantic query), add (custom preset), delete (remove), status (integration info), migrate (from files to Chroma). Deprecated aliases: list_slim (use list+verbosity:slim), core (use get+verbosity:core). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["list" "list_slim" "get" "core" "header" "search" "add" "delete" "status" "migrate" "help"]
                                         :description "Preset operation to perform"}
                              ;; verbosity control (for list and get)
                              "verbosity" {:type "string"
                                           :enum ["full" "slim" "core"]
                                           :description "Verbosity level. list: full (default) or slim (names+categories). get: full (default) or core (~200 token summary)."}
                              ;; get/delete/core params
                              "name" {:type "string"
                                      :description "Preset name"}
                              ;; header params
                              "presets" {:type "array"
                                         :items {:type "string"}
                                         :description "Vector of preset names for header generation"}
                              "lazy" {:type "boolean"
                                      :description "If true (default), return compact header with fetch instructions (~300 tokens). If false, return full concatenated content."}
                              ;; search params
                              "query" {:type "string"
                                       :description "Natural language search query"}
                              "limit" {:type "integer"
                                       :description "Maximum results to return"}
                              "category" {:type "string"
                                          :enum ["testing" "coding" "architecture" "coordination" "workflow" "general"]
                                          :description "Filter by category"}
                              ;; add params
                              "content" {:type "string"
                                         :description "Full markdown content of preset"}
                              "tags" {:type "array"
                                      :items {:type "string"}
                                      :description "Tags for searchability"}
                              ;; migrate params
                              "directory" {:type "string"
                                           :description "Path to directory containing .md preset files"}}
                 :required ["command"]}
   :handler handle-preset})

(def tools
  "Tool definitions for registration."
  [tool-def])
