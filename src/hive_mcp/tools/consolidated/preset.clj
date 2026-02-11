(ns hive-mcp.tools.consolidated.preset
  "Consolidated Preset CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.presets :as preset-handlers]
            [taoensso.timbre :as log]))

(defn- handle-list
  "List presets with verbosity control."
  [params]
  (if (= (:verbosity params) "slim")
    (preset-handlers/handle-preset-list-slim params)
    (preset-handlers/handle-preset-list params)))

(defn- handle-get
  "Get preset with verbosity control."
  [params]
  (if (= (:verbosity params) "core")
    (preset-handlers/handle-preset-core params)
    (preset-handlers/handle-preset-get params)))

(def ^:private deprecated-aliases
  {:list_slim {:canonical :list :params {:verbosity "slim"}}
   :core      {:canonical :get  :params {:verbosity "core"}}})

(defn- wrap-deprecated
  [alias-kw canonical-kw handler-fn param-overrides]
  (fn [params]
    (log/warn (str "DEPRECATED: command '" (name alias-kw)
                   "' is deprecated, use '" (name canonical-kw) "' instead."))
    (handler-fn (merge params param-overrides))))

(def canonical-handlers
  {:list      handle-list
   :get       handle-get
   :header    preset-handlers/handle-preset-header
   :search    preset-handlers/handle-preset-search
   :add       preset-handlers/handle-preset-add
   :delete    preset-handlers/handle-preset-delete
   :status    preset-handlers/handle-preset-status
   :migrate   preset-handlers/handle-preset-migrate})

(def handlers
  (merge canonical-handlers
         (reduce-kv (fn [m alias-kw {:keys [canonical params]}]
                      (assoc m alias-kw
                             (wrap-deprecated alias-kw canonical
                                              (get canonical-handlers canonical)
                                              params)))
                    {} deprecated-aliases)))

(def handle-preset
  (make-cli-handler handlers))

(def tool-def
  {:name "preset"
   :consolidated true
   :description "Swarm preset management: list (all presets, verbosity: full|slim), get (by name, verbosity: full|core), header (generate system prompt header), search (semantic query), add (custom preset), delete (remove), status (integration info), migrate (from files to Chroma). Deprecated aliases: list_slim (use list+verbosity:slim), core (use get+verbosity:core). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["list" "list_slim" "get" "core" "header" "search" "add" "delete" "status" "migrate" "help"]
                                         :description "Preset operation to perform"}
                              "verbosity" {:type "string"
                                           :enum ["full" "slim" "core"]
                                           :description "Verbosity level. list: full (default) or slim (names+categories). get: full (default) or core (~200 token summary)."}
                              "name" {:type "string"
                                      :description "Preset name"}
                              "presets" {:type "array"
                                         :items {:type "string"}
                                         :description "Vector of preset names for header generation"}
                              "lazy" {:type "boolean"
                                      :description "If true (default), return compact header with fetch instructions (~300 tokens). If false, return full concatenated content."}
                              "query" {:type "string"
                                       :description "Natural language search query"}
                              "limit" {:type "integer"
                                       :description "Maximum results to return"}
                              "category" {:type "string"
                                          :enum ["testing" "coding" "architecture" "coordination" "workflow" "general"]
                                          :description "Filter by category"}
                              "content" {:type "string"
                                         :description "Full markdown content of preset"}
                              "tags" {:type "array"
                                      :items {:type "string"}
                                      :description "Tags for searchability"}
                              "directory" {:type "string"
                                           :description "Path to directory containing .md preset files"}}
                 :required ["command"]}
   :handler handle-preset})

(def tools [tool-def])
