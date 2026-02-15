(ns hive-mcp.tools.composite
  "Build consolidated MCP tools dynamically from addon command contributions.

   Produces tool definitions identical in shape to other consolidated tools
   (:name, :consolidated, :description, :inputSchema, :handler).

   New addons auto-extend the composite command tree without touching hive-mcp core."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.cli :as cli]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Composite Handler Builder
;; =============================================================================

(defn build-composite-handler
  "Build a handler fn that dispatches to contributed addon handlers.
   Re-resolves contributions on each call so hot-reload picks up changes."
  [tool-name]
  (fn [params]
    (let [commands (ext/get-contributed-commands tool-name)
          handlers (into {} (map (fn [[cmd {:keys [handler]}]]
                                   [(keyword cmd) handler])
                                 commands))
          cli-fn (cli/make-cli-handler handlers)]
      (cli-fn params))))

;; =============================================================================
;; Composite Tool Definition Builder
;; =============================================================================

(defn build-composite-tool
  "Build a consolidated tool definition from addon contributions.
   description-prefix: e.g. \"Code analysis\"
   Returns tool-def map identical in shape to other consolidated tools."
  [tool-name description-prefix]
  (let [commands (ext/get-contributed-commands tool-name)
        cmd-names (vec (sort (keys commands)))
        all-params (apply merge-with merge (map :params (vals commands)))
        handler (build-composite-handler tool-name)]
    {:name tool-name
     :consolidated true
     :composite true
     :description (str description-prefix ": "
                       (str/join ", " cmd-names)
                       ". Use command='help' to list all.")
     :inputSchema {:type "object"
                   :properties (merge
                                {"command" {:type "string"
                                            :enum (conj cmd-names "help")
                                            :description (str tool-name " operation to perform")}}
                                all-params)
                   :required ["command"]}
     :handler handler}))

;; =============================================================================
;; Handler Map for Registry Introspection
;; =============================================================================

(defn build-composite-handlers
  "Build handler map for registry introspection (consolidated-handler-maps).
   Returns keyword->fn map compatible with cli/extract-commands."
  [tool-name]
  (let [commands (ext/get-contributed-commands tool-name)]
    (into {:help (fn [_] {:type "text" :text "help"})}
          (map (fn [[cmd {:keys [handler]}]]
                 [(keyword cmd) handler])
               commands))))

;; =============================================================================
;; Batch Builder
;; =============================================================================

(defn build-all-composite-tools
  "Build tool definitions for all tool names that have contributions.
   descriptions: map of tool-name -> description prefix."
  [descriptions]
  (vec (for [tool-name (ext/contributed-tool-names)
             :let [desc (get descriptions tool-name tool-name)]]
         (build-composite-tool tool-name desc))))
