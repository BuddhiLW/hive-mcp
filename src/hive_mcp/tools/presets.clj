(ns hive-mcp.tools.presets
  "MCP tool handlers for swarm preset operations.

   Provides semantic search and management of swarm presets stored in Chroma.
   Presets can be queried by natural language (e.g., 'find testing-focused preset')."
  (:require [hive-mcp.presets :as presets]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.config :as config]
            [hive-mcp.tools.swarm.prompt :as prompt]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Handlers
;;; ============================================================

(defn handle-preset-search
  "Search presets using semantic similarity.
   Returns presets matching the natural language query."
  [{:keys [query limit category]}]
  (log/info "preset-search:" query)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str
            {:error "Chroma not configured"
             :message "Semantic search requires Chroma with embedding provider."
             :fallback "Use preset_list to see available presets by name."})
     :isError true}
    (try
      (let [results (presets/search-presets query
                                            :limit (or limit 5)
                                            :category category)]
        {:type "text"
         :text (json/write-str {:results results
                                :count (count results)
                                :query query})})
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Search failed: " (.getMessage e))})
         :isError true}))))

(defn handle-preset-get
  "Get full content of a specific preset by name."
  [{:keys [name]}]
  (log/info "preset-get:" name)
  (if-not (chroma/embedding-configured?)
    ;; Fallback to file-based
    (let [preset-dir (config/get-service-value :presets :dir :env "HIVE_MCP_PRESETS_DIR")
          preset-dir (or preset-dir
                         (str (System/getProperty "user.dir") "/presets"))]
      (if-let [preset (presets/get-preset-from-file preset-dir name)]
        {:type "text"
         :text (json/write-str {:preset preset
                                :source "file-fallback"})}
        {:type "text"
         :text (json/write-str {:error (str "Preset not found: " name)})
         :isError true}))
    (try
      (if-let [preset (presets/get-preset name)]
        {:type "text"
         :text (json/write-str {:preset preset})}
        {:type "text"
         :text (json/write-str {:error (str "Preset not found in Chroma: " name)
                                :hint "Try preset_list to see available presets"})
         :isError true})
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Failed to get preset: " (.getMessage e))})
         :isError true}))))

(defn handle-preset-list
  "List all available presets."
  [_]
  (log/info "preset-list")
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"
                            :message "Use file-based presets via swarm_list_presets"})}
    (try
      (let [presets (presets/list-presets)]
        {:type "text"
         :text (json/write-str {:presets presets
                                :count (count presets)})})
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Failed to list presets: " (.getMessage e))})
         :isError true}))))

(defn handle-preset-list-slim
  "List presets with minimal info (name + category only).
  Optimized for lazy-loading context - minimal token usage."
  [_]
  (log/info "preset list_slim")
  (try
    (let [presets (presets/list-presets)]
      {:type "text"
       :text (json/write-str
              {:presets (mapv #(select-keys % [:name :category]) presets)
               :count (count presets)})})
    (catch Exception e
      {:type "text"
       :text (json/write-str {:error (str "Failed: " (.getMessage e))})
       :isError true})))

(defn handle-preset-core
  "Get minimal summary of a preset for lazy loading.
   Returns ~200 tokens instead of full ~1500 token content.
   Use this when you need preset info but not the full instructions."
  [{:keys [name]}]
  (log/info "preset core:" name)
  (if-not (chroma/embedding-configured?)
    ;; Fallback to file-based
    (let [preset-dir (config/get-service-value :presets :dir :env "HIVE_MCP_PRESETS_DIR")
          preset-dir (or preset-dir
                         (str (System/getProperty "user.dir") "/presets"))]
      (if-let [preset (presets/get-preset-from-file preset-dir name)]
        {:type "text"
         :text (json/write-str {:core (presets/extract-preset-core preset)
                                :name name
                                :source "file-fallback"})}
        {:type "text"
         :text (json/write-str {:error (str "Preset not found: " name)})
         :isError true}))
    (try
      (if-let [preset (presets/get-preset name)]
        {:type "text"
         :text (json/write-str {:core (presets/extract-preset-core preset)
                                :name name
                                :source "chroma"})}
        {:type "text"
         :text (json/write-str {:error (str "Preset not found in Chroma: " name)
                                :hint "Try preset_list to see available presets"})
         :isError true})
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Failed to get preset core: " (.getMessage e))})
         :isError true}))))

(defn handle-preset-header
  "Generate preset header for system prompt.

   When lazy=true (default): returns compact header with names + query instructions (~300 tokens)
   When lazy=false: returns full preset content concatenated

   Params:
   - presets: vector of preset names
   - lazy: boolean (default true)

   Use case: elisp calls this to build system prompts for spawned lings."
  [{:keys [presets lazy]}]
  (log/info "preset header:" presets "lazy:" lazy)
  (let [lazy? (if (nil? lazy) true lazy)
        preset-names (if (sequential? presets) presets [presets])]
    (try
      (if lazy?
        ;; Lazy mode: compact header with fetch instructions
        {:type "text"
         :text (json/write-str {:header (prompt/build-lazy-preset-header preset-names)
                                :mode "lazy"
                                :preset-count (count preset-names)
                                :approx-tokens 300})}
        ;; Full mode: concatenate all preset content
        (let [preset-contents
              (for [name preset-names]
                (if-let [preset (or (presets/get-preset name)
                                    (let [dir (or (config/get-service-value :presets :dir :env "HIVE_MCP_PRESETS_DIR")
                                                  (str (System/getProperty "user.dir") "/presets"))]
                                      (presets/get-preset-from-file dir name)))]
                  {:name name :content (:content preset)}
                  {:name name :error "not found"}))
              full-content (->> preset-contents
                                (filter :content)
                                (map :content)
                                (str/join "\n\n---\n\n"))
              missing (->> preset-contents
                           (filter :error)
                           (map :name))]
          {:type "text"
           :text (json/write-str {:header full-content
                                  :mode "full"
                                  :preset-count (count preset-names)
                                  :approx-tokens (* 1500 (count preset-names))
                                  :missing (when (seq missing) missing)})}))
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Failed to build header: " (.getMessage e))})
         :isError true}))))

(defn handle-preset-migrate
  "Migrate presets from .md files to Chroma.
   Requires preset directory path."
  [{:keys [directory]}]
  (log/info "preset-migrate:" directory)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"
                            :message "Configure Chroma with embedding provider before migration."})
     :isError true}
    (try
      (let [result (presets/migrate-presets-from-dir! directory)]
        {:type "text"
         :text (json/write-str result)})
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Migration failed: " (.getMessage e))})
         :isError true}))))

(defn handle-preset-status
  "Get presets integration status."
  [_]
  (log/info "preset-status")
  (try
    (let [status (presets/status)]
      {:type "text"
       :text (json/write-str status)})
    (catch Exception e
      {:type "text"
       :text (json/write-str {:error (str "Status check failed: " (.getMessage e))})
       :isError true})))

(defn handle-preset-add
  "Add a custom preset to Chroma (not file-based)."
  [{:keys [name content category tags]}]
  (log/info "preset-add:" name)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"})
     :isError true}
    (try
      (let [preset {:id name
                    :name name
                    :title name
                    :content content
                    :category (or category "custom")
                    :tags (if (coll? tags)
                            (str/join "," tags)
                            (or tags name))
                    :source "custom"}
            id (presets/index-preset! preset)]
        {:type "text"
         :text (json/write-str {:success true
                                :id id
                                :message (str "Added preset: " name)})})
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Failed to add preset: " (.getMessage e))})
         :isError true}))))

(defn handle-preset-delete
  "Delete a preset from Chroma."
  [{:keys [name]}]
  (log/info "preset-delete:" name)
  (if-not (chroma/embedding-configured?)
    {:type "text"
     :text (json/write-str {:error "Chroma not configured"})
     :isError true}
    (try
      (presets/delete-preset! name)
      {:type "text"
       :text (json/write-str {:success true
                              :message (str "Deleted preset: " name)})}
      (catch Exception e
        {:type "text"
         :text (json/write-str {:error (str "Failed to delete preset: " (.getMessage e))})
         :isError true}))))

;;; ============================================================
;;; Tool Definitions
;;; ============================================================

(def tools
  [{:name "preset_search"
    :description "Search swarm presets using semantic similarity. Find presets by describing what you need (e.g., 'testing-focused preset', 'coordination for multi-agent work'). Returns matching presets ranked by relevance."
    :inputSchema {:type "object"
                  :properties {"query" {:type "string"
                                        :description "Natural language query describing the preset you need"}
                               "limit" {:type "integer"
                                        :description "Maximum results to return (default: 5)"}
                               "category" {:type "string"
                                           :enum ["testing" "coding" "architecture" "coordination" "workflow" "general"]
                                           :description "Filter by category"}}
                  :required ["query"]}
    :handler handle-preset-search}

   {:name "preset_get"
    :description "Get the full content of a specific preset by name. Use after preset_search to retrieve the complete preset content."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Name of the preset (e.g., 'tdd', 'clarity', 'hivemind')"}}
                  :required ["name"]}
    :handler handle-preset-get}

   {:name "preset_list"
    :description "List all available presets with their names, titles, and categories."
    :inputSchema {:type "object"
                  :properties {}}
    :handler handle-preset-list}

   {:name "preset_migrate"
    :description "Migrate presets from .md files to Chroma vector database. Run this once to enable semantic search over presets."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Path to directory containing .md preset files"}}
                  :required ["directory"]}
    :handler handle-preset-migrate}

   {:name "preset_status"
    :description "Get presets integration status including Chroma configuration and preset counts."
    :inputSchema {:type "object"
                  :properties {}}
    :handler handle-preset-status}

   {:name "preset_add"
    :description "Add a custom preset to Chroma. Use this to create project-specific or experimental presets."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Unique name for the preset"}
                               "content" {:type "string"
                                          :description "Full markdown content of the preset"}
                               "category" {:type "string"
                                           :enum ["testing" "coding" "architecture" "coordination" "workflow" "custom"]
                                           :description "Category for the preset (default: custom)"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Tags for searchability"}}
                  :required ["name" "content"]}
    :handler handle-preset-add}

   {:name "preset_delete"
    :description "Delete a preset from Chroma. Does not affect original .md files."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Name of the preset to delete"}}
                  :required ["name"]}
    :handler handle-preset-delete}])
