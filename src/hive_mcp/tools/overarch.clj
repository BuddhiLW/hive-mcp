(ns hive-mcp.tools.overarch
  "MCP tools for Overarch architecture diagrams.
   Wraps the overarch CLI for EDN-based model rendering.

   CLARITY: A - Architectural performance (diagram generation)
   SOLID: SRP - Only MCP tool handlers for Overarch

   Provides MCP tools for:
   - overarch_render: Render models to plantuml/graphviz/markdown
   - overarch_export: Export models to JSON/Structurizr
   - overarch_select: Query model elements by criteria
   - overarch_help: Show CLI usage"
  (:require [babashka.process :as p]
            [clojure.string :as str]
            [hive-mcp.tools.core :refer [mcp-error mcp-json mcp-success]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private overarch-bin "/home/linuxbrew/.linuxbrew/bin/overarch")

(def ^:private valid-render-formats
  #{"all" "graphviz" "markdown" "plantuml"})

(def ^:private valid-export-formats
  #{"json" "structurizr"})

;; =============================================================================
;; CLI Execution
;; =============================================================================

(defn run-overarch!
  "Execute overarch CLI with given args using babashka.process.
   Returns {:success true :output ...} or {:success false :error ... :exit ...}"
  [& args]
  (log/debug "Running overarch" {:args args})
  (try
    (let [cmd (into [overarch-bin] args)
          result (p/shell {:out :string :err :string :continue true} cmd)
          exit-code (:exit result)]
      (if (zero? exit-code)
        {:success true
         :output (str/trim (:out result))}
        {:success false
         :error (or (not-empty (str/trim (:err result)))
                    (str/trim (:out result))
                    "Unknown error")
         :exit exit-code}))
    (catch java.io.IOException e
      {:success false
       :error (str "Failed to execute overarch: " (.getMessage e))
       :exit -1})))

(defn- validate-format
  "Validate format against allowed set. Returns nil if valid, error map if not."
  [format valid-formats param-name]
  (when (and format (not (contains? valid-formats format)))
    {:error (format "Invalid %s: '%s'. Must be one of: %s"
                    param-name format (str/join ", " (sort valid-formats)))}))

;; =============================================================================
;; Tool Handlers
;; =============================================================================

(defn handle-render
  "Render architecture models to specified format.
   Supports plantuml, graphviz, markdown, or 'all' formats."
  [{:keys [model_dir renderer output_dir format_subdirs]}]
  (log/info "overarch-render" {:model_dir model_dir :renderer renderer :output_dir output_dir})
  (let [model-dir (or model_dir "models")
        render-dir (or output_dir "export")
        render-format (or renderer "plantuml")]
    ;; Validate format
    (if-let [err (validate-format render-format valid-render-formats "render format")]
      (mcp-error (:error err))
      (let [args (cond-> ["-m" model-dir "-r" render-format "-R" render-dir]
                   (false? format_subdirs) (conj "--no-render-format-subdirs"))
            result (apply run-overarch! args)]
        (if (:success result)
          (mcp-json {:status "success"
                     :model_dir model-dir
                     :renderer render-format
                     :output_dir render-dir
                     :message (or (:output result) "Render complete")})
          (mcp-error (str "Render failed: " (:error result))))))))

(defn handle-export
  "Export architecture models to JSON or Structurizr format."
  [{:keys [model_dir format output_dir]}]
  (log/info "overarch-export" {:model_dir model_dir :format format :output_dir output_dir})
  (let [model-dir (or model_dir "models")
        export-dir (or output_dir "export")
        export-format (or format "json")]
    ;; Validate format
    (if-let [err (validate-format export-format valid-export-formats "export format")]
      (mcp-error (:error err))
      (let [result (run-overarch! "-m" model-dir "-x" export-format "-X" export-dir)]
        (if (:success result)
          (mcp-json {:status "success"
                     :model_dir model-dir
                     :format export-format
                     :output_dir export-dir
                     :message (or (:output result) "Export complete")})
          (mcp-error (str "Export failed: " (:error result))))))))

(defn handle-select
  "Select and query model elements by criteria.
   Returns matching elements from the model."
  [{:keys [model_dir criteria as_references]}]
  (log/info "overarch-select" {:model_dir model_dir :criteria criteria})
  (when-not criteria
    (mcp-error "criteria parameter is required"))
  (let [model-dir (or model_dir "models")
        flag (if as_references "-S" "-s")
        result (run-overarch! "-m" model-dir flag criteria)]
    (if (:success result)
      (mcp-json {:status "success"
                 :model_dir model-dir
                 :criteria criteria
                 :as_references (boolean as_references)
                 :result (:output result)})
      (mcp-error (str "Select failed: " (:error result))))))

(defn handle-help
  "Show overarch CLI help and usage information."
  [_]
  (log/info "overarch-help")
  (let [result (run-overarch! "--help")]
    (if (:success result)
      (mcp-success (:output result))
      (mcp-error (str "Failed to get help: " (:error result))))))

(defn handle-validate
  "Validate model files and check for warnings.
   Returns model info and any validation messages."
  [{:keys [model_dir]}]
  (log/info "overarch-validate" {:model_dir model_dir})
  (let [model-dir (or model_dir "models")
        result (run-overarch! "-m" model-dir "--model-warnings" "--model-info")]
    (if (:success result)
      (mcp-json {:status "success"
                 :model_dir model-dir
                 :validation (:output result)})
      (mcp-error (str "Validation failed: " (:error result))))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "overarch_render"
    :description "Render architecture models to diagram formats. Reads EDN model files and generates PlantUML, Graphviz, or Markdown output. Use for visualizing system architecture, component diagrams, and deployment views."
    :inputSchema {:type "object"
                  :properties {"model_dir" {:type "string"
                                            :description "Path to models directory (default: 'models')"}
                               "renderer" {:type "string"
                                           :enum ["plantuml" "graphviz" "markdown" "all"]
                                           :description "Output format (default: 'plantuml')"}
                               "output_dir" {:type "string"
                                             :description "Output directory (default: 'export')"}
                               "format_subdirs" {:type "boolean"
                                                 :description "Create subdirectories per format (default: true)"}}
                  :required []}
    :handler handle-render}

   {:name "overarch_export"
    :description "Export architecture models to data formats. Converts EDN models to JSON or Structurizr format for integration with other tools."
    :inputSchema {:type "object"
                  :properties {"model_dir" {:type "string"
                                            :description "Path to models directory (default: 'models')"}
                               "format" {:type "string"
                                         :enum ["json" "structurizr"]
                                         :description "Export format (default: 'json')"}
                               "output_dir" {:type "string"
                                             :description "Output directory (default: 'export')"}}
                  :required []}
    :handler handle-export}

   {:name "overarch_select"
    :description "Query model elements by criteria. Use to find specific elements, relationships, or views in the architecture model. Returns matching elements or references."
    :inputSchema {:type "object"
                  :properties {"model_dir" {:type "string"
                                            :description "Path to models directory (default: 'models')"}
                               "criteria" {:type "string"
                                           :description "Selection criteria (e.g., ':el :system' or '{:el :container :subtype :database}')"}
                               "as_references" {:type "boolean"
                                                :description "Return as references instead of full elements (default: false)"}}
                  :required ["criteria"]}
    :handler handle-select}

   {:name "overarch_validate"
    :description "Validate architecture model files. Checks for warnings, errors, and displays model info. Use to verify model integrity before rendering."
    :inputSchema {:type "object"
                  :properties {"model_dir" {:type "string"
                                            :description "Path to models directory (default: 'models')"}}
                  :required []}
    :handler handle-validate}

   {:name "overarch_help"
    :description "Show overarch CLI help and usage information. Lists all available options, formats, and examples."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-help}])
