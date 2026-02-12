(ns hive-mcp.tools.consolidated.lsp
  "Consolidated LSP tools facade for the multi interface.

   Delegates to lsp-mcp.tools/handle-lsp via requiring-resolve.
   lsp-mcp is an optional dependency — graceful error when unavailable.

   Operations: analyze, lint, callers, calls, graph, scc, hotspots, file, compare.
   Additional lsp-mcp commands (definitions, ns-graph, sync, status, references)
   are available when lsp-mcp is on the classpath."
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- resolve-lsp-handler
  "Lazily resolve lsp-mcp.tools/handle-lsp via requiring-resolve.
   Returns the handler fn or nil if lsp-mcp is not on classpath."
  []
  (try
    (requiring-resolve 'lsp-mcp.tools/handle-lsp)
    (catch Exception e
      (log/debug "lsp-mcp not available:" (ex-message e))
      nil)))

(def ^:private lsp-unavailable-error
  {:content [{:type "text"
              :text (pr-str {:error "lsp-mcp not available"
                             :hint  "Add lsp-mcp to deps.local.edn as {:local/root \"../lsp-mcp\"}"})}]
   :isError true})

(defn handle-lsp
  "Route LSP commands to lsp-mcp.tools/handle-lsp.
   Returns graceful error when lsp-mcp is not on classpath.

   Supports 'help' command locally (no lsp-mcp needed)."
  [{:keys [command] :as params}]
  (let [cmd (some-> command str .trim .toLowerCase)]
    (if (= "help" cmd)
      {:type "text"
       :text (str "LSP analysis tool — Clojure LSP code intelligence + static analysis.\n\n"
                  "Commands:\n"
                  "  analyze   — Analyze project structure (files, namespaces, vars)\n"
                  "  lint      — Run clj-kondo lint on project\n"
                  "  callers   — Find callers of a function\n"
                  "  calls     — Find calls made by a function/namespace\n"
                  "  graph     — Generate namespace dependency graph\n"
                  "  scc       — Code metrics via scc (lines, complexity)\n"
                  "  hotspots  — Find complexity hotspots\n"
                  "  file      — Analyze a single file\n"
                  "  compare   — Compare two directories\n\n"
                  "Additional commands (when lsp-mcp available):\n"
                  "  definitions — List var definitions in project/namespace\n"
                  "  ns-graph    — Namespace dependency graph (LSP-based)\n"
                  "  sync        — Sync analysis results to Knowledge Graph\n"
                  "  status      — Check LSP bridge and cache status\n"
                  "  references  — Find references to a function\n\n"
                  "Requires lsp-mcp on classpath. Add to deps.local.edn:\n"
                  "  :lsp-mcp {:local/root \"../lsp-mcp\"}")}
      (if-let [handler (resolve-lsp-handler)]
        (handler params)
        lsp-unavailable-error))))

(def tool-def
  {:name "lsp"
   :consolidated true
   :description (str "LSP analysis: analyze (project structure), lint (kondo errors), "
                     "callers/calls (call graph), graph (namespace deps), "
                     "scc (code metrics), hotspots (complexity), "
                     "file (single file), compare (diff dirs). "
                     "Use command='help' to list all.")
   :inputSchema {:type "object"
                 :properties {"command"      {:type "string"
                                              :enum ["analyze" "lint" "callers" "calls" "graph"
                                                     "scc" "hotspots" "file" "compare" "help"]
                                              :description "LSP operation to perform"}
                              "path"         {:type "string"
                                              :description "Path to file or directory to analyze"}
                              "project_root" {:type "string"
                                              :description "Path to the project root directory"}
                              "project_id"   {:type "string"
                                              :description "Project identifier for KG sync"}
                              "scope"        {:type "string"
                                              :description "Scope for KG sync operations"}
                              "namespace"    {:type "string"
                                              :description "Filter by namespace (e.g., my.app.core)"}
                              "function"     {:type "string"
                                              :description "Filter by function name"}
                              "ns"           {:type "string"
                                              :description "Namespace of the target/source function"}
                              "var_name"     {:type "string"
                                              :description "Name of the function"}
                              "level"        {:type "string"
                                              :enum ["error" "warning" "info"]
                                              :description "Minimum severity level (lint)"}
                              "threshold"    {:type "number"
                                              :description "Minimum complexity for hotspots (default: 20)"}
                              "file_path"    {:type "string"
                                              :description "Path to specific file (file command)"}
                              "path_a"       {:type "string"
                                              :description "First directory for comparison"}
                              "path_b"       {:type "string"
                                              :description "Second directory for comparison"}}
                 :required ["command"]}
   :handler handle-lsp})

(def tools [tool-def])
