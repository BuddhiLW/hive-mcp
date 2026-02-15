(ns hive-mcp.tools.projectile
  "Projectile integration handlers for MCP.

   Provides project management capabilities through Emacs projectile,
   including project info, file listing, search, and navigation.

   Requires the hive-mcp-projectile addon to be loaded in Emacs."
  (:require [hive-mcp.emacs.client :as ec]
            [hive-mcp.emacs.elisp :as el]
            [hive-mcp.tools.core :refer [mcp-error]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Projectile Integration Handlers (requires hive-mcp-projectile addon)
;; =============================================================================

(defn projectile-addon-available?
  "Check if hive-mcp-projectile addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-projectile)")]
    (and success (= result "t"))))

(defn- eval-projectile
  "Evaluate projectile elisp, returning MCP response."
  [elisp]
  (let [{:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      (mcp-error (str "Error: " error)))))

(defn handle-projectile-info
  "Get current project info including name, root, type, and file count."
  [_]
  (log/info "projectile-info")
  (eval-projectile (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-project-info)))

(defn handle-projectile-files
  "List files in current project, optionally filtered by pattern."
  [{:keys [pattern]}]
  (log/info "projectile-files" {:pattern pattern})
  (eval-projectile
   (if pattern
     (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-project-files pattern)
     (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-project-files))))

(defn handle-projectile-find-file
  "Find files matching a filename in current project."
  [{:keys [filename]}]
  (log/info "projectile-find-file" {:filename filename})
  (eval-projectile (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-find-file filename)))

(defn handle-projectile-search
  "Search project for a pattern using ripgrep or grep."
  [{:keys [pattern]}]
  (log/info "projectile-search" {:pattern pattern})
  (eval-projectile (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-search pattern)))

(defn handle-projectile-recent
  "Get recently visited files in current project."
  [_]
  (log/info "projectile-recent")
  (eval-projectile (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-recent-files)))

(defn handle-projectile-list-projects
  "List all known projectile projects."
  [_]
  (log/info "projectile-list-projects")
  (eval-projectile (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-list-projects)))

(def tools
  "Projectile tool definitions for the MCP tool registry."
  [{:name "projectile_info"
    :description "Get current project info including name, root, type (with extended detection for npm, cargo, go-mod, etc.), and file count. Requires hive-mcp-projectile addon."
    :inputSchema {:type "object" :properties {}}
    :handler handle-projectile-info}

   {:name "projectile_files"
    :description "List files in current project, optionally filtered by glob pattern."
    :inputSchema {:type "object"
                  :properties {"pattern" {:type "string"
                                          :description "Optional glob pattern to filter files (e.g., '*.clj', 'src/*.ts')"}}
                  :required []}
    :handler handle-projectile-files}

   {:name "projectile_find_file"
    :description "Find files matching a filename in current project. Returns relative and absolute paths."
    :inputSchema {:type "object"
                  :properties {"filename" {:type "string"
                                           :description "Filename or partial filename to search for"}}
                  :required ["filename"]}
    :handler handle-projectile-find-file}

   {:name "projectile_search"
    :description "Search current project for a pattern using ripgrep (preferred) or grep. Returns file, line number, and matching content."
    :inputSchema {:type "object"
                  :properties {"pattern" {:type "string"
                                          :description "Search pattern (regex supported)"}}
                  :required ["pattern"]}
    :handler handle-projectile-search}

   {:name "projectile_recent"
    :description "Get recently visited files in current project."
    :inputSchema {:type "object" :properties {}}
    :handler handle-projectile-recent}

   {:name "projectile_list_projects"
    :description "List all known projectile projects with their roots, names, existence status, and detected types."
    :inputSchema {:type "object" :properties {}}
    :handler handle-projectile-list-projects}])
