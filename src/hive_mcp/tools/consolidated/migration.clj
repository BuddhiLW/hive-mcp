(ns hive-mcp.tools.consolidated.migration
  "Consolidated Migration CLI tool.

   Subcommands: status, backup, restore, list, switch, sync, export, import, validate, adapters

   Usage via MCP: migration {\"command\": \"status\"}

   SOLID: Facade pattern - single tool entry point for migration operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.migration :as migration-handlers]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:status   migration-handlers/cmd-status
   :backup   migration-handlers/cmd-backup
   :restore  migration-handlers/cmd-restore
   :list     migration-handlers/cmd-list
   :switch   migration-handlers/cmd-switch
   :sync     migration-handlers/cmd-sync
   :export   migration-handlers/cmd-export
   :import   migration-handlers/cmd-import
   :validate migration-handlers/cmd-validate
   :adapters migration-handlers/cmd-adapters})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-migration
  "Unified CLI handler for migration operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated migration command."
  {:name "migration"
   :consolidated true
   :description "KG/Memory migration: status (backend info), backup (timestamped snapshot), restore (from backup), list (available backups), switch (change backend), sync (mirror to secondary), export/import (with adapters), validate (check integrity), adapters (list available). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["status" "backup" "restore" "list" "switch" "sync" "export" "import" "validate" "adapters" "help"]
                                         :description "Migration operation to perform"}
                              ;; backup params
                              "scope" {:type "string"
                                       :enum ["kg" "memory" "full"]
                                       :description "Scope for backup/export (default: kg)"}
                              "dir" {:type "string"
                                     :description "Backup directory (default: data/backups)"}
                              "adapter" {:type "string"
                                         :enum ["edn" "json"]
                                         :description "Export/import adapter (default: edn)"}
                              ;; restore params
                              "path" {:type "string"
                                      :description "Backup file path (for restore/validate/export/import)"}
                              "latest" {:type "string"
                                        :enum ["kg" "memory"]
                                        :description "Restore latest backup for scope"}
                              "dry-run" {:type "boolean"
                                         :description "Preview without applying (default: false)"}
                              ;; switch params
                              "target" {:type "string"
                                        :enum ["datascript" "datalevin" "datahike"]
                                        :description "Target backend for switch/sync"}
                              "db-path" {:type "string"
                                         :description "Database path for persistent backends"}
                              "backup" {:type "boolean"
                                        :description "Create backup before switch (default: true)"}
                              ;; list params
                              "backend" {:type "string"
                                         :description "Filter backups by backend"}
                              "limit" {:type "integer"
                                       :description "Max results (default: 20)"}}
                 :required ["command"]}
   :handler handle-migration})

(def tools
  "Tool definitions for registration."
  [tool-def])
