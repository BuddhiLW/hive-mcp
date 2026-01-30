(ns hive-mcp.tools.consolidated.memory
  "Consolidated memory tool using CLI dispatcher pattern.

   Single entry point for all memory operations:
   - add: Store new memory entries
   - query: Query by type with filters
   - get: Get full entry by ID
   - search: Semantic vector search
   - promote: Increase duration tier
   - demote: Decrease duration tier
   - feedback: Submit helpfulness feedback
   - tags: Update entry tags
   - cleanup: Remove expired entries
   - expiring: List soon-to-expire entries"
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.memory :as mem]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Handler Registry
;; ============================================================

(def handlers
  "Map of command keywords to their handler functions."
  {:add         mem/handle-mcp-memory-add
   :query       mem/handle-mcp-memory-query
   :metadata    mem/handle-mcp-memory-query-metadata
   :get         mem/handle-mcp-memory-get-full
   :search      mem/handle-mcp-memory-search-semantic
   :duration    mem/handle-mcp-memory-set-duration
   :promote     mem/handle-mcp-memory-promote
   :demote      mem/handle-mcp-memory-demote
   :log_access  mem/handle-mcp-memory-log-access
   :feedback    mem/handle-mcp-memory-feedback
   :helpfulness mem/handle-mcp-memory-helpfulness-ratio
   :tags        mem/handle-mcp-memory-update-tags
   :cleanup     mem/handle-mcp-memory-cleanup-expired
   :expiring    mem/handle-mcp-memory-expiring-soon
   :migrate     mem/handle-mcp-memory-migrate-project
   :import      mem/handle-mcp-memory-import-json})

;; ============================================================
;; CLI Handler
;; ============================================================

(def handle-memory
  "CLI-style handler that dispatches on :command param."
  (make-cli-handler handlers))

;; ============================================================
;; Tool Definition
;; ============================================================

(def tool-def
  "MCP tool definition for consolidated memory operations."
  {:name "memory"
   :description "Consolidated memory operations. Commands: add, query, metadata, get, search, duration, promote, demote, log_access, feedback, helpfulness, tags, cleanup, expiring, migrate, import. Use 'help' command to list all available commands."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["add" "query" "metadata" "get" "search" "duration" "promote" "demote" "log_access" "feedback" "helpfulness" "tags" "cleanup" "expiring" "migrate" "import" "help"]
                                         :description "Command to execute"}
                              ;; add command params
                              "type" {:type "string"
                                      :enum ["note" "snippet" "convention" "decision" "axiom"]
                                      :description "[add/query] Type of memory entry"}
                              "content" {:type "string"
                                         :description "[add] Content of the memory entry"}
                              "tags" {:type "array"
                                      :items {:type "string"}
                                      :description "[add/query/tags] Tags for categorization"}
                              "duration" {:type "string"
                                          :enum ["ephemeral" "short" "medium" "long" "permanent"]
                                          :description "[add/query] Duration/TTL category"}
                              "directory" {:type "string"
                                           :description "[add/query] Working directory for project scope"}
                              "agent_id" {:type "string"
                                          :description "[add] Agent identifier for attribution"}
                              "kg_implements" {:type "array"
                                               :items {:type "string"}
                                               :description "[add] Entry IDs this implements (KG edge)"}
                              "kg_supersedes" {:type "array"
                                               :items {:type "string"}
                                               :description "[add] Entry IDs this supersedes (KG edge)"}
                              "kg_depends_on" {:type "array"
                                               :items {:type "string"}
                                               :description "[add] Entry IDs this depends on (KG edge)"}
                              "kg_refines" {:type "array"
                                            :items {:type "string"}
                                            :description "[add] Entry IDs this refines (KG edge)"}
                              "abstraction_level" {:type "integer"
                                                   :minimum 1
                                                   :maximum 4
                                                   :description "[add] Abstraction level 1-4"}
                              ;; query/get params
                              "limit" {:type "integer"
                                       :description "[query/search] Maximum number of results"}
                              "scope" {:type "string"
                                       :description "[query] Scope filter: nil=auto, 'all', 'global', or specific"}
                              ;; get/promote/demote/feedback/tags params
                              "id" {:type "string"
                                    :description "[get/promote/demote/feedback/tags] Memory entry ID"}
                              ;; search params
                              "query" {:type "string"
                                       :description "[search] Natural language query for semantic search"}
                              ;; feedback params
                              "feedback" {:type "string"
                                          :enum ["helpful" "unhelpful"]
                                          :description "[feedback] Helpfulness rating"}
                              ;; expiring params
                              "days" {:type "integer"
                                      :description "[expiring] Days to look ahead (default: 7)"}}
                 :required ["command"]}
   :handler handle-memory})

(def tools
  "List of tool definitions for registration."
  [tool-def])
