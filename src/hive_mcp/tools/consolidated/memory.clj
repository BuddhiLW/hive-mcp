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
   - expiring: List soon-to-expire entries
   - expire: Force-expire (delete) entry by ID
   - decay: Scheduled staleness decay for low-access entries (W2)
   - cross_pollinate: Auto-promote entries with cross-project access (W5)
   - rename: Unified project rename (Chroma + KG + .edn + config)
   - batch-add: Batch-add multiple entries [{type, content, tags, ...}, ...]
   - batch-feedback: Batch-feedback on multiple entries [{id, feedback}, ...]
   - batch-get: Batch-get multiple entries by IDs [id1, id2, ...]"
  (:require [hive-mcp.tools.cli :refer [make-cli-handler make-batch-handler]]
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
   :expire      mem/handle-mcp-memory-expire
   :migrate     mem/handle-mcp-memory-migrate-project
   :import      mem/handle-mcp-memory-import-json
   :decay             mem/handle-mcp-memory-decay
   :cross_pollinate   mem/handle-mcp-memory-cross-pollination-promote
   :rename            mem/handle-mcp-memory-rename-project
   :batch-get         mem/handle-mcp-memory-batch-get})

;; ============================================================
;; Batch Helpers
;; ============================================================

(defn- make-single-command-batch
  "Wraps make-batch-handler for batch ops that all target one command.
   Auto-injects :command into each operation so callers don't need to."
  [cmd-kw handler-fn]
  (let [batch-fn (make-batch-handler {cmd-kw handler-fn})]
    (fn [{:keys [operations] :as params}]
      (batch-fn (assoc params :operations
                       (mapv #(assoc % :command (name cmd-kw)) operations))))))

(def canonical-handlers
  "Full handler map including batch commands."
  (assoc handlers
         :batch-add      (make-single-command-batch :add (:add handlers))
         :batch-feedback (make-single-command-batch :feedback (:feedback handlers))))

;; ============================================================
;; CLI Handler
;; ============================================================

(def handle-memory
  "CLI-style handler that dispatches on :command param."
  (make-cli-handler canonical-handlers))

;; ============================================================
;; Tool Definition
;; ============================================================

(def tool-def
  "MCP tool definition for consolidated memory operations."
  {:name "memory"
   :consolidated true
   :description "Consolidated memory operations. Commands: add, query, metadata, get, search, duration, promote, demote, log_access, feedback, helpfulness, tags, cleanup, expiring, expire, migrate, import, decay, cross_pollinate, rename, batch-add, batch-feedback, batch-get. Use 'help' command to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["add" "query" "metadata" "get" "search" "duration" "promote" "demote" "log_access" "feedback" "helpfulness" "tags" "cleanup" "expiring" "expire" "migrate" "import" "decay" "cross_pollinate" "rename" "batch-add" "batch-feedback" "batch-get" "help"]
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
                                       :description "[query/search/expiring] Maximum number of results"}
                              "scope" {:type "string"
                                       :description "[query] Scope filter: nil=auto, 'all', 'global', or specific"}
                              "verbosity" {:type "string"
                                           :enum ["full" "metadata"]
                                           :description "[query] Output detail: 'full' (default) returns complete entries, 'metadata' returns only id/type/preview/tags/created (~10x fewer tokens)"}
                              ;; get/promote/demote/feedback/tags params
                              "id" {:type "string"
                                    :description "[get/promote/demote/feedback/tags] Memory entry ID"}
                              "ids" {:type "array"
                                     :items {:type "string"}
                                     :description "[batch-get] Array of memory entry IDs to retrieve"}
                              ;; search params
                              "query" {:type "string"
                                       :description "[search] Natural language query for semantic search"}
                              ;; feedback params
                              "feedback" {:type "string"
                                          :enum ["helpful" "unhelpful"]
                                          :description "[feedback] Helpfulness rating"}
                              ;; expiring params
                              "days" {:type "integer"
                                      :description "[expiring] Days to look ahead (default: 7). Use 1-2 for short-duration memories"}
                              ;; HCR Wave 4: hierarchy params
                              "include_descendants" {:type "boolean"
                                                     :description "[query] Include child project memories in results (HCR Wave 4). Default false."}
                              ;; rename params
                              "old-project-id" {:type "string"
                                                :description "[rename/migrate] Old project-id to rename from"}
                              "new-project-id" {:type "string"
                                                :description "[rename/migrate] New project-id to rename to"}
                              "dry-run" {:type "boolean"
                                         :description "[rename] Preview what would happen without modifying (default: false)"}
                              ;; batch params
                              "operations" {:type "array"
                                            :items {:type "object"}
                                            :description "[batch-add/batch-feedback] Array of operation objects. batch-add: [{type, content, tags, ...}]. batch-feedback: [{id, feedback}]."}
                              "parallel" {:type "boolean"
                                          :description "[batch-add/batch-feedback] Run batch operations in parallel (default: false)"}}
                 :required ["command"]}
   :handler handle-memory})

(def tools
  "List of tool definitions for registration."
  [tool-def])
