(ns hive-mcp.tools
  "MCP tool definitions for Emacs interaction.

   This namespace aggregates tool definitions and their handlers from
   domain-specific modules under hive-mcp.tools.*

   Capability-based tool switching:
   - When Chroma is available: exposes mcp_mem_kanban_* tools (memory-based)
   - When Chroma is unavailable: exposes org_kanban_native_* tools (fallback)"
  (:require ;; Domain-specific tool modules (SOLID refactoring)
   [hive-mcp.tools.buffer :as buffer]
   [hive-mcp.tools.memory :as memory]
   [hive-mcp.tools.memory-kanban :as mem-kanban]
   [hive-mcp.tools.cider :as cider]
   [hive-mcp.tools.magit :as magit]
   [hive-mcp.tools.projectile :as projectile]
   [hive-mcp.tools.kanban :as kanban]
   [hive-mcp.tools.swarm :as swarm]
   [hive-mcp.tools.org :as org]
   [hive-mcp.tools.prompt :as prompt]
   [hive-mcp.tools.presets :as presets-tools]
   [hive-mcp.tools.diff :as diff]
   [hive-mcp.tools.kondo :as kondo]
   [hive-mcp.tools.scc :as scc]
   [hive-mcp.tools.crystal :as crystal]
   [hive-mcp.tools.hot :as hot]
   [hive-mcp.tools.health :as health]
   [hive-mcp.tools.drone-feedback :as drone-feedback]
   [hive-mcp.tools.session-complete :as session-complete]
   [hive-mcp.tools.hive-project :as hive-project]
   [hive-mcp.tools.telemetry :as telemetry]
   [hive-mcp.hivemind :as hivemind]
   [hive-mcp.channel :as channel]
   [hive-mcp.agent :as agent]
   [hive-mcp.chroma :as chroma]
   [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Capability-Based Kanban Tool Switching
;; =============================================================================

(def ^:private org-kanban-native-tool-names
  "Tool names for org-kanban-native (fallback when Chroma unavailable)."
  #{"org_kanban_native_status" "org_kanban_native_move" "org_kanban_render"})

(defn- org-tools-without-kanban
  "Org tools excluding the kanban-native tools (used when Chroma is available)."
  []
  (filterv #(not (org-kanban-native-tool-names (:name %))) org/tools))

;; =============================================================================
;; Base Tools (always included)
;; =============================================================================

(def ^:private base-tools
  "Tools that are always included regardless of capability state.
   Excludes kanban tools (both mem-kanban and org-kanban) - these are added
   conditionally in get-filtered-tools based on Chroma availability."
  (vec (concat buffer/tools
               crystal/tools
               memory/tools
               cider/tools
               magit/tools
               projectile/tools
               ;; kanban/tools removed - now conditional on Chroma availability
               swarm/tools
               prompt/tools
               presets-tools/tools
               diff/tools
               kondo/tools
               scc/tools
               hot/tools     ; hot reload coordination tools
               health/tools  ; MCP health check
               drone-feedback/tools
               session-complete/tools  ; ling session lifecycle
               hive-project/tools      ; .hive-project.edn generator
               telemetry/tools         ; prometheus_query (CLARITY-T)
               hivemind/tools
               channel/channel-tools
               agent/tools)))

;; =============================================================================
;; Dynamic Tool Aggregation
;; =============================================================================

(defn get-filtered-tools
  "Get tools with capability-based kanban switching.

   When Chroma is available:
   - Include mcp_mem_kanban_* tools (memory-based kanban)
   - Include org tools WITHOUT org_kanban_native_* (avoid duplication)
   - EXCLUDE kanban/tools (elisp org-kanban addon) to prevent LLM confusion

   When Chroma is unavailable:
   - Include kanban/tools (elisp org-kanban addon) as primary kanban
   - Include org tools WITH org_kanban_native_* (additional fallback)
   - Exclude mcp_mem_kanban_* tools

   CLARITY: Open/Closed - new capabilities can be added without modifying base tools."
  []
  (let [chroma-up? (chroma/chroma-available?)]
    (log/info "Kanban capability check: Chroma available?" chroma-up?)
    (if chroma-up?
      ;; Chroma available: use mem-kanban only, no org-kanban tools
      (vec (concat base-tools
                   mem-kanban/tools
                   (org-tools-without-kanban)))
      ;; Chroma unavailable: use kanban/tools (elisp addon) + org-kanban-native as fallback
      (vec (concat base-tools
                   kanban/tools  ; elisp org-kanban addon (fallback when Chroma down)
                   org/tools)))))

(def tools
  "Aggregated tool definitions from domain-specific modules.

   DEPRECATED for dynamic use. Prefer get-filtered-tools for capability-aware list.

   Each module exports its own tools vector following the tool registry pattern.
   This enables Open/Closed Principle - new tools are added to their respective
   modules without modifying this aggregation.

   NOTE: This static def includes ALL tools for backward compatibility.
   Use get-filtered-tools at server startup for capability-based filtering."
  (vec (concat buffer/tools
               crystal/tools
               memory/tools
               mem-kanban/tools
               cider/tools
               magit/tools
               projectile/tools
               kanban/tools
               swarm/tools
               org/tools
               prompt/tools
               presets-tools/tools
               diff/tools
               kondo/tools
               scc/tools
               hot/tools     ; hot reload coordination tools
               health/tools  ; MCP health check
               drone-feedback/tools
               session-complete/tools  ; ling session lifecycle
               hive-project/tools      ; .hive-project.edn generator
               hivemind/tools
               channel/channel-tools
               agent/tools)))

(defn get-tool-by-name
  "Find a tool definition by name."
  [name]
  (first (filter #(= (:name %) name) tools)))
