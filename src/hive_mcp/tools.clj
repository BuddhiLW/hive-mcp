(ns hive-mcp.tools
  "MCP tool definitions for Emacs interaction.

   This namespace aggregates tool definitions and their handlers from
   domain-specific modules under hive-mcp.tools.*

   Capability-based tool switching:
   - When Chroma is available: exposes mcp_mem_kanban_* tools (memory-based)
   - When Chroma is unavailable: exposes org_kanban_native_* tools (fallback)"
  (:require ;; Domain-specific tool modules (SOLID refactoring)
   [hive-mcp.tools.buffer :as buffer]
   ;; memory/tools REMOVED - compat shims delegate to consolidated
   [hive-mcp.tools.memory-kanban :as mem-kanban]
   [hive-mcp.tools.cider :as cider]
   ;; magit/tools REMOVED - compat shims delegate to consolidated
   [hive-mcp.tools.projectile :as projectile]
   [hive-mcp.tools.kanban :as kanban]
   ;; swarm require removed - tools now via compat shims
   [hive-mcp.tools.swarm.claim :as claim]
   [hive-mcp.tools.org :as org]
   [hive-mcp.tools.prompt :as prompt]
   [hive-mcp.tools.presets :as presets-tools]
   [hive-mcp.tools.diff :as diff]
   [hive-mcp.tools.kondo :as kondo]
   [hive-mcp.tools.scc :as scc]
   ;; kg/tools REMOVED - compat shims delegate to consolidated
   [hive-mcp.tools.crystal :as crystal]
   [hive-mcp.tools.hot :as hot]
   [hive-mcp.tools.health :as health]
   [hive-mcp.tools.drone-feedback :as drone-feedback]
   [hive-mcp.tools.session-complete :as session-complete]
   [hive-mcp.tools.hive-project :as hive-project]
   [hive-mcp.tools.telemetry :as telemetry]
   [hive-mcp.tools.olympus :as olympus]
   [hive-mcp.tools.agora :as agora]
   [hive-mcp.tools.seeds :as seeds]
   [hive-mcp.tools.cost :as cost]
   [hive-mcp.tools.routing :as routing]
   [hive-mcp.tools.delegate :as delegate]
   [hive-mcp.tools.overarch :as overarch]
   [hive-mcp.plan.tool :as plan]
   ;; hivemind/tools REMOVED - compat shims delegate to consolidated
   [hive-mcp.channel :as channel]
   [hive-mcp.agent :as agent]
   [hive-mcp.chroma :as chroma]
   ;; Consolidated CLI tools (new unified interface)
   [hive-mcp.tools.consolidated.agent :as c-agent]
   [hive-mcp.tools.consolidated.memory :as c-memory]
   [hive-mcp.tools.consolidated.kg :as c-kg]
   [hive-mcp.tools.consolidated.hivemind :as c-hivemind]
   [hive-mcp.tools.consolidated.magit :as c-magit]
   [hive-mcp.tools.consolidated.cider :as c-cider]
   [hive-mcp.tools.consolidated.kanban :as c-kanban]
   [hive-mcp.tools.consolidated.preset :as c-preset]
   [hive-mcp.tools.consolidated.olympus :as c-olympus]
   [hive-mcp.tools.consolidated.agora :as c-agora]
   [hive-mcp.tools.consolidated.analysis :as c-analysis]
   [hive-mcp.tools.consolidated.project :as c-project]
   [hive-mcp.tools.consolidated.session :as c-session]
   [hive-mcp.tools.consolidated.emacs :as c-emacs]
   [hive-mcp.tools.consolidated.wave :as c-wave]
   [hive-mcp.tools.consolidated.migration :as c-migration]
   ;; Backward-compatibility shims for deprecated tools
   [hive-mcp.tools.compat :as compat]
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

(defn ^:private get-base-tools
  "Get tools that are always included regardless of capability state.
   Excludes kanban tools (both mem-kanban and org-kanban) - these are added
   conditionally in get-filtered-tools based on Chroma availability.

   CRITICAL: This is a function, not a def, to support hot-reload.
   Each call dereferences the current var values from tool modules,
   ensuring handlers point to fresh function references after reload.

   Includes deprecated tool shims (compat/tools) for backward compatibility.
   These log warnings and delegate to consolidated handlers."
  []
  (vec (concat buffer/tools
               crystal/tools
               ;; memory/tools REMOVED - compat/tools provides shims → consolidated
               cider/tools
               ;; magit/tools REMOVED - compat/tools provides shims → consolidated
               projectile/tools
               ;; kanban/tools removed - now conditional on Chroma availability
               ;; swarm/tools removed - use compat/tools shims instead
               claim/tools  ; File claim management (claim_list, claim_clear)
               prompt/tools
               presets-tools/tools
               diff/tools
               ;; kondo/tools and scc/tools REMOVED - now in consolidated/analysis
               ;; kg/tools REMOVED - compat/tools provides shims → consolidated
               hot/tools     ; hot reload coordination tools
               health/tools  ; MCP health check
               drone-feedback/tools
               session-complete/tools  ; ling session lifecycle
               hive-project/tools      ; .hive-project.edn generator
               telemetry/tools         ; prometheus_query (CLARITY-T)
               olympus/tools           ; grid layout for swarm visualization
               agora/tools             ; Agora multi-ling dialogue (Nash equilibrium)
               seeds/tools             ; Seed memory import from markdown files
               cost/tools              ; Token cost tracking and budget management
               routing/tools           ; Smart model routing for drones
               delegate/tools          ; Unified delegate API (ADR-20260123161700)
               plan/tools              ; plan_to_kanban workflow (exploration -> kanban)
               overarch/tools          ; Overarch architecture diagram generation
               ;; hivemind/tools REMOVED - compat/tools provides shims → consolidated
               channel/channel-tools
               agent/tools
               ;; Consolidated CLI tools (new unified interface)
               ;; Each exposes single tool with subcommands: agent spawn|status|kill
               c-agent/tools
               c-memory/tools
               c-kg/tools
               c-hivemind/tools
               c-magit/tools
               c-cider/tools
               c-kanban/tools
               c-preset/tools
               c-olympus/tools
               c-agora/tools
               c-analysis/tools
               c-project/tools
               c-session/tools
               c-emacs/tools
               c-wave/tools
               c-migration/tools  ; KG/Memory migration operations
               ;; Backward-compatibility shims (deprecated, sunset: 2026-04-01)
               ;; These provide old tool names (magit_status, mcp_memory_add, etc.)
               ;; and delegate to consolidated handlers
               compat/tools)))

;; =============================================================================
;; Dynamic Tool Aggregation
;; =============================================================================

(defn get-all-tools
  "Get ALL tools including deprecated shims (for dispatch/calling).

   CLARITY: Deprecated tools are still callable during grace period.
   This ensures backward compatibility for existing code that calls them directly.

   Parameters:
   - include-deprecated?: boolean, when true includes deprecated shims

   Returns: Vector of tool definitions."
  [& {:keys [include-deprecated?] :or {include-deprecated? true}}]
  (let [chroma-up? (chroma/chroma-available?)
        base (get-base-tools)
        all-tools (if chroma-up?
                    ;; Chroma available: use mem-kanban only, no org-kanban tools
                    (vec (concat base
                                 mem-kanban/tools
                                 (org-tools-without-kanban)))
                    ;; Chroma unavailable: use kanban/tools (elisp addon) + org-kanban-native as fallback
                    (vec (concat base
                                 kanban/tools  ; elisp org-kanban addon (fallback when Chroma down)
                                 org/tools)))]
    (if include-deprecated?
      all-tools
      ;; Filter out deprecated tools (those with :deprecated true)
      (filterv #(not (:deprecated %)) all-tools))))

(defn get-consolidated-tools
  "Get only consolidated 'root' tools for minimal tool listing.

   Returns only tools with :consolidated true flag - the 16 unified
   command tools (agent, memory, kanban, kg, preset, magit, cider,
   emacs, wave, hivemind, agora, analysis, olympus, project, session).

   Use this for external tool discovery (bb-mcp, Claude Code) where
   showing 15 consolidated tools is cleaner than 200+ flat tools.

   All flat tools remain callable via get-all-tools for dispatch."
  []
  (filterv :consolidated (get-all-tools :include-deprecated? false)))

(defn get-filtered-tools
  "Get tools with capability-based kanban switching, EXCLUDING deprecated tools.

   PHASE 2 STRANGLE: Deprecated tools are excluded from tools/list response.
   They remain callable via get-all-tools (for backward compatibility).

   When Chroma is available:
   - Include mcp_mem_kanban_* tools (memory-based kanban)
   - Include org tools WITHOUT org_kanban_native_* (avoid duplication)
   - EXCLUDE kanban/tools (elisp org-kanban addon) to prevent LLM confusion

   When Chroma is unavailable:
   - Include kanban/tools (elisp org-kanban addon) as primary kanban
   - Include org tools WITH org_kanban_native_* (additional fallback)
   - Exclude mcp_mem_kanban_* tools

   CLARITY: Open/Closed - new capabilities can be added without modifying base tools.

   HOT-RELOAD: Calls (get-base-tools) to get fresh handler references.
   This ensures hot-reload updates propagate to tool dispatch."
  []
  (let [visible-tools (get-all-tools :include-deprecated? false)]
    (log/info "Filtered tools for listing:" (count visible-tools)
              "(deprecated tools hidden from tools/list)")
    visible-tools))

(def tools
  "Aggregated tool definitions from domain-specific modules.

   DEPRECATED for dynamic use. Prefer get-filtered-tools for capability-aware list.

   Each module exports its own tools vector following the tool registry pattern.
   This enables Open/Closed Principle - new tools are added to their respective
   modules without modifying this aggregation.

   NOTE: This static def includes ALL tools for backward compatibility.
   Use get-filtered-tools at server startup for capability-based filtering.

   Includes deprecated tool shims (compat/tools) for backward compatibility.
   These log deprecation warnings and delegate to consolidated handlers."
  (vec (concat buffer/tools
               crystal/tools
               ;; memory/tools REMOVED - compat/tools provides shims → consolidated
               mem-kanban/tools
               cider/tools
               ;; magit/tools REMOVED - compat/tools provides shims → consolidated
               projectile/tools
               kanban/tools
               ;; swarm/tools removed - use compat/tools shims instead
               claim/tools  ; File claim management (claim_list, claim_clear)
               org/tools
               prompt/tools
               presets-tools/tools
               diff/tools
               ;; kondo/tools and scc/tools REMOVED - now in consolidated/analysis
               ;; kg/tools REMOVED - compat/tools provides shims → consolidated
               hot/tools     ; hot reload coordination tools
               health/tools  ; MCP health check
               drone-feedback/tools
               session-complete/tools  ; ling session lifecycle
               hive-project/tools      ; .hive-project.edn generator
               telemetry/tools         ; prometheus_query (CLARITY-T)
               olympus/tools           ; grid layout for swarm visualization
               agora/tools             ; Agora multi-ling dialogue (Nash equilibrium)
               seeds/tools             ; Seed memory import from markdown files
               cost/tools              ; Token cost tracking and budget management
               routing/tools           ; Smart model routing for drones
               delegate/tools          ; Unified delegate API (ADR-20260123161700)
               plan/tools              ; plan_to_kanban workflow (exploration -> kanban)
               overarch/tools          ; Overarch architecture diagram generation
               ;; hivemind/tools REMOVED - compat/tools provides shims → consolidated
               channel/channel-tools
               agent/tools
               ;; Consolidated CLI tools (new unified interface)
               c-agent/tools
               c-memory/tools
               c-kg/tools
               c-hivemind/tools
               c-magit/tools
               c-cider/tools
               c-kanban/tools
               c-preset/tools
               c-olympus/tools
               c-agora/tools
               c-analysis/tools
               c-project/tools
               c-session/tools
               c-emacs/tools
               c-wave/tools
               c-migration/tools  ; KG/Memory migration operations
               ;; Backward-compatibility shims (deprecated, sunset: 2026-04-01)
               ;; These provide old tool names (magit_status, mcp_memory_add, etc.)
               ;; and delegate to consolidated handlers
               compat/tools)))

(defn get-tool-by-name
  "Find a tool definition by name."
  [name]
  (first (filter #(= (:name %) name) tools)))
