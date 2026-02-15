(ns hive-mcp.tools.registry
  "MCP tool definitions registry — aggregates tool definitions from
   domain-specific modules under hive-mcp.tools.*

   Moved from hive-mcp.tools (now a backward-compat facade).

   Capability-based tool switching:
   - When Chroma is available: exposes mcp_mem_kanban_* tools (memory-based)
   - When Chroma is unavailable: exposes org_kanban_native_* tools (fallback)"
  (:require ;; Domain-specific tool modules
   [hive-mcp.tools.buffer :as buffer]
   [hive-mcp.tools.memory-kanban :as mem-kanban]
   [hive-mcp.tools.cider :as cider]
   [hive-mcp.tools.projectile :as projectile]
   [hive-mcp.tools.kanban :as kanban]
   [hive-mcp.tools.swarm.claim :as claim]
   [hive-mcp.tools.presets :as presets-tools]
   [hive-mcp.tools.diff :as diff]
   [hive-mcp.tools.crystal :as crystal]
   [hive-mcp.tools.session-complete :as session-complete]
   [hive-mcp.tools.hive-project :as hive-project]
   [hive-mcp.tools.olympus :as olympus]
   [hive-mcp.tools.agora :as agora]
   [hive-mcp.tools.cost :as cost]
   [hive-mcp.tools.delegate :as delegate]
   [hive-mcp.plan.tool :as plan]
   [hive-mcp.channel.core :as channel]
   [hive-mcp.agent.core :as agent]
   [hive-mcp.chroma.core :as chroma]
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
   [hive-mcp.tools.composite :as composite]
   [hive-mcp.extensions.registry :as ext]
   [hive-mcp.tools.consolidated.project :as c-project]
   [hive-mcp.tools.consolidated.session :as c-session]
   [hive-mcp.tools.consolidated.emacs :as c-emacs]
   [hive-mcp.tools.consolidated.wave :as c-wave]
   [hive-mcp.tools.consolidated.migration :as c-migration]
   [hive-mcp.tools.consolidated.config :as c-config]
   [hive-mcp.tools.consolidated.workflow :as c-workflow]
   [hive-mcp.tools.consolidated.multi :as c-multi]
   ;; Backward-compatibility shims for deprecated tools
   [hive-mcp.tools.compat :as compat]
   [clojure.string :as str]
   [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Capability-Based Kanban Tool Switching
;; =============================================================================

;; org-kanban-native tools REMOVED — org_clj module deleted (dead code W1)

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
               cider/tools
               projectile/tools
               claim/tools
               presets-tools/tools
               diff/tools
               session-complete/tools
               hive-project/tools
               olympus/tools
               agora/tools
               cost/tools
               delegate/tools
               plan/tools
               channel/channel-tools
               agent/tools
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
               ;; analysis tool built dynamically via composite/build-composite-tool
               c-project/tools
               c-session/tools
               c-emacs/tools
               c-wave/tools
               c-migration/tools  ; KG/Memory migration operations
               c-config/tools     ; Config management (~/.config/hive-mcp/config.edn)
               c-workflow/tools   ; Forja Belt workflow automation
               c-multi/tools     ; Meta-facade: single tool for all consolidated ops
               ;; Backward-compatibility shims (deprecated, sunset: 2026-04-01)
               ;; These provide old tool names (magit_status, mcp_memory_add, etc.)
               ;; and delegate to consolidated handlers
               compat/tools)))

;; Forward declarations for mutual references
(declare get-all-tools)

;; =============================================================================
;; Child Ling Tool Restriction (Self-Call Prevention)
;; =============================================================================

(def child-excluded-tool-names
  "Tool names that MUST be excluded from child ling MCP servers.

   Prevents recursive spawning chains (agent, wave, workflow) and
   coordinator-only operations (olympus, emacs, delegate).

   The 'multi' tool is excluded because it's a meta-facade that can
   route to any consolidated tool, bypassing per-tool exclusions.

   Defense-in-depth: Even if a tool name passes this filter, individual
   handler guards (spawn, wave, workflow) provide secondary protection."
  #{"agent"     ;; spawns/kills lings and drones -> recursive self-call
    "wave"      ;; dispatches drone waves -> resource amplification
    "workflow"  ;; forge-strike spawns lings -> recursive self-call
    "multi"     ;; meta-facade routes to excluded tools -> bypass vector
    "delegate"  ;; drone delegation -> resource amplification
    "olympus"   ;; Emacs grid control -> coordinator-only
    "emacs"})   ;; direct Emacs operations -> coordinator-only

(def ^:private child-excluded-deprecated-prefixes
  "Prefixes of deprecated shim names that map to excluded categories.

   Deprecated shims like swarm_spawn -> agent, swarm_kill -> agent,
   delegate_drone -> delegate are excluded. But hivemind_shout -> hivemind,
   mcp_memory_add -> memory, magit_status -> magit are safe to keep."
  ["swarm_"      ;; swarm_spawn/kill/dispatch/etc -> agent (excluded)
   "delegate_"   ;; delegate_drone/validated_wave -> delegate (excluded)
   "lings_"])    ;; lings_available -> agent (excluded)

(defn- child-ling-excluded?
  "Check if a tool should be excluded from child ling servers.

   Matches on:
   1. Exact tool name against child-excluded-tool-names
   2. Deprecated shims whose names map to excluded categories
      (swarm_*, delegate_*, lings_*) — NOT all deprecated tools.

   Safe deprecated shims (hivemind_*, mcp_memory_*, magit_*, kg_*)
   are kept since their consolidated parents are safe for child lings."
  [{:keys [name deprecated]}]
  (or (contains? child-excluded-tool-names name)
      ;; Only exclude deprecated shims that delegate to excluded categories
      (when deprecated
        (some #(str/starts-with? name %) child-excluded-deprecated-prefixes))))

(defn get-child-ling-tools
  "Get restricted tools for child ling MCP servers.

   Excludes tools that could cause recursive self-calls (agent spawn,
   wave dispatch, workflow forge-strike) and coordinator-only tools
   (olympus grid, emacs operations, multi meta-facade, delegate).

   Excludes deprecated shims that map to excluded categories (swarm_*,
   delegate_*, lings_*) but keeps safe ones (hivemind_*, mcp_memory_*, etc.).

   NOTE: Not to be confused with get-child-tools which returns
   sub-commands for a consolidated tool (introspection).

   Returns: Vector of tool definitions safe for child ling use."
  []
  (let [all (get-all-tools :include-deprecated? true)]
    (filterv (complement child-ling-excluded?) all)))

;; =============================================================================
;; Dynamic Tool Aggregation
;; =============================================================================

(defn get-all-tools
  "Get ALL tools including deprecated shims (for dispatch/calling).

   This ensures backward compatibility for existing code that calls them directly.

   Parameters:
   - include-deprecated?: boolean, when true includes deprecated shims

   Returns: Vector of tool definitions."
  [& {:keys [include-deprecated?] :or {include-deprecated? true}}]
  (let [chroma-up? (chroma/chroma-available?)
        base (get-base-tools)
        all-tools (if chroma-up?
                    ;; Chroma available: use mem-kanban
                    (vec (concat base mem-kanban/tools))
                    ;; Chroma unavailable: use kanban/tools (elisp addon) as fallback
                    (vec (concat base kanban/tools)))]
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
               mem-kanban/tools
               cider/tools
               projectile/tools
               kanban/tools
               claim/tools
               presets-tools/tools
               diff/tools
               session-complete/tools
               hive-project/tools
               olympus/tools
               agora/tools
               cost/tools
               delegate/tools
               plan/tools
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
               ;; analysis tool built dynamically via composite/build-composite-tool
               c-project/tools
               c-session/tools
               c-emacs/tools
               c-wave/tools
               c-migration/tools  ; KG/Memory migration operations
               c-config/tools     ; Config management (~/.config/hive-mcp/config.edn)
               c-workflow/tools   ; Forja Belt workflow automation
               c-multi/tools     ; Meta-facade: single tool for all consolidated ops
               ;; Backward-compatibility shims (deprecated, sunset: 2026-04-01)
               ;; These provide old tool names (magit_status, mcp_memory_add, etc.)
               ;; and delegate to consolidated handlers
               compat/tools)))

(defn get-tool-by-name
  "Find a tool definition by name."
  [name]
  (first (filter #(= (:name %) name) tools)))

;; =============================================================================
;; Consolidated Tool Introspection
;; =============================================================================

(defn- extract-commands
  "Extract command paths from a handler tree (same shape as cli.clj handlers).
   Returns seq of string command paths.

   Flat map:   {:spawn fn :status fn} => [\"spawn\" \"status\"]
   Nested map: {:dag {:start fn :stop fn :_handler fn}} => [\"dag\" \"dag start\" \"dag stop\"]"
  ([handlers] (extract-commands handlers []))
  ([handlers prefix]
   (reduce-kv
    (fn [acc k v]
      (if (= k :_handler)
        acc
        (cond
          (fn? v)
          (conj acc (str/join " " (map name (conj prefix k))))

          (map? v)
          (let [nested (extract-commands v (conj prefix k))
                with-default (if (contains? v :_handler)
                               (into [(str/join " " (map name (conj prefix k)))] nested)
                               nested)]
            (into acc with-default))

          :else acc)))
    [] handlers)))

(def ^:private consolidated-handler-maps
  "Map of consolidated tool name → their handler dispatch maps.
   Used by get-child-tools for sub-command introspection.

   Uses `handlers` (not canonical-handlers) to include deprecated aliases,
   since they remain valid callable commands."
  {:agent     c-agent/handlers
   :memory    c-memory/canonical-handlers
   :kg        c-kg/handlers
   :hivemind  c-hivemind/handlers
   :magit     c-magit/handlers
   :cider     c-cider/handlers
   :kanban    c-kanban/handlers
   :preset    c-preset/handlers
   :olympus   c-olympus/handlers
   :agora     c-agora/handlers
   ;; :analysis resolved dynamically via composite/build-composite-handlers
   :project   c-project/handlers
   :session   c-session/handlers
   :emacs     c-emacs/handlers
   :wave      c-wave/handlers
   :migration c-migration/handlers
   :config    c-config/handlers
   :workflow  c-workflow/handlers})

(defn get-child-tools
  "Get available sub-commands for a consolidated tool by name.

   Returns sorted vector of command path strings, or nil if tool not found.
   Supports nested handler trees (e.g. agent's dag sub-commands).
   Falls back to composite tool handlers for dynamically built tools.

   Example:
     (get-child-tools \"agent\")
     => [\"batch-spawn\" \"broadcast\" \"claims\" \"cleanup\" \"collect\"
         \"dag\" \"dag start\" \"dag status\" \"dag stop\"
         \"dispatch\" \"interrupt\" \"kill\" \"kill-batch\" \"list\"
         \"spawn\" \"status\"]

     (get-child-tools \"emacs\")
     => [\"buffers\" \"current\" \"eval\" \"find\" \"notify\"
         \"save\" \"status\" \"switch\"]

   Notes:
   - lsp and multi are not included (lsp uses requiring-resolve,
     multi is a meta-facade routing to other tools)
   - Returns nil for unknown tool names"
  [tool-name]
  (when-let [handlers (or (get consolidated-handler-maps (keyword tool-name))
                          (when (seq (ext/get-contributed-commands tool-name))
                            (composite/build-composite-handlers tool-name)))]
    (vec (sort (extract-commands handlers)))))
