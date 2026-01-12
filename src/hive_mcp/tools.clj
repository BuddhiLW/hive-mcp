(ns hive-mcp.tools
  "MCP tool definitions for Emacs interaction.
   
   This namespace aggregates tool definitions and their handlers from
   domain-specific modules under hive-mcp.tools.*"
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
   [hive-mcp.tools.jvm :as jvm]
   [hive-mcp.tools.kondo :as kondo]
   [hive-mcp.tools.crystal :as crystal]
   [hive-mcp.hivemind :as hivemind]
   [hive-mcp.channel :as channel]
   [hive-mcp.agent :as agent]))

(def tools
  "Aggregated tool definitions from domain-specific modules.

   Each module exports its own tools vector following the tool registry pattern.
   This enables Open/Closed Principle - new tools are added to their respective
   modules without modifying this aggregation."
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
               hivemind/tools
               channel/channel-tools
               agent/tools)))

(defn get-tool-by-name
  "Find a tool definition by name."
  [name]
  (first (filter #(= (:name %) name) tools)))
