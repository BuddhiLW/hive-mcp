(ns hive-mcp.agent.core
  "Agent delegation for two-tier LLM architecture.

   Provides drone delegation for task execution:
   - delegate-drone! for standard drone tasks (routes through agentic path)
   - delegate-agentic-drone! for in-process agentic execution with session store

   Architecture:
   - LLMBackend protocol for pluggable model backends (see agent.protocol)
   - hive-agent bridge for multi-turn agentic execution
   - Task-based model selection for OpenRouter free tier

   Usage:
     ;; Delegate to agentic drone (primary path)
     (delegate-agentic-drone! {:task \"Fix the bug in auth.clj\"
                               :files [\"src/auth.clj\"]})

     ;; Delegate to drone (convenience wrapper)
     (delegate-drone! {:task \"Write tests\"
                       :files [\"test/core_test.clj\"]})"
  (:require [hive-mcp.agent.mcp :as mcp]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.agent.registry :as registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Public API
;;; ============================================================
;; Protocol/Config/Registry APIs moved to:
;;   hive-mcp.agent.{protocol,config,registry}

;;; ============================================================
;;; Drone Delegation (delegated to agent/drone.clj)
;;; ============================================================

(defn delegate-drone!
  "Delegate a task to a drone (token-optimized leaf agent).
   Routes through the agentic path (in-process multi-turn loop).
   See hive-mcp.agent.drone/delegate-agentic! for full documentation."
  [opts]
  (drone/delegate-agentic! opts))

(defn delegate-agentic-drone!
  "Delegate a task to an in-process agentic drone with session store.
   This runs the full agentic loop in-process with Datalevin store.
   See hive-mcp.agent.drone/delegate-agentic! for full documentation."
  [opts]
  (drone/delegate-agentic! opts))

;;; ============================================================
;;; MCP Tool Definitions (delegated to agent/mcp.clj)
;;; ============================================================

(def tools
  "MCP tools for agent delegation. See agent/mcp.clj for definitions."
  (mcp/make-tools delegate-drone!))

;;; ============================================================
;;; Re-exports from agent.registry (for backwards compatibility)
;;; ============================================================

(def register-tools!
  "Register tools for agent delegation. Delegates to registry/register!"
  registry/register!)
