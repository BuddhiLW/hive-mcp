(ns hive-mcp.agent.context
  "Agent execution context - provides thread-local state for tool execution.

   CRITICAL: This namespace MUST have minimal dependencies to avoid cycles.
   It exists specifically to break the executor<->hivemind cycle.

   Usage:
     ;; In tool handlers that need agent context:
     (require '[hive-mcp.agent.context :as ctx])

     ;; Get specific values:
     (ctx/current-agent-id)
     (ctx/current-project-id)
     (ctx/current-directory)

     ;; Or get full context:
     (ctx/request-ctx)

     ;; Bind context for execution:
     (ctx/with-request-context {:agent-id \"worker-1\" :project-id \"my-proj\"}
       (do-work))")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; -----------------------------------------------------------------------------
;; Dynamic Context Var
;; -----------------------------------------------------------------------------

(def ^:dynamic *request-ctx*
  "Dynamic var holding the full request context during tool execution.

   Shape:
     {:agent-id    \"swarm-worker-123\"  ; Agent identifier
      :project-id  \"hive-mcp\"          ; Project scope
      :directory   \"/path/to/project\"  ; Working directory
      :session-id  \"session-xxx\"       ; Session identifier
      :timestamp   #inst \"...\"         ; Request timestamp
      :depth       1}                    ; Nesting depth (for recursive calls)

   CRITICAL: This fixes the 'unknown-agent' attribution bug where
   drones calling hivemind_shout would show up as 'unknown-agent'
   because the CLAUDE_SWARM_SLAVE_ID env var isn't set in the
   MCP server JVM.

   Bound by executor/execute-tool-calls before executing any tools."
  nil)

;; Legacy support - deprecated but kept for backward compatibility
(def ^:dynamic ^:deprecated *current-agent-id*
  "DEPRECATED: Use *request-ctx* instead.
   Kept for backward compatibility during migration."
  nil)

;; -----------------------------------------------------------------------------
;; Context Binding Macro
;; -----------------------------------------------------------------------------

(defmacro with-request-context
  "Execute body with the given request context bound.

   Usage:
     (with-request-context {:agent-id \"worker-1\"
                            :project-id \"my-proj\"
                            :directory \"/path/to/proj\"}
       (do-tool-work))

   The context is available to all code in body via accessor functions
   or directly via *request-ctx*."
  [ctx & body]
  #_{:clj-kondo/ignore [:deprecated-var]}
  `(binding [*request-ctx* ~ctx
             ;; Also bind legacy var for backward compat
             *current-agent-id* (:agent-id ~ctx)]
     ~@body))

;; -----------------------------------------------------------------------------
;; Accessor Functions
;; -----------------------------------------------------------------------------

(defn current-agent-id
  "Get the current agent-id from execution context.

   Returns the agent-id bound during tool execution, or nil if
   called outside of a tool execution context.

   Fallback chain for tool handlers:
     1. Explicit agent_id parameter from caller
     2. (ctx/current-agent-id) - executor context
     3. (System/getenv \"CLAUDE_SWARM_SLAVE_ID\") - env var
     4. \"unknown-agent\" - last resort"
  []
  (or (:agent-id *request-ctx*)
      #_{:clj-kondo/ignore [:deprecated-var]}
      *current-agent-id*))

(defn current-project-id
  "Get the current project-id from execution context.

   Returns the project-id bound during tool execution, or nil if
   not available."
  []
  (:project-id *request-ctx*))

(defn current-directory
  "Get the current working directory from execution context.

   Returns the directory bound during tool execution, or nil if
   not available."
  []
  (:directory *request-ctx*))

(defn current-session-id
  "Get the current session-id from execution context.

   Returns the session-id bound during tool execution, or nil if
   not available."
  []
  (:session-id *request-ctx*))

(defn current-timestamp
  "Get the request timestamp from execution context.

   Returns the timestamp when the request context was created,
   or nil if not available."
  []
  (:timestamp *request-ctx*))

(defn current-depth
  "Get the current nesting depth from execution context.

   Returns the depth level (1 = top-level), useful for preventing
   infinite recursion in nested tool calls. Returns nil if not set."
  []
  (:depth *request-ctx*))

(defn request-ctx
  "Get the full request context map.

   Returns the complete context map bound during tool execution,
   or nil if called outside of a tool execution context."
  []
  *request-ctx*)

;; -----------------------------------------------------------------------------
;; Context Construction Helpers
;; -----------------------------------------------------------------------------

(defn make-request-ctx
  "Create a new request context map.

   Required:
     :agent-id - The agent identifier

   Optional (will use defaults if not provided):
     :project-id - Project scope
     :directory  - Working directory
     :session-id - Session identifier
     :timestamp  - Request timestamp (defaults to now)
     :depth      - Nesting depth (defaults to 1)"
  [{:keys [agent-id project-id directory session-id timestamp depth]
    :or {timestamp (java.util.Date.)
         depth 1}}]
  {:agent-id   agent-id
   :project-id project-id
   :directory  directory
   :session-id session-id
   :timestamp  timestamp
   :depth      depth})

(defn increment-depth
  "Return a new context with depth incremented.

   Useful when making nested/recursive tool calls to track depth."
  [ctx]
  (update ctx :depth (fnil inc 0)))
