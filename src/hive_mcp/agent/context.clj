(ns hive-mcp.agent.context
  "Thread-local execution context for agent tool calls. Minimal dependencies to avoid cycles."
  (:require))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *request-ctx*
  "Dynamic var holding request context during tool execution."
  nil)

(def ^:dynamic ^:deprecated *current-agent-id*
  "DEPRECATED: Use *request-ctx* instead."
  nil)

(defmacro with-request-context
  "Execute body with the given request context bound."
  [ctx & body]
  #_{:clj-kondo/ignore [:deprecated-var]}
  `(binding [*request-ctx* ~ctx
             *current-agent-id* (:agent-id ~ctx)]
     ~@body))

(defn current-agent-id
  "Get the current agent-id from execution context."
  []
  (or (:agent-id *request-ctx*)
      #_{:clj-kondo/ignore [:deprecated-var]}
      *current-agent-id*))

(defn current-project-id
  "Get the current project-id from execution context."
  []
  (:project-id *request-ctx*))

(defn current-directory
  "Get the current working directory from execution context."
  []
  (:directory *request-ctx*))

(defn current-session-id
  "Get the current session-id from execution context."
  []
  (:session-id *request-ctx*))

(defn current-timestamp
  "Get the request timestamp from execution context."
  []
  (:timestamp *request-ctx*))

(defn current-depth
  "Get the current nesting depth from execution context."
  []
  (:depth *request-ctx*))

(defn request-ctx
  "Get the full request context map."
  []
  *request-ctx*)

(defn make-request-ctx
  "Create a new request context map from the given options."
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
  "Return a new context with depth incremented."
  [ctx]
  (update ctx :depth (fnil inc 0)))
