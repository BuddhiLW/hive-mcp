(ns hive-mcp.tools.cider
  "CIDER integration handlers for MCP.

   Provides Clojure REPL operations via CIDER:
   - Status checking and connection info
   - Silent and explicit code evaluation
   - Multi-session support for parallel agent work
   - Auto-connect fallback when CIDER is not connected

   Result DSL: Internal logic returns Result maps ({:ok val} or {:error category}).
   Single try-result boundary at each handler level. Zero nested try-catch."
  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.tools.core :refer [mcp-success mcp-error]]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.emacs.elisp :as el]
            [hive-mcp.telemetry.core :as telemetry]
            [hive-mcp.dns.validation :as v]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Result DSL Helpers (boundary pattern)
;;; =============================================================================

(defn- elisp->result
  "Execute elisp and convert response to Result.
   {:success true :result r} -> (ok r), {:success false :error e} -> (err ...)"
  [elisp]
  (let [{:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (result/ok result)
      (result/err :cider/elisp-failed {:message (str error)}))))

(defn- try-result
  "Execute thunk f returning Result; catch unexpected exceptions as error Result.
   Unlike try-effect, expects f to return a Result map directly."
  [category f]
  (try
    (f)
    (catch Exception e
      (log/error e (str (name category) " failed"))
      (result/err category {:message (.getMessage e)}))))

(defn- result->mcp
  "Convert Result to MCP response.
   {:ok data} -> (mcp-success data), {:error ...} -> (mcp-error message)."
  [r]
  (if (result/ok? r)
    (mcp-success (:ok r))
    (mcp-error (str "Error: " (or (:message r) (:error r))))))

(defn- handle-elisp
  "Common handler: execute elisp via try-result boundary, return MCP response.
   DRYs the repeated pattern: eval-elisp -> if success -> mcp-success/mcp-error."
  [category elisp]
  (result->mcp (try-result category #(elisp->result elisp))))

;;; =============================================================================
;;; Auto-Connect Fallback Helpers (Result-returning)
;;; =============================================================================

(defn- cider-not-connected-error?
  "Check if error indicates CIDER is not connected."
  [error]
  (and (string? error)
       (str/includes? (str/lower-case error) "cider not connected")))

(defn- list-sessions*
  "List CIDER sessions. Returns Result with session vector."
  []
  (result/let-ok [raw (elisp->result (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-list-sessions))]
    (result/try-effect* :cider/parse-sessions
      (let [parsed (json/read-str raw :key-fn keyword)]
        (if (vector? parsed) parsed (vec parsed))))))

(defn- find-connected-session
  "Find a session with status 'connected'. Returns session name or nil."
  [sessions]
  (some (fn [s] (when (= "connected" (:status s)) (:name s)))
        sessions))

(defn- spawn-session-internal
  "Internal call to spawn a new CIDER session. Returns true on success.
   Optionally accepts project-dir to ensure session connects to correct nREPL."
  ([session-name] (spawn-session-internal session-name nil))
  ([session-name project-dir]
   (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-spawn-session
                                         session-name project-dir nil)
         {:keys [success]} (ec/eval-elisp elisp)]
     success)))

(defn- wait-for-session-ready
  "Wait briefly for a session to become ready. Returns true if ready, false on timeout."
  [_session-name max-attempts]
  (loop [attempt 0]
    (if (>= attempt max-attempts)
      false
      (let [r (list-sessions*)]
        (if (and (result/ok? r) (find-connected-session (:ok r)))
          true
          (do (Thread/sleep 500)
              (recur (inc attempt))))))))

(defn- spawn-and-wait*
  "Spawn a session and wait for readiness. Returns Result with session name."
  [session-name project-dir]
  (if (spawn-session-internal session-name project-dir)
    (if (wait-for-session-ready session-name 5)
      (result/ok session-name)
      (result/err :cider/session-timeout
                  {:message (str "Spawned session '" session-name "' but it didn't become ready in time")}))
    (result/err :cider/spawn-failed
                {:message (str "Failed to spawn session '" session-name "'")})))

(defn- ensure-connected*
  "Ensure CIDER is connected, auto-spawning a session if needed.
   Returns Result with session name.

   When spawning a new session, uses the current project root to ensure
   CIDER connects to the correct project's nREPL."
  []
  (log/debug "ensure-cider-connected: checking sessions")
  (result/let-ok [sessions (list-sessions*)]
    (if-let [session (find-connected-session sessions)]
      (do (log/info "ensure-cider-connected: using existing session" session)
          (result/ok session))
      (let [project-dir (try (ec/project-root) (catch Exception _ nil))]
        (log/info "ensure-cider-connected: spawning 'auto'" {:project-dir project-dir})
        (spawn-and-wait* "auto" project-dir)))))

(defn- with-auto-connect*
  "Execute eval-thunk with auto-connect retry on 'not connected' errors.
   eval-thunk should be a zero-arg fn that returns a Result.
   Returns the Result from eval-thunk or retry."
  [eval-thunk]
  (let [r (eval-thunk)]
    (if (result/ok? r)
      r
      (if (cider-not-connected-error? (:message r))
        (result/let-ok [session (ensure-connected*)]
          (log/info "with-auto-connect: reconnected via session" session)
          (eval-thunk))
        r))))

;;; =============================================================================
;;; Eval Handlers (with validation + telemetry + auto-connect)
;;; =============================================================================

(defn- handle-cider-eval-common
  "Common eval handler with validation, telemetry, and auto-connect retry.
   DRYs handle-cider-eval-silent and handle-cider-eval-explicit.
   elisp-fn: (fn [code] elisp-expression-string)"
  [params telemetry-key elisp-fn]
  (try
    (v/validate-cider-eval-request params)
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry telemetry-key code nil
        (result->mcp
          (try-result :cider/eval-failed
            #(with-auto-connect*
               (fn [] (elisp->result (elisp-fn code))))))))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

(defn handle-cider-eval-silent
  "Evaluate Clojure code via CIDER silently with telemetry.
   Auto-connects to CIDER if not connected (spawns 'auto' session if needed)."
  [params]
  (handle-cider-eval-common params :cider-silent
    (fn [code] (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-silent code))))

(defn handle-cider-eval-explicit
  "Evaluate Clojure code via CIDER interactively (shows in REPL) with telemetry.
   Auto-connects to CIDER if not connected (spawns 'auto' session if needed)."
  [params]
  (handle-cider-eval-common params :cider-explicit
    (fn [code] (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-explicit code))))

;;; =============================================================================
;;; Simple Handlers (thin wrappers over handle-elisp)
;;; =============================================================================

(defn handle-cider-status
  "Get CIDER connection status."
  [_]
  (log/info "cider-status")
  (handle-elisp :cider/status-failed
    (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-status)))

(defn handle-cider-doc
  "Get documentation for a Clojure symbol via CIDER."
  [{:keys [symbol]}]
  (log/info "cider-doc" {:symbol symbol})
  (handle-elisp :cider/doc-failed
    (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-doc symbol)))

(defn handle-cider-apropos
  "Search for symbols matching a pattern via CIDER."
  [{:keys [pattern search_docs]}]
  (log/info "cider-apropos" {:pattern pattern :search_docs search_docs})
  (handle-elisp :cider/apropos-failed
    (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-apropos
                              pattern (boolean search_docs))))

(defn handle-cider-info
  "Get full semantic info for a symbol via CIDER."
  [{:keys [symbol]}]
  (log/info "cider-info" {:symbol symbol})
  (handle-elisp :cider/info-failed
    (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-info symbol)))

(defn handle-cider-complete
  "Get completions for a prefix via CIDER."
  [{:keys [prefix]}]
  (log/info "cider-complete" {:prefix prefix})
  (handle-elisp :cider/complete-failed
    (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-complete prefix)))

;;; =============================================================================
;;; Multi-Session Handlers
;;; =============================================================================

(defn handle-cider-spawn-session
  "Spawn a new named CIDER session with its own nREPL server.
   Useful for parallel agent work where each agent needs isolated REPL."
  [{:keys [name project_dir agent_id]}]
  (log/info "cider-spawn-session" {:name name :agent_id agent_id})
  (handle-elisp :cider/spawn-failed
    (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-spawn-session
                              name project_dir agent_id)))

(defn handle-cider-list-sessions
  "List all active CIDER sessions with their status and ports."
  [_]
  (log/info "cider-list-sessions")
  (handle-elisp :cider/list-sessions-failed
    (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-list-sessions)))

(defn handle-cider-eval-session
  "Evaluate Clojure code in a specific named CIDER session."
  [{:keys [session_name code]}]
  (log/info "cider-eval-session" {:session session_name :code-length (count code)})
  (handle-elisp :cider/eval-session-failed
    (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-in-session
                              session_name code)))

(defn- kill-session*
  "Kill a named session. Returns Result with confirmation message."
  [{:keys [session_name]}]
  (result/let-ok [_ (elisp->result (el/require-and-call 'hive-mcp-cider 'hive-mcp-cider-kill-session session_name))]
    (result/ok (format "Session '%s' killed" session_name))))

(defn handle-cider-kill-session
  "Kill a specific named CIDER session."
  [{:keys [session_name] :as params}]
  (log/info "cider-kill-session" {:session session_name})
  (result->mcp (try-result :cider/kill-session-failed #(kill-session* params))))

(defn- kill-all-sessions*
  "Kill all sessions. Returns Result with confirmation message."
  [_]
  (result/let-ok [_ (elisp->result (el/require-and-call 'hive-mcp-cider 'hive-mcp-cider-kill-all-sessions))]
    (result/ok "All CIDER sessions killed")))

(defn handle-cider-kill-all-sessions
  "Kill all CIDER sessions."
  [params]
  (log/info "cider-kill-all-sessions")
  (result->mcp (try-result :cider/kill-all-failed #(kill-all-sessions* params))))

;;; =============================================================================
;;; Unified Eval Router
;;; =============================================================================

(defn handle-cider-eval
  "Unified eval handler. Routes based on mode and session_name params.
   - session_name provided → eval in named session
   - mode=\"explicit\" → explicit eval (shows in REPL buffer)
   - default (mode=\"silent\") → silent eval
   Backward compatible: eval-explicit and eval-session commands still work as aliases."
  [params]
  (let [{:keys [session_name mode] :or {mode "silent"}} params]
    (cond
      ;; Session eval takes priority when session_name provided
      session_name
      (handle-cider-eval-session params)

      ;; Explicit mode: show in REPL buffer
      (= mode "explicit")
      (handle-cider-eval-explicit params)

      ;; Default: silent eval
      :else
      (handle-cider-eval-silent params))))

;;; =============================================================================
;;; Tool Definitions
;;; =============================================================================

(def tools
  [{:name "cider_status"
    :description "Get CIDER connection status including connected state, REPL buffer name, current namespace, and REPL type."
    :inputSchema {:type "object" :properties {}}
    :handler handle-cider-status}

   {:name "cider_eval_silent"
    :description "Evaluate Clojure code via CIDER silently. Fast evaluation without REPL buffer output. Use for routine/automated evals."
    :inputSchema {:type "object"
                  :properties {"code" {:type "string"
                                       :description "Clojure code to evaluate"}}
                  :required ["code"]}
    :handler handle-cider-eval-silent}

   {:name "cider_eval_explicit"
    :description "Evaluate Clojure code via CIDER interactively. Shows output in REPL buffer for collaborative debugging. Use when stuck or want user to see output."
    :inputSchema {:type "object"
                  :properties {"code" {:type "string"
                                       :description "Clojure code to evaluate"}}
                  :required ["code"]}
    :handler handle-cider-eval-explicit}

   {:name "cider_spawn_session"
    :description "Spawn a new named CIDER session with its own nREPL server. Useful for parallel agent work where each agent needs an isolated REPL. Sessions auto-connect when nREPL starts."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Session identifier (e.g., 'agent-1', 'task-render')"}
                               "project_dir" {:type "string"
                                              :description "Directory to start nREPL in (optional, defaults to current project)"}
                               "agent_id" {:type "string"
                                           :description "Optional swarm agent ID to link this session to"}}
                  :required ["name"]}
    :handler handle-cider-spawn-session}

   {:name "cider_list_sessions"
    :description "List all active CIDER sessions with their status, ports, and linked agents."
    :inputSchema {:type "object" :properties {}}
    :handler handle-cider-list-sessions}

   {:name "cider_eval_session"
    :description "Evaluate Clojure code in a specific named CIDER session. Use for isolated evaluation in multi-agent scenarios."
    :inputSchema {:type "object"
                  :properties {"session_name" {:type "string"
                                               :description "Name of the session to evaluate in"}
                               "code" {:type "string"
                                       :description "Clojure code to evaluate"}}
                  :required ["session_name" "code"]}
    :handler handle-cider-eval-session}

   {:name "cider_kill_session"
    :description "Kill a specific named CIDER session and its nREPL server."
    :inputSchema {:type "object"
                  :properties {"session_name" {:type "string"
                                               :description "Name of the session to kill"}}
                  :required ["session_name"]}
    :handler handle-cider-kill-session}

   {:name "cider_kill_all_sessions"
    :description "Kill all CIDER sessions. Useful for cleanup after parallel agent work."
    :inputSchema {:type "object" :properties {}}
    :handler handle-cider-kill-all-sessions}

   ;; Documentation Tools
   {:name "cider_doc"
    :description "Get documentation for a Clojure symbol. Returns docstring, arglists, namespace, source location. Use for looking up function/var documentation."
    :inputSchema {:type "object"
                  :properties {"symbol" {:type "string"
                                         :description "Fully qualified or unqualified symbol name (e.g., 'map', 'clojure.string/join')"}}
                  :required ["symbol"]}
    :handler handle-cider-doc}

   {:name "cider_apropos"
    :description "Search for Clojure symbols matching a pattern. Finds functions, vars, macros by name. Optionally searches docstrings too."
    :inputSchema {:type "object"
                  :properties {"pattern" {:type "string"
                                          :description "Regex pattern to match symbol names (e.g., 'map', 'str.*join')"}
                               "search_docs" {:type "boolean"
                                              :description "Also search in docstrings (default: false)"}}
                  :required ["pattern"]}
    :handler handle-cider-apropos}

   {:name "cider_info"
    :description "Get full semantic info for a Clojure symbol via CIDER. Returns comprehensive metadata: namespace, arglists, docstring, source file/line, specs, deprecation info, etc."
    :inputSchema {:type "object"
                  :properties {"symbol" {:type "string"
                                         :description "Fully qualified or unqualified symbol name"}}
                  :required ["symbol"]}
    :handler handle-cider-info}

   {:name "cider_complete"
    :description "Get code completions for a prefix. Returns matching symbols with their types and namespaces. Useful for discovering available functions."
    :inputSchema {:type "object"
                  :properties {"prefix" {:type "string"
                                         :description "Prefix to complete (e.g., 'clojure.string/jo', 'map')"}}
                  :required ["prefix"]}
    :handler handle-cider-complete}])
