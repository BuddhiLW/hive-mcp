(ns hive-mcp.tools.cider
  "CIDER integration handlers for MCP.

   Provides Clojure REPL operations via CIDER:
   - Status checking and connection info
   - Silent and explicit code evaluation
   - Multi-session support for parallel agent work
   - Auto-connect fallback when CIDER is not connected"
  (:require [hive-mcp.tools.core :refer [mcp-success mcp-error]]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.elisp :as el]
            [hive-mcp.telemetry :as telemetry]
            [hive-mcp.validation :as v]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; CIDER Integration Tools (requires hive-mcp-cider addon)
;; ============================================================

;; -----------------------------------------------------------------------------
;; Auto-Connect Fallback Helpers (CLARITY-Y: Yield safe failure)
;; -----------------------------------------------------------------------------

(defn- cider-not-connected-error?
  "Check if error indicates CIDER is not connected."
  [error]
  (and (string? error)
       (str/includes? (str/lower-case error) "cider not connected")))

(defn- list-sessions-internal
  "Internal call to list CIDER sessions. Returns vector of sessions or nil on error."
  []
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-list-sessions)
        {:keys [success result]} (ec/eval-elisp elisp)]
    (when success
      (try
        (let [parsed (json/read-str result :key-fn keyword)]
          (if (vector? parsed) parsed (vec parsed)))
        (catch Exception _ nil)))))

(defn- spawn-session-internal
  "Internal call to spawn a new CIDER session. Returns true on success.
   Optionally accepts project-dir to ensure session connects to correct nREPL."
  ([session-name]
   (spawn-session-internal session-name nil))
  ([session-name project-dir]
   (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-spawn-session
                                         session-name project-dir nil)
         {:keys [success]} (ec/eval-elisp elisp)]
     success)))

(defn- find-connected-session
  "Find a session with status 'connected'. Returns session name or nil."
  [sessions]
  (some (fn [s]
          (when (= "connected" (:status s))
            (:name s)))
        sessions))

(defn- _eval-in-session-internal
  "Internal call to evaluate code in a specific session. Returns {:success :result/:error}."
  [session-name code]
  (let [elisp (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-in-session
                                        session-name code)]
    (ec/eval-elisp elisp)))

(defn- wait-for-session-ready
  "Wait briefly for a session to become ready. Returns true if ready, false on timeout."
  [_session-name max-attempts]
  (loop [attempt 0]
    (if (>= attempt max-attempts)
      false
      (let [sessions (list-sessions-internal)
            connected (find-connected-session sessions)]
        (if connected
          true
          (do
            (Thread/sleep 500)
            (recur (inc attempt))))))))

(defn- ensure-cider-connected
  "Ensure CIDER is connected, auto-spawning a session if needed.
   Returns {:connected true :session name} on success, {:connected false :error msg} on failure.

   When spawning a new session, uses the current project root to ensure
   CIDER connects to the correct project's nREPL (not a random one from
   another project)."
  []
  (log/debug "ensure-cider-connected: checking available sessions")
  (let [sessions (list-sessions-internal)]
    (if-let [connected-session (find-connected-session sessions)]
      ;; Use existing connected session
      (do
        (log/info "ensure-cider-connected: using existing session" connected-session)
        {:connected true :session connected-session})
      ;; No connected sessions - spawn a new one with project context
      (let [project-dir (try (ec/project-root) (catch Exception _ nil))]
        (log/info "ensure-cider-connected: no connected sessions, spawning 'auto'"
                  {:project-dir project-dir})
        (if (spawn-session-internal "auto" project-dir)
          ;; Wait for session to become ready (up to 5 attempts = 2.5 seconds)
          (if (wait-for-session-ready "auto" 5)
            {:connected true :session "auto"}
            {:connected false :error "Spawned session 'auto' but it didn't become ready in time"})
          {:connected false :error "Failed to spawn auto session"})))))

(defn- with-auto-connect-retry
  "Execute eval-fn, and if it fails with 'not connected', attempt auto-connect and retry.
   eval-fn should be a zero-arg function that performs the evaluation.
   Returns the result of eval-fn (success or error)."
  [eval-fn]
  (let [{:keys [success error] :as first-result} (eval-fn)]
    (if success
      first-result
      (if (cider-not-connected-error? error)
        ;; Attempt auto-connect
        (let [{:keys [connected session error]} (ensure-cider-connected)]
          (if connected
            (do
              (log/info "with-auto-connect-retry: auto-connected to session" session)
              ;; Retry the eval
              (eval-fn))
            (do
              (log/warn "with-auto-connect-retry: auto-connect failed:" error)
              ;; Return original error enhanced with auto-connect failure info
              {:success false
               :error (str "CIDER not connected and auto-connect failed: " error)})))
        ;; Non-connection error - return as-is
        first-result))))

;; -----------------------------------------------------------------------------
;; Status Handler
;; -----------------------------------------------------------------------------

(defn handle-cider-status
  "Get CIDER connection status."
  [_]
  (log/info "cider-status")
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-status)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-cider-eval-silent
  "Evaluate Clojure code via CIDER silently with telemetry.
   Auto-connects to CIDER if not connected (spawns 'auto' session if needed)."
  [params]
  (try
    (v/validate-cider-eval-request params)
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry :cider-silent code nil
        (let [eval-fn (fn []
                        (let [elisp (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-silent code)]
                          (ec/eval-elisp elisp)))
              {:keys [success result error]} (with-auto-connect-retry eval-fn)]
          (if success
            (mcp-success result)
            (mcp-error (str "Error: " error))))))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

(defn handle-cider-eval-explicit
  "Evaluate Clojure code via CIDER interactively (shows in REPL) with telemetry.
   Auto-connects to CIDER if not connected (spawns 'auto' session if needed)."
  [params]
  (try
    (v/validate-cider-eval-request params)
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry :cider-explicit code nil
        (let [eval-fn (fn []
                        (let [elisp (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-explicit code)]
                          (ec/eval-elisp elisp)))
              {:keys [success result error]} (with-auto-connect-retry eval-fn)]
          (if success
            (mcp-success result)
            (mcp-error (str "Error: " error))))))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

;; =============================================================================
;; Multi-Session CIDER Tools
;; =============================================================================

(defn handle-cider-spawn-session
  "Spawn a new named CIDER session with its own nREPL server.
   Useful for parallel agent work where each agent needs isolated REPL."
  [{:keys [name project_dir agent_id]}]
  (log/info "cider-spawn-session" {:name name :agent_id agent_id})
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-spawn-session
                                        name project_dir agent_id)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-list-sessions
  "List all active CIDER sessions with their status and ports."
  [_]
  (log/info "cider-list-sessions")
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-list-sessions)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-eval-session
  "Evaluate Clojure code in a specific named CIDER session."
  [{:keys [session_name code]}]
  (log/info "cider-eval-session" {:session session_name :code-length (count code)})
  (let [elisp (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-in-session
                                        session_name code)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-kill-session
  "Kill a specific named CIDER session."
  [{:keys [session_name]}]
  (log/info "cider-kill-session" {:session session_name})
  (let [elisp (el/require-and-call 'hive-mcp-cider 'hive-mcp-cider-kill-session session_name)
        {:keys [success error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success (format "Session '%s' killed" session_name))
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-kill-all-sessions
  "Kill all CIDER sessions."
  [_]
  (log/info "cider-kill-all-sessions")
  (let [elisp (el/require-and-call 'hive-mcp-cider 'hive-mcp-cider-kill-all-sessions)
        {:keys [success error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success "All CIDER sessions killed")
      (mcp-error (format "Error: %s" error)))))

;; =============================================================================
;; CIDER Documentation Tools
;; =============================================================================

(defn handle-cider-doc
  "Get documentation for a Clojure symbol via CIDER."
  [{:keys [symbol]}]
  (log/info "cider-doc" {:symbol symbol})
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-doc symbol)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-cider-apropos
  "Search for symbols matching a pattern via CIDER."
  [{:keys [pattern search_docs]}]
  (log/info "cider-apropos" {:pattern pattern :search_docs search_docs})
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-apropos
                                        pattern (boolean search_docs))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-cider-info
  "Get full semantic info for a symbol via CIDER."
  [{:keys [symbol]}]
  (log/info "cider-info" {:symbol symbol})
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-info symbol)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-cider-complete
  "Get completions for a prefix via CIDER."
  [{:keys [prefix]}]
  (log/info "cider-complete" {:prefix prefix})
  (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-complete prefix)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

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

