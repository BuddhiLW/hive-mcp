(ns emacs-mcp.tools.cider
  "CIDER integration handlers for MCP.

   Provides Clojure REPL operations via CIDER:
   - Status checking and connection info
   - Silent and explicit code evaluation
   - Multi-session support for parallel agent work"
  (:require [emacs-mcp.tools.core :refer [mcp-success mcp-error]]
            [emacs-mcp.emacsclient :as ec]
            [emacs-mcp.elisp :as el]
            [emacs-mcp.telemetry :as telemetry]
            [emacs-mcp.validation :as v]
            [taoensso.timbre :as log]))

;; ============================================================
;; CIDER Integration Tools (requires emacs-mcp-cider addon)
;; ============================================================

(defn handle-cider-status
  "Get CIDER connection status."
  [_]
  (log/info "cider-status")
  (let [elisp (el/require-and-call-json 'emacs-mcp-cider 'emacs-mcp-cider-status)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (str "Error: " error)))))

(defn handle-cider-eval-silent
  "Evaluate Clojure code via CIDER silently with telemetry."
  [params]
  (try
    (v/validate-cider-eval-request params)
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry :cider-silent code nil
        (let [elisp (el/require-and-call-text 'emacs-mcp-cider 'emacs-mcp-cider-eval-silent code)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            (mcp-success result)
            (mcp-error (str "Error: " error))))))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

(defn handle-cider-eval-explicit
  "Evaluate Clojure code via CIDER interactively (shows in REPL) with telemetry."
  [params]
  (try
    (v/validate-cider-eval-request params)
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry :cider-explicit code nil
        (let [elisp (el/require-and-call-text 'emacs-mcp-cider 'emacs-mcp-cider-eval-explicit code)
              {:keys [success result error]} (ec/eval-elisp elisp)]
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
  (let [elisp (el/require-and-call-json 'emacs-mcp-cider 'emacs-mcp-cider-spawn-session
                                        name project_dir agent_id)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-list-sessions
  "List all active CIDER sessions with their status and ports."
  [_]
  (log/info "cider-list-sessions")
  (let [elisp (el/require-and-call-json 'emacs-mcp-cider 'emacs-mcp-cider-list-sessions)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-eval-session
  "Evaluate Clojure code in a specific named CIDER session."
  [{:keys [session_name code]}]
  (log/info "cider-eval-session" {:session session_name :code-length (count code)})
  (let [elisp (el/require-and-call-text 'emacs-mcp-cider 'emacs-mcp-cider-eval-in-session
                                        session_name code)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-kill-session
  "Kill a specific named CIDER session."
  [{:keys [session_name]}]
  (log/info "cider-kill-session" {:session session_name})
  (let [elisp (el/require-and-call 'emacs-mcp-cider 'emacs-mcp-cider-kill-session session_name)
        {:keys [success error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success (format "Session '%s' killed" session_name))
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-kill-all-sessions
  "Kill all CIDER sessions."
  [_]
  (log/info "cider-kill-all-sessions")
  (let [elisp (el/require-and-call 'emacs-mcp-cider 'emacs-mcp-cider-kill-all-sessions)
        {:keys [success error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success "All CIDER sessions killed")
      (mcp-error (format "Error: %s" error)))))

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
    :handler handle-cider-kill-all-sessions}])
