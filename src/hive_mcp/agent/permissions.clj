(ns hive-mcp.agent.permissions
  "IAgentPermissions implementation + hooks bridge.

   Bridges Claude Code's permission/hooks system with the hive agent framework.
   Provides configurable permission policies for autonomous agents:

   Permission Levels:
   - :allow-all    — Bypass all checks (dangerous, dev/test only)
   - :allow-safe   — Allow read + safe writes, prompt for destructive ops
   - :prompt-user  — Prompt for every mutation (interactive mode)
   - :deny-all     — Deny all tool calls (lockdown)

   Tool Categories:
   - :read-only    — File reads, search, inspection
   - :safe-write   — File edits, propose_diff
   - :destructive  — Shell commands, git push, kill
   - :coordination — Hivemind shouts, memory ops
   - :eval         — REPL evaluation (cider, clojure_eval)

   Integration:
   - Bridges tool_allowlist.clj for drone tool filtering
   - Supports hooks-style callbacks for permission prompts
   - Default policy: safe-autonomy (read + safe-write allowed)

   SOLID-I: Permission logic separate from session/backend.
   SOLID-O: Open for new permission levels via policy maps.
   CLARITY-I: Inputs guarded — every tool call checked.
   CLARITY-Y: Yield safe failure — deny returns error map, never throws."
  (:require [hive-mcp.agent.drone.tool-allowlist :as allowlist]
            [hive-mcp.protocols.agent-bridge :as bridge]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; Tool Categories
;;; ============================================================================

(def tool-categories
  "Map of tool names to their permission category.
   Categories determine what permission level is required.

   :read-only    — Always safe, no side effects
   :safe-write   — File modifications within sandbox
   :destructive  — Shell exec, git push, agent kill
   :coordination — Hivemind, memory, kanban (agent infra)
   :eval         — REPL evaluation (side effects possible)"
  {;; Read-only tools
   "read_file"      :read-only
   "grep"           :read-only
   "glob_files"     :read-only
   "cider_doc"      :read-only
   "cider_info"     :read-only
   "cider_complete" :read-only
   "magit_status"   :read-only
   "magit_diff"     :read-only
   "magit_log"      :read-only
   "magit_branches" :read-only
   "kondo_lint"     :read-only
   "kondo_analyze"  :read-only
   "scc"            :read-only

   ;; Safe write tools
   "file_write"     :safe-write
   "propose_diff"   :safe-write
   "magit_stage"    :safe-write
   "magit_commit"   :safe-write

   ;; Destructive tools
   "bash"           :destructive
   "magit_push"     :destructive
   "magit_pull"     :destructive
   "magit_fetch"    :destructive

   ;; Coordination tools
   "hivemind_shout" :coordination
   "memory"         :coordination
   "kanban"         :coordination
   "kg"             :coordination
   "session"        :coordination

   ;; Eval tools
   "clojure_eval"      :eval
   "cider_eval_silent" :eval
   "cider_eval"        :eval})

(defn categorize-tool
  "Return the permission category for a tool name.

   Arguments:
     tool-name - String name of the tool

   Returns:
     Keyword category (:read-only, :safe-write, :destructive,
     :coordination, :eval) or :unknown if not categorized."
  [tool-name]
  (get tool-categories tool-name :unknown))

;;; ============================================================================
;;; Permission Levels (Policy Maps)
;;; ============================================================================

(def permission-levels
  "Permission level definitions. Each level maps tool categories to actions.

   Actions:
   - :allow  — Tool call proceeds
   - :deny   — Tool call rejected with reason
   - :prompt — Defer to handler-fn (hooks callback)"
  {:allow-all
   {:read-only    :allow
    :safe-write   :allow
    :destructive  :allow
    :coordination :allow
    :eval         :allow
    :unknown      :allow}

   :allow-safe
   {:read-only    :allow
    :safe-write   :allow
    :destructive  :prompt
    :coordination :allow
    :eval         :allow
    :unknown      :prompt}

   :prompt-user
   {:read-only    :allow
    :safe-write   :prompt
    :destructive  :prompt
    :coordination :allow
    :eval         :prompt
    :unknown      :prompt}

   :deny-all
   {:read-only    :deny
    :safe-write   :deny
    :destructive  :deny
    :coordination :deny
    :eval         :deny
    :unknown      :deny}})

(def ^:private valid-levels (set (keys permission-levels)))

(defn valid-permission-level?
  "Check if a keyword is a recognized permission level."
  [level]
  (contains? valid-levels level))

;;; ============================================================================
;;; Permission Decision Engine
;;; ============================================================================

(defn- category-action
  "Resolve the action for a tool category under a permission level.

   Arguments:
     level    - Permission level keyword
     category - Tool category keyword

   Returns:
     :allow, :deny, or :prompt"
  [level category]
  (get-in permission-levels [level category] :deny))

(defn check-permission
  "Check whether a tool call is permitted under the given policy.

   Arguments:
     policy    - Map with :level and optional :handler-fn
     tool-name - String name of the tool being called
     input     - Tool input data (passed to handler if :prompt)
     context   - Additional context map (agent-id, session-id, etc.)

   Returns:
     Permission result map:
     {:action :allow}
     {:action :allow :updated-input <map>}
     {:action :deny :message <string>}
     {:action :deny :message <string> :interrupt? true}"
  [{:keys [level handler-fn]} tool-name input context]
  (let [category (categorize-tool tool-name)
        action   (category-action level category)]
    (case action
      :allow
      (do (log/trace "Permission granted" {:tool tool-name :category category :level level})
          {:action :allow})

      :deny
      (let [msg (str "Permission denied: tool '" tool-name "' (category: " (name category)
                     ") not allowed under policy '" (name level) "'")]
        (log/info "Permission denied" {:tool tool-name :category category :level level})
        {:action :deny :message msg})

      :prompt
      (if handler-fn
        (try
          (let [result (handler-fn tool-name input context)]
            (log/debug "Permission handler result" {:tool tool-name :result result})
            result)
          (catch Exception e
            (log/warn "Permission handler threw exception, denying" {:tool tool-name :error (.getMessage e)})
            {:action :deny
             :message (str "Permission handler error: " (.getMessage e))}))
        ;; No handler → deny (safe default)
        (do
          (log/info "Permission prompt with no handler, denying" {:tool tool-name :category category})
          {:action :deny
           :message (str "Tool '" tool-name "' requires approval but no permission handler is configured")})))))

;;; ============================================================================
;;; Permission Policy Construction
;;; ============================================================================

(defn ->policy
  "Create a permission policy map.

   Arguments:
     level      - Permission level keyword (:allow-all, :allow-safe, :prompt-user, :deny-all)
     handler-fn - Optional (fn [tool-name input context] -> permission-result)
                  Called when action is :prompt

   Returns:
     Policy map {:level <kw> :handler-fn <fn-or-nil>}"
  ([level]
   (->policy level nil))
  ([level handler-fn]
   {:pre [(valid-permission-level? level)]}
   {:level      level
    :handler-fn handler-fn}))

(def default-policy
  "Default permission policy: allow-safe.
   Read and safe writes permitted, destructive ops denied unless handler set.
   This is the safe default for autonomous agents (lings/drones)."
  (->policy :allow-safe))

;;; ============================================================================
;;; Hooks-Style Handler Factories
;;; ============================================================================

(defn logging-handler
  "Create a permission handler that logs and allows all prompted tools.
   Useful for development/debugging — see what would be prompted.

   Returns:
     (fn [tool-name input context] -> {:action :allow})"
  []
  (fn [tool-name _input context]
    (log/info "Permission prompt (auto-allowed)"
              {:tool tool-name
               :agent-id (:agent-id context)})
    {:action :allow}))

(defn allowlist-handler
  "Create a permission handler that checks against an allowlist.
   Bridges tool_allowlist.clj for drone-style filtering on :prompt actions.

   Arguments:
     allowlist-opts - Options for allowlist/resolve-allowlist
                      {:tool-allowlist #{...}} or {:task-type :testing}

   Returns:
     (fn [tool-name input context] -> permission-result)"
  [allowlist-opts]
  (let [al (allowlist/resolve-allowlist allowlist-opts)]
    (fn [tool-name _input _context]
      (if (allowlist/tool-allowed? tool-name al)
        {:action :allow}
        {:action :deny
         :message (str "Tool '" tool-name "' not on allowlist. "
                       "Allowed: " (pr-str (sort al)))}))))

(defn callback-handler
  "Create a permission handler that invokes a callback function.
   The callback receives full context and returns a boolean or result map.

   Arguments:
     callback-fn - (fn [tool-name input context] -> boolean | permission-result)
                   If boolean: true → allow, false → deny
                   If map: passed through as-is

   Returns:
     Permission handler fn"
  [callback-fn]
  (fn [tool-name input context]
    (let [result (callback-fn tool-name input context)]
      (cond
        (true? result)  {:action :allow}
        (false? result) {:action :deny :message (str "Callback denied tool: " tool-name)}
        (map? result)   result
        :else           {:action :deny :message (str "Unexpected callback result for: " tool-name)}))))

(defn composite-handler
  "Create a handler that chains multiple handlers. First non-allow result wins.
   If all handlers allow, the final result is :allow.

   Arguments:
     handlers - Sequence of permission handler fns

   Returns:
     Composite permission handler fn"
  [handlers]
  (fn [tool-name input context]
    (reduce
     (fn [_acc handler]
       (let [result (handler tool-name input context)]
         (if (= :allow (:action result))
           result
           (reduced result))))
     {:action :allow}
     handlers)))

;;; ============================================================================
;;; Protocol Bridge: mode keyword → policy mapping
;;; ============================================================================

(defn mode->policy
  "Convert IAgentPermissions mode keywords to internal policies.
   Maps the protocol's 3 modes to our 4-level system.

   Arguments:
     mode - Protocol mode keyword (:default, :accept-edits, :bypass)

   Returns:
     Policy map"
  [mode]
  (case mode
    :default      (->policy :prompt-user)
    :accept-edits (->policy :allow-safe)
    :bypass       (->policy :allow-all)
    ;; Fallback for unknown modes
    (do (log/warn "Unknown permission mode, using default" {:mode mode})
        (->policy :prompt-user))))

(defn mode->sdk-string
  "Convert internal permission mode keywords to Claude SDK string values.
   Used when delegating to headless_sdk.clj.

   Arguments:
     mode - Keyword (:default, :accept-edits, :bypass)

   Returns:
     String for SDK API (\"default\", \"acceptEdits\", \"bypassPermissions\")"
  [mode]
  (case mode
    :default      "default"
    :accept-edits "acceptEdits"
    :bypass       "bypassPermissions"
    ;; Fallback
    (do (log/warn "Unknown mode for SDK string, using default" {:mode mode})
        "default")))

;;; ============================================================================
;;; Agent-Specific Policy Presets
;;; ============================================================================

(defn ling-policy
  "Permission policy for ling agents.
   Lings are coordinators: they read, search, and delegate.
   They should NOT do destructive ops directly.

   Returns:
     Policy with :allow-safe level"
  []
  (->policy :allow-safe))

(defn drone-policy
  "Permission policy for drone agents.
   Drones execute atomic tasks within a bounded allowlist.
   Uses allowlist-handler to enforce tool_allowlist.clj.

   Arguments:
     opts - Drone options map with optional:
            :tool-allowlist - Explicit allowlist set
            :task-type      - Task type for profile-based allowlist

   Returns:
     Policy with :allow-safe level + allowlist handler for :prompt actions"
  [opts]
  (->policy :allow-safe (allowlist-handler opts)))

(defn coordinator-policy
  "Permission policy for the coordinator (hivemind).
   Full access — the coordinator is human-supervised.

   Returns:
     Policy with :allow-all level"
  []
  (->policy :allow-all))

;;; ============================================================================
;;; Stateful Permission Manager (per-session)
;;; ============================================================================

(defrecord PermissionManager [policy-atom]
  bridge/IAgentPermissions

  (set-permission-mode! [_ _session mode]
    (let [new-policy (mode->policy mode)]
      (reset! policy-atom new-policy)
      (log/info "Permission mode set" {:mode mode :level (:level new-policy)})
      {:success? true}))

  (set-permission-handler! [_ _session handler-fn]
    (swap! policy-atom assoc :handler-fn handler-fn)
    (log/info "Permission handler set" {:has-handler? true})
    {:success? true}))

(defn ->permission-manager
  "Create a new PermissionManager with the given initial policy.

   Arguments:
     policy - Initial policy map (default: default-policy)

   Returns:
     PermissionManager record implementing IAgentPermissions"
  ([] (->permission-manager default-policy))
  ([policy]
   (->PermissionManager (atom policy))))

(defn check-tool-permission
  "Convenience: check permission on a PermissionManager instance.

   Arguments:
     manager   - PermissionManager
     tool-name - String tool name
     input     - Tool input data
     context   - Context map {:agent-id ... :session-id ...}

   Returns:
     Permission result map"
  [^PermissionManager manager tool-name input context]
  (check-permission @(.policy-atom manager) tool-name input context))

;;; ============================================================================
;;; Enforcement Middleware
;;; ============================================================================

(defn wrap-permission-check
  "Middleware that wraps a tool handler with permission enforcement.
   Returns a new handler that checks permissions before executing.

   Arguments:
     handler  - Original tool handler (fn [tool-name input] -> result)
     manager  - PermissionManager instance
     context  - Base context map (agent-id, session-id)

   Returns:
     Wrapped handler fn that checks permissions first"
  [handler manager context]
  (fn [tool-name input]
    (let [result (check-tool-permission manager tool-name input context)]
      (case (:action result)
        :allow
        (handler tool-name (or (:updated-input result) input))

        :deny
        {:error true
         :message (:message result)
         :tool tool-name}

        ;; Shouldn't reach here, but safe fallback
        {:error true
         :message (str "Unexpected permission result for tool: " tool-name)
         :tool tool-name}))))
