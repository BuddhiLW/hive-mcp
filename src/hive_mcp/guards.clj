(ns hive-mcp.guards
  "Enforcement guards for swarm delegation patterns.

   Provides runtime interception of ling file mutations:
   - Warn mode: Log violation and allow (default)
   - Block mode: Reject with guidance message

   Purpose: Enforce ADR-004 token efficiency pattern where lings
   NEVER implement directly - all file mutations via drones.

   SOLID: SRP - Enforcement logic only
   CLARITY: I - Inputs are guarded at tool boundaries"
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Configuration
;;; =============================================================================

(def ^:dynamic *enforcement-mode*
  "Enforcement mode for ling file mutations.

   Values:
     :warn  - Log warning but allow (default)
     :block - Reject with guidance message"
  :warn)

(def ^:dynamic *guard-enabled?*
  "Whether guard checks are enabled at all."
  true)

;; Atom tracking whether coordinator is running.
;; Set via mark-coordinator-running!, checked by when-not-coordinator.
(defonce ^:private coordinator-running-atom (atom false))

;;; =============================================================================
;;; Mutation Tools Registry
;;; =============================================================================

(def mutation-tool-names
  "Tool names that perform file mutations.
   Lings should NEVER call these directly."
  #{"file_write"
    "file_edit"
    "clojure_edit"
    "mcp__emacs__file_write"
    "mcp__emacs__insert_text"
    "propose_diff"})  ;; propose_diff is for drones only

(def delegation-guidance
  "Guidance message for lings attempting direct mutation."
  (str "ENFORCEMENT VIOLATION: Lings must delegate file mutations to drones.\n\n"
       "Instead of direct file writes, use:\n"
       "  dispatch_validated_wave(tasks: [{file: \"...\", task: \"...\"}])\n"
       "  dispatch_drone_wave(tasks: [{file: \"...\", task: \"...\"}])\n"
       "  delegate_drone(task: \"...\", files: [\"...\"])\n\n"
       "This saves 92% of premium tokens and provides quality gates."))

;;; =============================================================================
;;; Agent Classification
;;; =============================================================================

(defn ling-agent?
  "Check if the current agent is a ling (not a drone).

   Lings are identified by:
   - CLAUDE_SWARM_SLAVE_ID containing 'swarm-' prefix
   - NOT containing 'drone-' in the ID

   Returns true if agent appears to be a ling."
  []
  (let [slave-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")]
    (and slave-id
         (re-find #"^swarm-" slave-id)
         (not (re-find #"drone-" slave-id)))))

(defn drone-agent?
  "Check if the current agent is a drone.

   Drones are identified by 'drone-' in CLAUDE_SWARM_SLAVE_ID."
  []
  (let [slave-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")]
    (and slave-id
         (re-find #"drone-" slave-id))))

;;; =============================================================================
;;; Guard Functions
;;; =============================================================================

(defn- log-violation!
  "Log a delegation violation.

   Arguments:
     tool-name   - Name of the mutation tool attempted
     agent-id    - Agent attempting the violation
     file-path   - Target file path (if available)"
  [tool-name agent-id file-path]
  (let [event-data {:tool tool-name
                    :agent-id agent-id
                    :file-path file-path
                    :timestamp (System/currentTimeMillis)}]
    ;; Log warning (no event dispatch to avoid cyclic dep)
    (log/warn "LING DELEGATION VIOLATION" event-data)))

(defn check-mutation-guard
  "Check if a mutation tool call should be allowed.

   Arguments:
     tool-name - Name of the tool being called
     params    - Tool parameters (may contain file_path, path, etc.)

   Returns:
     {:allowed? true/false :message \"...\" :logged? true/false}

   Behavior depends on *enforcement-mode*:
     :warn  - Returns {:allowed? true} but logs violation
     :block - Returns {:allowed? false :message guidance}"
  [tool-name params]
  (if-not *guard-enabled?*
    {:allowed? true}

    (let [is-mutation? (contains? mutation-tool-names tool-name)
          is-ling? (ling-agent?)
          agent-id (or (System/getenv "CLAUDE_SWARM_SLAVE_ID") "unknown")]

      (cond
        ;; Not a mutation tool - always allow
        (not is-mutation?)
        {:allowed? true}

        ;; Not a ling (drone or coordinator) - allow
        (not is-ling?)
        {:allowed? true}

        ;; Ling attempting mutation - enforce
        :else
        (let [file-path (or (:file_path params)
                            (:path params)
                            (get params "file_path")
                            (get params "path"))]
          ;; Log the violation
          (log-violation! tool-name agent-id file-path)

          (case *enforcement-mode*
            :warn
            {:allowed? true
             :logged? true
             :message "Warning: Ling used mutation tool directly. Consider using dispatch_validated_wave."}

            :block
            {:allowed? false
             :logged? true
             :message delegation-guidance}

            ;; Default to warn
            {:allowed? true
             :logged? true
             :message "Warning: Unknown enforcement mode, defaulting to warn."}))))))

;;; =============================================================================
;;; Middleware
;;; =============================================================================

(defn wrap-mutation-guard
  "Middleware wrapper for tool handlers.

   Checks mutation guard before allowing handler execution.
   If blocked, returns error response instead of calling handler.

   Arguments:
     handler - Original tool handler function

   Returns:
     Wrapped handler with guard check."
  [handler]
  (fn [params]
    (let [tool-name (or (:tool-name (meta handler))
                        "unknown-tool")
          guard-result (check-mutation-guard tool-name params)]
      (if (:allowed? guard-result)
        (handler params)
        {:type "text"
         :text (:message guard-result)
         :isError true}))))

;;; =============================================================================
;;; Configuration Helpers
;;; =============================================================================

(defn set-enforcement-mode!
  "Set the enforcement mode.

   Arguments:
     mode - :warn or :block"
  [mode]
  (when (#{:warn :block} mode)
    (alter-var-root #'*enforcement-mode* (constantly mode))
    (log/info "Guard enforcement mode set to:" mode)))

(defn enable-guards!
  "Enable delegation guards."
  []
  (alter-var-root #'*guard-enabled?* (constantly true))
  (log/info "Delegation guards enabled"))

(defn disable-guards!
  "Disable delegation guards."
  []
  (alter-var-root #'*guard-enabled?* (constantly false))
  (log/info "Delegation guards disabled"))

;;; =============================================================================
;;; Status
;;; =============================================================================

(defn guard-status
  "Get current guard configuration status.

   Returns map with :enabled?, :mode, :mutation-tools."
  []
  {:enabled? *guard-enabled?*
   :mode *enforcement-mode*
   :mutation-tools mutation-tool-names
   :is-ling? (ling-agent?)
   :is-drone? (drone-agent?)
   :agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")
   :coordinator-running? @coordinator-running-atom})

;;; =============================================================================
;;; Coordinator Protection (CLARITY-Y)
;;; =============================================================================

(defn coordinator-running?
  "Check if coordinator is currently running.

   Used to protect production state from test fixtures.
   Returns true after mark-coordinator-running! is called."
  []
  @coordinator-running-atom)

(defn mark-coordinator-running!
  "Mark that coordinator is running, enabling production guards.

   Called once during server startup. After this:
   - when-not-coordinator will skip destructive operations
   - Test fixtures won't corrupt production state"
  []
  (reset! coordinator-running-atom true)
  (log/info "Coordinator marked as running - production guards active"))

(defn mark-coordinator-stopped!
  "Mark that coordinator has stopped. For testing only."
  []
  (reset! coordinator-running-atom false)
  (log/info "Coordinator marked as stopped"))

(defmacro when-not-coordinator
  "Execute body only if coordinator is NOT running.

   CLARITY-Y: Yield safe failure - prevents test fixtures from
   corrupting production state when tests run in same JVM.

   Arguments:
     msg  - Description logged when skipped
     body - Forms to execute when coordinator not running

   Example:
     (when-not-coordinator \"reset-conn! blocked\"
       (reset! conn (create-conn)))"
  [msg & body]
  `(if (coordinator-running?)
     (log/debug "Guarded operation skipped (coordinator running):" ~msg)
     (do ~@body)))
