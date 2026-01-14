(ns hive-mcp.tools.swarm.dispatch
  "Swarm dispatch handler - send prompts to slaves.

   Runs pre-flight conflict checks via coordinator before dispatch.
   Integrates with hive-mcp.swarm.coordinator for task queueing.

   Layer 3: Dispatch wrapper injects shout reminder into ALL prompts,
   achieving ~85% compliance by appending mandatory instructions.

   SOLID: SRP - Single responsibility for dispatch operations.
   CLARITY: I - Inputs validated, conflicts checked before dispatch."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.swarm.coordinator :as coord]
            [clojure.data.json :as json]))

;; ============================================================
;; Layer 3: Shout Reminder Injection
;; ============================================================

(def ^:const shout-reminder-suffix
  "Mandatory suffix appended to ALL dispatch prompts.
   Ensures lings always know to report completion status.

   This is Layer 3 of the 4-Layer Convergence Pattern:
   - Layer 1: Preset footer (system prompt)
   - Layer 2: Terminal introspection (detect idle)
   - Layer 3: Dispatch wrapper (THIS - inject reminder)
   - Layer 4: Sync hooks (auto-check on MCP calls)"
  "\n\n---\nREMINDER: When task is complete, call hivemind_shout with event_type 'completed' and include your task summary in the message. This is MANDATORY for hivemind coordination.")

(defn inject-shout-reminder
  "Append shout reminder to prompt.
   Preserves original prompt content.

   CLARITY: R - Represented intent via explicit suffix."
  [prompt]
  (str prompt shout-reminder-suffix))

;; ============================================================
;; Pre-flight Check Results
;; ============================================================

(defn- handle-blocked-dispatch
  "Handle blocked dispatch due to circular dependency.

   CLARITY: R - Clear error response for blocked state"
  [preflight slave_id]
  (core/mcp-error-json
   {:error "Dispatch blocked: circular dependency detected"
    :status "blocked"
    :would_deadlock (:would-deadlock preflight)
    :slave_id slave_id}))

(defn- handle-queued-dispatch
  "Handle queued dispatch due to file conflicts.

   CLARITY: R - Clear response for queued state"
  [preflight slave_id]
  (core/mcp-success
   {:status "queued"
    :task_id (:task-id preflight)
    :queue_position (:position preflight)
    :conflicts (:conflicts preflight)
    :slave_id slave_id
    :message "Task queued - waiting for file conflicts to clear"}))

(defn- execute-dispatch
  "Execute actual dispatch after pre-flight approval.

   Returns MCP response with task_id on success.
   Injects Layer 3 shout reminder before dispatch.

   CLARITY: Y - Yield safe failure with timeout handling"
  [slave_id prompt timeout_ms effective-files]
  (let [;; Layer 3: Inject shout reminder into prompt
        enhanced-prompt (inject-shout-reminder prompt)
        elisp (format "(json-encode (hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %s))"
                      (v/escape-elisp-string slave_id)
                      (v/escape-elisp-string enhanced-prompt)
                      (or timeout_ms "nil"))
        ;; Dispatch should be quick - 5s default timeout
        {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
    (cond
      timed-out
      (core/mcp-timeout-error "Dispatch operation" :extra-data {:slave_id slave_id})

      success
      (do
        ;; Register file claims for successful dispatch
        (when-let [task-id (try
                             (:task-id (json/read-str result :key-fn keyword))
                             (catch Exception _ nil))]
          (when (seq effective-files)
            (coord/register-task-claims! task-id slave_id effective-files)))
        (core/mcp-success result))

      :else
      (core/mcp-error (str "Error: " error)))))

;; ============================================================
;; Dispatch Handler
;; ============================================================

(defn handle-swarm-dispatch
  "Dispatch a prompt to a slave.
   Runs pre-flight conflict checks before dispatch.
   Uses timeout to prevent MCP blocking.

   Parameters:
   - slave_id: Target slave for dispatch (required)
   - prompt: The prompt/task to send (required)
   - timeout_ms: Optional timeout in milliseconds
   - files: Optional explicit list of files task will modify

   CLARITY: I - Inputs validated via coordinator pre-flight
   SOLID: OCP - Open for extension via coordinator actions"
  [{:keys [slave_id prompt timeout_ms files]}]
  (core/with-swarm
    ;; Pre-flight check: detect conflicts before dispatch
    (let [preflight (coord/dispatch-or-queue!
                     {:slave-id slave_id
                      :prompt prompt
                      :files files
                      :timeout-ms timeout_ms})]
      (case (:action preflight)
        ;; Blocked due to circular dependency - cannot proceed
        :blocked
        (handle-blocked-dispatch preflight slave_id)

        ;; Queued due to file conflicts - will dispatch when conflicts clear
        :queued
        (handle-queued-dispatch preflight slave_id)

        ;; Approved - proceed with dispatch
        :dispatch
        (execute-dispatch slave_id prompt timeout_ms (:files preflight))

        ;; Fallback for unknown action
        (core/mcp-error-json {:error "Unknown pre-flight result"
                              :preflight preflight})))))
