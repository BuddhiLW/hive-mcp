(ns hive-mcp.tools.core
  "Core utilities for MCP tool responses.

   All tool handler modules should require this namespace for
   consistent response formatting.

   Piggyback system delegates to hive-mcp.channel.piggyback for:
   - Instruction queue (hivemind → ling)
   - Message cursors (ling reads hivemind broadcasts)"
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.emacsclient :as ec]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Instruction Queue (delegated to piggyback)
;; =============================================================================

(def push-instruction!
  "Push an instruction to an agent's queue. Delegates to piggyback module."
  piggyback/push-instruction!)

(def drain-instructions!
  "Drain all pending instructions for an agent. Delegates to piggyback module."
  piggyback/drain-instructions!)

(defn- attach-instructions
  "Attach pending instructions to a response if agent-id provided and instructions exist."
  [response agent-id]
  (if agent-id
    (let [instructions (drain-instructions! agent-id)]
      (if (seq instructions)
        (assoc response :pending_instructions instructions)
        response))
    response))

;; =============================================================================
;; MCP Response Constructors
;; =============================================================================

(defn mcp-success
  "Create a successful MCP response. Text can be string or will be pr-str'd.

   Options:
   - :agent-id - If provided, drains and attaches pending instructions for this agent"
  [text & {:keys [agent-id]}]
  (-> {:type "text" :text (if (string? text) text (pr-str text))}
      (attach-instructions agent-id)))

(defn mcp-error
  "Create an error MCP response."
  [message]
  {:type "text" :text message :isError true})

(defn mcp-json
  "Create a successful MCP response with JSON-encoded data.

   Options:
   - :agent-id - If provided, drains and attaches pending instructions for this agent"
  [data & {:keys [agent-id]}]
  (-> {:type "text" :text (json/write-str data)}
      (attach-instructions agent-id)))

;; =============================================================================
;; Addon Availability Checks (DRY Pattern)
;; =============================================================================

(def ^:private addon-feature-map
  "Maps addon keywords to their Elisp feature names.

   Note: :kanban uses 'hive-mcp-org-kanban (with org- prefix)
         All others use 'hive-mcp-<addon-name>"
  {:kanban     "hive-mcp-org-kanban"
   :docs       "hive-mcp-docs"
   :swarm      "hive-mcp-swarm"
   :magit      "hive-mcp-magit"
   :projectile "hive-mcp-projectile"})

(defn addon-available?
  "Check if an addon is loaded in Emacs.

   Accepts keyword from addon-feature-map or string for custom features:
     (addon-available? :kanban)     ; checks 'hive-mcp-org-kanban
     (addon-available? :swarm)      ; checks 'hive-mcp-swarm
     (addon-available? \"my-feat\") ; checks 'my-feat

   CLARITY: Y - Yield safe failure (returns false on error/timeout)"
  [addon-key]
  (let [feature-name (if (keyword? addon-key)
                       (get addon-feature-map addon-key
                            (str "hive-mcp-" (name addon-key)))
                       addon-key)]
    (try
      (let [{:keys [success result]} (ec/eval-elisp (format "(featurep '%s)" feature-name))]
        (and success (= "t" (str/trim (or result "")))))
      (catch Exception _ false))))

(defn addon-not-loaded-error
  "Create error response for unavailable addon.

   Accepts keyword or string, generates appropriate error message."
  [addon-key]
  (let [addon-name (if (keyword? addon-key) (name addon-key) addon-key)]
    (mcp-error (str addon-name " addon not available"))))

(defmacro with-addon
  "Execute body only if addon is available, else return error.

   DRYs up the repeated pattern:
     (if (kanban-addon-available?)
       (do-kanban-thing)
       (mcp-error \"kanban addon not available\"))

   Usage:
     (with-addon :kanban
       (let [result (ec/eval-elisp ...)]
         (mcp-success result)))

   CLARITY: Y - Yield safe failure (graceful addon check)
   SOLID: DRY - Centralizes addon validation pattern"
  [addon-key & body]
  `(if (addon-available? ~addon-key)
     (do ~@body)
     (addon-not-loaded-error ~addon-key)))

;; =============================================================================
;; Elisp Execution Wrapper (DRY Pattern)
;; =============================================================================

(defmacro call-elisp-safe
  "Execute elisp and return MCP response. Handles success/error uniformly.

   Eliminates the repeated pattern:
     (let [{:keys [success result error]} (ec/eval-elisp elisp)]
       (if success
         (mcp-json result)
         (mcp-json {:error error})))

   Usage:
     ;; Simple case - returns mcp-json wrapped result
     (call-elisp-safe \"(my-elisp)\")

     ;; With custom success transform
     (call-elisp-safe \"(my-elisp)\"
       :on-success #(mcp-json {:transformed (:data %)}))

     ;; With custom error handler
     (call-elisp-safe \"(my-elisp)\"
       :on-error #(mcp-error (str \"Custom: \" %)))

   Options:
     :on-success - Function (fn [result] ...) called on success. Default: mcp-json
     :on-error   - Function (fn [error-msg] ...) called on error. Default: (mcp-json {:error msg})"
  [elisp-expr & {:keys [on-success on-error]}]
  `(let [{:keys [~'success ~'result ~'error]} (ec/eval-elisp ~elisp-expr)]
     (if ~'success
       ~(if on-success
          `(~on-success ~'result)
          `(mcp-json ~'result))
       ~(if on-error
          `(~on-error ~'error)
          `(mcp-json {:error ~'error})))))

;; =============================================================================
;; Validation Wrapper (DRY Pattern)
;; =============================================================================

(defmacro with-validation
  "Execute body only if validation passes, else return error response.

   DRYs up the repeated pattern:
     (if-let [validation-error (validate-params params)]
       (mcp-json validation-error)
       (do-actual-work))

   Usage:
     (with-validation [params validate-fn]
       (let [result (process params)]
         (mcp-success result)))

   The validator function should return:
     - nil if valid
     - error map (e.g., {:error \"message\"}) if invalid

   CLARITY: I - Inputs are guarded (validation at boundaries)
   SOLID: DRY - Centralizes validation pattern"
  [[params validator] & body]
  `(if-let [errors# (~validator ~params)]
     (mcp-json errors#)
     (do ~@body)))

;; =============================================================================
;; Hivemind Message Piggyback (delegated to piggyback)
;; =============================================================================

(defn get-hivemind-piggyback
  "Get new hivemind messages since last tool call for specific agent+project.
   Delegates to piggyback module. Returns nil if no new messages.

   Arguments:
   - agent-id: Requesting agent's identifier
   - :project-id: Optional project-id for scoped retrieval

   CRITICAL: project-id scoping prevents cross-project shout leakage."
  [agent-id & {:keys [project-id]}]
  (piggyback/get-messages agent-id :project-id project-id))

(def fetch-hivemind-history
  "Get hivemind messages without marking as read. Delegates to piggyback module."
  piggyback/fetch-history)

(defn mcp-success+hm
  "MCP success response with hivemind piggyback for coordinator.
   Includes any new agent shouts since last tool call.

   Arguments:
   - text: Response text
   - :agent-id: Required to track per-agent read cursors
   - :project-id: Optional project-id for scoped message retrieval

   CRITICAL: project-id scoping ensures coordinators only see their
   project's shouts, preventing cross-project message consumption."
  [text & {:keys [agent-id project-id]}]
  (let [base (mcp-success text :agent-id agent-id)
        hm (when agent-id (get-hivemind-piggyback agent-id :project-id project-id))]
    (if (seq hm)
      (assoc base :_hm hm)
      base)))
