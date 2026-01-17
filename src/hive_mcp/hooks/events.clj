(ns hive-mcp.hooks.events
  "Canonical event definitions for the hooks domain.

   Provides type-safe constructors for all hook events:
   - task-start, task-complete
   - session-start, session-end
   - file-modified, error
   - ling-spawn, ling-terminate

   All events are validated against specs and include:
   - :event - the event type keyword
   - :timestamp - java.time.Instant when event was created
   - Additional context fields per event type

   SOLID: Single responsibility - event construction only
   CLARITY: Inputs guarded via spec validation"
  (:require [hive-mcp.specs.hooks :as specs])
  (:import [java.time Instant]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Validation Helpers
;; =============================================================================

(defn- validate-required!
  "Validates required keys are present and non-empty.
   Throws ExceptionInfo if validation fails."
  [context required-keys event-type]
  (doseq [k required-keys]
    (let [v (get context k)]
      (when (or (nil? v)
                (and (string? v) (empty? v))
                (and (coll? v) (empty? v)))
        (throw (ex-info (str "Missing required field '" (name k) "' for " event-type)
                        {:event-type event-type
                         :missing-key k
                         :context context}))))))

(defn- validate-event-type!
  "Validates event type is in the allowed set."
  [event-type]
  (when-not (specs/valid-hook-event? event-type)
    (throw (ex-info (str "Invalid event type: " event-type)
                    {:event-type event-type
                     :valid-events #{:task-start :task-complete :session-start :session-end
                                     :file-modified :error :ling-spawn :ling-terminate}}))))

;; =============================================================================
;; Base Event Constructor
;; =============================================================================

(defn create-event
  "Generic event constructor. Creates an event of the given type with context.

   Arguments:
   - event-type: Keyword from hook-events set
   - context: Map of event context data

   Returns: Event map with :event, :timestamp, and context fields merged

   Throws: ExceptionInfo if event-type is invalid"
  [event-type context]
  (validate-event-type! event-type)
  (merge context
         {:event event-type
          :timestamp (Instant/now)}))

;; =============================================================================
;; Task Events
;; =============================================================================

(defn task-start-event
  "Creates a task-start event.

   Required:
   - :task - Task description (non-empty string)
   - :slave-id - Agent identifier

   Optional:
   - :files - Files involved
   - :data - Additional context"
  [{:keys [_task _slave-id] :as context}]
  (validate-required! context [:task :slave-id] :task-start)
  (create-event :task-start context))

(defn task-complete-event
  "Creates a task-complete event.

   Required:
   - :task - Task description
   - :slave-id - Agent identifier

   Optional:
   - :files - Files modified
   - :message - Completion message
   - :data - Additional context"
  [{:keys [_task _slave-id] :as context}]
  (validate-required! context [:task :slave-id] :task-complete)
  (create-event :task-complete context))

;; =============================================================================
;; Session Events
;; =============================================================================

(defn session-start-event
  "Creates a session-start event.

   Required:
   - :slave-id - Session identifier

   Optional:
   - :data - Session context (project, branch, etc.)"
  [{:keys [_slave-id] :as context}]
  (validate-required! context [:slave-id] :session-start)
  (create-event :session-start context))

(defn session-end-event
  "Creates a session-end event.

   Required:
   - :slave-id - Session identifier

   Optional:
   - :data - Session summary (:wrap-completed, :summary-id, etc.)
   - :message - Closing message"
  [{:keys [_slave-id] :as context}]
  (validate-required! context [:slave-id] :session-end)
  (create-event :session-end context))

;; =============================================================================
;; File Events
;; =============================================================================

(defn file-modified-event
  "Creates a file-modified event.

   Required:
   - :files - Non-empty vector of file paths
   - :slave-id - Agent identifier

   Optional:
   - :task - Related task
   - :message - Description of changes"
  [{:keys [_files _slave-id] :as context}]
  (validate-required! context [:files] :file-modified)
  (create-event :file-modified context))

;; =============================================================================
;; Error Events
;; =============================================================================

(defn error-event
  "Creates an error event.

   Required:
   - :error - Error (string, exception, or map)
   - :slave-id - Agent identifier

   Optional:
   - :task - Task that failed
   - :message - Error summary
   - :data - Additional error context"
  [{:keys [_error _slave-id] :as context}]
  (validate-required! context [:error :slave-id] :error)
  (create-event :error context))

;; =============================================================================
;; Ling (Agent) Events
;; =============================================================================

(defn ling-spawn-event
  "Creates a ling-spawn event when a new agent is created.

   Required:
   - :slave-id - New agent identifier

   Optional:
   - :data - Spawn context (:presets, :cwd, :role, etc.)"
  [{:keys [_slave-id] :as context}]
  (validate-required! context [:slave-id] :ling-spawn)
  (create-event :ling-spawn context))

(defn ling-terminate-event
  "Creates a ling-terminate event when an agent is terminated.

   Required:
   - :slave-id - Agent identifier

   Optional:
   - :message - Termination reason
   - :data - Final state/stats"
  [{:keys [_slave-id] :as context}]
  (validate-required! context [:slave-id] :ling-terminate)
  (create-event :ling-terminate context))

;; =============================================================================
;; Event -> Shout Payload Conversion
;; =============================================================================

(defn- format-error-message
  "Formats error for shout payload message."
  [error]
  (cond
    (string? error) error
    (instance? Throwable error) (.getMessage ^Throwable error)
    (map? error) (or (:message error) (pr-str error))
    :else (str error)))

(defn event->shout-payload
  "Converts an event to a hivemind shout payload.

   The payload follows the ::specs/hook-payload spec:
   - :hook-type - Keyword identifying the event type
   - :files - Optional file list
   - :message - Human-readable description
   - :data - Optional additional data

   Arguments:
   - event: Event map from any event constructor

   Returns: Map suitable for hivemind/shout!"
  [{:keys [event task files message error data slave-id] :as _evt}]
  (let [base-payload {:hook-type event}
        ;; Add files if present
        with-files (if (seq files)
                     (assoc base-payload :files files)
                     base-payload)
        ;; Generate appropriate message
        gen-message (case event
                      :task-start (str "Started: " task)
                      :task-complete (or message (str "Completed: " task))
                      :session-start (str "Session started: " slave-id)
                      :session-end (or message (str "Session ended: " slave-id))
                      :file-modified (or message (str "Modified: " (count files) " file(s)"))
                      :error (str "Error: " (format-error-message error))
                      :ling-spawn (str "Spawned: " slave-id)
                      :ling-terminate (or message (str "Terminated: " slave-id))
                      (str event))
        with-message (assoc with-files :message gen-message)
        ;; Add data if present
        with-data (if data
                    (assoc with-message :data data)
                    with-message)]
    with-data))

;; =============================================================================
;; Event Predicates
;; =============================================================================

(defn task-event?
  "Returns true if event is a task-related event."
  [event]
  (contains? #{:task-start :task-complete} (:event event)))

(defn session-event?
  "Returns true if event is a session-related event."
  [event]
  (contains? #{:session-start :session-end} (:event event)))

(defn ling-event?
  "Returns true if event is a ling/agent-related event."
  [event]
  (contains? #{:ling-spawn :ling-terminate} (:event event)))

(defn error-event?
  "Returns true if event is an error event."
  [event]
  (= :error (:event event)))
