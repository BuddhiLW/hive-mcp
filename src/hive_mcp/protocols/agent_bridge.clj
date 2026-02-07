(ns hive-mcp.protocols.agent-bridge
  "Protocols for agent backend abstraction.

   Defines generic interfaces for programmatic agent interaction:
   - IAgentBackend: Backend lifecycle (connect, disconnect, capabilities)
   - IAgentSession: Session query/response/interrupt cycle
   - IAgentTools: Custom tool registration and MCP server bridging
   - IAgentPermissions: Permission mode and handler configuration
   - ISAAOrchestrator: Silence-Abstract-Act phase orchestration

   Architecture:
   - NoopAgentBackend: No-op fallback (always available, like NoopBrowserAutomation)
   - Active implementation atom pattern (set!/get/clear!)
   - All protocols are backend-agnostic — no Claude-specific concepts

   Implementations (external):
   - hive-agent-bridge: ClaudeSDKBackend via libpython-clj + Claude Agent SDK Python

   SOLID-O: Open for extension via new agent backends.
   SOLID-D: Depend on IAgentBackend abstraction, not concretions.
   SOLID-I: Session, Tools, Permissions, SAA separated.
   CLARITY-Y: Yield safe failure — return error maps, never throw.
   CLARITY-L: Protocol boundary between agent domain and SDK implementation.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IAgentSession Protocol
;;; ============================================================================

(defprotocol IAgentSession
  "Protocol for agent session interaction.

   A session represents an active conversation with an agent backend.
   Sessions support querying, interrupting, and streaming messages.

   Relationship to IAgentBackend:
   - IAgentBackend: Lifecycle (connect, disconnect, capabilities)
   - IAgentSession: Interaction (query, interrupt, receive)

   SOLID-I: Session interaction separate from backend lifecycle."

  (session-id [this]
    "Return unique string identifier for this session.
     Used for registry lookup, logging, session resume.")

  (query! [this prompt opts]
    "Send a prompt to the agent and get a response.

     Arguments:
       prompt - Task/query string
       opts   - Query options:
                :system-prompt   - Override system prompt
                :allowed-tools   - Vector of tool name strings
                :permission-mode - Permission mode keyword
                :max-turns       - Maximum conversation turns
                :max-budget-usd  - Maximum spend in USD
                :resume?         - Resume from previous session state

     Returns:
       core.async channel yielding response messages.
       Channel closes when query completes.")

  (interrupt! [this]
    "Interrupt the current query.

     Returns map with:
       :success?  - Whether interrupt was acknowledged
       :errors    - Vector of error messages")

  (receive-messages [this]
    "Get a core.async channel of streaming messages from current query.

     Returns:
       core.async channel yielding message maps.
       Each message has at minimum :type and :content keys.")

  (receive-response [this]
    "Get a core.async channel that yields the final response.

     Returns:
       core.async channel yielding a single response map,
       then closes."))

;;; ============================================================================
;;; IAgentBackend Protocol
;;; ============================================================================

(defprotocol IAgentBackend
  "Protocol for agent backend lifecycle management.

   Provides the core operations for agent backend control:
   - Availability detection
   - Capability introspection
   - Session creation and teardown
   - One-shot task execution

   Implementations:
   - NoopAgentBackend: No-op fallback (always available)
   - ClaudeSDKBackend: Claude Agent SDK via libpython-clj (external)

   CLARITY-Y: Must not throw — return :success? false on failure.
   CLARITY-T: Log operations for observability."

  (backend-id [this]
    "Return keyword identifier for this backend.
     Examples: :noop, :claude-sdk, :openai-sdk")

  (available? [this]
    "Check if this backend is ready for use.

     Returns boolean.

     A backend is unavailable when:
     - Required dependencies not on classpath
     - External SDK not installed
     - Python initialization failed")

  (capabilities [this]
    "Return set of keyword capabilities this backend supports.

     Possible capabilities:
       :streaming    - Supports streaming message delivery
       :tools        - Supports custom tool registration
       :sessions     - Supports persistent sessions
       :hooks        - Supports lifecycle hooks
       :interrupts   - Supports query interruption
       :permissions  - Supports permission control
       :saa          - Supports SAA phase orchestration
       :mcp-servers  - Supports MCP server registration

     Returns:
       Set of keyword capabilities.")

  (execute! [this task opts]
    "Execute a one-shot task (no persistent session).

     Arguments:
       task - Task description string
       opts - Execution options:
              :cwd             - Working directory
              :system-prompt   - System prompt
              :allowed-tools   - Vector of tool name strings
              :permission-mode - Permission mode keyword
              :max-turns       - Maximum conversation turns
              :max-budget-usd  - Maximum spend in USD
              :env             - Environment variables map

     Returns:
       core.async channel yielding response messages.
       Final message has :type :complete.")

  (connect! [this opts]
    "Create and return an IAgentSession.

     Arguments:
       opts - Connection options:
              :cwd             - Working directory (required)
              :system-prompt   - Base system prompt
              :mcp-servers     - MCP server configurations
              :model           - Model identifier
              :env             - Environment variables map

     Returns map with:
       :success? - Boolean indicating connection success
       :session  - IAgentSession instance (nil on failure)
       :errors   - Vector of error messages")

  (disconnect! [this session]
    "Disconnect and clean up an agent session.

     Arguments:
       session - IAgentSession to disconnect

     Returns map with:
       :success? - Boolean indicating clean disconnect
       :errors   - Vector of error messages

     Idempotent — safe to call multiple times."))

;;; ============================================================================
;;; IAgentTools Protocol
;;; ============================================================================

(defprotocol IAgentTools
  "Protocol for custom tool registration with an agent backend.

   Allows registering Clojure functions as tools the agent can call,
   and configuring MCP servers for additional tool access.

   SOLID-I: Tool management separate from session interaction."

  (register-tool! [this session tool-spec]
    "Register a custom tool with the agent session.

     Arguments:
       session   - IAgentSession to register tool with
       tool-spec - Map with:
                   :name         - Tool name string
                   :description  - Tool description
                   :input-schema - JSON Schema map for inputs
                   :handler      - (fn [args] -> result-map)

     Returns map with:
       :success? - Whether registration succeeded
       :errors   - Vector of error messages")

  (register-mcp-server! [this session server-config]
    "Register an MCP server with the agent session.

     Arguments:
       session       - IAgentSession to register server with
       server-config - Map with:
                       :name     - Server name
                       :type     - Server type (:stdio :sse :http :sdk)
                       :command  - Command for :stdio servers
                       :args     - Args for :stdio servers
                       :url      - URL for :sse/:http servers
                       :tools    - Tool specs for :sdk servers

     Returns map with:
       :success? - Whether registration succeeded
       :errors   - Vector of error messages")

  (list-tools [this session]
    "List tools currently available to the agent session.

     Returns:
       Vector of tool spec maps."))

;;; ============================================================================
;;; IAgentPermissions Protocol
;;; ============================================================================

(defprotocol IAgentPermissions
  "Protocol for agent permission configuration.

   Controls what actions the agent is allowed to take, either via
   a permission mode or a custom handler function.

   SOLID-I: Permission control separate from session interaction."

  (set-permission-mode! [this session mode]
    "Set the permission mode for the agent session.

     Arguments:
       session - IAgentSession
       mode    - Permission mode keyword:
                 :default          - Normal interactive permissions
                 :accept-edits     - Auto-accept file edits
                 :bypass           - Bypass all permission checks

     Returns map with:
       :success? - Whether mode was set
       :errors   - Vector of error messages")

  (set-permission-handler! [this session handler-fn]
    "Set a custom permission handler for the agent session.

     Arguments:
       session    - IAgentSession
       handler-fn - (fn [tool-name input-data context] -> permission-result)

                    permission-result is one of:
                    {:action :allow}
                    {:action :allow :updated-input updated-input-map}
                    {:action :deny :message \"reason\"}
                    {:action :deny :message \"reason\" :interrupt? true}

     Returns map with:
       :success? - Whether handler was set
       :errors   - Vector of error messages"))

;;; ============================================================================
;;; ISAAOrchestrator Protocol
;;; ============================================================================

(defprotocol ISAAOrchestrator
  "Protocol for SAA (Silence-Abstract-Act) phase orchestration.

   SAA is a three-phase strategy for careful, grounded agent work:
   1. Silence: Observe with read-only tools, collect context
   2. Abstract: Synthesize observations into a structured plan
   3. Act: Execute the plan with full tool access

   Each phase maps to a query with different tool sets and permissions.

   SOLID-I: SAA orchestration separate from basic session interaction.
   Philosophy: 'The map is not the territory' — ground first, abstract second."

  (run-silence! [this session task opts]
    "Execute the Silence phase: observe with read-only tools.

     Arguments:
       session - IAgentSession
       task    - Task description string
       opts    - Phase options:
                 :allowed-tools   - Override default read-only tool set
                 :system-prompt   - Additional system prompt
                 :max-turns       - Maximum turns for this phase

     Returns:
       core.async channel yielding observation messages.
       Final message has :type :phase-complete with :observations vector.")

  (run-abstract! [this session observations opts]
    "Execute the Abstract phase: synthesize observations into a plan.

     Arguments:
       session      - IAgentSession
       observations - Vector of observation data from Silence phase
       opts         - Phase options:
                      :allowed-tools   - Override default tool set
                      :system-prompt   - Additional system prompt

     Returns:
       core.async channel yielding synthesis messages.
       Final message has :type :phase-complete with :plan string.")

  (run-act! [this session plan opts]
    "Execute the Act phase: execute the plan with full tools.

     Arguments:
       session - IAgentSession
       plan    - Plan string from Abstract phase
       opts    - Phase options:
                 :allowed-tools   - Override default tool set
                 :permission-mode - Override (default: :accept-edits)
                 :system-prompt   - Additional system prompt

     Returns:
       core.async channel yielding execution messages.
       Final message has :type :phase-complete with :result map.")

  (run-full-saa! [this session task opts]
    "Execute the complete SAA cycle: Silence -> Abstract -> Act.

     Arguments:
       session - IAgentSession
       task    - Task description string
       opts    - Cycle options:
                 :skip-silence?  - Skip Silence phase (default: false)
                 :skip-abstract? - Skip Abstract phase (default: false)
                 :phase-opts     - Per-phase option overrides
                                   {:silence {...} :abstract {...} :act {...}}

     Returns:
       core.async channel yielding messages from all phases.
       Each message tagged with :saa-phase keyword.
       Final message has :type :saa-complete."))

;;; ============================================================================
;;; NoopAgentSession (No-Op Fallback)
;;; ============================================================================

(defrecord NoopAgentSession [id created-at]
  IAgentSession

  (session-id [_] id)

  (query! [_ _prompt _opts]
    (let [ch (clojure.core.async/chan 1)]
      (clojure.core.async/put! ch {:type :error
                                   :message "NoopAgentSession: No agent backend configured. Set one via set-agent-backend!"})
      (clojure.core.async/close! ch)
      ch))

  (interrupt! [_]
    {:success? false
     :errors ["NoopAgentSession: No agent backend configured."]})

  (receive-messages [_]
    (let [ch (clojure.core.async/chan)]
      (clojure.core.async/close! ch)
      ch))

  (receive-response [_]
    (let [ch (clojure.core.async/chan)]
      (clojure.core.async/close! ch)
      ch)))

(defn ->noop-session
  "Create a NoopAgentSession.

   Arguments:
     id - String session identifier (default: auto-generated)"
  ([] (->noop-session (str "noop-agent-session-" (System/currentTimeMillis))))
  ([id] (->NoopAgentSession id (java.time.Instant/now))))

;;; ============================================================================
;;; NoopAgentBackend (No-Op Fallback)
;;; ============================================================================

(def ^:private noop-msg "NoopAgentBackend: No agent backend configured. Set one via set-agent-backend!")

(defrecord NoopAgentBackend []
  IAgentBackend

  (backend-id [_] :noop)

  (available? [_] false)

  (capabilities [_] #{})

  (execute! [_ _task _opts]
    (let [ch (clojure.core.async/chan 1)]
      (clojure.core.async/put! ch {:type :error :message noop-msg})
      (clojure.core.async/close! ch)
      ch))

  (connect! [_ _opts]
    {:success? false
     :session nil
     :errors [noop-msg]})

  (disconnect! [_ _session]
    {:success? true
     :errors []})

  IAgentTools

  (register-tool! [_ _session _tool-spec]
    {:success? false
     :errors [noop-msg]})

  (register-mcp-server! [_ _session _server-config]
    {:success? false
     :errors [noop-msg]})

  (list-tools [_ _session]
    [])

  IAgentPermissions

  (set-permission-mode! [_ _session _mode]
    {:success? false
     :errors [noop-msg]})

  (set-permission-handler! [_ _session _handler-fn]
    {:success? false
     :errors [noop-msg]})

  ISAAOrchestrator

  (run-silence! [_ _session _task _opts]
    (let [ch (clojure.core.async/chan 1)]
      (clojure.core.async/put! ch {:type :error :message noop-msg})
      (clojure.core.async/close! ch)
      ch))

  (run-abstract! [_ _session _observations _opts]
    (let [ch (clojure.core.async/chan 1)]
      (clojure.core.async/put! ch {:type :error :message noop-msg})
      (clojure.core.async/close! ch)
      ch))

  (run-act! [_ _session _plan _opts]
    (let [ch (clojure.core.async/chan 1)]
      (clojure.core.async/put! ch {:type :error :message noop-msg})
      (clojure.core.async/close! ch)
      ch))

  (run-full-saa! [_ _session _task _opts]
    (let [ch (clojure.core.async/chan 1)]
      (clojure.core.async/put! ch {:type :error :message noop-msg})
      (clojure.core.async/close! ch)
      ch)))

;;; ============================================================================
;;; Active Implementation Management
;;; ============================================================================

;; Atom holding the currently active IAgentBackend implementation.
;; Defaults to NoopAgentBackend (no-op fallback).
(defonce ^:private active-agent-backend (atom nil))

(defn set-agent-backend!
  "Set the active agent backend implementation.

   Arguments:
     impl - Implementation of IAgentBackend protocol

   Returns:
     The implementation.

   Throws:
     AssertionError if impl doesn't satisfy protocol."
  [impl]
  {:pre [(satisfies? IAgentBackend impl)]}
  (reset! active-agent-backend impl)
  impl)

(defn get-agent-backend
  "Get the active agent backend implementation.

   Returns NoopAgentBackend if no backend is set.
   This ensures hive-mcp always works, with or without a real backend."
  []
  (or @active-agent-backend
      (->NoopAgentBackend)))

(defn agent-backend-set?
  "Check if an agent backend is configured.

   Returns:
     true if set-agent-backend! has been called."
  []
  (some? @active-agent-backend))

(defn clear-agent-backend!
  "Clear the active agent backend. Used for testing.

   Returns nil."
  []
  (reset! active-agent-backend nil)
  nil)

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defn agent-backend?
  "Check if object implements IAgentBackend protocol."
  [x]
  (satisfies? IAgentBackend x))

(defn agent-session?
  "Check if object implements IAgentSession protocol."
  [x]
  (satisfies? IAgentSession x))

(defn enhanced?
  "Check if enhanced agent backend capabilities are available.

   Returns:
     true if a non-noop IAgentBackend implementation is active."
  []
  (and (agent-backend-set?)
       (not (instance? NoopAgentBackend @active-agent-backend))))

(defn backend-capabilities
  "Get a summary of available agent backend capabilities.

   Returns:
     Map describing what features are available."
  []
  (let [backend (get-agent-backend)]
    {:backend-id (backend-id backend)
     :enhanced? (enhanced?)
     :available? (available? backend)
     :capabilities (capabilities backend)}))
