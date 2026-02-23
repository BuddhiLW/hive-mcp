(ns hive-mcp.agent.agentic-loop
  "IAgenticLoop protocol — rich abstraction for headless agentic sessions.

   Provides async lifecycle, collect-with-timeout, cost tracking, transcript
   access, mid-session constraints, and control gradient (transparent vs opaque).

   Architecture:
     IAgenticLoop is the richer protocol beneath IHeadlessBackend.
     An adapter (hive-agent.loop.headless-adapter) maps IAgenticLoop
     to the 6-method IHeadlessBackend interface for registry compatibility.

     Transparent loops (e.g. TransparentAgenticLoop) broker tools in-process —
     the caller sees every tool call and can inject results.

     Opaque loops (e.g. ClaudeSDKAgenticLoop) delegate iteration to an external
     SDK — the caller hooks lifecycle events but doesn't broker tools.

   Control gradient:
     :cap/transparent — loop exposes tool calls, caller brokers results
     :cap/opaque     — loop iterates internally, caller hooks events

   See also:
   - hive-mcp.addons.headless          — IHeadlessBackend (6-method interface)
   - hive-agent.loop.agentic           — TransparentAgenticLoop (IP)
   - hive-claude.sdk.agentic-loop      — ClaudeSDKAgenticLoop (AGPL)
   - hive-agent.loop.headless-adapter  — IAgenticLoop -> IHeadlessBackend adapter")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; IAgenticLoop Protocol
;; =============================================================================

(defprotocol IAgenticLoop
  "Rich protocol for headless agentic sessions with lifecycle control,
   cost tracking, transcript access, and mid-session constraints.

   Implementors choose a control gradient:
   - Transparent: tool calls exposed, caller brokers results (tool-results!)
   - Opaque: iteration delegated to SDK, caller hooks events

   Lifecycle: start! -> send-message! -> collect-response! -> abort!
   State machine: :idle -> :running -> :done | :errored"

  (start! [this config]
    "Start the agentic session with the given configuration.
     Config keys depend on implementation:
       :task          — initial task string
       :model         — model ID
       :max-turns     — turn limit
       :cwd           — working directory
       :preset-content — system prompt
     Returns: {:session-id str}")

  (abort! [this]
    "Abort the running session. Idempotent — safe to call multiple times.
     Returns: {:aborted? bool}")

  (session-state [this]
    "Return the current session state keyword.
     Returns: :idle | :running | :done | :errored")

  (send-message! [this message]
    "Send a message/task to the running session.
     For transparent loops: injects into the message queue between turns.
     For opaque loops: dispatches via the SDK.
     Returns: implementation-specific (task-id, channel, or promise)")

  (collect-response! [this opts]
    "Blocking collect with timeout. Waits for the session to produce a result.
     Opts:
       :timeout-ms — max wait time in milliseconds (default: 60000)
     Returns: {:result str :cost-usd double :turns int}
              or {:timeout true} on timeout")

  (cost [this]
    "Return accumulated cost and turn information.
     Returns: {:total-cost-usd double :turns int}")

  (transcript [this]
    "Return the full transcript of messages exchanged.
     Returns: vector of message maps [{:role str :content str ...} ...]")

  (tool-results! [this results]
    "Inject tool execution results (transparent loops only).
     For opaque loops, throws or returns {:unsupported true}.
     Results: vector of {:tool_call_id str :content str}
     Returns: {:accepted? bool}")

  (hooks [this]
    "Return the set of capability/hook keywords this loop supports.
     Always includes :cap/transparent or :cap/opaque.
     May include: :pre-tool-use :post-tool-use :PreToolUse :PostToolUse
                  :Notification :Stop :PreCompact
     Returns: set of keywords")

  (constrain! [this constraints]
    "Apply mid-session constraints to the running loop.
     Constraints map:
       :max-turns    — turn limit (overrides initial config)
       :max-cost-usd — cost budget in USD
       :tools        — restrict available tools
     Returns: {:applied? bool}"))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn agentic-loop?
  "Check if object implements IAgenticLoop."
  [x]
  (satisfies? IAgenticLoop x))

(defn transparent?
  "Check if an agentic loop is transparent (brokers tools in-process).
   Returns false for non-IAgenticLoop objects."
  [x]
  (and (agentic-loop? x)
       (contains? (hooks x) :cap/transparent)))

(defn opaque?
  "Check if an agentic loop is opaque (delegates iteration to SDK).
   Returns false for non-IAgenticLoop objects."
  [x]
  (and (agentic-loop? x)
       (contains? (hooks x) :cap/opaque)))
