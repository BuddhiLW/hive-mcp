# ADR-003: Non-Blocking Prompt Forwarding

## Status
Proposed

## Context

### Problem
The current `hivemind_ask` implementation blocks Emacs when called:

```clojure
;; hivemind.clj:155-157 - BLOCKING!
(let [result (alt!!
               response-chan ([v] v)
               (timeout timeout-ms) {:timeout true :ask-id ask-id})]
```

When an agent calls `hivemind_ask` via MCP, the JSON-RPC call blocks until:
1. A human responds via Emacs (`respond-ask!`)
2. The timeout expires (default 5 minutes)

This freezes Emacs during the entire wait period because MCP tool calls are synchronous.

### Current Architecture Gap

```
Agent calls hivemind_ask (MCP tool)
        │
        ▼
    ask! blocks on alt!!  ◄── PROBLEM: Blocks Emacs
        │
        ▼
    Emacs receives :hivemind-ask event
        │
        ▼
    User sees prompt, responds via hive-mcp-hivemind-respond
        │
        ▼
    Elisp sends "hivemind-response" via channel
        │
        ▼
    ??? ◄── BUG: No handler routes this to respond-ask!
```

The Emacs response goes into the void because:
1. Channel publishes `hivemind-response` to internal pub/sub
2. **No subscriber** listens for this event type
3. The blocked `ask!` never receives the response

### Requirements

1. **Non-blocking emit**: Agent can submit prompt and continue work
2. **Multiple response paths**: Poll, callback, or Emacs channel
3. **Desktop notifications**: Urgent prompts trigger notify-send
4. **State visibility**: Coordinator can see all pending prompts
5. **Timeout handling**: Expired prompts cleaned up automatically

## Decision

Implement three-layer DDD architecture for prompt lifecycle management:

```
┌─────────────────────────────────────────────────────────────────────┐
│                        APPLICATION LAYER                            │
│  src/hive_mcp/prompts/application.clj                              │
│                                                                     │
│  emit-prompt!  poll-prompt  respond-prompt!  list-pending          │
│       │             │            │               │                  │
└───────┼─────────────┼────────────┼───────────────┼──────────────────┘
        │             │            │               │
        ▼             ▼            ▼               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         DOMAIN LAYER                                │
│  src/hive_mcp/prompts/domain.clj                                   │
│                                                                     │
│  Prompt lifecycle: :pending → :responded | :expired | :cancelled   │
│  Pure functions: create-prompt, respond-prompt, expire-prompt      │
│  Value objects: Prompt, PromptResponse                             │
└─────────────────────────────────────────────────────────────────────┘
        │             │            │               │
        ▼             ▼            ▼               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      INFRASTRUCTURE LAYER                           │
│  src/hive_mcp/prompts/infra.clj                                    │
│                                                                     │
│  store-prompt!       channel-broadcast!       notify!              │
│  update-prompt!      subscribe-responses!     callback-notify!     │
│  get-prompt                                                         │
└─────────────────────────────────────────────────────────────────────┘
```

## Module Responsibilities

### Domain Layer: `prompts/domain.clj`

Pure functions defining prompt lifecycle. No side effects, no I/O.

```clojure
;; State machine
(def prompt-states #{:pending :responded :expired :cancelled})

;; Value objects
(defrecord Prompt [id agent-id question options
                   created-at timeout-ms state callback-fn])

(defrecord PromptResponse [decision by responded-at])

;; Pure transitions
(defn create-prompt [agent-id question options timeout-ms callback-fn]
  ;; Returns Prompt with :pending state
  )

(defn respond-prompt [prompt decision by]
  ;; Returns updated Prompt with :responded state and PromptResponse
  )

(defn expire-prompt [prompt]
  ;; Returns Prompt with :expired state
  )

(defn prompt-expired? [prompt now]
  ;; Pure predicate: created-at + timeout-ms < now
  )
```

### Application Layer: `prompts/application.clj`

Orchestrates domain logic with infrastructure. Entry points for MCP tools.

```clojure
(defn emit-prompt!
  "Non-blocking prompt submission. Returns {:ask-id ... :status :pending}."
  [agent-id question options & {:keys [timeout-ms callback-fn]}]
  ;; 1. Domain: create-prompt
  ;; 2. Infra: store-prompt!
  ;; 3. Infra: channel-broadcast!
  ;; 4. Infra: notify! (if urgent)
  ;; 5. Return ask-id immediately (NON-BLOCKING)
  )

(defn poll-prompt
  "Check prompt status. Returns {:status :pending|:responded|:expired, ...}."
  [ask-id]
  ;; 1. Infra: get-prompt
  ;; 2. Domain: prompt-expired? check
  ;; 3. If expired, expire-prompt and update
  ;; 4. Return current state
  )

(defn respond-prompt!
  "Record response to prompt. Called from channel handler."
  [ask-id decision by]
  ;; 1. Infra: get-prompt
  ;; 2. Domain: respond-prompt
  ;; 3. Infra: update-prompt!
  ;; 4. Infra: callback-notify! (if callback-fn registered)
  )

(defn list-pending
  "List all pending prompts for coordinator visibility."
  []
  ;; Infra: query pending prompts
  )

(defn start-response-listener!
  "Subscribe to channel for hivemind-response events."
  []
  ;; Subscribe to :hivemind-response
  ;; Route to respond-prompt!
  )
```

### Infrastructure Layer: `prompts/infra.clj` (extend existing)

External system adapters. Already has D-Bus notifications.

```clojure
;; Storage (atom-based for now, could be Chroma later)
(defonce prompt-store (atom {}))

(defn store-prompt! [prompt]
  (swap! prompt-store assoc (:id prompt) prompt))

(defn get-prompt [ask-id]
  (get @prompt-store ask-id))

(defn update-prompt! [ask-id updates]
  (swap! prompt-store update ask-id merge updates))

(defn query-by-state [state]
  (->> @prompt-store vals (filter #(= state (:state %)))))

;; Channel integration
(defn channel-broadcast! [event]
  (channel/broadcast! event))

(defn subscribe-responses! [handler-fn]
  ;; Subscribe to :hivemind-response, call handler-fn
  (let [sub-ch (channel/subscribe! :hivemind-response)]
    (async/go-loop []
      (when-let [msg (async/<! sub-ch)]
        (handler-fn msg)
        (recur)))))

;; Callback notification
(defn callback-notify! [prompt response]
  (when-let [cb (:callback-fn prompt)]
    (cb response)))

;; Already exists: notify!, notify-permission-request!, etc.
```

## Data Flow Diagram

```
┌────────────────────────────────────────────────────────────────────────────┐
│                              NON-BLOCKING FLOW                             │
└────────────────────────────────────────────────────────────────────────────┘

  AGENT                      CLOJURE MCP                        EMACS
    │                            │                                │
    │  hivemind_ask_async        │                                │
    │  (question, options)       │                                │
    │ ─────────────────────────► │                                │
    │                            │                                │
    │                      emit-prompt!                           │
    │                            │                                │
    │                            ├─► store-prompt!                │
    │                            ├─► channel-broadcast!───────────┼──► :hivemind-ask
    │                            ├─► notify! (D-Bus)              │     event received
    │                            │                                │
    │  {:ask-id "abc" :pending}  │                                │
    │ ◄───────────────────────── │                                │
    │                            │                                │
    │  (agent continues work)    │                                │
    │                            │                                │
    │                            │                        Human responds
    │                            │                                │
    │                            │  :hivemind-response ◄──────────┤
    │                            │  {ask-id, decision, by}        │
    │                            │                                │
    │                      subscribe-responses!                   │
    │                            │                                │
    │                      respond-prompt!                        │
    │                            │                                │
    │                            ├─► update-prompt! (:responded)  │
    │                            ├─► callback-notify! (if set)    │
    │                            │                                │
    │  hivemind_poll(ask-id)     │                                │
    │ ─────────────────────────► │                                │
    │                            │                                │
    │  {:status :responded       │                                │
    │   :decision "yes"          │                                │
    │   :by "human"}             │                                │
    │ ◄───────────────────────── │                                │
    │                            │                                │


┌────────────────────────────────────────────────────────────────────────────┐
│                             CALLBACK FLOW                                  │
└────────────────────────────────────────────────────────────────────────────┘

  AGENT                      CLOJURE MCP                        EMACS
    │                            │                                │
    │  emit-prompt!              │                                │
    │  :callback-fn my-handler   │                                │
    │ ─────────────────────────► │                                │
    │                            │                                │
    │  {:ask-id "abc"}           │                                │
    │ ◄───────────────────────── │                                │
    │                            │                                │
    │                            │  :hivemind-response ◄──────────┤
    │                            │                                │
    │                      respond-prompt!                        │
    │                            │                                │
    │  callback-fn invoked ◄─────┤                                │
    │  with {:decision ...}      │                                │
    │                            │                                │
```

## MCP Tool Changes

### New Tools

```clojure
{:name "hivemind_ask_async"
 :description "Non-blocking prompt. Returns ask-id immediately.
               Use hivemind_poll to check for response."
 :inputSchema {...}
 :handler #'application/emit-prompt!}

{:name "hivemind_poll"
 :description "Check status of a prompt. Returns response if available."
 :inputSchema {:properties {"ask_id" {:type "string"}}}
 :handler #'application/poll-prompt}
```

### Deprecate (but keep for compatibility)

```clojure
{:name "hivemind_ask"
 :description "DEPRECATED: Blocks Emacs. Use hivemind_ask_async + hivemind_poll."
 :handler (fn [args]
            (log/warn "hivemind_ask is blocking! Consider hivemind_ask_async")
            (current-blocking-impl args))}
```

## Migration Path

1. **Phase 1**: Add new modules alongside existing hivemind.clj
2. **Phase 2**: Wire channel response listener on startup
3. **Phase 3**: Add `hivemind_ask_async` and `hivemind_poll` tools
4. **Phase 4**: Update agent presets to use async pattern
5. **Phase 5**: Deprecate blocking `hivemind_ask`

## Consequences

### Positive
- Emacs no longer freezes during prompt wait
- Agents can continue work while waiting for human input
- Clear separation of concerns (DDD layers)
- Desktop notifications work independently of blocking
- Callback pattern enables reactive agent architectures

### Negative
- More complex agent logic (must poll or handle callbacks)
- Two code paths during migration (blocking + async)
- Additional state management (prompt store lifecycle)

### Neutral
- Response latency unchanged (human is the bottleneck)
- Memory usage slightly increased (stored prompts)

## Implementation Notes

### Startup Wiring

```clojure
;; In server startup
(defn start! []
  ;; ... existing startup ...
  (prompts.application/start-response-listener!))
```

### Timeout Cleanup

```clojure
;; Periodic cleanup of expired prompts
(defn start-cleanup-timer! []
  (async/go-loop []
    (async/<! (async/timeout 60000)) ; Every minute
    (doseq [prompt (infra/query-by-state :pending)]
      (when (domain/prompt-expired? prompt (System/currentTimeMillis))
        (application/expire-prompt! (:id prompt))))
    (recur)))
```

### Testing Strategy

```clojure
;; Domain tests (pure)
(deftest create-prompt-test
  (let [p (domain/create-prompt "agent-1" "Delete files?" ["yes" "no"] 5000 nil)]
    (is (= :pending (:state p)))
    (is (some? (:id p)))))

;; Application tests (with mocked infra)
(deftest emit-prompt-test
  (with-redefs [infra/store-prompt! (fn [p] (is (= :pending (:state p))))
                infra/channel-broadcast! (fn [e] (is (= :hivemind-ask (:type e))))]
    (let [result (application/emit-prompt! "agent-1" "Question?" nil)]
      (is (contains? result :ask-id)))))
```

## References

- [hive-mcp/hivemind.clj](../../src/hive_mcp/hivemind.clj) - Current blocking implementation
- [hive-mcp/channel.clj](../../src/hive_mcp/channel.clj) - Bidirectional channel
- [hive-mcp/prompts/infra.clj](../../src/hive_mcp/prompts/infra.clj) - D-Bus notifications
- [hive-mcp-hivemind.el](../../elisp/hive-mcp-hivemind.el) - Emacs coordinator UI

---

## TBD: Terminal Forwarding to Claude Code

**Status**: Deferred (2026-01-08) - User may use Claude inside Emacs instead

### Context

When using Claude Code in a terminal session, prompts from `hivemind_ask` appear in Emacs but the user is working in the terminal. The question: can we forward Emacs prompts to the Claude terminal?

### Current Flow

```
┌─────────────┐     broadcast      ┌─────────────┐
│ Ling calls  │ ─────────────────► │   Emacs     │
│ hivemind_ask│                    │ (receives)  │
└─────────────┘                    └──────┬──────┘
       │                                  │
       │ blocks on                        │ shows in minibuffer
       │ response-chan                    │ + desktop notification
       ▼                                  ▼
┌─────────────┐     respond-ask!   ┌─────────────┐
│  Waits...   │ ◄───────────────── │ User types  │
└─────────────┘                    │ response    │
                                   └─────────────┘
```

**Problem**: User is in Claude terminal, not Emacs. Desktop notifications help but require context switch.

### Considered Approaches

| Approach | Pros | Cons |
|----------|------|------|
| **A. Long-poll tool** (`hivemind_await_ask`) | Clean, blocks Claude until ask arrives | Ties up tool slot while waiting |
| **B. Watch *Hivemind Log* buffer** | Uses existing `mcp_watch_buffer` infra | Polling, must parse log text |
| **C. Insert prompt into terminal** | Immediate visibility | Intrusive, complex Emacs→terminal IPC |
| **D. MCP tool notification hook** | Native Claude flow | Requires new MCP protocol feature |

### Recommended: Approach A

Add a `hivemind_await_ask` tool that:
1. Blocks until a pending ask exists (or timeout)
2. Returns the question + options
3. Coordinator responds via `hivemind_respond`

```clojure
{:name "hivemind_await_ask"
 :description "Block until an agent needs human input. Returns pending ask or timeout."
 :inputSchema {:properties {"timeout_ms" {:type "integer" :default 60000}}}
 :handler (fn [{:keys [timeout_ms]}]
            (let [result-ch (async/chan 1)]
              ;; Subscribe to new asks, return first one or timeout
              ...))}
```

### Alternative: Claude Inside Emacs

If using Claude inside Emacs (via `claude-code-ide.el` or similar), prompts naturally appear in the same environment. This may obviate the need for terminal forwarding entirely.

### Decision

Deferred. Revisit if terminal-based Claude workflow becomes primary use case.
