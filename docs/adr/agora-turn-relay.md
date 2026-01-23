# ADR: Event-Driven Agora Turn Relay

## Status
PROPOSED

## Context
Agora dialogues stop after each turn. When a ling dispatches a message via `agora_dispatch`, the target ling receives it but:
1. Doesn't know they're in an Agora dialogue
2. Doesn't know to respond via `agora_dispatch`
3. Goes idle after responding naturally

This violates the Behavioral Architecture principle [Arch>Bh]p - agents should be autonomous.

## Decision

### Architecture: Two-Layer Solution

**Layer 1: Prompt Enhancement**
Inject Agora context into every dispatch prompt so lings understand:
- They're in an Agora dialogue
- How to respond (via `agora_dispatch`)
- Nash equilibrium signal semantics

**Layer 2: Event-Driven Relay**
After `agora_dispatch` succeeds:
1. Emit `:agora/turn-dispatched` event
2. Handler checks dialogue status (consensus reached?)
3. If active, format and relay to target ling's terminal

### Data Flow

```
┌────────────────────────────────────────────────────────────────┐
│                    AGORA TURN LIFECYCLE                        │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  Ling A calls agora_dispatch(to: Ling B, message: "...")       │
│                          │                                     │
│                          ▼                                     │
│  ┌────────────────────────────────────────────┐               │
│  │ tools/agora.clj:handle-agora-dispatch      │               │
│  │  └─> dialogue/dialogue-dispatch            │               │
│  │       ├─ record turn to DataScript         │               │
│  │       ├─ check Nash equilibrium            │               │
│  │       └─ emit :agora/turn-dispatched ◄─────┼── NEW         │
│  └────────────────────────────────────────────┘               │
│                          │                                     │
│                          ▼                                     │
│  ┌────────────────────────────────────────────┐               │
│  │ handlers/agora.clj (NEW)                   │               │
│  │  handle-agora-turn-dispatched              │               │
│  │   ├─ check: dialogue still active?         │               │
│  │   ├─ check: target ling alive?             │               │
│  │   ├─ format: inject Agora context prompt   │               │
│  │   └─ effect: {:swarm-send-prompt ...}      │               │
│  └────────────────────────────────────────────┘               │
│                          │                                     │
│                          ▼                                     │
│  ┌────────────────────────────────────────────┐               │
│  │ Ling B's Terminal                          │               │
│  │  Receives enhanced prompt with:            │               │
│  │   - Dialogue ID                            │               │
│  │   - Sender info                            │               │
│  │   - Expected response format               │               │
│  │   - Signal semantics                       │               │
│  └────────────────────────────────────────────┘               │
│                          │                                     │
│                          ▼                                     │
│  Ling B responds via agora_dispatch ─► Cycle continues        │
│                                                                │
└────────────────────────────────────────────────────────────────┘
```

### Prompt Enhancement Template

```
---
AGORA DIALOGUE CONTEXT
Dialogue ID: {dialogue-id}
From: {sender-slave-id}
Topic: {topic}

You are participating in a multi-agent Nash Equilibrium dialogue.
After reading the message, respond using agora_dispatch with:
- dialogue_id: "{dialogue-id}"
- to: "{sender-slave-id}" (or another participant)
- signal: One of [propose|counter|approve|no-change|defer]
- message: Your response

Signal Guide:
- propose: You're suggesting a change/new idea
- counter: You disagree and have an alternative
- approve: You accept the current state
- no-change: No changes needed from your perspective
- defer: You yield to others' judgment

Consensus is reached when all participants signal approve/no-change.
---

{original-message}
```

## Edge Cases

### 1. Dead/Stuck Ling
**Detection**: Query DataScript for ling status before dispatch
**Action**: Skip relay, emit `:agora/ling-unavailable` event
**Fallback**: Coordinator notified, can intervene or timeout dialogue

### 2. Infinite Loops
**Prevention**: Max turns limit per dialogue (configurable, default: 50)
**Detection**: Check turn count before relay
**Action**: Set dialogue status to `:timeout`, emit `:agora/max-turns-reached`

### 3. Consensus Reached
**Detection**: `dialogue/nash-equilibrium?` returns true
**Action**: Do NOT relay, dialogue is complete
**Status**: Already handled - dialogue status set to `:consensus`

### 4. Ling Not in Dialogue
**Detection**: Check `dialogue/get-participants` before relay
**Action**: Skip relay, log warning

## Implementation Plan

### Phase 1: Event Emission (agora/dialogue.clj)
```clojure
;; In dialogue-dispatch, after successful dispatch:
(ws/emit! :agora/turn-dispatched
  {:dialogue-id dialogue-id
   :from sender-id
   :to to
   :turn-num (:turn-num turn)
   :signal final-signal
   :message cleaned-message
   :topic (:topic (get-dialogue dialogue-id))})
```

### Phase 2: New Handler Module (handlers/agora.clj)
```clojure
(ns hive-mcp.events.handlers.agora
  "Agora dialogue event handlers."
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.consensus :as consensus]))

(def ^:const max-turns 50)

(def agora-context-template
  "Template for injecting Agora context into prompts."
  "---
AGORA DIALOGUE CONTEXT
Dialogue ID: %s
From: %s
Topic: %s

You are in a Nash Equilibrium dialogue. Respond via agora_dispatch:
- dialogue_id: \"%s\"
- to: \"%s\" (or another participant)
- signal: [propose|counter|approve|no-change|defer]

Signals: propose/counter=reset equilibrium, approve/no-change=toward consensus
---

%s")

(defn- format-agora-prompt
  "Inject Agora context into message."
  [{:keys [dialogue-id from topic message]}]
  (format agora-context-template
          dialogue-id from (or topic "Unspecified")
          dialogue-id from
          message))

(defn handle-agora-turn-dispatched
  "Relay Agora turn to target ling's terminal."
  [_coeffects [_ {:keys [dialogue-id from to turn-num message topic]}]]
  (let [dialogue (dialogue/get-dialogue dialogue-id)]
    (cond
      ;; Dialogue already reached consensus
      (= :consensus (:status dialogue))
      {:log {:level :info
             :message (str "Agora " dialogue-id " already at consensus, not relaying")}}

      ;; Max turns exceeded
      (>= turn-num max-turns)
      {:log {:level :warn
             :message (str "Agora " dialogue-id " exceeded max turns (" max-turns ")")}
       :dispatch [:agora/timeout {:dialogue-id dialogue-id
                                  :reason :max-turns}]}

      ;; Normal case: relay to target
      :else
      {:log {:level :debug
             :message (str "Relaying Agora turn to " to)}
       :swarm-send-prompt {:slave-id to
                           :prompt (format-agora-prompt
                                    {:dialogue-id dialogue-id
                                     :from from
                                     :topic topic
                                     :message message})}})))

(defn register-handlers!
  []
  (ev/reg-event :agora/turn-dispatched
                [interceptors/debug]
                handle-agora-turn-dispatched))
```

### Phase 3: New Effect (effects.clj)
```clojure
;; :swarm-send-prompt effect - Send prompt directly to ling terminal
(defn- handle-swarm-send-prompt
  [{:keys [slave-id prompt]}]
  (when (and slave-id prompt)
    (try
      (let [elisp (format "(hive-mcp-swarm-send-to-terminal \"%s\" \"%s\")"
                          (v/escape-elisp-string slave-id)
                          (v/escape-elisp-string prompt))
            {:keys [success error]} (ec/eval-elisp elisp)]
        (if success
          (log/info "[EVENT] Sent prompt to ling:" slave-id)
          (log/warn "[EVENT] Failed to send prompt to" slave-id ":" error)))
      (catch Exception e
        (log/error "[EVENT] Swarm send-prompt error:" (.getMessage e))))))

(ev/reg-fx :swarm-send-prompt handle-swarm-send-prompt)
```

### Phase 4: Registration (handlers.clj)
```clojure
;; Add to handlers namespace
(:require [hive-mcp.events.handlers.agora :as agora])

;; In register-handlers!
(agora/register-handlers!)
```

## Files to Modify

1. `src/hive_mcp/agora/dialogue.clj` - Emit `:agora/turn-dispatched` event
2. `src/hive_mcp/events/handlers/agora.clj` - NEW: Handler module
3. `src/hive_mcp/events/effects.clj` - Add `:swarm-send-prompt` effect
4. `src/hive_mcp/events/handlers.clj` - Register agora handlers

## Alternatives Considered

### 1. Polling-Based Relay
Coordinator polls dialogue state and manually prompts next ling.
**Rejected**: Violates event-driven architecture, adds latency.

### 2. Ling Self-Discovery
Lings query dialogue state on each turn to discover they're in a dialogue.
**Rejected**: Too much coupling, lings would need dialogue awareness built-in.

### 3. Full Message Transform (chosen)
Inject Agora context at dispatch time.
**Chosen**: Clean, stateless, each ling receives full context.

## Consequences

### Positive
- Autonomous dialogue flow (no coordinator intervention)
- Event-driven, follows existing patterns
- Self-documenting prompts teach lings the protocol
- Graceful degradation (dead ling = dialogue continues with others)

### Negative
- Increased prompt size (~300 chars overhead per turn)
- Lings must parse and follow Agora instructions
- Max turns limit is somewhat arbitrary

## References
- [Arch>Bh]p: Behavioral Architecture principle
- existing: handlers/ling.clj (event handler pattern)
- existing: tools/swarm/dispatch.clj (shout reminder injection pattern)
- existing: agora/dialogue.clj (dialogue dispatch flow)
