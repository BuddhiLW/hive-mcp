---
type: decision
tags: [architecture, agora, nash-equilibrium, consensus, swarm-pattern, scope:global]
duration: permanent
---

# Agora Consensus: Nash Equilibrium as Stopping Condition

## Core Principle

A dialogue reaches **consensus** when it achieves **Nash equilibrium** - no participant would unilaterally change their position given the others' positions.

## Detection Algorithm

```clojure
(defn nash-equilibrium?
  "Check if dialogue has reached Nash equilibrium.
   Returns true when all participants have signaled 'no changes'
   in their most recent turn."
  [dialogue-id]
  (let [participants (get-participants dialogue-id)
        last-turns (map #(last-turn-for dialogue-id %) participants)
        signals (map :signal last-turns)]
    (every? #{:no-change :approve :lgtm} signals)))
```

## Turn Signals

Each turn includes a **signal** indicating participant's stance:

| Signal | Meaning | Effect |
|--------|---------|--------|
| `:propose` | "Here's a change" | Resets equilibrium |
| `:counter` | "I disagree, here's alternative" | Resets equilibrium |
| `:no-change` | "I wouldn't change anything" | +1 toward equilibrium |
| `:approve` | "I accept current state" | +1 toward equilibrium |
| `:defer` | "I have no opinion" | Neutral (doesn't block) |

## Equilibrium States

```
2-party (Writer <-> Critic):
  Both signal :no-change -> CONSENSUS

3-party (Author <-> Reviewer1 <-> Reviewer2):
  All three signal :approve -> CONSENSUS

N-party with threshold:
  (count :approve) / (count participants) >= 0.8 -> CONSENSUS
```

## Ling Protocol

Lings must include signal in dispatches:

```
swarm_dispatch(
  slave_id: "critic-123",
  prompt: "[SIGNAL: propose] Iteration 2. Changes: tightened hook. Review."
)

swarm_dispatch(
  slave_id: "writer-456",
  prompt: "[SIGNAL: no-change] LGTM. No further improvements needed."
)
```

The Agora parser extracts `[SIGNAL: X]` and records it.

## Why Nash Equilibrium?

1. **Principled stopping**: Not "3 iterations" or "timeout" - stop when mathematically stable
2. **Self-regulating**: Participants decide when done, not coordinator
3. **Quality signal**: Equilibrium means all perspectives satisfied
4. **Provable**: Can verify consensus was reached legitimately

## Edge Cases

| Scenario | Handling |
|----------|----------|
| Infinite loop (A proposes, B counters, repeat) | Timeout after N turns, escalate to coordinator |
| Participant goes silent | Timeout -> treat as `:defer` or remove |
| Deadlock (mutual :counter) | Recruit mediator participant |
| Rapid false consensus | Require minimum turns before equilibrium valid |

## Integration with Self-Organizing

When dialogue is stuck (no equilibrium after threshold):
```clojure
(when (stuck? dialogue-id)
  (dialogue-recruit dialogue-id
                    :mediator
                    ["ling" "neutral" "clarity"]
                    "Breaking deadlock"))
```

The mediator brings fresh perspective to reach equilibrium.
