---
type: convention
tags: [hive-architecture, anti-pattern, micromanagement, communication, coordinator-essential, scope:global]
duration: permanent
---

# Anti-Pattern: Micromanaging Lings via Buffer Polling

## The Fatal Mistake

**NEVER poll ling buffers (get_buffer_content) to check what they're doing.**

This is micromanagement. It wastes coordinator tokens, breaks the trust hierarchy, and bypasses the communication architecture.

## Why It's Wrong

1. **Token waste** - Reading vterm buffers consumes expensive coordinator context
2. **Breaks trust** - Lings are trusted agents, not processes to babysit
3. **Bypasses architecture** - The hive has proper communication channels
4. **Doesn't scale** - Can't poll 10 lings simultaneously

## Correct Pattern: Use Hive Communication

```
WRONG:
coordinator -> get_buffer_content(*swarm-ling*) -> parse output -> understand state

RIGHT:
ling -> hivemind_shout(progress/blocked/completed) -> coordinator sees in piggyback
coordinator -> swarm_status() -> structured state from DataScript
coordinator -> hivemind_messages(agent_id) -> recent shouts from specific ling
```

## The Communication Stack

| Channel | Direction | Use Case |
|---------|-----------|----------|
| `hivemind_shout` | ling -> coordinator | Progress, completion, errors, blocks |
| `piggyback` | automatic | Shouts appear in next MCP response |
| `swarm_status` | coordinator -> system | Structured ling state |
| `swarm_collect` | coordinator -> ling | Get task result when done |
| `hivemind_ask` | ling -> human | Decisions requiring approval |

## The Rule

**If you're tempted to read a ling's buffer, you should instead:**
1. Wait for their shout in piggyback
2. Use `swarm_status` for state
3. Use `swarm_collect` for results
4. Trust the ling to communicate
