---
type: convention
tags: [axiom, hive-architecture, micromanagement, trust, polling, piggyback, coordinator-essential, catchup-priority, scope:global]
duration: permanent
---

# AXIOM: No Micromanagement - Trust the Hive

## INVIOLABLE RULE

**NEVER poll ling buffers or repeatedly check status.** Dispatch tasks, then WAIT. Shouts arrive via piggyback automatically.

## What "Trusting the Hive" Means

Trusting = Dispatch task, then STOP. Do something else or wait. Shouts come to you.

**Polling/Micromanaging** = Repeatedly calling status tools to "check on" lings.

## TRUSTING (Correct)

```
1. swarm_dispatch(ling, task)
2. [STOP - wait for user or do other work]
3. [Any future MCP call] → piggyback contains ling shouts automatically
4. See shout → respond if needed
```

The coordinator dispatches and trusts. Shouts arrive in piggyback on the NEXT MCP call, whatever it is. No need to actively check.

## POLLING (Wrong - Axiom Violation)

```
1. swarm_dispatch(ling, task)
2. swarm_status()           ← polling
3. hivemind_messages()      ← polling
4. swarm_status()           ← polling again
5. mcp_memory_query()       ← "checking if they stored anything"
6. swarm_status()           ← still polling
```

This wastes tokens, shows distrust, and violates the architecture.

## Real Example of Violation

**What coordinator did wrong:**
```
dispatch → swarm_status → hivemind_messages (x3) → swarm_status → mcp_memory_query (x2) → swarm_status
```
User correctly called out: "you are polling"

**What coordinator should have done:**
```
dispatch → [wait/do other work] → shouts arrive in piggyback naturally
```

## The Trust Contract

| Coordinator Does | Ling Does |
|------------------|-----------|
| Dispatch task with clear instructions | Shout progress as they work |
| WAIT (no polling) | Shout completion with results |
| Receive shouts via piggyback | Store findings in memory |
| Act on shouts when received | Create kanban tasks |

## When Status Checks Are OK

- **Once** after dispatch to confirm task accepted
- When user asks "what's the status?"
- After receiving completion shout, to see final state

## When Status Checks Are WRONG

- Repeatedly in a loop
- "Just to see what's happening"
- Before enough time has passed for work to complete
- Multiple checks without user interaction between them

## The Axiom

**Dispatch → Trust → Wait → Receive via Piggyback**

If you find yourself calling swarm_status or hivemind_messages more than once without user input between calls, you are micromanaging. STOP.
