# Swarm Memory Coordination Pattern

**Status:** RFC / Design Proposal
**Author:** Claude (Opus 4.5)
**Date:** 2025-01-03

## Overview

This document describes how to use the `mcp_memory_*` system for **knowledge sharing and coordination** between swarm agents. While `swarm-protocol.md` covers task dispatch and collection, this addresses the **persistent knowledge layer** that enables agents to:

1. Share discoveries without direct communication
2. Avoid duplicate work
3. Build on each other's findings
4. Maintain session continuity across restarts

```
┌─────────────────────────────────────────────────────────────────┐
│                         Shared Memory                            │
│                    (mcp_memory_* system)                        │
│                                                                 │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐           │
│  │ findings │ │  claims  │ │ decisions│ │ context  │           │
│  └────▲─────┘ └────▲─────┘ └────▲─────┘ └────▲─────┘           │
└───────┼────────────┼────────────┼────────────┼──────────────────┘
        │ write      │ write      │ read       │ read/write
        │            │            │            │
   ┌────┴────┐  ┌────┴────┐  ┌────┴────┐  ┌────┴────┐
   │Explorer │  │ Worker  │  │Architect│  │  Any    │
   │  Agent  │  │  Agent  │  │  Agent  │  │  Agent  │
   └─────────┘  └─────────┘  └─────────┘  └─────────┘
```

## Memory Types for Swarm Coordination

### Core Types (existing)

| Type | Swarm Use Case |
|------|----------------|
| `note` | General findings, observations, hypotheses |
| `snippet` | Code patterns, reusable solutions |
| `convention` | Codebase rules agents must follow |
| `decision` | Architectural choices, rationale |

### Recommended Tag Taxonomy

```
# Scope tags - who should read this
scope:global              # All agents, all sessions
scope:session:{id}        # Only agents in this session
scope:project:{name}      # Project-specific knowledge

# Role tags - who produced this
role:explorer             # Discovery/research agent
role:architect            # Planning/design agent
role:implementer          # Coding agent
role:tester               # Test runner agent
role:reviewer             # Code review agent

# Priority tags - urgency of reading
priority:critical         # MUST read before any action
priority:high             # Should read before related work
priority:normal           # Useful context
priority:low              # Background information

# Status tags - lifecycle state
status:active             # Currently relevant
status:superseded         # Replaced by newer entry
status:resolved           # Historical, issue fixed
status:stale              # Needs verification/refresh

# Content tags - what kind of info
content:finding           # Discovery about codebase
content:blocker           # Something blocking progress
content:claim             # "I am working on X"
content:hypothesis        # Unverified theory
content:verified          # Confirmed fact
```

### Duration Strategy

| Duration | TTL | Use Case |
|----------|-----|----------|
| `ephemeral` | 1 hour | Task claims, "working on X" |
| `short-term` | 1 day | Session context, active blockers |
| `medium-term` | 1 week | Recent decisions, fresh findings |
| `long-term` | 3 months | Conventions, architecture patterns |
| `permanent` | Never | Core project knowledge, critical conventions |

## Coordination Patterns

### Pattern 1: Explorer → Hivemind Knowledge Transfer

Explorer agents investigate codebases and write findings for other agents to consume.

```
Explorer Agent Workflow:
1. Receive exploration task
2. BEFORE writing: semantic search for existing findings
   → mcp_memory_search_semantic(query: "auth flow keycloak")
   → mcp_memory_check_duplicate(type: "note", content: "...")
3. Investigate codebase
4. Write findings with structured tags
   → mcp_memory_add(
       type: "note",
       content: "Auth flow: Frontend → Envoy → Keycloak. Tokens stored in httpOnly cookies.",
       tags: ["role:explorer", "scope:project:sisf", "content:finding", "priority:high"]
     )
5. Set appropriate duration
   → mcp_memory_set_duration(id: "...", duration: "long-term")
```

**Example finding note:**
```markdown
## Auth Flow Discovery

**Path:** Frontend → Envoy (port 10000) → Keycloak (port 8080)

**Key observations:**
- Envoy handles OIDC via Lua filter at `/oauth2/`
- Tokens stored as `httpOnly` cookies (access_token, refresh_token)
- Session validation: `/api/auth/user/me` returns 401 if invalid
- Keycloak client: `sisf-web-client` (public client, PKCE enabled)

**Files involved:**
- `k8s-a3f/envoy/envoy.yaml` - Lua filter config
- `auth-service/internal/handler/auth.go` - Token validation
```

### Pattern 2: Task Claiming (Avoid Duplicate Work)

Workers claim tasks before starting to prevent collision.

```
Worker Agent Workflow:
1. Query available work
   → mcp_memory_search_semantic(query: "tasks ready unclaimed", type: "note")
2. Check for existing claims
   → mcp_memory_search_semantic(query: "claim auth-service refactor")
3. If unclaimed, create ephemeral claim
   → mcp_memory_add(
       type: "note",
       content: "CLAIM: Working on auth-service token refresh bug",
       tags: ["content:claim", "role:implementer", "scope:session:abc123", "status:active"]
     )
   → mcp_memory_set_duration(id: "...", duration: "ephemeral")
4. Do work
5. On completion: delete claim, write result as finding
```

**Claim note format:**
```markdown
CLAIM: [task description]
Agent: swarm-worker-1704067890
Started: 2025-01-03T10:30:00Z
Expected completion: ~30min
```

### Pattern 3: Blocker Broadcasting

When an agent hits a blocker, broadcast it so others don't waste time.

```
mcp_memory_add(
  type: "note",
  content: "BLOCKER: Cannot run integration tests - DB migration pending on staging",
  tags: ["content:blocker", "priority:critical", "scope:session:abc123", "status:active"]
)
```

Other agents should query for blockers before starting work:
```
mcp_memory_search_semantic(query: "blocker", limit: 5)
```

### Pattern 4: Decision Recording

Architectural decisions should be recorded with rationale for future agents.

```
mcp_memory_add(
  type: "decision",
  content: """
## Decision: Use JWT over Session Cookies

**Context:** Need auth for new mobile API

**Options considered:**
1. Session cookies (existing) - not suitable for mobile
2. JWT tokens - stateless, mobile-friendly
3. OAuth2 tokens - overkill for internal API

**Decision:** JWT with 15min expiry, refresh via httpOnly cookie

**Rationale:** Mobile apps can't reliably handle cookies. Short JWT expiry limits damage if leaked.

**Consequences:**
- Need token refresh endpoint
- Must validate JWT signature on every request
- Logout requires token blacklist (Redis)
""",
  tags: ["role:architect", "scope:project:sisf", "priority:high", "content:decision"]
)
→ mcp_memory_set_duration(id: "...", duration: "permanent")
```

### Pattern 5: Convention Enforcement

Conventions should be queried before writing code.

```
# Before coding, agent queries conventions:
mcp_memory_query_metadata(type: "convention")
mcp_memory_search_semantic(query: "error handling patterns")

# Convention entry example:
mcp_memory_add(
  type: "convention",
  content: """
## Error Handling Convention

All Go services must:
1. Use `pkg/errors` for wrapping: `errors.Wrap(err, "context")`
2. Return structured errors: `&AppError{Code: "AUTH_001", Message: "..."}`
3. Log at handler level only, not in business logic
4. Never expose internal errors to clients

Example:
```go
if err != nil {
    return nil, errors.Wrap(err, "failed to fetch user")
}
```
""",
  tags: ["scope:project:sisf", "priority:critical", "content:convention"]
)
→ mcp_memory_set_duration(id: "...", duration: "permanent")
```

## Agent Startup Protocol

Every swarm agent should execute this on startup:

```
1. Query critical priorities
   → mcp_memory_search_semantic(query: "priority:critical", limit: 10)

2. Query active blockers
   → mcp_memory_search_semantic(query: "blocker active", limit: 5)

3. Query relevant conventions
   → mcp_memory_query_metadata(type: "convention")

4. Query session context (if resuming)
   → mcp_memory_search_semantic(query: "scope:session:{session_id}")

5. Check for existing claims on assigned task
   → mcp_memory_search_semantic(query: "claim {task_description}")
```

## Session Handoff

When a session ends, the coordinator should:

```
1. Promote valuable findings
   → mcp_memory_promote(id: "...") # ephemeral → short-term → medium-term

2. Expire stale claims
   → mcp_memory_cleanup_expired()

3. Write session summary
   → mcp_memory_add(
       type: "note",
       content: "SESSION SUMMARY: Completed auth refactor, 3 tests still failing...",
       tags: ["content:session-summary", "scope:global"]
     )

4. Log any unresolved blockers for next session
```

## Missing Primitives (Future Work)

The current memory system lacks:

1. **Agent ID tracking** - Who created this entry?
   - Workaround: Include in content or tags

2. **Session ID tracking** - Which work session?
   - Workaround: Use `scope:session:{id}` tag

3. **Conflict resolution** - Two agents claim same task
   - Workaround: Check before claim, use ephemeral duration

4. **Subscriptions** - Alert when new critical finding
   - Workaround: Poll at startup and periodically

5. **Structured queries** - Filter by multiple tags
   - Workaround: Semantic search approximates this

## Integration with Swarm Protocol

This memory layer complements the dispatch/collect protocol:

| Mechanism | Use For |
|-----------|---------|
| `swarm_dispatch` | Direct task assignment |
| `swarm_collect` | Get task results |
| `mcp_memory_*` | Shared knowledge, coordination |

**Recommended flow:**
```
Master:
1. Write task context to memory (findings, conventions)
2. Dispatch task to slave with prompt referencing memory
3. Collect result
4. Promote useful findings from result to memory

Slave:
1. On startup: query memory for context
2. During work: write discoveries to memory
3. On completion: return result via dispatch protocol
```

## Example: Full Exploration Session

```
# Master spawns explorer
swarm_spawn(name: "explorer", role: "researcher")

# Master dispatches with memory context
swarm_dispatch(
  slave_id: "swarm-explorer-123",
  prompt: """
  Investigate the authentication flow in this codebase.

  Before starting:
  - Query memory for existing auth findings
  - Check for relevant conventions

  After investigation:
  - Write findings to memory with tags: role:explorer, content:finding
  - Set duration based on confidence (verified → long-term, hypothesis → short-term)

  Return a summary of what you found and what you wrote to memory.
  """
)

# Explorer does work, writes to memory...

# Master collects and reviews
result = swarm_collect(task_id: "task-explorer-123-001")

# Master promotes valuable findings
mcp_memory_promote(id: "finding-auth-flow-001")
mcp_memory_set_duration(id: "finding-auth-flow-001", duration: "permanent")
```

## Conclusion

The memory system provides a **persistent, searchable knowledge base** that enables:

- **Async coordination** - Agents don't need direct communication
- **Knowledge accumulation** - Findings survive session boundaries
- **Duplicate avoidance** - Check before writing, claim before working
- **Convention enforcement** - Query rules before coding
- **Session continuity** - New agents can bootstrap from existing knowledge

This complements the synchronous dispatch/collect protocol with a shared memory layer, enabling true hivemind behavior where the collective knowledge exceeds any individual agent's context window.
