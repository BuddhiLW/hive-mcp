# Catch Up (Memory-Integrated)

Restore context from project memory and get up to speed at the start of a new session.

## IMPORTANT: Directory Scoping

**All MCP tools that accept a `directory` parameter MUST receive your current working directory** to ensure operations target YOUR project, not the MCP server's directory.

Get your working directory from:
- Your prompt path (e.g., `~/PP/funeraria/sisf-web$`)
- Run `pwd` in bash

Pass it to: `mcp__hive__memory`, `mcp__hive__kanban`, `mcp__hive__magit` tools.

## Instructions

### 1. Load Context from Memory

**a) Query AXIOMS (INVIOLABLE - load first):**
```
mcp__hive__memory
  command: "query"
  type: "axiom"
  limit: 10
  directory: "/path/to/your/project"
```

**b) Query priority conventions (tagged catchup-priority):**
```
mcp__hive__memory
  command: "query"
  type: "convention"
  tags: ["catchup-priority"]
  limit: 10
  directory: "/path/to/your/project"
```

**c) Query recent session notes:**
```
mcp__hive__memory
  command: "query"
  type: "note"
  tags: ["session-summary"]
  limit: 3
  directory: "/path/to/your/project"
```

**d) Query active decisions:**
```
mcp__hive__memory
  command: "query"
  type: "decision"
  limit: 10
  directory: "/path/to/your/project"
```

**e) Query code conventions (excluding priority ones):**
```
mcp__hive__memory
  command: "query"
  type: "convention"
  limit: 10
  directory: "/path/to/your/project"
```

**f) Query useful snippets (if needed):**
```
mcp__hive__memory
  command: "query"
  type: "snippet"
  limit: 5
  directory: "/path/to/your/project"
```

### 2. Load File Context

**a) Read SESSION_CONTEXT.json if exists:**
```
Read .claude/SESSION_CONTEXT.json
```

**b) Read project CLAUDE.md if exists:**
```
Read CLAUDE.md
```

### 3. Check Kanban Status

**a) Load Kanban status:**
```
mcp__hive__kanban
  command: "status"
  directory: "/path/to/your/project"
```

**b) List in-progress tasks:**
```
mcp__hive__kanban
  command: "list"
  status: "inprogress"
  directory: "/path/to/your/project"
```

Show INPROGRESS tasks as "In Progress" in the summary.

Check for stale TODO tasks (> 5 days old):
- Suggest promoting priority if still relevant
- Suggest moving to INPROGRESS if ready to start
- Suggest deleting if no longer relevant

### 4. Check Git State

**Use magit MCP tools with your working directory:**
```
mcp__hive__magit
  command: "status"
  directory: "/path/to/your/project"
```
```
mcp__hive__magit
  command: "branches"
  directory: "/path/to/your/project"
```
```
mcp__hive__magit
  command: "log"
  directory: "/path/to/your/project"
```

### 5. Check Memory Health

**a) Check for entries expiring soon:**
```
mcp__hive__memory
  command: "expiring"
  days: 7
```

**b) If there are expiring entries, ask user if they should be:**
- Promoted to longer duration (important info to keep)
- Left to expire (no longer relevant)
- Explicitly demoted (outdated info)

### 6. Present Summary

Output:
```markdown
## Session Catch-Up

## AXIOMS (Inviolable - Follow Word-for-Word)

These rules MUST be followed exactly as written. No exceptions.

1. **[Axiom Title]**: [Full axiom content loaded from memory]
   - Source: [memory ID]

## Priority Conventions

Conventions tagged with 'catchup-priority' - follow these closely.

1. **[Convention]**: [Full content]

### Current State
- Active branch: `branch-name`
- Uncommitted changes: yes/no
- Last commit: `abc1234 - message`

### Memory Context
**Recent Sessions:**
- [Date]: [Summary from session notes]

**Active Decisions:**
- [Decision 1]
- [Decision 2]

**Conventions:**
- [Convention 1]
- [Convention 2]

### In-Progress Tasks (Kanban)
- Task 1: status, notes
- Task 2: status, notes

### Memory Health
- Entries expiring in 7 days: N
- [List if any]

### Recommended Starting Point
Based on kanban priorities, memory context, and git state:
1. First priority
2. Second priority

### Quick Commands
- `/ship` - Merge feature branches to staging
- `/wrap` - End-of-session documentation
```

### 7. Ask for Direction

After presenting the summary, ask:
"What would you like to focus on this session?"

## Memory Duration Reference

When reviewing memory entries, understand the duration hierarchy:
- **session**: Temporary context, expires same day
- **short-term**: 7 days, good for active work context
- **long-term**: 90 days, for project-wide knowledge
- **permanent**: Never expires, for critical info

Use `mcp__hive__memory(command: "promote", id: "...")` or `mcp__hive__memory(command: "demote", id: "...")` to adjust entry durations based on ongoing relevance.
