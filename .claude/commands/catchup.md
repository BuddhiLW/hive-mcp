# Catch Up (Memory-Integrated)

Restore context from project memory and get up to speed at the start of a new session.

## Instructions

### 1. Load Context from Memory

**a) Query recent session notes:**
```
mcp__emacs-mcp__mcp_memory_query
  type: "note"
  tags: ["session-summary"]
  limit: 3
```

**b) Query active decisions:**
```
mcp__emacs-mcp__mcp_memory_query
  type: "decision"
  limit: 10
```

**c) Query code conventions:**
```
mcp__emacs-mcp__mcp_memory_query
  type: "convention"
  limit: 10
```

**d) Query useful snippets (if needed):**
```
mcp__emacs-mcp__mcp_memory_query_metadata
  type: "snippet"
  limit: 5
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

**a) Load In-Memory Kanban first:**
```
mcp__emacs-mcp__mcp_mem_kanban_stats
mcp__emacs-mcp__mcp_mem_kanban_list status:"doing"
```

Show DOING tasks as "In Progress" in the summary.

Check for stale TODO tasks (> 5 days old):
- Suggest promoting priority if still relevant
- Suggest moving to DOING if ready to start
- Suggest deleting if no longer relevant

**b) Dynamic Kanban:**
```
mcp__dynamic-kanban__kanban_status
mcp__dynamic-kanban__kanban_get_ready_tasks
```

**c) Vibe Kanban (if applicable):**
```
mcp__vibe_kanban__list_tasks
```

### 4. Check Git State

**Use magit MCP tools:**
```
mcp__emacs-mcp__magit_status         # Full repo status
mcp__emacs-mcp__magit_branches       # Branch info
mcp__emacs-mcp__magit_log            # Recent commits (default: 10)
```

### 5. Check Memory Health

**a) Check for entries expiring soon:**
```
mcp__emacs-mcp__mcp_memory_expiring_soon
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

Use `mcp_memory_promote` or `mcp_memory_demote` to adjust entry durations based on ongoing relevance.
