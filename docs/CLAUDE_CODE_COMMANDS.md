# Claude Code Slash Commands Setup

This guide explains how to set up Claude Code slash commands (`/wrap`, `/catchup`) that integrate with hive-mcp's memory workflows.

## Overview

hive-mcp provides two essential workflows for session continuity:

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/catchup` | Restore context from memory | Start of session |
| `/wrap` | Preserve context to memory | End of session |

These commands invoke hive-mcp MCP tools that handle memory storage, kanban sync, and git status.

---

## CRITICAL: Directory Scoping

**All MCP tools that accept a `directory` parameter MUST receive the caller's working directory** to ensure operations target the correct project, not the MCP server's directory.

### Why This Matters

The MCP server runs in its own working directory (typically hive-mcp's directory). When Claude invokes tools like `magit_status` or `mcp_memory_add` without a `directory` parameter, the operations target the MCP server's project instead of the user's project.

### How to Get Directory

In your command instructions, tell Claude to:
1. Get directory from prompt path (e.g., `~/PP/funeraria/sisf-web$`)
2. Or run `pwd` in bash

### Example Tool Calls

```
# WRONG - uses MCP server's directory
mcp__hive__magit_status

# CORRECT - uses caller's project
mcp__hive__magit_status directory:"/home/user/my-project"
```

---

## Setup Instructions

### Step 1: Create Commands Directory

Claude Code looks for custom commands in these locations (in priority order):
1. `.claude/commands/` in your project root (project-specific)
2. `~/.claude/commands/` (user-global)

```bash
# For project-specific commands
mkdir -p .claude/commands

# For user-global commands
mkdir -p ~/.claude/commands
```

### Step 2: Create `/catchup` Command

Create `catchup.md` with direct MCP tool calls:

```markdown
# Catch Up (Memory-Integrated)

Restore context from project memory at session start.

## IMPORTANT: Directory Scoping

**All MCP tools that accept a `directory` parameter MUST receive your current working directory.**

Get your working directory from your prompt path or run `pwd`.

## Instructions

### 1. Load Context from Memory

**Query recent session notes:**
\`\`\`
mcp__hive__mcp_memory_query
  type: "note"
  tags: ["session-summary"]
  limit: 3
  directory: "/path/to/your/project"
\`\`\`

**Query active decisions:**
\`\`\`
mcp__hive__mcp_memory_query
  type: "decision"
  limit: 10
  directory: "/path/to/your/project"
\`\`\`

### 2. Check Kanban Status

\`\`\`
mcp__hive__mcp_mem_kanban_stats directory:"/path/to/your/project"
mcp__hive__mcp_mem_kanban_list_slim status:"doing" directory:"/path/to/your/project"
\`\`\`

### 3. Check Git State

\`\`\`
mcp__hive__magit_status directory:"/path/to/your/project"
mcp__hive__magit_branches directory:"/path/to/your/project"
mcp__hive__magit_log directory:"/path/to/your/project"
\`\`\`

### 4. Present Summary

Output formatted catchup report and ask: "What would you like to focus on?"
```

### Step 3: Create `/wrap` Command

Create `wrap.md` with direct MCP tool calls:

```markdown
# End-of-Session Wrap-up (Memory-Integrated)

Preserve session context to memory at session end.

## IMPORTANT: Directory Scoping

**All MCP tools that accept a `directory` parameter MUST receive your current working directory.**

## Instructions

### 1. Document Progress to Memory

**Store session accomplishments:**
\`\`\`
mcp__hive__mcp_memory_add
  type: "note"
  content: "Session YYYY-MM-DD: [accomplishments]"
  tags: ["session-log", "progress"]
  directory: "/path/to/your/project"
\`\`\`

**Store important decisions:**
\`\`\`
mcp__hive__mcp_memory_add
  type: "decision"
  content: "Decision: [what and why]"
  tags: ["architecture"]
  directory: "/path/to/your/project"
\`\`\`

### 2. Sync Kanban

\`\`\`
mcp__hive__mcp_mem_kanban_stats directory:"/path/to/your/project"
mcp__hive__mcp_mem_kanban_list_slim status:"doing" directory:"/path/to/your/project"
\`\`\`

### 3. Check Git Status

\`\`\`
mcp__hive__magit_status directory:"/path/to/your/project"
mcp__hive__magit_feature_branches directory:"/path/to/your/project"
\`\`\`

### 4. Create Session Summary

Store comprehensive summary for next `/catchup` to retrieve.
```

---

## Usage

### Starting a Session

```
> /catchup
```

Claude will:
1. Query your project memory for recent sessions, decisions, conventions
2. Check kanban for active tasks
3. Report git status
4. Suggest where to start

### Ending a Session

```
> /wrap
```

Claude will:
1. Summarize what was accomplished
2. Store decisions and conventions to memory
3. Update kanban tasks
4. Create session summary for next `/catchup`

---

## Prerequisites

1. **hive-mcp installed and running** - The MCP server must be registered with Claude Code
2. **Emacs daemon running** - hive-mcp connects via emacsclient
3. **hive-mcp.el loaded** - Memory and workflow functions available in Emacs

Verify setup:
```bash
claude mcp list
# Should show: emacs (or hive-mcp)
```

---

## How It Works

### Memory Flow

```
Session N                           Session N+1
─────────────────────────────────────────────────────
Work happens...                     /catchup
     │                                  │
     ▼                                  ▼
/wrap                              Queries memory
     │                              ├── Session summaries
     ▼                              ├── Decisions
mcp_run_workflow("wrap")           ├── Conventions
     │                              └── Kanban tasks
     ├── Stores accomplishments         │
     ├── Stores decisions               ▼
     ├── Stores conventions        Presents context
     ├── Updates kanban            "What to focus on?"
     └── Creates session summary
```

### Memory Types

| Type | Duration | Purpose |
|------|----------|---------|
| Note | Short (7 days) | Session summaries, temporary info |
| Decision | Long (90 days) | Architectural choices, rationale |
| Convention | Permanent | Patterns, coding standards |
| Snippet | Medium (30 days) | Reusable code patterns |

---

## Customization

### Project-Specific Commands

You can also place commands in your project's `.claude/commands/` directory for project-specific workflows.

### Workflow Arguments

The `wrap` workflow accepts these optional arguments:

| Argument | Type | Description |
|----------|------|-------------|
| `accomplishments` | string[] | What was completed |
| `decisions` | string[] | Key decisions made |
| `conventions` | string[] | Patterns to store permanently |
| `in-progress` | string[] | Tasks still being worked on |
| `next-actions` | string[] | Priority items for next session |
| `completed-tasks` | string[] | Kanban task IDs to mark done |

Claude analyzes the session context and populates these automatically.

---

## Troubleshooting

### "Workflow not found"

Ensure hive-mcp workflows are defined in Emacs:

```elisp
M-x eval-expression RET
(hive-mcp-workflow-list)
;; Should include "wrap" and "catchup"
```

### "Memory empty on catchup"

Check memory directory exists:
```bash
ls ~/.emacs.d/.local/cache/hive-mcp/
```

Verify project scope is correct:
```bash
# Check for .hive-project.edn in project root
cat /path/to/project/.hive-project.edn
```

### "MCP tool not available"

Verify hive-mcp is registered:
```bash
claude mcp list
```

Check Emacs server is running:
```bash
emacsclient -e '(hive-mcp-mode)'
```

---

## Related Documentation

- [Memory System](./PROJECT_SUMMARY.md#memory)
- [Workflows](./addon-api.md#workflows)
- [Swarm Multi-Agent](./SWARM_BACKENDS.md)
