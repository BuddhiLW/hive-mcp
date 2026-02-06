# End-of-Session Wrap-up

Unified session wrap-up that works for both lings and coordinators with proper project scoping and attribution.

## Step 1: Get Your Context

**Get your working directory** (for project scoping):
```bash
pwd
```

**Get your agent ID** (for attribution):
```bash
echo $CLAUDE_SWARM_SLAVE_ID
```

- If set (e.g., `swarm-task-name-123456`) → you're a ling
- If empty → you're the coordinator

## Step 2: Call wrap_crystallize

**This works for EVERYONE - lings and coordinators alike:**

```
mcp__hive__wrap_crystallize(
  agent_id: "<your CLAUDE_SWARM_SLAVE_ID or omit if coordinator>",
  directory: "<your pwd result>"
)
```

**Example (ling):**
```
mcp__hive__wrap_crystallize(
  agent_id: "swarm-fix-bug-1768840244",
  directory: "/home/user/projects/my-project"
)
```

**Example (coordinator):**
```
mcp__hive__wrap_crystallize(
  directory: "/home/user/projects/my-project"
)
```

## What wrap_crystallize Does

1. **Harvests session data** - Recent notes, git commits, kanban completions
2. **Tags with project scope** - Uses `.hive-project.edn` or directory hash
3. **Tags with agent attribution** - Your agent_id or "coordinator"
4. **Emits wrap_notify event** - Hivemind can permeate your crystals
5. **Stores session summary** - Available via `/catchup`

## Step 3: Report Completion

After wrap_crystallize returns, you're done. Shout completion if you're a ling:

```
hivemind_shout(event_type: "completed", message: "WRAPPED: <brief summary>")
```

## Why Project Scoping Matters

Multiple hiveminds may work on different projects simultaneously. Without `directory`:
- Your wrap might be tagged with wrong project
- Another hivemind's `/catchup` might load your crystals
- Cross-project memory contamination

The `directory` parameter ensures your session is scoped to YOUR project via `.hive-project.edn`.

## Alternative: session_complete (Full Lifecycle)

If you also want to commit changes and complete kanban tasks:

```
mcp__hive__session_complete(
  commit_msg: "feat: your work summary",
  task_ids: ["kanban-task-1", "kanban-task-2"],
  agent_id: "<your CLAUDE_SWARM_SLAVE_ID or omit>",
  directory: "<your pwd>"
)
```

This does wrap_crystallize + git commit + kanban completion in one call.

---

## Advanced: Manual Wrap (Coordinators Only)

For fine-grained control over what gets stored, coordinators can use manual memory calls instead of wrap_crystallize. **Lings should NOT use this section.**

<details>
<summary>Click to expand manual workflow</summary>

### Document Progress

```
mcp__hive__mcp_memory_add(
  type: "decision",
  content: "Decision: [what and why]",
  tags: ["architecture"],
  directory: "<your pwd>"
)
```

### Sync Kanban

```
mcp__hive__mcp_mem_kanban_stats(directory: "<your pwd>")
mcp__hive__mcp_mem_kanban_list_slim(status: "doing", directory: "<your pwd>")
```

### Git Status

```
mcp__hive__magit_status(directory: "<your pwd>")
mcp__hive__magit_feature_branches(directory: "<your pwd>")
```

### Memory Maintenance

```
mcp__hive__mcp_memory_cleanup_expired()
mcp__hive__mcp_memory_expiring_soon(days: 7)
```

</details>
