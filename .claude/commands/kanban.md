# In-Memory Kanban Management

Efficient task management using memory-based storage. Tasks are stored as short-term memory entries and automatically deleted when marked DONE.

## Quick Reference

| Status | Duration | Deletion |
|--------|----------|----------|
| TODO   | 7 days   | Manual   |
| DOING  | 7 days   | Manual   |
| REVIEW | 7 days   | Manual   |
| DONE   | -        | Auto-deleted |

## Instructions

### Quick Add Task

For fast task creation with defaults (TODO, medium priority):
```
mcp__emacs-mcp__mcp_mem_kanban_quick
  title: "Task description"
```

### Create Task with Options

```
mcp__emacs-mcp__mcp_mem_kanban_create
  title: "Task description"
  priority: "high"     # high | medium | low
  context: "Additional notes"
```

### List Tasks

**All tasks:**
```
mcp__emacs-mcp__mcp_mem_kanban_list
```

**By status:**
```
mcp__emacs-mcp__mcp_mem_kanban_list
  status: "todo"       # todo | doing | review
```

### Move Task

Move task through workflow stages:
```
mcp__emacs-mcp__mcp_mem_kanban_move
  task_id: "memory-entry-id"
  new_status: "doing"  # todo | doing | review | done
```

**Note:** Moving to "done" automatically deletes the task from memory.

### Get Statistics

```
mcp__emacs-mcp__mcp_mem_kanban_stats
```

Returns counts: `{:todo N :doing M :review K}`

### Update Task

```
mcp__emacs-mcp__mcp_mem_kanban_update
  task_id: "memory-entry-id"
  title: "Updated title"
  priority: "high"
  context: "Updated notes"
```

## Workflow Examples

### Start Session
```
mcp__emacs-mcp__mcp_mem_kanban_stats
mcp__emacs-mcp__mcp_mem_kanban_list status:"doing"
```

### Begin Work on Task
```
mcp__emacs-mcp__mcp_mem_kanban_move task_id:"xxx" new_status:"doing"
```

### Complete Task (auto-deletes)
```
mcp__emacs-mcp__mcp_mem_kanban_move task_id:"xxx" new_status:"done"
```

### End Session Review
```
mcp__emacs-mcp__mcp_mem_kanban_stats
mcp__emacs-mcp__mcp_mem_kanban_list status:"doing"
# For each DOING task, decide: complete it, keep as DOING, or move back to TODO
```

## Integration

- Use `/wrap` at end of session to sync kanban state
- Use `/catchup` at start of session to load active tasks
- Tasks persist for 7 days as short-term memory entries
- DONE tasks are immediately deleted (not persisted)

## Memory Tags

Tasks are tagged for efficient querying:
- `kanban` - All kanban tasks
- `todo`, `doing`, or `review` - Status-specific
- `priority-high`, `priority-medium`, `priority-low` - Priority-specific
