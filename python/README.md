# hive-tools

Python `@tool` wrappers for hive-mcp coordination, designed for use with the Claude Agent SDK.

## Overview

This package bridges Claude Agent SDK headless lings to hive-mcp's Clojure coordination layer. Tools are exposed as `@tool`-decorated async functions that can be registered with `create_sdk_mcp_server()`.

## Tools

| Tool | Description |
|------|-------------|
| `hivemind_shout` | Broadcast status/progress to coordinator |
| `hivemind_ask` | Request decision from human coordinator |
| `kanban_list` | List kanban tasks (filtered by status) |
| `kanban_update` | Update task status |
| `kanban_status` | Board overview with counts |
| `session_wrap` | Crystallize session learnings |
| `session_catchup` | Restore context from memory |
| `session_complete` | Full lifecycle endpoint |

## Usage

```python
from claude_agent_sdk import ClaudeAgentOptions, ClaudeSDKClient
from hive_tools.server import create_hive_server, get_allowed_tools

server = create_hive_server()
options = ClaudeAgentOptions(
    mcp_servers={"hive": server},
    allowed_tools=get_allowed_tools("hive"),
)

async with ClaudeSDKClient(options=options) as client:
    await client.query("List my kanban tasks")
```

## Bridge Architecture

Tools call back to Clojure via two strategies:
1. **libpython-clj** (in-process, zero overhead) - when running inside JVM
2. **nREPL socket** (fallback) - when running as external process
