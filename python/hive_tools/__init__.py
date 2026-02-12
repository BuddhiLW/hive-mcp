"""hive-tools: Python @tool wrappers for hive-mcp coordination.

This package provides Claude Agent SDK @tool-decorated functions that bridge
to hive-mcp's Clojure consolidated handlers. These tools enable SDK-spawned
headless lings to participate in the hive swarm.

Modules:
    bridge        - nREPL/libpython-clj bridge to Clojure JVM
    coordination  - Hivemind, kanban, and session tools

Quick Start:
    >>> from claude_agent_sdk import create_sdk_mcp_server
    >>> from hive_tools.coordination import all_tools
    >>>
    >>> server = create_sdk_mcp_server(
    ...     name="hive",
    ...     version="0.12.0",
    ...     tools=all_tools,
    ... )

Tool Naming Convention:
    When registered with server name "hive", tools are accessible as:
    - mcp__hive__hivemind_shout
    - mcp__hive__hivemind_ask
    - mcp__hive__kanban_list
    - mcp__hive__kanban_update
    - mcp__hive__kanban_status
    - mcp__hive__session_wrap
    - mcp__hive__session_catchup
    - mcp__hive__session_complete
"""

__version__ = "0.12.0"

# Re-export for convenience
from .bridge import call_handler, is_bridge_available
from .coordination import (
    all_tools,
    hivemind_ask,
    hivemind_shout,
    hivemind_tools,
    kanban_list,
    kanban_status,
    kanban_tools,
    kanban_update,
    session_catchup,
    session_complete,
    session_tools,
    session_wrap,
)

__all__ = [
    # Bridge
    "call_handler",
    "is_bridge_available",
    # Hivemind tools
    "hivemind_shout",
    "hivemind_ask",
    "hivemind_tools",
    # Kanban tools
    "kanban_list",
    "kanban_update",
    "kanban_status",
    "kanban_tools",
    # Session tools
    "session_wrap",
    "session_catchup",
    "session_complete",
    "session_tools",
    # All tools
    "all_tools",
]
