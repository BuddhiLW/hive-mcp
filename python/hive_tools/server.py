"""SDK MCP server factory for hive-tools.

Creates a ready-to-use MCP server with all hive coordination tools registered.
This is the primary integration point for headless_sdk.clj.

Usage:
    >>> from hive_tools.server import create_hive_server, get_allowed_tools
    >>>
    >>> server = create_hive_server()
    >>> options = ClaudeAgentOptions(
    ...     mcp_servers={"hive": server},
    ...     allowed_tools=get_allowed_tools("hive"),
    ... )
"""

from __future__ import annotations

from claude_agent_sdk import create_sdk_mcp_server
from claude_agent_sdk.types import McpSdkServerConfig

from .coordination import all_tools, hivemind_tools, kanban_tools, session_tools


def create_hive_server(
    name: str = "hive",
    version: str = "0.12.0",
    include_hivemind: bool = True,
    include_kanban: bool = True,
    include_session: bool = True,
) -> McpSdkServerConfig:
    """Create an MCP server with hive coordination tools.

    Args:
        name: Server name (affects tool prefixes: mcp__{name}__{tool})
        version: Server version string
        include_hivemind: Include hivemind_shout, hivemind_ask
        include_kanban: Include kanban_list, kanban_update, kanban_status
        include_session: Include session_wrap, session_catchup, session_complete

    Returns:
        McpSdkServerConfig ready for ClaudeAgentOptions.mcp_servers
    """
    tools = []
    if include_hivemind:
        tools.extend(hivemind_tools)
    if include_kanban:
        tools.extend(kanban_tools)
    if include_session:
        tools.extend(session_tools)

    return create_sdk_mcp_server(name=name, version=version, tools=tools)


def get_allowed_tools(server_name: str = "hive") -> list[str]:
    """Get the list of allowed_tools strings for all hive tools.

    Args:
        server_name: The server name used in create_hive_server()

    Returns:
        List of tool permission strings like ["mcp__hive__hivemind_shout", ...]
    """
    tool_names = [t.name for t in all_tools]
    return [f"mcp__{server_name}__{name}" for name in tool_names]


def get_tool_names() -> list[str]:
    """Get bare tool names (without server prefix).

    Returns:
        List like ["hivemind_shout", "hivemind_ask", "kanban_list", ...]
    """
    return [t.name for t in all_tools]
