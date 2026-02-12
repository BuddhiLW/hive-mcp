"""Tests for coordination tools.

Tests tool definitions, schema correctness, and handler wiring.
Does NOT require a running nREPL - uses mock bridge.
"""

from __future__ import annotations

import json
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

# Import the tool definitions (SdkMcpTool instances)
from hive_tools.coordination import (
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


class TestToolDefinitions:
    """Verify @tool decorator produced correct SdkMcpTool instances."""

    def test_all_tools_count(self):
        """We should have 8 coordination tools total."""
        assert len(all_tools) == 8

    def test_hivemind_tools_count(self):
        assert len(hivemind_tools) == 2

    def test_kanban_tools_count(self):
        assert len(kanban_tools) == 3

    def test_session_tools_count(self):
        assert len(session_tools) == 3

    def test_tool_names_unique(self):
        """All tool names must be unique."""
        names = [t.name for t in all_tools]
        assert len(names) == len(set(names)), f"Duplicate tool names: {names}"

    @pytest.mark.parametrize(
        "tool_obj,expected_name",
        [
            (hivemind_shout, "hivemind_shout"),
            (hivemind_ask, "hivemind_ask"),
            (kanban_list, "kanban_list"),
            (kanban_update, "kanban_update"),
            (kanban_status, "kanban_status"),
            (session_wrap, "session_wrap"),
            (session_catchup, "session_catchup"),
            (session_complete, "session_complete"),
        ],
    )
    def test_tool_has_correct_name(self, tool_obj, expected_name):
        assert tool_obj.name == expected_name

    def test_all_tools_have_descriptions(self):
        for t in all_tools:
            assert t.description, f"Tool {t.name} missing description"
            assert len(t.description) > 10, f"Tool {t.name} description too short"

    def test_all_tools_have_input_schemas(self):
        for t in all_tools:
            schema = t.input_schema
            assert isinstance(schema, dict), f"Tool {t.name} schema not a dict"
            assert schema.get("type") == "object", f"Tool {t.name} schema type not object"
            assert "properties" in schema, f"Tool {t.name} schema missing properties"

    def test_all_tools_have_async_handlers(self):
        import asyncio

        for t in all_tools:
            assert asyncio.iscoroutinefunction(t.handler), (
                f"Tool {t.name} handler is not async"
            )


class TestToolSchemas:
    """Verify input schemas match Clojure handler expectations."""

    def test_hivemind_shout_required_fields(self):
        schema = hivemind_shout.input_schema
        required = schema.get("required", [])
        assert "agent_id" in required
        assert "event_type" in required

    def test_hivemind_shout_event_type_enum(self):
        schema = hivemind_shout.input_schema
        event_type = schema["properties"]["event_type"]
        assert set(event_type["enum"]) == {
            "started", "progress", "completed", "error", "blocked"
        }

    def test_hivemind_ask_required_fields(self):
        schema = hivemind_ask.input_schema
        required = schema.get("required", [])
        assert "agent_id" in required
        assert "question" in required

    def test_kanban_update_required_fields(self):
        schema = kanban_update.input_schema
        required = schema.get("required", [])
        assert "task_id" in required
        assert "new_status" in required

    def test_kanban_update_status_enum(self):
        schema = kanban_update.input_schema
        new_status = schema["properties"]["new_status"]
        assert set(new_status["enum"]) == {"todo", "inprogress", "inreview", "done"}

    def test_session_complete_required_fields(self):
        schema = session_complete.input_schema
        required = schema.get("required", [])
        assert "agent_id" in required
        assert "directory" in required


class TestToolHandlers:
    """Test tool handler functions with mocked bridge."""

    @pytest.fixture(autouse=True)
    def mock_bridge(self):
        """Mock the bridge.call_handler to avoid nREPL dependency."""
        with patch("hive_tools.coordination.call_handler") as mock:
            mock.return_value = {"success": True}
            self.mock_call = mock
            yield mock

    @pytest.mark.asyncio
    async def test_hivemind_shout_calls_handler(self):
        result = await hivemind_shout.handler({
            "agent_id": "test-ling-1",
            "event_type": "progress",
            "message": "50% done",
        })
        self.mock_call.assert_called_once_with("hivemind", {
            "command": "shout",
            "agent_id": "test-ling-1",
            "event_type": "progress",
            "message": "50% done",
        })
        assert "content" in result
        assert result["content"][0]["type"] == "text"

    @pytest.mark.asyncio
    async def test_hivemind_shout_with_task(self):
        await hivemind_shout.handler({
            "agent_id": "test-ling-1",
            "event_type": "started",
            "task": "Implement feature X",
        })
        call_args = self.mock_call.call_args[0][1]
        assert call_args["task"] == "Implement feature X"
        assert call_args["command"] == "shout"

    @pytest.mark.asyncio
    async def test_hivemind_ask_calls_handler(self):
        self.mock_call.return_value = {"decision": "yes", "ask_id": "ask-123"}
        result = await hivemind_ask.handler({
            "agent_id": "test-ling-1",
            "question": "Delete these files?",
            "options": ["yes", "no", "show list"],
        })
        self.mock_call.assert_called_once_with("hivemind", {
            "command": "ask",
            "agent_id": "test-ling-1",
            "question": "Delete these files?",
            "options": ["yes", "no", "show list"],
        })

    @pytest.mark.asyncio
    async def test_kanban_list_default(self):
        self.mock_call.return_value = [{"id": "task-1", "title": "Fix bug"}]
        result = await kanban_list.handler({})
        self.mock_call.assert_called_once_with("kanban", {"command": "list"})

    @pytest.mark.asyncio
    async def test_kanban_list_with_status_filter(self):
        await kanban_list.handler({"status": "inprogress"})
        self.mock_call.assert_called_once_with("kanban", {
            "command": "list",
            "status": "inprogress",
        })

    @pytest.mark.asyncio
    async def test_kanban_update_calls_handler(self):
        await kanban_update.handler({
            "task_id": "task-123",
            "new_status": "done",
        })
        self.mock_call.assert_called_once_with("kanban", {
            "command": "update",
            "task_id": "task-123",
            "new_status": "done",
        })

    @pytest.mark.asyncio
    async def test_kanban_status_calls_handler(self):
        self.mock_call.return_value = {"todo": 5, "doing": 2, "done": 10}
        await kanban_status.handler({})
        self.mock_call.assert_called_once_with("kanban", {"command": "status"})

    @pytest.mark.asyncio
    async def test_session_wrap_calls_handler(self):
        await session_wrap.handler({
            "agent_id": "test-ling-1",
            "directory": "/home/user/project",
        })
        self.mock_call.assert_called_once_with("session", {
            "command": "wrap",
            "agent_id": "test-ling-1",
            "directory": "/home/user/project",
        })

    @pytest.mark.asyncio
    async def test_session_catchup_calls_workflow(self):
        """Catchup routes to workflow handler, not session."""
        await session_catchup.handler({
            "directory": "/home/user/project",
        })
        self.mock_call.assert_called_once_with("workflow", {
            "command": "catchup",
            "directory": "/home/user/project",
        })

    @pytest.mark.asyncio
    async def test_session_complete_calls_handler(self):
        await session_complete.handler({
            "agent_id": "test-ling-1",
            "directory": "/home/user/project",
            "commit_msg": "feat: added coordination tools",
            "task_ids": ["task-1", "task-2"],
        })
        self.mock_call.assert_called_once_with("session", {
            "command": "complete",
            "agent_id": "test-ling-1",
            "directory": "/home/user/project",
            "commit_msg": "feat: added coordination tools",
            "task_ids": ["task-1", "task-2"],
        })

    @pytest.mark.asyncio
    async def test_error_handling(self):
        """Tools should return is_error response on exceptions."""
        self.mock_call.side_effect = ConnectionError("nREPL unavailable")
        result = await hivemind_shout.handler({
            "agent_id": "test-ling-1",
            "event_type": "progress",
        })
        assert result["is_error"] is True
        assert "nREPL unavailable" in result["content"][0]["text"]


class TestBridge:
    """Test bridge module (without real nREPL)."""

    def test_python_to_clj_literal_string(self):
        from hive_tools.bridge import _python_to_clj_literal

        assert _python_to_clj_literal("hello") == '"hello"'
        assert _python_to_clj_literal('say "hi"') == '"say \\"hi\\""'

    def test_python_to_clj_literal_number(self):
        from hive_tools.bridge import _python_to_clj_literal

        assert _python_to_clj_literal(42) == "42"
        assert _python_to_clj_literal(3.14) == "3.14"

    def test_python_to_clj_literal_bool(self):
        from hive_tools.bridge import _python_to_clj_literal

        assert _python_to_clj_literal(True) == "true"
        assert _python_to_clj_literal(False) == "false"

    def test_python_to_clj_literal_none(self):
        from hive_tools.bridge import _python_to_clj_literal

        assert _python_to_clj_literal(None) == "nil"

    def test_python_to_clj_literal_list(self):
        from hive_tools.bridge import _python_to_clj_literal

        assert _python_to_clj_literal(["a", "b"]) == '["a" "b"]'

    def test_python_to_clj_literal_dict(self):
        from hive_tools.bridge import _python_to_clj_literal

        result = _python_to_clj_literal({"command": "shout"})
        assert '"command"' in result
        assert '"shout"' in result

    def test_bencode_encode_string(self):
        from hive_tools.bridge import _bencode_encode

        assert _bencode_encode("hello") == b"5:hello"

    def test_bencode_encode_int(self):
        from hive_tools.bridge import _bencode_encode

        assert _bencode_encode(42) == b"i42e"

    def test_bencode_encode_dict(self):
        from hive_tools.bridge import _bencode_encode

        result = _bencode_encode({"op": "eval"})
        assert result == b"d2:op4:evale"

    def test_bencode_roundtrip(self):
        from hive_tools.bridge import _bencode_decode, _bencode_encode

        original = {"op": "eval", "code": "(+ 1 2)"}
        encoded = _bencode_encode(original)
        decoded, _ = _bencode_decode(encoded)
        assert decoded == original

    def test_bridge_availability_check(self):
        """is_bridge_available should return a dict with required keys."""
        from hive_tools.bridge import is_bridge_available

        result = is_bridge_available()
        assert "available" in result
        assert "strategy" in result
        assert "details" in result


class TestServerFactory:
    """Test server factory module."""

    def test_get_allowed_tools_default(self):
        from hive_tools.server import get_allowed_tools

        tools = get_allowed_tools()
        assert "mcp__hive__hivemind_shout" in tools
        assert "mcp__hive__kanban_list" in tools
        assert "mcp__hive__session_complete" in tools
        assert len(tools) == 8

    def test_get_allowed_tools_custom_name(self):
        from hive_tools.server import get_allowed_tools

        tools = get_allowed_tools("swarm")
        assert "mcp__swarm__hivemind_shout" in tools

    def test_get_tool_names(self):
        from hive_tools.server import get_tool_names

        names = get_tool_names()
        assert "hivemind_shout" in names
        assert "kanban_list" in names
        assert len(names) == 8
