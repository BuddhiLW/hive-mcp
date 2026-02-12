"""Tests for the PostToolUse auto-observation hook.

Tests the full hook pipeline: event → scoring → memory storage → context return.
Uses dry_run mode to avoid requiring a running nREPL server.
"""

# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later

import asyncio
import time

import pytest

from hive_hooks.auto_observation import (
    AutoObservationConfig,
    ObservationRecord,
    _build_memory_content,
    _summarize_input,
    _summarize_response,
    clear_observation_log,
    create_auto_observation_hook,
    get_observation_log,
)
from hive_hooks.significance import DEFAULT_SCORER, SignificanceScorer


@pytest.fixture(autouse=True)
def clean_observation_log():
    """Clear observation log before each test."""
    clear_observation_log()
    yield
    clear_observation_log()


class TestSummarizeInput:
    """Test input summarization for various tool types."""

    def test_read_shows_path(self):
        summary = _summarize_input("Read", {"file_path": "/src/foo.clj"})
        assert "/src/foo.clj" in summary

    def test_grep_shows_pattern_and_path(self):
        summary = _summarize_input("Grep", {"path": "/src", "pattern": "defn.*foo"})
        assert "/src" in summary
        assert "defn.*foo" in summary

    def test_write_shows_file_path(self):
        summary = _summarize_input("Write", {"file_path": "/src/new.clj", "content": "big content"})
        assert "file=/src/new.clj" in summary

    def test_bash_shows_command(self):
        summary = _summarize_input("Bash", {"command": "git status"})
        assert "git status" in summary

    def test_mcp_shows_command(self):
        summary = _summarize_input("mcp__hive__memory", {"command": "add", "content": "x"})
        assert "command=add" in summary

    def test_generic_truncation(self):
        summary = _summarize_input("Unknown", {"data": "x" * 500})
        assert len(summary) <= 200

    def test_long_bash_command_truncated(self):
        long_cmd = "find / -name " + "x" * 200
        summary = _summarize_input("Bash", {"command": long_cmd})
        assert len(summary) <= 150


class TestSummarizeResponse:
    """Test response summarization."""

    def test_none_response(self):
        assert _summarize_response(None) == "(no output)"

    def test_short_response_kept(self):
        assert _summarize_response("OK") == "OK"

    def test_long_response_truncated(self):
        long_resp = "x" * 5000
        summary = _summarize_response(long_resp)
        assert "truncated" in summary
        assert len(summary) < len(long_resp)

    def test_dict_response_stringified(self):
        resp = {"success": True, "id": "123"}
        summary = _summarize_response(resp)
        assert "success" in summary


class TestBuildMemoryContent:
    """Test memory content generation."""

    def test_contains_tool_name(self):
        significance = DEFAULT_SCORER.score("Write", {"file_path": "x.clj"}, "ok")
        content = _build_memory_content("Write", {"file_path": "x.clj"}, "ok", significance)
        assert "Write" in content

    def test_contains_score(self):
        significance = DEFAULT_SCORER.score("Edit", {"file_path": "x.clj"}, "fixed bug")
        content = _build_memory_content("Edit", {"file_path": "x.clj"}, "fixed bug", significance)
        assert str(round(significance.score, 1)) in content

    def test_contains_tags(self):
        significance = DEFAULT_SCORER.score("Edit", {"file_path": "x.clj"}, "fixed bug")
        content = _build_memory_content("Edit", {"file_path": "x.clj"}, "fixed bug", significance)
        for tag in significance.tags[:3]:
            assert tag in content

    def test_markdown_structure(self):
        significance = DEFAULT_SCORER.score("Bash", {"command": "echo hi"}, "hi")
        content = _build_memory_content("Bash", {"command": "echo hi"}, "hi", significance)
        assert "## Auto-Observation" in content
        assert "### Input" in content
        assert "### Output" in content


class TestAutoObservationHook:
    """Test the full hook callback."""

    @pytest.mark.asyncio
    async def test_significant_event_returns_context(self):
        """Significant events should return additionalContext."""
        config = AutoObservationConfig(
            dry_run=True,
            enabled=True,
            batch_interval_s=0,  # No debounce for tests
        )
        hook = create_auto_observation_hook(config)

        # Write is significant (score ~4.0+)
        result = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "/src/new_module.py", "content": "code"},
                "tool_response": "File written successfully",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "tool-use-123",
            {"signal": None},
        )

        assert "hookSpecificOutput" in result
        assert result["hookSpecificOutput"]["hookEventName"] == "PostToolUse"
        assert "additionalContext" in result["hookSpecificOutput"]
        assert "Auto-observation" in result["hookSpecificOutput"]["additionalContext"]

    @pytest.mark.asyncio
    async def test_insignificant_event_returns_empty(self):
        """Low-significance events should return empty dict."""
        config = AutoObservationConfig(
            dry_run=True,
            enabled=True,
            threshold=5.0,  # High threshold
        )
        hook = create_auto_observation_hook(config)

        # Read is low significance
        result = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Read",
                "tool_input": {"file_path": "/src/existing.clj"},
                "tool_response": "file contents",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "tool-use-456",
            {"signal": None},
        )

        assert result == {}

    @pytest.mark.asyncio
    async def test_disabled_hook_returns_empty(self):
        """Disabled hook should always return empty."""
        config = AutoObservationConfig(enabled=False)
        hook = create_auto_observation_hook(config)

        result = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "/critical.py"},
                "tool_response": "done",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "tool-use-789",
            {"signal": None},
        )

        assert result == {}

    @pytest.mark.asyncio
    async def test_observation_log_populated(self):
        """Observations should be recorded in the log."""
        config = AutoObservationConfig(
            dry_run=True,
            store_locally=True,
            batch_interval_s=0,
        )
        hook = create_auto_observation_hook(config)

        await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Edit",
                "tool_input": {"file_path": "/src/x.clj"},
                "tool_response": "edited",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "tool-use-100",
            {"signal": None},
        )

        log = get_observation_log()
        assert len(log) >= 1
        assert log[0].tool_name == "Edit"
        assert log[0].significance.score > 0

    @pytest.mark.asyncio
    async def test_debounce_prevents_rapid_storage(self):
        """Rapid consecutive calls should be debounced."""
        config = AutoObservationConfig(
            dry_run=True,
            batch_interval_s=10.0,  # 10 second debounce
        )
        hook = create_auto_observation_hook(config)

        # First call — should pass through
        result1 = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "/a.py"},
                "tool_response": "ok",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "id-1",
            {"signal": None},
        )

        # Second call immediately after — should be debounced
        result2 = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "/b.py"},
                "tool_response": "ok",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "id-2",
            {"signal": None},
        )

        # First should have context, second should be empty (debounced)
        assert "hookSpecificOutput" in result1
        assert result2 == {}

    @pytest.mark.asyncio
    async def test_observation_record_to_dict(self):
        """ObservationRecord.to_dict should serialize properly."""
        from hive_hooks.significance import SignificanceResult

        record = ObservationRecord(
            timestamp=1234567890.0,
            tool_name="Write",
            tool_input_summary="file=/test.py",
            significance=SignificanceResult(
                score=5.0,
                reasons=["tool:Write=3.0", "file-write"],
                category="medium",
                memory_type="note",
                tags=["file-mutation", "write"],
            ),
            memory_id="abc-123",
        )

        d = record.to_dict()
        assert d["tool_name"] == "Write"
        assert d["score"] == 5.0
        assert d["category"] == "medium"
        assert d["memory_id"] == "abc-123"
        assert "file-mutation" in d["tags"]

    @pytest.mark.asyncio
    async def test_custom_scorer_used(self):
        """Hook should use the provided custom scorer."""
        custom_scorer = SignificanceScorer(
            threshold=1.0,
            extra_patterns=[
                (r"MAGIC_PATTERN", 5.0, "magic"),
            ],
        )
        config = AutoObservationConfig(
            dry_run=True,
            scorer=custom_scorer,
            batch_interval_s=0,
        )
        hook = create_auto_observation_hook(config)

        result = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Read",
                "tool_input": {"file_path": "/x.txt"},
                "tool_response": "MAGIC_PATTERN found here",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "id-99",
            {"signal": None},
        )

        log = get_observation_log()
        assert len(log) >= 1
        assert "magic" in log[0].significance.tags

    @pytest.mark.asyncio
    async def test_dry_run_returns_id(self):
        """In dry_run mode, memory_id should be 'dry-run-id'."""
        config = AutoObservationConfig(
            dry_run=True,
            batch_interval_s=0,
        )
        hook = create_auto_observation_hook(config)

        await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "/critical.py"},
                "tool_response": "Written. Fixed the error.",
                "session_id": "test",
                "transcript_path": "/tmp/test",
                "cwd": "/tmp",
            },
            "id-dry",
            {"signal": None},
        )

        log = get_observation_log()
        significant = [r for r in log if r.memory_id is not None]
        if significant:
            assert significant[0].memory_id == "dry-run-id"


class TestIntegrationWithSDKTypes:
    """Test that hook output conforms to Claude Agent SDK type expectations."""

    @pytest.mark.asyncio
    async def test_output_matches_post_tool_use_spec(self):
        """Output should conform to PostToolUseHookSpecificOutput."""
        config = AutoObservationConfig(dry_run=True, batch_interval_s=0)
        hook = create_auto_observation_hook(config)

        result = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Write",
                "tool_input": {"file_path": "/important.py", "content": "x"},
                "tool_response": "File written",
                "session_id": "sess-1",
                "transcript_path": "/tmp/t",
                "cwd": "/tmp",
                "tool_use_id": "tu-1",
            },
            "tu-1",
            {"signal": None},
        )

        if "hookSpecificOutput" in result:
            output = result["hookSpecificOutput"]
            assert output["hookEventName"] == "PostToolUse"
            assert isinstance(output.get("additionalContext", ""), str)
            # Should not have fields from other hook types
            assert "permissionDecision" not in output
            assert "updatedInput" not in output

    @pytest.mark.asyncio
    async def test_empty_output_is_valid(self):
        """Empty dict is a valid hook output (no-op)."""
        config = AutoObservationConfig(
            dry_run=True,
            threshold=99.0,  # Nothing will be significant
        )
        hook = create_auto_observation_hook(config)

        result = await hook(
            {
                "hook_event_name": "PostToolUse",
                "tool_name": "Read",
                "tool_input": {"file_path": "/x"},
                "tool_response": "content",
                "session_id": "s",
                "transcript_path": "/t",
                "cwd": "/c",
            },
            None,
            {"signal": None},
        )

        assert result == {}
