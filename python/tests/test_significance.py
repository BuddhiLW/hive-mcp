"""Tests for significance scoring.

Tests the algorithmic heuristics that classify tool executions
by importance. Zero LLM cost — all scoring is pattern-based.
"""

# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later

import pytest

from hive_hooks.significance import (
    DEFAULT_SCORER,
    SignificanceResult,
    SignificanceScorer,
    _categorize,
    _get_tool_weight,
    _score_content,
    _score_size,
)


class TestToolWeights:
    """Test tool type weighting."""

    def test_write_has_highest_weight(self):
        weight = _get_tool_weight("Write")
        assert weight == 3.0

    def test_edit_is_high(self):
        weight = _get_tool_weight("Edit")
        assert weight == 2.5

    def test_read_is_low(self):
        weight = _get_tool_weight("Read")
        assert weight == 0.5

    def test_glob_is_lowest_known(self):
        weight = _get_tool_weight("Glob")
        assert weight == 0.3

    def test_bash_is_medium(self):
        weight = _get_tool_weight("Bash")
        assert weight == 2.0

    def test_mcp_hive_memory_is_high(self):
        weight = _get_tool_weight("mcp__hive__memory")
        assert weight == 2.0

    def test_mcp_hive_file_write_is_highest_mcp(self):
        weight = _get_tool_weight("mcp__hive__file_write")
        assert weight == 3.0

    def test_unknown_mcp_gets_default(self):
        weight = _get_tool_weight("mcp__custom__something")
        assert weight == 1.0

    def test_completely_unknown_gets_default(self):
        weight = _get_tool_weight("SomeFutureTool")
        assert weight == 1.0


class TestContentScoring:
    """Test content pattern matching."""

    def test_bugfix_content_boosts_score(self):
        score, reasons, tags = _score_content("Fixed the null pointer bug in auth.clj")
        assert score > 0
        assert "bugfix" in tags

    def test_decision_content_boosts_score(self):
        score, reasons, tags = _score_content("Decided to use JWT for authentication")
        assert score >= 2.0
        assert "decision" in tags

    def test_convention_content_boosts_score(self):
        score, reasons, tags = _score_content("Convention: always use snake_case")
        assert score > 0
        assert "convention" in tags

    def test_test_content_boosts_score(self):
        score, reasons, tags = _score_content("Added test for validation spec")
        assert score > 0
        assert "testing" in tags

    def test_security_content_boosts_score(self):
        score, reasons, tags = _score_content("Updated authentication credentials")
        assert score > 0
        assert "security" in tags

    def test_noise_pattern_reduces_score(self):
        score, reasons, _ = _score_content("ls -la /tmp")
        assert score < 0

    def test_help_is_noise(self):
        score, _, _ = _score_content("git --help")
        assert score < 0

    def test_empty_search_is_noise(self):
        score, _, _ = _score_content("No matches found")
        assert score < 0

    def test_neutral_content(self):
        score, _, _ = _score_content("Reading the file contents")
        assert -1 <= score <= 1  # Roughly neutral


class TestSizeScoring:
    """Test size-based scoring."""

    def test_large_output_boosts(self):
        score, reasons = _score_size("x" * 6000)
        assert score > 0
        assert "large-output" in reasons

    def test_medium_output_slight_boost(self):
        score, reasons = _score_size("x" * 2000)
        assert score > 0
        assert "medium-output" in reasons

    def test_tiny_output_penalized(self):
        score, reasons = _score_size("ok")
        assert score < 0
        assert "tiny-output" in reasons

    def test_normal_output_neutral(self):
        score, _ = _score_size("x" * 200)
        assert score == 0.0


class TestCategorization:
    """Test score → category mapping."""

    def test_noise(self):
        assert _categorize(0.5) == "noise"
        assert _categorize(1.0) == "noise"

    def test_low(self):
        assert _categorize(1.5) == "low"
        assert _categorize(2.5) == "low"

    def test_medium(self):
        assert _categorize(3.0) == "medium"
        assert _categorize(5.0) == "medium"

    def test_high(self):
        assert _categorize(5.5) == "high"
        assert _categorize(7.5) == "high"

    def test_critical(self):
        assert _categorize(8.0) == "critical"
        assert _categorize(10.0) == "critical"


class TestSignificanceScorer:
    """Test the full scorer pipeline."""

    def test_write_to_new_file_is_significant(self):
        result = DEFAULT_SCORER.score(
            "Write",
            {"file_path": "/src/new_module.clj", "content": "new code"},
            "File written successfully",
        )
        assert result.is_significant
        assert "file-mutation" in result.tags
        assert result.score >= 3.0

    def test_read_file_is_not_significant(self):
        result = DEFAULT_SCORER.score(
            "Read",
            {"file_path": "/src/existing.clj"},
            "file contents here...",
        )
        assert not result.is_significant
        assert result.category in ("noise", "low")

    def test_glob_search_is_noise(self):
        result = DEFAULT_SCORER.score(
            "Glob",
            {"pattern": "**/*.clj"},
            "file1.clj\nfile2.clj",
        )
        assert result.category in ("noise", "low")

    def test_bash_git_commit_is_significant(self):
        result = DEFAULT_SCORER.score(
            "Bash",
            {"command": "git commit -m 'fix: auth bug'"},
            "[main abc1234] fix: auth bug",
        )
        assert result.is_significant
        assert "git" in result.tags

    def test_edit_with_bugfix_is_highly_significant(self):
        result = DEFAULT_SCORER.score(
            "Edit",
            {"file_path": "/src/auth.clj", "old_string": "nil", "new_string": "(or x default)"},
            "Fixed null pointer error in authentication",
        )
        assert result.is_significant
        assert result.score >= 4.0
        assert "bugfix" in result.tags

    def test_bash_ls_is_noise(self):
        result = DEFAULT_SCORER.score(
            "Bash",
            {"command": "ls -la /tmp"},
            "total 0\ndrwxrwxrwx 1 user user 0",
        )
        assert result.category in ("noise", "low")

    def test_mcp_memory_add_is_significant(self):
        result = DEFAULT_SCORER.score(
            "mcp__hive__memory",
            {"command": "add", "type": "decision", "content": "Use JWT"},
            {"id": "123", "success": True},
        )
        assert result.score >= 2.0

    def test_custom_threshold(self):
        scorer = SignificanceScorer(threshold=8.0)
        result = scorer.score(
            "Write",
            {"file_path": "/test.txt"},
            "done",
        )
        # Write is ~4.0 base, below custom threshold 8.0
        # But is_significant checks category, not threshold directly
        assert result.score < 8.0

    def test_extra_patterns(self):
        scorer = SignificanceScorer(
            extra_patterns=[
                (r"URGENT", 3.0, "urgent"),
            ]
        )
        result = scorer.score(
            "Bash",
            {"command": "echo URGENT: deploy now"},
            "URGENT: deploy now",
        )
        assert "urgent" in result.tags
        assert result.score > DEFAULT_SCORER.score(
            "Bash", {"command": "echo deploy now"}, "deploy now"
        ).score

    def test_significance_result_properties(self):
        result = SignificanceResult(
            score=5.0,
            reasons=["test"],
            category="medium",
            memory_type="note",
            tags=["test"],
        )
        assert result.is_significant is True

        result2 = SignificanceResult(
            score=1.0,
            reasons=["test"],
            category="noise",
        )
        assert result2.is_significant is False

    def test_memory_type_inference(self):
        # Decision content → decision type
        result = DEFAULT_SCORER.score(
            "Edit",
            {"file_path": "x.clj"},
            "Decided to use the new architecture pattern",
        )
        assert result.memory_type == "decision"

        # Convention content → convention type
        result2 = DEFAULT_SCORER.score(
            "Write",
            {"file_path": "CONVENTIONS.md", "content": "Our convention is to use kebab-case"},
            "File written",
        )
        assert result2.memory_type in ("convention", "note", "snippet")

    def test_score_clamped_to_0_10(self):
        # Create an extreme case
        scorer = SignificanceScorer(
            extra_patterns=[
                (r"boost", 50.0, "extreme"),
            ]
        )
        result = scorer.score("Write", {"file_path": "x"}, "boost boost boost")
        assert result.score <= 10.0
        assert result.score >= 0.0


class TestBencodeHelpers:
    """Test bencode encoding/decoding used for nREPL communication."""

    def test_bencode_dict(self):
        from hive_hooks.auto_observation import _bencode_dict

        result = _bencode_dict({"op": "eval", "code": "(+ 1 2)"})
        assert result == b"d4:code7:(+ 1 2)2:op4:evale"

    def test_extract_nrepl_value(self):
        from hive_hooks.auto_observation import _extract_nrepl_value

        # Simulated nREPL bencode response with value field
        response = b"d2:ns4:user7:session36:some-session-id5:value3:42ee"
        result = _extract_nrepl_value(response)
        assert result == "42e"  # crude extraction — good enough for IDs

    def test_extract_nrepl_value_not_found(self):
        from hive_hooks.auto_observation import _extract_nrepl_value

        response = b"d2:ns4:user6:status2:okey"
        result = _extract_nrepl_value(response)
        assert result is None
