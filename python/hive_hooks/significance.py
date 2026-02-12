"""Significance scoring for tool observations.

Algorithmic heuristics that classify tool executions by importance,
replacing the need for an LLM observer. Zero extra cost.

Scoring dimensions:
1. Tool type signal  — Write/Edit >> Read/Grep (mutations matter more)
2. Content signal    — error/fix/decision/pattern keywords boost score
3. Size signal       — large outputs suggest substantial work
4. Novelty signal    — file creation > file modification
5. Negative signal   — status checks, empty reads, help commands score low

Score range: 0.0 (noise) to 10.0 (critical observation).
Threshold default: 3.0 (above = worth recording).
"""

# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any


@dataclass
class SignificanceResult:
    """Result of significance scoring for a tool execution."""

    score: float
    reasons: list[str]
    category: str  # "noise", "low", "medium", "high", "critical"
    memory_type: str | None = None  # Suggested memory type: "note", "snippet", "convention", "decision"
    tags: list[str] = field(default_factory=list)

    @property
    def is_significant(self) -> bool:
        """Whether this observation exceeds the recording threshold."""
        return self.category in ("medium", "high", "critical")


# --- Tool type weights ---
# Mutations and decisions are more valuable than reads
TOOL_WEIGHTS: dict[str, float] = {
    # High-value mutations
    "Write": 3.0,
    "Edit": 2.5,
    "MultiEdit": 2.5,
    "NotebookEdit": 2.5,
    # Medium-value actions
    "Bash": 2.0,
    "Task": 1.5,
    # Low-value reads (but still contextual)
    "Read": 0.5,
    "Glob": 0.3,
    "Grep": 0.5,
    "WebSearch": 1.0,
    "WebFetch": 1.0,
    # MCP tools — treated by prefix
}

MCP_TOOL_WEIGHTS: dict[str, float] = {
    "mcp__hive__memory": 2.0,      # Memory operations are decisions
    "mcp__hive__kanban": 1.5,      # Task management
    "mcp__hive__magit": 2.0,       # Git operations
    "mcp__hive__kg": 2.0,          # KG operations
    "mcp__hive__hivemind": 1.0,    # Coordination
    "mcp__hive__wave": 2.5,        # Drone dispatch
    "mcp__hive__file_write": 3.0,  # File mutation
    "mcp__hive__cider": 1.5,       # REPL eval
    "mcp__hive__session": 2.0,     # Session lifecycle
}

# --- Content signal patterns ---
# Regex patterns that indicate significant content
HIGH_SIGNAL_PATTERNS: list[tuple[str, float, str]] = [
    # (pattern, score_boost, tag)
    (r"(?i)\b(bug|error|exception|fix|fixed|resolve[ds]?)\b", 2.0, "bugfix"),
    (r"(?i)\b(decision|decided|chose|architecture|design)\b", 2.5, "decision"),
    (r"(?i)\b(convention|pattern|standard|rule)\b", 2.0, "convention"),
    (r"(?i)\b(created?|new file|added)\b", 1.5, "creation"),
    (r"(?i)\b(test|spec|assert|expect)\b", 1.5, "testing"),
    (r"(?i)\b(refactor|rename|restructur)\b", 1.5, "refactor"),
    (r"(?i)\b(security|auth\w*|permissions?|credentials?)\b", 2.0, "security"),
    (r"(?i)\b(deprecat|removed?|delet)\b", 1.5, "deprecation"),
    (r"(?i)\b(TODO|FIXME|HACK|XXX)\b", 1.0, "todo"),
    (r"(?i)\b(performance|optimi[sz]|slow|fast)\b", 1.5, "performance"),
]

# Patterns that indicate noise (reduce score)
NOISE_PATTERNS: list[tuple[str, float]] = [
    (r"(?i)^(ls|pwd|echo|cat|head|tail)\s", -1.5),       # Simple shell commands
    (r"(?i)\bgit status\b", -1.0),                         # Status checks
    (r"(?i)\bgit log\b", -0.5),                            # Log viewing
    (r"(?i)--help\b", -1.5),                               # Help lookups
    (r"(?i)^#", -1.0),                                     # Comments
    (r"No matches found", -1.0),                            # Empty searches
]


def _get_tool_weight(tool_name: str) -> float:
    """Get the base weight for a tool by name."""
    # Exact match first
    if tool_name in TOOL_WEIGHTS:
        return TOOL_WEIGHTS[tool_name]

    # MCP tool prefix matching
    for prefix, weight in MCP_TOOL_WEIGHTS.items():
        if tool_name.startswith(prefix):
            return weight

    # Unknown MCP tools get moderate weight
    if tool_name.startswith("mcp__"):
        return 1.0

    # Completely unknown tools
    return 1.0


def _score_content(content: str) -> tuple[float, list[str], list[str]]:
    """Score content by pattern matching.

    Returns: (score_delta, reasons, tags)
    """
    score = 0.0
    reasons: list[str] = []
    tags: list[str] = []

    # Check high-signal patterns
    for pattern, boost, tag in HIGH_SIGNAL_PATTERNS:
        if re.search(pattern, content):
            score += boost
            reasons.append(f"content:{tag}")
            if tag not in tags:
                tags.append(tag)

    # Check noise patterns
    for pattern, penalty in NOISE_PATTERNS:
        if re.search(pattern, content):
            score += penalty  # penalty is negative
            reasons.append("noise-pattern")

    return score, reasons, tags


def _score_size(content: str) -> tuple[float, list[str]]:
    """Score based on content size (proxy for substantial work)."""
    length = len(content)
    reasons: list[str] = []

    if length > 5000:
        reasons.append("large-output")
        return 1.5, reasons
    elif length > 1000:
        reasons.append("medium-output")
        return 0.5, reasons
    elif length < 50:
        reasons.append("tiny-output")
        return -0.5, reasons

    return 0.0, reasons


def _infer_memory_type(tool_name: str, tags: list[str], score: float) -> str | None:
    """Infer the best memory type for this observation."""
    if "decision" in tags:
        return "decision"
    if "convention" in tags or "pattern" in tags:
        return "convention"
    if "bugfix" in tags or "testing" in tags:
        return "note"
    if tool_name in ("Write", "Edit", "MultiEdit") and score >= 5.0:
        return "snippet"
    if score >= 3.0:
        return "note"
    return None


def _categorize(score: float) -> str:
    """Categorize a score into human-readable significance level."""
    if score <= 1.0:
        return "noise"
    elif score <= 2.5:
        return "low"
    elif score <= 5.0:
        return "medium"
    elif score <= 7.5:
        return "high"
    else:
        return "critical"


@dataclass
class SignificanceScorer:
    """Configurable significance scorer for tool observations.

    Attributes:
        threshold: Minimum score to consider an observation significant.
        tool_weights: Override tool weights (merged with defaults).
        extra_patterns: Additional (pattern, boost, tag) tuples.
    """

    threshold: float = 3.0
    tool_weights: dict[str, float] = field(default_factory=dict)
    extra_patterns: list[tuple[str, float, str]] = field(default_factory=list)

    def score(
        self,
        tool_name: str,
        tool_input: dict[str, Any],
        tool_response: Any,
    ) -> SignificanceResult:
        """Score a tool execution for observation significance.

        Args:
            tool_name: Name of the tool that was used.
            tool_input: Input parameters passed to the tool.
            tool_response: Output/response from the tool.

        Returns:
            SignificanceResult with score, category, and tags.
        """
        reasons: list[str] = []
        tags: list[str] = []

        # 1. Tool type signal
        merged_weights = {**TOOL_WEIGHTS, **self.tool_weights}
        tool_weight = self.tool_weights.get(tool_name) or _get_tool_weight(tool_name)
        base_score = tool_weight
        reasons.append(f"tool:{tool_name}={tool_weight:.1f}")

        # 2. Content signal from input
        input_str = str(tool_input)
        input_score, input_reasons, input_tags = _score_content(input_str)
        base_score += input_score
        reasons.extend(input_reasons)
        tags.extend(input_tags)

        # Also check extra patterns against input
        for pattern, boost, tag in self.extra_patterns:
            if re.search(pattern, input_str):
                base_score += boost
                reasons.append(f"extra:{tag}")
                if tag not in tags:
                    tags.append(tag)

        # 3. Content signal from response
        response_str = str(tool_response) if tool_response else ""
        if response_str:
            resp_score, resp_reasons, resp_tags = _score_content(response_str)
            base_score += resp_score
            reasons.extend(resp_reasons)
            tags.extend(t for t in resp_tags if t not in tags)

            # Also check extra patterns against response
            for pattern, boost, tag in self.extra_patterns:
                if re.search(pattern, response_str):
                    base_score += boost
                    reasons.append(f"extra-resp:{tag}")
                    if tag not in tags:
                        tags.append(tag)

        # 4. Size signal
        size_score, size_reasons = _score_size(response_str)
        base_score += size_score
        reasons.extend(size_reasons)

        # 5. Special cases
        # File creation is always significant
        if tool_name == "Write" and tool_input.get("file_path"):
            base_score += 1.0
            reasons.append("file-write")
            tags.append("file-mutation")

        # Git commits are always significant
        if tool_name in ("mcp__hive__magit", "Bash"):
            cmd = tool_input.get("command", "") or str(tool_input)
            if "commit" in cmd.lower():
                base_score += 2.0
                reasons.append("git-commit")
                tags.append("git")

        # Clamp to [0, 10]
        final_score = max(0.0, min(10.0, base_score))
        category = _categorize(final_score)
        memory_type = _infer_memory_type(tool_name, tags, final_score)

        # Add tool name as tag
        tool_tag = tool_name.lower().replace("mcp__hive__", "hive:")
        if tool_tag not in tags:
            tags.append(tool_tag)

        return SignificanceResult(
            score=final_score,
            reasons=reasons,
            category=category,
            memory_type=memory_type,
            tags=tags,
        )


# Singleton default scorer
DEFAULT_SCORER = SignificanceScorer()
