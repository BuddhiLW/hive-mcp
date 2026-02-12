"""Hive hooks for Claude Agent SDK integration.

Provides PostToolUse hooks for auto-observation, significance scoring,
and automatic memory crystallization — zero extra LLM cost.

Usage:
    from hive_hooks import create_auto_observation_hook, AutoObservationConfig

    hook = create_auto_observation_hook(config=AutoObservationConfig(
        nrepl_host="localhost",
        nrepl_port=7910,
        project_dir="/path/to/project",
        agent_id="my-agent-id",
    ))

    options = ClaudeAgentOptions(
        hooks={
            "PostToolUse": [HookMatcher(hooks=[hook])],
        }
    )
"""

# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later

from .auto_observation import (
    AutoObservationConfig,
    ObservationRecord,
    create_auto_observation_hook,
    get_observation_log,
    clear_observation_log,
)
from .significance import (
    SignificanceScorer,
    SignificanceResult,
    DEFAULT_SCORER,
)

__all__ = [
    "AutoObservationConfig",
    "ObservationRecord",
    "create_auto_observation_hook",
    "get_observation_log",
    "clear_observation_log",
    "SignificanceScorer",
    "SignificanceResult",
    "DEFAULT_SCORER",
]

__version__ = "0.1.0"
