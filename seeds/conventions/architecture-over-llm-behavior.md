---
type: convention
tags: [principle, llm-reliability, architecture, foundational, scope:global]
duration: permanent
---

# Principle: Architecture > LLM Behavior

## The Rule

**Never rely on LLMs following correct behavior consistently across sessions.**

Build systems that make wrong behavior impossible, not instructions that hope for right behavior.

## Why

- LLMs have no persistent memory across sessions
- Instructions in presets/CLAUDE.md are suggestions, not guarantees
- Each new context window starts fresh
- Compliance varies with model, temperature, context length

## Examples

| Bad (LLM-dependent) | Good (Architecture-enforced) |
|---------------------|------------------------------|
| "Always pass agent_id to wrap" | Auto-inject agent_id from env var |
| "Remember to shout progress" | Require shout for task completion |
| "Use directory param for scoping" | Extract project from connection context |
| "Don't modify files outside scope" | File claims block unauthorized writes |

## Application

When designing hive-mcp features:
1. Assume the LLM will forget instructions
2. Make correct behavior the default/only path
3. Use architecture to enforce invariants
4. Fallback gracefully when LLM doesn't comply
