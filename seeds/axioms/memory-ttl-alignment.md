---
type: axiom
tags:
  - memory-architecture
  - ttl
  - duration
  - catchup-priority
  - foundational
duration: permanent
---

# Axiom: Memory Types and TTL Alignment

## INVIOLABLE RULE

**Memory duration MUST match information volatility.** Ephemeral context gets short TTL. Extracted wisdom gets permanent TTL.

## The Memory Hierarchy

| Type | Purpose | Default TTL | Rationale |
|------|---------|-------------|-----------|
| `axiom` | Inviolable rules | **permanent** | Core principles never expire |
| `decision` | Architectural choices | **long/permanent** | ADRs guide future work |
| `convention` | Patterns, rules | **long/permanent** | Team knowledge persists |
| `snippet` | Reusable code | **medium/long** | Reference material |
| `note` | General observations | **varies** | Context-dependent |
| `session-summary` | What happened when | **short** | Ephemeral context |

## TTL Durations

| Duration | Days | Use For |
|----------|------|---------|
| `ephemeral` | 1 | Debugging notes, temp context |
| `short` | 7 | Session summaries, investigation logs |
| `medium` | 30 | Bug analyses, sprint notes |
| `long` | 90 | Decisions, conventions, snippets |
| `permanent` | ∞ | Axioms, core architecture, lessons learned |

## The Extraction Pattern

```
Session Work → Session Summary (short)
                    ↓
            Extract valuable bits
                    ↓
         Decision/Convention (permanent)
```

**Session summaries are disposable.** The value is extracted into permanent entries. The log itself expires.

## Anti-Patterns

❌ Session summary with `permanent` TTL → Memory bloat
❌ Axiom with `short` TTL → Lost principles
❌ All notes as `medium` → No differentiation
❌ Never setting TTL → Defaults may be wrong

## Why This Matters

1. **Catchup efficiency** - Loading 1000 old session logs wastes tokens
2. **Signal vs noise** - Important decisions surface, noise expires
3. **Memory hygiene** - Auto-cleanup via TTL prevents manual pruning
4. **Dual-layer sync** - Permanent items also go to git seeds
