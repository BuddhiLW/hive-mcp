---
type: convention
tags: [architecture, invariant, project-scoping, pollination, foundational, scope:global]
duration: permanent
---

# Fundamental Invariant: Project Scoping + Memory Pollination

## The Law

**All hive-mcp subsystems are project-scoped. Cross-project knowledge transfer happens exclusively through memory pollination.**

```
+-------------------------------------------------------------+
|                    INVARIANT STRUCTURE                       |
+-------------------------------------------------------------+
|                                                              |
|   Project A                         Project B                |
|   +---------+                       +---------+             |
|   | Lings   |                       | Lings   |             |
|   | Shouts  |                       | Shouts  |             |
|   | Crystals|                       | Crystals|             |
|   | KG Edges|                       | KG Edges|             |
|   +----+----+                       +----+----+             |
|        |                                 |                   |
|        | promote                         | promote           |
|        v                                 v                   |
|   +---------------------------------------------+           |
|   |              GLOBAL SCOPE                    |           |
|   |         (Memory Pollination Layer)           |           |
|   |                                              |           |
|   |  - Hellenic Patterns                         |           |
|   |  - General Semantics                         |           |
|   |  - Hive Principles                           |           |
|   |  - Promoted learnings from any project       |           |
|   +---------------------------------------------+           |
|                                                              |
+-------------------------------------------------------------+
```

## The Rules

1. **Default: Isolated** - Projects cannot see each other's internal state
2. **Inheritance: Down** - Child scopes see parent knowledge automatically
3. **Promotion: Up** - Knowledge bubbles up only through explicit action
4. **Pollination: Global** - Cross-project sharing requires promotion to global

## Subsystem Compliance

| Subsystem | Scoping Mechanism | Status |
|-----------|-------------------|--------|
| Memory Queries | `scope:project:X` tags + visible-scopes | Done |
| Crystal Permeation | `directory` param -> project-id filter | Done |
| Knowledge Graph | `scope.clj` hierarchy | Done |
| HIVEMIND Shouts | `directory` param -> project-id filter | Done |
| Ling Spawning | `cwd` param sets project context | Done |
| Wrap/Crystallize | `directory` param tags with project-id | Done |

## Why This Matters

- **Isolation**: Project A's internal debates don't leak to Project B
- **Collaboration**: Valuable learnings CAN be shared via explicit promotion
- **Evolution**: Each project develops its own conventions, best ones rise to global
- **Safety**: No accidental cross-contamination

## Pollination Flow

```
Project A discovers pattern
    |
    +-- Coordinator promotes to global
            |
            +-- Global memory now contains pattern
                    |
                    +-- Project B's catchup sees it
                    +-- Project C's catchup sees it
```

## Anti-Patterns

- Reading another project's internal shouts
- Permeating crystals without project filter
- Querying memory without scope awareness
- Global-by-default (should be project-by-default)

## The Invariant Statement

> **Every tool that reads or writes hive-mcp state MUST respect project scoping. Cross-project visibility happens ONLY through the global scope, and entry into global requires explicit promotion.**

This is non-negotiable. All new tools, features, and subsystems must comply.
