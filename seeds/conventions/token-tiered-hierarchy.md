---
type: convention
tags: [hive-architecture, token-hierarchy, coordinator, ling, drone, scope:global]
duration: permanent
---

# Hive Architecture: Token-Tiered Hierarchy

## The Three Tiers

```
Hivemind (premium Claude) --- Project coordination, architecture decisions
    |
    +-- Lings (Claude instances) --- Task supervision, drone orchestration
            |
            +-- Drones (OpenRouter free-tier) --- File mutations, grunt work
```

## Token Budget Mental Model

```
Coordinator budget: EXPENSIVE (Opus 4.5)
+-- Strategic decisions
+-- Spawning/dispatch
+-- Architecture reviews
+-- Memory curation

Ling budget: MODERATE (Claude instances)
+-- Task planning
+-- Drone orchestration (dispatch_drone_wave, delegate_drone)
+-- Validation/review of drone output
+-- Shouting progress

Drone budget: CHEAP (OpenRouter free-tier)
+-- File mutations (the actual edits)
+-- Repetitive changes
+-- Bulk operations
```

## Default: Drones Do Grunt Work

**Try to use drones for file mutations.** Lings should orchestrate, not implement directly.

```
# Preferred pattern (ling supervises drones)
ling spawned -> reads code -> dispatch_validated_wave(tasks) -> reviews results

# Avoid (ling does implementation directly)
ling spawned -> reads code -> edits files itself
```

Note: Drone tooling still being refined. Fall back to ling implementation if drone friction is high.

## Key Principle

**Coordinator should NEVER:** Read large files, grep codebases, run tests, or do implementation work.

**Lings should prefer:** Delegating file mutations to drones rather than editing directly.

Delegate down the hierarchy to conserve expensive tokens for strategic work.
