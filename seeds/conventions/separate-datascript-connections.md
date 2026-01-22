---
type: convention
tags: [datascript, ddd, bounded-context, scope:project:hive-mcp]
duration: permanent
---

# Convention: Separate DataScript Connections for Bounded Contexts

## Pattern

Each DDD bounded context gets its own DataScript connection atom:
- `hive-mcp.swarm.datascript.connection/conn` - Swarm coordination state
- `hive-mcp.knowledge-graph.connection/conn` - Knowledge Graph edges

## Why

1. **Schema evolution** - KG schema can change independently of swarm schema
2. **Reset isolation** - `reset-conn!` for testing doesn't wipe unrelated data
3. **SOLID-D** - Modules depend on abstractions (their own connection), not concretions

## How to Add New Bounded Context

1. Create `<domain>/connection.clj` with private `(defonce conn (atom nil))`
2. Create `<domain>/schema.clj` with domain-specific schema
3. `create-conn` uses domain schema, not merged global schema

## Anti-pattern

Avoid: Single global DataScript with merged schemas. Couples unrelated domains.
