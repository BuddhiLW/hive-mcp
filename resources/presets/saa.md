# SAA Preset: Silence-Abstract-Act

Use **SAA** to turn grounded exploration into a durable plan and verified implementation.

| Phase | Korzybski term | Action |
|-------|----------------|--------|
| **Silence** | Silence on objective levels | Read the territory, trace behavior, gather evidence |
| **Abstract** | Abstracting | Form concrete steps and dependencies |
| **Act** | Acting | Execute ready work in parallel where possible, then verify results |

> "The map is not the territory." Read first, abstract second, act third.

## The d[l]e Pattern

```text
d[l]e SAA <task-description>

kanban task → explorer → observations → type=plan memory
            → kanban plan-to-kanban → scoped forge strikes → verified completion
```

## Phase 1: Silence

1. Run `project workflow catchup` with the absolute project directory.
2. Search the repository atlas; read relevant memory bodies and KG neighbors.
3. Use Carto for Clojure discovery: `carto auto`, `carto ask`, searches, definitions, callers, and enriched reads.
4. Trace actual behavior and existing solutions before proposing new machinery.
5. Record observations, uncertainty, affected files, and verification evidence.

Explore independent questions in parallel when useful. Keep exploration read-only until findings support implementation.

## Phase 2: Abstract

Discover the installed plan contract. JSON examples below are arguments to Hive's `multi` tool:

```json
{"tool":"kanban","command":"plan-schema","directory":"/absolute/project"}
```

Plans require `:id`, `:title`, and `:steps`. Each step requires its own string `:id` and `:title`. Include concrete descriptions, files, and dependencies. Optional `:decision-id` links the plan to its source decision memory.

- Priority: `:high`, `:medium`, `:low`.
- Estimate: `:small`, `:medium`, `:large`.
- Every dependency names another step's string ID.
- IDs must be unique and dependencies acyclic.
- Overlapping files require an explicit ownership or ordering decision.

### Example Complete Plan

Two independent implementation steps precede an integration check. Paths below are illustrative.

```edn
{:id "plan-preset-loading"
 :title "Add preset summaries and lazy loading"
 :description "Provide compact summaries and verify lazy loading end to end"
 :tags ["presets" "SAA"]
 :steps
 [{:id "step-1"
   :title "Implement compact preset summaries"
   :description "Expose a summary while preserving access to full content"
   :files ["src/example/preset_summary.clj"]
   :depends-on []
   :priority :high
   :estimate :small
   :tags ["preset-summary"]}
  {:id "step-2"
   :title "Implement lazy preset loading"
   :description "Load full content on demand through the existing preset API"
   :files ["src/example/preset_loading.clj"]
   :depends-on []
   :priority :high
   :estimate :medium
   :tags ["preset-loading"]}
  {:id "step-3"
   :title "Verify summary and lazy loading integration"
   :description "Verify summary discovery leads to the requested full content"
   :files ["test/example/preset_loading_test.clj"]
   :depends-on ["step-1" "step-2"]
   :priority :high
   :estimate :small
   :tags ["integration"]}]}
```

### Dependency Waves

`plan-to-kanban` derives zero-based waves from `:depends-on`. The example yields `{"step-1" 0, "step-2" 0, "step-3" 1}` and adds `wave:N` card tags. Dependencies establish ordering; do not substitute hand-written `:wave` fields or `:parallel` flags.

Ready independent steps may run concurrently, subject to slots and file ownership. A successor becomes ready when all predecessors are done; unrelated work at the same depth need not finish first.

## Phase 3: Act

### Store and Convert

Store the full EDN plan in a memory whose **type is `plan`**. Replace the JSON `content` placeholder with the complete EDN map; a fenced `edn` block containing that map is also supported.

```json
{"tool":"memory","command":"add","type":"plan","content":"<complete EDN plan>","tags":["plan","SAA","presets"],"duration":"medium","directory":"/absolute/project","async":false}
```

Use the returned memory entry ID, which differs from the plan's internal `:id`:

```json
{"tool":"kanban","command":"plan-to-kanban","plan_id":"<returned-memory-id>","directory":"/absolute/project"}
```

Retain `task-ids`, `step-mapping`, and `waves`. Conversion creates cards, plan→task `depends-on` edges, and task→predecessor `depends-on` edges. Inspect `kg-degraded?` and `kg-warnings`: incomplete links require repair before a plan-scoped strike. A partial conversion may already have created cards; inspect existing results before retrying.

An optional EDN file can preserve the plan for review. Keep the memory entry as the execution anchor for forge's `plan_id`.

### Execute Scoped Strikes

The coordinator invokes forge with the exact plan memory ID and absolute directory:

```json
{"tool":"workflow","command":"forge strike","plan_id":"<returned-memory-id>","directory":"/absolute/project","max_slots":3,"spawn_mode":"headless"}
```

`headless` selects the installed headless backend. Choose provider/model/preset options supported by the installed spawn and plan contracts. Source changes do not update a running host automatically; verify the installed contract before relying on new fields.

Forge reaps terminal agents, surveys ready plan cards, and dispatches available work. `task_ids` may narrow the plan scope. An explicit empty whitelist selects nothing. Child agents request a coordinator strike through the permitted session/control channel rather than recursively forging more agents.

The strike acknowledgement means queued, not completed. Inspect its result:

```json
{"tool":"workflow","command":"forge status","directory":"/absolute/project"}
```

Observe actual agent/run handles and card states. Once results are verified and predecessor cards become done, invoke another strike with the **same plan ID**. Do not start another strike merely because an observation timed out.

### Interpret Completion Evidence

The state-aware forge implementation reports:

- `ready`: selected cards can execute.
- `blocked`: dependencies remain open, missing, invalid, or unreadable; inspect blockers.
- `no-ready`: other plan cards may be running, in review, or excluded by a filter.
- `complete`: every canonical card in the scoped plan is done.

Done is a stored status: completed cards and KG links remain present. Missing cards and lookup errors do not prove completion. Finish SAA only after the requested behavior and acceptance checks are demonstrated. A queued strike, successful spawn, or empty ready list is insufficient.

## Progress and Memory

Record findings, decisions, blockers, and verification results in scoped memories, linked to the plan through KG edges. Keep card status aligned with observed work. Use installed hivemind/session reporting commands for phase progress, including relevant plan/task/run IDs.

Before implementation, verify:

- The plan is grounded in observed code and runtime behavior.
- IDs and dependencies are valid; parallel tasks have compatible ownership.
- The memory type is `plan`, with `plan`, `SAA`, and topic tags.
- Conversion returned expected cards and complete dependency links.
- Every forge strike is scoped to the intended plan.

Before reporting completion, verify acceptance criteria and capture evidence in memory and kanban.

## Composability

Combine with `explorer` for exploration, `mcp-first` for tools, `hivemind` for coordination, and `tdd` for verification. Persona presets can specialize catchup memories and priorities when the installed integration supports them.
