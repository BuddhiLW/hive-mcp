# Forge task execution and catchup personas

Each plan step can carry `:execution`. `plan-to-kanban` preserves it in the
kanban card's `:context`, alongside `:plan-step-id` and `:files`. Step tags
remain card tags, including the computed wave tag.

```clojure
{:id "inspect" :title "Inspect the session control boundary"
 :files ["src/session_control.clj"]
 :tags ["saa" "session-control"]
 :execution {:presets ["ling" "mcp-first" "saa"]
             :persona {:priority-tags ["session-control"]
                       :caps {:decisions 30 :conventions 10 :snippets 0}}}}

{:id "implement" :title "Implement the inspected boundary"
 :depends-on ["inspect"]
 :execution {:presets ["ling" "mcp-first" "saa"]
             :persona {:priority-tags ["session-control"]
                       :caps {:decisions 5 :conventions 25 :snippets 20}}}}
```

Optional `:spawn-mode` selects a registered terminal/headless strategy. It must
name an available backend. An explicit provider incompatible with a backend's
declared providers fails before spawn (for example OpenAI on Claude-only).
`:headless` resolves the configured default and is not a provider selector.
The `:hive-agent` mode carries provider/model through its in-process API router;
it is distinct from native Codex/Claude subscription CLI runtimes. Native
subscription run-control ports are not swarm spawn modes.

Optional `:provider` and `:model` are strings passed to the existing provider
registry. Use configured provider/model IDs; omitted fields use strike defaults
and existing provider resolution. No new provider or model is selected by these
examples. Per-step presets replace the default preset vector.

Mixed mode sends cards with explicit execution settings to lings. Explicit
drone and orchestrator modes reject these settings instead of ignoring them.
Per-task settings do not yet route drone workers or orchestrator subagents.

`:persona` is a catchup lens map, not a named profile. Before starting the CLI,
forge registers it under the generated agent ID through hive-agent's
`:agent/register-persona-lens` extension. Catchup resolves that exact caller ID
through `:catchup/persona-lens` and `:catchup/bundle-profile`. A missing persona
registration extension fails the spawn explicitly.

Caps select memory buckets before content hydration. Supported adjustable
buckets are `:principles`, `:priority-principles`, `:priority-conventions`,
`:sessions`, `:recent-wraps`, `:decisions`, `:conventions`, `:snippets`, and
`:expiring`. Zero omits a bucket; valid counts are integers from 0 to 1000.
Omitted or invalid counts retain the normal limit. Axiom and nomination limits
remain unchanged even when a persona requests zero. The knowledge addon also
uses `:atlas` for its separate atlas lane.

Personalized bundles bypass the shared project bundle cache, preventing one
caller's selection from changing another caller's memories. Persona registrations
are process-local. Native run continuations restore them from completion
checkpoints as described below; ordinary existing sessions still need their
registration restored after a coordinator restart. Presets and configured
provider backends retain their existing availability requirements.

## Native continuation

Native subscription submission accepts an optional `:run/opts` containing only
`:persona`. The runtime requires a checkpoint port when a persona is supplied:

```clojure
{:run/adapter :codex
 :run/agent-id "exploration-session"
 :run/task "Inspect the session boundary."
 :run/opts {:persona {:priority-tags ["session-control"]
                     :caps {:decisions 30 :snippets 0}}}}
```

Completion persists the normalized persona beside the native session checkpoint
before terminal publication. A continuation with a fresh `:run/agent-id`
inherits that persona unless it explicitly supplies another. Restoring a
checkpoint through a new runtime control works after the in-memory registry is
lost. Registration happens before worker execution under both the transcript
agent ID and the native driver run UUID.

The host-side restore path has real-file checkpoint tests. Actual native CLI MCP
caller-ID alignment remains to be verified; no live native catchup proof is
implied by those tests.
