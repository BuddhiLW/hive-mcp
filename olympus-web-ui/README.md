# Olympus Web UI

Web-based dashboard for hive-mcp swarm monitoring and visualization.

## Features

- **Agent Graph**: Visualize ling/drone hierarchy in real-time
- **Wave Graph**: Track parallel task execution progress
- **Knowledge Graph**: Explore memory entries and relationships
- **WebSocket**: Live updates from hive-mcp channel (port 9999)

## Quick Start

```bash
# Install dependencies
npm install

# Start development server (http://localhost:3000)
npm run dev

# Or with shadow-cljs directly
npx shadow-cljs watch app
```

## Architecture

```
src/olympus_web/
├── core.cljs          # Entry point
├── config.cljs        # Configuration
├── db.cljs            # Initial app-db state
├── events/            # re-frame event handlers
│   ├── core.cljs      # UI events
│   ├── ws.cljs        # WebSocket events
│   ├── agents.cljs    # Agent events
│   ├── waves.cljs     # Wave events
│   └── kg.cljs        # Knowledge Graph events
├── subs/              # re-frame subscriptions
│   └── core.cljs
├── views/             # Reagent components
│   ├── app.cljs       # Main shell
│   ├── header.cljs
│   ├── sidebar.cljs
│   └── detail.cljs
└── graphs/            # ReactFlow visualizations
    ├── common.cljs    # Shared utilities (dagre layout)
    ├── agent.cljs     # Agent hierarchy graph
    ├── wave.cljs      # Wave/task progress graph
    └── kg.cljs        # Knowledge graph
```

## WebSocket Protocol

Connects to `ws://localhost:9999` (hive-mcp channel).

### Incoming Events

| Event Type | Payload |
|------------|---------|
| `started` | `{:agent-id :task :message}` |
| `progress` | `{:agent-id :message}` |
| `completed` | `{:agent-id :message}` |
| `error` | `{:agent-id :message}` |
| `blocked` | `{:agent-id :message}` |
| `agent-spawned` | `{:agent-id :type :parent-id}` |
| `wave-started` | `{:wave-id :tasks}` |
| `memory-added` | `{:id :type :content :tags}` |

## Color Scheme

### Agents
- Coordinator: `#9333ea` (purple)
- Ling: `#2563eb` (blue)
- Drone: `#16a34a` (green)

### Wave Tasks
- Pending: `#6b7280` (gray)
- Running: `#eab308` (yellow)
- Completed: `#16a34a` (green)
- Failed: `#dc2626` (red)

### Memory Types
- Note: `#06b6d4` (cyan)
- Snippet: `#ec4899` (pink)
- Convention: `#eab308` (yellow)
- Decision: `#9333ea` (purple)
- Axiom: `#dc2626` (red)

## Development

### REPL

```bash
# Start nREPL + shadow-cljs
npm run server

# In another terminal, connect to CLJS REPL
npm run repl
```

### Production Build

```bash
npm run release
```

Output: `resources/public/js/main.js`
