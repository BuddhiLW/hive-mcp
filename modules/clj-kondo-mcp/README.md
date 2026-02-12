# clj-kondo-mcp

Fast Clojure/ClojureScript code analysis MCP server powered by [clj-kondo](https://github.com/clj-kondo/clj-kondo) and [Babashka](https://babashka.org/).

## Features

| Tool | Description |
|------|-------------|
| `analyze` | Full code analysis: var definitions, usages, namespace graph, findings |
| `find_callers` | Find all call sites for a specific var |
| `find_calls` | Find all vars that a function calls (dependencies) |
| `find_var` | Find var definition by name with docstring and arglists |
| `namespace_graph` | Dependency graph of namespaces (nodes + edges) |
| `lint` | Lint code and return findings (errors/warnings) |
| `unused_vars` | Find unused private vars (dead code detection) |

## Quick Start

```bash
# Run the server
bb --config modules/clj-kondo-mcp/bb.edn -m clj-kondo-mcp.server

# Or use the task
cd modules/clj-kondo-mcp && bb server
```

## Usage with Claude Code

Add to `~/.config/claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "clj-kondo": {
      "command": "bb",
      "args": ["--config", "/path/to/modules/clj-kondo-mcp/bb.edn", "-m", "clj-kondo-mcp.server"]
    }
  }
}
```

## Tool Examples

### Analyze a codebase

```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{
  "name":"analyze",
  "arguments":{"path":"src"}
}}
```

Returns: var-definitions count, var-usages count, namespaces, findings summary.

### Find callers of a function

```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{
  "name":"find_callers",
  "arguments":{"path":"src","ns":"my.app.core","var_name":"process-data"}
}}
```

Returns: list of call sites with filename, line, column, caller function.

### Get namespace dependency graph

```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{
  "name":"namespace_graph",
  "arguments":{"path":"src"}
}}
```

Returns: `{nodes: [...], edges: [...]}` suitable for visualization.

### Lint code

```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{
  "name":"lint",
  "arguments":{"path":"src","level":"warning"}
}}
```

Returns: findings with type, message, filename, row, col.

## Requirements

- Babashka 1.3+
- clj-kondo pod (auto-loaded)
