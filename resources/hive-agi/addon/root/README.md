# {{raw-name}}

{{description}}

A [hive-mcp](https://github.com/hive-agi/hive-mcp) addon implementing the IAddon protocol.

## Quick Start

### 1. Configure dependency

Update `deps.edn` to point to your hive-mcp installation:

```clojure
;; For local development (recommended):
io.github.hive-agi/hive-mcp {:local/root "../hive-mcp"}

;; For released version:
io.github.hive-agi/hive-mcp {:git/tag "vX.Y.Z" :git/sha "..."}
```

### 2. Start REPL

```bash
clojure -M:dev:nrepl
```

### 3. Test your addon

```clojure
(require '[{{top/ns}}.{{main/ns}}.addon :as addon])
(require '[hive-mcp.addons.protocol :as proto])

;; Create instance
(def a (addon/->addon))

;; Verify protocol
(proto/addon-id a)        ;=> "{{artifact/id}}"
(proto/addon-type a)      ;=> :native
(proto/capabilities a)    ;=> #{:tools}

;; Lifecycle
(proto/initialize! a {})  ;=> {:success? true ...}
(proto/health a)          ;=> {:status :ok ...}
(proto/tools a)           ;=> [{:name "hello" ...}]
(proto/shutdown! a)       ;=> {:success? true ...}
```

### 4. Run tests

```clojure
;; Via nREPL (preferred):
(require '[clojure.test :refer [run-tests]])
(run-tests '{{top/ns}}.{{main/ns}}.addon-test)
```

## Architecture

### Addon Lifecycle

```
classpath scan (META-INF/hive-addons/{{artifact/id}}.edn)
  -> loader resolves init-ns + init-fn
  -> init-as-addon! called (returns IAddon record)
  -> register-addon! (adds to registry)
  -> init-addon! (calls initialize!, registers tools/schemas)
  -> addon is ACTIVE (tools exposed via MCP)
  -> shutdown-addon! (calls shutdown!, deregisters tools)
```

### Manifest

The classpath manifest at `resources/META-INF/hive-addons/{{artifact/id}}.edn` tells
hive-mcp how to discover and initialize this addon:

```edn
{:addon/id          "{{artifact/id}}"
 :addon/type        :native
 :addon/init-ns     "{{top/ns}}.{{main/ns}}.addon"
 :addon/init-fn     "init-as-addon!"
 :addon/capabilities #{:tools}}
```

### IAddon Protocol (8 methods)

| Method | Purpose |
|--------|---------|
| `addon-id` | Unique string identifier |
| `addon-type` | `:native`, `:mcp-bridge`, or `:external` |
| `capabilities` | Set of capability keywords (`:tools`, `:schema`, etc.) |
| `initialize!` | Start services, return `{:success? bool}` |
| `shutdown!` | Release resources |
| `tools` | Return seq of MCP tool definitions |
| `schema-extensions` | DataScript schema additions |
| `health` | Return `{:status :ok/:degraded/:down}` |

### Adding Tools

Edit `addon.clj` and add entries to `tool-defs`:

```clojure
(def ^:private tool-defs
  [{:name        "my-tool"
    :description "What my tool does"
    :inputSchema {:type       "object"
                  :properties {"param" {:type "string" :description "..."}}
                  :required   ["param"]}
    :handler     handle-my-tool}])
```

Handlers receive keyword-ized params and return content items:

```clojure
(defn- handle-my-tool [{:keys [param]}]
  {:type "text" :text (str "Result: " param)})
```

### MCP Bridge Addons

For addons that proxy to external MCP servers, implement both `IAddon` and
`IMcpBridge`. See `hive-mcp.addons.stdio_bridge` for a reference implementation.

Change manifest type to `:mcp-bridge` and add `:mcp-bridge` to capabilities.

## Project Structure

```
{{raw-name}}/
+-- deps.edn                                    ;; Dependencies
+-- .hive-project.edn                           ;; Hive project descriptor
+-- README.md                                   ;; This file
+-- src/{{top/file}}/{{main/file}}/
|   +-- addon.clj                               ;; IAddon implementation
+-- test/{{top/file}}/{{main/file}}/
|   +-- addon_test.clj                          ;; Tests
+-- resources/META-INF/hive-addons/
    +-- {{artifact/id}}.edn                      ;; Classpath manifest
```

## License

Copyright {{now/year}} {{developer}}

Distributed under the AGPL-3.0-or-later license.
