# Debugging hive-mcp Startup Errors

This guide documents how to diagnose and fix startup failures when the MCP server won't connect.

## Symptoms

- `/mcp` shows `emacs · ✘ failed`
- Claude Code reports "Failed to reconnect to emacs"
- bb-mcp hangs during initialization

## Architecture Overview

```
Claude Code
    │
    ▼
.mcp.json (emacs server config)
    │
    ▼
start-bb-mcp.sh (lightweight wrapper)
    │
    ▼
bb-mcp (Babashka) ──auto-spawns──▶ hive-mcp (JVM)
    │                                    │
    └────── nREPL connection ◀───────────┘
                (port 7910)
```

When `emacs` MCP server starts:
1. `start-bb-mcp.sh` launches bb-mcp
2. bb-mcp checks if hive-mcp is running (port 7910)
3. If not running, bb-mcp auto-spawns hive-mcp
4. Spawn logs go to `~/.config/hive-mcp/server.log`

## Step-by-Step Debugging

### Step 1: Check the MCP Configuration

```bash
cat .mcp.json
```

Look for the `emacs` server entry:
```json
{
  "mcpServers": {
    "emacs": {
      "type": "stdio",
      "command": "/path/to/hive-mcp/start-bb-mcp.sh",
      "args": []
    }
  }
}
```

### Step 2: Test bb-mcp Directly

Send an MCP initialize message to see what happens:

```bash
cd ~/PP/bb-mcp  # or wherever bb-mcp lives
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' | timeout 10 bb -m bb-mcp.core 2>&1
```

**Expected output if hive-mcp is running:**
```
bb-mcp: ready {...}
bb-mcp: connected to hive-mcp on port 7910
{"jsonrpc":"2.0","id":1,"result":{...}}
```

**Output if hive-mcp needs to start:**
```
bb-mcp: not-running {...}
bb-mcp: spawning hive-mcp...
```

### Step 3: Check the Server Log (Critical!)

The actual error is in the server log:

```bash
cat ~/.config/hive-mcp/server.log
```

This reveals JVM compilation/runtime errors that bb-mcp's stdio interface hides.

**Example error:**
```
=== Starting hive-mcp at Fri Jan 16 09:53:06 AM -03 2026 ===
Syntax error compiling at (hive_mcp/chroma.clj:256:14).
Unable to resolve symbol: metadata-defaults in this context
```

### Step 4: Verify Compilation

Test that the Clojure code compiles:

```bash
cd /path/to/hive-mcp
clojure -M:dev -e "(require 'hive-mcp.chroma) (println :ok)"
```

If this fails, you have a compilation error to fix.

### Step 5: Check Port Status

```bash
# Is anything listening on 7910?
nc -zv localhost 7910

# Find hive-mcp process
pgrep -f "hive-mcp.*clojure"
```

### Step 6: Kill Stale Processes and Restart

```bash
# Kill any stuck hive-mcp processes
kill $(pgrep -f "hive-mcp.*clojure") 2>/dev/null

# Start fresh
cd /path/to/hive-mcp
nohup clojure -X:mcp >> ~/.config/hive-mcp/server.log 2>&1 &

# Wait and verify
sleep 5
nc -zv localhost 7910
```

## Common Errors and Fixes

### Error: "Unable to resolve symbol: X in this context"

**Cause:** Clojure forward reference - a symbol is used before it's defined.

**Fix:** Move the `def` or `defn` to appear before its first usage.

**Example (this session's fix):**
```clojure
;; WRONG: metadata-defaults used on line 256, defined on line 350
(defn index-memory-entry! [...]
  (let [meta (merge metadata-defaults ...)]  ; line 256 - ERROR!
    ...))

(def ^:private metadata-defaults {...})       ; line 350 - too late!

;; CORRECT: definition moved before usage
(def ^:private metadata-defaults {...})       ; line 232

(defn index-memory-entry! [...]
  (let [meta (merge metadata-defaults ...)]  ; line 262 - OK
    ...))
```

### Error: "Connection refused" on port 7910

**Causes:**
1. hive-mcp failed to start (check server.log)
2. hive-mcp started but crashed
3. Wrong port configured

**Fix:** Check `~/.config/hive-mcp/server.log` for the real error.

### Error: "nREPL" (single word error)

**Cause:** bb-mcp connected but the nREPL evaluation failed.

**Fix:** Ensure Emacs has `(server-start)` running, or check CIDER connection.

### Error: Lock file stale

**Cause:** Previous startup attempt didn't clean up.

**Fix:**
```bash
rm ~/.config/hive-mcp/starting.lock
```

## Key Files and Locations

| File | Purpose |
|------|---------|
| `.mcp.json` | MCP server configuration |
| `start-bb-mcp.sh` | Entry point script |
| `~/PP/bb-mcp/` | Babashka MCP wrapper |
| `~/.config/hive-mcp/server.log` | **JVM startup logs (check this!)** |
| `~/.config/hive-mcp/starting.lock` | Lock file during startup |
| `src/hive_mcp/*.clj` | Clojure source files |

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `HIVE_MCP_DIR` | `~/hive-mcp` | Path to hive-mcp project |
| `BB_MCP_NREPL_PORT` | `7910` | nREPL port for communication |
| `BB_MCP_PROJECT_DIR` | Script dir | Project context |

## Quick Diagnostic Commands

```bash
# Full diagnostic
echo "=== Port ===" && nc -zv localhost 7910 2>&1
echo "=== Process ===" && pgrep -fa "hive-mcp.*clojure"
echo "=== Lock ===" && cat ~/.config/hive-mcp/starting.lock 2>/dev/null || echo "No lock"
echo "=== Log (last 20 lines) ===" && tail -20 ~/.config/hive-mcp/server.log
```

## Manual MCP Server Testing

MCP servers use **JSON-RPC 2.0 over stdio**. You can test them directly without Claude Code by sending the initialization handshake.

### The MCP Initialize Message

Copy this exact message for testing:

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"debug","version":"1.0"}}}' | timeout 10 bb -m bb-mcp.core 2>&1
```

### Message Breakdown

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {},
    "clientInfo": {
      "name": "debug",
      "version": "1.0"
    }
  }
}
```

| Field | Value | Purpose |
|-------|-------|---------|
| `jsonrpc` | `"2.0"` | JSON-RPC protocol version (always 2.0) |
| `id` | `1` | Request ID - server echoes this in response |
| `method` | `"initialize"` | First message in MCP handshake |
| `params.protocolVersion` | `"2024-11-05"` | MCP specification version |
| `params.capabilities` | `{}` | Client capabilities (empty = basic) |
| `params.clientInfo.name` | `"debug"` | Identifies the client |
| `params.clientInfo.version` | `"1.0"` | Client version |

### Expected Responses

**Success (server running):**
```
bb-mcp: ready {:port {:available? true, :evidence "socket connected"}, ...}
bb-mcp: connected to hive-mcp on port 7910
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}},"serverInfo":{"name":"bb-mcp","version":"0.1.0"}}}
```

**Auto-spawn triggered (server not running):**
```
bb-mcp: not-running {:port {:available? false, :evidence "connection refused"}, :lock {:starting? false, :evidence "no lock file"}, :process {:running? false, :evidence "no match"}}
bb-mcp: spawning hive-mcp...
```

**Compilation error (check server.log):**
```
bb-mcp: not-running {...}
bb-mcp: spawning hive-mcp...
bb-mcp: failed to start hive-mcp
```

### Testing the JVM Server Directly

To bypass bb-mcp and test hive-mcp directly:

```bash
cd /path/to/hive-mcp
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"debug","version":"1.0"}}}' | timeout 10 clojure -X:mcp 2>&1
```

### Other Useful MCP Messages

**List available tools:**
```bash
echo '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}' | bb -m bb-mcp.core 2>&1
```

**Call a simple tool (bash):**
```bash
echo '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"bash","arguments":{"command":"echo hello"}}}' | bb -m bb-mcp.core 2>&1
```

### Why This Works

MCP servers are stdio-based:
- **stdin** → JSON-RPC requests (what Claude Code sends)
- **stdout** → JSON-RPC responses (tool results, capabilities)
- **stderr** → Diagnostic logs (bb-mcp status messages)

By piping JSON directly, you simulate exactly what Claude Code does - revealing startup behavior and errors before they get swallowed by the MCP client.
