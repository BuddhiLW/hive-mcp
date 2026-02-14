#!/usr/bin/env python3
"""Slow echo MCP server for pool saturation testing.

Identical to echo-mcp-server.py but with a 0.5s delay on tools/call.
Used to verify pool borrow timeouts when all connections are occupied.

Speaks JSON-RPC 2.0 over stdin/stdout (newline-delimited).
Exposes two tools:
  - echo: returns the input message (after 500ms delay)
  - add:  returns the sum of two numbers (after 500ms delay)

Usage:
  python3 scripts/slow-echo-mcp-server.py
"""

import json
import sys
import time

TOOL_DELAY_SECONDS = 0.5


def handle_request(req):
    method = req.get("method")
    req_id = req.get("id")
    params = req.get("params", {})

    if method == "initialize":
        return {
            "jsonrpc": "2.0",
            "id": req_id,
            "result": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "serverInfo": {"name": "slow-echo-mcp", "version": "0.1.0"},
            },
        }

    elif method == "notifications/initialized":
        return None

    elif method == "tools/list":
        return {
            "jsonrpc": "2.0",
            "id": req_id,
            "result": {
                "tools": [
                    {
                        "name": "echo",
                        "description": "Echo back the input message (slow)",
                        "inputSchema": {
                            "type": "object",
                            "properties": {
                                "message": {
                                    "type": "string",
                                    "description": "Message to echo back",
                                }
                            },
                            "required": ["message"],
                        },
                    },
                    {
                        "name": "add",
                        "description": "Add two numbers together (slow)",
                        "inputSchema": {
                            "type": "object",
                            "properties": {
                                "a": {
                                    "type": "number",
                                    "description": "First number",
                                },
                                "b": {
                                    "type": "number",
                                    "description": "Second number",
                                },
                            },
                            "required": ["a", "b"],
                        },
                    },
                ]
            },
        }

    elif method == "tools/call":
        tool_name = params.get("name")
        args = params.get("arguments", {})

        # Deliberate delay to simulate slow external service
        time.sleep(TOOL_DELAY_SECONDS)

        if tool_name == "echo":
            msg = args.get("message", "")
            return {
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {
                    "content": [{"type": "text", "text": msg}]
                },
            }

        elif tool_name == "add":
            a = args.get("a", 0)
            b = args.get("b", 0)
            return {
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {
                    "content": [{"type": "text", "text": str(a + b)}]
                },
            }

        else:
            return {
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {
                    "isError": True,
                    "content": [
                        {"type": "text", "text": f"Unknown tool: {tool_name}"}
                    ],
                },
            }

    else:
        return {
            "jsonrpc": "2.0",
            "id": req_id,
            "error": {
                "code": -32601,
                "message": f"Method not found: {method}",
            },
        }


def main():
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
            resp = handle_request(req)
            if resp is not None:
                sys.stdout.write(json.dumps(resp) + "\n")
                sys.stdout.flush()
        except json.JSONDecodeError:
            err = {
                "jsonrpc": "2.0",
                "id": None,
                "error": {"code": -32700, "message": "Parse error"},
            }
            sys.stdout.write(json.dumps(err) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()
