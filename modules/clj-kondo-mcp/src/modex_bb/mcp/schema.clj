(ns modex-bb.mcp.schema
  "MCP schema constants for Babashka - Malli schemas removed for BB compatibility.")

;;; Constants
(def latest-protocol-version "2024-11-05")
(def json-rpc-version "2.0")

;; Standard JSON-RPC error codes
(def error-parse -32700)
(def error-invalid-request -32600)
(def error-method-not-found -32601)
(def error-invalid-params -32602)
(def error-internal -32603)
