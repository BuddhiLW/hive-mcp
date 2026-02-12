(ns clj-kondo-mcp.server
  "Main entry point for the clj-kondo MCP server.
   Starts a stdio-based JSON-RPC server exposing clj-kondo analysis tools."
  (:require [clj-kondo-mcp.mcp :as mcp]
            [clj-kondo-mcp.tools :refer [kondo-tools]]))

(def mcp-server
  "The MCP server instance with all clj-kondo tools registered."
  (mcp/make-server
   {:name "clj-kondo-mcp"
    :version "0.1.0"
    :tools kondo-tools}))

(defn -main
  "Start the clj-kondo MCP server.
   Reads JSON-RPC requests from stdin and writes responses to stdout."
  [& _args]
  (mcp/run-server! mcp-server))

(comment
  ;; For REPL testing - can test individual tool handlers
  (require '[clj-kondo-mcp.tools :as tools])
  ((:handler (:analyze kondo-tools)) {:path "src"})
  ((:handler (:lint kondo-tools)) {:path "src" :level "error"}))
