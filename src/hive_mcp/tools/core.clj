(ns hive-mcp.tools.core
  "Core utilities for MCP tool responses.
   
   All tool handler modules should require this namespace for
   consistent response formatting."
  (:require [clojure.data.json :as json]))

(defn mcp-success
  "Create a successful MCP response. Text can be string or will be pr-str'd."
  [text]
  {:type "text" :text (if (string? text) text (pr-str text))})

(defn mcp-error
  "Create an error MCP response."
  [message]
  {:type "text" :text message :isError true})

(defn mcp-json
  "Create a successful MCP response with JSON-encoded data."
  [data]
  {:type "text" :text (json/write-str data)})
