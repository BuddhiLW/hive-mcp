(ns hive-mcp.agent.protocol
  "LLMBackend protocol for agent delegation.
   
   Extracted to separate namespace to allow multiple backend implementations
   without circular dependencies (OCP compliant).")

(defprotocol LLMBackend
  "Protocol for LLM backends that support tool calling."
  (chat [this messages tools]
    "Send messages to the model with available tools.
     Returns {:type :text :content \"...\"} or {:type :tool_calls :calls [...]}
     where each call is {:id \"...\" :name \"tool_name\" :arguments {...}}")
  (model-name [this] "Return the model identifier string."))
