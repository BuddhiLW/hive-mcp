(ns hive-mcp.agent.executor
  "Tool execution with permission gates.
   
   Application layer use case for executing tool calls with:
   - Permission checking (dangerous tools require approval)
   - Human approval flow via hivemind
   - Result formatting for conversation history"
  (:require [hive-mcp.agent.registry :as registry]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.permissions :as permissions]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Permission Gates
;;; ============================================================

(defn requires-approval?
  "Check if a tool call requires human approval."
  [tool-name perms]
  (and (permissions/dangerous-tool? tool-name)
       (not (contains? (set perms) :auto-approve))))

(defn request-approval!
  "Request human approval via hivemind channel. Blocks until response."
  [agent-id tool-name arguments]
  (let [question (format "Agent %s wants to call %s with:\n%s\n\nApprove?"
                         agent-id tool-name (json/write-str arguments))
        response (hivemind/ask! agent-id question ["yes" "no"]
                                :timeout-ms 60000)]
    (= "yes" (:decision response))))

;;; ============================================================
;;; Tool Execution
;;; ============================================================

(defn execute-tool
  "Execute a tool by name with arguments. Returns MCP response format."
  [tool-name arguments]
  (if-let [tool (registry/get-tool tool-name)]
    (try
      (let [handler (:handler tool)
            result (handler arguments)]
        {:success true :result result})
      (catch Exception e
        (log/error e "Tool execution failed:" tool-name)
        {:success false :error (ex-message e)}))
    {:success false :error (str "Unknown tool: " tool-name)}))

(defn format-tool-result
  "Format tool result as assistant message for conversation history."
  [call-id tool-name result]
  {:role "tool"
   :tool_call_id call-id
   :name tool-name
   :content (if (:success result)
              (let [r (:result result)]
                (if (string? (:text r)) (:text r) (json/write-str r)))
              (str "Error: " (:error result)))})

(defn execute-tool-calls
  "Execute a batch of tool calls, respecting permissions.
   
   Returns vector of tool result messages for conversation history."
  [agent-id tool-calls permissions]
  (mapv (fn [{:keys [id name arguments]}]
          (let [approved? (or (not (requires-approval? name permissions))
                              (request-approval! agent-id name arguments))]
            (if approved?
              (let [result (execute-tool name arguments)]
                (format-tool-result id name result))
              (format-tool-result id name
                                  {:success false :error "Rejected by human"}))))
        tool-calls))
