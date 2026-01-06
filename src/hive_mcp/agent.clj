(ns hive-mcp.agent
  "Agent delegation with tool-use loop for two-tier LLM architecture.
   
   Allows Claude (coordinator) to delegate tasks to local models (Ollama)
   while giving them access to hive-mcp tools.
   
   Architecture:
   - LLMBackend protocol for pluggable model backends (see agent.protocol)
   - Tool-use loop: send → execute tool calls → append results → repeat
   - Permission gates via hivemind.ask! for dangerous operations
   - Max steps guardrail to prevent runaway loops
   
   Usage:
     (delegate! {:model \"devstral-small\"
                 :task \"Implement the foo function in src/bar.clj\"
                 :tools [:read_file :file_edit :grep :glob_files]
                 :max_steps 10})"
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel :as channel]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

;;; ============================================================
;;; LLMBackend Protocol
;;; ============================================================

;; LLMBackend protocol is defined in hive-mcp.agent.protocol
;; Import it here for convenience
(def LLMBackend proto/LLMBackend)
(def chat proto/chat)
(def model-name proto/model-name)

;;; ============================================================
;;; HTTP Client (shared)
;;; ============================================================

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.connectTimeout (Duration/ofSeconds 30))
        (.build))))

(defn- http-post
  "Make HTTP POST request, return parsed JSON response."
  [url body & {:keys [timeout-secs] :or {timeout-secs 300}}]
  (let [request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.header "Content-Type" "application/json")
                    (.POST (HttpRequest$BodyPublishers/ofString (json/write-str body)))
                    (.timeout (Duration/ofSeconds timeout-secs))
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofString))
        status (.statusCode response)
        body-str (.body response)]
    (if (= status 200)
      (json/read-str body-str :key-fn keyword)
      (throw (ex-info "HTTP request failed"
                      {:status status :body body-str :url url})))))

;;; ============================================================
;;; Ollama Backend
;;; ============================================================

(defn- ollama-tools->openai-format
  "Convert hive-mcp tool schemas to OpenAI function format for Ollama."
  [tools]
  (mapv (fn [{:keys [name description inputSchema]}]
          {:type "function"
           :function {:name name
                      :description description
                      :parameters (or inputSchema {:type "object" :properties {}})}})
        tools))

(defn- parse-ollama-response
  "Parse Ollama chat response into normalized format."
  [response]
  (let [message (get-in response [:message])
        content (:content message)
        tool-calls (:tool_calls message)]
    (cond
      ;; Tool calls present
      (seq tool-calls)
      {:type :tool_calls
       :calls (mapv (fn [tc]
                      {:id (str (java.util.UUID/randomUUID))
                       :name (get-in tc [:function :name])
                       :arguments (let [args (get-in tc [:function :arguments])]
                                    (if (string? args)
                                      (json/read-str args :key-fn keyword)
                                      args))})
                    tool-calls)}

      ;; Text response
      content
      {:type :text :content content}

      ;; Empty response
      :else
      {:type :text :content ""})))

(defrecord OllamaBackend [host model]
  proto/LLMBackend
  (chat [_ messages tools]
    (let [ollama-tools (when (seq tools)
                         (ollama-tools->openai-format tools))
          body (cond-> {:model model
                        :messages messages
                        :stream false}
                 (seq ollama-tools) (assoc :tools ollama-tools))
          response (http-post (str host "/api/chat") body :timeout-secs 300)]
      (parse-ollama-response response)))

  (model-name [_] model))

(defn ollama-backend
  "Create an Ollama backend for agent delegation.
   
   Options:
     :host - Ollama server URL (default: http://localhost:11434)
     :model - Model name (default: devstral-small:24b)"
  ([] (ollama-backend {}))
  ([{:keys [host model]
     :or {host "http://localhost:11434"
          model "devstral-small:24b"}}]
   (->OllamaBackend host model)))

;;; ============================================================
;;; Tool Registry & Execution
;;; ============================================================

(defonce tool-registry (atom {}))

(defn register-tools!
  "Register tools for agent use. Takes a seq of tool maps with :name and :handler."
  [tools]
  (doseq [{:keys [name handler] :as tool} tools]
    (swap! tool-registry assoc name (assoc tool :handler handler)))
  (log/info "Registered" (count tools) "tools for agent delegation"))

(defn get-tool-schemas
  "Get tool schemas for specified tool names (or all if nil)."
  [tool-names]
  (let [all-tools @tool-registry
        selected (if tool-names
                   (select-keys all-tools tool-names)
                   all-tools)]
    (mapv #(dissoc % :handler) (vals selected))))

(defn execute-tool
  "Execute a tool by name with arguments. Returns MCP response format."
  [tool-name arguments]
  (if-let [tool (get @tool-registry tool-name)]
    (try
      (let [handler (:handler tool)
            result (handler arguments)]
        {:success true :result result})
      (catch Exception e
        (log/error e "Tool execution failed:" tool-name)
        {:success false :error (ex-message e)}))
    {:success false :error (str "Unknown tool: " tool-name)}))

;;; ============================================================
;;; Permission Gates
;;; ============================================================

(def ^:private dangerous-tools
  "Tools that require human approval before execution."
  #{"file_write" "file_edit" "clojure_edit" "bash" "magit_commit" "magit_push"})

(defn- requires-approval?
  "Check if a tool call requires human approval."
  [tool-name permissions]
  (and (contains? dangerous-tools tool-name)
       (not (contains? (set permissions) :auto-approve))))

(defn- request-approval!
  "Request human approval via hivemind channel. Blocks until response."
  [agent-id tool-name arguments]
  (let [question (format "Agent %s wants to call %s with:\n%s\n\nApprove?"
                         agent-id tool-name (json/write-str arguments))
        response (hivemind/ask! agent-id question ["yes" "no"]
                                :timeout-ms 60000)]
    (= "yes" (:decision response))))

;;; ============================================================
;;; Tool-Use Loop
;;; ============================================================

(defn- format-tool-result
  "Format tool result as assistant message for conversation history."
  [call-id tool-name result]
  {:role "tool"
   :tool_call_id call-id
   :name tool-name
   :content (if (:success result)
              (let [r (:result result)]
                (if (string? (:text r)) (:text r) (json/write-str r)))
              (str "Error: " (:error result)))})

(defn- execute-tool-calls
  "Execute a batch of tool calls, respecting permissions."
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

(defn run-tool-loop
  "Run the agent tool-use loop until completion or max steps.
   
   Returns {:status :completed|:max_steps|:error
            :result \"final text response\"
            :steps [{:type :text|:tool_calls ...} ...]
            :tool_calls_made N}
   
   When trace? is true, emits progress events via channel for monitoring."
  [{:keys [backend task tools permissions max-steps agent-id trace?]
    :or {max-steps 15
         permissions #{}
         trace? false
         agent-id (str "agent-" (System/currentTimeMillis))}}]
  (let [tool-schemas (get-tool-schemas tools)
        initial-messages [{:role "system"
                           :content "You are a helpful coding assistant. Use tools to complete the task. Be concise."}
                          {:role "user"
                           :content task}]
        emit! (fn [event-type data]
                (when trace?
                  (channel/emit-event! event-type (assoc data :agent-id agent-id))))]
    (loop [messages initial-messages
           steps []
           tool-calls-made 0
           step-count 0]
      (if (>= step-count max-steps)
        (do
          (emit! :agent-max-steps {:step step-count :max max-steps})
          {:status :max_steps
           :result (str "Reached max steps (" max-steps ")")
           :steps steps
           :tool_calls_made tool-calls-made})

        (let [_ (log/debug "Agent step" step-count "- calling" (model-name backend))
              _ (emit! :agent-step {:step step-count :phase :calling-llm})
              response (chat backend messages tool-schemas)]

          (case (:type response)
            ;; Text response = task complete
            :text
            (do
              (emit! :agent-completed {:step step-count :tool-calls-made tool-calls-made})
              {:status :completed
               :result (:content response)
               :steps (conj steps response)
               :tool_calls_made tool-calls-made})

            ;; Tool calls = execute and continue
            :tool_calls
            (let [calls (:calls response)
                  tool-names (mapv :name calls)
                  _ (log/info "Agent executing" (count calls) "tool calls:" tool-names)
                  _ (emit! :agent-step {:step step-count :phase :executing-tools :tools tool-names})
                  tool-results (execute-tool-calls agent-id calls permissions)
                  ;; Add assistant message with tool_calls (arguments as object, not string)
                  assistant-msg {:role "assistant"
                                 :tool_calls (mapv (fn [{:keys [id name arguments]}]
                                                     {:id id
                                                      :type "function"
                                                      :function {:name name
                                                                 :arguments arguments}})
                                                   calls)}
                  new-messages (vec (concat messages [assistant-msg] tool-results))]
              (recur new-messages
                     (conj steps response)
                     (+ tool-calls-made (count calls))
                     (inc step-count)))

            ;; Unknown response type
            (do
              (emit! :agent-error {:step step-count :error (str "Unknown response: " (:type response))})
              {:status :error
               :result (str "Unknown response type: " (:type response))
               :steps steps
               :tool_calls_made tool-calls-made})))))))

;;; ============================================================
;;; Public API
;;; ============================================================

(defn delegate!
  "Delegate a task to a local model with tool access.
   
   Options:
     :model - Model name (default: devstral-small:24b)
     :host - Ollama host (default: http://localhost:11434)
     :task - Task description (required)
     :tools - List of tool names to allow (nil = all registered)
     :permissions - Set of permissions (:auto-approve skips human checks)
     :max-steps - Maximum tool-use iterations (default: 15)
     :trace - If true, emit progress events via channel for monitoring
   
   Returns result map with :status, :result, :steps, :tool_calls_made
   
   Progress events (when trace=true):
     :agent-started - Task begins
     :agent-step - Each LLM call or tool execution
     :agent-completed - Task finished
     :agent-max-steps - Hit iteration limit
     :agent-error - Something went wrong"
  [{:keys [model host task tools permissions max-steps trace]
    :or {model "devstral-small:24b"
         host "http://localhost:11434"
         max-steps 15
         permissions #{}
         trace false}
    :as opts}]
  (when-not task
    (throw (ex-info "Task is required" {:opts opts})))

  (let [backend (ollama-backend {:host host :model model})
        agent-id (str "delegate-" (System/currentTimeMillis))]

    (when trace
      (channel/emit-event! :agent-started {:agent-id agent-id :model model :task task}))

    (try
      (let [result (run-tool-loop {:backend backend
                                   :task task
                                   :tools tools
                                   :permissions permissions
                                   :max-steps max-steps
                                   :agent-id agent-id
                                   :trace? trace})]
        result)

      (catch Exception e
        (log/error e "Agent delegation failed")
        (when trace
          (channel/emit-event! :agent-failed {:agent-id agent-id :error (ex-message e)}))
        {:status :error
         :result (ex-message e)
         :steps []
         :tool_calls_made 0}))))

;;; ============================================================
;;; MCP Tool Definition
;;; ============================================================

(defn handle-agent-delegate
  "MCP handler for agent.delegate tool."
  [{:keys [model task tools permissions max_steps trace]}]
  (try
    (let [result (delegate! {:model (or model "devstral-small:24b")
                             :task task
                             :tools (when tools (set tools))
                             :permissions (set (map keyword (or permissions [])))
                             :max-steps (or max_steps 15)
                             :trace (boolean trace)})]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Delegation failed: " (ex-message e))))))

(def tools
  [{:name "agent_delegate"
    :description "Delegate a task to a local LLM (Ollama) with MCP tool access. The delegated model runs a tool-use loop until task completion. Use for implementation tasks to conserve coordinator context."
    :inputSchema {:type "object"
                  :properties {"model" {:type "string"
                                        :description "Ollama model (default: devstral-small:24b)"}
                               "task" {:type "string"
                                       :description "Task description for the agent"}
                               "tools" {:type "array"
                                        :items {:type "string"}
                                        :description "Tool names to allow (nil = all registered)"}
                               "permissions" {:type "array"
                                              :items {:type "string"}
                                              :description "Permissions: 'auto-approve' skips human checks"}
                               "max_steps" {:type "integer"
                                            :description "Max tool-use iterations (default: 15)"}
                               "trace" {:type "boolean"
                                        :description "Emit progress events via channel"}}
                  :required ["task"]}
    :handler handle-agent-delegate}])
