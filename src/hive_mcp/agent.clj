(ns hive-mcp.agent
  "Agent delegation with tool-use loop for two-tier LLM architecture.
   
   Allows Claude (coordinator) to delegate tasks to local models (Ollama)
   or cloud models (OpenRouter) while giving them access to hive-mcp tools.
   
   Architecture:
   - LLMBackend protocol for pluggable model backends (see agent.protocol)
   - Tool-use loop: send → execute tool calls → append results → repeat
   - Permission gates via hivemind.ask! for dangerous operations
   - Max steps guardrail to prevent runaway loops
   - Task-based model selection for OpenRouter free tier
   
   Usage:
     ;; Ollama (local)
     (delegate! {:model \"devstral-small\"
                 :task \"Implement the foo function in src/bar.clj\"
                 :tools [:read_file :file_edit :grep :glob_files]
                 :max_steps 10})
     
     ;; OpenRouter with task type
     (delegate! {:backend :openrouter
                 :task-type :coding
                 :task \"Write a palindrome function\"})
     
     ;; OpenRouter with explicit model
     (delegate! {:backend :openrouter
                 :model \"mistralai/devstral-2512:free\"
                 :task \"Fix the bug in auth.clj\"})"
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ollama :as ollama :refer [ollama-backend]]
            [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel :as channel]
            [hive-mcp.permissions :as permissions]
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
;;; OpenRouter Backend (Task-Based Model Selection)
;;; ============================================================

(defonce openrouter-task-models
  ;; Task-type to model mapping for OpenRouter free tier.
  ;; Configurable via MCP tools from Elisp.
  ;;
  ;; Default task types:
  ;;   :coding      - Code generation, implementation, bug fixes
  ;;   :coding-alt  - Fallback for coding tasks  
  ;;   :arch        - Architecture, design decisions, planning
  ;;   :docs        - Documentation, explanations, comments
  (atom {:coding "mistralai/devstral-2512:free"
         :coding-alt "google/gemma-3-4b-it:free"
         :arch "xiaomi/mimo-v2-flash:free"
         :docs "openai/gpt-oss-120b:free"}))

(defonce preset-task-types
  ;; Mapping from swarm presets/roles to OpenRouter task types.
  ;; Configurable via MCP tools from Elisp.
  ;;
  ;; Preset categories:
  ;;   :coding - Implementation, testing, bug fixing
  ;;   :arch   - Architecture, design, review, planning
  ;;   :docs   - Documentation, explanations
  (atom {;; Implementation-focused
         "tdd" :coding
         "tester" :coding
         "fixer" :coding
         "refactorer" :coding
         "ling" :coding
         "minimal" :coding
         ;; Architecture/design-focused
         "reviewer" :arch
         "clarity" :arch
         "solid" :arch
         "ddd" :arch
         "researcher" :arch
         "task-coordinator" :arch
         "hivemind" :arch
         "hivemind-master" :arch
         "hive-master" :arch
         "mcp-first" :arch
         "ling-pattern" :arch
         ;; Documentation-focused
         "documenter" :docs}))

(defn preset->task-type
  "Get the task type for a preset name. Returns :coding as default."
  [preset]
  (get @preset-task-types (name preset) :coding))

(defn list-preset-mappings
  "List all preset to task-type mappings."
  []
  @preset-task-types)

(defn set-preset-task-type!
  "Set the task type for a preset.
   
   Example: (set-preset-task-type! \"my-preset\" :arch)"
  [preset task-type]
  (swap! preset-task-types assoc (name preset) (keyword task-type))
  @preset-task-types)

(defn list-openrouter-models
  "List all configured OpenRouter task models."
  []
  @openrouter-task-models)

(defn set-openrouter-model!
  "Set the model for a task type.
   
   Example: (set-openrouter-model! :coding \"anthropic/claude-3-haiku\")"
  [task-type model]
  (swap! openrouter-task-models assoc task-type model)
  @openrouter-task-models)

(defn remove-openrouter-model!
  "Remove a task type from the model mapping."
  [task-type]
  (swap! openrouter-task-models dissoc task-type)
  @openrouter-task-models)

(defn openrouter-backend
  "Create an OpenRouter backend for agent delegation.
   
   Options:
     :model     - Explicit model name (highest priority)
     :preset    - Swarm preset name for auto task-type selection
     :task-type - Task type for model selection (:coding :arch :docs)
     :api-key   - OpenRouter API key (or set OPENROUTER_API_KEY env)
   
   Priority: model > preset > task-type > :coding (default)
   
   Task types (configurable via MCP):
     :coding     - mistralai/devstral-2512:free
     :coding-alt - google/gemma-3-4b-it:free  
     :arch       - xiaomi/mimo-v2-flash:free
     :docs       - openai/gpt-oss-120b:free
   
   Preset mappings:
     tdd, tester, fixer, refactorer, ling → :coding
     reviewer, clarity, solid, ddd, researcher → :arch
     documenter → :docs"
  [{:keys [model preset task-type api-key]
    :or {task-type :coding}}]
  (let [models @openrouter-task-models
        ;; Priority: explicit model > preset-derived > task-type > default
        resolved-task-type (or (when preset (preset->task-type preset))
                               (keyword task-type)
                               :coding)
        resolved-model (or model
                           (get models resolved-task-type)
                           (get models :coding))
        key (or api-key (System/getenv "OPENROUTER_API_KEY"))]
    (when-not key
      (throw (ex-info "OpenRouter API key required" {:env "OPENROUTER_API_KEY"})))
    (log/debug "OpenRouter backend" {:preset preset :task-type resolved-task-type :model resolved-model})
    (require 'hive-mcp.agent.openrouter)
    ((resolve 'hive-mcp.agent.openrouter/->OpenRouterBackend) key resolved-model)))

;;; ============================================================
;;; Tool Registry & Execution
;;; ============================================================

(defonce tool-registry (atom {}))

(defonce ^:private tools-initialized? (atom false))

;; Forward declaration for ensure-tools-registered!
(declare register-tools!)

(defn ensure-tools-registered!
  "Lazily initialize tool registry on first use.
   This allows delegate! to work from REPL without manual registration."
  []
  (when (and (empty? @tool-registry)
             (not @tools-initialized?))
    (reset! tools-initialized? true)
    (try
      (log/info "Auto-registering tools for agent delegation...")
      (require 'hive-mcp.tools)
      (let [tools-var (resolve 'hive-mcp.tools/tools)]
        (when tools-var
          (register-tools! @tools-var)))
      (catch Exception e
        (log/warn "Failed to auto-register tools:" (ex-message e))))))

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

(def ^:private drone-allowed-tools
  "Safe tools for drone agents. Drones cannot write files directly - they must use propose_diff."
  ["read_file" "grep" "glob_files" "clojure_eval" "clojure_inspect_project"
   "magit_status" "magit_diff" "magit_log" "magit_branches"
   "propose_diff" "hivemind_shout"])

(defn- requires-approval?
  "Check if a tool call requires human approval."
  [tool-name permissions]
  (and (permissions/dangerous-tool? tool-name)
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
  "Execute a batch of tool calls, using tiered permissions."
  [agent-id tool-calls permissions]
  (mapv (fn [{:keys [id name arguments]}]
          (let [;; Check if auto-approve permission is set
                skip-check? (contains? (set permissions) :auto-approve)
                ;; Get permission tier and escalate if needed
                result (if skip-check?
                         {:approved true :tier :bypass :reviewer :auto}
                         (permissions/escalate! agent-id name arguments))]
            (if (:approved result)
              (let [exec-result (execute-tool name arguments)]
                (format-tool-result id name exec-result))
              (format-tool-result id name
                                  {:success false
                                   :error (format "Rejected by %s: %s"
                                                  (clojure.core/name (:reviewer result))
                                                  (:reason result "No reason"))}))))
        tool-calls))

(defn run-tool-loop
  "Run the agent tool-use loop until completion or max steps.
   
   Returns {:status :completed|:max_steps|:error
            :result \"final text response\"
            :steps [{:type :text|:tool_calls ...} ...]
            :tool_calls_made N}
   
   When trace? is true, emits progress events via channel for monitoring."
  [{:keys [backend task tools permissions max-steps agent-id trace?]
    :or {max-steps 50
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
                  ;; Add assistant message with tool_calls 
                  ;; OpenAI format requires arguments as JSON string, not object
                  assistant-msg {:role "assistant"
                                 :tool_calls (mapv (fn [{:keys [id name arguments]}]
                                                     {:id id
                                                      :type "function"
                                                      :function {:name name
                                                                 :arguments (if (string? arguments)
                                                                              arguments
                                                                              (json/write-str arguments))}})
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
  "Delegate a task to a local or cloud model with tool access.
   
   Options:
     :backend   - Backend type: :openrouter (default) or :ollama
     :model     - Model name (backend-specific)
     :preset    - Swarm preset for auto model selection (OpenRouter)
     :task-type - For OpenRouter: :coding, :coding-alt, :arch, :docs
     :host      - Ollama host (default: http://localhost:11434)
     :api-key   - OpenRouter API key (or set OPENROUTER_API_KEY env)
     :task      - Task description (required)
     :tools     - List of tool names to allow (nil = all registered)
     :permissions - Set of permissions (:auto-approve skips human checks)
     :max-steps - Maximum tool-use iterations (default: 50)
     :trace     - If true, emit progress events via channel for monitoring
   
   Returns result map with :status, :result, :steps, :tool_calls_made
   
   Examples:
     ;; OpenRouter with preset (auto model selection) - DEFAULT
     (delegate! {:preset \"tdd\" :task \"Write tests\"})
     
     ;; OpenRouter with task type
     (delegate! {:task-type :arch :task \"Review design\"})
     
     ;; OpenRouter with explicit model
     (delegate! {:model \"mistralai/devstral-2512:free\" :task \"...\"})
     
     ;; Ollama (local)
     (delegate! {:backend :ollama :task \"Fix the bug\" :model \"devstral-small:24b\"})"
  [{:keys [backend model host task preset task-type api-key tools permissions max-steps trace]
    :or {backend :openrouter
         host "http://localhost:11434"
         max-steps 50
         permissions #{}
         trace false}
    :as opts}]
  ;; Ensure tools are registered (lazy init for REPL usage)
  (ensure-tools-registered!)

  (when-not task
    (throw (ex-info "Task is required" {:opts opts})))

  (let [;; Only use default Ollama model if backend is :ollama and no model provided
        effective-model (or model
                            (when (= backend :ollama) "devstral-small:24b"))
        backend-instance (case backend
                           :ollama (ollama-backend {:host host :model (or effective-model "devstral-small:24b")})
                           :openrouter (openrouter-backend {:model effective-model
                                                            :preset preset
                                                            :task-type (or task-type :coding)
                                                            :api-key api-key})
                           ;; Default to openrouter
                           (openrouter-backend {:model effective-model
                                                :preset preset
                                                :task-type (or task-type :coding)
                                                :api-key api-key}))
        agent-id (str "delegate-" (System/currentTimeMillis))]

    (when trace
      (channel/emit-event! :agent-started {:agent-id agent-id
                                           :backend backend
                                           :preset preset
                                           :model (proto/model-name backend-instance)
                                           :task task}))

    (try
      (let [result (run-tool-loop {:backend backend-instance
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
  [{:keys [backend model task preset task_type api_key tools permissions max_steps trace]}]
  (try
    (let [result (delegate! {:backend (keyword (or backend "openrouter"))
                             :model model
                             :preset preset
                             :task-type (when task_type (keyword task_type))
                             :api-key api_key
                             :task task
                             :tools (when tools (set tools))
                             :permissions (set (map keyword (or permissions [])))
                             :max-steps (or max_steps 50)
                             :trace (boolean trace)})]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Delegation failed: " (ex-message e))))))

(defn prepare-drone-context
  "Prepare context for drone delegation by gathering catchup data."
  []
  (try
    (let [;; Get catchup tool handler
          catchup-handler (get @tool-registry "mcp_get_context")
          context (when catchup-handler
                    (catchup-handler {}))]
      (if (and context (:text context))
        (let [parsed (json/read-str (:text context) :key-fn keyword)]
          {:conventions (get-in parsed [:memory :conventions] [])
           :decisions (get-in parsed [:memory :decisions] [])
           :snippets (get-in parsed [:memory :snippets] [])
           :project (get parsed :project {})})
        {}))
    (catch Exception e
      (log/warn e "Failed to gather ling context")
      {})))

(defn delegate-drone!
  "Delegate a task to a drone (token-optimized leaf agent).
   
   Automatically:
   - Pre-injects file contents (drone doesn't need to read)
   - Injects catchup context (conventions, decisions, snippets)
   - Uses drone-worker preset for OpenRouter
   - Auto-applies any diffs proposed by the drone
   - Records results to hivemind for review
   - Reports status to parent ling (if parent-id provided) for swarm state sync
   
   Options:
     :task      - Task description (required)
     :files     - List of files the drone will modify (contents pre-injected)
     :preset    - Override preset (default: drone-worker)
     :trace     - Enable progress events (default: true)
     :parent-id - Parent ling's slave-id (for swarm status sync)
   
   Returns result map with :status, :result, :agent-id, :files-modified"
  [{:keys [task files preset trace parent-id]
    :or {preset "drone-worker"
         trace true}}]
  (let [;; Try to get parent-id from env var if not provided
        effective-parent-id (or parent-id
                                (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        context (prepare-drone-context)
        context-str (when (seq context)
                      (str "## Project Context\n"
                           (when (seq (:conventions context))
                             (str "### Conventions\n"
                                  (str/join "\n" (map :content (:conventions context)))
                                  "\n\n"))
                           (when (seq (:decisions context))
                             (str "### Decisions\n"
                                  (str/join "\n" (map :content (:decisions context)))
                                  "\n\n"))))
        ;; Pre-read file contents so drone has exact content for propose_diff
        file-contents-str (when (seq files)
                            (let [project-root (or (diff/get-project-root) "")
                                  contents (for [f files]
                                             (let [abs-path (if (str/starts-with? f "/")
                                                              f
                                                              (str project-root "/" f))]
                                               (try
                                                 (let [content (slurp abs-path)]
                                                   (str "### " f "\n```\n" content "```\n"))
                                                 (catch Exception e
                                                   (str "### " f "\n(File not found or unreadable: " (.getMessage e) ")\n")))))]
                              (str "## Current File Contents\n"
                                   "IMPORTANT: Use this EXACT content as old_content in propose_diff.\n"
                                   "Do NOT guess or assume file content - use what is provided below.\n\n"
                                   (str/join "\n" contents))))
        augmented-task (str context-str
                            "## Task\n" task
                            (when (seq files)
                              (str "\n\n## Files to modify\n"
                                   (str/join "\n" (map #(str "- " %) files))))
                            (when file-contents-str
                              (str "\n\n" file-contents-str)))
        ;; Generate drone-specific agent-id for tracking
        agent-id (str "drone-" (System/currentTimeMillis))
        ;; Capture diff state before drone runs
        diffs-before (set (keys @diff/pending-diffs))]

    ;; Shout started to parent ling's slave-id (for swarm registry sync)
    (when effective-parent-id
      (hivemind/shout! effective-parent-id :started
                       {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                        :message (format "Delegated drone %s working" agent-id)}))

    (let [result (delegate! {:backend :openrouter
                             :preset preset
                             :task augmented-task
                             :tools drone-allowed-tools
                             :trace trace})
          ;; Find new diffs proposed during this drone's execution
          diffs-after (set (keys @diff/pending-diffs))
          new-diff-ids (clojure.set/difference diffs-after diffs-before)
          ;; Auto-apply the new diffs
          diff-results (when (seq new-diff-ids)
                         (let [results (for [diff-id new-diff-ids]
                                         (let [diff-info (get @diff/pending-diffs diff-id)
                                               response (diff/handle-apply-diff {:diff_id diff-id})
                                               parsed (try (json/read-str (:text response) :key-fn keyword)
                                                           (catch Exception _ nil))]
                                           (if (:isError response)
                                             {:status :failed :file (:file-path diff-info) :error (:error parsed)}
                                             {:status :applied :file (:file-path diff-info)})))
                               {applied :applied failed :failed} (group-by :status results)]
                           (when (seq applied)
                             (log/info "Auto-applied drone diffs" {:drone agent-id :files (mapv :file applied)}))
                           (when (seq failed)
                             (log/warn "Some drone diffs failed to apply" {:drone agent-id :failures failed}))
                           {:applied (mapv :file applied)
                            :failed (mapv #(select-keys % [:file :error]) failed)}))]

      ;; Shout completion to parent ling's slave-id (for swarm registry sync)
      (when effective-parent-id
        (if (= :completed (:status result))
          (hivemind/shout! effective-parent-id :completed
                           {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                            :message (format "Drone %s completed. Files: %s"
                                             agent-id
                                             (str/join ", " (or (:applied diff-results) [])))})
          (hivemind/shout! effective-parent-id :error
                           {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                            :message (format "Drone %s failed: %s" agent-id (:result result))})))

      ;; Record result for coordinator review
      (hivemind/record-ling-result! agent-id
                                    {:task task
                                     :files files
                                     :result result
                                     :diff-results diff-results
                                     :parent-id effective-parent-id
                                     :timestamp (System/currentTimeMillis)})
      (assoc result
             :agent-id agent-id
             :parent-id effective-parent-id
             :files-modified (:applied diff-results)
             :files-failed (:failed diff-results)))))

(defn handle-delegate-drone
  "MCP tool handler for delegate_drone."
  [{:keys [task files preset trace parent_id]}]
  (if (str/blank? task)
    (mcp-error "Task is required")
    (let [result (delegate-drone! {:task task
                                   :files files
                                   :preset (or preset "drone-worker")
                                   :trace (if (nil? trace) true trace)
                                   :parent-id parent_id})]
      (mcp-json result))))

(def tools
  [{:name "agent_delegate"
    :description "Delegate a task to a local LLM (Ollama) or cloud LLM (OpenRouter) with MCP tool access. The delegated model runs a tool-use loop until task completion. Use for implementation tasks to conserve coordinator context."
    :inputSchema {:type "object"
                  :properties {"backend" {:type "string"
                                          :enum ["ollama" "openrouter"]
                                          :description "Backend: 'ollama' (local, default) or 'openrouter' (cloud)"}
                               "model" {:type "string"
                                        :description "Model name (backend-specific)"}
                               "preset" {:type "string"
                                         :description "Swarm preset for auto model selection (e.g., 'tdd', 'reviewer', 'documenter')"}
                               "task_type" {:type "string"
                                            :enum ["coding" "coding-alt" "arch" "docs"]
                                            :description "OpenRouter task type (preset takes priority if both specified)"}
                               "api_key" {:type "string"
                                          :description "OpenRouter API key (or set OPENROUTER_API_KEY env)"}
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
    :handler handle-agent-delegate}

   {:name "delegate_drone"
    :description "Delegate a task to a drone (token-optimized leaf agent). Drones use OpenRouter free-tier models and receive catchup context automatically. Use for file mutations to save coordinator tokens."
    :inputSchema {:type "object"
                  :properties {:task {:type "string"
                                      :description "Task description for the drone"}
                               :files {:type "array"
                                       :items {:type "string"}
                                       :description "List of files the drone will modify"}
                               :preset {:type "string"
                                        :description "Preset to use (default: drone-worker)"}
                               :trace {:type "boolean"
                                       :description "Enable progress events (default: true)"}
                               :parent_id {:type "string"
                                           :description "Parent ling's slave-id for swarm status sync. Pass your CLAUDE_SWARM_SLAVE_ID env var value."}}
                  :required ["task"]}
    :handler handle-delegate-drone}

   ;; OpenRouter model configuration
   {:name "openrouter_list_models"
    :description "List all configured OpenRouter task-type to model mappings. Shows which models are used for :coding, :arch, :docs tasks."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               (mcp-json {:models @openrouter-task-models
                          :task-types (keys @openrouter-task-models)}))}

   {:name "openrouter_set_model"
    :description "Set the OpenRouter model for a specific task type. Task types: coding, coding-alt, arch, docs (or custom)."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type (e.g., 'coding', 'arch', 'docs')"}
                               "model" {:type "string"
                                        :description "OpenRouter model ID (e.g., 'mistralai/devstral-2512:free')"}}
                  :required ["task_type" "model"]}
    :handler (fn [{:keys [task_type model]}]
               (let [task-key (keyword task_type)
                     updated (set-openrouter-model! task-key model)]
                 (mcp-json {:success true
                            :message (format "Set %s → %s" task_type model)
                            :models updated})))}

   {:name "openrouter_remove_model"
    :description "Remove an OpenRouter task-type mapping."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type to remove"}}
                  :required ["task_type"]}
    :handler (fn [{:keys [task_type]}]
               (let [task-key (keyword task_type)
                     updated (remove-openrouter-model! task-key)]
                 (mcp-json {:success true
                            :message (format "Removed %s" task_type)
                            :models updated})))}

   ;; Preset to task-type mappings
   {:name "preset_list_mappings"
    :description "List all swarm preset to task-type mappings. Shows which presets map to :coding, :arch, :docs."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               (let [mappings @preset-task-types
                     by-type (group-by val mappings)]
                 (mcp-json {:mappings mappings
                            :by-task-type {:coding (keys (get by-type :coding))
                                           :arch (keys (get by-type :arch))
                                           :docs (keys (get by-type :docs))}})))}

   {:name "preset_set_task_type"
    :description "Set the task type for a swarm preset. This determines which OpenRouter model is used when delegating with that preset."
    :inputSchema {:type "object"
                  :properties {"preset" {:type "string"
                                         :description "Preset name (e.g., 'tdd', 'reviewer', 'documenter')"}
                               "task_type" {:type "string"
                                            :enum ["coding" "coding-alt" "arch" "docs"]
                                            :description "Task type to map to"}}
                  :required ["preset" "task_type"]}
    :handler (fn [{:keys [preset task_type]}]
               (let [updated (set-preset-task-type! preset task_type)]
                 (mcp-json {:success true
                            :message (format "Set preset %s → %s" preset task_type)
                            :mappings updated})))}])
