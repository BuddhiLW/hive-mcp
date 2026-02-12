(ns hive-mcp.tools.consolidated.multi
  "Single MCP entry point routing to all consolidated tools.

   Supports three input modes:
   - Single dispatch: {tool, command, ...params}
   - Batch dispatch: {operations: [...]} with dependency-ordered wave execution
   - DSL dispatch: {dsl: [[verb, params], ...]} compiled to batch operations

   Plus async execution for long-running batches."
  (:require [hive-mcp.tools.consolidated.agent :as c-agent]
            [hive-mcp.tools.consolidated.memory :as c-memory]
            [hive-mcp.tools.consolidated.kg :as c-kg]
            [hive-mcp.tools.consolidated.hivemind :as c-hivemind]
            [hive-mcp.tools.consolidated.magit :as c-magit]
            [hive-mcp.tools.consolidated.cider :as c-cider]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.tools.consolidated.preset :as c-preset]
            [hive-mcp.tools.consolidated.olympus :as c-olympus]
            [hive-mcp.tools.consolidated.agora :as c-agora]
            [hive-mcp.tools.consolidated.analysis :as c-analysis]
            [hive-mcp.tools.consolidated.project :as c-project]
            [hive-mcp.tools.consolidated.session :as c-session]
            [hive-mcp.tools.consolidated.emacs :as c-emacs]
            [hive-mcp.tools.consolidated.wave :as c-wave]
            [hive-mcp.tools.consolidated.migration :as c-migration]
            [hive-mcp.tools.consolidated.config :as c-config]
            [hive-mcp.tools.consolidated.lsp :as c-lsp]
            [hive-mcp.tools.consolidated.workflow :as c-workflow]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool-handlers
  {:agent     c-agent/handle-agent
   :memory    c-memory/handle-memory
   :kg        c-kg/handle-kg
   :hivemind  c-hivemind/handle-hivemind
   :magit     c-magit/handle-magit
   :cider     c-cider/handle-cider
   :kanban    c-kanban/handle-kanban
   :preset    c-preset/handle-preset
   :olympus   c-olympus/handle-olympus
   :agora     c-agora/handle-agora
   :analysis  c-analysis/handle-analysis
   :project   c-project/handle-project
   :session   c-session/handle-session
   :emacs     c-emacs/handle-emacs
   :wave      c-wave/handle-wave
   :migration c-migration/handle-migration
   :config    c-config/handle-config
   :lsp       c-lsp/handle-lsp
   :workflow  c-workflow/handle-workflow})

(def ^:private tool-names
  (sort (map name (keys tool-handlers))))

(defn get-tool-handler
  "Resolve a consolidated tool handler by name."
  [tool-name]
  (get tool-handlers (keyword tool-name)))

(defn- resolve-run-multi
  "Lazily resolve hive-mcp.tools.multi/run-multi to avoid circular deps."
  []
  (try
    (requiring-resolve 'hive-mcp.tools.multi/run-multi)
    (catch Exception e
      (log/error {:event :batch-resolve-error
                  :error (ex-message e)})
      nil)))

(defn- resolve-format-results
  "Lazily resolve hive-mcp.tools.multi/format-results."
  []
  (try
    (requiring-resolve 'hive-mcp.tools.multi/format-results)
    (catch Exception _
      nil)))

(defn- resolve-compile-paragraph
  "Lazily resolve hive-mcp.dsl.verbs/compile-paragraph for DSL→ops compilation.
   Returns the fn or nil if the DSL module is not available."
  []
  (try
    (requiring-resolve 'hive-mcp.dsl.verbs/compile-paragraph)
    (catch Exception e
      (log/debug {:event :dsl-resolve-error :error (ex-message e)})
      nil)))

(defn- resolve-async-fn
  "Lazily resolve a function from hive-mcp.tools.multi-async namespace.
   Returns the fn or nil if the async module is not available."
  [fn-name]
  (try
    (requiring-resolve (symbol "hive-mcp.tools.multi-async" (name fn-name)))
    (catch Exception e
      (log/debug {:event :async-resolve-error :fn fn-name :error (ex-message e)})
      nil)))

(defn- format-multi-help
  "Format help listing all available tools and their commands."
  []
  (str "Multi tool — single entry point for all hive-mcp operations.\n\n"
       "== Single Dispatch ==\n"
       "  multi {\"tool\": \"<name>\", \"command\": \"<cmd>\", ...params}\n\n"
       "== Batch Dispatch ==\n"
       "  multi {\"operations\": [{\"id\": \"op1\", \"tool\": \"memory\", \"command\": \"add\", ...},\n"
       "                         {\"id\": \"op2\", \"tool\": \"kg\", \"command\": \"edge\", \"depends_on\": [\"op1\"]}],\n"
       "         \"dry_run\": false}\n"
       "  Operations are topologically sorted by depends_on and executed in waves.\n"
       "  Independent ops run in parallel within each wave.\n\n"
       "== DSL Dispatch (verb syntax) ==\n"
       "  multi {\"dsl\": [[\"m+\", {\"c\": \"hello\", \"t\": \"note\"}],\n"
       "                   [\"k>\", {\"from\": \"$1\", \"to\": \"node-2\", \"rel\": \"implements\"}]]}\n"
       "  Verbs: m+ m? m@ m/ (memory), k> k^ k! k# (kg), a+ a? a! ax (agent),\n"
       "         b+ b> b? b# (kanban), s. s~ s? s< (session), g? g+ g! g> (magit),\n"
       "         w! w? wy wn (wave), h! h? (hivemind), p? p@ p/ (preset), c? c! c* (config)\n"
       "  Param aliases: c→content, t→type, #→tags, d→directory, q→query, n→name, f→files\n\n"
       "== Async Execution ==\n"
       "  multi {\"operations\": [...], \"async\": true}       → returns {\"batch_id\": \"...\"}\n"
       "  multi {\"dsl\": [...], \"async\": true}              → returns {\"batch_id\": \"...\"}\n"
       "  multi {\"command\": \"collect\", \"batch_id\": \"...\"}  → get async batch results\n"
       "  multi {\"command\": \"list-async\"}                  → list pending async batches\n"
       "  multi {\"command\": \"cancel-async\", \"batch_id\": \"...\"}  → cancel running batch\n\n"
       "Available tools:\n"
       (str/join "\n" (map #(str "  - " %) tool-names))
       "\n\nTo see commands for a specific tool:\n"
       "  multi {\"tool\": \"memory\", \"command\": \"help\"}\n\n"
       "All additional params are forwarded to the target tool handler."))

(defn- handle-batch
  "Handle batch dispatch mode with dependency-ordered wave execution.
   Supports async: true for non-blocking dispatch."
  [{:keys [operations dry_run async] :as _params}]
  (cond
    (nil? operations)
    {:isError true
     :text "Batch mode requires 'operations' array. Each op: {id, tool, command, ...params, depends_on?: [ids]}"}

    (not (sequential? operations))
    {:isError true
     :text "operations must be an array of {id, tool, command, ...} objects"}

    (empty? operations)
    {:isError true
     :text "operations array is empty. Provide at least one operation."}

    ;; Async mode: dispatch and return batch-id immediately
    async
    (if-let [async-fn (resolve-async-fn 'run-multi-async)]
      (try
        (let [normalized-ops (mapv (fn [op]
                                     (into {} (map (fn [[k v]] [(keyword k) v]) op)))
                                   operations)
              result (async-fn normalized-ops (cond-> {}
                                                dry_run (assoc :dry-run true)))]
          {:type "text" :text (pr-str result)})
        (catch Exception e
          {:isError true
           :text (str "Async dispatch failed: " (ex-message e))}))
      {:isError true
       :text (pr-str {:error "Async module not loaded"
                      :hint "hive-mcp.tools.multi-async is not available"})})

    ;; Synchronous batch execution
    :else
    (let [run-multi-fn (resolve-run-multi)
          format-fn    (resolve-format-results)
          ;; Resolve compact mode for batch envelope compression
          compact-mode (try
                         (when-let [resolve-fn (requiring-resolve
                                                'hive-mcp.dsl.response/resolve-compress-mode)]
                           (resolve-fn _params))
                         (catch Exception _ nil))]
      (if-not run-multi-fn
        {:isError true
         :text "Batch execution engine not available (hive-mcp.tools.multi/run-multi could not be resolved)"}

        ;; Normalize each op's keys to keywords (MCP sends string keys)
        (let [normalized-ops (mapv (fn [op]
                                     (into {} (map (fn [[k v]] [(keyword k) v]) op)))
                                   operations)
              result (if dry_run
                       (run-multi-fn normalized-ops :dry-run true)
                       (run-multi-fn normalized-ops))]
          (if format-fn
            (format-fn result :compact compact-mode)
            ;; Fallback if format-results not resolved
            {:type "text" :text (pr-str result)}))))))

;; =============================================================================
;; DSL Handling
;; =============================================================================

(defn- handle-dsl
  "Handle DSL verb input: compile paragraph → run via batch engine.
   DSL sentences are compiled to standard operations, then routed through
   the existing batch execution pipeline (sync or async)."
  [{:keys [dsl] :as params}]
  (if-let [compile-fn (resolve-compile-paragraph)]
    (try
      (let [ops (compile-fn dsl)]
        ;; Route compiled ops through handle-batch (reuses all validation/execution)
        (handle-batch (-> params
                          (dissoc :dsl)
                          (assoc :operations ops))))
      (catch Exception e
        {:isError true
         :text (str "DSL compilation failed: " (ex-message e))}))
    {:isError true
     :text (pr-str {:error "DSL module not loaded"
                    :hint "hive-mcp.dsl.verbs is not available"})}))

;; =============================================================================
;; Async Commands
;; =============================================================================

(defn- handle-async-collect
  "Collect results from an async batch by batch_id."
  [{:keys [batch_id]}]
  (if (str/blank? (str batch_id))
    {:isError true
     :text "collect requires 'batch_id' parameter"}
    (if-let [collect-fn (resolve-async-fn 'collect-async-result)]
      (try
        (let [result (collect-fn (str batch_id))]
          {:type "text" :text (pr-str result)})
        (catch Exception e
          {:isError true
           :text (str "Collect failed: " (ex-message e))}))
      {:isError true
       :text (pr-str {:error "Async module not loaded"
                      :hint "hive-mcp.tools.multi-async is not available"})})))

(defn- handle-async-list
  "List all pending/completed async batches."
  [_params]
  (if-let [list-fn (resolve-async-fn 'list-async-batches)]
    (try
      {:type "text" :text (pr-str (list-fn))}
      (catch Exception e
        {:isError true
         :text (str "List async failed: " (ex-message e))}))
    {:isError true
     :text (pr-str {:error "Async module not loaded"
                    :hint "hive-mcp.tools.multi-async is not available"})}))

(defn- handle-async-cancel
  "Cancel a running async batch by batch_id."
  [{:keys [batch_id]}]
  (if (str/blank? (str batch_id))
    {:isError true
     :text "cancel-async requires 'batch_id' parameter"}
    (if-let [cancel-fn (resolve-async-fn 'cancel-async-batch)]
      (try
        {:type "text" :text (pr-str (cancel-fn (str batch_id)))}
        (catch Exception e
          {:isError true
           :text (str "Cancel failed: " (ex-message e))}))
      {:isError true
       :text (pr-str {:error "Async module not loaded"
                      :hint "hive-mcp.tools.multi-async is not available"})})))

;; =============================================================================
;; Main Router
;; =============================================================================

(defn handle-multi
  "Route to consolidated tool by :tool param, forwarding remaining params.

   Supports five dispatch modes:
   1. DSL mode: :dsl present → compile verbs → batch execute
   2. Batch mode: :operations present → dependency-ordered wave execution
   3. Async commands: :command in #{collect, list-async, cancel-async}
   4. Single dispatch: :tool + :command → route to consolidated handler
   5. Help: no tool/operations/dsl → show help text"
  [params]
  ;; Normalize string keys -> keywords (MCP sends JSON with string keys)
  (let [normalized (into {} (map (fn [[k v]] [(keyword k) v]) params))
        {:keys [tool command operations dsl]} normalized]
    (cond
      ;; Mutual exclusion: dsl and operations cannot both be present
      (and (some? dsl) (some? operations))
      {:isError true
       :text "Cannot specify both 'dsl' and 'operations'. Use one input mode."}

      ;; DSL mode: dsl present → compile and execute
      (some? dsl)
      (handle-dsl normalized)

      ;; Async commands (no tool required)
      (= "collect" (str command))
      (handle-async-collect normalized)

      (= "list-async" (str command))
      (handle-async-list normalized)

      (= "cancel-async" (str command))
      (handle-async-cancel normalized)

      ;; Batch mode: operations present, no tool specified
      (and (some? operations) (or (nil? tool) (str/blank? (str tool))))
      (handle-batch normalized)

      ;; No tool and no operations -- show help
      (or (nil? tool) (str/blank? (str tool)))
      {:type "text" :text (format-multi-help)}

      ;; Help command at multi level
      (and (= "help" (str tool)) (nil? command))
      {:type "text" :text (format-multi-help)}

      ;; Route to consolidated handler (single dispatch)
      :else
      (let [tool-kw (keyword (str/lower-case (str tool)))
            handler (get tool-handlers tool-kw)]
        (if handler
          ;; Forward to consolidated handler (strip :tool, keep :command + rest)
          (handler (dissoc normalized :tool))
          ;; Unknown tool
          {:isError true
           :text (str "Unknown tool: " (name tool-kw)
                      ". Available: " (str/join ", " tool-names))})))))

(def tool-def
  {:name "multi"
   :consolidated true
   :description (str "Unified entry point for ALL hive-mcp operations. "
                     "Routes to any consolidated tool via {tool, command, ...params}. "
                     "Tools: " (str/join ", " tool-names) ". "
                     "Example: {\"tool\": \"memory\", \"command\": \"add\", \"content\": \"...\"}. "
                     "Use {\"tool\": \"<name>\", \"command\": \"help\"} to list commands for a tool.")
   :inputSchema {:type "object"
                 :properties {"tool"    {:type "string"
                                         :enum (vec tool-names)
                                         :description "Target consolidated tool name"}
                              "command" {:type "string"
                                         :description (str "Subcommand for the target tool (e.g. 'add', 'spawn', 'status'). "
                                                           "Also accepts async commands without tool: 'collect', 'list-async', 'cancel-async'")}
                              "directory"  {:type "string"
                                            :description "Working directory for project-scoped operations"}
                              "agent_id"   {:type "string"
                                            :description "Agent identifier for attribution/routing"}
                              "id"         {:type "string"
                                            :description "Entity ID (memory entry, task, node, etc.)"}
                              "content"    {:type "string"
                                            :description "Content for add/create operations"}
                              "type"       {:type "string"
                                            :description "Entity type (note, snippet, ling, drone, etc.)"}
                              "tags"       {:type "array"
                                            :items {:type "string"}
                                            :description "Tags for categorization/filtering"}
                              "query"      {:type "string"
                                            :description "Search query (semantic or text)"}
                              "name"       {:type "string"
                                            :description "Name identifier (agent, preset, etc.)"}
                              "message"    {:type "string"
                                            :description "Message content (shout, commit, etc.)"}
                              "prompt"     {:type "string"
                                            :description "Task prompt for dispatch operations"}
                              "files"      {:type "array"
                                            :items {:type "string"}
                                            :description "File paths for file-scoped operations"}
                              "operations" {:type "array"
                                            :items {:type "object"}
                                            :description (str "Batch operation array. Each op: {id, tool, command, ...params, depends_on?: [ids]}. "
                                                              "Mutually exclusive with 'dsl'.")}
                              "dsl"        {:type "array"
                                            :items {:type "array"}
                                            :description (str "DSL verb sentences as [verb, params] tuples. "
                                                              "Compiled to batch operations automatically. "
                                                              "Verbs: m+ (memory add), m? (query), m@ (get), m/ (search), "
                                                              "k> (kg edge), k^ (traverse), k! (impact), k# (stats), "
                                                              "a+ (agent spawn), a? (status), a! (dispatch), ax (kill), "
                                                              "b+ (kanban create), b> (update), b? (list), b# (status), "
                                                              "s. (session complete), s~ (wrap), s? (whoami), s< (catchup), "
                                                              "g? (git status), g+ (stage), g! (commit), g> (push), "
                                                              "w! (wave dispatch), w? (status), wy (approve), wn (reject), "
                                                              "h! (hivemind shout), h? (ask), "
                                                              "p? (preset list), p@ (get), p/ (search), "
                                                              "c? (config get), c! (set), c* (list). "
                                                              "Param aliases: c=content, t=type, #=tags, d=directory, "
                                                              "q=query, n=name, id=id, p=prompt, f=files. "
                                                              "Example: [[\"m+\", {\"c\": \"note text\", \"t\": \"note\"}], "
                                                              "[\"k>\", {\"from\": \"$1\", \"to\": \"node-2\", \"rel\": \"implements\"}]]. "
                                                              "Mutually exclusive with 'operations'.")}
                              "async"      {:type "boolean"
                                            :description (str "When true, execute batch/DSL asynchronously. "
                                                              "Returns {batch_id: \"...\"} immediately. "
                                                              "Use command 'collect' with batch_id to retrieve results later.")}
                              "batch_id"   {:type "string"
                                            :description "Batch ID for async commands: collect, cancel-async"}
                              "parallel"   {:type "boolean"
                                            :description "Run batch operations in parallel"}
                              "dry_run"    {:type "boolean"
                                            :description "Batch mode: validate and plan without executing"}}
                 :required []}
   :handler handle-multi})

(def tools [tool-def])
