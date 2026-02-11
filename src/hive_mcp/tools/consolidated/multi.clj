(ns hive-mcp.tools.consolidated.multi
  "Single MCP entry point routing to all consolidated tools."
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
   :lsp       (fn [params] (if-let [h (try (requiring-resolve 'lsp-mcp.tools/handle-lsp) (catch Exception _ nil))] (h params) {:content [{:type "text" :text (pr-str {:error "lsp-mcp not available" :hint "Add lsp-mcp to deps.local.edn"})}]}))
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
       "Available tools:\n"
       (str/join "\n" (map #(str "  - " %) tool-names))
       "\n\nTo see commands for a specific tool:\n"
       "  multi {\"tool\": \"memory\", \"command\": \"help\"}\n\n"
       "All additional params are forwarded to the target tool handler."))

(defn- handle-batch
  "Handle batch dispatch mode with dependency-ordered wave execution."
  [{:keys [operations dry_run] :as _params}]
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

    :else
    (let [run-multi-fn (resolve-run-multi)
          format-fn    (resolve-format-results)]
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
            (format-fn result)
            ;; Fallback if format-results not resolved
            {:type "text" :text (pr-str result)}))))))

(defn handle-multi
  "Route to consolidated tool by :tool param, forwarding remaining params."
  [params]
  ;; Normalize string keys -> keywords (MCP sends JSON with string keys)
  (let [normalized (into {} (map (fn [[k v]] [(keyword k) v]) params))
        {:keys [tool command operations]} normalized]
    (cond
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
                                         :description "Subcommand for the target tool (e.g. 'add', 'spawn', 'status')"}
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
                                            :description "Batch operation array for batch commands"}
                              "parallel"   {:type "boolean"
                                            :description "Run batch operations in parallel"}
                              "dry_run"    {:type "boolean"
                                            :description "Batch mode: validate and plan without executing"}}
                 :required []}
   :handler handle-multi})

(def tools [tool-def])
