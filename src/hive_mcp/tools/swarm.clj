(ns hive-mcp.tools.swarm
  "Swarm management and JVM resource cleanup tools.

   Facade module delegating to focused submodules:
   - swarm.core      - Shared utilities, addon check
   - swarm.registry  - Lings registry, event-driven sync
   - swarm.state     - Hivemind state integration
   - swarm.lifecycle - Spawn, kill handlers
   - swarm.dispatch  - Dispatch handler with coordinator
   - swarm.collect   - Collect handler with push/poll
   - swarm.status    - Status, lings-available, broadcast, presets
   - swarm.prompt    - Pending prompts, respond handlers
   - swarm.channel   - Channel event management
   - swarm.jvm       - JVM process cleanup
   - swarm.wave      - Batch drone wave execution

   SOLID: Facade pattern - thin delegation to focused modules.
   CLARITY: L - Layers stay pure (facade separate from implementation)."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.tools.swarm.state :as state]
            [hive-mcp.tools.swarm.lifecycle :as lifecycle]
            [hive-mcp.tools.swarm.dispatch :as dispatch]
            [hive-mcp.tools.swarm.collect :as collect]
            [hive-mcp.tools.swarm.status :as status]
            [hive-mcp.tools.swarm.prompt :as prompt]
            [hive-mcp.tools.swarm.channel :as channel]
            [hive-mcp.tools.swarm.jvm :as jvm]
            [hive-mcp.tools.swarm.wave :as wave]
            [hive-mcp.swarm.coordinator :as coord]
            [clojure.data.json :as json]))

;; ============================================================
;; Backward Compatibility: Re-export Registry API
;; ============================================================
;; Tests access these directly from hive-mcp.tools.swarm

(def lings-registry
  "Atom tracking spawned lings. Re-exported from registry module."
  registry/lings-registry)

(def register-ling!
  "Register a spawned ling. Re-exported from registry module."
  registry/register-ling!)

(def unregister-ling!
  "Unregister a ling. Re-exported from registry module."
  registry/unregister-ling!)

(def get-available-lings
  "Get all registered lings. Re-exported from registry module."
  registry/get-available-lings)

(def start-registry-sync!
  "Start event-driven registry sync. Re-exported from registry module."
  registry/start-registry-sync!)

(def stop-registry-sync!
  "Stop registry sync. Re-exported from registry module."
  registry/stop-registry-sync!)

;; ============================================================
;; Backward Compatibility: Re-export State API
;; ============================================================

(def get-slave-working-status
  "Get slave working status from hivemind. Re-exported from state module."
  state/get-slave-working-status)

(def get-unified-swarm-status
  "Get unified swarm status. Re-exported from state module."
  state/get-unified-swarm-status)

;; ============================================================
;; Backward Compatibility: Re-export Channel API
;; ============================================================

(def start-channel-subscriptions!
  "Start channel subscriptions. Re-exported from channel module."
  channel/start-channel-subscriptions!)

(def stop-channel-subscriptions!
  "Stop channel subscriptions. Re-exported from channel module."
  channel/stop-channel-subscriptions!)

(def check-event-journal
  "Check event journal for task. Re-exported from channel module."
  channel/check-event-journal)

(def clear-event-journal!
  "Clear event journal. Re-exported from channel module."
  channel/clear-event-journal!)

;; ============================================================
;; Backward Compatibility: Re-export Core API
;; ============================================================

(def swarm-addon-available?
  "Check if swarm addon is available. Re-exported from core module."
  core/swarm-addon-available?)

;; ============================================================
;; Backward Compatibility: Re-export Status Helpers
;; ============================================================

(def query-elisp-lings
  "Query elisp for lings. Re-exported from status module."
  status/query-elisp-lings)

(def format-lings-for-response
  "Format lings for response. Re-exported from status module."
  status/format-lings-for-response)

;; ============================================================
;; Handler Delegation
;; ============================================================

(def handle-swarm-spawn
  "Spawn a new slave. Delegated to lifecycle module."
  lifecycle/handle-swarm-spawn)

(def handle-swarm-kill
  "Kill a slave. Delegated to lifecycle module."
  lifecycle/handle-swarm-kill)

(def handle-swarm-dispatch
  "Dispatch to a slave. Delegated to dispatch module."
  dispatch/handle-swarm-dispatch)

(def handle-swarm-collect
  "Collect task result. Delegated to collect module."
  collect/handle-swarm-collect)

(def handle-swarm-status
  "Get swarm status. Delegated to status module."
  status/handle-swarm-status)

(def handle-lings-available
  "List available lings. Delegated to status module."
  status/handle-lings-available)

(def handle-swarm-broadcast
  "Broadcast to all slaves. Delegated to status module."
  status/handle-swarm-broadcast)

(def handle-swarm-list-presets
  "List presets. Delegated to status module."
  status/handle-swarm-list-presets)

(def handle-swarm-pending-prompts
  "Get pending prompts. Delegated to prompt module."
  prompt/handle-swarm-pending-prompts)

(def handle-swarm-respond-prompt
  "Respond to prompt. Delegated to prompt module."
  prompt/handle-swarm-respond-prompt)

(def handle-dispatch-drone-wave
  "Dispatch batch drone wave. Delegated to wave module."
  wave/handle-dispatch-drone-wave)

;; ============================================================
;; JVM Process Cleanup (delegated to jvm module)
;; ============================================================

(def parse-jvm-process-line
  "Parse a ps output line. Delegated to jvm module."
  jvm/parse-jvm-process-line)

(def parse-etime-to-minutes
  "Parse elapsed time to minutes. Delegated to jvm module."
  jvm/parse-etime-to-minutes)

(def find-jvm-processes
  "Find all JVM processes. Delegated to jvm module."
  jvm/find-jvm-processes)

(def get-all-process-parents
  "Get parent info for all processes. Delegated to jvm module."
  jvm/get-all-process-parents)

(def enrich-with-parent-info
  "Enrich process with parent info. Delegated to jvm module."
  jvm/enrich-with-parent-info)

(def get-process-swarm-info
  "Get swarm env vars for process. Delegated to jvm module."
  jvm/get-process-swarm-info)

(def classify-jvm-process
  "Classify JVM process. Delegated to jvm module."
  jvm/classify-jvm-process)

(def handle-jvm-cleanup
  "Handle JVM cleanup. Delegated to jvm module."
  jvm/handle-jvm-cleanup)

(def get-memory-usage
  "Get current RAM usage. Delegated to jvm module."
  jvm/get-memory-usage)

(def handle-resource-guard
  "Handle resource guard. Delegated to jvm module."
  jvm/handle-resource-guard)

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  "MCP tool definitions for swarm management.

   SOLID: ISP - Separate tool schemas for each operation."
  [{:name "swarm_spawn"
    :description "Spawn a new Claude slave instance for parallel task execution. Slaves run in vterm buffers with optional presets (system prompts)."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Name for the slave (used in buffer name)"}
                               "presets" {:type "array"
                                          :items {:type "string"}
                                          :description "List of preset names to apply (e.g., [\"tdd\", \"clarity\"])"}
                               "cwd" {:type "string"
                                      :description "Working directory for the slave (optional)"}
                               "role" {:type "string"
                                       :description "Predefined role (tester, reviewer, documenter, etc.)"}
                               "terminal" {:type "string"
                                           :description "Terminal type: vterm or eat (default: vterm)"}}
                  :required ["name"]}
    :handler handle-swarm-spawn}

   {:name "swarm_dispatch"
    :description "Send a prompt to a slave Claude instance. Runs pre-flight conflict checks. Returns task_id, or queues task if file conflicts detected."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "ID of the slave to send prompt to"}
                               "prompt" {:type "string"
                                         :description "The prompt/task to send to the slave"}
                               "timeout_ms" {:type "integer"
                                             :description "Optional timeout in milliseconds"}
                               "files" {:type "array"
                                        :items {:type "string"}
                                        :description "Explicit list of files this task will modify (optional, extracted from prompt if not provided)"}}
                  :required ["slave_id" "prompt"]}
    :handler handle-swarm-dispatch}

   {:name "swarm_status"
    :description "Get swarm status including all active slaves, their states, and task counts."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "Optional: get status of specific slave only"}}
                  :required []}
    :handler handle-swarm-status}

   {:name "swarm_collect"
    :description "Collect the response from a dispatched task. Waits for completion up to timeout."
    :inputSchema {:type "object"
                  :properties {"task_id" {:type "string"
                                          :description "ID of the task to collect results from"}
                               "timeout_ms" {:type "integer"
                                             :description "How long to wait for completion (default: 5000)"}}
                  :required ["task_id"]}
    :handler handle-swarm-collect}

   {:name "swarm_list_presets"
    :description "List all available swarm presets (system prompts for slave specialization)."
    :inputSchema {:type "object" :properties {}}
    :handler handle-swarm-list-presets}

   {:name "swarm_kill"
    :description "Kill a slave instance or all slaves."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "ID of slave to kill, or \"all\" to kill all slaves"}}
                  :required ["slave_id"]}
    :handler handle-swarm-kill}

   {:name "lings_available"
    :description "List all available lings (spawned slaves) with metadata. Use this to find ling IDs without having to remember them. Returns name, presets, cwd, and age for each ling."
    :inputSchema {:type "object" :properties {}}
    :handler handle-lings-available}

   {:name "swarm_broadcast"
    :description "Send the same prompt to all active slaves simultaneously."
    :inputSchema {:type "object"
                  :properties {"prompt" {:type "string"
                                         :description "The prompt to broadcast to all slaves"}}
                  :required ["prompt"]}
    :handler handle-swarm-broadcast}

   {:name "swarm_pending_prompts"
    :description "Get list of pending prompts from slaves awaiting human decision. Only relevant when hive-mcp-swarm-prompt-mode is 'human'."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-swarm-pending-prompts}

   {:name "swarm_respond_prompt"
    :description "Send a response to a pending prompt from a specific slave. Use to answer permission prompts when prompt-mode is 'human'."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "ID of the slave whose prompt to respond to"}
                               "response" {:type "string"
                                           :description "Response to send (e.g., 'y', 'n', or custom text)"}}
                  :required ["slave_id" "response"]}
    :handler handle-swarm-respond-prompt}

   {:name "jvm_cleanup"
    :description "Find and optionally kill orphaned JVM processes. Uses true orphan detection (parent dead or PID 1). Efficient: only 2 ps calls total. Keeps processes managed by living Claude sessions."
    :inputSchema {:type "object"
                  :properties {"min_age_minutes" {:type "integer"
                                                  :description "Minimum age in minutes for age-based cleanup (default: 30)"}
                               "dry_run" {:type "boolean"
                                          :description "If true, only report without killing (default: true)"}
                               "keep_types" {:type "array"
                                             :items {:type "string"}
                                             :description "JVM types to protect from cleanup (default: [\"shadow-cljs\", \"leiningen\"])"}
                               "swarm_only" {:type "boolean"
                                             :description "If true, only consider swarm-spawned processes"}
                               "true_orphans_only" {:type "boolean"
                                                    :description "If true, only kill truly orphaned processes (default: true)"}}
                  :required []}
    :handler handle-jvm-cleanup}

   {:name "resource_guard"
    :description "Check system resources and automatically clean up orphaned JVMs if memory is high. Use BEFORE spawning new Claude swarm slaves to prevent OOM. Returns spawn permission based on memory state."
    :inputSchema {:type "object"
                  :properties {"ram_threshold" {:type "integer"
                                                :description "Percentage threshold for high memory (default: 80)"}
                               "min_available_mb" {:type "integer"
                                                   :description "Minimum available RAM in MB (default: 2048)"}
                               "auto_cleanup" {:type "boolean"
                                               :description "Whether to auto-run jvm_cleanup when high (default: true)"}
                               "cleanup_dry_run" {:type "boolean"
                                                  :description "If auto_cleanup, whether to actually kill orphans (default: false)"}}
                  :required []}
    :handler handle-resource-guard}

   {:name "swarm_coordinator_status"
    :description "Get hivemind coordinator status including task queue, file claims, and logic database stats."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               {:type "text"
                :text (json/write-str (coord/coordinator-status))})}

   {:name "swarm_process_queue"
    :description "Process queued tasks - dispatch any tasks whose file conflicts have cleared."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               (let [ready (coord/process-queue!)]
                 {:type "text"
                  :text (json/write-str {:processed (count ready)
                                         :tasks (mapv #(select-keys % [:id :slave-id]) ready)})}))}

   {:name "dispatch_drone_wave"
    :description "Dispatch multiple drones in parallel for batch file mutations. Creates a change plan with multiple tasks and executes them concurrently (max 3). Returns wave status with results from each drone."
    :inputSchema {:type "object"
                  :properties {"tasks" {:type "array"
                                        :items {:type "object"
                                                :properties {"file" {:type "string"
                                                                     :description "File path to modify"}
                                                             "task" {:type "string"
                                                                     :description "Task description for this file"}}
                                                :required ["file" "task"]}
                                        :description "Array of {file, task} objects to execute in parallel"}
                               "preset" {:type "string"
                                         :description "Drone preset (default: drone-worker)"}
                               "trace" {:type "boolean"
                                        :description "Emit progress events (default: true)"}}
                  :required ["tasks"]}
    :handler handle-dispatch-drone-wave}])
