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
   - swarm.team      - Team composition selection
   - swarm.health    - Drone health monitoring and auto-recovery

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
            [hive-mcp.tools.swarm.jvm.parser :as parser]
            [hive-mcp.tools.swarm.jvm.orphan :as orphan]
            [hive-mcp.tools.swarm.jvm.classifier :as classifier]
            [hive-mcp.tools.swarm.jvm.memory :as memory]
            [hive-mcp.tools.swarm.wave :as wave]
            [hive-mcp.tools.swarm.validated-wave :as validated-wave]
            [hive-mcp.tools.swarm.team :as team]
            [hive-mcp.tools.swarm.health :as health]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.agent.drone.cache :as drone-cache]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Backward Compatibility: Re-export Registry API
;; ============================================================
;; Tests access these directly from hive-mcp.tools.swarm

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

(def handle-get-wave-status
  "Get wave execution status. Delegated to wave module."
  wave/handle-get-wave-status)

(def handle-dispatch-validated-wave
  "Dispatch validated wave with self-healing. Delegated to validated-wave module."
  validated-wave/handle-dispatch-validated-wave)

(def handle-team-select
  "Select team composition for task type. Delegated to team module."
  team/handle-team-select)

;; ============================================================
;; Drone Health Monitoring (delegated to health module)
;; ============================================================

(def handle-drone-health-status
  "Get drone health status. Delegated to health module."
  health/handle-drone-health-status)

(def handle-recover-drone
  "Manually recover stuck drone. Delegated to health module."
  health/handle-recover-drone)

(def handle-drone-health-control
  "Control drone health monitoring. Delegated to health module."
  health/handle-drone-health-control)

(def init-drone-health!
  "Initialize drone health monitoring. Delegated to health module."
  health/init!)

(def record-drone-heartbeat!
  "Record drone heartbeat. Delegated to health module."
  health/record-heartbeat!)

(def get-drone-health
  "Get drone health status. Delegated to health module."
  health/get-wave-health)

;; ============================================================
;; JVM Process Cleanup (delegated to jvm module)
;; ============================================================

(def parse-jvm-process-line
  "Parse a ps output line. Delegated to parser module."
  parser/parse-process-line)

(def parse-etime-to-minutes
  "Parse elapsed time to minutes. Delegated to parser module."
  parser/parse-etime-to-minutes)

(def find-jvm-processes
  "Find all JVM processes. Delegated to classifier module."
  classifier/find-jvm-processes)

(def get-all-process-parents
  "Get parent info for all processes. Delegated to classifier module."
  classifier/get-all-process-parents)

(def enrich-with-parent-info
  "Enrich process with parent info. Delegated to orphan module."
  orphan/enrich-with-parent-info)

(def get-process-swarm-info
  "Get swarm env vars for process. Delegated to classifier module."
  classifier/get-process-swarm-info)

(def classify-jvm-process
  "Classify JVM process. Delegated to classifier module."
  classifier/classify-jvm-process)

(def handle-jvm-cleanup
  "Handle JVM cleanup. Delegated to jvm module."
  jvm/handle-jvm-cleanup)

(def get-memory-usage
  "Get current RAM usage. Delegated to memory module."
  memory/get-memory-usage)

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
                                           :description "Terminal type: vterm or eat (default: vterm)"}
                               "kanban_task_id" {:type "string"
                                                 :description "Optional kanban task ID to link this ling with. Enables task-aware lifecycle (auto-move task to done when ling wraps via session_complete)."}}
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
    :description "Kill a slave instance or all slaves. When slave_id='all' and directory is provided, only kills lings belonging to that project (project-scoped kill). Cross-project kills are blocked by default for security."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "ID of slave to kill, or \"all\" to kill all slaves"}
                               "directory" {:type "string"
                                            :description "Working directory to scope kill (optional). When provided with slave_id='all', only kills lings belonging to that project."}
                               "force_cross_project" {:type "boolean"
                                                      :description "Allow cross-project kill (default: false). Set to true only when you explicitly need to kill a ling from another project."}}
                  :required ["slave_id"]}
    :handler handle-swarm-kill}

   {:name "lings_available"
    :description "List all available lings (spawned slaves) with metadata. Use this to find ling IDs without having to remember them. Returns name, presets, cwd, project-id, and age for each ling."
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
                                        :description "Emit progress events (default: true)"}
                               "cwd" {:type "string"
                                      :description "Working directory override for path resolution. Pass ling's cwd when spawned with a different project directory."}}
                  :required ["tasks"]}
    :handler handle-dispatch-drone-wave}

   {:name "get_wave_status"
    :description "Get current status of a wave execution. Use after dispatch_drone_wave to check progress and see any failed items with their error details."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID returned from dispatch_drone_wave"}}
                  :required ["wave_id"]}
    :handler handle-get-wave-status}

   {:name "dispatch_validated_wave"
    :description "Dispatch multiple drones with post-execution validation and self-healing. Runs kondo_lint after each iteration, generates fix tasks for errors, and re-dispatches until validation passes or max retries reached. Use this instead of dispatch_drone_wave when you need quality gates on drone output."
    :inputSchema {:type "object"
                  :properties {"tasks" {:type "array"
                                        :items {:type "object"
                                                :properties {"file" {:type "string"
                                                                     :description "File path to modify"}
                                                             "task" {:type "string"
                                                                     :description "Task description for this file"}}
                                                :required ["file" "task"]}
                                        :description "Array of {file, task} objects to execute"}
                               "validate" {:type "boolean"
                                           :description "Run clj-kondo lint after execution (default: true)"}
                               "max_retries" {:type "integer"
                                              :description "Max retry iterations for validation failures (default: 3)"}
                               "lint_level" {:type "string"
                                             :enum ["error" "warning" "info"]
                                             :description "Lint severity threshold (default: error)"}
                               "preset" {:type "string"
                                         :description "Drone preset (default: drone-worker)"}
                               "trace" {:type "boolean"
                                        :description "Emit progress events (default: true)"}
                               "cwd" {:type "string"
                                      :description "Working directory override for path resolution"}}
                  :required ["tasks"]}
    :handler handle-dispatch-validated-wave}

   {:name "team_select"
    :description "Select a pre-configured team composition for common task types. Returns ling specs with presets, parallelization strategy, and coordinator checklist. Task types: implementation, refactoring, greenfield, simplification, quality-review, documentation."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :enum ["implementation" "refactoring" "greenfield" "simplification" "quality-review" "documentation"]
                                            :description "Type of task to assemble a team for"}
                               "context" {:type "string"
                                          :description "Context string for name interpolation (e.g., auth-feature)"}
                               "auto_spawn" {:type "boolean"
                                             :description "Execute spawns immediately (default: false)"}}
                  :required ["task_type"]}
    :handler handle-team-select}

   ;; Drone Health Monitoring Tools
   {:name "drone_health_status"
    :description "Get current drone health status including active, alert, and stuck drones. Use to monitor drone wave execution and identify problems. Shows timeout thresholds: alert at 2 min, auto-kill at 5 min."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Optional wave ID to filter results"}}
                  :required []}
    :handler handle-drone-health-status}

   {:name "drone_recover"
    :description "Manually recover a stuck drone by releasing its file claims and killing the process. Use when auto-recovery hasn't triggered or you need immediate intervention."
    :inputSchema {:type "object"
                  :properties {"drone_id" {:type "string"
                                           :description "ID of the drone to recover"}
                               "retry" {:type "boolean"
                                        :description "Whether to queue task for retry (default: false)"}}
                  :required ["drone_id"]}
    :handler handle-recover-drone}

   {:name "drone_health_control"
    :description "Start or stop the background drone health monitoring loop. Monitoring checks every 30 seconds, alerts at 2 minutes of no progress, auto-kills stuck drones at 5 minutes."
    :inputSchema {:type "object"
                  :properties {"action" {:type "string"
                                         :enum ["start" "stop"]
                                         :description "Action to perform: 'start' or 'stop' monitoring"}}
                  :required ["action"]}
    :handler handle-drone-health-control}

   ;; Drone Cache Tools
   {:name "drone_cache_stats"
    :description "Get drone result cache statistics including hit rate, entries, evictions, and time saved. Use to monitor cache efficiency and decide if TTL adjustments are needed."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler (fn [_]
               {:type "text"
                :text (json/write-str (drone-cache/cache-stats))})}

   {:name "drone_cache_clear"
    :description "Clear all cached drone results. Use when you need to force fresh execution of all tasks, or for debugging cache-related issues."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler (fn [_]
               (let [cleared (drone-cache/clear-cache!)]
                 {:type "text"
                  :text (json/write-str {:cleared cleared
                                         :message (str "Cleared " cleared " cache entries")})}))}

   {:name "drone_cache_invalidate"
    :description "Invalidate cache entries for specific files. Use when you know files have changed outside of drone execution and cached results are stale."
    :inputSchema {:type "object"
                  :properties {"files" {:type "array"
                                        :items {:type "string"}
                                        :description "List of file paths to invalidate cached results for"}}
                  :required ["files"]}
    :handler (fn [{:keys [files]}]
               (let [invalidated (drone-cache/invalidate-cache! files)]
                 {:type "text"
                  :text (json/write-str {:invalidated invalidated
                                         :files files})}))}

   {:name "drone_cache_list"
    :description "List all cached task fingerprints with metadata. Use for debugging to see what's cached and when entries expire."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler (fn [_]
               {:type "text"
                :text (json/write-str {:cached-tasks (drone-cache/list-cached-tasks)})})}])
