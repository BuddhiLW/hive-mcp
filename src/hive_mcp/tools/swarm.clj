(ns hive-mcp.tools.swarm
  "Swarm management and JVM resource cleanup tools.

   Handles Claude swarm slave spawning, dispatch, and lifecycle management.
   Also provides JVM process cleanup for garbage collecting orphaned processes.

   Push-based updates:
   When hive-mcp.channel is available, subscribes to swarm events for
   sub-100ms task completion detection. Falls back to polling if channel
   not connected."
  (:require [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.tools.swarm.jvm :as jvm]
            [hive-mcp.tools.swarm.channel :as channel]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.swarm.coordinator :as coord]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; ============================================================
;; Lings Registry - Track spawned lings for easy lookup
;; ============================================================

(defonce lings-registry
  ;; Atom tracking spawned lings: {slave-id {:name, :presets, :cwd, :spawned-at}}
  (atom {}))

(defn register-ling!
  "Register a spawned ling in the registry."
  [slave-id {:keys [name presets cwd]}]
  (swap! lings-registry assoc slave-id
         {:name name
          :presets presets
          :cwd cwd
          :spawned-at (System/currentTimeMillis)}))

(defn unregister-ling!
  "Remove a ling from the registry."
  [slave-id]
  (swap! lings-registry dissoc slave-id))

(defn get-available-lings
  "Get all registered lings."
  []
  @lings-registry)

;; ============================================================
;; Swarm Management Tools
;; ============================================================

;; ============================================================
;; Channel Delegation (see hive-mcp.tools.swarm.channel)
;; ============================================================

(defn start-channel-subscriptions!
  "Start listening for swarm events via channel.
   Delegates to hive-mcp.tools.swarm.channel."
  []
  (channel/start-channel-subscriptions!))

(defn stop-channel-subscriptions!
  "Stop all channel subscriptions.
   Delegates to hive-mcp.tools.swarm.channel."
  []
  (channel/stop-channel-subscriptions!))

(defn check-event-journal
  "Check event journal for task completion.
   Delegates to hive-mcp.tools.swarm.channel."
  [task-id]
  (channel/check-event-journal task-id))

(defn clear-event-journal!
  "Clear all entries from the event journal.
   Delegates to hive-mcp.tools.swarm.channel."
  []
  (channel/clear-event-journal!))

;; ============================================================
;; Swarm Addon Check
;; ============================================================

(defn swarm-addon-available?
  "Check if hive-mcp-swarm addon is loaded.
   Uses short timeout (2s) to fail fast if Emacs is unresponsive."
  []
  (let [{:keys [success result timed-out]} (ec/eval-elisp-with-timeout "(featurep 'hive-mcp-swarm)" 2000)]
    (and success (not timed-out) (= result "t"))))

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance.
   Uses timeout to prevent MCP blocking.
   Registers successful spawns in lings-registry for easy lookup."
  [{:keys [name presets cwd role terminal]}]
  (if (swarm-addon-available?)
    (let [presets-str (when (seq presets)
                        (format "'(%s)" (clojure.string/join " " (map #(format "\"%s\"" %) presets))))
          elisp (format "(json-encode (hive-mcp-swarm-api-spawn \"%s\" %s %s %s))"
                        (v/escape-elisp-string (or name "slave"))
                        (or presets-str "nil")
                        (if cwd (format "\"%s\"" (v/escape-elisp-string cwd)) "nil")
                        (if terminal (format "\"%s\"" terminal) "nil"))
          ;; Use 10s timeout for spawn as it may take longer
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
      (cond
        timed-out
        {:type "text"
         :text (json/write-str {:error "Spawn operation timed out"
                                :status "timeout"
                                :slave_name name})
         :isError true}

        success
        (do
          ;; Parse result to extract slave_id and register
          (try
            (let [parsed (json/read-str result :key-fn keyword)]
              (when-let [slave-id (or (:slave_id parsed) (:slave-id parsed))]
                (register-ling! slave-id {:name name :presets presets :cwd cwd})))
            (catch Exception e
              (log/warn "Failed to parse spawn result for registry:" (ex-message e))))
          {:type "text" :text result})

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded. Run (require 'hive-mcp-swarm)" :isError true}))

(defn handle-swarm-dispatch
  "Dispatch a prompt to a slave.
   Runs pre-flight conflict checks before dispatch.
   Uses timeout to prevent MCP blocking.

   Parameters:
   - slave_id: Target slave for dispatch
   - prompt: The prompt/task to send
   - timeout_ms: Optional timeout in milliseconds
   - files: Optional explicit list of files task will modify"
  [{:keys [slave_id prompt timeout_ms files]}]
  (if (swarm-addon-available?)
    ;; Pre-flight check: detect conflicts before dispatch
    (let [preflight (coord/dispatch-or-queue!
                     {:slave-id slave_id
                      :prompt prompt
                      :files files
                      :timeout-ms timeout_ms})]
      (case (:action preflight)
        ;; Blocked due to circular dependency - cannot proceed
        :blocked
        {:type "text"
         :text (json/write-str {:error "Dispatch blocked: circular dependency detected"
                                :status "blocked"
                                :would_deadlock (:would-deadlock preflight)
                                :slave_id slave_id})
         :isError true}

        ;; Queued due to file conflicts - will dispatch when conflicts clear
        :queued
        {:type "text"
         :text (json/write-str {:status "queued"
                                :task_id (:task-id preflight)
                                :queue_position (:position preflight)
                                :conflicts (:conflicts preflight)
                                :slave_id slave_id
                                :message "Task queued - waiting for file conflicts to clear"})}

        ;; Approved - proceed with dispatch
        :dispatch
        (let [effective-files (:files preflight)
              elisp (format "(json-encode (hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %s))"
                            (v/escape-elisp-string slave_id)
                            (v/escape-elisp-string prompt)
                            (or timeout_ms "nil"))
              ;; Dispatch should be quick - 5s default timeout
              {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
          (cond
            timed-out
            {:type "text"
             :text (json/write-str {:error "Dispatch operation timed out"
                                    :status "timeout"
                                    :slave_id slave_id})
             :isError true}

            success
            (do
              ;; Register file claims for successful dispatch
              (when-let [task-id (try
                                   (:task-id (json/read-str result :key-fn keyword))
                                   (catch Exception _ nil))]
                (when (seq effective-files)
                  (coord/register-task-claims! task-id slave_id effective-files)))
              {:type "text" :text result})

            :else
            {:type "text" :text (str "Error: " error) :isError true}))

        ;; Fallback
        {:type "text"
         :text (json/write-str {:error "Unknown pre-flight result"
                                :preflight preflight})
         :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-status
  "Get swarm status including all slaves and their states.
   Uses timeout to prevent MCP blocking."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if slave_id
                  (format "(json-encode (hive-mcp-swarm-status \"%s\"))" slave_id)
                  "(json-encode (hive-mcp-swarm-api-status))")
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
      (cond
        timed-out
        {:type "text"
         :text (json/write-str {:error "Status check timed out"
                                :status "timeout"})
         :isError true}

        success
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-collect
  "Collect response from a task with push-first, poll-fallback strategy.

   PHASE 2 OPTIMIZATION: Check event journal first for sub-100ms detection.
   If event not in journal, falls back to elisp polling with exponential backoff.

   Note: emacsclient returns a quoted string which unwrap-emacs-string handles.
   We only need ONE json/read-str to parse the actual JSON from elisp."
  [{:keys [task_id timeout_ms]}]
  (if (swarm-addon-available?)
    (let [timeout (or timeout_ms 300000) ; default 5 minutes
          start-time (System/currentTimeMillis)
          poll-interval-ms (atom 500) ; start at 500ms
          max-poll-interval 5000 ; max 5 seconds between polls
          elisp-timeout 10000] ; 10s timeout per elisp call

      ;; PHASE 2: Check event journal first (push-based, sub-100ms)
      (if-let [journal-event (check-event-journal task_id)]
        ;; Event found in journal - return immediately!
        (let [{:keys [status result error slave-id timestamp]} journal-event]
          (log/info "Task" task_id "found in event journal (push-based)")
          {:type "text"
           :text (json/write-str {:task_id task_id
                                  :status status
                                  :result result
                                  :error error
                                  :slave_id slave-id
                                  :via "channel-push"
                                  :elapsed_ms (- (System/currentTimeMillis) start-time)})})

        ;; Not in journal - fall back to polling
        (loop []
          (let [elapsed (- (System/currentTimeMillis) start-time)
                ;; Check journal again (event might have arrived during poll wait)
                journal-check (check-event-journal task_id)]
            (if journal-check
              ;; Found in journal during poll loop
              (let [{:keys [status result error slave-id]} journal-check]
                {:type "text"
                 :text (json/write-str {:task_id task_id
                                        :status status
                                        :result result
                                        :error error
                                        :slave_id slave-id
                                        :via "channel-push-delayed"
                                        :elapsed_ms elapsed})})

              ;; Still not in journal - poll elisp
              (let [elisp (format "(json-encode (hive-mcp-swarm-api-collect \"%s\" %s))"
                                  (v/escape-elisp-string task_id)
                                  (or timeout_ms "nil"))
                    {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp elisp-timeout)]
                (cond
                  ;; Elisp call timed out
                  timed-out
                  {:type "text"
                   :text (json/write-str {:task_id task_id
                                          :status "error"
                                          :error "Elisp evaluation timed out"
                                          :elapsed_ms elapsed})
                   :isError true}

                  ;; Elisp call failed
                  (not success)
                  {:type "text" :text (str "Error: " error) :isError true}

                  ;; Parse the result - SINGLE parse since unwrap-emacs-string
                  ;; already handles the emacsclient quote layer
                  :else
                  (let [;; Single parse: result is already the JSON string
                        parsed (try (json/read-str result :key-fn keyword)
                                    (catch Exception e
                                      (log/warn "Failed to parse collect result:" result
                                                "Error:" (.getMessage e))
                                      nil))
                        status (:status parsed)]
                    (cond
                      ;; Parse failed - log and return error
                      (nil? parsed)
                      {:type "text"
                       :text (json/write-str {:task_id task_id
                                              :status "error"
                                              :error "Failed to parse elisp response"
                                              :raw_result result
                                              :elapsed_ms elapsed})
                       :isError true}

                      ;; Task complete or failed - return result
                      (contains? #{"completed" "timeout" "error"} status)
                      {:type "text" :text (json/write-str parsed)}

                      ;; Still polling and within timeout - wait and retry
                      (and (= status "polling") (< elapsed timeout))
                      (do
                        (Thread/sleep @poll-interval-ms)
                        ;; Exponential backoff
                        (swap! poll-interval-ms #(min max-poll-interval (* % 2)))
                        (recur))

                      ;; Exceeded our timeout or unknown status
                      :else
                      {:type "text"
                       :text (json/write-str {:task_id task_id
                                              :status "timeout"
                                              :error (format "Collection timed out after %dms (status was: %s)" elapsed status)
                                              :elapsed_ms elapsed})})))))))))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-list-presets
  "List available swarm presets.
   Uses timeout to prevent MCP blocking."
  [_]
  (if (swarm-addon-available?)
    (let [{:keys [success result error timed-out]} (ec/eval-elisp-with-timeout "(json-encode (hive-mcp-swarm-api-list-presets))" 5000)]
      (cond
        timed-out
        {:type "text"
         :text (json/write-str {:error "List presets timed out"
                                :status "timeout"})
         :isError true}

        success
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-kill
  "Kill a slave or all slaves.
   Uses short timeout (3s) as kill should be fast.
   Removes killed lings from registry."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if (= slave_id "all")
                  "(json-encode (hive-mcp-swarm-api-kill-all))"
                  (format "(json-encode (hive-mcp-swarm-api-kill \"%s\"))"
                          (v/escape-elisp-string slave_id)))
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
      (cond
        timed-out
        {:type "text"
         :text (json/write-str {:error "Kill operation timed out"
                                :status "timeout"
                                :slave_id slave_id})
         :isError true}

        success
        (do
          ;; Unregister from lings registry
          (if (= slave_id "all")
            (reset! lings-registry {})
            (unregister-ling! slave_id))
          {:type "text" :text result})

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-lings-available
  "List all available lings (spawned slaves) with their metadata.
   Returns lings from registry, no Emacs call needed."
  [_]
  (let [lings (get-available-lings)
        formatted (into {}
                        (map (fn [[id info]]
                               [id (assoc info
                                          :age-minutes (quot (- (System/currentTimeMillis)
                                                                (:spawned-at info))
                                                             60000))])
                             lings))]
    {:type "text"
     :text (json/write-str {:count (count lings)
                            :lings formatted})}))

(defn handle-swarm-broadcast
  "Broadcast a prompt to all slaves.
   Uses timeout to prevent MCP blocking."
  [{:keys [prompt]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (hive-mcp-swarm-broadcast \"%s\"))"
                        (v/escape-elisp-string prompt))
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
      (cond
        timed-out
        {:type "text"
         :text (json/write-str {:error "Broadcast operation timed out"
                                :status "timeout"})
         :isError true}

        success
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-pending-prompts
  "Get list of pending prompts awaiting human decision.
   Only relevant when prompt-mode is 'human'."
  [_]
  (if (swarm-addon-available?)
    (let [{:keys [success result error timed-out]}
          (ec/eval-elisp-with-timeout
           "(json-encode (hive-mcp-swarm-api-pending-prompts))" 5000)]
      (cond
        timed-out
        {:type "text"
         :text (json/write-str {:error "Pending prompts check timed out"
                                :status "timeout"})
         :isError true}

        success
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-respond-prompt
  "Send a response to a pending prompt from a specific slave.
   Use this to answer permission prompts when prompt-mode is 'human'."
  [{:keys [slave_id response]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (hive-mcp-swarm-api-respond-prompt \"%s\" \"%s\"))"
                        (v/escape-elisp-string slave_id)
                        (v/escape-elisp-string response))
          {:keys [success result error timed-out]}
          (ec/eval-elisp-with-timeout elisp 5000)]
      (cond
        timed-out
        {:type "text"
         :text (json/write-str {:error "Respond prompt timed out"
                                :status "timeout"
                                :slave_id slave_id})
         :isError true}

        success
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

;; ============================================================
;; JVM Process Cleanup (for swarm garbage collection)
;; ============================================================

(def parse-jvm-process-line
  "Parse a ps output line into a process map. Delegated to jvm module."
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
  "Get swarm environment variables for a process. Delegated to jvm module."
  jvm/get-process-swarm-info)

(def classify-jvm-process
  "Classify a JVM process by type and swarm status. Delegated to jvm module."
  jvm/classify-jvm-process)

(def handle-jvm-cleanup
  "Find and optionally kill orphaned JVM processes. Delegated to jvm module."
  jvm/handle-jvm-cleanup)

;; ============================================================
;; Resource Guard (Memory-based spawn protection)
;; ============================================================

(def get-memory-usage
  "Get current RAM usage. Delegated to jvm module."
  jvm/get-memory-usage)

(def handle-resource-guard
  "Check system resources and cleanup if needed. Delegated to jvm module."
  jvm/handle-resource-guard)

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
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
                                         :tasks (mapv #(select-keys % [:id :slave-id]) ready)})}))}])
