(ns hive-mcp.agent.headless
  "Headless ling process management - subprocess-based Claude Code instances.

   Enables ling spawning WITHOUT Emacs/vterm dependency by using
   java.lang.ProcessBuilder to launch `claude` CLI as a child process.

   Key components:
   - Ring buffer (bounded atom) for stdout/stderr capture
   - Process handle tracking for lifecycle management
   - Stdin pipe for task dispatch
   - JVM shutdown hook for cleanup

   This is the critical scale unlock - headless lings can run on any
   machine with the Claude CLI installed, no GUI required."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.lang ProcessBuilder]
           [java.io BufferedReader InputStreamReader BufferedWriter OutputStreamWriter]
           [java.util.concurrent ConcurrentHashMap]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; Forward declarations for functions referenced before definition
(declare dispatch-via-stdin!)
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Ring Buffer (Bounded Stdout/Stderr Capture)
;;; =============================================================================

(def ^:const default-buffer-capacity
  "Default max lines in ring buffer."
  5000)

(defn create-ring-buffer
  "Create a bounded ring buffer backed by an atom.

   The buffer stores the last `capacity` lines of output.
   When capacity is exceeded, oldest lines are dropped.

   Each line has a corresponding timestamp in the :timestamps vector
   for efficient time-based retrieval (e.g., 'get all lines since T').

   Returns:
     Atom containing {:lines (vector of strings)
                      :timestamps (vector of epoch-ms longs)
                      :capacity int
                      :total-lines-seen int
                      :dropped int}"
  ([] (create-ring-buffer default-buffer-capacity))
  ([capacity]
   (atom {:lines []
          :timestamps []
          :capacity capacity
          :total-lines-seen 0
          :dropped 0})))

(defn ring-buffer-append!
  "Append a line to the ring buffer, dropping oldest if at capacity.

   Each line is stored alongside a timestamp (epoch-ms) for time-based
   retrieval via ring-buffer-contents-since.

   Arguments:
     buffer - Ring buffer atom
     line   - String to append

   Returns:
     Updated buffer state"
  [buffer line]
  (let [ts (System/currentTimeMillis)]
    (swap! buffer
           (fn [{:keys [lines timestamps capacity total-lines-seen dropped] :as state}]
             (let [new-lines (conj lines line)
                   new-ts (conj (or timestamps []) ts)
                   over (- (count new-lines) capacity)]
               (if (pos? over)
                 (assoc state
                        :lines (subvec new-lines over)
                        :timestamps (subvec new-ts over)
                        :total-lines-seen (inc total-lines-seen)
                        :dropped (+ dropped over))
                 (assoc state
                        :lines new-lines
                        :timestamps new-ts
                        :total-lines-seen (inc total-lines-seen))))))))

(defn ring-buffer-contents
  "Get current ring buffer contents.

   Arguments:
     buffer - Ring buffer atom
     opts   - Optional map:
              :last-n - Only return last N lines (default: all)

   Returns:
     Vector of strings"
  ([buffer] (ring-buffer-contents buffer {}))
  ([buffer {:keys [last-n]}]
   (let [{:keys [lines]} @buffer]
     (if (and last-n (pos? last-n) (> (count lines) last-n))
       (subvec lines (- (count lines) last-n))
       lines))))

(defn ring-buffer-contents-since
  "Get ring buffer lines appended after a given timestamp.

   Uses binary search on the timestamps vector for O(log n) lookup.
   Returns entries as maps with :text and :ts for the client to track
   cursor position for subsequent polling.

   Arguments:
     buffer - Ring buffer atom
     since  - Epoch milliseconds; returns lines with ts > since

   Returns:
     Vector of {:text string :ts epoch-ms} maps"
  [buffer since]
  (let [{:keys [lines timestamps]} @buffer
        ts-vec (or timestamps [])
        n (count ts-vec)]
    (if (or (zero? n) (nil? since))
      ;; No timestamps or no since — return all with timestamps
      (mapv (fn [line ts] {:text line :ts ts})
            lines
            (if (seq ts-vec) ts-vec (repeat (count lines) 0)))
      ;; Binary search for first index where ts > since
      (let [idx (loop [lo 0 hi n]
                  (if (>= lo hi)
                    lo
                    (let [mid (quot (+ lo hi) 2)]
                      (if (<= (nth ts-vec mid) since)
                        (recur (inc mid) hi)
                        (recur lo mid)))))]
        (if (>= idx n)
          []
          (mapv (fn [i] {:text (nth lines i) :ts (nth ts-vec i)})
                (range idx n)))))))

(defn ring-buffer-stats
  "Get ring buffer statistics.

   Returns:
     {:current-lines N :capacity N :total-lines-seen N :dropped N}"
  [buffer]
  (let [{:keys [lines capacity total-lines-seen dropped]} @buffer]
    {:current-lines (count lines)
     :capacity capacity
     :total-lines-seen total-lines-seen
     :dropped dropped}))

;;; =============================================================================
;;; Process Registry (Track all headless processes for cleanup)
;;; =============================================================================

(defonce ^{:private true
           :doc "Registry of active headless processes.
   Key: ling-id (String)
   Value: {:process Process
           :stdout-buffer ring-buffer-atom
           :stderr-buffer ring-buffer-atom
           :stdin-writer BufferedWriter
           :stdout-reader-thread Thread
           :stderr-reader-thread Thread
           :cwd String
           :started-at long
           :pid long}"}
  process-registry
  (ConcurrentHashMap.))

(defonce ^{:private true
           :doc "Flag to track whether shutdown hook is registered."}
  shutdown-hook-registered?
  (atom false))

(defn- register-shutdown-hook!
  "Register a JVM shutdown hook to kill all headless processes.
   Idempotent - only registers once."
  []
  (when (compare-and-set! shutdown-hook-registered? false true)
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      (fn []
        (log/info "JVM shutdown hook: killing" (.size process-registry) "headless processes")
        (doseq [[ling-id entry] process-registry]
          (try
            (when-let [^Process process (:process entry)]
              (when (.isAlive process)
                (log/info "Killing headless process" {:ling-id ling-id :pid (.pid process)})
                (.destroyForcibly process)))
            (catch Exception e
              (log/warn "Failed to kill headless process on shutdown"
                        {:ling-id ling-id :error (.getMessage e)})))))
      "hive-headless-shutdown-hook"))
    (log/info "Registered JVM shutdown hook for headless processes")))

;;; =============================================================================
;;; Stream Reader Threads
;;; =============================================================================

(defn- start-stream-reader!
  "Start a daemon thread that reads lines from a stream into a ring buffer.

   Arguments:
     stream      - InputStream to read from
     buffer      - Ring buffer atom to write to
     label       - Label for logging (e.g., 'stdout', 'stderr')
     ling-id     - Ling ID for logging context

   Returns:
     The daemon Thread (already started)"
  [stream buffer label ling-id]
  (let [reader (BufferedReader. (InputStreamReader. stream))
        thread (Thread.
                (fn []
                  (try
                    (loop []
                      (when-let [line (.readLine reader)]
                        (ring-buffer-append! buffer line)
                        (when (str/includes? line "error")
                          (log/debug (str "Headless " label " [" ling-id "]: " line)))
                        (recur)))
                    (log/debug (str "Headless " label " reader exited") {:ling-id ling-id})
                    (catch java.io.IOException _
                      ;; Stream closed - normal during process termination
                      (log/debug (str "Headless " label " stream closed") {:ling-id ling-id}))
                    (catch Exception e
                      (log/warn (str "Headless " label " reader error")
                                {:ling-id ling-id :error (.getMessage e)}))))
                (str "hive-headless-" label "-" ling-id))]
    (.setDaemon thread true)
    (.start thread)
    thread))

;;; =============================================================================
;;; Process Lifecycle
;;; =============================================================================

(defn- build-command-parts
  "Build the command parts for spawning a headless ling.

   For Claude models (or nil model): uses claude CLI with --print flag.
   For OpenRouter models: uses claude CLI with --print and --model flags.
   For test overrides (claude-cmd like 'cat'): uses the override directly.

   The task is passed as a positional argument (not stdin) because
   claude --print reads ALL of stdin until EOF before processing.
   Keeping stdin open for future dispatches would cause it to hang.

   L2 Phase 3 (P3-T4): When system-prompt is provided, appends it to the
   Claude Code built-in system prompt via --append-system-prompt flag.
   This replaces the previous pattern of prepending L2 context to the task.

   Arguments:
     claude-cmd    - Override command (e.g., 'cat' for tests)
     model         - Model identifier (nil or 'claude' for default Claude Code CLI)
     task          - Initial task string (appended as positional arg for claude)
     system-prompt - Optional text to append to the system prompt (L2 context)

   Returns:
     Vector of command parts"
  [claude-cmd model task system-prompt]
  (let [claude-like? (str/includes? (or claude-cmd "claude") "claude")
        ;; Determine if this is a non-claude OpenRouter model
        openrouter-model? (and model
                               (not= model "claude")
                               claude-like?)]
    (cond-> [(or claude-cmd "claude")]
      ;; Add -p for non-interactive mode (same as --print)
      claude-like? (conj "-p")
      ;; Add task as positional argument (claude -p "task")
      (and task claude-like?) (conj task)
      ;; Add --output-format stream-json for streaming output to ring buffer
      claude-like? (conj "--output-format" "stream-json" "--verbose")
      ;; Add --model for non-default model selection (Claude Code CLI supports --model)
      openrouter-model? (conj "--model" model)
      ;; L2 Phase 3: Append L2 context to system prompt (not task prefix)
      ;; Uses --append-system-prompt to preserve Claude Code's built-in system prompt
      (and system-prompt claude-like?) (conj "--append-system-prompt" system-prompt)
      ;; For non-claude commands (e.g., 'cat' in tests), just append task
      (and task (not claude-like?)) (conj task))))

(defn spawn-headless!
  "Spawn a headless ling subprocess.

   Uses ProcessBuilder to launch a CLI process. The subprocess inherits
   environment variables and can auto-discover MCP servers via .mcp.json.

   Multi-model support:
   - Claude models (nil/'claude'): launches `claude --print`
   - OpenRouter models: launches `claude --print --model <model-id>`
   - Test overrides (claude-cmd): launches the override command directly

   Arguments:
     ling-id - Unique identifier for this ling
     opts    - Map with:
               :cwd        - Working directory (required)
               :task        - Initial task to send via stdin (optional)
               :presets     - Preset names (passed as --presets flag, optional)
               :model       - Model identifier (nil/'claude' for Claude Code,
                              or OpenRouter model ID like 'deepseek/deepseek-v3.2')
               :env-extra   - Extra environment variables map (optional)
               :claude-cmd  - Override claude command (default: 'claude')
               :buffer-capacity - Ring buffer size (default: 5000)
               :system-prompt - Text to append to system prompt via
                                --append-system-prompt (L2 context, optional)

   Returns:
     Map with:
       :ling-id    - The ling ID
       :pid        - OS process ID
       :process    - java.lang.Process handle
       :stdout-buf - Stdout ring buffer atom
       :stderr-buf - Stderr ring buffer atom
       :model      - Model used for this ling

   Throws:
     ExceptionInfo on failure to start process"
  [ling-id {:keys [cwd task presets model env-extra claude-cmd buffer-capacity system-prompt]
            :or {claude-cmd "claude"
                 buffer-capacity default-buffer-capacity}}]
  {:pre [(string? ling-id)
         (string? cwd)]}
  ;; Ensure shutdown hook is registered
  (register-shutdown-hook!)

  ;; Check for duplicate
  (when (.containsKey process-registry ling-id)
    (throw (ex-info "Headless ling already exists with this ID"
                    {:ling-id ling-id})))

  (let [;; Build command using helper for multi-model support
        ;; L2 Phase 3: system-prompt flows to --append-system-prompt CLI flag
        cmd-parts (build-command-parts claude-cmd model task system-prompt)

        ;; Create ProcessBuilder
        pb (ProcessBuilder. ^java.util.List (vec cmd-parts))
        _ (.directory pb (java.io.File. cwd))
        _ (.redirectErrorStream pb false)

        ;; Merge extra env vars
        env (.environment pb)
        _ (when env-extra
            (doseq [[k v] env-extra]
              (.put env (name k) (str v))))

        ;; Set CLAUDE_SWARM_SLAVE_ID for the subprocess
        _ (.put env "CLAUDE_SWARM_SLAVE_ID" ling-id)

        ;; Set OPENROUTER_MODEL for non-claude models (used by some wrappers)
        _ (when (and model (not= model "claude"))
            (.put env "OPENROUTER_MODEL" model))

        ;; Start the process
        _ (log/info "Spawning headless ling" {:ling-id ling-id
                                              :cwd cwd
                                              :model (or model "claude")
                                              :cmd (str/join " " cmd-parts)})
        process (try
                  (.start pb)
                  (catch Exception e
                    (throw (ex-info "Failed to start headless ling process"
                                    {:ling-id ling-id
                                     :cwd cwd
                                     :model model
                                     :cmd cmd-parts
                                     :error (.getMessage e)}
                                    e))))

        pid (.pid process)
        stdout-buf (create-ring-buffer buffer-capacity)
        stderr-buf (create-ring-buffer buffer-capacity)

        ;; Start stream reader threads
        stdout-thread (start-stream-reader! (.getInputStream process)
                                            stdout-buf "stdout" ling-id)
        stderr-thread (start-stream-reader! (.getErrorStream process)
                                            stderr-buf "stderr" ling-id)

        ;; Create stdin writer
        stdin-writer (BufferedWriter.
                      (OutputStreamWriter.
                       (.getOutputStream process)))

        entry {:process process
               :pid pid
               :stdout-buffer stdout-buf
               :stderr-buffer stderr-buf
               :stdin-writer stdin-writer
               :stdout-reader-thread stdout-thread
               :stderr-reader-thread stderr-thread
               :cwd cwd
               :model model
               :started-at (System/currentTimeMillis)}]

    ;; Register in process registry
    (.put process-registry ling-id entry)

    (log/info "Headless ling spawned" {:ling-id ling-id :pid pid :cwd cwd
                                       :model (or model "claude")})

    {:ling-id ling-id
     :pid pid
     :process process
     :stdout-buf stdout-buf
     :stderr-buf stderr-buf
     :model model}))

(defn dispatch-via-stdin!
  "Send a task to a headless ling via its stdin pipe.

   The Claude CLI in --print mode reads from stdin.
   Each message is sent as a line followed by a newline.

   Arguments:
     ling-id - ID of the headless ling
     message - String message to send

   Returns:
     true on success

   Throws:
     ExceptionInfo if ling not found or write fails"
  [ling-id message]
  {:pre [(string? ling-id)
         (string? message)]}
  (if-let [entry (.get process-registry ling-id)]
    (let [^BufferedWriter writer (:stdin-writer entry)
          ^Process process (:process entry)]
      (if (.isAlive process)
        (try
          (.write writer ^String message)
          (.newLine writer)
          (.flush writer)
          (log/debug "Dispatched to headless ling via stdin"
                     {:ling-id ling-id
                      :message-length (count message)})
          true
          (catch java.io.IOException e
            (throw (ex-info "Failed to write to headless ling stdin"
                            {:ling-id ling-id
                             :error (.getMessage e)}
                            e))))
        (throw (ex-info "Headless ling process is not alive"
                        {:ling-id ling-id
                         :pid (.pid process)}))))
    (throw (ex-info "Headless ling not found in registry"
                    {:ling-id ling-id}))))

(defn kill-headless!
  "Terminate a headless ling process.

   Attempts graceful shutdown first (SIGTERM via destroy),
   then forceful (SIGKILL via destroyForcibly) after timeout.

   Arguments:
     ling-id    - ID of the headless ling
     opts       - Optional map:
                  :force?     - Skip graceful and force-kill immediately
                  :timeout-ms - Graceful shutdown timeout (default: 5000)

   Returns:
     {:killed? true :ling-id ling-id :pid pid :exit-code N}

   Throws:
     ExceptionInfo if ling not found"
  ([ling-id] (kill-headless! ling-id {}))
  ([ling-id {:keys [force? timeout-ms] :or {timeout-ms 5000}}]
   (if-let [entry (.get process-registry ling-id)]
     (let [^Process process (:process entry)
           pid (:pid entry)
           ^BufferedWriter writer (:stdin-writer entry)]
       (log/info "Killing headless ling" {:ling-id ling-id :pid pid :force? force?})

       ;; Close stdin first to signal EOF
       (try
         (.close writer)
         (catch Exception _))

       (if force?
         ;; Force kill immediately
         (.destroyForcibly process)
         ;; Graceful: destroy, wait, then force if needed
         (do
           (.destroy process)
           (when-not (.waitFor process timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
             (log/warn "Headless ling didn't exit gracefully, force-killing"
                       {:ling-id ling-id :pid pid})
             (.destroyForcibly process))))

       ;; Wait for exit and clean up
       (let [exit-code (try (.waitFor process 5000 java.util.concurrent.TimeUnit/MILLISECONDS)
                            (.exitValue process)
                            (catch Exception _ -1))]
         ;; Remove from registry
         (.remove process-registry ling-id)

         (log/info "Headless ling killed" {:ling-id ling-id :pid pid :exit-code exit-code})

         {:killed? true
          :ling-id ling-id
          :pid pid
          :exit-code exit-code}))
     (throw (ex-info "Headless ling not found in registry"
                     {:ling-id ling-id})))))

(defn headless-status
  "Get the status of a headless ling.

   Arguments:
     ling-id - ID of the headless ling

   Returns:
     Map with process info, buffer stats, and liveness, or nil if not found"
  [ling-id]
  (when-let [entry (.get process-registry ling-id)]
    (let [^Process process (:process entry)
          alive? (.isAlive process)]
      {:ling-id ling-id
       :pid (:pid entry)
       :alive? alive?
       :exit-code (when-not alive?
                    (try (.exitValue process) (catch Exception _ nil)))
       :cwd (:cwd entry)
       :model (:model entry)
       :started-at (:started-at entry)
       :uptime-ms (- (System/currentTimeMillis) (:started-at entry))
       :stdout (ring-buffer-stats (:stdout-buffer entry))
       :stderr (ring-buffer-stats (:stderr-buffer entry))})))

(defn get-stdout
  "Get stdout contents of a headless ling.

   Arguments:
     ling-id - ID of the headless ling
     opts    - Optional map:
               :last-n - Only return last N lines

   Returns:
     Vector of strings, or nil if ling not found"
  ([ling-id] (get-stdout ling-id {}))
  ([ling-id opts]
   (when-let [entry (.get process-registry ling-id)]
     (ring-buffer-contents (:stdout-buffer entry) opts))))

(defn get-stderr
  "Get stderr contents of a headless ling.

   Arguments:
     ling-id - ID of the headless ling
     opts    - Optional map:
               :last-n - Only return last N lines

   Returns:
     Vector of strings, or nil if ling not found"
  ([ling-id] (get-stderr ling-id {}))
  ([ling-id opts]
   (when-let [entry (.get process-registry ling-id)]
     (ring-buffer-contents (:stderr-buffer entry) opts))))

(defn get-stdout-since
  "Get stdout lines appended after a given timestamp.

   Efficient incremental retrieval for streaming/polling use cases.
   Returns entries with timestamps so the caller can use the last
   timestamp as the cursor for the next call.

   Arguments:
     ling-id - ID of the headless ling
     since   - Epoch milliseconds; returns lines with ts > since.
               Pass 0 or nil to get all lines.

   Returns:
     Vector of {:text string :ts epoch-ms} maps, or nil if ling not found"
  [ling-id since]
  (when-let [entry (.get process-registry ling-id)]
    (ring-buffer-contents-since (:stdout-buffer entry) since)))

;;; =============================================================================
;;; Registry Queries
;;; =============================================================================

(defn list-headless
  "List all active headless ling processes.

   Returns:
     Vector of status maps (see headless-status)"
  []
  (->> process-registry
       (.keySet)
       (map headless-status)
       (remove nil?)
       vec))

(defn headless-count
  "Get count of active headless processes."
  []
  (.size process-registry))

(defn headless?
  "Check if a ling-id corresponds to a headless process."
  [ling-id]
  (.containsKey process-registry ling-id))

(defn get-process
  "Get the raw Process handle for a headless ling.
   Mainly for testing and advanced use cases.

   Returns:
     java.lang.Process or nil"
  [ling-id]
  (when-let [entry (.get process-registry ling-id)]
    (:process entry)))

(defn get-stdout-buffer
  "Get the raw stdout ring buffer atom for a headless ling.
   Useful for direct buffer access in tests.

   Returns:
     Ring buffer atom or nil"
  [ling-id]
  (when-let [entry (.get process-registry ling-id)]
    (:stdout-buffer entry)))

(defn get-stderr-buffer
  "Get the raw stderr ring buffer atom for a headless ling.

   Returns:
     Ring buffer atom or nil"
  [ling-id]
  (when-let [entry (.get process-registry ling-id)]
    (:stderr-buffer entry)))

;;; =============================================================================
;;; Cleanup
;;; =============================================================================

(defn kill-all-headless!
  "Kill all active headless processes. Used for testing and cleanup.

   Returns:
     {:killed count :errors count}"
  []
  (let [ids (vec (.keySet process-registry))
        results (for [id ids]
                  (try
                    (kill-headless! id {:force? true})
                    {:success true :id id}
                    (catch Exception e
                      {:success false :id id :error (.getMessage e)})))]
    {:killed (count (filter :success results))
     :errors (count (remove :success results))}))

(comment
  ;; Usage examples

  ;; Spawn a headless ling
  ;; (spawn-headless! "test-ling-1" {:cwd "/home/user/project"})

  ;; Check status
  ;; (headless-status "test-ling-1")

  ;; Get stdout
  ;; (get-stdout "test-ling-1" {:last-n 50})

  ;; Dispatch a task
  ;; (dispatch-via-stdin! "test-ling-1" "Find all test files and list them")

  ;; Kill
  ;; (kill-headless! "test-ling-1")

  ;; List all
  ;; (list-headless)
  )