(ns emacs-mcp.tools.swarm
  "Swarm management and JVM resource cleanup tools.

   Handles Claude swarm slave spawning, dispatch, and lifecycle management.
   Also provides JVM process cleanup for garbage collecting orphaned processes.

   Push-based updates:
   When emacs-mcp.channel is available, subscribes to swarm events for
   sub-100ms task completion detection. Falls back to polling if channel
   not connected."
  (:require [emacs-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [emacs-mcp.emacsclient :as ec]
            [emacs-mcp.validation :as v]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop <! close!]]
            [taoensso.timbre :as log]))

;; ============================================================
;; Swarm Management Tools
;; ============================================================

;; ============================================================
;; Event Journal (Push-based task tracking)
;; ============================================================

;; In-memory journal of swarm events received via channel.
;; Maps task-id -> {:status :result :timestamp :slave-id}
(defonce ^:private event-journal (atom {}))

;; Active channel subscriptions (for cleanup).
(defonce ^:private channel-subscriptions (atom []))

(defn- try-require-channel
  "Attempt to require the channel namespace. Returns true if available."
  []
  (try
    (require 'emacs-mcp.channel)
    true
    (catch Exception _
      false)))

(defn- channel-subscribe!
  "Subscribe to an event type if channel is available.
   Returns the subscription channel or nil."
  [event-type]
  (when (try-require-channel)
    (when-let [subscribe-fn (resolve 'emacs-mcp.channel/subscribe!)]
      (subscribe-fn event-type))))

(defn- handle-task-completed
  "Handle task-completed event from channel.
   Note: bencode returns string keys, so we use get instead of keywords."
  [event]
  (let [task-id (get event "task-id")
        slave-id (get event "slave-id")
        result (get event "result")
        timestamp (get event "timestamp")]
    (log/info "Channel: task-completed" task-id "from" slave-id)
    (swap! event-journal assoc (str task-id)
           {:status "completed"
            :result result
            :slave-id slave-id
            :timestamp (or timestamp (System/currentTimeMillis))})))

(defn- handle-task-failed
  "Handle task-failed event from channel.
   Note: bencode returns string keys, so we use get instead of keywords."
  [event]
  (let [task-id (get event "task-id")
        slave-id (get event "slave-id")
        error (get event "error")
        timestamp (get event "timestamp")]
    (log/info "Channel: task-failed" task-id "from" slave-id ":" error)
    (swap! event-journal assoc (str task-id)
           {:status "failed"
            :error error
            :slave-id slave-id
            :timestamp (or timestamp (System/currentTimeMillis))})))

(defn- handle-prompt-shown
  "Handle prompt-shown event from channel.
   Note: bencode returns string keys, so we use get instead of keywords."
  [event]
  (let [slave-id (get event "slave-id")
        prompt (get event "prompt")
        timestamp (get event "timestamp")]
    (log/info "Channel: prompt-shown from" slave-id)
    ;; For now just log - could add to a prompts journal if needed
    ))

(defn start-channel-subscriptions!
  "Start listening for swarm events via channel.
   Called at startup if channel is available."
  []
  (when (try-require-channel)
    (log/info "Starting channel subscriptions for swarm events...")

    ;; Subscribe to task-completed
    (when-let [sub (channel-subscribe! :task-completed)]
      (swap! channel-subscriptions conj sub)
      (go-loop []
        (when-let [event (<! sub)]
          (handle-task-completed event)
          (recur))))

    ;; Subscribe to task-failed
    (when-let [sub (channel-subscribe! :task-failed)]
      (swap! channel-subscriptions conj sub)
      (go-loop []
        (when-let [event (<! sub)]
          (handle-task-failed event)
          (recur))))

    ;; Subscribe to prompt-shown
    (when-let [sub (channel-subscribe! :prompt-shown)]
      (swap! channel-subscriptions conj sub)
      (go-loop []
        (when-let [event (<! sub)]
          (handle-prompt-shown event)
          (recur))))

    (log/info "Channel subscriptions started")))

(defn stop-channel-subscriptions!
  "Stop all channel subscriptions."
  []
  (doseq [sub @channel-subscriptions]
    (close! sub))
  (reset! channel-subscriptions [])
  (log/info "Channel subscriptions stopped"))

(defn check-event-journal
  "Check event journal for task completion.
   Returns the event if found, nil otherwise."
  [task-id]
  (get @event-journal (str task-id)))

(defn clear-event-journal!
  "Clear all entries from the event journal."
  []
  (reset! event-journal {}))

;; ============================================================
;; Swarm Addon Check
;; ============================================================

(defn swarm-addon-available?
  "Check if emacs-mcp-swarm addon is loaded.
   Uses short timeout (2s) to fail fast if Emacs is unresponsive."
  []
  (let [{:keys [success result timed-out]} (ec/eval-elisp-with-timeout "(featurep 'emacs-mcp-swarm)" 2000)]
    (and success (not timed-out) (= result "t"))))

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance.
   Uses timeout to prevent MCP blocking."
  [{:keys [name presets cwd role terminal]}]
  (if (swarm-addon-available?)
    (let [presets-str (when (seq presets)
                        (format "'(%s)" (clojure.string/join " " (map #(format "\"%s\"" %) presets))))
          elisp (format "(json-encode (emacs-mcp-swarm-api-spawn \"%s\" %s %s %s))"
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
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded. Run (require 'emacs-mcp-swarm)" :isError true}))

(defn handle-swarm-dispatch
  "Dispatch a prompt to a slave.
   Uses timeout to prevent MCP blocking."
  [{:keys [slave_id prompt timeout_ms]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (emacs-mcp-swarm-api-dispatch \"%s\" \"%s\" %s))"
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
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-status
  "Get swarm status including all slaves and their states.
   Uses timeout to prevent MCP blocking."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if slave_id
                  (format "(json-encode (emacs-mcp-swarm-status \"%s\"))" slave_id)
                  "(json-encode (emacs-mcp-swarm-api-status))")
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
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-collect
  "Collect response from a task with push-first, poll-fallback strategy.

   PHASE 2 OPTIMIZATION: Check event journal first for sub-100ms detection.
   If event not in journal, falls back to elisp polling with exponential backoff.

   Note: emacsclient returns a quoted string, and json-encode creates another
   layer of JSON, so we need to parse twice to get the actual data."
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
              (let [elisp (format "(json-encode (emacs-mcp-swarm-api-collect \"%s\" %s))"
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

                  ;; Parse the result - need TWO parses:
                  ;; 1. First parse: emacsclient quotes the output -> get inner JSON string
                  ;; 2. Second parse: parse the actual JSON object
                  :else
                  (let [;; First parse: unwrap emacsclient quotes
                        json-str (try (json/read-str result) (catch Exception _ nil))
                        ;; Second parse: parse the actual JSON from elisp
                        parsed (when (string? json-str)
                                 (try (json/read-str json-str :key-fn keyword)
                                      (catch Exception _ nil)))
                        status (:status parsed)]
                    (cond
                      ;; Parse failed - log and return error
                      (nil? parsed)
                      {:type "text"
                       :text (json/write-str {:task_id task_id
                                              :status "error"
                                              :error "Failed to parse elisp response"
                                              :raw_result result
                                              :first_parse json-str
                                              :elapsed_ms elapsed})
                       :isError true}

                      ;; Task complete or failed - return the inner JSON directly
                      (contains? #{"completed" "timeout" "error"} status)
                      {:type "text" :text json-str}

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
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-list-presets
  "List available swarm presets.
   Uses timeout to prevent MCP blocking."
  [_]
  (if (swarm-addon-available?)
    (let [{:keys [success result error timed-out]} (ec/eval-elisp-with-timeout "(json-encode (emacs-mcp-swarm-api-list-presets))" 5000)]
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
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-kill
  "Kill a slave or all slaves.
   Uses short timeout (3s) as kill should be fast."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if (= slave_id "all")
                  "(json-encode (emacs-mcp-swarm-api-kill-all))"
                  (format "(json-encode (emacs-mcp-swarm-api-kill \"%s\"))"
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
        {:type "text" :text result}

        :else
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-broadcast
  "Broadcast a prompt to all slaves.
   Uses timeout to prevent MCP blocking."
  [{:keys [prompt]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (emacs-mcp-swarm-broadcast \"%s\"))"
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
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-pending-prompts
  "Get list of pending prompts awaiting human decision.
   Only relevant when prompt-mode is 'human'."
  [_]
  (if (swarm-addon-available?)
    (let [{:keys [success result error timed-out]}
          (ec/eval-elisp-with-timeout
           "(json-encode (emacs-mcp-swarm-api-pending-prompts))" 5000)]
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
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-respond-prompt
  "Send a response to a pending prompt from a specific slave.
   Use this to answer permission prompts when prompt-mode is 'human'."
  [{:keys [slave_id response]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (emacs-mcp-swarm-api-respond-prompt \"%s\" \"%s\"))"
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
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

;; ============================================================
;; JVM Process Cleanup (for swarm garbage collection)
;; ============================================================

(defn parse-jvm-process-line
  "Parse a ps output line into a process map."
  [line]
  (let [parts (str/split (str/trim line) #"\s+" 5)]
    (when (>= (count parts) 5)
      {:pid (first parts)
       :cpu (second parts)
       :mem (nth parts 2)
       :etime (nth parts 3)
       :cmd (nth parts 4)})))

(defn parse-etime-to-minutes
  "Parse elapsed time (format: [[DD-]HH:]MM:SS) to minutes."
  [etime]
  (try
    (let [parts (str/split etime #"[-:]")
          nums (map #(Integer/parseInt %) parts)]
      (case (count nums)
        2 (first nums) ; MM:SS -> minutes
        3 (+ (* 60 (first nums)) (second nums)) ; HH:MM:SS -> minutes
        4 (+ (* 24 60 (first nums)) (* 60 (second nums)) (nth nums 2)) ; DD-HH:MM:SS
        0))
    (catch Exception _ 0)))

(defn find-jvm-processes
  "Find all JVM processes with their details including parent info."
  []
  (try
    (let [result (shell/sh "ps" "-eo" "pid,ppid,pcpu,pmem,etime,args" "--no-headers")
          lines (str/split-lines (:out result))
          jvm-lines (filter #(re-find #"java" %) lines)]
      (keep (fn [line]
              (let [parts (str/split (str/trim line) #"\s+" 6)]
                (when (>= (count parts) 6)
                  {:pid (nth parts 0)
                   :ppid (nth parts 1)
                   :cpu (nth parts 2)
                   :mem (nth parts 3)
                   :etime (nth parts 4)
                   :cmd (nth parts 5)})))
            jvm-lines))
    (catch Exception e
      (log/error "Error finding JVM processes:" (.getMessage e))
      [])))

(defn get-all-process-parents
  "Get pid->{:ppid :comm} map for all processes in ONE ps call. Efficient!"
  []
  (try
    (let [result (shell/sh "ps" "-eo" "pid,ppid,comm" "--no-headers")
          lines (str/split-lines (:out result))]
      (into {}
            (keep (fn [line]
                    (let [parts (str/split (str/trim line) #"\s+" 3)]
                      (when (= 3 (count parts))
                        [(first parts) {:ppid (second parts) :comm (nth parts 2)}])))
                  lines)))
    (catch Exception _ {})))

(defn enrich-with-parent-info
  "Enrich process with parent info from pre-fetched map."
  [proc all-parents]
  (let [ppid (:ppid proc)
        parent (get all-parents ppid)
        parent-alive (boolean parent)
        parent-comm (:comm parent)
        is-init (= "1" ppid)
        is-claude (= "claude" parent-comm)
        truly-orphaned (or (not parent-alive) is-init)]
    (assoc proc
           :parent-alive parent-alive
           :parent-comm parent-comm
           :parent-is-claude is-claude
           :truly-orphaned truly-orphaned)))

(defn get-process-swarm-info
  "Get swarm environment variables from /proc/<pid>/environ.
   Returns nil if not a swarm-spawned process, or a map with swarm info."
  [pid]
  (try
    (let [environ-file (str "/proc/" pid "/environ")
          content (slurp environ-file)
          ;; environ file has null-separated entries
          entries (str/split content #"\x00")
          env-map (into {} (keep #(let [parts (str/split % #"=" 2)]
                                    (when (= 2 (count parts))
                                      [(first parts) (second parts)]))
                                 entries))
          slave-id (get env-map "CLAUDE_SWARM_SLAVE_ID")
          master-id (get env-map "CLAUDE_SWARM_MASTER")
          depth (get env-map "CLAUDE_SWARM_DEPTH")]
      (when (or slave-id master-id depth)
        {:swarm-slave-id slave-id
         :swarm-master-id master-id
         :swarm-depth (when depth (try (Integer/parseInt depth) (catch Exception _ nil)))}))
    (catch Exception _
      ;; Can't read environ (permission denied or process gone)
      nil)))

(defn classify-jvm-process
  "Classify a JVM process by type and swarm status (parent info added separately)."
  [{:keys [cmd pid] :as proc}]
  (let [swarm-info (get-process-swarm-info pid)
        proc-type (cond
                    (re-find #"shadow-cljs|shadow\.cljs" cmd) :shadow-cljs
                    (re-find #"emacs-mcp|emacs_mcp" cmd) :emacs-mcp
                    (re-find #"clojure-mcp|clj-mcp" cmd) :clojure-mcp
                    (re-find #"nrepl" cmd) :nrepl
                    (re-find #"leiningen" cmd) :leiningen
                    :else :other)]
    (-> proc
        (assoc :type proc-type)
        (assoc :swarm-spawned (boolean swarm-info))
        (merge swarm-info))))

(defn handle-jvm-cleanup
  "Find and optionally kill orphaned JVM processes.

   TRUE ORPHAN DETECTION:
   - Parent process is dead (not running)
   - Parent is PID 1 (reparented to init/systemd)

   EFFICIENT: Uses only 2 ps calls total (not O(n) per process).

   Keeps processes whose parent is a living Claude session."
  [{:keys [min_age_minutes dry_run keep_types swarm_only true_orphans_only]}]
  (try
    (let [min-age (or min_age_minutes 30)
          dry-run (if (nil? dry_run) true dry_run)
          keep-types-set (set (or keep_types ["shadow-cljs" "leiningen"]))
          swarm-only (boolean swarm_only)
          true-orphans-only (if (nil? true_orphans_only) true true_orphans_only)

          ;; EFFICIENT: Get all parent info in ONE call
          all-parents (get-all-process-parents)

          ;; Get JVM processes and classify
          all-procs (find-jvm-processes)
          classified (->> all-procs
                          (map classify-jvm-process)
                          (map #(enrich-with-parent-info % all-parents)))

          ;; Filter to swarm-only if requested
          working-procs (if swarm-only
                          (filter :swarm-spawned classified)
                          classified)

          ;; Swarm statistics
          swarm-procs (filter :swarm-spawned classified)
          by-slave (group-by :swarm-slave-id swarm-procs)

          ;; Group by type
          by-type (group-by :type working-procs)

          ;; Identify orphans based on detection mode
          identify-orphans (fn [procs]
                             (map (fn [p]
                                    (let [age (parse-etime-to-minutes (:etime p))
                                          protected-type (contains? keep-types-set (name (:type p)))
                                          truly-orphaned (:truly-orphaned p)
                                          age-orphaned (>= age min-age)
                                          is-orphan (cond
                                                      protected-type false
                                                      true-orphans-only truly-orphaned
                                                      :else (and truly-orphaned age-orphaned))
                                          reason (cond
                                                   protected-type "protected-type"
                                                   truly-orphaned (str "truly-orphaned (parent: "
                                                                       (or (:parent-comm p) "dead") ")")
                                                   (:parent-is-claude p) (str "managed-by-claude (ppid: "
                                                                              (:ppid p) ")")
                                                   :else (str "has-parent: " (:parent-comm p)))]
                                      (assoc p
                                             :orphan is-orphan
                                             :age-minutes age
                                             :reason reason)))
                                  procs))

          all-classified (identify-orphans working-procs)
          orphans (filter :orphan all-classified)
          managed (filter :parent-is-claude all-classified)

          ;; Kill orphans if not dry run
          killed-pids (when (and (not dry-run) (seq orphans))
                        (doseq [{:keys [pid]} orphans]
                          (try
                            (shell/sh "kill" pid)
                            (catch Exception e
                              (log/warn "Failed to kill PID" pid ":" (.getMessage e)))))
                        (map :pid orphans))

          summary {:total-jvm-processes (count all-procs)
                   :by-type (into {} (map (fn [[k v]] [(name k) (count v)]) by-type))
                   :swarm {:total-swarm-spawned (count swarm-procs)
                           :by-slave (into {} (map (fn [[k v]]
                                                     [(or k "unknown")
                                                      {:count (count v)
                                                       :pids (map :pid v)}])
                                                   by-slave))}
                   :orphan-detection {:mode (if true-orphans-only "true-orphans" "age-based")
                                      :truly-orphaned-count (count (filter :truly-orphaned all-classified))
                                      :managed-by-claude (count managed)}
                   :orphans-found (count orphans)
                   :orphan-pids (map :pid orphans)
                   :dry-run dry-run
                   :swarm-only-mode swarm-only
                   :killed (if dry-run [] (or killed-pids []))
                   :min-age-threshold min-age
                   :details (map #(select-keys % [:pid :ppid :type :etime :orphan :reason :age-minutes
                                                  :truly-orphaned :parent-alive :parent-comm :parent-is-claude
                                                  :swarm-spawned :swarm-slave-id :swarm-master-id :swarm-depth])
                                 all-classified)}]

      (mcp-json summary))
    (catch Exception e
      (mcp-error (str "Error during JVM cleanup: " (.getMessage e))))))

;; ============================================================
;; Resource Guard (Memory-based spawn protection)
;; ============================================================

(defn get-memory-usage
  "Get current RAM usage from /proc/meminfo. Returns {:total :used :available :percent-used}.
   Uses shell command instead of slurp due to JVM issues with procfs."
  []
  (try
    (let [{:keys [exit out]} (shell/sh "cat" "/proc/meminfo")]
      (if (zero? exit)
        (let [meminfo out
              parse-kb (fn [pattern]
                         (when-let [m (re-find (re-pattern (str pattern ":\\s+(\\d+)")) meminfo)]
                           (Long/parseLong (second m))))
              total-kb (parse-kb "MemTotal")
              available-kb (parse-kb "MemAvailable")
              used-kb (- total-kb available-kb)
              percent-used (double (* 100 (/ used-kb total-kb)))]
          {:total-mb (quot total-kb 1024)
           :used-mb (quot used-kb 1024)
           :available-mb (quot available-kb 1024)
           :percent-used (Math/round percent-used)})
        {:error "Failed to read /proc/meminfo"}))
    (catch Exception e
      {:error (.getMessage e)})))

(defn handle-resource-guard
  "Check system resources and automatically clean up orphaned JVMs if memory is high.

   WORKFLOW:
   1. Check current RAM usage
   2. If above threshold (default 80%), run jvm_cleanup automatically
   3. Re-check memory after cleanup
   4. Return spawn permission based on final memory state

   Use this BEFORE spawning new Claude swarm slaves to prevent OOM.

   Parameters:
   - ram_threshold: Percentage threshold (default 80)
   - min_available_mb: Minimum available RAM in MB (default 2048)
   - auto_cleanup: Whether to auto-run jvm_cleanup when high (default true)
   - cleanup_dry_run: If auto_cleanup, whether to actually kill (default false)"
  [{:keys [ram_threshold min_available_mb auto_cleanup cleanup_dry_run]}]
  (try
    (let [threshold (or ram_threshold 80)
          min-available (or min_available_mb 2048)
          auto-clean (if (nil? auto_cleanup) true auto_cleanup)
          cleanup-dry (if (nil? cleanup_dry_run) false cleanup_dry_run)

          ;; Initial memory check
          initial-mem (get-memory-usage)

          _ (when (:error initial-mem)
              (throw (Exception. (str "Cannot read memory: " (:error initial-mem)))))

          initial-high? (or (>= (:percent-used initial-mem) threshold)
                            (< (:available-mb initial-mem) min-available))

          ;; Auto cleanup if needed
          cleanup-result (when (and initial-high? auto-clean)
                           (log/info "Memory high (" (:percent-used initial-mem) "%), running jvm_cleanup...")
                           (handle-jvm-cleanup {:dry_run cleanup-dry
                                                :true_orphans_only true}))

          ;; Parse cleanup result
          cleanup-data (when cleanup-result
                         (try
                           (json/read-str (:text cleanup-result) :key-fn keyword)
                           (catch Exception _ nil)))

          orphans-killed (when cleanup-data
                           (count (:killed cleanup-data)))

          ;; Re-check memory after cleanup
          final-mem (if (and cleanup-data (pos? (or orphans-killed 0)))
                      (do
                        (Thread/sleep 500) ;; Wait for processes to fully exit
                        (get-memory-usage))
                      initial-mem)

          final-high? (or (>= (:percent-used final-mem) threshold)
                          (< (:available-mb final-mem) min-available))

          ;; Determine spawn permission
          can-spawn (not final-high?)

          summary {:can-spawn can-spawn
                   :memory {:initial initial-mem
                            :final final-mem
                            :threshold-percent threshold
                            :min-available-mb min-available}
                   :status (cond
                             (not initial-high?) :healthy
                             (and initial-high? (not final-high?)) :recovered-after-cleanup
                             :else :capacity-reached)
                   :cleanup (when cleanup-data
                              {:ran true
                               :dry-run cleanup-dry
                               :orphans-found (:orphans-found cleanup-data)
                               :killed (count (:killed cleanup-data))})
                   :recommendation (cond
                                     can-spawn "Safe to spawn new processes"
                                     (not auto-clean) "Memory high - consider enabling auto_cleanup"
                                     cleanup-dry "Memory high - set cleanup_dry_run=false to actually kill orphans"
                                     :else "Capacity reached - wait for running tasks to complete")}]

      (mcp-json summary))
    (catch Exception e
      (mcp-error (str "Resource guard error: " (.getMessage e))))))

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
    :description "Send a prompt to a slave Claude instance. Returns a task_id for tracking."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "ID of the slave to send prompt to"}
                               "prompt" {:type "string"
                                         :description "The prompt/task to send to the slave"}
                               "timeout_ms" {:type "integer"
                                             :description "Optional timeout in milliseconds"}}
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

   {:name "swarm_broadcast"
    :description "Send the same prompt to all active slaves simultaneously."
    :inputSchema {:type "object"
                  :properties {"prompt" {:type "string"
                                         :description "The prompt to broadcast to all slaves"}}
                  :required ["prompt"]}
    :handler handle-swarm-broadcast}

   {:name "swarm_pending_prompts"
    :description "Get list of pending prompts from slaves awaiting human decision. Only relevant when emacs-mcp-swarm-prompt-mode is 'human'."
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
    :handler handle-resource-guard}])
