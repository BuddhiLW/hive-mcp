(ns hive-mcp.tools.swarm.jvm
  "JVM process management and resource guard tools.

   Provides:
   - JVM process discovery and classification
   - Orphan detection (true orphans vs age-based)
   - Process cleanup with swarm awareness
   - Memory-based spawn protection (resource guard)

   Used by swarm orchestration to garbage collect orphaned JVM processes
   and prevent OOM conditions before spawning new agents."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.tools.swarm.jvm.parser :as parser]
            [hive-mcp.tools.swarm.jvm.orphan :as orphan]))



;; ============================================================
;; Process Discovery
;; ============================================================

(defn find-jvm-processes
  "Find all JVM processes with their details including parent info."
  []
  (try
    (let [result (shell/sh "ps" "-eo" "pid,ppid,pcpu,pmem,etime,args" "--no-headers")
          lines (str/split-lines (:out result))
          jvm-lines (filter #(re-find #"java" %) lines)]
      (keep (fn [line]
              (parser/parse-process-line-extended line))
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

;; ============================================================
;; Process Enrichment & Classification
;; ============================================================



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
                    (re-find #"hive-mcp|hive_mcp" cmd) :hive-mcp
                    (re-find #"clojure-mcp|clj-mcp" cmd) :clojure-mcp
                    (re-find #"nrepl" cmd) :nrepl
                    (re-find #"leiningen" cmd) :leiningen
                    :else :other)]
    (-> proc
        (assoc :type proc-type)
        (assoc :swarm-spawned (boolean swarm-info))
        (merge swarm-info))))

;; ============================================================
;; JVM Cleanup Handler
;; ============================================================

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
                          (map #(orphan/enrich-with-parent-info % all-parents)))

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
          all-classified (orphan/identify-orphans working-procs :protected-types keep-types-set :true-orphans-only true-orphans-only :min-age-minutes min-age)


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
