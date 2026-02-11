(ns hive-mcp.agent.headless
  "Headless ling process management - subprocess-based Claude Code instances."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.lang ProcessBuilder]
           [java.io BufferedReader InputStreamReader BufferedWriter OutputStreamWriter]
           [java.util.concurrent ConcurrentHashMap]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(declare dispatch-via-stdin!)
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const default-buffer-capacity
  "Default max lines in ring buffer."
  5000)

(defn create-ring-buffer
  "Create a bounded ring buffer for stdout/stderr capture."
  ([] (create-ring-buffer default-buffer-capacity))
  ([capacity]
   (atom {:lines []
          :timestamps []
          :capacity capacity
          :total-lines-seen 0
          :dropped 0})))

(defn ring-buffer-append!
  "Append a line to the ring buffer, dropping oldest if at capacity."
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
  "Get current ring buffer contents, optionally last N lines."
  ([buffer] (ring-buffer-contents buffer {}))
  ([buffer {:keys [last-n]}]
   (let [{:keys [lines]} @buffer]
     (if (and last-n (pos? last-n) (> (count lines) last-n))
       (subvec lines (- (count lines) last-n))
       lines))))

(defn ring-buffer-contents-since
  "Get ring buffer lines appended after a given timestamp."
  [buffer since]
  (let [{:keys [lines timestamps]} @buffer
        ts-vec (or timestamps [])
        n (count ts-vec)]
    (if (or (zero? n) (nil? since))
      (mapv (fn [line ts] {:text line :ts ts})
            lines
            (if (seq ts-vec) ts-vec (repeat (count lines) 0)))
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
  "Get ring buffer statistics."
  [buffer]
  (let [{:keys [lines capacity total-lines-seen dropped]} @buffer]
    {:current-lines (count lines)
     :capacity capacity
     :total-lines-seen total-lines-seen
     :dropped dropped}))

(defonce ^:private process-registry
  (ConcurrentHashMap.))

(defonce ^:private shutdown-hook-registered?
  (atom false))

(defn- register-shutdown-hook!
  "Register a JVM shutdown hook to kill all headless processes."
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

(defn- start-stream-reader!
  "Start a daemon thread reading lines from a stream into a ring buffer."
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
                      (log/debug (str "Headless " label " stream closed") {:ling-id ling-id}))
                    (catch Exception e
                      (log/warn (str "Headless " label " reader error")
                                {:ling-id ling-id :error (.getMessage e)}))))
                (str "hive-headless-" label "-" ling-id))]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn- build-command-parts
  "Build command parts for spawning a headless ling."
  [claude-cmd model task system-prompt]
  (let [claude-like? (str/includes? (or claude-cmd "claude") "claude")
        openrouter-model? (and model
                               (not= model "claude")
                               claude-like?)]
    (cond-> [(or claude-cmd "claude")]
      claude-like? (conj "-p")
      (and task claude-like?) (conj task)
      claude-like? (conj "--output-format" "stream-json" "--verbose")
      openrouter-model? (conj "--model" model)
      (and system-prompt claude-like?) (conj "--append-system-prompt" system-prompt)
      (and task (not claude-like?)) (conj task))))

(defn spawn-headless!
  "Spawn a headless ling subprocess via ProcessBuilder."
  [ling-id {:keys [cwd task presets model env-extra claude-cmd buffer-capacity system-prompt]
            :or {claude-cmd "claude"
                 buffer-capacity default-buffer-capacity}}]
  {:pre [(string? ling-id)
         (string? cwd)]}
  (register-shutdown-hook!)

  (when (.containsKey process-registry ling-id)
    (throw (ex-info "Headless ling already exists with this ID"
                    {:ling-id ling-id})))

  (let [cmd-parts (build-command-parts claude-cmd model task system-prompt)
        pb (ProcessBuilder. ^java.util.List (vec cmd-parts))
        _ (.directory pb (java.io.File. cwd))
        _ (.redirectErrorStream pb false)
        env (.environment pb)
        _ (when env-extra
            (doseq [[k v] env-extra]
              (.put env (name k) (str v))))
        _ (.put env "CLAUDE_SWARM_SLAVE_ID" ling-id)
        _ (when (and model (not= model "claude"))
            (.put env "OPENROUTER_MODEL" model))
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
        stdout-thread (start-stream-reader! (.getInputStream process)
                                            stdout-buf "stdout" ling-id)
        stderr-thread (start-stream-reader! (.getErrorStream process)
                                            stderr-buf "stderr" ling-id)
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
  "Send a task to a headless ling via its stdin pipe."
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
  "Terminate a headless ling process."
  ([ling-id] (kill-headless! ling-id {}))
  ([ling-id {:keys [force? timeout-ms] :or {timeout-ms 5000}}]
   (if-let [entry (.get process-registry ling-id)]
     (let [^Process process (:process entry)
           pid (:pid entry)
           ^BufferedWriter writer (:stdin-writer entry)]
       (log/info "Killing headless ling" {:ling-id ling-id :pid pid :force? force?})

       (try
         (.close writer)
         (catch Exception _))

       (if force?
         (.destroyForcibly process)
         (do
           (.destroy process)
           (when-not (.waitFor process timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
             (log/warn "Headless ling didn't exit gracefully, force-killing"
                       {:ling-id ling-id :pid pid})
             (.destroyForcibly process))))

       (let [exit-code (try (.waitFor process 5000 java.util.concurrent.TimeUnit/MILLISECONDS)
                            (.exitValue process)
                            (catch Exception _ -1))]
         (.remove process-registry ling-id)
         (log/info "Headless ling killed" {:ling-id ling-id :pid pid :exit-code exit-code})
         {:killed? true
          :ling-id ling-id
          :pid pid
          :exit-code exit-code}))
     (throw (ex-info "Headless ling not found in registry"
                     {:ling-id ling-id})))))

(defn headless-status
  "Get the status of a headless ling."
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
  "Get stdout contents of a headless ling."
  ([ling-id] (get-stdout ling-id {}))
  ([ling-id opts]
   (when-let [entry (.get process-registry ling-id)]
     (ring-buffer-contents (:stdout-buffer entry) opts))))

(defn get-stderr
  "Get stderr contents of a headless ling."
  ([ling-id] (get-stderr ling-id {}))
  ([ling-id opts]
   (when-let [entry (.get process-registry ling-id)]
     (ring-buffer-contents (:stderr-buffer entry) opts))))

(defn get-stdout-since
  "Get stdout lines appended after a given timestamp."
  [ling-id since]
  (when-let [entry (.get process-registry ling-id)]
    (ring-buffer-contents-since (:stdout-buffer entry) since)))

(defn list-headless
  "List all active headless ling processes."
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
  "Get the raw Process handle for a headless ling."
  [ling-id]
  (when-let [entry (.get process-registry ling-id)]
    (:process entry)))

(defn get-stdout-buffer
  "Get the raw stdout ring buffer atom for a headless ling."
  [ling-id]
  (when-let [entry (.get process-registry ling-id)]
    (:stdout-buffer entry)))

(defn get-stderr-buffer
  "Get the raw stderr ring buffer atom for a headless ling."
  [ling-id]
  (when-let [entry (.get process-registry ling-id)]
    (:stderr-buffer entry)))

(defn kill-all-headless!
  "Kill all active headless processes."
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
  ;; (spawn-headless! "test-ling-1" {:cwd "/home/user/project"})
  ;; (headless-status "test-ling-1")
  ;; (get-stdout "test-ling-1" {:last-n 50})
  ;; (dispatch-via-stdin! "test-ling-1" "Find all test files and list them")
  ;; (kill-headless! "test-ling-1")
  ;; (list-headless)
  )
