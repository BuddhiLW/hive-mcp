(ns hive-mcp.emacsclient
  "Shell wrapper for emacsclient communication with running Emacs.

   CLARITY-Y: Yield safe failure — daemon crash detection, circuit breaker,
   and structured telemetry prevent cascading failures when Emacs dies.
   CLARITY-T: Telemetry first — every crash/recovery event is logged with
   structured data for observability."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [clojure.lang IDeref]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *emacsclient-path*
  "Path to emacsclient binary."
  (or (System/getenv "EMACSCLIENT") "emacsclient"))

(def ^:dynamic *emacs-socket-name*
  "Emacs daemon socket name. When set, emacsclient calls include `-s <name>`.
   Reads from EMACS_SOCKET_NAME env var. nil = default daemon (no -s flag)."
  (System/getenv "EMACS_SOCKET_NAME"))

(def ^:dynamic *default-timeout-ms*
  "Default timeout for emacsclient calls in milliseconds."
  5000)

(def ^:dynamic *max-timeout-ms*
  "Hard ceiling for any emacsclient call. The client layer is the last line of
   defense — no call may exceed this regardless of what callers request."
  30000)

;;; =============================================================================
;;; Crash Telemetry — CLARITY-T + CLARITY-Y
;;; =============================================================================

(def ^:private daemon-dead-patterns
  "Stderr patterns from emacsclient that indicate daemon death.
   Each is [regex-pattern keyword-tag] for structured telemetry."
  [[#"(?i)can't find socket"           :socket-not-found]
   [#"(?i)No such file or directory"   :socket-missing]
   [#"(?i)Connection refused"          :connection-refused]
   [#"(?i)connection reset"            :connection-reset]
   [#"(?i)server did not respond"      :server-unresponsive]
   [#"(?i)socket.*not available"       :socket-unavailable]])

(defonce ^{:private true
           :doc "Circuit breaker state for emacsclient calls.
   :alive?        — false when daemon is confirmed dead
   :tripped-at    — System/currentTimeMillis when circuit opened
   :crash-count   — total crash detections since JVM start
   :last-error    — last daemon-death stderr string
   :last-tag      — last daemon-death keyword tag
   :recovery-at   — last time circuit was re-closed"}
  circuit-breaker
  (atom {:alive? true
         :tripped-at nil
         :crash-count 0
         :last-error nil
         :last-tag nil
         :recovery-at nil}))

(def ^:dynamic *circuit-breaker-cooldown-ms*
  "Minimum time (ms) the circuit stays open before a recovery probe is allowed.
   Prevents thundering-herd probes right after a crash."
  5000)

;;; =============================================================================
;;; Daemon Death Detection & Error Reporting (IEmacsDaemon Integration)
;;; =============================================================================

(defn- detect-daemon-death
  "Check if an error string indicates daemon death.
   Returns [matched? tag] where tag is the specific death pattern matched."
  [error-str]
  (when (string? error-str)
    (some (fn [[pattern tag]]
            (when (re-find pattern error-str)
              [true tag]))
          daemon-dead-patterns)))

(defn- report-daemon-error!
  "Report a daemon error to the daemon store.
   Uses requiring-resolve for lazy loading to avoid cyclic dependencies.

   This is the integration point between emacsclient circuit breaker
   and the IEmacsDaemon lifecycle tracking system."
  [error-message death-tag]
  (try
    ;; Use requiring-resolve for lazy loading
    (when-let [mark-error! (requiring-resolve 'hive-mcp.emacs.daemon-store/mark-error!)]
      (when-let [default-daemon-id (requiring-resolve 'hive-mcp.emacs.daemon-store/default-daemon-id)]
        (let [daemon-id (default-daemon-id)]
          (mark-error! daemon-id (str "[" (name death-tag) "] " error-message))
          (log/info :daemon-error-reported {:daemon-id daemon-id
                                            :death-tag death-tag
                                            :message error-message}))))
    (catch Exception e
      ;; Don't fail the operation if reporting fails
      (log/debug "Could not report daemon error to store:" (.getMessage e)))))

(defn- unwrap-emacs-string
  "Unwrap emacsclient print format quoting.
   Emacs wraps string results in quotes: \"foo\" -> \"\\\"foo\\\"\"
   This undoes that wrapping for JSON and other string results."
  [s]
  (if (and (string? s)
           (>= (count s) 2)
           (str/starts-with? s "\"")
           (str/ends-with? s "\""))
    ;; Parse the outer quotes away using Clojure reader
    (try
      (read-string s)
      (catch Exception _ s))
    s))

(defn eval-elisp-with-timeout
  "Execute elisp code with a timeout. Returns immediately if the operation
   takes longer than timeout-ms milliseconds.
   Clamps timeout-ms to *max-timeout-ms* (30s) as a hard ceiling.

   IEmacsDaemon integration: On daemon death detection (matching daemon-dead-patterns),
   reports the error to the daemon store for lifecycle tracking.

   Returns a map with :success, :result or :error keys.
   On timeout, returns {:success false :error \"Timeout...\" :timed-out true}"
  ([code] (eval-elisp-with-timeout code *default-timeout-ms*))
  ([code timeout-ms]
   (let [timeout-ms (min (or timeout-ms *default-timeout-ms*) *max-timeout-ms*)
         _          (log/debug "Executing elisp with timeout:" timeout-ms "ms -" code)
         start      (System/currentTimeMillis)
         f          (future
                      (try
                        (let [{:keys [exit out err]} (apply sh (cond-> [*emacsclient-path*]
                                                                 *emacs-socket-name* (conj "-s" *emacs-socket-name*)
                                                                 true (conj "--eval" code)))]
                          (if (zero? exit)
                            {:success true :result (unwrap-emacs-string (str/trim out))}
                            {:success false :error (str/trim err)}))
                        (catch Exception e
                          {:success false :error (str "Failed to execute emacsclient: " (.getMessage e))})))]
     (try
       (let [result (deref f timeout-ms ::timeout)
             duration (- (System/currentTimeMillis) start)]
         (if (= result ::timeout)
           (do
             (future-cancel f)
             (log/warn :emacsclient-timeout {:timeout-ms timeout-ms
                                             :code-preview (subs code 0 (min 100 (count code)))})
             {:success false
              :error (format "Emacsclient call timed out after %dms" timeout-ms)
              :timed-out true
              :duration-ms duration})
           (do
             (if (:success result)
               (log/debug :emacsclient-success {:duration-ms duration})
               (do
                 (log/warn :emacsclient-failure {:duration-ms duration :error (:error result)})
                 ;; IEmacsDaemon integration: detect and report daemon death
                 (when-let [[_ death-tag] (detect-daemon-death (:error result))]
                   (log/warn :daemon-death-detected {:tag death-tag :error (:error result)})
                   (report-daemon-error! (:error result) death-tag)
                   (swap! circuit-breaker assoc
                          :alive? false
                          :tripped-at (System/currentTimeMillis)
                          :crash-count (inc (:crash-count @circuit-breaker))
                          :last-error (:error result)
                          :last-tag death-tag))))
             (assoc result :duration-ms duration))))
       (catch Exception e
         (let [duration (- (System/currentTimeMillis) start)]
           (log/error :emacsclient-exception {:duration-ms duration :exception (.getMessage e)} e)
           {:success false
            :error (str "Exception during emacsclient call: " (.getMessage e))
            :duration-ms duration}))))))

(defn eval-elisp
  "Execute elisp code in running Emacs and return the result.
   Returns a map with :success, :result or :error keys.
   Includes timing information for observability.
   Enforces *default-timeout-ms* (clamped to *max-timeout-ms*) automatically."
  [code]
  (eval-elisp-with-timeout code *default-timeout-ms*))

(defn eval-elisp!
  "Execute elisp and return result string, or throw on non-timeout error.
   On timeout, returns {:error :timeout :msg \"...\"}  instead of throwing,
   so callers can degrade gracefully (CLARITY-Y)."
  [code]
  (let [{:keys [success result error timed-out]} (eval-elisp code)]
    (cond
      success           result
      timed-out         {:error :timeout :msg error}
      :else             (throw (ex-info "Elisp evaluation failed"
                                        {:error error :code code})))))

(defn emacs-running?
  "Check if Emacs server is running. Returns false on timeout."
  []
  (:success (eval-elisp-with-timeout "t" 2000)))

;; Convenience functions for common operations

(defn buffer-list
  "Get list of buffer names."
  []
  (eval-elisp! "(mapcar #'buffer-name (buffer-list))"))

(defn current-buffer
  "Get current buffer name."
  []
  (eval-elisp! "(buffer-name)"))

(defn current-file
  "Get current file path, or nil if buffer is not visiting a file."
  []
  (let [result (eval-elisp! "(buffer-file-name)")]
    (when (not= result "nil")
      result)))

(defn buffer-content
  "Get content of a buffer by name."
  [buffer-name]
  (eval-elisp! (format "(with-current-buffer \"%s\" (buffer-string))" buffer-name)))

(defn switch-to-buffer
  "Switch to a buffer by name."
  [buffer-name]
  (eval-elisp! (format "(switch-to-buffer \"%s\")" buffer-name)))

(defn find-file
  "Open a file in Emacs."
  [file-path]
  (eval-elisp! (format "(find-file \"%s\")" file-path)))

(defn save-buffer
  "Save the current buffer."
  []
  (eval-elisp! "(save-buffer)"))

(defn goto-line
  "Go to a specific line number."
  [line-number]
  (eval-elisp! (format "(goto-line %d)" line-number)))

(defn insert-text
  "Insert text at point."
  [text]
  (eval-elisp! (format "(insert \"%s\")" (str/escape text {\" "\\\"" \\ "\\\\"}))))

(defn project-root
  "Get the current project root."
  []
  (let [result (eval-elisp! "(project-root (project-current))")]
    (when (not= result "nil")
      result)))

(defn recent-files
  "Get list of recent files."
  []
  (eval-elisp! "recentf-list"))

(comment
  ;; Test functions
  (emacs-running?)
  (buffer-list)
  (current-buffer)
  (current-file)
  (project-root))
