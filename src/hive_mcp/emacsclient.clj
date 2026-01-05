(ns hive-mcp.emacsclient
  "Shell wrapper for emacsclient communication with running Emacs."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent TimeoutException]))

(def ^:dynamic *emacsclient-path*
  "Path to emacsclient binary."
  (or (System/getenv "EMACSCLIENT") "emacsclient"))

(def ^:dynamic *default-timeout-ms*
  "Default timeout for emacsclient calls in milliseconds."
  5000)

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
   
   Returns a map with :success, :result or :error keys.
   On timeout, returns {:success false :error \"Timeout...\" :timed-out true}"
  ([code] (eval-elisp-with-timeout code *default-timeout-ms*))
  ([code timeout-ms]
   (log/debug "Executing elisp with timeout:" timeout-ms "ms -" code)
   (let [start (System/currentTimeMillis)
         f (future
             (try
               (let [{:keys [exit out err]} (sh *emacsclient-path* "--eval" code)]
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
               (log/warn :emacsclient-failure {:duration-ms duration :error (:error result)}))
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
   Includes timing information for observability."
  [code]
  (log/debug "Executing elisp:" code)
  (let [start (System/currentTimeMillis)]
    (try
      (let [{:keys [exit out err]} (sh *emacsclient-path* "--eval" code)
            duration (- (System/currentTimeMillis) start)]
        (if (zero? exit)
          (do
            (log/debug :emacsclient-success {:duration-ms duration
                                             :result-length (count out)})
            {:success true
             :result (unwrap-emacs-string (str/trim out))
             :duration-ms duration})
          (do
            (log/warn :emacsclient-failure {:duration-ms duration
                                            :exit-code exit
                                            :error err})
            {:success false
             :error (str/trim err)
             :duration-ms duration})))
      (catch Exception e
        (let [duration (- (System/currentTimeMillis) start)]
          (log/error :emacsclient-exception {:duration-ms duration
                                             :exception (.getMessage e)}
                     e)
          {:success false
           :error (str "Failed to execute emacsclient: " (.getMessage e))
           :duration-ms duration})))))

(defn eval-elisp!
  "Execute elisp and return result string, or throw on error."
  [code]
  (let [{:keys [success result error]} (eval-elisp code)]
    (if success
      result
      (throw (ex-info "Elisp evaluation failed" {:error error :code code})))))

(defn emacs-running?
  "Check if Emacs server is running."
  []
  (:success (eval-elisp "t")))

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
