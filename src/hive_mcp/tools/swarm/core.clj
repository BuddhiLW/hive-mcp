(ns hive-mcp.tools.swarm.core
  "Core utilities for swarm tool handlers.

   SOLID: SRP - Centralized validation and error handling for swarm operations.
   CLARITY: Y - Yield safe failure with graceful error messages.

   Provides:
   - swarm-addon-available? - Check if hive-mcp-swarm addon is loaded
   - with-swarm macro - Execute body only if swarm addon available
   - Response builders for consistent MCP response format"
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [clojure.data.json :as json]
            [clojure.string :as str]))

;; ============================================================
;; Swarm Addon Check
;; ============================================================

(defn swarm-addon-available?
  "Check if hive-mcp-swarm addon is loaded.
   Uses short timeout (2s) to fail fast if Emacs is unresponsive.

   CLARITY: Y - Yield safe failure (returns false on timeout/error)"
  []
  (let [{:keys [success result timed-out]} (ec/eval-elisp-with-timeout "(featurep 'hive-mcp-swarm)" 2000)]
    (and success (not timed-out) (= result "t"))))

;; ============================================================
;; Response Builders
;; ============================================================

(defn mcp-success
  "Build a successful MCP response.

   SOLID: DRY - Consistent response format across all handlers."
  [data]
  {:type "text" :text (if (string? data) data (json/write-str data))})

(defn mcp-error
  "Build an error MCP response.

   SOLID: DRY - Consistent error format across all handlers."
  [message]
  {:type "text" :text message :isError true})

(defn mcp-error-json
  "Build an error MCP response with JSON payload.

   SOLID: DRY - For errors that include structured data."
  [data]
  {:type "text" :text (json/write-str data) :isError true})

(defn mcp-timeout-error
  "Build a timeout error response.

   SOLID: DRY - Consistent timeout format across all handlers."
  [operation & {:keys [extra-data]}]
  (let [base {:error (str operation " timed out")
              :status "timeout"}
        data (if extra-data (merge base extra-data) base)]
    {:type "text" :text (json/write-str data) :isError true}))

(defn addon-not-loaded-error
  "Return standard error when swarm addon not loaded.

   SOLID: DRY - Single definition of this common error."
  []
  {:type "text" :text "hive-mcp-swarm addon not loaded. Run (require 'hive-mcp-swarm)" :isError true})

;; ============================================================
;; Swarm Macro
;; ============================================================

(defmacro with-swarm
  "Execute body only if swarm addon is available.
   Returns addon-not-loaded error if unavailable.

   Usage:
     (with-swarm
       (let [result (do-something)]
         (mcp-success result)))

   CLARITY: Y - Yield safe failure (graceful addon check)
   SOLID: SRP - Centralizes addon validation"
  [& body]
  `(if (swarm-addon-available?)
     (do ~@body)
     (addon-not-loaded-error)))

;; ============================================================
;; Elisp Helpers
;; ============================================================

(defn eval-elisp-safe
  "Evaluate elisp with timeout and structured response handling.

   Returns {:ok true :result result} on success,
           {:ok false :error error :timed-out bool} on failure.

   CLARITY: R - Represented intent with clear result structure
   SOLID: SRP - Single function for elisp evaluation"
  [elisp timeout-ms]
  (let [{:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp timeout-ms)]
    (cond
      timed-out {:ok false :error "Elisp evaluation timed out" :timed-out true}
      (not success) {:ok false :error error :timed-out false}
      :else {:ok true :result result})))

(defn format-elisp-list
  "Format a Clojure sequence as elisp list.

   Usage: (format-elisp-list [\"a\" \"b\"]) => \"'(\\\"a\\\" \\\"b\\\")\"

   CLARITY: R - Clear intent for elisp string building"
  [items format-item-fn]
  (when (seq items)
    (format "'(%s)" (str/join " " (map format-item-fn items)))))

(defn escape-for-elisp
  "Escape a string for safe embedding in elisp.
   Delegates to validation namespace."
  [s]
  (v/escape-elisp-string s))
