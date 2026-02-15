(ns hive-mcp.agent.drone.loop-predicates
  "Termination and evaluation predicates for the agentic drone loop.

   Pure functions — no side effects, no session state.
   Extracted from loop.clj to isolate conditional complexity."
  (:require [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Completion Language Detection
;; =============================================================================

(def ^:private completion-patterns
  "Regex patterns that indicate the LLM considers its task complete."
  [#"(?i)task\s+(is\s+)?complet(e|ed)"
   #"(?i)i('ve|\s+have)\s+(successfully\s+)?(complet|finish|done)"
   #"(?i)all\s+(changes|modifications|updates)\s+(have\s+been\s+)?(made|applied|complet)"
   #"(?i)the\s+(fix|implementation|change|update)\s+(is|has been)\s+(ready|complet|done|applied)"
   #"(?i)successfully\s+(implemented|applied|fixed|updated|created|modified)"
   #"(?i)here('s| is)\s+the\s+(summary|result|final)"
   #"(?i)(nothing|no)\s+(more|else|further)\s+(to|needs?\s+to\s+be)\s+(do|change|fix)"
   #"(?i)all\s+done"])

(defn completion-language?
  "Check if text contains language indicating task completion."
  [text]
  (when (and text (not (str/blank? text)))
    (boolean (some #(re-find % text) completion-patterns))))

;; =============================================================================
;; No-Progress Detection
;; =============================================================================

(defn- step-failed?
  "True if a step represents a failed action (error or all tool calls failed)."
  [step]
  (or (= :error (:type step))
      (and (= :tool_calls (:type step))
           (every? (fn [c] (not (:success c))) (:calls step)))))

(defn no-progress?
  "True if the last N steps all failed (no forward progress)."
  [steps n]
  (and (>= (count steps) n)
       (every? step-failed? (take-last n steps))))

;; =============================================================================
;; Termination
;; =============================================================================

(def ^:private failure-threshold
  "Maximum consecutive failures before forced termination."
  3)

(defn should-terminate?
  "Determine if the agentic loop should stop.
   Returns {:terminate? bool :reason string}."
  [state & [opts]]
  (if-let [ext-fn (ext/get-extension :al/terminate?)]
    (ext-fn state opts)
    (let [{:keys [turn steps consecutive-failures last-response-type last-text]} state
          max-turns (or (:max-turns opts) 10)]
      (cond
        (>= turn max-turns)
        {:terminate? true :reason (str "Max turns reached (" max-turns ")")}

        (= :text last-response-type)
        {:terminate? true :reason "Text-only response (no tool calls = task complete)"}

        (>= (or consecutive-failures 0) failure-threshold)
        {:terminate? true :reason (str "Too many consecutive failures (" consecutive-failures ")")}

        (and last-text (completion-language? last-text))
        {:terminate? true :reason "Completion language detected in response"}

        (no-progress? steps 3)
        {:terminate? true :reason "No progress in last 3 turns"}

        :else
        {:terminate? false :reason "Continuing"}))))

;; =============================================================================
;; Result Evaluation
;; =============================================================================

(def ^:private retryable-patterns
  "Error message patterns that indicate a retryable failure."
  [#"(?i)not found" #"(?i)no such file" #"(?i)timeout"])

(def ^:private fatal-patterns
  "Error message patterns that indicate a non-recoverable failure."
  [#"(?i)unknown tool" #"(?i)rejected"])

(defn- matches-any-pattern?
  "True if text matches any of the given regex patterns."
  [text patterns]
  (boolean (some #(re-find % text) patterns)))

(defn evaluate-result
  "Evaluate whether a tool result moves toward the goal.
   Returns {:quality :good|:bad :continue? bool :reason string}."
  [result goal history]
  (if-let [ext-fn (ext/get-extension :al/evaluate)]
    (ext-fn result goal history)
    (let [success? (:success result)
          error-msg (str (:error result))]
      (cond
        success?
        {:quality :good :continue? true :reason "Tool execution succeeded"}

        (matches-any-pattern? error-msg fatal-patterns)
        {:quality :bad :continue? false :reason (str "Fatal error: " error-msg)}

        (matches-any-pattern? error-msg retryable-patterns)
        {:quality :bad :continue? true :reason (str "Retryable error: " error-msg)}

        :else
        {:quality :bad :continue? true :reason (str "Error: " error-msg)}))))

;; =============================================================================
;; Tool Selection (extension point)
;; =============================================================================

(defn select-next-tool
  "Select the next tool to invoke based on current state and observations."
  [observations available-tools task-context]
  (if-let [ext-fn (ext/get-extension :al/next-tool)]
    (ext-fn observations available-tools task-context)
    nil))

;; =============================================================================
;; Final Status Determination
;; =============================================================================

(defn determine-final-status
  "Determine the loop's final status from termination state."
  [{:keys [turn last-response-type]} max-turns]
  (if (>= turn max-turns) :max_steps :completed))
