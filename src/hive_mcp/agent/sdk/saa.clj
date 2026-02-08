(ns hive-mcp.agent.sdk.saa
  "SAA (Silence-Abstract-Act) phase definitions, scoring, and tracking.

   Contains:
   - Phase configuration (tools, permissions, prompts per phase)
   - Observation scoring (L1/L2 heuristic, L3+ requiring-resolve stub)
   - Silence tracking integration with silence.clj

   SAA Strategy (Korzybski structural differential):
   - Silence: Observe quietly via read-only tools, collect context
   - Abstract: Synthesize observations into a plan
   - Act: Execute the plan with full tool access"
  (:require [hive-mcp.agent.sdk.session :as session]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; SAA Phase Definitions
;;; =============================================================================

(def saa-phases
  "SAA phase configuration.
   Each phase defines the tools allowed and the system prompt modifier."
  {:silence
   {:name :silence
    :description "Observe quietly - read files, query memory, traverse KG"
    :allowed-tools ["Read" "Glob" "Grep" "WebSearch" "WebFetch"]
    :permission-mode "bypassPermissions"
    :system-prompt-suffix
    (str "You are in SILENCE phase (SAA Strategy).\n"
         "Your goal: Observe and collect context WITHOUT acting.\n"
         "- Read files, search code, query memory\n"
         "- Record what you find as structured observations\n"
         "- Do NOT edit files or run commands\n"
         "- At the end, produce a JSON summary of observations")}

   :abstract
   {:name :abstract
    :description "Synthesize observations into a plan"
    :allowed-tools ["Read" "Glob" "Grep"]
    :permission-mode "bypassPermissions"
    :system-prompt-suffix
    (str "You are in ABSTRACT phase (SAA Strategy).\n"
         "Your goal: Synthesize the observations from Silence phase into an action plan.\n"
         "- Prioritize findings by importance and relevance\n"
         "- Identify patterns and connections\n"
         "- Produce a structured plan with specific steps\n"
         "- Each step should name the file, the change, and the rationale")}

   :act
   {:name :act
    :description "Execute the plan with full tool access"
    :allowed-tools ["Read" "Edit" "Write" "Bash" "Glob" "Grep"]
    :permission-mode "acceptEdits"
    :system-prompt-suffix
    (str "You are in ACT phase (SAA Strategy).\n"
         "Your goal: Execute the plan from the Abstract phase.\n"
         "- Follow the plan step by step\n"
         "- Make precise, focused changes\n"
         "- Verify each change before moving to the next")}})

;;; =============================================================================
;;; Integration with silence.clj
;;; =============================================================================

(defn- resolve-silence-fn
  "Dynamically resolve silence.clj functions."
  [fn-sym]
  (try
    (requiring-resolve (symbol "hive-mcp.hot.silence" (name fn-sym)))
    (catch Exception _ nil)))

(defn with-silence-tracking
  "Wrap a phase execution with silence.clj tracking.
   Records file reads and discoveries during the Silence phase."
  [ling-id task body-fn]
  (if-let [start-fn (resolve-silence-fn 'start-exploration!)]
    (let [_session-id (start-fn {:task task :agent-id ling-id})]
      (try
        (body-fn)
        (finally
          (when-let [end-fn (resolve-silence-fn 'end-exploration!)]
            (let [summary (end-fn)]
              (session/update-session! ling-id {:silence-summary summary}))))))
    ;; silence.clj not available, just run
    (body-fn)))

;;; =============================================================================
;;; Crystal/Core Integration (Recall Scoring)
;;; =============================================================================

(defn score-observations
  "Score observations using crystal/core promotion scoring.
   Filters and prioritizes what matters for the Abstract phase.

   Uses requiring-resolve stub pattern for L3+ scoring."
  [observations]
  (if-let [score-fn (try (requiring-resolve 'hive-knowledge.scoring/score-observations)
                         (catch Exception _ nil))]
    ;; L3+ proprietary scoring
    (score-fn observations)
    ;; L1/L2 fallback: simple heuristic scoring
    (let [score-entry (fn [obs]
                        (let [content (str (or (:data obs) obs))
                              ;; Simple heuristics
                              has-pattern? (re-find #"pattern|convention|decision" content)
                              has-issue? (re-find #"bug|error|issue|fix" content)
                              has-test? (re-find #"test|spec|assert" content)
                              base-score 1.0]
                          (cond-> base-score
                            has-pattern? (+ 2.0)
                            has-issue? (+ 3.0)
                            has-test? (+ 1.5))))]
      (->> observations
           (map (fn [obs] {:observation obs :score (score-entry obs)}))
           (sort-by :score >)
           vec))))
