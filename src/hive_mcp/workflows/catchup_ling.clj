(ns hive-mcp.workflows.catchup-ling
  "Lightweight ling-specific context preparation for spawn injection.

   Delegates to extension layer for context reconstruction.
   Falls back to nil when extension absent — lings run /catchup themselves.
   When extension returns structured context, applies token budget allocation
   via budget/allocate-ling-context for priority-based truncation."

  (:require [hive-mcp.context.budget :as budget]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const max-context-chars
  "Hard cap for ling context output (safety net beyond budget)."
  10000)

(def ^:const default-ling-budget
  "Default token budget for ling catchup context.
   Lings get more context than drones (8000 vs 4000)."
  8000)

(defn- budget-truncate-string
  "Apply token budget truncation to a plain string result.
   Used when extension returns a string (not structured map)."
  [text token-budget]
  (let [{:keys [content tokens truncated?]} (budget/truncate-to-budget text token-budget)]
    (when truncated?
      (log/info "ling-catchup: budget-truncated string context"
                {:original-tokens (budget/estimate-tokens text)
                 :budget-tokens tokens
                 :token-budget token-budget}))
    content))

(defn- budget-allocate-structured
  "Apply budget/allocate-ling-context to structured catchup result.
   Used when extension returns a map with {:preset :axioms :context :history}."
  [{:keys [preset axioms context history] :as _structured} token-budget]
  (let [allocated (budget/allocate-ling-context
                   {:total-budget token-budget
                    :preset       (or preset "")
                    :axioms       (or axioms "")
                    :context      (or context "")
                    :history      (or history "")})]
    (log/info "ling-catchup: budget-allocated structured context"
              {:total-budget (get-in allocated [:metadata :total-budget])
               :total-tokens (get-in allocated [:metadata :total-tokens])
               :remaining (get-in allocated [:metadata :remaining])})
    ;; Reassemble as single string
    (str (when (seq (:preset allocated))   (str (:preset allocated) "\n\n"))
         (when (seq (:axioms allocated))   (str (:axioms allocated) "\n\n"))
         (when (seq (:context allocated))  (str (:context allocated) "\n\n"))
         (when (seq (:history allocated))  (:history allocated)))))

(defn ling-catchup
  "Produce compact context for ling spawn injection.

   Delegates to extension layer. Returns nil when extension absent.
   When extension returns a string, applies token budget truncation.
   When extension returns a structured map, applies budget/allocate-ling-context
   for priority-based allocation across preset/axioms/context/history layers.

   Arguments:
     opts - Map with:
            :directory      - Working directory for project scoping (required)
            :task           - Task description string (optional)
            :kanban-task-id - Kanban task ID for context targeting (optional)
            :token-budget   - Token budget override (default: 8000)

   Returns:
     Compact context string (budget-managed, hard-capped at 10K chars), or nil."
  [{:keys [_directory token-budget] :as opts}]
  (rescue nil
          (when-let [f (requiring-resolve 'hive-mcp.extensions.context/ling-catchup)]
            (let [result (f opts)
                  effective-budget (or token-budget default-ling-budget)]
              (cond
          ;; Structured result: apply layered budget allocation
                (and (map? result) (or (:preset result) (:axioms result)
                                       (:context result) (:history result)))
                (let [budgeted (budget-allocate-structured result effective-budget)]
                  (when (and budgeted (seq budgeted))
                    (if (> (count budgeted) max-context-chars)
                      (subs budgeted 0 max-context-chars)
                      budgeted)))

          ;; String result: apply token budget truncation
                (and (string? result) (pos? (count result)))
                (let [budgeted (budget-truncate-string result effective-budget)]
                  (if (> (count budgeted) max-context-chars)
                    (do (log/info "ling-catchup: hard-capped post-budget" {:chars (count budgeted)})
                        (subs budgeted 0 max-context-chars))
                    budgeted))

          ;; Nil or empty
                :else nil)))))
