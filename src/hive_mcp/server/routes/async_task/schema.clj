(ns hive-mcp.server.routes.async-task.schema
  "Value objects for an async tool call.

   The single source for this subsystem's shapes: `m/=>` contracts and the
   synthesized property tests both read from here."
  (:require [malli.core :as m]
            [hive-dsl.adt :as adt :refer [defadt]]))

;; =============================================================================
;; Identifiers
;; =============================================================================

(def TaskId    [:string {:min 1}])
(def ToolName  [:string {:min 1}])
(def CallerId  [:string {:min 1}])

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defadt AsyncTaskState
  "Where a task is, as reported to a caller.

   `:cancelled` and `:timed-out` are distinct on purpose: both stop the work,
   but one is an operator's decision and the other is the task exceeding a
   bound it was given. Collapsing them loses the only fact that says which.

   Named `AsyncTaskState`, not `TaskState`: the adt registry is keyed by BARE
   NAME and is process-global, so a generic name is last-writer-wins against
   any other library in the same JVM."
  :task/queued
  :task/running
  :task/done
  :task/cancelled
  :task/timed-out
  :task/failed)

(def TaskState
  "Malli projection of `AsyncTaskState`.

   Derived, never retyped: an enum listing the variants a second time is the
   copy that goes stale the first time a variant is added."
  (into [:enum] (sort (adt/type-variants :AsyncTaskState))))

(def terminal-states
  "The variants that mean the task has stopped.

   A subset of the ADT, checked against it at load: naming a variant here that
   `AsyncTaskState` does not declare is a typo that would otherwise make
   `terminal?` quietly answer false forever."
  (let [ts #{:task/done :task/cancelled :task/timed-out :task/failed}
        declared (set (adt/type-variants :AsyncTaskState))]
    (assert (every? declared ts)
            (str "terminal-states names variants AsyncTaskState does not declare: "
                 (remove declared ts)))
    ts))

(def HandleView
  "What the pure layer is allowed to know about a running handle.

   A `java.util.concurrent.Future` never crosses into the pure layer; this is
   its projection, so state decisions stay testable without a thread."
  [:map {:closed true}
   [:present? :boolean]
   [:done?    :boolean]
   [:cancelled? :boolean]])

(def TaskEntry
  "One registry row, as the pure layer sees it."
  [:map {:closed true}
   [:tool        ToolName]
   [:caller-id   CallerId]
   [:state       TaskState]
   [:started-at  :int]
   [:timeout-ms  [:maybe pos-int?]]
   [:handle      HandleView]])

(def TaskSummary
  "One row rendered for a caller."
  [:map {:closed true}
   [:task-id     TaskId]
   [:tool        ToolName]
   [:caller-id   CallerId]
   [:state       TaskState]
   [:elapsed-ms  :int]
   [:timeout-ms {:optional true} pos-int?]])

;; =============================================================================
;; Cancellation
;; =============================================================================

(defadt AsyncCancelOutcome
  "What a cancel request achieved.

   A closed set, so it is a sum rather than a `:reason` keyword floating in a
   map: `adt-case` over this is exhaustive, and adding an outcome breaks every
   dispatch that has not considered it, which is the point.

   `:cancel/unknown-task` is an ANSWER, not an error: asking about a task that
   has already been swept is a reasonable question."
  [:cancel/interrupted      {:task-id string?}]
  [:cancel/already-finished {:task-id string? :state keyword?}]
  [:cancel/not-started      {:task-id string?}]
  [:cancel/unknown-task     {:task-id string?}])

(def CancelReason
  "Malli projection of `AsyncCancelOutcome`'s variant tags."
  (into [:enum] (sort (adt/type-variants :AsyncCancelOutcome))))

(def CancelOutcome
  "Malli shape of an `AsyncCancelOutcome` value.

   Open, not `:closed true`: the variants carry different fields, and a closed
   map here would reject the ones that carry more than `:task-id`."
  [:map
   [:adt/type    [:= :AsyncCancelOutcome]]
   [:adt/variant CancelReason]
   [:task-id     TaskId]
   [:state {:optional true} TaskState]])

;; =============================================================================
;; Submission
;; =============================================================================

(def TaskSpec
  "What a caller must supply to start a task.

   `:timeout-ms` is optional because most async calls have no defensible
   bound; a caller who knows one supplies it, and the task then ends by itself
   rather than needing an operator."
  [:map
   [:task-id    TaskId]
   [:tool       ToolName]
   [:caller-id  CallerId]
   [:timeout-ms {:optional true} [:maybe pos-int?]]
   [:f          fn?]])

(def valid-state? (m/validator TaskState))
