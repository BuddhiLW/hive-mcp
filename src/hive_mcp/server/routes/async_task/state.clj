(ns hive-mcp.server.routes.async-task.state
  "Pure decisions about an async task's lifecycle.

   One function, one decision. Nothing here touches a thread, a clock or an
   atom: a `java.util.concurrent.Future` reaches this layer only as a
   `schema/HandleView`, so every rule below is testable without concurrency."
  (:require [malli.core :as m]
            [hive-mcp.server.routes.async-task.schema :as s]
            [hive-dsl.adt :as adt :refer [adt-case]]))

;; =============================================================================
;; Promoters
;; =============================================================================

(defn terminal?
  "Has the task stopped, for any reason?"
  [state]
  (contains? s/terminal-states state))

(defn reported-state
  "The state to show a caller, given what we RECORDED and what the handle says.

   The recorded state is an intention; the handle is the fact. Where they
   disagree the handle wins, so a task that died on its own is never listed as
   running. The exception is a recorded terminal state: `:task/cancelled`,
   `:task/timed-out` and `:task/failed` all leave a handle that merely reads as
   done, and that reading would erase the only record of WHY it stopped."
  [recorded {:keys [present? done? cancelled?]}]
  (cond
    (terminal? recorded) recorded
    (not present?)       recorded
    cancelled?           :task/cancelled
    done?                :task/done
    :else                recorded))

(defn cancel-outcome
  "What a cancel request achieves against `entry`, decided before acting.

   Separated from the act so the decision is testable and so a caller can see
   a refusal without a side effect. Returns an `AsyncCancelOutcome` variant."
  [task-id entry]
  (cond
    (nil? entry)
    (s/async-cancel-outcome :cancel/unknown-task {:task-id task-id})

    (not (get-in entry [:handle :present?]))
    (s/async-cancel-outcome :cancel/not-started {:task-id task-id})

    (or (terminal? (:state entry))
        (get-in entry [:handle :done?]))
    (s/async-cancel-outcome :cancel/already-finished
                            {:task-id task-id
                             :state   (reported-state (:state entry) (:handle entry))})

    :else
    (s/async-cancel-outcome :cancel/interrupted {:task-id task-id})))

(defn summarize
  "Render one registry row for a caller, as of `now-ms`."
  [task-id {:keys [tool caller-id state started-at timeout-ms handle]} now-ms]
  (cond-> {:task-id    task-id
           :tool       tool
           :caller-id  caller-id
           :state      (reported-state state handle)
           :elapsed-ms (- now-ms started-at)}
    timeout-ms (assoc :timeout-ms timeout-ms)))

(defn completion-state
  "The state a task lands in when its body returns.

   A body that returns has not necessarily WON: it may be unwinding from an
   interrupt, and the cancel that caused it already recorded why. Only a task
   still believed to be running is promoted to done."
  [recorded]
  (if (= :task/running recorded) :task/done recorded))

(defn collectable?
  "May this row be dropped by a sweep? Only a finished one."
  [entry]
  (terminal? (reported-state (:state entry) (:handle entry))))

(defn stops-the-work?
  "Did this outcome actually interrupt something? Exhaustive over the sum.

   `adt-case` rather than a truthy field: a new outcome variant then fails
   here instead of silently defaulting to false."
  [outcome]
  (adt-case s/AsyncCancelOutcome outcome
    :cancel/interrupted      true
    :cancel/already-finished false
    :cancel/not-started      false
    :cancel/unknown-task     false))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> terminal? [:=> [:cat :any] :boolean])
(m/=> reported-state [:=> [:cat s/TaskState s/HandleView] s/TaskState])
(m/=> cancel-outcome [:=> [:cat s/TaskId [:maybe s/TaskEntry]] s/CancelOutcome])
(m/=> summarize [:=> [:cat s/TaskId s/TaskEntry :int] s/TaskSummary])
(m/=> completion-state [:=> [:cat s/TaskState] s/TaskState])
(m/=> collectable? [:=> [:cat s/TaskEntry] :boolean])
