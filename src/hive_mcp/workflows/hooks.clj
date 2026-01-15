(ns hive-mcp.workflows.hooks
  "Workflow lifecycle hooks using hive-events.
   
   Provides before/after hooks for workflow execution.
   The wrap workflow's wrap_notify emission becomes a registered hook,
   not a hardcoded special case.
   
   SOLID: O - Open/Closed (add hooks via registration, not modification)
   CLARITY: C - Composition over modification (hooks compose via events)
   CLARITY: T - Telemetry first (all workflow executions emit events)"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]))

(defonce ^:private *initialized (atom false))

(defn dispatch-before
  "Emit :workflow/before event.
   
   Called before workflow execution starts.
   Allows hooks to prepare state, log, etc."
  [workflow-name args]
  (when (ev/handler-registered? :workflow/before)
    (ev/dispatch [:workflow/before {:workflow-name workflow-name
                                    :args args}])))

(defn dispatch-after
  "Emit :workflow/after event.
   
   Called after workflow execution completes.
   Result can be success or error - hooks can inspect and react."
  [workflow-name args result]
  (when (ev/handler-registered? :workflow/after)
    (ev/dispatch [:workflow/after {:workflow-name workflow-name
                                   :args args
                                   :result result}])))

(defn- handle-workflow-before
  "Handler for :workflow/before events.
   
   Currently just logs. Can be extended to add pre-execution hooks."
  [_coeffects event]
  (let [{:keys [workflow-name]} (second event)]
    {:log {:level :debug
           :message (str "Workflow starting: " workflow-name)}}))

(defn- parse-result-safely
  "Parse result as JSON if string, otherwise return as-is."
  [result]
  (if (string? result)
    (try
      (json/read-str result :key-fn keyword)
      (catch Exception _ {}))
    (or result {})))

(defn- handle-workflow-after
  "Handler for :workflow/after events.
   
   Special behavior for 'wrap' workflow:
   - Dispatches :crystal/wrap-notify for Crystal Convergence
   - This replaces the hardcoded special case in buffer.clj"
  [_coeffects event]
  (let [{:keys [workflow-name result]} (second event)
        agent-id (or (System/getenv "CLAUDE_SWARM_SLAVE_ID") "coordinator")]
    (if (= workflow-name "wrap")
      ;; Wrap workflow: emit wrap_notify for Crystal Convergence
      (let [parsed (parse-result-safely result)
            stats (:counts parsed)]
        {:dispatch [:crystal/wrap-notify
                    {:agent-id agent-id
                     :session-id (str "session:" (:date parsed) ":" agent-id)
                     :created-ids []
                     :stats stats}]
         :log {:level :info
               :message (str "Workflow completed: " workflow-name " - emitting wrap_notify")}})
      ;; Other workflows: just log
      {:log {:level :debug
             :message (str "Workflow completed: " workflow-name)}})))

(defn register-hooks!
  "Register workflow lifecycle event handlers.
   
   Safe to call multiple times - only registers once.
   
   Hooks registered:
   - :workflow/before - Pre-execution hook (logging)
   - :workflow/after  - Post-execution hook (wrap_notify for wrap workflow)
   
   Returns true if hooks were registered, false if already registered."
  []
  (when-not @*initialized
    ;; Ensure effects are registered first
    ((resolve 'hive-mcp.events.effects/register-effects!))

    ;; Register workflow lifecycle handlers
    (ev/reg-event :workflow/before []
                  handle-workflow-before)

    (ev/reg-event :workflow/after []
                  handle-workflow-after)

    (reset! *initialized true)
    (log/info "[workflow-hooks] Registered: :workflow/before :workflow/after")
    true))

(defn reset-hooks!
  "Reset hook registration state. For testing."
  []
  (reset! *initialized false))

(defn hooks-registered?
  "Check if workflow hooks have been registered."
  []
  @*initialized)
