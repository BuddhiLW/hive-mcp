(ns hive-mcp.prompts.infra
  "Infrastructure adapters for the prompts/permissions domain.
   
   Provides:
   - Desktop notifications via notify-send
   - WebSocket channel emission for Emacs integration"
  (:require [hive-mcp.notify :as notify]
            [hive-mcp.channel :as channel]
            [taoensso.timbre :as log]))

;;; ---------------------------------------------------------------------------
;;; Desktop Notifications (delegates to hive-mcp.notify)
;;; ---------------------------------------------------------------------------

(defn notify-permission-request!
  "Send critical notification for permission request requiring human decision."
  [agent-id tool-name summary]
  (notify/notify! {:summary (str "🔐 Permission: " tool-name)
                   :body (str "Agent: " agent-id "\n" summary)
                   :type "error" ; critical urgency
                   :timeout 0})) ; never auto-dismiss

(defn notify-agent-blocked!
  "Send notification when agent is blocked waiting for input."
  [agent-id reason]
  (notify/notify! {:summary (str "⏸️ Agent Blocked: " agent-id)
                   :body reason
                   :type "warning"}))

(defn notify-agent-completed!
  "Send notification when agent completes a task."
  [agent-id task-summary]
  (notify/notify! {:summary (str "✅ Agent Completed: " agent-id)
                   :body task-summary
                   :type "info"}))

;;; ---------------------------------------------------------------------------
;;; Channel Emission Adapters
;;; ---------------------------------------------------------------------------

(defn channel-emit!
  "Emit an event to WebSocket channel for Emacs integration.
   
   Wraps hive-mcp.channel/emit-event! for the prompts domain.
   
   Arguments:
     event-type - Keyword identifying the event (e.g. :prompt-pending)
     data       - Map of event data
   
   Returns result of channel emission."
  [event-type data]
  (log/debug "Emitting channel event" {:event-type event-type :data data})
  (channel/emit-event! event-type data))

(defn emit-prompt-pending!
  "Emit prompt-pending event when a new prompt requires human decision.
   
   Arguments:
     prompt - Map with :id, :agent-id, :question, :options, :created-at
   
   Notifies Emacs that a prompt is awaiting response."
  [prompt]
  (channel-emit! :prompt-pending
                 {:prompt-id (:id prompt)
                  :agent-id (:agent-id prompt)
                  :question (:question prompt)
                  :options (:options prompt)
                  :created-at (:created-at prompt)}))

(defn emit-prompt-resolved!
  "Emit prompt-resolved event when a prompt has been answered.
   
   Arguments:
     prompt - Map with :id, :agent-id, :response, :status, :resolved-at
   
   Notifies Emacs that the prompt was resolved."
  [prompt]
  (channel-emit! :prompt-resolved
                 {:prompt-id (:id prompt)
                  :agent-id (:agent-id prompt)
                  :response (:response prompt)
                  :status (:status prompt)
                  :resolved-at (:resolved-at prompt)}))

(defn emit-prompt-expired!
  "Emit prompt-expired event when a prompt times out without response.
   
   Arguments:
     prompt - Map with :id, :agent-id, :resolved-at
   
   Notifies Emacs that the prompt expired."
  [prompt]
  (channel-emit! :prompt-expired
                 {:prompt-id (:id prompt)
                  :agent-id (:agent-id prompt)
                  :resolved-at (:resolved-at prompt)}))
