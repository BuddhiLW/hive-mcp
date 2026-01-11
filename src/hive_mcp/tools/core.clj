(ns hive-mcp.tools.core
  "Core utilities for MCP tool responses.

   All tool handler modules should require this namespace for
   consistent response formatting.

   Piggyback system delegates to hive-mcp.channel.piggyback for:
   - Instruction queue (hivemind → ling)
   - Message cursors (ling reads hivemind broadcasts)"
  (:require [clojure.data.json :as json]
            [hive-mcp.channel.piggyback :as piggyback]))

;; =============================================================================
;; Instruction Queue (delegated to piggyback)
;; =============================================================================

(def push-instruction!
  "Push an instruction to an agent's queue. Delegates to piggyback module."
  piggyback/push-instruction!)

(def drain-instructions!
  "Drain all pending instructions for an agent. Delegates to piggyback module."
  piggyback/drain-instructions!)

(defn- attach-instructions
  "Attach pending instructions to a response if agent-id provided and instructions exist."
  [response agent-id]
  (if agent-id
    (let [instructions (drain-instructions! agent-id)]
      (if (seq instructions)
        (assoc response :pending_instructions instructions)
        response))
    response))

;; =============================================================================
;; MCP Response Constructors
;; =============================================================================

(defn mcp-success
  "Create a successful MCP response. Text can be string or will be pr-str'd.

   Options:
   - :agent-id - If provided, drains and attaches pending instructions for this agent"
  [text & {:keys [agent-id]}]
  (-> {:type "text" :text (if (string? text) text (pr-str text))}
      (attach-instructions agent-id)))

(defn mcp-error
  "Create an error MCP response."
  [message]
  {:type "text" :text message :isError true})

(defn mcp-json
  "Create a successful MCP response with JSON-encoded data.

   Options:
   - :agent-id - If provided, drains and attaches pending instructions for this agent"
  [data & {:keys [agent-id]}]
  (-> {:type "text" :text (json/write-str data)}
      (attach-instructions agent-id)))

;; =============================================================================
;; Hivemind Message Piggyback (delegated to piggyback)
;; =============================================================================

(def get-hivemind-piggyback
  "Get new hivemind messages since last tool call for specific agent.
   Delegates to piggyback module. Returns nil if no new messages."
  piggyback/get-messages)

(def fetch-hivemind-history
  "Get hivemind messages without marking as read. Delegates to piggyback module."
  piggyback/fetch-history)

(defn mcp-success+hm
  "MCP success response with hivemind piggyback for coordinator.
   Includes any new agent shouts since last tool call.

   The agent-id parameter is required to track per-agent read cursors,
   ensuring each agent only sees messages once."
  [text & {:keys [agent-id]}]
  (let [base (mcp-success text :agent-id agent-id)
        hm (when agent-id (get-hivemind-piggyback agent-id))]
    (if (seq hm)
      (assoc base :_hm hm)
      base)))
