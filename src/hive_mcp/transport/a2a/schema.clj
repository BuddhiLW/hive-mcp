(ns hive-mcp.transport.a2a.schema
  "A2A protocol data structures and JSON-RPC helpers.

   Pure data layer — no dependencies on hive-mcp internals.
   Defines A2A Task, Message, StatusUpdateEvent factories
   and JSON-RPC 2.0 success/error wrappers.

   Spec: https://google.github.io/A2A/"
  (:require [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Error Codes (JSON-RPC + A2A-specific)
;; =============================================================================

(def error-codes
  "A2A-specific JSON-RPC error codes."
  {:parse-error      -32700
   :invalid-request  -32600
   :method-not-found -32601
   :invalid-params   -32602
   :internal-error   -32603
   :task-not-found   -32001
   :agent-not-found  -32002
   :task-canceled    -32003
   :unauthorized     -32004})

;; =============================================================================
;; Agent Status -> A2A Task State Translation
;; =============================================================================

(def agent-status->task-state
  "Maps DataScript slave status to A2A task state."
  {:spawning    "submitted"
   :working     "working"
   :blocked     "input-required"
   :idle        "completed"
   :error       "failed"
   :terminated  "canceled"})

;; =============================================================================
;; A2A Data Factories
;; =============================================================================

(defn make-message
  "Build an A2A Message.
   role: \"user\" | \"agent\"
   parts: vector of Part maps, e.g. [{:type \"text\" :text \"hello\"}]"
  [role parts]
  {:role role
   :parts (vec parts)})

(defn make-task
  "Build an A2A Task object.
   id: unique task identifier
   context-id: conversation context (groups related tasks)
   state: A2A task state string
   status-msg: human-readable status message
   opts: optional keys — :messages, :artifacts, :metadata"
  [id context-id state status-msg & {:as opts}]
  (cond-> {:id id
           :contextId context-id
           :status {:state state
                    :message (make-message "agent"
                                           [{:type "text" :text status-msg}])}
           :kind "task"}
    (:messages opts)  (assoc :messages (:messages opts))
    (:artifacts opts) (assoc :artifacts (:artifacts opts))
    (:metadata opts)  (assoc :metadata (:metadata opts))))

(defn make-status-update-event
  "Build an A2A TaskStatusUpdateEvent for SSE streaming."
  [task-id state msg]
  {:id task-id
   :status {:state state
            :message (make-message "agent"
                                   [{:type "text" :text msg}])}
   :kind "status-update"})

(defn make-artifact-update-event
  "Build an A2A TaskArtifactUpdateEvent for SSE streaming."
  [task-id artifact]
  {:id task-id
   :artifact artifact
   :kind "artifact-update"})

;; =============================================================================
;; JSON-RPC 2.0 Wrappers
;; =============================================================================

(defn jsonrpc-success
  "Wrap result in JSON-RPC 2.0 success response."
  [id result]
  {:jsonrpc "2.0"
   :id id
   :result result})

(defn jsonrpc-error
  "Wrap error in JSON-RPC 2.0 error response."
  [id code msg & {:as data}]
  (cond-> {:jsonrpc "2.0"
           :id id
           :error {:code code :message msg}}
    data (assoc-in [:error :data] data)))
