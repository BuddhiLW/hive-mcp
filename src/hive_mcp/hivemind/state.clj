(ns hive-mcp.hivemind.state
  "Hivemind state atoms and direct accessors."

  (:require [hive-mcp.guards :as guards]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^{:doc "Map of ask-id -> {:question ... :response-chan ...}"}
  pending-asks
  (atom {}))

(defonce ^{:doc "Agent message history ring buffer. Map of agent-id -> {:messages [...] :last-seen timestamp}."}
  agent-registry
  (atom {}))

(defonce ^{:doc "Map of slave-id -> {:prompt :timestamp :session-id}. Permission prompts from Emacs slaves."}
  pending-swarm-prompts
  (atom {}))

(defonce ling-results (atom {}))

(defn record-ling-result!
  "Record ling completion for coordinator review."
  [agent-id result]
  (swap! ling-results assoc agent-id
         {:result result
          :timestamp (System/currentTimeMillis)
          :reviewed? false}))

(defn get-pending-ling-results
  "Get ling results awaiting coordinator review."
  []
  (->> @ling-results
       (filter (fn [[_ v]] (not (:reviewed? v))))
       (into {})))

(defn mark-ling-reviewed!
  "Mark a ling result as reviewed by coordinator."
  [agent-id]
  (swap! ling-results assoc-in [agent-id :reviewed?] true))

(defn clear-ling-results!
  "Clear all ling results (e.g., at session end)."
  []
  (reset! ling-results {}))

(defn add-swarm-prompt!
  "Add a permission prompt from a swarm slave."
  [slave-id prompt-text session-id timestamp]
  (let [prompt-data {:prompt prompt-text
                     :timestamp timestamp
                     :session-id session-id
                     :received-at (System/currentTimeMillis)}]
    (swap! pending-swarm-prompts assoc slave-id prompt-data)
    (log/info "Swarm prompt received from" slave-id ":"
              (subs prompt-text 0 (min 50 (count prompt-text))))
    prompt-data))

(defn remove-swarm-prompt!
  "Remove a swarm prompt (after it's been answered)."
  [slave-id]
  (swap! pending-swarm-prompts dissoc slave-id)
  (log/debug "Swarm prompt cleared:" slave-id))

(defn get-swarm-prompts
  "Get all pending swarm prompts."
  []
  @pending-swarm-prompts)

(defn clear-agent-registry!
  "Clear agent registry. Guarded — no-op if coordinator running."
  []
  (guards/when-not-coordinator
   "clear-agent-registry! called"
   (reset! agent-registry {})))
