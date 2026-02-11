(ns hive-mcp.agent.drone.kg-session
  "Compressed context reconstruction for agent communication via extension delegation."
  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def session-schema
  "DataScript schema for session nodes."
  {:node/id         {:db/unique :db.unique/identity
                     :db/doc "Unique node ID within session"}
   :node/type       {:db/doc "Node type: :observation :action :discovery :decision :goal-state"}
   :node/content    {:db/doc "Compact text summary of the node (not raw content)"}
   :node/turn       {:db/doc "Turn number when this node was created"}
   :node/timestamp  {:db/doc "Creation timestamp (epoch ms)"}
   :node/source     {:db/doc "Source tool or message type that produced this node"}
   :node/files      {:db/cardinality :db.cardinality/many
                     :db/doc "Files referenced by this node"}
   :node/superseded {:db/doc "If true, this node was superseded by a later observation"}
   :node/importance {:db/doc "Importance score 0.0-1.0 for reconstruction priority"}
   :node/tags       {:db/cardinality :db.cardinality/many
                     :db/doc "Tags for categorization and filtering"}})

(def node-types
  "Valid node types for session."
  #{:observation :action :discovery :decision :goal-state})

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

(def ^:private noop-session nil)

(def ^:private noop-stats
  {:turns-compressed 0
   :tokens-saved 0
   :compression-ratio "N/A (noop)"
   :nodes-created 0
   :nodes-active 0
   :nodes-superseded 0
   :raw-tokens 0
   :compressed-tokens 0})

(defn create-session-kg!
  "Create a new session for an agent execution."
  [agent-id task]
  (delegate-or-noop :cr/create! noop-session
                    [agent-id task]))

(defn compress-turn!
  "Compress a turn's messages into session nodes."
  [session messages]
  (if session
    (delegate-or-noop :cr/compress! 0 [session messages])
    0))

(defn reconstruct-context
  "Reconstruct a compact context prompt from the session."
  [session & [opts]]
  (if session
    (delegate-or-noop :cr/reconstruct ""
                      (if opts [session opts] [session]))
    ""))

(defn build-compressed-messages
  "Build the message array for the next LLM call using context compression."
  [session system-prompt all-messages recent-msgs & [opts]]
  (if session
    (delegate-or-noop :cr/messages all-messages
                      (if opts
                        [session system-prompt recent-msgs opts]
                        [session system-prompt recent-msgs]))
    all-messages))

(defn promote-to-global!
  "Promote valuable session nodes to the global store."
  [session global-store & [opts]]
  (if session
    (delegate-or-noop :cr/promote! {:promoted 0 :edges-created 0}
                      (if opts [session global-store opts] [session global-store]))
    {:promoted 0 :edges-created 0}))

(defn promotable-nodes
  "Get nodes from session worth promoting to global store."
  [session & [opts]]
  (if session
    (delegate-or-noop :cr/promotable []
                      (if opts [session opts] [session]))
    []))

(defn session-stats
  "Get compression statistics for the session."
  [session]
  (if session
    (delegate-or-noop :cr/stats noop-stats [session])
    noop-stats))

(defn close-session!
  "Close a session and release resources."
  [session]
  (if session
    (delegate-or-noop :cr/close! noop-stats [session])
    noop-stats))

(defn compression-available?
  "Check if compressed context reconstruction is available."
  []
  (ext/extension-available? :cr/create!))
