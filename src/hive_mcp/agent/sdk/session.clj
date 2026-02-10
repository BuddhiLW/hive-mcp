(ns hive-mcp.agent.sdk.session
  "SDK session registry for tracking active agent SDK sessions.

   Manages the lifecycle state of SDK sessions including:
   - Registration and unregistration
   - State updates (phase transitions, turn counts)
   - Session lookup and existence checks
   - Ling ID to Python-safe identifier conversion

   Session data structure:
   {:ling-id             String
    :phase               keyword (:idle :silence :abstract :act)
    :phase-history       [{:phase kw :started-at inst :ended-at inst}]
    :observations        vector (collected during silence phase)
    :plan                string (produced during abstract phase)
    :compressed-context  string (compressed observations from last phase transition)
    :message-ch          core.async channel (streaming messages)
    :result-ch           core.async channel (final result)
    :turn-count          int (number of query() calls)
    :client-ref          String (Python global var name for client)
    :py-loop-var         String (Python global var name for event loop)
    :py-safe-id          String (Python-safe ling identifier)
    :started-at          long
    :cwd                 string
    :system-prompt       string}"
  (:require [clojure.core.async :refer [close!]]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private session-registry (atom {}))

(defn register-session!
  "Register a new SDK session."
  [ling-id session-data]
  (swap! session-registry assoc ling-id session-data)
  (log/info "[sdk.session] Session registered" {:ling-id ling-id
                                                :phase (:phase session-data)}))

(defn update-session!
  "Update an existing SDK session."
  [ling-id updates]
  (swap! session-registry update ling-id merge updates))

(defn unregister-session!
  "Remove a session from registry."
  [ling-id]
  (when-let [session (get @session-registry ling-id)]
    ;; Close channels
    (when-let [ch (:message-ch session)] (close! ch))
    (when-let [ch (:result-ch session)] (close! ch)))
  (swap! session-registry dissoc ling-id)
  (log/info "[sdk.session] Session unregistered" {:ling-id ling-id}))

(defn get-session
  "Get session data for a ling."
  [ling-id]
  (get @session-registry ling-id))

(defn session-registry-ref
  "Return the session registry atom. For internal use by lifecycle functions."
  []
  session-registry)

(defn ling-id->safe-id
  "Convert a ling-id to a Python-safe identifier.
   Replaces non-alphanumeric chars with underscores to use as Python variable suffix."
  [ling-id]
  (str/replace ling-id #"[^a-zA-Z0-9_]" "_"))
