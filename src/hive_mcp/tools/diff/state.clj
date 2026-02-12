(ns hive-mcp.tools.diff.state
  "Diff state management: atoms, error helpers, TDD status."
  (:require [hive-mcp.server.guards :as guards]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn mcp-error-json
  "Create an error MCP response with JSON-encoded error message."
  [error-message]
  {:type "text"
   :text (json/write-str {:error error-message})
   :isError true})

(defonce pending-diffs (atom {}))

(def default-auto-approve-rules
  "Default rules for auto-approving diff proposals."
  {:max-lines-changed 100
   :no-deletions-only true
   :require-description true
   :allowed-path-patterns [#".*\.clj[sx]?$"
                           #".*\.edn$"
                           #".*\.md$"
                           #".*\.json$"]})

(defonce auto-approve-rules (atom default-auto-approve-rules))

(defn clear-pending-diffs!
  "Clear pending diffs (guarded, no-op if coordinator running)."
  []
  (guards/when-not-coordinator
   "clear-pending-diffs! called"
   (reset! pending-diffs {})))

(defn update-diff-tdd-status!
  "Update a diff's TDD status after drone runs tests/lint."
  [diff-id tdd-status]
  (when (contains? @pending-diffs diff-id)
    (swap! pending-diffs update diff-id
           (fn [diff]
             (update diff :tdd-status
                     (fn [existing]
                       (merge existing tdd-status)))))
    (log/info "Updated diff TDD status" {:diff-id diff-id :tdd-status tdd-status})
    (get @pending-diffs diff-id)))
