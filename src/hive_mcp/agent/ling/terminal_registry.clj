(ns hive-mcp.agent.ling.terminal-registry
  "Registry mapping terminal-mode keywords to ITerminalAddon instances.

   Addons register during initialize! lifecycle:
     (register-terminal! :opencode my-terminal-addon)

   Core resolves at spawn time:
     (resolve-terminal-strategy :opencode)
     ;; => TerminalAddonStrategy wrapping the addon (implements ILingStrategy)

   Thread-safety: atom + swap! (all operations atomic).
   Idempotent: Re-registering the same key replaces silently (last-write-wins)."
  (:require [hive-mcp.addons.terminal :as terminal]
            [hive-mcp.agent.ling.terminal-addon-strategy :as terminal-strat]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registry State
;; =============================================================================

;; Maps keyword terminal-id -> ITerminalAddon instance.
(defonce ^:private registry (atom {}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn register-terminal!
  "Register a terminal addon under a keyword identifier.
   Validates that addon satisfies ITerminalAddon protocol.
   Idempotent: re-registration replaces the previous addon (last-write-wins).

   Returns {:registered? true  :terminal-id id} on success,
           {:registered? false :terminal-id id :errors [...]} on failure."
  [terminal-id terminal-addon]
  {:pre [(keyword? terminal-id)]}
  (if-not (satisfies? terminal/ITerminalAddon terminal-addon)
    (do (log/warn "Cannot register terminal: does not satisfy ITerminalAddon"
                  {:terminal-id terminal-id})
        {:registered? false
         :terminal-id terminal-id
         :errors ["Object does not satisfy ITerminalAddon protocol"]})
    (do (swap! registry assoc terminal-id terminal-addon)
        (log/info "Terminal registered" {:terminal-id terminal-id})
        {:registered? true
         :terminal-id terminal-id})))

(defn resolve-terminal-strategy
  "Look up terminal-id in registry and wrap as ILingStrategy.
   Returns a TerminalAddonStrategy instance, or nil if not found."
  [terminal-id]
  (when-let [addon (get @registry terminal-id)]
    (terminal-strat/->terminal-addon-strategy addon)))

(defn registered-terminals
  "Return the set of registered terminal-id keywords."
  []
  (set (keys @registry)))

(defn get-terminal-addon
  "Get the raw ITerminalAddon instance for a terminal-id, or nil."
  [terminal-id]
  (get @registry terminal-id))

(defn deregister-terminal!
  "Remove a terminal from the registry (for addon shutdown).
   Safe to call with unregistered terminal-id (no-op).
   Returns {:deregistered? bool :terminal-id id}."
  [terminal-id]
  (let [had-entry? (contains? @registry terminal-id)]
    (swap! registry dissoc terminal-id)
    (when had-entry?
      (log/info "Terminal deregistered" {:terminal-id terminal-id}))
    {:deregistered? had-entry?
     :terminal-id terminal-id}))

(defn clear-registry!
  "Reset the terminal registry. Intended for testing only."
  []
  (reset! registry {})
  nil)
