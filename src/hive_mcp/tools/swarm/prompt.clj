(ns hive-mcp.tools.swarm.prompt
  "Swarm prompt handlers - pending prompts and response.

   Handles human-in-the-loop prompt management when prompt-mode is 'human'.
   Allows coordinator to view and respond to pending permission prompts.

   SOLID: SRP - Single responsibility for prompt management.
   CLARITY: I - Inputs validated for slave_id and response."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; ============================================================
;; Pending Prompts Handler
;; ============================================================

(defn handle-swarm-pending-prompts
  "Get list of pending prompts awaiting human decision.
   Only relevant when prompt-mode is 'human'.

   Returns list of prompts with slave IDs and content."
  [_]
  (core/with-swarm
    (let [{:keys [success result error timed-out]}
          (ec/eval-elisp-with-timeout
           "(json-encode (hive-mcp-swarm-api-pending-prompts))" 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Pending prompts check")

        success
        (core/mcp-success result)

        :else
        (core/mcp-error (str "Error: " error))))))

;; ============================================================
;; Respond Prompt Handler
;; ============================================================

(defn handle-swarm-respond-prompt
  "Send a response to a pending prompt from a specific slave.
   Use this to answer permission prompts when prompt-mode is 'human'.

   Parameters:
   - slave_id: ID of the slave whose prompt to respond to (required)
   - response: Response to send (required)

   CLARITY: I - Inputs validated (slave_id and response required)"
  [{:keys [slave_id response]}]
  (core/with-swarm
    (let [elisp (format "(json-encode (hive-mcp-swarm-api-respond-prompt \"%s\" \"%s\"))"
                        (v/escape-elisp-string slave_id)
                        (v/escape-elisp-string response))
          {:keys [success result error timed-out]}
          (ec/eval-elisp-with-timeout elisp 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Respond prompt" :extra-data {:slave_id slave_id})

        success
        (core/mcp-success result)

        :else
        (core/mcp-error (str "Error: " error))))))
