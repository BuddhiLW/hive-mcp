(ns hive-mcp.tools.swarm.prompt
  "Swarm prompt handlers - pending prompts and response.

   Handles human-in-the-loop prompt management when prompt-mode is 'human'.
   Allows coordinator to view and respond to pending permission prompts.

   Emits :ling/prompt-pending events to Olympus for real-time notifications.

   SOLID: SRP - Single responsibility for prompt management.
   CLARITY: I - Inputs validated for slave_id and response."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.channel :as channel]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Pending Prompts Handler
;; ============================================================

(defn- emit-prompt-pending-events!
  "Emit :ling/prompt-pending events for each pending prompt.
   Called after fetching pending prompts to notify Olympus dashboard."
  [prompts-data]
  (try
    (let [parsed (if (string? prompts-data)
                   (json/read-str prompts-data :key-fn keyword)
                   prompts-data)
          prompts (get parsed :prompts [])]
      (doseq [prompt prompts]
        (let [slave-id (get prompt :slave-id)
              prompt-text (get prompt :prompt "")
              timestamp (get prompt :timestamp)]
          (when slave-id
            (log/debug "Emitting :ling/prompt-pending for" slave-id)
            (channel/emit-event! :ling/prompt-pending
                                 {:slave-id slave-id
                                  :prompt-preview (subs prompt-text 0 (min 100 (count prompt-text)))
                                  :pending-since timestamp})))))
    (catch Exception e
      (log/warn "Failed to emit prompt-pending events:" (ex-message e)))))

(defn handle-swarm-pending-prompts
  "Get list of pending prompts awaiting human decision.
   Only relevant when prompt-mode is 'human'.

   Returns list of prompts with slave IDs and content.
   Emits :ling/prompt-pending events to Olympus for real-time notifications."
  [_]
  (core/with-swarm
    (let [{:keys [success result error timed-out]}
          (ec/eval-elisp-with-timeout
           "(json-encode (hive-mcp-swarm-api-pending-prompts))" 5000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Pending prompts check")

        success
        (do
          ;; Emit events for Olympus notification chain
          (emit-prompt-pending-events! result)
          (core/mcp-success result))

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

;; ============================================================
;; Lazy Preset Header Builder
;; ============================================================

(def lazy-instructions
  "Static instruction template for lazy preset loading headers.
   Contains two format placeholders (%s):
     1. Comma-separated preset names (bold)
     2. Per-preset fetch commands (one per line)

   Token budget: ~190 fixed tokens + ~10 tokens per preset name.
   Total stays well under 300 tokens for typical 1-3 preset configs."
  (str
   ;; Section 1: Assigned presets header (~30 tokens)
   "## Assigned Presets\n\n"
   "You have access to: **%s**\n\n"

   ;; Section 2: IMMEDIATE fetch instruction (~50 tokens)
   "### IMMEDIATE: Fetch at Session Start\n\n"
   "**Before starting work**, fetch your assigned presets:\n"
   "```\n"
   "%s\n"
   "```\n\n"

   ;; Section 3: Quick summary option (~40 tokens)
   "### Quick Summary (Lower Tokens)\n\n"
   "For orientation without full content (~200 tokens vs ~1500):\n"
   "```\n"
   "preset(command: \"core\", name: \"<preset-name>\")\n"
   "```\n\n"

   ;; Section 4: Discovery (~40 tokens)
   "### Discovery\n\n"
   "Find presets by topic:\n"
   "```\n"
   "preset(command: \"search\", query: \"testing patterns\")\n"
   "preset(command: \"list_slim\")  ; Names + categories only\n"
   "```\n\n"

   ;; Section 5: When to fetch (~30 tokens)
   "### When to Fetch\n\n"
   "- Session start: fetch assigned presets\n"
   "- Unfamiliar task: search for relevant presets\n"
   "- Need guidance: use `core` for quick summary\n"))

(defn build-lazy-preset-header
  "Generate lightweight system prompt header with preset names + fetch instructions.
   Returns ~300 tokens instead of ~1500 tokens per preset.

   Instead of injecting full preset content, this generates compact instructions
   telling lings how to fetch presets on-demand via the consolidated preset tool.

   Uses the `lazy-instructions` constant template, filling in:
     1. Comma-separated preset names
     2. Per-preset fetch commands

   Args:
     preset-names - vector of preset name strings (e.g., [\"ling\" \"mcp-first\"])

   Returns:
     String with preset names and instructions to fetch via preset(command: 'get', name: ...)

   Token budget: ~300 tokens max (vs ~1500 per full preset)

   Example output for [\"ling\" \"tdd\"]:
     ## Assigned Presets
     You have access to these presets: ling, tdd
     ...fetch instructions..."
  [preset-names]
  (when (seq preset-names)
    (let [names-str (str/join ", " preset-names)
          fetch-cmds (str/join "\n" (map #(str "preset(command: \"get\", name: \"" % "\")") preset-names))]
      (format lazy-instructions names-str fetch-cmds))))
