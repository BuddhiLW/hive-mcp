(ns hive-mcp.tools.consolidated.workflow.readiness
  "Agent readiness checking for spawn workflows.

   Provides spawn-mode-dispatched readiness checks:
   - vterm: Emacs buffer ready for input
   - headless: subprocess alive with stdout
   - agent-sdk: session idle with event loop thread
   - openrouter: always ready (stateless API)

   Extracted from workflow.clj to reduce cyclomatic complexity."
  (:require [hive-mcp.emacs.client :as ec]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.dns.result :as result]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ling-ready-timeout-ms 5000)

(def ^:private ling-ready-poll-ms 50)

;; ── Per-Mode Readiness Checks ───────────────────────────────────────────────

(defn vterm-ready?
  "Check if a vterm ling's CLI is ready for input."
  [agent-id]
  (result/rescue false
                 (let [elisp (format "(if (hive-mcp-swarm--slave-ready-p \"%s\") \"t\" \"nil\")" agent-id)
                       result (ec/eval-elisp-with-timeout elisp 2000)]
                   (and (:success result)
                        (= "t" (:result result))))))

(defn headless-ready?
  "Check if a headless ling's process is alive and has produced stdout."
  [agent-id]
  (result/rescue false
                 (when-let [status (headless/headless-status agent-id)]
                   (and (:alive? status)
                        (pos? (get-in status [:stdout :total-lines-seen] 0))))))

(defn agent-sdk-ready?
  "Check if an agent-sdk ling's session is idle and event loop thread is alive."
  [agent-id]
  (result/rescue false
                 (when-let [get-session-fn (requiring-resolve 'hive-claude.sdk.session/get-session)]
                   (when-let [sess (get-session-fn agent-id)]
                     (and (= :idle (:phase sess))
                          (some? (:client-ref sess))
                          (let [safe-id (:py-safe-id sess)]
                            (if safe-id
                              (when-let [py-get-fn (requiring-resolve 'hive-claude.sdk.python/py-get-global)]
                                (let [thread-obj (py-get-fn (str "_hive_loop_thread_" safe-id))]
                                  (when thread-obj
                                    (when-let [py-call-fn (requiring-resolve 'hive-claude.sdk.python/py-call)]
                                      (boolean (py-call-fn thread-obj "is_alive"))))))
                              false)))))))

;; ── Mode Dispatch ───────────────────────────────────────────────────────────

(defn ling-cli-ready?
  "Mode-dispatch readiness check for a ling's CLI.
   Checks terminal modes via Emacs, headless via process/SDK status."
  [agent-id spawn-mode]
  (case spawn-mode
    :headless        (headless-ready? agent-id)
    :claude-process  (headless-ready? agent-id)
    :openrouter      true
    :agent-sdk       (agent-sdk-ready? agent-id)
    :claude-sdk      (agent-sdk-ready? agent-id)
    ;; default: claude / vterm (both Emacs-bound)
    (:claude :vterm) (vterm-ready? agent-id)
    (vterm-ready? agent-id)))

;; ── Polling Loop ────────────────────────────────────────────────────────────

(defn wait-for-ling-ready
  "Poll for ling readiness before dispatching (two-phase: DataScript + CLI)."
  [agent-id spawn-mode]
  (let [start-ms (System/currentTimeMillis)]
    (loop [attempt 1]
      (let [slave   (queries/get-slave agent-id)
            cli-ok? (when slave (ling-cli-ready? agent-id spawn-mode))
            elapsed (- (System/currentTimeMillis) start-ms)]
        (cond
          (and slave cli-ok?)
          (do
            (log/debug "SPARK: ling ready" {:agent-id   agent-id
                                            :attempts   attempt
                                            :elapsed-ms elapsed
                                            :spawn-mode spawn-mode})
            {:ready?     true
             :slave      slave
             :attempts   attempt
             :elapsed-ms elapsed
             :phase      :cli-ready})

          (>= elapsed ling-ready-timeout-ms)
          (do
            (log/warn "SPARK: ling readiness timeout"
                      {:agent-id   agent-id
                       :attempts   attempt
                       :elapsed-ms elapsed
                       :spawn-mode spawn-mode
                       :ds-found?  (some? slave)
                       :phase      (if slave :cli-timeout :ds-timeout)})
            {:ready?     false
             :slave      slave
             :attempts   attempt
             :elapsed-ms elapsed
             :phase      (if slave :cli-timeout :ds-timeout)})

          :else
          (do
            (Thread/sleep ling-ready-poll-ms)
            (recur (inc attempt))))))))
