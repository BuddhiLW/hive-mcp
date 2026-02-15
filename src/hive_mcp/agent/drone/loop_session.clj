(ns hive-mcp.agent.drone.loop-session
  "Enhanced session lifecycle management for the agentic drone loop.

   Isolates KG session side-effects: create, seed, promote, close.
   Uses rescue macro to eliminate try/catch ceremony for non-fatal operations."
  (:require [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.kg-session :as kg-session]
            [hive-mcp.protocols.kg]
            [hive-dsl.result :refer [rescue]]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Enhanced Session Lifecycle
;; =============================================================================

(defn create-enhanced-session
  "Create an enhanced KG session if compression is available.
   Returns session or nil (never throws)."
  [drone-id task]
  (when (kg-session/compression-available?)
    (rescue nil (kg-session/create-session-kg! drone-id task))))

(defn seed-session-store!
  "Seed a session store from global KG context. Non-fatal on error."
  [session-store task files cwd]
  (when (and session-store (hive-mcp.protocols.kg/store-set?))
    (rescue nil
            (session-kg/seed-from-global!
             session-store
             (hive-mcp.protocols.kg/get-store)
             {:task task :files files :cwd cwd}))))

(defn promote-enhanced-session!
  "Promote completed session facts to global KG. Non-fatal on error."
  [enhanced-session status]
  (when (and enhanced-session
             (= status :completed)
             (hive-mcp.protocols.kg/store-set?))
    (let [result (rescue nil
                         (kg-session/promote-to-global!
                          enhanced-session
                          (hive-mcp.protocols.kg/get-store)))]
      (when result
        (log/debug "Enhanced session promotion result" result)))))

(defn close-enhanced-session!
  "Close an enhanced session. Non-fatal on error."
  [enhanced-session]
  (when enhanced-session
    (let [stats (rescue nil (kg-session/close-session! enhanced-session))]
      (when stats
        (log/debug "Enhanced session closed" stats)))))

(defn finalize-enhanced-session!
  "Complete enhanced session lifecycle: promote (if completed) then close."
  [enhanced-session status]
  (when enhanced-session
    (promote-enhanced-session! enhanced-session status)
    (close-enhanced-session! enhanced-session)))

(defn compress-turn-safely!
  "Compress a turn in the enhanced session. Non-fatal on error."
  [enhanced-session messages]
  (when enhanced-session
    (rescue nil (kg-session/compress-turn! enhanced-session messages))))

;; =============================================================================
;; Session Recording Helpers
;; =============================================================================

(defn record-text-reasoning!
  "Record a text-only response reasoning event. Non-fatal on error."
  [session-store turn content]
  (when session-store
    (rescue nil
            (session-kg/record-reasoning!
             session-store turn
             "Final response (text-only)"
             (subs content 0 (min 200 (count content)))))))

(defn record-tool-observations!
  "Record tool call observations in session store. Returns updated obs-count."
  [session-store turn calls tool-results obs-count task-spec]
  (if-not session-store
    obs-count
    (do
      (doseq [[call result-msg] (map vector calls tool-results)]
        (let [tool-name (:name call)
              content (or (:content result-msg) "")
              success? (not (str/starts-with? content "Error:"))]
          (rescue nil
                  (session-kg/record-observation!
                   session-store turn tool-name
                   {:success success?
                    :result {:text content}
                    :error (when-not success? content)}
                   {:file (first (:files task-spec))}))))
      (rescue nil
              (session-kg/record-reasoning!
               session-store turn
               (str "Execute tools: " (str/join ", " (mapv :name calls)))
               "LLM requested tool execution"))
      (+ obs-count (count calls)))))

(defn record-error-observation!
  "Record an LLM error observation. Non-fatal on error."
  [session-store turn error-msg]
  (when session-store
    (rescue nil
            (session-kg/record-observation!
             session-store turn "llm-error"
             {:success false :error error-msg}))))

;; =============================================================================
;; Message Building
;; =============================================================================

(defn build-turn-messages
  "Build messages for a loop turn, using enhanced session compression when available."
  [session-store enhanced-session system-prompt task turn]
  (let [context-prompt (if (and session-store (pos? turn))
                         (session-kg/reconstruct-context session-store task turn)
                         task)
        base-messages [{:role "system" :content system-prompt}
                       {:role "user" :content context-prompt}]]
    (if (and enhanced-session (pos? turn))
      (kg-session/build-compressed-messages
       enhanced-session system-prompt base-messages base-messages)
      base-messages)))
