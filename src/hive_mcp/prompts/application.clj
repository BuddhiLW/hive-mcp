(ns hive-mcp.prompts.application
  "Application layer for permission prompts.
   
   Manages the lifecycle of permission prompts from emission to resolution.
   Acts as the coordination point between:
   - Domain logic (permissions.clj tier classification)
   - Infrastructure (infra.clj D-Bus notifications)
   - Channel events (for UI updates)
   
   State is managed via the pending-prompts atom, which stores prompts
   awaiting human or coordinator decision.
   
   Usage:
     ;; Emit a new prompt (typically called from permissions.clj)
     (emit-prompt! \"agent-1\" \"file_edit\" {:file_path \"src/foo.clj\"} :tier-2 \"Edit source file\")
     
     ;; Poll pending prompts
     (poll-prompts)        ; all pending
     (poll-prompts :tier-3) ; only tier-3
     
     ;; Respond to a prompt
     (respond-prompt! \"prompt-123\" true :coordinator)"
  (:require [hive-mcp.prompts.infra :as infra]
            [hive-mcp.channel :as channel]
            [hive-mcp.guards :as guards]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; State Management
;;; ============================================================

(defonce ^{:doc "Map of prompt-id -> prompt-data for pending permission prompts.
                 
                 Prompt structure:
                   {:id          \"prompt-<timestamp>-<random>\"
                    :agent-id    \"ling-worker-1\"
                    :tool-name   \"file_edit\"
                    :arguments   {:file_path \"...\" :content \"...\"}
                    :tier        :tier-2 | :tier-3
                    :summary     \"Human-readable description\"
                    :created-at  <epoch-ms>
                    :status      :pending}"}
  pending-prompts
  (atom {}))

;;; ============================================================
;;; Helpers
;;; ============================================================

(defn- generate-prompt-id
  "Generate unique prompt ID using timestamp and random suffix."
  []
  (str "prompt-" (System/currentTimeMillis) "-" (rand-int 10000)))

(defn- current-timestamp
  "Return current epoch milliseconds."
  []
  (System/currentTimeMillis))

;;; ============================================================
;;; Core Functions
;;; ============================================================

(defn emit-prompt!
  "Emit a new permission prompt requiring human/coordinator decision.
   
   Stores the prompt in pending-prompts, sends D-Bus desktop notification,
   and emits a channel event for connected UIs.
   
   Args:
     agent-id  - Agent requesting permission (e.g., \"ling-worker-1\")
     tool-name - Tool being requested (e.g., \"file_edit\", \"bash\")
     arguments - Tool arguments map
     tier      - Permission tier (:tier-2 for coordinator, :tier-3 for human)
     summary   - Human-readable summary of what's being requested
   
   Returns:
     The prompt-id (string) of the created prompt.
   
   Side effects:
     - Stores prompt in pending-prompts atom
     - Sends D-Bus notification via infra layer
     - Emits :prompt-emitted channel event"
  [agent-id tool-name arguments tier summary]
  (let [prompt-id (generate-prompt-id)
        prompt {:id prompt-id
                :agent-id agent-id
                :tool-name tool-name
                :arguments arguments
                :tier tier
                :summary summary
                :created-at (current-timestamp)
                :status :pending}]
    ;; Store in pending prompts
    (swap! pending-prompts assoc prompt-id prompt)
    (log/info "Prompt emitted:" prompt-id "agent:" agent-id "tool:" tool-name "tier:" tier)

    ;; Send D-Bus notification
    (infra/notify-permission-request! agent-id tool-name summary)

    ;; Emit channel event for UI updates
    (try
      (channel/emit-event! :prompt-emitted
                           {:prompt-id prompt-id
                            :agent-id agent-id
                            :tool-name tool-name
                            :tier tier
                            :summary summary})
      (catch Exception e
        (log/debug "Channel emit failed (may not be running):" (.getMessage e))))

    prompt-id))

(defn poll-prompts
  "Get all pending prompts, optionally filtered by tier.
   
   Args:
     tier - (optional) Filter by :tier-2 or :tier-3
   
   Returns:
     Sequence of prompt maps sorted by created-at (oldest first)."
  ([]
   (->> (vals @pending-prompts)
        (sort-by :created-at)))
  ([tier]
   (->> (vals @pending-prompts)
        (filter #(= tier (:tier %)))
        (sort-by :created-at))))

(defn get-prompt
  "Get a specific prompt by ID.
   
   Args:
     prompt-id - The prompt ID to look up
   
   Returns:
     The prompt map, or nil if not found."
  [prompt-id]
  (get @pending-prompts prompt-id))

(defn respond-prompt!
  "Respond to a pending prompt, resolving it.
   
   Removes the prompt from pending-prompts and emits a channel event
   with the resolution details.
   
   Args:
     prompt-id - ID of prompt to respond to
     approved? - boolean, whether the action was approved
     responder - Who responded: :auto, :coordinator, or :human
   
   Returns:
     The resolved prompt with :response added, containing:
       {:approved    boolean
        :responder   keyword
        :responded-at epoch-ms}
     Returns nil if prompt-id not found."
  [prompt-id approved? responder]
  (when-let [prompt (get @pending-prompts prompt-id)]
    (let [response {:approved approved?
                    :responder responder
                    :responded-at (current-timestamp)}
          resolved-prompt (-> prompt
                              (assoc :status :resolved)
                              (assoc :response response))]
      ;; Remove from pending
      (swap! pending-prompts dissoc prompt-id)
      (log/info "Prompt resolved:" prompt-id "approved:" approved? "by:" responder)

      ;; Emit channel event
      (try
        (channel/emit-event! :prompt-resolved
                             {:prompt-id prompt-id
                              :agent-id (:agent-id prompt)
                              :tool-name (:tool-name prompt)
                              :approved approved?
                              :responder responder})
        (catch Exception e
          (log/debug "Channel emit failed:" (.getMessage e))))

      resolved-prompt)))

;;; ============================================================
;;; Query Functions
;;; ============================================================

(defn pending-count
  "Get count of pending prompts, optionally by tier."
  ([] (count @pending-prompts))
  ([tier] (count (poll-prompts tier))))

(defn prompt-ids
  "Get all pending prompt IDs."
  []
  (keys @pending-prompts))

(defn prompts-by-agent
  "Get all pending prompts for a specific agent."
  [agent-id]
  (->> (vals @pending-prompts)
       (filter #(= agent-id (:agent-id %)))
       (sort-by :created-at)))

;;; ============================================================
;;; Cleanup
;;; ============================================================

(defn clear-prompts!
  "Clear all pending prompts. Use with caution - mainly for testing."
  []
  (let [count (pending-count)]
    (reset! pending-prompts {})
    (log/warn "Cleared all pending prompts:" count)
    count))

(defn clear-pending-prompts!
  "Clear pending prompts. GUARDED - no-op if coordinator running.

   CLARITY-Y: Yield safe failure - prevents test fixtures from
   corrupting production prompt state."
  []
  (guards/when-not-coordinator
   "clear-pending-prompts! called"
   (reset! pending-prompts {})))

(defn expire-old-prompts!
  "Remove prompts older than the given age in milliseconds.
   Returns count of expired prompts."
  [max-age-ms]
  (let [cutoff (- (current-timestamp) max-age-ms)
        expired-ids (->> (vals @pending-prompts)
                         (filter #(< (:created-at %) cutoff))
                         (map :id))]
    (doseq [id expired-ids]
      (swap! pending-prompts dissoc id)
      (log/info "Expired old prompt:" id))
    (count expired-ids)))
