(ns hive-mcp.channel.memory-piggyback
  "Memory piggyback channel for incremental delivery of axioms and conventions.

   Problem: /catchup returns follow-up-queries telling the LLM to manually fetch
   axioms and priority conventions. This is unreliable (LLM may skip) and when it
   works, returns ~278K in one response (token bomb).

   Solution: Buffer entries from catchup and drain them incrementally via
   ---MEMORY--- blocks appended to subsequent MCP tool responses, using a cursor
   to ensure once-delivery.

   Buffer keyed by [agent-id project-id] so each agent+project gets independent
   delivery. Axioms are enqueued first (highest priority).

   Design:
   - enqueue! is idempotent: repeated /catchup calls don't re-enqueue
   - drain! returns entries within a 32K char budget (~8K tokens)
   - Cursor tracks delivery position; :done true on last batch"
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const drain-char-budget
  "Max chars per drain batch. ~32K chars = ~8K tokens.
   Prevents token bombs while delivering in 2-3 batches."
  32000)

;; =============================================================================
;; Buffer State
;; =============================================================================

(defonce ^{:doc "Map of [agent-id project-id] -> {:entries [...] :cursor 0 :done false}
                 Entries are compact maps: {:id :T :S :C :tags}"}
  buffers
  (atom {}))

;; =============================================================================
;; Entry Formatting
;; =============================================================================

(defn- format-entry
  "Convert a catchup entry to compact piggyback format.
   Keeps only essential fields to minimize token usage."
  [entry]
  (cond-> {:id (:id entry)
           :T (or (:type entry) "note")
           :C (or (:content entry) (:preview entry) "")}
    (:severity entry) (assoc :S (:severity entry))
    (seq (:tags entry)) (assoc :tags (vec (:tags entry)))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn enqueue!
  "Enqueue entries into the memory piggyback buffer for incremental delivery.

   Idempotent: if buffer already exists for this agent+project, this is a no-op.
   This prevents repeated /catchup calls from re-enqueuing entries.

   Entries should be ordered: axioms first, then priority conventions.
   Each entry should have at minimum :id and :content (or :preview).

   Optional context-refs: map of category->ctx-id from context-store.
   When provided, refs are included in the first drain batch to enable
   future :ref mode where piggyback sends only refs instead of full content."
  ([agent-id project-id entries]
   (enqueue! agent-id project-id entries nil))
  ([agent-id project-id entries context-refs]
   (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]]
     (if (get @buffers buffer-key)
       (log/debug "memory-piggyback: buffer already exists for" buffer-key "- skipping enqueue")
       (let [formatted (mapv format-entry entries)]
         (swap! buffers assoc buffer-key
                (cond-> {:entries formatted
                         :cursor 0
                         :done false
                         :seq-num 0}
                  (some? context-refs)
                  (assoc :context-refs context-refs)))
         (log/info "memory-piggyback: enqueued" (count formatted) "entries for" buffer-key
                   (when context-refs (str " with " (count context-refs) " context-refs"))))))))

(defn drain!
  "Drain next batch of entries within char budget for an agent+project.

   Returns map:
   {:batch [...entries...]
    :remaining N    ;; entries still pending
    :total M        ;; total entries enqueued
    :delivered D    ;; entries delivered so far (including this batch)
    :seq S}         ;; sequence number of this drain

   Last batch includes :done true. After that, buffer is cleaned up.
   Returns nil if no pending entries."
  [agent-id project-id]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]
        buf (get @buffers buffer-key)]
    (when (and buf (not (:done buf)))
      (let [{:keys [entries cursor seq-num]} buf
            total (count entries)
            ;; Collect entries within char budget
            [batch new-cursor]
            (loop [batch []
                   chars 0
                   idx cursor]
              (if (>= idx total)
                [batch idx]
                (let [entry (nth entries idx)
                      entry-str (pr-str entry)
                      entry-chars (count entry-str)
                      new-chars (+ chars entry-chars)]
                  (if (and (seq batch) (> new-chars drain-char-budget))
                    ;; Over budget and we have at least one entry
                    [batch idx]
                    ;; Add entry (always add at least one even if over budget)
                    (recur (conj batch entry)
                           new-chars
                           (inc idx))))))
            is-done (>= new-cursor total)
            new-seq (inc seq-num)
            delivered new-cursor
            remaining (- total new-cursor)]
        ;; Update buffer state
        (if is-done
          ;; Clean up buffer when done
          (swap! buffers dissoc buffer-key)
          ;; Advance cursor
          (swap! buffers assoc buffer-key
                 {:entries entries
                  :cursor new-cursor
                  :done false
                  :seq-num new-seq}))
        (cond-> {:batch batch
                 :remaining remaining
                 :total total
                 :delivered delivered
                 :seq new-seq}
          is-done (assoc :done true)
          ;; Include context-refs on FIRST drain only (seq-num was 0 → new-seq is 1)
          ;; This enables :ref mode where consumers can use refs instead of full content
          (and (= new-seq 1) (some? (:context-refs buf)))
          (assoc :context-refs (:context-refs buf)))))))

(defn has-pending?
  "Check if an agent+project has undrained memory entries."
  [agent-id project-id]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]
        buf (get @buffers buffer-key)]
    (and (some? buf) (not (:done buf)))))

(defn clear-buffer!
  "Clear buffer for a specific agent+project. For testing."
  [agent-id project-id]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]]
    (swap! buffers dissoc buffer-key)))

(defn reset-all!
  "Reset all buffers. For testing."
  []
  (clojure.core/reset! buffers {}))
