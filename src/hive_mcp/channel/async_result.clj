(ns hive-mcp.channel.async-result
  "Async tool result buffer for piggyback delivery.

   Problem: MCP tool calls block the JSON-RPC thread. Slow operations
   (Chroma search, KG traversal, Ollama embeddings) block the LLM for 2-30s.

   Solution: When a tool call includes `async: true`, the handler middleware
   returns an immediate ack, spawns a future for the real work, and results
   drain via piggyback on subsequent tool calls.

   Buffer keyed by [agent-id project-id] so each agent+project gets independent
   delivery. Uses cursor+budget pattern mirroring memory_piggyback.clj.

   Design:
   - enqueue-result! appends completed results with content-hash dedup
   - drain! returns results within 32K char budget, advances cursor
   - 5-min TTL GC prevents memory leaks from abandoned async tasks
   - poll-task returns specific task status for explicit polling"
  (:require [taoensso.timbre :as log])
  (:import [java.time Instant]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const drain-char-budget
  "Max chars per drain batch. ~32K chars = ~8K tokens.
   Matches memory_piggyback.clj budget for consistency."
  32000)

(def ^:const ttl-seconds
  "Time-to-live for async results in seconds.
   Results older than this are garbage collected to prevent memory leaks
   from abandoned tasks (e.g., client disconnected before draining)."
  300) ;; 5 minutes

;; =============================================================================
;; Buffer State
;; =============================================================================

(defonce ^{:doc "Map of [agent-id project-id] -> {:entries [...] :cursor 0}
                 Each entry: {:task-id :tool :status :result :timestamp :content-hash}"}
  buffers
  (atom {}))

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- now-epoch-seconds
  "Current epoch seconds for TTL comparison."
  []
  (.getEpochSecond (Instant/now)))

(defn- content-hash
  "Generate content hash for dedup. Uses Clojure hash for speed."
  [result-map]
  (hash (select-keys result-map [:task-id :tool :status :result])))

(defn- expired?
  "Check if an entry has expired based on TTL."
  [entry now-secs]
  (> (- now-secs (:timestamp entry 0)) ttl-seconds))

;; =============================================================================
;; Garbage Collection
;; =============================================================================

(defn gc-expired!
  "Remove expired entries from all buffers.
   Adjusts cursors when entries before cursor are removed.
   Removes empty buffers entirely.
   Returns count of entries removed."
  []
  (let [now-secs (now-epoch-seconds)
        removed (atom 0)]
    (swap! buffers
           (fn [bufs]
             (reduce-kv
              (fn [acc buffer-key {:keys [entries cursor] :as buf}]
                (let [live-entries (vec (remove #(expired? % now-secs) entries))
                      removed-count (- (count entries) (count live-entries))
                      ;; Adjust cursor: count how many removed entries were before cursor
                      removed-before-cursor (count (filter
                                                    (fn [idx]
                                                      (expired? (nth entries idx) now-secs))
                                                    (range (min cursor (count entries)))))
                      new-cursor (max 0 (- cursor removed-before-cursor))]
                  (swap! removed + removed-count)
                  (if (empty? live-entries)
                    acc ;; Remove empty buffer entirely
                    (assoc acc buffer-key
                           (assoc buf
                                  :entries live-entries
                                  :cursor new-cursor)))))
              {}
              bufs)))
    (let [total @removed]
      (when (pos? total)
        (log/info "async-result: GC removed" total "expired entries"))
      total)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn enqueue-result!
  "Enqueue a completed async result into the buffer.

   Result-map should contain:
   - :task-id  - unique task identifier (e.g. \"atask-<uuid>\")
   - :tool     - tool name that produced the result
   - :status   - :completed or :error
   - :result   - the actual result data (for :completed)
   - :error    - error message (for :error)

   Idempotent: content-hash dedup prevents re-enqueue of identical results."
  [agent-id project-id result-map]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]
        entry (assoc result-map
                     :timestamp (now-epoch-seconds)
                     :content-hash (content-hash result-map))]
    (swap! buffers update buffer-key
           (fn [buf]
             (let [buf (or buf {:entries [] :cursor 0})
                   ;; Dedup: check if content-hash already exists
                   existing-hashes (set (map :content-hash (:entries buf)))]
               (if (contains? existing-hashes (:content-hash entry))
                 (do
                   (log/debug "async-result: dedup skip for task" (:task-id result-map))
                   buf)
                 (update buf :entries conj entry)))))
    (log/info "async-result: enqueued result for task" (:task-id result-map)
              "tool:" (:tool result-map) "status:" (:status result-map)
              "buffer:" buffer-key)))

(defn drain!
  "Drain next batch of async results within char budget for an agent+project.

   Returns map:
   {:results [...entries...]
    :remaining N    ;; entries still pending after cursor
    :total M        ;; total entries in buffer
    :delivered D    ;; entries delivered so far (including this batch)
    :done true}     ;; present only when all entries drained

   Returns nil if no pending entries past cursor.
   Cleans up buffer when all entries are drained and expired entries GC'd."
  [agent-id project-id]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]
        buf (get @buffers buffer-key)]
    (when (and buf (< (:cursor buf) (count (:entries buf))))
      (let [{:keys [entries cursor]} buf
            total (count entries)
            ;; Collect entries within char budget
            [batch new-cursor]
            (loop [batch []
                   chars 0
                   idx cursor]
              (if (>= idx total)
                [batch idx]
                (let [entry (nth entries idx)
                      ;; Format for output (strip internal fields)
                      output-entry (dissoc entry :timestamp :content-hash)
                      entry-str (pr-str output-entry)
                      entry-chars (count entry-str)
                      new-chars (+ chars entry-chars)]
                  (if (and (seq batch) (> new-chars drain-char-budget))
                    ;; Over budget and we have at least one entry
                    [batch idx]
                    ;; Add entry (always add at least one even if over budget)
                    (recur (conj batch output-entry)
                           new-chars
                           (inc idx))))))
            is-done (>= new-cursor total)
            delivered new-cursor
            remaining (- total new-cursor)]
        ;; Update buffer state
        (if is-done
          ;; All delivered - remove buffer
          (swap! buffers dissoc buffer-key)
          ;; Advance cursor
          (swap! buffers assoc buffer-key
                 (assoc buf :cursor new-cursor)))
        (cond-> {:results batch
                 :remaining remaining
                 :total total
                 :delivered delivered}
          is-done (assoc :done true))))))

(defn has-pending?
  "Check if an agent+project has undrained async results."
  [agent-id project-id]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]
        buf (get @buffers buffer-key)]
    (and (some? buf)
         (< (:cursor buf) (count (:entries buf))))))

(defn poll-task
  "Get status of a specific async task.

   Returns the entry map if found, nil otherwise.
   Useful for explicit polling by task-id when client wants
   to check a specific async operation.

   Searches across all buffers (task-id is globally unique)."
  [task-id]
  (some (fn [[_buffer-key {:keys [entries]}]]
          (some #(when (= task-id (:task-id %))
                   (dissoc % :timestamp :content-hash))
                entries))
        @buffers))

(defn clear-buffer!
  "Clear buffer for a specific agent+project. For testing."
  [agent-id project-id]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]]
    (swap! buffers dissoc buffer-key)))

(defn reset-all!
  "Reset all buffers. For testing."
  []
  (reset! buffers {}))

(defn stats
  "Get buffer statistics. For monitoring/debugging."
  []
  (let [bufs @buffers]
    {:buffer-count (count bufs)
     :total-entries (reduce + 0 (map (comp count :entries val) bufs))
     :pending-entries (reduce + 0
                              (map (fn [[_ {:keys [entries cursor]}]]
                                     (- (count entries) cursor))
                                   bufs))
     :buffers (into {}
                    (map (fn [[k {:keys [entries cursor]}]]
                           [k {:entries (count entries)
                               :cursor cursor
                               :pending (- (count entries) cursor)}])
                         bufs))}))
