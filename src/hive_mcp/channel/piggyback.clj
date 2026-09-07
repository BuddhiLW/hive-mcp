(ns hive-mcp.channel.piggyback
  "Piggyback communication — instruction queue and message cursor for hivemind↔ling messaging.

   The instruction queue side of this namespace is now a thin facade over
   `hive-mcp.channel.instruction-store`, which owns the cross-process
   (NATS-backed) delivery logic. The public functions here keep their old
   signatures and single-process behavior unchanged; when a NATS-backed
   store has been installed (via server/init), they transparently route
   pushes through the backbone and pick up remote pushes via a wildcard
   subscription."
  (:require [clojure.spec.alpha :as s]
            [hive-mcp.channel.instruction-store :as istore]
            [hive-mcp.server.guards :as guards]
            [taoensso.timbre :as log]
            [hive-mcp.channel.audience :as audience]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Specs for piggyback messages
(s/def ::agent-id string?)
(s/def ::a string?)  ; abbreviated agent-id
(s/def ::e string?)  ; event-type
(s/def ::m string?)  ; message
(s/def ::hivemind-message (s/keys :req-un [::a ::e ::m]))
(s/def ::messages (s/nilable (s/coll-of ::hivemind-message :kind vector?)))

;; Instruction Queue (hivemind → ling) ------------------------------------------
;;
;; The queues atom is the shared backing store for every IInstructionStore
;; implementation. Keeping it defonce'd here preserves compatibility for
;; any legacy callers that used to reach in directly; the store records
;; swap on this same atom.

(defonce ^{:doc "Map of agent-id -> [envelopes...]. Shared with instruction-store."}
  instruction-queues
  (atom {}))

(defonce ^:private default-local-store
  (istore/local-store instruction-queues))

(defn- current-store
  "Return the installed instruction store, or a default local one bound to
   `instruction-queues`. Side-effect-free."
  []
  (or (istore/get-store) default-local-store))

(defn clear-instruction-queues!
  "Clear instruction queues. Guarded — no-op if coordinator running."
  []
  (guards/when-not-coordinator
   "clear-instruction-queues! called"
   (istore/clear! (current-store))
   ;; Ensure the shared atom is empty even if a caller swapped stores
   ;; out from under us.
   (reset! instruction-queues {})))

(defn push-instruction!
  "Push an instruction to an agent's queue for piggyback delivery.
   Delegates to the active IInstructionStore — NATS-backed when running
   distributed, atom-only otherwise."
  [agent-id instruction]
  (istore/push! (current-store) agent-id instruction)
  ;; Preserve historical return shape (the atom's new value) for any
  ;; caller that relied on it.
  @instruction-queues)

(defn drain-instructions!
  "Drain all pending instructions for an agent, returning payloads in
   insertion order. Returns a vector (possibly empty)."
  [agent-id]
  (istore/drain! (current-store) agent-id))

(defn peek-instructions
  "Peek at pending instructions without draining. For debugging."
  [agent-id]
  (istore/peek* (current-store) agent-id))

;; Message Source (injectable for DIP)

(defonce ^{:doc "Injected fn returning all hivemind messages."}
  message-source-fn
  (atom nil))

(defn register-message-source!
  "Register function that provides hivemind messages."
  [source-fn]
  (reset! message-source-fn source-fn))

;; Backbone Buffer (events received via IDeliveryChannel from NATS backbone)
;; Dual-path: supplements atom-based message-source-fn with backbone-mediated events.

(def ^:private max-backbone-buffer-size
  "Maximum number of backbone-buffered messages before oldest are evicted."
  500)

(defonce ^{:doc "Vector of backbone-mediated messages for piggyback delivery.
  Dual-path: local shouts come via message-source-fn (atom), remote
  shouts arrive here via PiggybackChannel (IDeliveryChannel)."}
  backbone-buffer
  (atom []))

(defn buffer-backbone-event!
  "Buffer an event received from the NATS backbone for piggyback delivery.
   Normalizes to the same shape as message-source-fn output.
   Events with nil agent-id (e.g. coordinator tool-executed) are silently dropped.
   Preserves :shout-id for cross-path dedup when present, :parent-id /
   :broadcast? so hive-mcp.channel.audience can route the remote path exactly
   as it routes the local one, and :deliberate? so the digest spares the remote
   path exactly as it spares the local one."
  [{:keys [agent-id event-type message task timestamp project-id shout-id
           parent-id broadcast? deliberate?]}]
  (when agent-id
    (let [normalized (cond-> {:agent-id    agent-id
                              :event-type  event-type
                              :message     (or message task "")
                              :task        task
                              :timestamp   (or timestamp (System/currentTimeMillis))
                              :project-id  (or project-id "global")}
                       shout-id (assoc :shout-id shout-id)
                       parent-id (assoc :parent-id parent-id)
                       broadcast? (assoc :broadcast? true)
                       deliberate? (assoc :deliberate? true))]
      (swap! backbone-buffer
             (fn [buf]
               (let [updated (conj buf normalized)]
                 (if (> (count updated) max-backbone-buffer-size)
                   (subvec updated (- (count updated) max-backbone-buffer-size))
                   updated))))
      (log/debug "piggyback: buffered backbone event from" agent-id
                 (name (or event-type :unknown))))))

(defn clear-backbone-buffer!
  "Clear backbone buffer. For testing."
  []
  (reset! backbone-buffer []))

(defn- dedupe-messages
  "Remove duplicate messages using :shout-id (idempotency key) when present,
   falling back to [agent-id timestamp event-type-name] composite key.
   Preserves insertion order (source-fn messages take precedence).

   The shout-id approach is needed because the atom path stores event-type
   as a keyword while the NATS/backbone path deserializes it as a string —
   the old composite key missed these cross-path duplicates."
  [msgs]
  (let [seen (volatile! #{})]
    (filterv (fn [msg]
               (let [k (or (:shout-id msg)
                           [(:agent-id msg)
                            (:timestamp msg)
                            (some-> (:event-type msg) name)])]
                 (if (@seen k)
                   false
                   (do (vswap! seen conj k) true))))
             msgs)))

(defn- merged-messages
  "Merge messages from message-source-fn and backbone-buffer with dedup.
   Source-fn messages take precedence (appear first in concat)."
  []
  (let [source-msgs  (when-let [sfn @message-source-fn] (sfn))
        backbone-msgs @backbone-buffer]
    (dedupe-messages (concat source-msgs backbone-msgs))))

;; Message Cursors (per-agent-per-project read tracking)

(defonce ^{:doc "Map of [agent-id project-id] -> last-read-timestamp for cursor isolation."}
  agent-read-cursors
  (atom {}))

(defonce ^{:doc "Monotonic counter for message IDs. Incremented atomically."}
  message-id-counter
  (atom 0))

(defn next-message-id!
  "Generate next monotonic message ID. Thread-safe."
  []
  (swap! message-id-counter inc))

(s/def ::project-id (s/nilable string?))

(s/def ::additional-project-ids (s/nilable (s/coll-of string? :kind set?)))

(s/fdef get-messages
  :args (s/cat :agent-id ::agent-id
               :kwargs (s/keys* :opt-un [::project-id ::additional-project-ids]))
  :ret ::messages)

(defn- config-value
  "Read a config path, nil on any failure. Lazy requiring-resolve keeps the
   channel layer free of a load-time dep on config bootstrap."
  [path]
  (try
    (when-let [f (requiring-resolve 'hive-mcp.config.core/get-in-config)]
      (f path))
    (catch Exception _ nil)))

(defn spawner-routing?
  "Is HIVEMIND delivery addressed to the SPAWNER (default) rather than
   broadcast project-wide? Set [:hivemind :piggyback-routing] to :project —
   or the string \"project\" — to restore the legacy broadcast."
  []
  (not (contains? #{:project "project"} (config-value [:hivemind :piggyback-routing]))))

(defn progress-digest?
  "Are :progress bursts collapsed into one row per agent? Default true;
   [:hivemind :progress-digest] false renders every turn verbatim."
  []
  (not (false? (config-value [:hivemind :progress-digest]))))

(defn get-messages
  "Get new hivemind messages since last call for this agent+project.
   Dual-path: merges messages from atom-based source and backbone buffer.

   Four stages, in order: cursor -> project -> audience -> digest.

   CURSOR is per [reader project], with one exception that is the whole
   point: a \"global\" shout is read against the reader's [reader \"global\"]
   cursor WHATEVER project the read asked for. Every project-scoped read
   accepts global shouts, so with a cursor per project each first read under
   a new project-id (a git call against another repo, a kanban call for a
   sibling) replayed the entire global history from timestamp 0 — the
   repeating-shouts symptom of kanban 20260519145332-0c5878a5, measured again
   2026-09-07 when two commits in two repos each redelivered 54 wave shouts.

   AUDIENCE is what keeps one ling's turns out of every other ling's context:
   a shout reaches the agent that spawned its author and nobody else (see
   hive-mcp.channel.audience). Set config [:hivemind :piggyback-routing] to
   :project to restore the legacy project-wide broadcast.

   DIGEST collapses a burst of per-turn :progress rows from one agent into a
   single row carrying the count, so the reader learns the state without
   reading the transcript. A row the agent shouted DELIBERATELY (through the
   hivemind tool, carried as :deliberate?) is never collapsed: measured
   2026-09-07, a wave member's own `hivemind shout` was folded into the
   runtime's `bb-ling turn 2` telemetry row and the reader never saw what the
   member said. Disable digesting entirely via [:hivemind :progress-digest] false.

   Options:
     :project-id              - Primary project scope for filtering
     :additional-project-ids  - Set of extra project-ids to include (for cross-project
                                descendant shouts). Messages from these projects are
                                included alongside the primary project's messages."
  [agent-id & {:keys [project-id additional-project-ids]}]
  (when (and (nil? project-id) (not= agent-id "coordinator"))
    (log/warn "Agent" agent-id "reading hivemind without project-id - using global cursor"))
  (let [all-msgs (merged-messages)]
    (when (seq all-msgs)
      (let [effective-project (or project-id "global")
            project-key    [agent-id effective-project]
            global-key     [agent-id "global"]
            cursors        @agent-read-cursors
            project-cursor (get cursors project-key 0)
            global-cursor  (get cursors global-key 0)
            ;; Project-scoped shouts this read accepts; global ones are
            ;; accepted by every read and judged against the global cursor.
            accepted-pids (cond-> #{}
                            project-id (conj project-id)
                            (seq additional-project-ids) (into additional-project-ids))
            global?  (fn [msg] (= "global" (:project-id msg)))
            fresh?   (fn [{:keys [timestamp] :as msg}]
                       (if (global? msg)
                         (> timestamp global-cursor)
                         (and (contains? accepted-pids (:project-id msg))
                              (> timestamp project-cursor))))
            new-msgs (->> all-msgs (filter fresh?) (sort-by :timestamp) vec)
            ;; Cursors advance over everything the project filter accepted, NOT
            ;; only over what this reader is addressed by — otherwise a shout
            ;; dropped by the audience filter would be re-examined forever.
            max-ts   (fn [msgs] (when (seq msgs) (apply max (map :timestamp msgs))))
            max-global  (max-ts (filter global? new-msgs))
            max-project (max-ts (remove global? new-msgs))
            addressed (if (spawner-routing?)
                        (audience/filter-messages agent-id new-msgs)
                        new-msgs)
            ;; :deliberate? rides along only as far as the digest, which is the
            ;; one stage that reads it; the row a reader sees never carries it.
            formatted-msgs (mapv (fn [{:keys [agent-id event-type message task deliberate?]}]
                                   (cond-> {:a agent-id
                                            :e (if (keyword? event-type)
                                                 (name event-type)
                                                 event-type)
                                            :m message}
                                     task (assoc :t task)
                                     deliberate? (assoc :deliberate? true)))
                                 addressed)
            digested (mapv #(dissoc % :deliberate?)
                           (if (progress-digest?)
                             (audience/digest formatted-msgs)
                             formatted-msgs))]
        (when max-global
          (swap! agent-read-cursors assoc global-key max-global))
        (when max-project
          (swap! agent-read-cursors assoc project-key max-project))
        (when (seq digested)
          digested)))))

(defn fetch-history
  "Get hivemind messages without marking as read.
   Dual-path: merges messages from atom-based source and backbone buffer."
  [& {:keys [since limit project-id] :or {since 0 limit 100}}]
  (->> (merged-messages)
       (filter (fn [msg]
                 (and (> (:timestamp msg) since)
                      (or (nil? project-id)
                          (= (:project-id msg) project-id)
                          (= (:project-id msg) "global")))))
       ;; Sort by timestamp BEFORE map for consistent FIFO order
       (sort-by :timestamp)
       (take limit)
       (mapv (fn [{:keys [agent-id event-type message task timestamp project-id]}]
               (cond-> {:a agent-id
                        :e (if (keyword? event-type)
                             (name event-type)
                             event-type)
                        :m message
                        :ts timestamp
                        :p project-id}
                 task (assoc :t task))))))

(defn reset-cursor!
  "Reset read cursor for an agent+project. Next get-messages returns all messages.

   Arguments:
   - agent-id: Agent identifier
   - :project-id: Optional project-id. When provided, resets only that
     project's cursor. When nil, resets the 'global' cursor."
  [agent-id & {:keys [project-id]}]
  (let [effective-project (or project-id "global")
        cursor-key [agent-id effective-project]]
    (swap! agent-read-cursors dissoc cursor-key)))

(defn reset-all-cursors!
  "Reset all read cursors. For testing/debugging."
  []
  (reset! agent-read-cursors {}))

(defn evict-stale-cursors!
  "Evict cursors that haven't been read for longer than max-age-ms.
   Called during catchup to prevent unbounded cursor atom growth from
   dead coordinator instances (each bb-mcp restart created a new instance-id).

   Returns count of evicted entries."
  [max-age-ms]
  (let [now (System/currentTimeMillis)
        cutoff (- now max-age-ms)
        stale-keys (->> @agent-read-cursors
                        (filter (fn [[_k ts]] (< ts cutoff)))
                        (map first)
                        vec)]
    (when (seq stale-keys)
      (swap! agent-read-cursors #(apply dissoc % stale-keys))
      (log/info "piggyback: evicted" (count stale-keys) "stale cursors older than"
                (quot max-age-ms 60000) "min"))
    (count stale-keys)))

(defn adopt-cursor!
  "Adopt another caller's cursor position for a project.
   Used when a coordinator restarts (new instance-id) to inherit the
   cursor position from the previous instance, preventing re-delivery.

   Returns the adopted cursor timestamp, or nil if no donor found."
  [new-caller-id project-id]
  (let [effective-project (or project-id "global")
        new-key [new-caller-id effective-project]
        ;; Find the most recent cursor from any coordinator for this project
        best-donor (->> @agent-read-cursors
                        (filter (fn [[[aid proj] _ts]]
                                  (and (= proj effective-project)
                                       (not= aid new-caller-id)
                                       (clojure.string/starts-with? aid "coordinator:"))))
                        (sort-by val >)
                        first)]
    (when-let [[_donor-key donor-ts] best-donor]
      ;; Only adopt if we don't already have a cursor (or ours is older)
      (let [current-ts (get @agent-read-cursors new-key 0)]
        (when (> donor-ts current-ts)
          (swap! agent-read-cursors assoc new-key donor-ts)
          (log/info "piggyback: adopted cursor from" (first _donor-key)
                    "→" new-caller-id "at ts" donor-ts)
          donor-ts)))))
