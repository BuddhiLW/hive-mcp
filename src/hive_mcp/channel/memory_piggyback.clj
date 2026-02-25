(ns hive-mcp.channel.memory-piggyback
  "Memory piggyback channel for incremental delivery of axioms and conventions via cursor+budget drain."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const drain-char-budget
  "Max chars per drain batch."
  32000)

(defonce ^{:doc "Map of [agent-id project-id] -> buffer state."}
  buffers
  (atom {}))

(defn- format-entry
  "Convert a catchup entry to compact piggyback format."
  [entry]
  (cond-> {:id (:id entry)
           :T (or (:type entry) "note")
           :C (or (:content entry) (:preview entry) "")}
    (:severity entry) (assoc :S (:severity entry))
    (seq (:tags entry)) (assoc :tags (vec (:tags entry)))))

(defn enqueue!
  "Enqueue entries into the memory piggyback buffer. Idempotent."
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
  "Drain next batch of entries within char budget for an agent+project."
  [agent-id project-id]
  (let [buffer-key [(or agent-id "coordinator") (or project-id "global")]
        buf (get @buffers buffer-key)]
    (when (and buf (not (:done buf)))
      (let [{:keys [entries cursor seq-num]} buf
            total (count entries)
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
                    [batch idx]
                    (recur (conj batch entry)
                           new-chars
                           (inc idx))))))
            is-done (>= new-cursor total)
            new-seq (inc seq-num)
            delivered new-cursor
            remaining (- total new-cursor)]
        (if is-done
          (swap! buffers dissoc buffer-key)
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

(defn adopt-buffer!
  "Adopt an orphaned buffer from a previous coordinator instance.
   When a coordinator restarts (new instance-id), the old buffer is orphaned.
   This function finds a matching buffer for the same project from any
   coordinator instance and re-keys it to the new caller.

   Returns true if a buffer was adopted, false otherwise."
  [new-agent-id project-id]
  (let [new-key [(or new-agent-id "coordinator") (or project-id "global")]
        ;; Find orphaned coordinator buffer for same project
        donor (->> @buffers
                   (filter (fn [[[aid proj] buf]]
                             (and (= proj (or project-id "global"))
                                  (not= aid new-agent-id)
                                  (clojure.string/starts-with? (str aid) "coordinator:")
                                  (not (:done buf)))))
                   first)]
    (when-let [[donor-key donor-buf] donor]
      (swap! buffers (fn [bufs]
                       (-> bufs
                           (dissoc donor-key)
                           (assoc new-key donor-buf))))
      (log/info "memory-piggyback: adopted buffer from" donor-key "→" new-key
                "(" (- (count (:entries donor-buf)) (:cursor donor-buf)) "entries remaining)")
      true)))

(defn evict-orphaned-buffers!
  "Evict coordinator buffers that have no matching active caller.
   Called during catchup to clean up buffers from dead bb-mcp processes.

   Takes a set of active caller-ids (with instance suffix) and evicts
   coordinator buffers whose agent-id prefix doesn't match any active caller.

   Returns count of evicted buffers."
  [active-caller-id]
  (let [orphaned (->> @buffers
                      (filter (fn [[[aid _proj] _buf]]
                                (and (clojure.string/starts-with? (str aid) "coordinator:")
                                     (not (clojure.string/starts-with? (str aid) (str active-caller-id "-"))))))
                      (map first)
                      vec)]
    (when (seq orphaned)
      (swap! buffers #(apply dissoc % orphaned))
      (log/info "memory-piggyback: evicted" (count orphaned) "orphaned buffers:" orphaned))
    (count orphaned)))
