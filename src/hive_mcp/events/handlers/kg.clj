(ns hive-mcp.events.handlers.kg
  "Knowledge Graph event handlers.

   Handles events related to KG edge lifecycle:
   - :kg/edge-created   - New edge added
   - :kg/edge-updated   - Confidence changed
   - :kg/edge-removed   - Edge deleted
   - :kg/node-promoted  - Knowledge promoted to parent scope

   SOLID: SRP - KG edge lifecycle only
   CLARITY: R - Represented intent through kg domain
   CLARITY: T - Telemetry first (logging and channel publish)"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.knowledge-graph.schema :as schema]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- format-edge-summary
  "Format edge data for logging."
  [{:keys [from to relation scope confidence created-by]}]
  (str relation " (" from " -> " to ")"
       (when scope (str " [scope: " scope "]"))
       (when confidence (str " [conf: " confidence "]"))
       (when created-by (str " by " created-by))))

(defn- _valid-edge-data?
  "Validate edge data has required fields."
  [{:keys [from to relation]}]
  (and (string? from)
       (string? to)
       (keyword? relation)
       (schema/valid-relation? relation)))

;; =============================================================================
;; Handler: :kg/edge-created
;; =============================================================================

(defn handle-kg-edge-created
  "Handler for :kg/edge-created events.

   Triggered when a new edge is added to the Knowledge Graph.
   Logs the creation and publishes to channel for real-time Emacs updates.

   Expects event data:
   {:from       \"memory-entry-id-1\"      ; source node ID
    :to         \"memory-entry-id-2\"      ; target node ID
    :relation   :implements                ; relation type keyword
    :scope      \"hive-mcp\"               ; optional scope
    :confidence 0.9                        ; optional confidence (default 1.0)
    :created-by \"agent:coordinator\"      ; optional creator
    :edge-id    \"edge-20260120-abc123\"}  ; assigned edge ID

   Produces effects:
   - :log             - Log edge creation at info level
   - :channel-publish - Emit :kg-change event for Emacs"
  [_coeffects [_ {:keys [edge-id from to relation scope confidence created-by] :as data}]]
  (let [summary (format-edge-summary data)]
    {:log {:level :info
           :message (str "KG edge created: " summary
                         (when edge-id (str " [id: " edge-id "]")))}
     :channel-publish {:event-type :kg-change
                       :data {:action :created
                              :edge-id edge-id
                              :from from
                              :to to
                              :relation relation
                              :scope scope
                              :confidence (or confidence 1.0)
                              :created-by created-by}}}))

;; =============================================================================
;; Handler: :kg/edge-updated
;; =============================================================================

(defn handle-kg-edge-updated
  "Handler for :kg/edge-updated events.

   Triggered when an edge's confidence score is updated.
   Common during Socratic validation cycles.

   Expects event data:
   {:edge-id        \"edge-20260120-abc123\"  ; edge to update
    :old-confidence 0.7                       ; previous confidence
    :new-confidence 0.9                       ; new confidence
    :reason         \"Socratic validation\"   ; optional reason for update
    :updated-by     \"agent:ling-task-123\"}  ; optional updater

   Produces effects:
   - :log             - Log confidence change
   - :channel-publish - Emit :kg-change event for Emacs"
  [_coeffects [_ {:keys [edge-id old-confidence new-confidence reason updated-by]}]]
  (let [delta (when (and old-confidence new-confidence)
                (- new-confidence old-confidence))
        direction (cond
                    (nil? delta) nil
                    (pos? delta) "increased"
                    (neg? delta) "decreased"
                    :else "unchanged")]
    {:log {:level :info
           :message (str "KG edge updated: " edge-id
                         " confidence " (or direction "changed")
                         (when delta (format " by %.2f" (Math/abs (double delta))))
                         " to " new-confidence
                         (when reason (str " (" reason ")"))
                         (when updated-by (str " by " updated-by)))}
     :channel-publish {:event-type :kg-change
                       :data {:action :updated
                              :edge-id edge-id
                              :old-confidence old-confidence
                              :new-confidence new-confidence
                              :delta delta
                              :reason reason
                              :updated-by updated-by}}}))

;; =============================================================================
;; Handler: :kg/edge-removed
;; =============================================================================

(defn handle-kg-edge-removed
  "Handler for :kg/edge-removed events.

   Triggered when an edge is deleted from the Knowledge Graph.
   May occur when knowledge is superseded or found invalid.

   Expects event data:
   {:edge-id    \"edge-20260120-abc123\"  ; removed edge ID
    :from       \"memory-entry-id-1\"     ; source node (for logging)
    :to         \"memory-entry-id-2\"     ; target node (for logging)
    :relation   :supersedes               ; relation type (for logging)
    :reason     \"Superseded by newer\"   ; optional removal reason
    :removed-by \"agent:coordinator\"}    ; optional remover

   Produces effects:
   - :log             - Log edge removal at info level
   - :channel-publish - Emit :kg-change event for Emacs"
  [_coeffects [_ {:keys [edge-id from to relation reason removed-by]}]]
  {:log {:level :info
         :message (str "KG edge removed: " edge-id
                       (when relation (str " (" relation ")"))
                       (when from (str " from " from))
                       (when to (str " to " to))
                       (when reason (str " - " reason))
                       (when removed-by (str " by " removed-by)))}
   :channel-publish {:event-type :kg-change
                     :data {:action :removed
                            :edge-id edge-id
                            :from from
                            :to to
                            :relation relation
                            :reason reason
                            :removed-by removed-by}}})

;; =============================================================================
;; Handler: :kg/node-promoted
;; =============================================================================

(defn handle-kg-node-promoted
  "Handler for :kg/node-promoted events.

   Triggered when knowledge (memory entry + edges) is promoted to parent scope.
   Part of the Crystal Convergence flow - local knowledge graduates to global.

   Expects event data:
   {:node-id     \"memory-entry-id-123\"    ; promoted memory entry
    :from-scope  \"hive-mcp:agora\"         ; original scope
    :to-scope    \"hive-mcp\"               ; target (parent) scope
    :edges-promoted [\"edge-1\" \"edge-2\"] ; edges that were also promoted
    :promoted-by \"agent:coordinator\"}     ; optional promoter

   Produces effects:
   - :log             - Log promotion at info level
   - :channel-publish - Emit :kg-promotion event for Emacs
   - :shout           - Broadcast to hivemind (coordinator visibility)"
  [_coeffects [_ {:keys [node-id from-scope to-scope edges-promoted promoted-by]}]]
  (let [edge-count (count (or edges-promoted []))]
    {:log {:level :info
           :message (str "KG node promoted: " node-id
                         " from " from-scope " to " to-scope
                         " with " edge-count " edges"
                         (when promoted-by (str " by " promoted-by)))}
     :channel-publish {:event-type :kg-promotion
                       :data {:node-id node-id
                              :from-scope from-scope
                              :to-scope to-scope
                              :edges-promoted edges-promoted
                              :promoted-by promoted-by}}
     :shout {:agent-id (or promoted-by "coordinator")
             :event-type :kg-promotion
             :data {:node-id node-id
                    :from-scope from-scope
                    :to-scope to-scope
                    :edge-count edge-count}}}))

;; =============================================================================
;; Handler: :kg/edges-batch-created (Bulk operations)
;; =============================================================================

(defn handle-kg-edges-batch-created
  "Handler for :kg/edges-batch-created events.

   Triggered when multiple edges are created at once (e.g., during synthesis).
   Provides aggregated logging to avoid log spam.

   Expects event data:
   {:edges      [{:edge-id \"e1\" :from \"n1\" :to \"n2\" :relation :implements}
                 {:edge-id \"e2\" :from \"n2\" :to \"n3\" :relation :depends-on}]
    :scope      \"hive-mcp\"
    :created-by \"agent:synthesis\"}

   Produces effects:
   - :log             - Log batch summary
   - :channel-publish - Emit :kg-batch-change event"
  [_coeffects [_ {:keys [edges scope created-by]}]]
  (let [edge-count (count edges)
        relations (frequencies (map :relation edges))]
    {:log {:level :info
           :message (str "KG batch: " edge-count " edges created"
                         (when scope (str " in " scope))
                         " - " (pr-str relations)
                         (when created-by (str " by " created-by)))}
     :channel-publish {:event-type :kg-batch-change
                       :data {:action :batch-created
                              :count edge-count
                              :by-relation relations
                              :scope scope
                              :created-by created-by}}}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register KG-related event handlers.

   Events registered:
   - :kg/edge-created      - New edge added
   - :kg/edge-updated      - Confidence changed
   - :kg/edge-removed      - Edge deleted
   - :kg/node-promoted     - Knowledge promoted to parent scope
   - :kg/edges-batch-created - Multiple edges created at once"
  []
  (ev/reg-event :kg/edge-created
                [interceptors/debug]
                handle-kg-edge-created)

  (ev/reg-event :kg/edge-updated
                [interceptors/debug]
                handle-kg-edge-updated)

  (ev/reg-event :kg/edge-removed
                [interceptors/debug]
                handle-kg-edge-removed)

  (ev/reg-event :kg/node-promoted
                [interceptors/debug]
                handle-kg-node-promoted)

  (ev/reg-event :kg/edges-batch-created
                [interceptors/debug]
                handle-kg-edges-batch-created))
