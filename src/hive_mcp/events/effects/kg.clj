(ns hive-mcp.events.effects.kg
  "Knowledge Graph effect handlers for the hive-mcp event system.

   Effects implemented:
   - :kg-add-edge               - Create edge in Knowledge Graph
   - :kg-update-confidence      - Update edge confidence score
   - :kg-increment-confidence   - Adjust confidence by delta (Socratic)
   - :kg-remove-edge            - Delete edge from Knowledge Graph
   - :kg-remove-edges-for-node  - Clean up edges for deleted node

   Usage:
   ```clojure
   (require '[hive-mcp.events.effects.kg :as kg-effects])
   (kg-effects/register-kg-effects!)
   ```

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Effect: :kg-add-edge
;; =============================================================================

(defn- handle-kg-add-edge
  "Execute a :kg-add-edge effect - create new edge in Knowledge Graph.

   Wraps kg-edges/add-edge! and dispatches :kg/edge-created event on success.

   Expected data shape:
   {:from       \"memory-entry-id-1\"     ; source node ID (required)
    :to         \"memory-entry-id-2\"     ; target node ID (required)
    :relation   :implements               ; relation type (required)
    :scope      \"hive-mcp\"              ; optional scope
    :confidence 0.9                       ; optional (default: 1.0)
    :created-by \"agent:coordinator\"}    ; optional creator

   Example:
   {:kg-add-edge {:from \"mem-123\"
                  :to \"mem-456\"
                  :relation :implements
                  :scope \"hive-mcp\"}}"
  [{:keys [from to relation] :as data}]
  (when (and from to relation)
    (try
      (let [edge-id (kg-edges/add-edge! data)]
        (log/debug "[EVENT] KG edge added:" edge-id)
        ;; Dispatch edge-created event for logging/channel publish
        (future
          (try
            (ev/dispatch [:kg/edge-created (assoc data :edge-id edge-id)])
            (catch Exception e
              (log/warn "[EVENT] Failed to dispatch kg/edge-created:" (.getMessage e)))))
        edge-id)
      (catch AssertionError e
        (log/error "[EVENT] KG add-edge validation failed:" (.getMessage e)))
      (catch Exception e
        (log/error "[EVENT] KG add-edge failed:" (.getMessage e))))))

;; =============================================================================
;; Effect: :kg-update-confidence
;; =============================================================================

(defn- handle-kg-update-confidence
  "Execute a :kg-update-confidence effect - update edge confidence score.

   Wraps kg-edges/update-edge-confidence! and dispatches :kg/edge-updated event.

   Expected data shape:
   {:edge-id    \"edge-20260120-abc123\"  ; edge to update (required)
    :confidence 0.9                       ; new confidence value (required)
    :reason     \"Socratic validation\"   ; optional reason
    :updated-by \"agent:ling-123\"}       ; optional updater

   Example:
   {:kg-update-confidence {:edge-id \"edge-123\"
                           :confidence 0.85
                           :reason \"Validated through usage\"}}"
  [{:keys [edge-id confidence reason updated-by]}]
  (when (and edge-id confidence)
    (try
      ;; Get old confidence for event
      (let [old-edge (kg-edges/get-edge edge-id)
            old-confidence (:kg-edge/confidence old-edge)]
        (kg-edges/update-edge-confidence! edge-id confidence)
        (log/debug "[EVENT] KG edge confidence updated:" edge-id "to" confidence)
        ;; Dispatch edge-updated event
        (future
          (try
            (ev/dispatch [:kg/edge-updated {:edge-id edge-id
                                            :old-confidence old-confidence
                                            :new-confidence confidence
                                            :reason reason
                                            :updated-by updated-by}])
            (catch Exception e
              (log/warn "[EVENT] Failed to dispatch kg/edge-updated:" (.getMessage e))))))
      (catch AssertionError e
        (log/error "[EVENT] KG update-confidence validation failed:" (.getMessage e)))
      (catch Exception e
        (log/error "[EVENT] KG update-confidence failed:" (.getMessage e))))))

;; =============================================================================
;; Effect: :kg-increment-confidence
;; =============================================================================

(defn- handle-kg-increment-confidence
  "Execute a :kg-increment-confidence effect - adjust confidence by delta.

   Wraps kg-edges/increment-confidence! for Socratic validation.
   Positive delta = assertion supported, negative = contradicted.

   Expected data shape:
   {:edge-id    \"edge-20260120-abc123\"  ; edge to update (required)
    :delta      0.1                       ; adjustment amount (required)
    :reason     \"Assertion supported\"}  ; optional reason

   Example:
   {:kg-increment-confidence {:edge-id \"edge-123\"
                              :delta 0.1
                              :reason \"Referenced in new decision\"}}"
  [{:keys [edge-id delta reason]}]
  (when (and edge-id delta)
    (try
      (let [old-edge (kg-edges/get-edge edge-id)
            old-confidence (or (:kg-edge/confidence old-edge) 1.0)
            new-confidence (kg-edges/increment-confidence! edge-id delta)]
        (log/debug "[EVENT] KG edge confidence incremented:" edge-id "by" delta "to" new-confidence)
        ;; Dispatch edge-updated event
        (when new-confidence
          (future
            (try
              (ev/dispatch [:kg/edge-updated {:edge-id edge-id
                                              :old-confidence old-confidence
                                              :new-confidence new-confidence
                                              :reason (or reason (str "Incremented by " delta))}])
              (catch Exception e
                (log/warn "[EVENT] Failed to dispatch kg/edge-updated:" (.getMessage e)))))))
      (catch Exception e
        (log/error "[EVENT] KG increment-confidence failed:" (.getMessage e))))))

;; =============================================================================
;; Effect: :kg-remove-edge
;; =============================================================================

(defn- handle-kg-remove-edge
  "Execute a :kg-remove-edge effect - delete edge from Knowledge Graph.

   Wraps kg-edges/remove-edge! and dispatches :kg/edge-removed event.

   Expected data shape:
   {:edge-id    \"edge-20260120-abc123\"  ; edge to remove (required)
    :reason     \"Superseded\"            ; optional reason
    :removed-by \"agent:coordinator\"}    ; optional remover

   Example:
   {:kg-remove-edge {:edge-id \"edge-123\"
                     :reason \"Knowledge outdated\"}}"
  [{:keys [edge-id reason removed-by]}]
  (when edge-id
    (try
      ;; Get edge data for event before removal
      (let [edge (kg-edges/get-edge edge-id)
            from (:kg-edge/from edge)
            to (:kg-edge/to edge)
            relation (:kg-edge/relation edge)]
        (kg-edges/remove-edge! edge-id)
        (log/debug "[EVENT] KG edge removed:" edge-id)
        ;; Dispatch edge-removed event
        (future
          (try
            (ev/dispatch [:kg/edge-removed {:edge-id edge-id
                                            :from from
                                            :to to
                                            :relation relation
                                            :reason reason
                                            :removed-by removed-by}])
            (catch Exception e
              (log/warn "[EVENT] Failed to dispatch kg/edge-removed:" (.getMessage e))))))
      (catch Exception e
        (log/error "[EVENT] KG remove-edge failed:" (.getMessage e))))))

;; =============================================================================
;; Effect: :kg-remove-edges-for-node
;; =============================================================================

(defn- handle-kg-remove-edges-for-node
  "Execute a :kg-remove-edges-for-node effect - clean up edges for deleted node.

   Wraps kg-edges/remove-edges-for-node! for memory entry cleanup.
   Call this when a memory entry is deleted from Chroma.

   Expected data shape:
   {:node-id    \"memory-entry-id-123\"   ; node being deleted (required)
    :reason     \"Memory entry deleted\"  ; optional reason
    :removed-by \"agent:coordinator\"}    ; optional remover

   Example:
   {:kg-remove-edges-for-node {:node-id \"mem-123\"
                               :reason \"Memory expired\"}}"
  [{:keys [node-id]}]
  (when node-id
    (try
      (let [removed-count (kg-edges/remove-edges-for-node! node-id)]
        (log/info "[EVENT] KG edges removed for node:" node-id "count:" removed-count)
        removed-count)
      (catch Exception e
        (log/error "[EVENT] KG remove-edges-for-node failed:" (.getMessage e))))))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-kg-effects!
  "Register all Knowledge Graph effect handlers.

   Effects registered:
   - :kg-add-edge               - Create edge in Knowledge Graph
   - :kg-update-confidence      - Update edge confidence score
   - :kg-increment-confidence   - Adjust confidence by delta (Socratic)
   - :kg-remove-edge            - Delete edge from Knowledge Graph
   - :kg-remove-edges-for-node  - Clean up edges for deleted node

   Called from hive-mcp.events.effects/register-effects!"
  []
  (ev/reg-fx :kg-add-edge handle-kg-add-edge)
  (ev/reg-fx :kg-update-confidence handle-kg-update-confidence)
  (ev/reg-fx :kg-increment-confidence handle-kg-increment-confidence)
  (ev/reg-fx :kg-remove-edge handle-kg-remove-edge)
  (ev/reg-fx :kg-remove-edges-for-node handle-kg-remove-edges-for-node)
  (log/info "[hive-events.kg] KG effects registered: :kg-add-edge :kg-update-confidence :kg-increment-confidence :kg-remove-edge :kg-remove-edges-for-node"))
