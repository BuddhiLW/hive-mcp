(ns hive-mcp.nats.backbone
  "NatsBackbone — IEventBackbone implementation backed by NATS.

   Wraps hive-mcp.nats.client as a protocol-compliant backbone.
   Connection lifecycle is managed by the client module;
   this record delegates all operations to it.

   Usage:
   ```clojure
   (require '[hive-mcp.nats.backbone :as nats-bb])
   (require '[hive-mcp.protocols.event-backbone :as eb])
   (let [bb (nats-bb/create-backbone)]
     (eb/set-backbone! bb)
     ;; Start with config
     (nats-bb/start-backbone! bb {:url \"nats://localhost:4222\"}))
   ```"

  (:require [hive-mcp.nats.client :as nats]
            [hive-mcp.protocols.event-backbone :as eb]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; NatsBackbone Record
;;; ============================================================================

(defrecord NatsBackbone []
  eb/IEventBackbone
  (backbone-id [_this] :nats)

  (connected? [_this]
    (nats/connected?))

  (publish! [_this subject payload]
    (nats/publish! subject payload))

  (subscribe! [_this subject handler-fn]
    (nats/subscribe! subject handler-fn))

  (unsubscribe! [_this subject]
    (nats/unsubscribe! subject)))

;;; ============================================================================
;;; Lifecycle (outside protocol — backbone-specific)
;;; ============================================================================

(defn start-backbone!
  "Start the NATS backbone. Connects to the NATS server.
   opts are passed directly to nats/start!."
  [_backbone opts]
  (nats/start! opts)
  (log/info "[NatsBackbone] Started"))

(defn stop-backbone!
  "Stop the NATS backbone. Disconnects from the NATS server."
  [_backbone]
  (nats/stop!)
  (log/info "[NatsBackbone] Stopped"))

;;; ============================================================================
;;; Factory
;;; ============================================================================

(defn create-backbone
  "Create a NatsBackbone instance."
  []
  (->NatsBackbone))
