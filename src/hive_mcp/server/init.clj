(ns hive-mcp.server.init
  "Server initialization and dependency wiring.

   This namespace handles initialization of server components
   and wires infrastructure implementations to abstract effect handlers.

   DDD: This is the composition root where infrastructure meets domain.
   SOLID: DIP - high-level modules don't depend on low-level modules;
          both depend on abstractions."
  (:require [hive-mcp.events.effects :as effects]
            [hive-mcp.tools.memory.crud :as memory-crud]
            [hive-mcp.tools.crystal :as crystal]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private *initialized (atom false))

(defn init-effects!
  "Initialize effect handlers with infrastructure implementations.

   Wires infrastructure handlers to effect abstractions:
   - memory-write -> memory-crud/handle-add
   - wrap-crystallize -> crystal/handle-wrap-crystallize

   This maintains layer separation - effects.clj doesn't import tools directly.

   Safe to call multiple times - only initializes once."
  []
  (when-not @*initialized
    (log/info "[server/init] Wiring effect handlers...")

    ;; Wire memory-write effect to memory-crud infrastructure
    (effects/set-memory-write-handler! memory-crud/handle-add)

    ;; Wire wrap-crystallize effect to crystal tools (DIP: avoids events->tools import)
    (effects/set-wrap-crystallize-handler! crystal/handle-wrap-crystallize)

    (reset! *initialized true)
    (log/info "[server/init] Effect handlers initialized")
    true))

(defn reset-initialization!
  "Reset initialization state. Primarily for testing."
  []
  (reset! *initialized false))
