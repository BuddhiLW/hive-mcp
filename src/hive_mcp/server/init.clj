(ns hive-mcp.server.init
  "Server initialization and dependency wiring.

   This namespace handles initialization of server components
   and wires infrastructure implementations to abstract effect handlers.

   DDD: This is the composition root where infrastructure meets domain.
   SOLID: DIP - high-level modules don't depend on low-level modules;
          both depend on abstractions."
  (:require [hive-mcp.events.effects :as effects]
            [hive-mcp.tools.memory.crud :as memory-crud]
            [taoensso.timbre :as log]))

(defonce ^:private *initialized (atom false))

(defn init-effects!
  "Initialize effect handlers with infrastructure implementations.

   Wires the memory CRUD handler to the :memory-write effect.
   This maintains layer separation - effects.clj doesn't import memory-crud directly.

   Safe to call multiple times - only initializes once."
  []
  (when-not @*initialized
    (log/info "[server/init] Wiring effect handlers...")

    ;; Wire memory-write effect to memory-crud infrastructure
    (effects/set-memory-write-handler! memory-crud/handle-add)

    (reset! *initialized true)
    (log/info "[server/init] Effect handlers initialized")
    true))

(defn reset-initialization!
  "Reset initialization state. Primarily for testing."
  []
  (reset! *initialized false))
