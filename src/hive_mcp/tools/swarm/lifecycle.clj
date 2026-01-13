(ns hive-mcp.tools.swarm.lifecycle
  "Swarm lifecycle handlers - spawn and kill operations.

   ADR-001 Phase 2: Registration handled by event-driven sync.
   The slave-spawned event from elisp triggers register-ling! via
   channel subscription (see registry/start-registry-sync!).

   SOLID: SRP - Single responsibility for spawn/kill operations.
   CLARITY: Y - Yield safe failure with timeout handling."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [clojure.data.json :as json]
            [clojure.string :as str]))

;; ============================================================
;; Spawn Handler
;; ============================================================

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance.
   Uses timeout to prevent MCP blocking.

   ADR-001 Phase 2: Registration now handled by event-driven sync.
   The slave-spawned event from elisp triggers register-ling! via
   channel subscription (see registry/start-registry-sync!).

   Parameters:
   - name: Name for the slave (required)
   - presets: List of preset names to apply
   - cwd: Working directory for the slave
   - role: Predefined role
   - terminal: Terminal type (vterm/eat)

   CLARITY: I - Inputs validated (name required)
   SOLID: SRP - Only handles spawn, not registration"
  [{:keys [name presets cwd role terminal]}]
  (core/with-swarm
    (let [presets-str (when (seq presets)
                        (format "'(%s)" (str/join " " (map #(format "\"%s\"" %) presets))))
          elisp (format "(json-encode (hive-mcp-swarm-api-spawn \"%s\" %s %s %s))"
                        (v/escape-elisp-string (or name "slave"))
                        (or presets-str "nil")
                        (if cwd (format "\"%s\"" (v/escape-elisp-string cwd)) "nil")
                        (if terminal (format "\"%s\"" terminal) "nil"))
          ;; Use 10s timeout for spawn as it may take longer
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Spawn operation" :extra-data {:slave_name name})

        success
        ;; ADR-001 Phase 2: No manual registration here.
        ;; Registration is handled by event-driven sync when elisp emits
        ;; slave-spawned event via channel (see registry/start-registry-sync!).
        (core/mcp-success result)

        :else
        (core/mcp-error (str "Error: " error))))))

;; ============================================================
;; Kill Handler
;; ============================================================

(defn handle-swarm-kill
  "Kill a slave or all slaves.
   Uses short timeout (3s) as kill should be fast.
   Removes killed lings from registry.

   Parameters:
   - slave_id: ID of slave to kill, or \"all\" to kill all

   CLARITY: Y - Yield safe failure with timeout handling
   SOLID: SRP - Only handles kill operation"
  [{:keys [slave_id]}]
  (core/with-swarm
    (let [elisp (if (= slave_id "all")
                  "(json-encode (hive-mcp-swarm-api-kill-all))"
                  (format "(json-encode (hive-mcp-swarm-api-kill \"%s\"))"
                          (v/escape-elisp-string slave_id)))
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Kill operation" :extra-data {:slave_id slave_id})

        success
        (do
          ;; Unregister from lings registry
          (if (= slave_id "all")
            (registry/clear-registry!)
            (registry/unregister-ling! slave_id))
          (core/mcp-success result))

        :else
        (core/mcp-error (str "Error: " error))))))
