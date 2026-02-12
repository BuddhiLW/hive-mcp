(ns hive-mcp.agent.drone.ext-router
  "Dynamic backend discovery and routing for drone execution.

   Probes available backend namespaces and selects the best one.
   Priority: fsm-agentic > sdk-drone > hive-agent > legacy-loop."
  (:require [taoensso.timbre :as log]))

(def ^:private backend-nses
  "Ordered list of [backend-key namespace-sym] by preference (highest first)."
  [[:fsm-agentic 'hive-mcp.agent.drone.backend.fsm-agentic]
   [:sdk-drone   'hive-mcp.agent.drone.backend.sdk-drone]
   [:hive-agent  'hive-mcp.agent.drone.backend.hive-agent]
   [:legacy-loop 'hive-mcp.agent.drone.backend.legacy-loop]])

(defonce ^:private available-backends (atom nil))

(defn load-available-backends!
  "Attempt to require each backend namespace. Idempotent after first successful load."
  []
  (when-not @available-backends
    (let [loaded (reduce
                  (fn [acc [k ns-sym]]
                    (try
                      (require ns-sym)
                      (conj acc k)
                      (catch Exception e
                        (log/debug "ext-router: backend not available" {:backend k :error (ex-message e)})
                        acc)))
                  []
                  backend-nses)]
      (reset! available-backends loaded)
      (log/info "ext-router: loaded backends" {:available loaded}))))

(defn best-backend
  "Return the highest-priority available backend keyword, or :legacy-loop as fallback."
  []
  (or (first @available-backends) :legacy-loop))
