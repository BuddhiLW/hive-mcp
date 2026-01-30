(ns user
  "Development namespace with spec instrumentation and REPL utilities.

   Loaded automatically via :dev alias. Provides:
   - Spec instrumentation toggle
   - Namespace reloading
   - Test runners"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.repl :refer [doc source]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :as trace]
            [clj-reload.core :as reload]))

;; ============================================================
;; Spec Instrumentation
;; ============================================================

(defonce ^:private instrumented? (atom false))

(defn instrument-specs!
  "Enable spec checking on all fdef'd functions.
   Validates args on every call - useful for catching contract violations."
  []
  (require '[hive-mcp.specs.memory :as mem-spec] :reload)
  (require '[hive-mcp.specs.agent :as agent-spec] :reload)
  (let [instrumented (stest/instrument)]
    (reset! instrumented? true)
    (println "Instrumented" (count instrumented) "functions")
    instrumented))

(defn unstrument-specs!
  "Disable spec checking for performance."
  []
  (stest/unstrument)
  (reset! instrumented? false)
  (println "Specs unstrumented"))

(defn check-specs
  "Run generative tests on specified namespace or all specs.
   Uses test.check for property-based testing."
  ([]
   (check-specs 'hive-mcp.specs.memory))
  ([ns-sym]
   (require ns-sym :reload)
   (stest/check (stest/enumerate-namespace ns-sym))))

;; ============================================================
;; Namespace Reloading
;; ============================================================

(defn reload!
  "Hot-reload changed namespaces using clj-reload."
  []
  (reload/reload))

(defn reset!
  "Full reset - unload and reload all namespaces."
  []
  (reload/reload {:only :changed}))

;; ============================================================
;; Quick Test Runners
;; ============================================================

(defn run-tests
  "Run tests for specified namespace."
  [ns-sym]
  (require 'clojure.test)
  (require ns-sym :reload)
  ((resolve 'clojure.test/run-tests) ns-sym))

(defn run-all-tests
  "Run all tests in test directory."
  []
  (require 'cognitect.test-runner.api)
  ((resolve 'cognitect.test-runner.api/test) {}))

;; ============================================================
;; Debugging Helpers
;; ============================================================

(defn trace-ns
  "Trace all function calls in namespace."
  [ns-sym]
  (trace/trace-ns ns-sym))

(defn untrace-ns
  "Stop tracing namespace."
  [ns-sym]
  (trace/untrace-ns ns-sym))

;; ============================================================
;; WebSocket Channel (for bb-mcp bridge)
;; ============================================================

(defn start-websocket!
  "Start the websocket channel server for bb-mcp communication.
   Default port 9999, configurable via HIVE_WS_PORT env var."
  ([] (start-websocket! (parse-long (or (System/getenv "HIVE_WS_PORT") "9999"))))
  ([port]
   (require '[hive-mcp.channel.websocket :as ws])
   ((resolve 'hive-mcp.channel.websocket/start!) {:port port})
   (println "WebSocket channel started on port" port)))

(defn stop-websocket!
  "Stop the websocket channel server."
  []
  (require '[hive-mcp.channel.websocket :as ws])
  ((resolve 'hive-mcp.channel.websocket/stop!))
  (println "WebSocket channel stopped"))

;; ============================================================
;; Startup
;; ============================================================

(println "\n=== hive-mcp dev environment ===")
(println "Commands:")
(println "  (instrument-specs!)  - Enable spec validation")
(println "  (unstrument-specs!)  - Disable spec validation")
(println "  (check-specs)        - Run generative tests")
(println "  (reload!)            - Hot-reload changed files")
(println "  (run-tests 'ns)      - Run tests for namespace")
(println "================================\n")

;; Auto-start disabled - call (start-websocket!) manually or via systemd
;; TODO: Re-enable when nREPL detection is reliable
#_(when-not *command-line-args*
    (future
      (try
        (Thread/sleep 3000)
        (start-websocket!)
        (catch Exception e
          (println "WebSocket auto-start failed (non-fatal):" (.getMessage e))))))
