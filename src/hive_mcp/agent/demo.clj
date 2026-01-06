(ns hive-mcp.agent.demo
  "Demo/test workflow for CiderBackend with session pool.
   
   Run from REPL:
     (require '[hive-mcp.agent.demo :as demo])
     (demo/run-demo!)
   
   Or step by step:
     (demo/step-1-init-pool!)
     (demo/step-2-create-backend)
     (demo/step-3-test-eval)
     (demo/step-4-test-complex)
     (demo/step-5-cleanup!)"
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.cider :as cider]
            [hive-mcp.agent :as agent]
            [clojure.pprint :as pp]))

;;; ============================================================
;;; Demo State
;;; ============================================================

(defonce demo-state (atom {:backend nil :results []}))

(defn reset-demo! []
  (reset! demo-state {:backend nil :results []})
  (println "Demo state reset."))

;;; ============================================================
;;; Step 1: Initialize Session Pool
;;; ============================================================

(defn step-1-init-pool!
  "Initialize the CIDER session pool with 2 sessions."
  []
  (println "\n╔══════════════════════════════════════════════════════════╗")
  (println "║  Step 1: Initialize Session Pool                         ║")
  (println "╚══════════════════════════════════════════════════════════╝\n")

  (println "Initializing pool with 2 CIDER sessions...")
  (println "(This spawns nREPL sessions via Emacs)\n")

  (cider/init-pool! 2)

  (let [status (cider/pool-status)]
    (println "\nPool Status:")
    (pp/pprint status)
    status))

;;; ============================================================
;;; Step 2: Create CiderBackend
;;; ============================================================

(defn step-2-create-backend
  "Create a CiderBackend from the pool."
  []
  (println "\n╔══════════════════════════════════════════════════════════╗")
  (println "║  Step 2: Create CiderBackend                             ║")
  (println "╚══════════════════════════════════════════════════════════╝\n")

  (println "Acquiring session from pool...")

  (try
    (let [backend (cider/cider-backend)]
      (swap! demo-state assoc :backend backend)
      (println "✓ Created backend:" (proto/model-name backend))
      (println "\nPool Status after acquire:")
      (pp/pprint (cider/pool-status))
      backend)
    (catch Exception e
      (println "✗ Failed to create backend:" (ex-message e))
      (println "\nNote: Pool must be initialized first with (step-1-init-pool!)")
      nil)))

;;; ============================================================
;;; Step 3: Test Simple Eval
;;; ============================================================

(defn step-3-test-eval
  "Test simple Clojure evaluation via CiderBackend."
  []
  (println "\n╔══════════════════════════════════════════════════════════╗")
  (println "║  Step 3: Test Simple Evaluation                          ║")
  (println "╚══════════════════════════════════════════════════════════╝\n")

  (if-let [backend (:backend @demo-state)]
    (let [test-cases [["(+ 1 2 3)" "Simple arithmetic"]
                      ["(map inc [1 2 3])" "Higher-order function"]
                      ["(str \"Hello, \" \"CiderBackend!\")" "String concatenation"]
                      ["(reduce + (range 10))" "Reduce over range"]]]
      (doseq [[code description] test-cases]
        (println (str "Testing: " description))
        (println (str "  Code: " code))
        (let [messages [{:role "user" :content code}]
              result (proto/chat backend messages nil)]
          (println (str "  Result: " (:content result)))
          (swap! demo-state update :results conj {:code code :result result})
          (println)))

      (println "✓ All simple evaluations completed!"))
    (println "✗ No backend available. Run (step-2-create-backend) first.")))

;;; ============================================================
;;; Step 4: Test Complex Evaluation
;;; ============================================================

(defn step-4-test-complex
  "Test more complex Clojure evaluation."
  []
  (println "\n╔══════════════════════════════════════════════════════════╗")
  (println "║  Step 4: Test Complex Evaluation                         ║")
  (println "╚══════════════════════════════════════════════════════════╝\n")

  (if-let [backend (:backend @demo-state)]
    (let [complex-code "(let [data [{:name \"Alice\" :score 85}
                                    {:name \"Bob\" :score 92}
                                    {:name \"Carol\" :score 78}]
                              avg (/ (reduce + (map :score data)) (count data))]
                          {:average avg
                           :top-scorer (apply max-key :score data)
                           :passed (filter #(>= (:score %) 80) data)})"]

      (println "Testing: Complex data transformation")
      (println "Code:")
      (println "  (let [data [{:name \"Alice\" :score 85}")
      (println "              {:name \"Bob\" :score 92}")
      (println "              {:name \"Carol\" :score 78}]")
      (println "        avg (/ (reduce + (map :score data)) (count data))]")
      (println "    {:average avg")
      (println "     :top-scorer (apply max-key :score data)")
      (println "     :passed (filter #(>= (:score %) 80) data)})")
      (println)

      (let [messages [{:role "user" :content complex-code}]
            result (proto/chat backend messages nil)]
        (println "Result:")
        (println (:content result))
        (swap! demo-state update :results conj {:code "complex-data" :result result})
        (println "\n✓ Complex evaluation completed!")))
    (println "✗ No backend available. Run (step-2-create-backend) first.")))

;;; ============================================================
;;; Step 5: Cleanup
;;; ============================================================

(defn step-5-cleanup!
  "Release session and shutdown pool."
  []
  (println "\n╔══════════════════════════════════════════════════════════╗")
  (println "║  Step 5: Cleanup                                         ║")
  (println "╚══════════════════════════════════════════════════════════╝\n")

  (when-let [backend (:backend @demo-state)]
    (println "Releasing session:" (:session-name backend))
    (cider/release-session! (:session-name backend)))

  (println "Shutting down pool...")
  (cider/shutdown-pool!)

  (println "\nFinal Pool Status:")
  (pp/pprint (cider/pool-status))

  (reset-demo!)
  (println "\n✓ Cleanup completed!"))

;;; ============================================================
;;; Full Demo
;;; ============================================================

(defn run-demo!
  "Run the complete CiderBackend demo workflow."
  []
  (println "\n")
  (println "╔══════════════════════════════════════════════════════════╗")
  (println "║                                                          ║")
  (println "║            CiderBackend Demo Workflow                    ║")
  (println "║                                                          ║")
  (println "║  This demo shows the CiderBackend with session pool:     ║")
  (println "║  1. Initialize session pool (spawns CIDER sessions)      ║")
  (println "║  2. Create CiderBackend from pool                        ║")
  (println "║  3. Test simple Clojure evaluation                       ║")
  (println "║  4. Test complex data transformation                     ║")
  (println "║  5. Cleanup and shutdown                                 ║")
  (println "║                                                          ║")
  (println "╚══════════════════════════════════════════════════════════╝")

  (step-1-init-pool!)
  (Thread/sleep 1000)

  (step-2-create-backend)
  (Thread/sleep 500)

  (step-3-test-eval)
  (Thread/sleep 500)

  (step-4-test-complex)
  (Thread/sleep 500)

  (step-5-cleanup!)

  (println "\n╔══════════════════════════════════════════════════════════╗")
  (println "║  Demo Complete!                                          ║")
  (println "╚══════════════════════════════════════════════════════════╝\n"))

;;; ============================================================
;;; Backend Comparison Demo
;;; ============================================================

(defn compare-backends!
  "Compare OllamaBackend vs CiderBackend for the same task."
  []
  (println "\n╔══════════════════════════════════════════════════════════╗")
  (println "║  Backend Comparison: Ollama vs CIDER                     ║")
  (println "╚══════════════════════════════════════════════════════════╝\n")

  (println "Creating backends via factory...")
  (let [ollama (cider/make-backend :ollama {:model "devstral-small:24b"})
        _ (println "✓ Ollama backend:" (proto/model-name ollama))]

    (println "\nNote: CiderBackend requires initialized pool.")
    (println "      Ollama executes LLM inference.")
    (println "      Cider executes Clojure code directly.")

    (println "\n--- Ollama Backend ---")
    (println "Purpose: LLM reasoning with tool-use loop")
    (println "Use for: Complex tasks requiring AI planning")

    (println "\n--- Cider Backend ---")
    (println "Purpose: Direct Clojure code execution")
    (println "Use for: REPL-driven development, code testing, runtime introspection")

    (println "\n--- Hybrid Backend ---")
    (println "Purpose: Ollama plans, Cider executes")
    (println "Use for: AI-guided Clojure development")

    {:ollama ollama}))
