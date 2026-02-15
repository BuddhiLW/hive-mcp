(ns hive-mcp.agent.demo
  "Demo/test workflow for CiderBackend with session pool."
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.cider :as cider]
            [clojure.pprint :as pp]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce demo-state (atom {:backend nil :results []}))

(defn reset-demo! []
  (reset! demo-state {:backend nil :results []})
  (println "Demo state reset."))

(defn step-1-init-pool!
  "Initialize the CIDER session pool with 2 sessions."
  []
  (println "\nStep 1: Initialize Session Pool\n")
  (cider/init-pool! 2)
  (let [status (cider/pool-status)]
    (println "\nPool Status:")
    (pp/pprint status)
    status))

(defn step-2-create-backend
  "Create a CiderBackend from the pool."
  []
  (println "\nStep 2: Create CiderBackend\n")
  (try
    (let [backend (cider/cider-backend)]
      (swap! demo-state assoc :backend backend)
      (println "Created backend:" (proto/model-name backend))
      (pp/pprint (cider/pool-status))
      backend)
    (catch Exception e
      (println "Failed to create backend:" (ex-message e))
      nil)))

(defn step-3-test-eval
  "Test simple Clojure evaluation via CiderBackend."
  []
  (println "\nStep 3: Test Simple Evaluation\n")
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
      (println "All simple evaluations completed!"))
    (println "No backend available. Run (step-2-create-backend) first.")))

(defn step-4-test-complex
  "Test complex Clojure evaluation."
  []
  (println "\nStep 4: Test Complex Evaluation\n")
  (if-let [backend (:backend @demo-state)]
    (let [complex-code "(let [data [{:name \"Alice\" :score 85}
                                    {:name \"Bob\" :score 92}
                                    {:name \"Carol\" :score 78}]
                              avg (/ (reduce + (map :score data)) (count data))]
                          {:average avg
                           :top-scorer (apply max-key :score data)
                           :passed (filter #(>= (:score %) 80) data)})"
          messages [{:role "user" :content complex-code}]
          result (proto/chat backend messages nil)]
      (println "Result:")
      (println (:content result))
      (swap! demo-state update :results conj {:code "complex-data" :result result})
      (println "\nComplex evaluation completed!"))
    (println "No backend available. Run (step-2-create-backend) first.")))

(defn step-5-cleanup!
  "Release session and shutdown pool."
  []
  (println "\nStep 5: Cleanup\n")
  (when-let [backend (:backend @demo-state)]
    (println "Releasing session:" (:session-name backend))
    (cider/release-session! (:session-name backend)))
  (println "Shutting down pool...")
  (cider/shutdown-pool!)
  (println "\nFinal Pool Status:")
  (pp/pprint (cider/pool-status))
  (reset-demo!)
  (println "\nCleanup completed!"))

(defn run-demo!
  "Run the complete CiderBackend demo workflow."
  []
  (println "\nCiderBackend Demo Workflow\n")
  (step-1-init-pool!)
  (Thread/sleep 1000)
  (step-2-create-backend)
  (Thread/sleep 500)
  (step-3-test-eval)
  (Thread/sleep 500)
  (step-4-test-complex)
  (Thread/sleep 500)
  (step-5-cleanup!)
  (println "\nDemo Complete!"))

(defn compare-backends!
  "Compare OllamaBackend vs CiderBackend."
  []
  (println "\nBackend Comparison: Ollama vs CIDER\n")
  (let [ollama (cider/make-backend :ollama {:model "devstral-small:24b"})
        _ (println "Ollama backend:" (proto/model-name ollama))]
    {:ollama ollama}))
