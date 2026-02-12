#!/usr/bin/env bb

;; chroma-check.bb - Check and setup Chroma vector database environment
;;
;; Usage:
;;   bb scripts/chroma-check.bb          # Check environment status
;;   bb scripts/chroma-check.bb start    # Start Chroma container
;;   bb scripts/chroma-check.bb stop     # Stop Chroma container
;;   bb scripts/chroma-check.bb status   # JSON status output for Emacs integration
;;
;; Exit codes:
;;   0 - All checks passed / operation successful
;;   1 - Missing dependencies (see warnings)
;;   2 - Operation failed

(require '[babashka.process :as p]
         '[clojure.string :as str]
         '[cheshire.core :as json])

(def ^:const ANSI-RESET "\u001B[0m")
(def ^:const ANSI-GREEN "\u001B[32m")
(def ^:const ANSI-YELLOW "\u001B[33m")
(def ^:const ANSI-RED "\u001B[31m")
(def ^:const ANSI-CYAN "\u001B[36m")
(def ^:const ANSI-BOLD "\u001B[1m")

(def project-root
  (-> (System/getProperty "user.dir")
      (java.io.File.)
      (.getAbsolutePath)))

(def docker-compose-file
  (str project-root "/docker-compose.yml"))

(def readme-url
  "https://github.com/BuddhiLW/emacs-mcp#semantic-memory-search")

(defn colorize [color text]
  (str color text ANSI-RESET))

(defn bold [text]
  (str ANSI-BOLD text ANSI-RESET))

(defn check-icon [ok?]
  (if ok?
    (colorize ANSI-GREEN "✓")
    (colorize ANSI-RED "✗")))

(defn warn-icon []
  (colorize ANSI-YELLOW "⚠"))

(defn info-icon []
  (colorize ANSI-CYAN "ℹ"))

;;; Dependency Checks

(defn command-exists? [cmd]
  (try
    (-> (p/process [cmd "--version"]
                   {:out :string :err :string})
        deref
        :exit
        zero?)
    (catch Exception _ false)))

(defn docker-available? []
  (try
    (-> (p/process ["docker" "info"]
                   {:out :string :err :string})
        deref
        :exit
        zero?)
    (catch Exception _ false)))

(defn docker-compose-available? []
  (or (try
        (-> (p/process ["docker" "compose" "version"]
                       {:out :string :err :string})
            deref
            :exit
            zero?)
        (catch Exception _ false))
      (command-exists? "docker-compose")))

(defn ollama-available? []
  (command-exists? "ollama"))

(defn ollama-model-available? [model]
  (try
    (let [result (-> (p/process ["ollama" "list"]
                                {:out :string :err :string})
                     deref)]
      (and (zero? (:exit result))
           (str/includes? (:out result) model)))
    (catch Exception _ false)))

(defn chroma-healthy? []
  (try
    (let [result (-> (p/process ["curl" "-sf"
                                 "http://localhost:8000/api/v2/heartbeat"]
                                {:out :string :err :string})
                     deref)]
      (zero? (:exit result)))
    (catch Exception _ false)))

(defn docker-compose-file-exists? []
  (.exists (java.io.File. docker-compose-file)))

;;; Docker Compose Operations

(defn compose-cmd []
  (if (try
        (-> (p/process ["docker" "compose" "version"]
                       {:out :string :err :string})
            deref
            :exit
            zero?)
        (catch Exception _ false))
    ["docker" "compose"]
    ["docker-compose"]))

(defn start-chroma! []
  (println (str (info-icon) " Starting Chroma container..."))
  (let [cmd (concat (compose-cmd) ["-f" docker-compose-file "up" "-d"])
        result (-> (p/process cmd {:out :inherit :err :inherit :dir project-root})
                   deref)]
    (if (zero? (:exit result))
      (do
        (println (str (check-icon true) " Chroma container started"))
        (println (str (info-icon) " Waiting for health check..."))
        (Thread/sleep 3000)
        (loop [attempts 0]
          (cond
            (chroma-healthy?)
            (do
              (println (str (check-icon true) " Chroma is healthy and ready"))
              true)

            (>= attempts 10)
            (do
              (println (str (warn-icon) " Chroma not yet healthy, may need more time"))
              true)

            :else
            (do
              (Thread/sleep 2000)
              (recur (inc attempts))))))
      (do
        (println (str (check-icon false) " Failed to start Chroma"))
        false))))

(defn stop-chroma! []
  (println (str (info-icon) " Stopping Chroma container..."))
  (let [cmd (concat (compose-cmd) ["-f" docker-compose-file "down"])
        result (-> (p/process cmd {:out :inherit :err :inherit :dir project-root})
                   deref)]
    (if (zero? (:exit result))
      (println (str (check-icon true) " Chroma container stopped"))
      (println (str (check-icon false) " Failed to stop Chroma")))))

;;; Status Output

(defn collect-status []
  {:docker (docker-available?)
   :docker-compose (docker-compose-available?)
   :ollama (ollama-available?)
   :ollama-model (ollama-model-available? "nomic-embed-text")
   :chroma-running (chroma-healthy?)
   :compose-file (docker-compose-file-exists?)
   :project-root project-root})

(defn print-json-status []
  (println (json/generate-string (collect-status))))

;;; Main Check Output

(defn print-header []
  (println)
  (println (bold "╔══════════════════════════════════════════════════════════╗"))
  (println (bold "║     emacs-mcp Semantic Memory Environment Check          ║"))
  (println (bold "╚══════════════════════════════════════════════════════════╝"))
  (println))

(defn print-check [label ok? & [note]]
  (println (str "  " (check-icon ok?) " " label
                (when note (str " " (colorize ANSI-CYAN note))))))

(defn print-warning [msg]
  (println (str "  " (warn-icon) " " (colorize ANSI-YELLOW msg))))

(defn print-info [msg]
  (println (str "  " (info-icon) " " msg)))

(defn print-checks []
  (print-header)

  (println (bold "Dependencies:"))
  (let [docker? (docker-available?)
        compose? (docker-compose-available?)
        ollama? (ollama-available?)
        model? (ollama-model-available? "nomic-embed-text")
        file? (docker-compose-file-exists?)
        chroma? (chroma-healthy?)]

    (print-check "Docker daemon" docker?)
    (print-check "docker-compose" compose?)
    (print-check "docker-compose.yml" file?)
    (print-check "Ollama" ollama?)
    (print-check "nomic-embed-text model" model?
                 (when (and ollama? (not model?)) "(run: ollama pull nomic-embed-text)"))
    (println)

    (println (bold "Services:"))
    (print-check "Chroma vector database" chroma?
                 (when (and compose? file? (not chroma?)) "(run: bb scripts/chroma-check.bb start)"))
    (println)

    ;; Warnings and guidance
    (let [all-ok? (and docker? compose? ollama? model? chroma?)]
      (if all-ok?
        (do
          (println (str (colorize ANSI-GREEN "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")))
          (println (str "  " (check-icon true) " " (colorize ANSI-GREEN "All systems operational! Semantic search is available.")))
          (println (str (colorize ANSI-GREEN "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")))
          0)

        (do
          (println (str (colorize ANSI-YELLOW "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")))
          (println (bold "  Recommendations:"))
          (println)

          (when-not docker?
            (print-warning "Docker is not running or not installed")
            (print-info "Install: https://docs.docker.com/get-docker/")
            (println))

          (when (and docker? (not compose?))
            (print-warning "docker-compose not found")
            (print-info "Usually included with Docker Desktop")
            (println))

          (when-not ollama?
            (print-warning "Ollama not installed - embeddings will use mock provider")
            (print-info "Install: curl -fsSL https://ollama.com/install.sh | sh")
            (println))

          (when (and ollama? (not model?))
            (print-warning "nomic-embed-text model not downloaded")
            (print-info "Run: ollama pull nomic-embed-text")
            (println))

          (when (and compose? file? (not chroma?))
            (print-warning "Chroma not running - using local memory fallback")
            (print-info "Start: bb scripts/chroma-check.bb start")
            (println))

          (println)
          (println (bold "  Without semantic search:"))
          (print-info "Memory search will use simple text matching")
          (print-info "You can still use all other emacs-mcp features")
          (println)
          (println (str "  📖 Setup guide: " (colorize ANSI-CYAN readme-url)))
          (println (str (colorize ANSI-YELLOW "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")))
          1)))))

(defn print-usage []
  (println "Usage: bb scripts/chroma-check.bb [command]")
  (println)
  (println "Commands:")
  (println "  (none)   Check environment status (human-readable)")
  (println "  start    Start Chroma container")
  (println "  stop     Stop Chroma container")
  (println "  status   Output JSON status (for Emacs integration)")
  (println "  help     Show this help"))

;;; Main

(defn -main [& args]
  (let [cmd (first args)]
    (case cmd
      nil (System/exit (print-checks))
      "start" (if (start-chroma!) (System/exit 0) (System/exit 2))
      "stop" (do (stop-chroma!) (System/exit 0))
      "status" (do (print-json-status) (System/exit 0))
      "help" (do (print-usage) (System/exit 0))
      (do
        (println (str "Unknown command: " cmd))
        (print-usage)
        (System/exit 1)))))

(apply -main *command-line-args*)
