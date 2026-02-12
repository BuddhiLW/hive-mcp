(ns diagram-deps
  "Check and install diagram dependencies for emacs-mcp.
   
   Usage:
     bb diagram-deps        # Check status
     bb diagram-deps install # Install missing deps via brew"
  (:require [babashka.process :refer [shell sh]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;;; Dependency definitions

(def deps
  "Diagram dependencies with detection and install info."
  [{:name "plantuml"
    :description "UML/sequence diagram generator"
    :check-cmd ["plantuml" "-version"]
    :brew-pkg "plantuml"
    :config-key :plantuml-path}

   {:name "graphviz"
    :description "Graph visualization (dot)"
    :check-cmd ["dot" "-V"]
    :brew-pkg "graphviz"
    :config-key :graphviz-path}

   {:name "overarch"
    :description "C4/architecture diagrams"
    :check-cmd ["overarch" "--help"]
    :brew-pkg "overarch"
    :config-key :overarch-path}])

;;; Detection

(defn which [cmd]
  (let [result (sh "which" cmd)]
    (when (zero? (:exit result))
      (str/trim (:out result)))))

(defn check-dep [{:keys [name check-cmd]}]
  (let [cmd (first check-cmd)
        path (which cmd)]
    {:name name
     :installed? (boolean path)
     :path path}))

(defn check-all-deps []
  (mapv check-dep deps))

(defn brew-available? []
  (boolean (which "brew")))

;;; Installation

(defn install-dep! [{:keys [name brew-pkg]}]
  (println (str "Installing " name " via brew..."))
  (let [result (shell {:continue true} "brew" "install" brew-pkg)]
    (if (zero? (:exit result))
      (do (println (str "✓ " name " installed"))
          true)
      (do (println (str "✗ Failed to install " name))
          false))))

(defn install-missing! []
  (if (brew-available?)
    (let [status (check-all-deps)
          missing (filter (complement :installed?) status)]
      (if (empty? missing)
        (println "All dependencies already installed!")
        (doseq [dep deps
                :when (some #(= (:name %) (:name dep)) missing)]
          (install-dep! dep))))
    (println "Homebrew not available. Please install deps manually.")))

;;; Configuration

(defn generate-config []
  "Generate EDN config for diagram adapters."
  (let [status (check-all-deps)
        config (into {}
                     (for [{:keys [name path installed?]} status
                           :when installed?
                           :let [dep (first (filter #(= (:name %) name) deps))]]
                       [(:config-key dep) path]))]
    config))

(defn write-config! []
  (let [config (generate-config)
        config-dir (str (System/getProperty "user.home") "/.config/emacs-mcp")
        config-file (str config-dir "/diagram-deps.edn")]
    (io/make-parents config-file)
    (spit config-file (pr-str config))
    (println (str "Config written to " config-file))
    config))

;;; CLI

(defn print-status []
  (println "\n╔════════════════════════════════════════╗")
  (println "║     Diagram Dependencies Status        ║")
  (println "╠════════════════════════════════════════╣")
  (doseq [{:keys [name installed? path]} (check-all-deps)]
    (let [status (if installed? "✓" "✗")
          path-info (if path (str " → " path) "")]
      (println (format "║ %s %-12s%s" status name
                       (if (> (count path-info) 22)
                         (str (subs path-info 0 19) "...")
                         path-info)))))
  (println "╚════════════════════════════════════════╝")
  (println)
  (when (brew-available?)
    (println "Homebrew: available")
    (println "Run 'bb diagram-deps install' to install missing deps")))

(defn -main [& args]
  (case (first args)
    "install" (do (install-missing!)
                  (println)
                  (print-status)
                  (write-config!))
    "config" (do (println "Generated config:")
                 (prn (generate-config)))
    "write" (write-config!)
    ;; default: status
    (print-status)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
