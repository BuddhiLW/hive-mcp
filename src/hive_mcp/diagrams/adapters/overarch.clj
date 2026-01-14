(ns hive-mcp.diagrams.adapters.overarch
  "Overarch adapter for C4/UML diagrams.
   
   Integrates with soulspace-org/overarch for architecture diagrams.
   Supports: C4 context/container/component, UML use-case, state machines.
   
   Requires: overarch JAR or as dependency."
  (:require [hive-mcp.diagrams.core :as diagrams]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; Configuration

;; Configurable path to overarch CLI or JAR.
;; Set via (set-overarch-path! "/path/to/overarch")
(def ^:private overarch-path-atom
  "Configurable path to overarch CLI or JAR.
   Set via (set-overarch-path! \"/path/to/overarch\")"
  (atom nil))

(defn set-overarch-path!
  "Set the overarch executable/JAR path.
   Examples:
     (set-overarch-path! \"/home/linuxbrew/.linuxbrew/bin/overarch\")
     (set-overarch-path! \"/path/to/overarch.jar\")"
  [path]
  (reset! overarch-path-atom path))

(defn get-overarch-path
  "Get the configured overarch path, or nil if not set."
  []
  @overarch-path-atom)

;;; Overarch EDN Generation

(defn- ensure-namespaced-keyword
  "Ensure a keyword has a namespace. Adds 'diagram' if missing."
  [kw]
  (if (namespace kw)
    kw
    (keyword "diagram" (name kw))))

(defn- element->overarch
  "Convert internal element to Overarch EDN format.
   Ensures :id is a namespaced keyword."
  [{:keys [el id name desc tech external? subtype] :as elem}]
  (cond-> {:el (or el :system)
           :id (ensure-namespaced-keyword id)
           :name name}
    desc (assoc :desc desc)
    tech (assoc :tech tech)
    external? (assoc :external true)
    subtype (assoc :subtype subtype)))

(defn- relation->overarch
  "Convert internal relation to Overarch EDN format.
   Generates :id from from/to if not provided. Ensures all IDs are namespaced."
  [{:keys [id from to name desc tech] :as rel}]
  (let [ns-from (ensure-namespaced-keyword from)
        ns-to (ensure-namespaced-keyword to)
        rel-id (ensure-namespaced-keyword
                (or id (keyword (str (clojure.core/name from) "-to-" (clojure.core/name to)))))]
    (cond-> {:el :rel
             :id rel-id
             :from ns-from
             :to ns-to}
      name (assoc :name name)
      desc (assoc :desc desc)
      tech (assoc :tech tech))))

(defn- spec->overarch-model
  "Convert diagram spec to Overarch model EDN.
   Returns a set of elements and relations (not a vector)."
  [{:keys [type title elements relations options]}]
  (let [model-elements (map element->overarch elements)
        model-relations (map relation->overarch relations)]
    {:title title
     :elements (set (concat model-elements model-relations))}))

(defn- spec->overarch-views
  "Generate Overarch view definition based on diagram type.
   Uses :ct with refs to all elements and relations.
   Ensures all refs use namespaced keywords."
  [{:keys [type title elements relations] :as spec}]
  (let [element-refs (mapv (fn [{:keys [id]}]
                             {:ref (ensure-namespaced-keyword id)})
                           elements)
        relation-refs (mapv (fn [{:keys [id from to]}]
                              {:ref (ensure-namespaced-keyword
                                     (or id (keyword (str (clojure.core/name from)
                                                          "-to-"
                                                          (clojure.core/name to)))))})
                            relations)
        view-type (case type
                    :c4-context :context-view
                    :c4-container :container-view
                    :c4-component :component-view
                    :use-case :use-case-view
                    :state-machine :state-machine-view
                    :deployment :deployment-view
                    :concept-map :concept-view
                    :context-view)]
    #{{:el view-type
       :id (keyword "diagram" (str (clojure.core/name type) "-view"))
       :title (or title "Diagram")
       :ct (into element-refs relation-refs)}}))

;;; File Operations

(defn- write-model-file!
  "Write Overarch model to temporary EDN file."
  [spec output-dir]
  (let [model (spec->overarch-model spec)
        views (spec->overarch-views spec)
        model-file (io/file output-dir "model.edn")
        views-file (io/file output-dir "views.edn")]
    (io/make-parents model-file)
    (spit model-file (pr-str (:elements model)))
    (spit views-file (pr-str views))
    {:model-file (.getAbsolutePath model-file)
     :views-file (.getAbsolutePath views-file)
     :output-dir output-dir}))

;;; Overarch Invocation

(defn- find-overarch-command
  "Find overarch CLI or JAR path.
   Checks in order:
   1. Configured path via set-overarch-path!
   2. 'overarch' CLI in PATH (Homebrew, etc.)
   3. OVERARCH_JAR environment variable
   4. ~/.local/lib/overarch.jar"
  []
  (letfn [(path-type [p]
            (cond
              (str/ends-with? p ".jar") :jar
              :else :cli))]
    (or
     ;; 1. Configured path
     (when-let [configured (get-overarch-path)]
       {:type (path-type configured) :path configured})
     ;; 2. CLI in PATH
     (let [cli-check (sh "which" "overarch")]
       (when (zero? (:exit cli-check))
         {:type :cli :path (str/trim (:out cli-check))}))
     ;; 3. OVERARCH_JAR env var
     (when-let [jar (System/getenv "OVERARCH_JAR")]
       {:type :jar :path jar})
     ;; 4. Default JAR location
     (let [home (System/getProperty "user.home")
           default-path (str home "/.local/lib/overarch.jar")]
       (when (.exists (io/file default-path))
         {:type :jar :path default-path})))))

(defn- run-overarch!
  "Run overarch CLI to generate diagrams.
   Supports both CLI (Homebrew) and JAR invocation."
  [{:keys [output-dir]} output-format]
  (if-let [{:keys [type path]} (find-overarch-command)]
    (let [render-format (case output-format
                          (:plantuml :puml) "plantuml"
                          :graphviz "graphviz"
                          :markdown "markdown"
                          "plantuml")
          args ["-m" output-dir "-r" render-format "-R" output-dir]
          result (case type
                   :cli (apply sh path args)
                   :jar (apply sh "java" "-jar" path args))]
      (if (zero? (:exit result))
        {:success? true
         :output-dir output-dir
         :stdout (:out result)}
        {:success? false
         :errors [(:err result)]}))
    {:success? false
     :errors ["Overarch not found. Install via 'brew install overarch' or set OVERARCH_JAR env var"]}))

;;; Adapter Implementation

(defrecord OverarchAdapter [config]
  diagrams/DiagramAdapter

  (adapter-id [_] :overarch)

  (supported-types [_]
    #{:c4-context :c4-container :c4-component
      :use-case :state-machine :deployment
      :concept-map :class-diagram})

  (validate-spec [_ spec]
    (let [errors (cond-> []
                   (not (:type spec))
                   (conj "Missing :type in diagram spec")

                   (empty? (:elements spec))
                   (conj "No elements defined")

                   (not (every? :id (:elements spec)))
                   (conj "All elements must have :id"))]
      {:valid? (empty? errors)
       :errors errors}))

  (render [_ spec output-format]
    (let [output-dir (or (:output-dir (:options spec))
                         (str "/tmp/overarch-" (System/currentTimeMillis)))
          files (write-model-file! spec output-dir)
          result (run-overarch! files output-format)]
      (if (:success? result)
        (let [output-files (->> (file-seq (io/file output-dir))
                                (filter #(.isFile %))
                                (filter #(or (.endsWith (.getName %) ".puml")
                                             (.endsWith (.getName %) ".dot")
                                             (.endsWith (.getName %) ".md")))
                                (mapv #(.getAbsolutePath %)))]
          {:success? true
           :output (first output-files)
           :all-outputs output-files
           :output-dir output-dir})
        result)))

  (preview-command [_ output-path]
    (cond
      (.endsWith output-path ".puml")
      (format "plantuml %s && xdg-open %s"
              output-path
              (str/replace output-path ".puml" ".png"))

      (.endsWith output-path ".dot")
      (format "dot -Tpng %s -o %s.png && xdg-open %s.png"
              output-path output-path output-path)

      :else nil)))

;;; Factory

(defn create-adapter
  "Create an Overarch adapter instance."
  ([] (create-adapter {}))
  ([config]
   (->OverarchAdapter config)))

;;; Registration

(defn register!
  "Register the Overarch adapter with the diagram system."
  []
  (diagrams/register-adapter! (create-adapter)))

;; Auto-register on load
(register!)
