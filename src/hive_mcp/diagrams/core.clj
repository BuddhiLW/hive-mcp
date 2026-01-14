(ns hive-mcp.diagrams.core
  "Core diagram system with adapter/plugin architecture.
   
   Adapters can be registered for different diagram backends:
   - :overarch  - C4/UML via soulspace-org/overarch
   - :tikz      - LaTeX/TikZ diagrams
   - :mermaid   - Mermaid.js diagrams
   - :plantuml  - Direct PlantUML
   - :graphviz  - Direct GraphViz/DOT
   
   Each adapter implements the DiagramAdapter protocol."
  (:require [clojure.string :as str]))

;;; Protocol Definition

(defprotocol DiagramAdapter
  "Protocol for diagram rendering adapters."
  (adapter-id [this]
    "Returns the adapter identifier keyword (e.g., :overarch, :tikz)")

  (supported-types [this]
    "Returns set of supported diagram types (e.g., #{:flowchart :sequence :c4-context})")

  (validate-spec [this spec]
    "Validates a diagram specification. Returns {:valid? bool :errors []}")

  (render [this spec output-format]
    "Renders the diagram spec to the specified format (:plantuml :tikz :svg :png :pdf).
     Returns {:success? bool :output string-or-path :errors []}")

  (preview-command [this output-path]
    "Returns shell command string to preview the output, or nil if N/A"))

;;; Adapter Registry

(defonce ^:private adapters (atom {}))

(defn register-adapter!
  "Register a diagram adapter."
  [adapter]
  (let [id (adapter-id adapter)]
    (swap! adapters assoc id adapter)
    (println (format "Registered diagram adapter: %s (types: %s)"
                     id (supported-types adapter)))
    id))

(defn get-adapter
  "Get adapter by id, or nil if not found."
  [adapter-id]
  (get @adapters adapter-id))

(defn list-adapters
  "List all registered adapters with their supported types."
  []
  (into {}
        (map (fn [[id adapter]]
               [id {:types (supported-types adapter)}])
             @adapters)))

(defn find-adapter-for-type
  "Find an adapter that supports the given diagram type."
  [diagram-type]
  (first
   (for [[id adapter] @adapters
         :when (contains? (supported-types adapter) diagram-type)]
     adapter)))

;;; Diagram Spec Schema

(def diagram-spec-schema
  "Schema for diagram specifications (Malli)"
  [:map
   [:type keyword?]
   [:id {:optional true} keyword?]
   [:title {:optional true} string?]
   [:elements {:optional true} [:vector :map]]
   [:relations {:optional true} [:vector :map]]
   [:options {:optional true} :map]
   [:adapter {:optional true} keyword?]])

;;; Core API

(defn create-diagram
  "Create a diagram from a specification map.
   
   Spec format:
   {:type :flowchart  ; or :sequence, :c4-context, etc.
    :title \"My Diagram\"
    :elements [{:id :a :label \"Start\"}
               {:id :b :label \"Process\"}]
    :relations [{:from :a :to :b :label \"next\"}]
    :options {:direction :TB}}
   
   Options:
   - :adapter    - Force specific adapter (default: auto-select)
   - :format     - Output format (:plantuml :tikz :svg :png :pdf)
   - :output-dir - Directory for output files"
  [spec & {:keys [adapter format output-dir]
           :or {format :plantuml
                output-dir "/tmp/diagrams"}}]
  (let [diagram-type (:type spec)
        adapter-instance (if adapter
                           (get-adapter adapter)
                           (find-adapter-for-type diagram-type))]
    (if-not adapter-instance
      {:success? false
       :errors [(format "No adapter found for diagram type: %s" diagram-type)]}
      (let [validation (validate-spec adapter-instance spec)]
        (if-not (:valid? validation)
          {:success? false :errors (:errors validation)}
          (render adapter-instance spec format))))))

(defn render-to-file
  "Render diagram spec to a file."
  [spec output-path & opts]
  (let [format (keyword (last (str/split output-path #"\.")))
        result (apply create-diagram spec :format format opts)]
    (when (:success? result)
      (spit output-path (:output result)))
    (assoc result :path output-path)))

;;; Convenience constructors

(defn flowchart
  "Create a flowchart diagram spec."
  [title & {:keys [nodes edges direction]
            :or {direction :TB}}]
  {:type :flowchart
   :title title
   :elements (vec nodes)
   :relations (vec edges)
   :options {:direction direction}})

(defn sequence-diagram
  "Create a sequence diagram spec."
  [title & {:keys [participants messages]}]
  {:type :sequence
   :title title
   :elements (vec participants)
   :relations (vec messages)})

(defn c4-context
  "Create a C4 context diagram spec."
  [title & {:keys [persons systems relations]}]
  {:type :c4-context
   :title title
   :elements (vec (concat persons systems))
   :relations (vec relations)})

;;; DSL helpers

(defn node
  "Create a node element."
  [id label & {:keys [style shape] :as opts}]
  (merge {:id id :label label} opts))

(defn edge
  "Create an edge/relation."
  [from to & {:keys [label style] :as opts}]
  (merge {:from from :to to} opts))

(defn person
  "Create a C4 person element."
  [id name & {:keys [desc] :as opts}]
  (merge {:el :person :id id :name name} opts))

(defn system
  "Create a C4 system element."
  [id name & {:keys [desc external?] :as opts}]
  (merge {:el :system :id id :name name} opts))

(defn container
  "Create a C4 container element."
  [id name tech & {:keys [desc] :as opts}]
  (merge {:el :container :id id :name name :tech tech} opts))

(defn rel
  "Create a C4 relationship."
  [from to & {:keys [name desc tech] :as opts}]
  (merge {:el :rel :from from :to to} opts))
