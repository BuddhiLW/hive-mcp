(ns hive-mcp.diagrams.tools
  "MCP tools for diagram generation.
   
   Provides tools for Claude to create and render diagrams using
   various backends (Overarch, TikZ, Mermaid)."
  (:require [hive-mcp.diagrams.core :as diagrams]
            ;; Load adapters to register them
            [hive-mcp.diagrams.adapters.overarch]
            [hive-mcp.diagrams.adapters.tikz]
            [hive-mcp.diagrams.adapters.mermaid]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;;; Tool: list-diagram-adapters

(defn list-diagram-adapters
  "List all registered diagram adapters and their capabilities."
  []
  (let [adapters (diagrams/list-adapters)]
    {:adapters (into {}
                     (map (fn [[id info]]
                            [id {:types (vec (:types info))}])
                          adapters))
     :count (count adapters)}))

;;; Tool: create-diagram

(defn create-diagram
  "Create a diagram from a specification.
   
   Arguments:
   - spec: Map with :type, :title, :elements, :relations, :options
   - adapter: Optional adapter keyword (:overarch, :tikz, :mermaid)
   - format: Output format (:plantuml, :tikz, :mermaid, :html, :pdf, :svg, :png)
   - output-dir: Optional output directory
   
   Returns:
   - {:success? true :output ... :path ...} on success
   - {:success? false :errors [...]} on failure
   
   Example spec:
   {:type :flowchart
    :title \"My Workflow\"
    :elements [{:id :start :label \"Start\" :shape :circle}
               {:id :process :label \"Process\"}
               {:id :end :label \"End\" :shape :circle}]
    :relations [{:from :start :to :process}
                {:from :process :to :end}]
    :options {:direction :TB}}"
  [{:keys [spec adapter format output-dir]
    :or {format :html}}]
  (let [spec-with-opts (if output-dir
                         (assoc-in spec [:options :output-dir] output-dir)
                         spec)]
    (if adapter
      (diagrams/create-diagram spec-with-opts :adapter adapter :format format)
      (diagrams/create-diagram spec-with-opts :format format))))

;;; Tool: create-c4-diagram

(defn create-c4-diagram
  "Create a C4 architecture diagram.
   
   Arguments:
   - type: :c4-context, :c4-container, or :c4-component
   - title: Diagram title
   - elements: Vector of C4 elements
   - relations: Vector of relationships
   - adapter: :overarch or :mermaid (default: :mermaid)
   - format: Output format (default: :html)
   
   Element types:
   - {:el :person :id :user :name \"User\" :desc \"...\" :external? false}
   - {:el :system :id :sys :name \"System\" :desc \"...\" :external? true}
   - {:el :container :id :api :name \"API\" :tech \"Clojure\" :desc \"...\"}
   - {:el :component :id :auth :name \"Auth\" :tech \"JWT\" :desc \"...\"}"
  [{:keys [type title elements relations adapter format]
    :or {type :c4-context
         adapter :mermaid
         format :html}}]
  (create-diagram
   {:spec {:type type
           :title title
           :elements elements
           :relations relations}
    :adapter adapter
    :format format}))

;;; Tool: create-flowchart

(defn create-flowchart
  "Create a flowchart diagram.
   
   Arguments:
   - title: Diagram title
   - nodes: Vector of nodes [{:id :a :label \"A\" :shape :diamond}]
   - edges: Vector of edges [{:from :a :to :b :label \"yes\"}]
   - direction: :TB, :LR, :BT, :RL (default: :TB)
   - adapter: :tikz or :mermaid (default: :mermaid)
   - format: Output format (default: :html)
   
   Node shapes: :rectangle, :diamond, :circle, :stadium, :database, :hexagon"
  [{:keys [title nodes edges direction adapter format]
    :or {direction :TB
         adapter :mermaid
         format :html}}]
  (create-diagram
   {:spec {:type :flowchart
           :title title
           :elements nodes
           :relations edges
           :options {:direction direction}}
    :adapter adapter
    :format format}))

;;; Tool: create-sequence-diagram

(defn create-sequence-diagram
  "Create a sequence diagram.
   
   Arguments:
   - title: Diagram title
   - participants: Vector of participants [{:id :user :label \"User\"}]
   - messages: Vector of messages [{:from :user :to :api :label \"request\"}]
   - format: Output format (default: :html)
   
   Message styles: :solid, :dotted, :async, :response"
  [{:keys [title participants messages format]
    :or {format :html}}]
  (create-diagram
   {:spec {:type :sequence
           :title title
           :elements participants
           :relations messages}
    :adapter :mermaid
    :format format}))

;;; Tool: render-diagram-from-edn

(defn render-diagram-from-edn
  "Render a diagram from an EDN file or string.
   
   Arguments:
   - source: EDN string or file path
   - adapter: Optional adapter to use
   - format: Output format
   - output-dir: Optional output directory"
  [{:keys [source adapter format output-dir]
    :or {format :html}}]
  (let [spec (if (.exists (io/file source))
               (edn/read-string (slurp source))
               (edn/read-string source))]
    (create-diagram {:spec spec
                     :adapter adapter
                     :format format
                     :output-dir output-dir})))

;;; Tool: preview-diagram

(defn preview-diagram
  "Generate preview command for a diagram output.
   
   Arguments:
   - path: Path to diagram output file
   - adapter: Adapter used to create the diagram
   
   Returns the shell command to preview the diagram."
  [{:keys [path adapter]}]
  (if-let [adapter-instance (diagrams/get-adapter adapter)]
    (if-let [cmd (diagrams/preview-command adapter-instance path)]
      {:command cmd}
      {:error "No preview command available for this output type"})
    {:error (str "Unknown adapter: " adapter)}))

;;; Tool: create-hive-mcp-architecture-diagram

(defn create-hive-mcp-architecture-diagram
  "Create the hive-mcp architecture diagram showing Claude-Emacs interaction.
   
   This is a predefined diagram showing the system architecture."
  [{:keys [adapter format]
    :or {adapter :mermaid
         format :html}}]
  (create-c4-diagram
   {:type :c4-container
    :title "Claude + Emacs MCP Architecture"
    :elements
    [{:el :person :id :user :name "User" :desc "Developer using Claude"}
     {:el :system :id :claude :name "Claude" :desc "AI Assistant (Opus 4.5)"}
     {:el :container :id :mcp-server :name "hive-mcp Server"
      :tech "Clojure" :desc "MCP protocol server"}
     {:el :container :id :emacs :name "Emacs"
      :tech "Emacs Lisp" :desc "Editor with hive-mcp.el"}
     {:el :container :id :memory :name "Memory Store"
      :tech "JSON" :desc "Project memory"}
     {:el :container :id :buffer :name "Buffers"
      :tech "Emacs" :desc "Editor buffers"}
     {:el :system :id :browser :name "Browser" :external? true
      :desc "For HTML preview"}]
    :relations
    [{:from :user :to :claude :name "prompts"}
     {:from :claude :to :mcp-server :name "MCP tools" :desc "JSON-RPC"}
     {:from :mcp-server :to :emacs :name "emacsclient" :desc "eval"}
     {:from :emacs :to :buffer :name "edit/read"}
     {:from :emacs :to :memory :name "store/query"}
     {:from :emacs :to :browser :name "preview" :desc "browse-url"}
     {:from :mcp-server :to :user :name "results"}]
    :adapter adapter
    :format format}))

;;; Tool definitions for MCP registration

(def diagram-tools
  "Tool definitions for MCP server registration."
  [{:name "diagram_list_adapters"
    :description "List all registered diagram adapters and their supported diagram types."
    :inputSchema {:type "object" :properties {}}
    :handler list-diagram-adapters}

   {:name "diagram_create"
    :description "Create a diagram from a specification map. Supports flowcharts, sequence diagrams, C4 architecture, state machines, etc."
    :inputSchema
    {:type "object"
     :properties
     {:spec {:type "object"
             :description "Diagram specification with :type, :title, :elements, :relations, :options"}
      :adapter {:type "string"
                :enum ["overarch" "tikz" "mermaid"]
                :description "Diagram adapter to use"}
      :format {:type "string"
               :enum ["html" "pdf" "svg" "png" "tex" "mermaid" "plantuml"]
               :description "Output format"}
      :output-dir {:type "string"
                   :description "Output directory for files"}}
     :required ["spec"]}
    :handler create-diagram}

   {:name "diagram_create_c4"
    :description "Create a C4 architecture diagram (context, container, or component level)."
    :inputSchema
    {:type "object"
     :properties
     {:type {:type "string"
             :enum ["c4-context" "c4-container" "c4-component"]
             :description "C4 diagram level"}
      :title {:type "string" :description "Diagram title"}
      :elements {:type "array" :description "C4 elements (persons, systems, containers, components)"}
      :relations {:type "array" :description "Relationships between elements"}
      :adapter {:type "string" :enum ["overarch" "mermaid"]}
      :format {:type "string" :enum ["html" "pdf" "svg" "png"]}}
     :required ["title" "elements"]}
    :handler create-c4-diagram}

   {:name "diagram_create_flowchart"
    :description "Create a flowchart or workflow diagram."
    :inputSchema
    {:type "object"
     :properties
     {:title {:type "string" :description "Diagram title"}
      :nodes {:type "array" :description "Flowchart nodes"}
      :edges {:type "array" :description "Connections between nodes"}
      :direction {:type "string" :enum ["TB" "LR" "BT" "RL"]}
      :adapter {:type "string" :enum ["tikz" "mermaid"]}
      :format {:type "string" :enum ["html" "pdf" "svg" "png" "tex"]}}
     :required ["title" "nodes"]}
    :handler create-flowchart}

   {:name "diagram_create_sequence"
    :description "Create a sequence diagram showing interactions between participants."
    :inputSchema
    {:type "object"
     :properties
     {:title {:type "string" :description "Diagram title"}
      :participants {:type "array" :description "Sequence diagram participants"}
      :messages {:type "array" :description "Messages between participants"}
      :format {:type "string" :enum ["html" "svg" "png"]}}
     :required ["title" "participants" "messages"]}
    :handler create-sequence-diagram}

   {:name "diagram_render_edn"
    :description "Render a diagram from an EDN specification file or string."
    :inputSchema
    {:type "object"
     :properties
     {:source {:type "string" :description "EDN file path or EDN string"}
      :adapter {:type "string" :enum ["overarch" "tikz" "mermaid"]}
      :format {:type "string" :enum ["html" "pdf" "svg" "png" "tex" "mermaid"]}
      :output-dir {:type "string"}}
     :required ["source"]}
    :handler render-diagram-from-edn}

   {:name "diagram_hive_mcp_architecture"
    :description "Generate the hive-mcp system architecture diagram."
    :inputSchema
    {:type "object"
     :properties
     {:adapter {:type "string" :enum ["overarch" "mermaid"]}
      :format {:type "string" :enum ["html" "pdf" "svg" "png"]}}}
    :handler create-hive-mcp-architecture-diagram}])
