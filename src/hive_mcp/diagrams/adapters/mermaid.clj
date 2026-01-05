(ns hive-mcp.diagrams.adapters.mermaid
  "Mermaid adapter for web-friendly diagrams.
   
   Generates Mermaid.js syntax for various diagram types.
   Can render via mmdc CLI (mermaid-cli) or embed in HTML.
   
   Supports: flowcharts, sequence, class, state, ER, gantt, C4."
  (:require [hive-mcp.diagrams.core :as diagrams]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; ID Conversion

(defn- sanitize-id
  "Convert Clojure keyword to valid Mermaid ID."
  [id]
  (-> (name id)
      (str/replace "/" "_")
      (str/replace "." "_")
      (str/replace "-" "_")
      (str/replace ":" "")))

(defn- escape-mermaid
  "Escape special characters for Mermaid labels."
  [s]
  (when s
    (-> s
        (str/replace "\"" "'")
        (str/replace "\n" "<br/>")
        (str/replace "#" "&#35;"))))

;;; Diagram Type Headers

(defn- diagram-header
  "Get Mermaid diagram type header."
  [type options]
  (let [direction (or (:direction options) :TB)]
    (case type
      :flowchart (format "flowchart %s" (name direction))
      :sequence "sequenceDiagram"
      :class "classDiagram"
      :state "stateDiagram-v2"
      :er "erDiagram"
      :gantt "gantt"
      :pie "pie"
      :c4-context "C4Context"
      :c4-container "C4Container"
      :c4-component "C4Component"
      :c4-deployment "C4Deployment"
      (format "flowchart %s" (name direction)))))

;;; Element Rendering by Diagram Type

(defmulti render-element-mermaid
  "Render an element for a specific diagram type."
  (fn [type elem] type))

(defmethod render-element-mermaid :flowchart
  [_ {:keys [id label name shape]}]
  (let [mid (sanitize-id id)
        text (escape-mermaid (or label name (clojure.core/name id)))]
    (case shape
      :diamond (format "    %s{%s}" mid text)
      :circle (format "    %s((%s))" mid text)
      :stadium (format "    %s([%s])" mid text)
      :database (format "    %s[(%s)]" mid text)
      :parallelogram (format "    %s[/%s/]" mid text)
      :hexagon (format "    %s{{%s}}" mid text)
      ;; Default: rectangle with rounded corners
      (format "    %s[%s]" mid text))))

(defmethod render-element-mermaid :sequence
  [_ {:keys [id label name]}]
  (let [mid (sanitize-id id)
        text (escape-mermaid (or label name (clojure.core/name id)))]
    (format "    participant %s as %s" mid text)))

(defmethod render-element-mermaid :class
  [_ {:keys [id label name methods attributes]}]
  (let [mid (sanitize-id id)
        text (or label name (clojure.core/name id))
        method-lines (map #(format "        +%s()" %) (or methods []))
        attr-lines (map #(format "        +%s" %) (or attributes []))]
    (str/join "\n"
              (concat [(format "    class %s {" mid)]
                      attr-lines
                      method-lines
                      ["    }"]))))

(defmethod render-element-mermaid :state
  [_ {:keys [id label name]}]
  (let [mid (sanitize-id id)
        text (escape-mermaid (or label name (clojure.core/name id)))]
    (format "    %s : %s" mid text)))

(defmethod render-element-mermaid :c4-context
  [_ {:keys [id el name desc external?]}]
  (let [mid (sanitize-id id)
        n (escape-mermaid name)
        d (escape-mermaid (or desc ""))]
    (case el
      :person (if external?
                (format "    Person_Ext(%s, \"%s\", \"%s\")" mid n d)
                (format "    Person(%s, \"%s\", \"%s\")" mid n d))
      :system (if external?
                (format "    System_Ext(%s, \"%s\", \"%s\")" mid n d)
                (format "    System(%s, \"%s\", \"%s\")" mid n d))
      (format "    System(%s, \"%s\", \"%s\")" mid n d))))

(defmethod render-element-mermaid :c4-container
  [_ {:keys [id el name desc tech external?]}]
  (let [mid (sanitize-id id)
        n (escape-mermaid name)
        d (escape-mermaid (or desc ""))
        t (or tech "")]
    (case el
      :container (format "    Container(%s, \"%s\", \"%s\", \"%s\")" mid n t d)
      :container-db (format "    ContainerDb(%s, \"%s\", \"%s\", \"%s\")" mid n t d)
      :person (if external?
                (format "    Person_Ext(%s, \"%s\", \"%s\")" mid n d)
                (format "    Person(%s, \"%s\", \"%s\")" mid n d))
      :system (if external?
                (format "    System_Ext(%s, \"%s\", \"%s\")" mid n d)
                (format "    System(%s, \"%s\", \"%s\")" mid n d))
      (format "    Container(%s, \"%s\", \"%s\", \"%s\")" mid n t d))))

(defmethod render-element-mermaid :default
  [_ elem]
  (render-element-mermaid :flowchart elem))

;;; Relation Rendering

(defmulti render-relation-mermaid
  "Render a relation for a specific diagram type."
  (fn [type rel] type))

(defmethod render-relation-mermaid :flowchart
  [_ {:keys [from to label name style]}]
  (let [from-id (sanitize-id from)
        to-id (sanitize-id to)
        text (escape-mermaid (or label name))
        arrow (case style
                :dotted "-.->|"
                :thick "==>|"
                :bidirectional "<-->|"
                "-->|")]
    (if text
      (format "    %s %s%s| %s" from-id arrow text to-id)
      (format "    %s --> %s" from-id to-id))))

(defmethod render-relation-mermaid :sequence
  [_ {:keys [from to label name style]}]
  (let [from-id (sanitize-id from)
        to-id (sanitize-id to)
        text (escape-mermaid (or label name ""))
        arrow (case style
                :dotted "-->>"
                :async "->>+"
                :response "-->>-"
                "->>")]
    (format "    %s%s%s: %s" from-id arrow to-id text)))

(defmethod render-relation-mermaid :class
  [_ {:keys [from to label name relation-type]}]
  (let [from-id (sanitize-id from)
        to-id (sanitize-id to)
        text (or label name "")
        arrow (case relation-type
                :inheritance "<|--"
                :composition "*--"
                :aggregation "o--"
                :association "--"
                :dependency "..>"
                :realization "..|>"
                "--")]
    (if (seq text)
      (format "    %s %s %s : %s" from-id arrow to-id text)
      (format "    %s %s %s" from-id arrow to-id))))

(defmethod render-relation-mermaid :state
  [_ {:keys [from to label name]}]
  (let [from-id (if (= from :start) "[*]" (sanitize-id from))
        to-id (if (= to :end) "[*]" (sanitize-id to))
        text (or label name)]
    (if text
      (format "    %s --> %s : %s" from-id to-id (escape-mermaid text))
      (format "    %s --> %s" from-id to-id))))

(defmethod render-relation-mermaid :c4-context
  [_ {:keys [from to name desc]}]
  (let [from-id (sanitize-id from)
        to-id (sanitize-id to)
        n (escape-mermaid (or name ""))
        d (escape-mermaid (or desc ""))]
    (format "    Rel(%s, %s, \"%s\", \"%s\")" from-id to-id n d)))

(defmethod render-relation-mermaid :c4-container
  [_ rel]
  (render-relation-mermaid :c4-context rel))

(defmethod render-relation-mermaid :default
  [_ rel]
  (render-relation-mermaid :flowchart rel))

;;; Main Rendering

(defn- spec->mermaid
  "Convert diagram spec to Mermaid syntax."
  [{:keys [type title elements relations options] :as spec}]
  (let [header (diagram-header type options)
        title-directive (when title
                          (format "---\ntitle: %s\n---\n" (escape-mermaid title)))
        rendered-elements (map #(render-element-mermaid type %) elements)
        rendered-relations (map #(render-relation-mermaid type %) relations)]
    (str (or title-directive "")
         header
         "\n"
         (str/join "\n" rendered-elements)
         "\n"
         (str/join "\n" rendered-relations))))

;;; HTML Wrapper

(defn- wrap-in-html
  "Wrap Mermaid diagram in HTML for browser viewing."
  [mermaid-code title]
  (format "<!DOCTYPE html>
<html>
<head>
    <meta charset=\"UTF-8\">
    <title>%s</title>
    <script src=\"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js\"></script>
    <style>
        body { 
            font-family: sans-serif; 
            display: flex; 
            justify-content: center; 
            padding: 20px;
            background: #f5f5f5;
        }
        .mermaid { 
            background: white; 
            padding: 20px; 
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
    </style>
</head>
<body>
    <div class=\"mermaid\">
%s
    </div>
    <script>mermaid.initialize({startOnLoad: true, theme: 'default'});</script>
</body>
</html>"
          (or title "Diagram")
          mermaid-code))

;;; CLI Rendering (optional)

(defn- render-with-mmdc!
  "Render Mermaid to image using mermaid-cli (mmdc)."
  [mmd-path output-path output-format]
  (let [format-flag (case output-format
                      :svg "svg"
                      :png "png"
                      :pdf "pdf"
                      "svg")
        result (sh "mmdc" "-i" mmd-path "-o" output-path "-e" format-flag)]
    (if (zero? (:exit result))
      {:success? true :path output-path}
      {:success? false
       :errors [(str "mmdc failed: " (:err result)
                     "\nInstall with: npm install -g @mermaid-js/mermaid-cli")]})))

;;; Adapter Implementation

(defrecord MermaidAdapter [config]
  diagrams/DiagramAdapter

  (adapter-id [_] :mermaid)

  (supported-types [_]
    #{:flowchart :sequence :class :state :er :gantt :pie
      :c4-context :c4-container :c4-component :c4-deployment
      :workflow :mindmap})

  (validate-spec [_ spec]
    (let [errors (cond-> []
                   (not (:type spec))
                   (conj "Missing :type in diagram spec")

                   (and (not= (:type spec) :gantt)
                        (not= (:type spec) :pie)
                        (empty? (:elements spec)))
                   (conj "No elements defined")

                   (and (seq (:elements spec))
                        (not (every? :id (:elements spec))))
                   (conj "All elements must have :id"))]
      {:valid? (empty? errors)
       :errors errors}))

  (render [_ spec output-format]
    (let [output-dir (or (:output-dir (:options spec))
                         (str "/tmp/mermaid-" (System/currentTimeMillis)))
          mermaid-code (spec->mermaid spec)
          mmd-file (io/file output-dir "diagram.mmd")
          html-file (io/file output-dir "diagram.html")]
      (io/make-parents mmd-file)
      (spit mmd-file mermaid-code)
      (spit html-file (wrap-in-html mermaid-code (:title spec)))

      (case output-format
        :mermaid {:success? true
                  :output mermaid-code
                  :path (.getAbsolutePath mmd-file)}

        :html {:success? true
               :output (slurp html-file)
               :path (.getAbsolutePath html-file)}

        (:svg :png :pdf)
        (let [out-file (io/file output-dir (str "diagram." (name output-format)))
              result (render-with-mmdc! (.getAbsolutePath mmd-file)
                                        (.getAbsolutePath out-file)
                                        output-format)]
          (if (:success? result)
            (assoc result :mermaid-code mermaid-code)
            ;; Fallback to HTML if mmdc not available
            {:success? true
             :output (slurp html-file)
             :path (.getAbsolutePath html-file)
             :fallback? true
             :note "mmdc not available, using HTML output"}))

        ;; Default: HTML output
        {:success? true
         :output mermaid-code
         :path (.getAbsolutePath html-file)
         :html-path (.getAbsolutePath html-file)})))

  (preview-command [_ output-path]
    (cond
      (or (.endsWith output-path ".html")
          (.endsWith output-path ".svg")
          (.endsWith output-path ".png"))
      (format "xdg-open %s" output-path)

      (.endsWith output-path ".mmd")
      (format "xdg-open %s"
              (str/replace output-path ".mmd" ".html"))

      :else nil)))

;;; Factory

(defn create-adapter
  "Create a Mermaid adapter instance."
  ([] (create-adapter {}))
  ([config]
   (->MermaidAdapter config)))

;;; Registration

(defn register!
  "Register the Mermaid adapter with the diagram system."
  []
  (diagrams/register-adapter! (create-adapter)))

;; Auto-register on load
(register!)
