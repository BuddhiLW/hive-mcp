(ns hive-mcp.diagrams.adapters.mermaid
  "Mermaid adapter for web-friendly diagrams.
   
   Generates Mermaid.js syntax for various diagram types.
   Can render via mmdc CLI (mermaid-cli) or embed in HTML.
   
   Supports: flowcharts, sequence, class, state, ER, gantt, C4.
   
   Architecture (SLAP refactored):
   - Lookup maps for shape/arrow formatting (declarative)
   - Helper functions for C4 element formatting
   - Extracted render output handlers"
  (:require [hive-mcp.diagrams.core :as diagrams]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


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

;;; Diagram Type Headers (declarative lookup)

(def ^:private diagram-headers
  {:flowchart   :direction-based
   :sequence    "sequenceDiagram"
   :class       "classDiagram"
   :state       "stateDiagram-v2"
   :er          "erDiagram"
   :gantt       "gantt"
   :pie         "pie"
   :c4-context  "C4Context"
   :c4-container "C4Container"
   :c4-component "C4Component"
   :c4-deployment "C4Deployment"})

(defn- diagram-header
  "Get Mermaid diagram type header."
  [type options]
  (let [direction (or (:direction options) :TB)
        header (get diagram-headers type :direction-based)]
    (if (= header :direction-based)
      (format "flowchart %s" (name direction))
      header)))

;;; Shape and Arrow Format Lookups (declarative)

(def ^:private flowchart-shape-formats
  {:diamond      "%s{%s}"
   :circle       "%s((%s))"
   :stadium      "%s([%s])"
   :database     "%s[(%s)]"
   :parallelogram "%s[/%s/]"
   :hexagon      "%s{{%s}}"})

(def ^:private flowchart-arrow-styles
  {:dotted       "-.->|"
   :thick        "==>|"
   :bidirectional "<-->|"})

(def ^:private sequence-arrow-styles
  {:dotted   "-->>"
   :async    "->>+"
   :response "-->>-"})

(def ^:private class-relation-arrows
  {:inheritance "<|--"
   :composition "*--"
   :aggregation "o--"
   :association "--"
   :dependency  "..>"
   :realization "..|>"})

;;; C4 Element Formatting Helpers

(defn- c4-person-or-system
  "Format C4 person or system element."
  [mid el-type n d external?]
  (let [base-fn (if external? "%s_Ext" "%s")
        el-name (case el-type
                  :person "Person"
                  :system "System"
                  "System")]
    (format (str "    " base-fn "(%s, \"%s\", \"%s\")") el-name mid n d)))

(defn- c4-container-element
  "Format C4 container element."
  [mid el-type n d t external?]
  (case el-type
    :container    (format "    Container(%s, \"%s\", \"%s\", \"%s\")" mid n t d)
    :container-db (format "    ContainerDb(%s, \"%s\", \"%s\", \"%s\")" mid n t d)
    (:person :system) (c4-person-or-system mid el-type n d external?)
    (format "    Container(%s, \"%s\", \"%s\", \"%s\")" mid n t d)))

;;; Element Rendering by Diagram Type

(defmulti render-element-mermaid
  "Render an element for a specific diagram type."
  (fn [type _elem] type))

(defmethod render-element-mermaid :flowchart
  [_ {:keys [id label name shape]}]
  (let [mid (sanitize-id id)
        text (escape-mermaid (or label name (clojure.core/name id)))
        fmt (get flowchart-shape-formats shape "%s[%s]")]
    (format (str "    " fmt) mid text)))

(defmethod render-element-mermaid :sequence
  [_ {:keys [id label name]}]
  (let [mid (sanitize-id id)
        text (escape-mermaid (or label name (clojure.core/name id)))]
    (format "    participant %s as %s" mid text)))

(defmethod render-element-mermaid :class
  [_ {:keys [id label name methods attributes]}]
  (let [mid (sanitize-id id)
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
    (c4-person-or-system mid el n d external?)))

(defmethod render-element-mermaid :c4-container
  [_ {:keys [id el name desc tech external?]}]
  (let [mid (sanitize-id id)
        n (escape-mermaid name)
        d (escape-mermaid (or desc ""))
        t (or tech "")]
    (c4-container-element mid el n d t external?)))

(defmethod render-element-mermaid :default
  [_ elem]
  (render-element-mermaid :flowchart elem))

;;; Relation Rendering

(defmulti render-relation-mermaid
  "Render a relation for a specific diagram type."
  (fn [type _rel] type))

(defmethod render-relation-mermaid :flowchart
  [_ {:keys [from to label name style]}]
  (let [from-id (sanitize-id from)
        to-id (sanitize-id to)
        text (escape-mermaid (or label name))
        arrow (get flowchart-arrow-styles style "-->|")]
    (if text
      (format "    %s %s%s| %s" from-id arrow text to-id)
      (format "    %s --> %s" from-id to-id))))

(defmethod render-relation-mermaid :sequence
  [_ {:keys [from to label name style]}]
  (let [from-id (sanitize-id from)
        to-id (sanitize-id to)
        text (escape-mermaid (or label name ""))
        arrow (get sequence-arrow-styles style "->>")]
    (format "    %s%s%s: %s" from-id arrow to-id text)))

(defmethod render-relation-mermaid :class
  [_ {:keys [from to label name relation-type]}]
  (let [from-id (sanitize-id from)
        to-id (sanitize-id to)
        text (or label name "")
        arrow (get class-relation-arrows relation-type "--")]
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
  [{:keys [type title elements relations options]}]
  (let [header (diagram-header type options)
        title-directive (when title
                          (format "---\ntitle: %s\n---\n" (escape-mermaid title)))
        rendered-elements (map #(render-element-mermaid type %) elements)
        rendered-relations (map #(render-relation-mermaid type %) relations)]
    (str (or title-directive "")
         header "\n"
         (str/join "\n" rendered-elements) "\n"
         (str/join "\n" rendered-relations))))

;;; HTML Wrapper

(def ^:private html-template
  "<!DOCTYPE html>
<html>
<head>
    <meta charset=\"UTF-8\">
    <title>%s</title>
    <script src=\"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js\"></script>
    <style>
        body { font-family: sans-serif; display: flex; justify-content: center; padding: 20px; background: #f5f5f5; }
        .mermaid { background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
    </style>
</head>
<body>
    <div class=\"mermaid\">%s</div>
    <script>mermaid.initialize({startOnLoad: true, theme: 'default'});</script>
</body>
</html>")

(defn- wrap-in-html
  "Wrap Mermaid diagram in HTML for browser viewing."
  [mermaid-code title]
  (format html-template (or title "Diagram") mermaid-code))

;;; CLI Rendering

(def ^:private output-format-flags
  {:svg "svg" :png "png" :pdf "pdf"})

(defn- render-with-mmdc!
  "Render Mermaid to image using mermaid-cli (mmdc)."
  [mmd-path output-path output-format]
  (let [format-flag (get output-format-flags output-format "svg")
        result (sh "mmdc" "-i" mmd-path "-o" output-path "-e" format-flag)]
    (if (zero? (:exit result))
      {:success? true :path output-path}
      {:success? false
       :errors [(str "mmdc failed: " (:err result)
                     "\nInstall with: npm install -g @mermaid-js/mermaid-cli")]})))

;;; Render Output Helpers

(defn- render-mermaid-output [mermaid-code mmd-file]
  {:success? true :output mermaid-code :path (.getAbsolutePath mmd-file)})

(defn- render-html-output [html-file]
  {:success? true :output (slurp html-file) :path (.getAbsolutePath html-file)})

(defn- render-image-output [output-dir mmd-file html-file mermaid-code output-format]
  (let [out-file (io/file output-dir (str "diagram." (name output-format)))
        result (render-with-mmdc! (.getAbsolutePath mmd-file)
                                  (.getAbsolutePath out-file)
                                  output-format)]
    (if (:success? result)
      (assoc result :mermaid-code mermaid-code)
      {:success? true
       :output (slurp html-file)
       :path (.getAbsolutePath html-file)
       :fallback? true
       :note "mmdc not available, using HTML output"})))

(defn- render-default-output [mermaid-code html-file]
  {:success? true
   :output mermaid-code
   :path (.getAbsolutePath html-file)
   :html-path (.getAbsolutePath html-file)})

;;; Validation Helpers

(defn- type-requires-elements? [type]
  (not (#{:gantt :pie} type)))

(defn- elements-have-ids? [elements]
  (every? :id elements))

(defn- validate-spec-errors [spec]
  (cond-> []
    (not (:type spec))
    (conj "Missing :type in diagram spec")
    
    (and (type-requires-elements? (:type spec))
         (empty? (:elements spec)))
    (conj "No elements defined")
    
    (and (seq (:elements spec))
         (not (elements-have-ids? (:elements spec))))
    (conj "All elements must have :id")))

;;; Adapter Implementation

(defrecord MermaidAdapter [config]
  diagrams/DiagramAdapter

  (adapter-id [_] :mermaid)

  (supported-types [_]
    #{:flowchart :sequence :class :state :er :gantt :pie
      :c4-context :c4-container :c4-component :c4-deployment
      :workflow :mindmap})

  (validate-spec [_ spec]
    (let [errors (validate-spec-errors spec)]
      {:valid? (empty? errors) :errors errors}))

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
        :mermaid (render-mermaid-output mermaid-code mmd-file)
        :html    (render-html-output html-file)
        (:svg :png :pdf) (render-image-output output-dir mmd-file html-file mermaid-code output-format)
        (render-default-output mermaid-code html-file))))

  (preview-command [_ output-path]
    (cond
      (some #(.endsWith output-path %) [".html" ".svg" ".png"])
      (format "xdg-open %s" output-path)

      (.endsWith output-path ".mmd")
      (format "xdg-open %s" (str/replace output-path ".mmd" ".html"))

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
