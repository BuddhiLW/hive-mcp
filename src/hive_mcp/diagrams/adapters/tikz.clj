(ns hive-mcp.diagrams.adapters.tikz
  "TikZ adapter for LaTeX diagrams.
   
   Generates TikZ/LaTeX code for diagrams, compiles to PDF.
   Supports: flowcharts, block diagrams, state machines, architecture diagrams.
   
   Requires: pdflatex with tikz package."
  (:require [hive-mcp.diagrams.core :as diagrams]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; TikZ Templates

(def tikz-preamble
  "\\documentclass[tikz,border=10pt]{standalone}
\\usepackage{tikz}
\\usetikzlibrary{arrows.meta, positioning, shapes.geometric, fit, backgrounds, calc}

\\begin{document}
\\begin{tikzpicture}[
    node distance=1.5cm,
    >={Stealth[round]},
    box/.style={rectangle, rounded corners, draw=black, fill=blue!20, 
                minimum width=2.5cm, minimum height=0.8cm, align=center, font=\\small},
    person/.style={rectangle, rounded corners, draw=black, fill=purple!20,
                minimum width=2cm, minimum height=0.8cm, align=center, font=\\small},
    system/.style={rectangle, rounded corners, draw=black, fill=blue!30,
                minimum width=3cm, minimum height=1cm, align=center, font=\\small},
    external/.style={rectangle, rounded corners, draw=black, fill=gray!20,
                minimum width=2.5cm, minimum height=0.8cm, align=center, font=\\small},
    container/.style={rectangle, rounded corners, draw=black, fill=green!20,
                minimum width=2.5cm, minimum height=0.8cm, align=center, font=\\footnotesize},
    component/.style={rectangle, rounded corners, draw=black, fill=cyan!20,
                minimum width=2cm, minimum height=0.6cm, align=center, font=\\footnotesize},
    tool/.style={rectangle, rounded corners, draw=black, fill=green!20,
                minimum width=2cm, minimum height=0.6cm, align=center, font=\\footnotesize},
    arrow/.style={->, thick, draw=gray!70},
    label/.style={font=\\tiny, text=gray},
    decision/.style={diamond, draw=black, fill=yellow!20, 
                minimum width=1.5cm, minimum height=1.5cm, align=center, font=\\small},
    start/.style={circle, draw=black, fill=green!30, minimum size=0.8cm},
    end/.style={circle, draw=black, fill=red!30, minimum size=0.8cm}
]
")

(def tikz-postamble
  "
\\end{tikzpicture}
\\end{document}
")

;;; Element Rendering

(defn- escape-tex
  "Escape special LaTeX characters."
  [s]
  (when s
    (-> s
        (str/replace "\\" "\\textbackslash{}")
        (str/replace "&" "\\&")
        (str/replace "%" "\\%")
        (str/replace "$" "\\$")
        (str/replace "#" "\\#")
        (str/replace "_" "\\_")
        (str/replace "{" "\\{")
        (str/replace "}" "\\}")
        (str/replace "~" "\\textasciitilde{}")
        (str/replace "^" "\\textasciicircum{}"))))

(defn- node-style
  "Determine TikZ style for element."
  [{:keys [el shape external?]}]
  (cond
    external? "external"
    (= el :person) "person"
    (= el :system) "system"
    (= el :container) "container"
    (= el :component) "component"
    (= shape :diamond) "decision"
    (= shape :circle) "start"
    :else "box"))

(defn- node-id->tikz
  "Convert Clojure keyword to valid TikZ node name."
  [id]
  (-> (name id)
      (str/replace "/" "-")
      (str/replace "." "-")
      (str/replace ":" "")))

(defn- position-for-index
  "Calculate position based on index and layout."
  [idx total {:keys [direction columns] :or {direction :TB columns 3}}]
  (let [row (quot idx columns)
        col (rem idx columns)
        x-spacing 4
        y-spacing 2]
    (case direction
      :TB [(* col x-spacing) (* row (- y-spacing))]
      :LR [(* row x-spacing) (* col (- y-spacing))]
      :BT [(* col x-spacing) (* row y-spacing)]
      :RL [(* row (- x-spacing)) (* col (- y-spacing))]
      [(* col x-spacing) (* row (- y-spacing))])))

(defn- render-element
  "Render a single element as TikZ node."
  [elem idx total options]
  (let [id (node-id->tikz (:id elem))
        label (or (:label elem) (:name elem) (name (:id elem)))
        style (node-style elem)
        [x y] (or (:position elem) (position-for-index idx total options))
        tech (when (:tech elem) (format "\\\\{\\tiny %s}" (escape-tex (:tech elem))))
        full-label (if tech
                     (str (escape-tex label) tech)
                     (escape-tex label))]
    (format "\\node[%s] (%s) at (%s, %s) {%s};"
            style id x y full-label)))

(defn- render-relation
  "Render a relation as TikZ edge."
  [{:keys [from to label name style]}]
  (let [from-id (node-id->tikz from)
        to-id (node-id->tikz to)
        edge-label (or label name)
        edge-style (or style "arrow")]
    (if edge-label
      (format "\\draw[%s] (%s) -- (%s) node[midway, above, label] {%s};"
              edge-style from-id to-id (escape-tex edge-label))
      (format "\\draw[%s] (%s) -- (%s);"
              edge-style from-id to-id))))

;;; Background Groups for C4

(defn- render-background-group
  "Render a background group/boundary."
  [{:keys [id name elements]}]
  (let [node-ids (str/join ") (" (map (comp node-id->tikz :id) elements))]
    (format "\\begin{scope}[on background layer]
    \\node[fit=(%s), fill=blue!5, rounded corners, 
          label={[font=\\footnotesize]above:%s}] {};
\\end{scope}"
            node-ids (escape-tex name))))

;;; Main Rendering

(defn- spec->tikz
  "Convert diagram spec to TikZ code."
  [{:keys [type title elements relations options] :as spec}]
  (let [elem-count (count elements)
        rendered-elements (map-indexed
                           (fn [idx elem]
                             (render-element elem idx elem-count options))
                           elements)
        rendered-relations (map render-relation relations)
        title-node (when title
                     (format "\\node[font=\\large\\bfseries] at (4, 3) {%s};"
                             (escape-tex title)))]
    (str tikz-preamble
         "\n% Title\n"
         (or title-node "")
         "\n\n% Elements\n"
         (str/join "\n" rendered-elements)
         "\n\n% Relations\n"
         (str/join "\n" rendered-relations)
         tikz-postamble)))

;;; Compilation

(defn- compile-tikz!
  "Compile TikZ/LaTeX to PDF."
  [tex-path output-dir]
  (let [result (sh "pdflatex"
                   "-interaction=nonstopmode"
                   "-output-directory" output-dir
                   tex-path)]
    (if (zero? (:exit result))
      {:success? true
       :pdf-path (str/replace tex-path ".tex" ".pdf")
       :stdout (:out result)}
      {:success? false
       :errors [(str "pdflatex failed: " (:err result))]})))

;;; Adapter Implementation

(defrecord TikZAdapter [config]
  diagrams/DiagramAdapter

  (adapter-id [_] :tikz)

  (supported-types [_]
    #{:flowchart :block-diagram :state-machine
      :c4-context :c4-container :architecture
      :workflow :network})

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
                         (str "/tmp/tikz-" (System/currentTimeMillis)))
          tex-content (spec->tikz spec)
          tex-file (io/file output-dir "diagram.tex")]
      (io/make-parents tex-file)
      (spit tex-file tex-content)

      (case output-format
        :tex {:success? true
              :output tex-content
              :path (.getAbsolutePath tex-file)}

        (:pdf :png)
        (let [compile-result (compile-tikz! (.getAbsolutePath tex-file) output-dir)]
          (if (:success? compile-result)
            {:success? true
             :output (:pdf-path compile-result)
             :tex-path (.getAbsolutePath tex-file)
             :output-dir output-dir}
            compile-result))

        ;; Default: return TeX
        {:success? true
         :output tex-content
         :path (.getAbsolutePath tex-file)})))

  (preview-command [_ output-path]
    (cond
      (.endsWith output-path ".pdf")
      (format "xdg-open %s" output-path)

      (.endsWith output-path ".tex")
      (format "pdflatex -interaction=nonstopmode %s && xdg-open %s"
              output-path
              (str/replace output-path ".tex" ".pdf"))

      :else nil)))

;;; Factory

(defn create-adapter
  "Create a TikZ adapter instance."
  ([] (create-adapter {}))
  ([config]
   (->TikZAdapter config)))

;;; Registration

(defn register!
  "Register the TikZ adapter with the diagram system."
  []
  (diagrams/register-adapter! (create-adapter)))

;; Auto-register on load
(register!)
