(ns hive-mcp.org-clj.render
  "Kanban board rendering with pluggable adapters (OCP).
   
   Architecture:
   - `KanbanRenderer` protocol defines the port
   - Each adapter (terminal, emacs, html, web) implements the protocol
   - `render-board` is the main entry point
   
   To add a new renderer:
   1. Create a new record implementing KanbanRenderer
   2. Implement render-board, render-column, render-card methods
   3. No changes needed to existing code (OCP)"
  (:require [clojure.string :as str]
            [hive-mcp.org-clj.parser :as parser]
            [hive-mcp.org-clj.query :as query]))

;; =============================================================================
;; Protocol (Port)
;; =============================================================================

(defprotocol KanbanRenderer
  "Protocol for rendering kanban boards. Implement this to add new output formats."
  (render-board [this board-data]
    "Render the complete kanban board. Returns renderer-specific output.")
  (render-column [this column-name tasks]
    "Render a single column with its tasks.")
  (render-card [this task]
    "Render a single task card."))

;; =============================================================================
;; Board Data Extraction
;; =============================================================================

(defn extract-board-data
  "Extract kanban board data from a parsed org document.
   Returns {:columns [...] :stats {...} :title ...}
   
   Uses level-2 tasks consistently for both stats and column counts
   to avoid mismatch between stats line and column headers."
  [doc]
  (let [;; Use find-tasks (level-2 only) for consistent counting
        tasks (query/find-tasks doc)
        ;; Group by status keyword
        by-status (group-by :keyword tasks)
        todos (get by-status "TODO" [])
        in-progress (get by-status "IN-PROGRESS" [])
        in-review (get by-status "IN-REVIEW" [])
        done (get by-status "DONE" [])
        ;; Stats derived from the same task set
        stats {:total (count tasks)
               :todo (count todos)
               :in-progress (count in-progress)
               :in-review (count in-review)
               :done (count done)}]
    {:title (get-in doc [:properties :TITLE] "Kanban Board")
     :stats stats
     :columns [{:name "TODO" :tasks todos :emoji "📋"}
               {:name "IN-PROGRESS" :tasks in-progress :emoji "🔄"}
               {:name "IN-REVIEW" :tasks in-review :emoji "👀"}
               {:name "DONE" :tasks done :emoji "✅"}]}))

(defn load-board
  "Load kanban board data from an org file path."
  [file-path]
  (let [content (slurp file-path)
        doc (parser/parse-document content)]
    (extract-board-data doc)))

;; =============================================================================
;; Terminal ASCII Adapter
;; =============================================================================

(defrecord TerminalRenderer [width column-width max-cards]
  KanbanRenderer

  (render-card [_ task]
    (let [title (or (:title task) "Untitled")
          id (get-in task [:properties :ID] "")
          short-id (if (> (count id) 8) (subs id 0 8) id)
          truncated (if (> (count title) (- column-width 4))
                      (str (subs title 0 (- column-width 7)) "...")
                      title)]
      (str "│ " truncated (apply str (repeat (- column-width (count truncated) 3) " ")) "│")))

  (render-column [this column-name tasks]
    (let [col-w column-width
          header (str "┌" (apply str (repeat (- col-w 2) "─")) "┐")
          title-line (let [padded (str " " column-name " (" (count tasks) ") ")]
                       (str "│" padded (apply str (repeat (- col-w (count padded) 2) " ")) "│"))
          separator (str "├" (apply str (repeat (- col-w 2) "─")) "┤")
          footer (str "└" (apply str (repeat (- col-w 2) "─")) "┘")
          task-cards (take max-cards (map #(render-card this %) tasks))
          more-indicator (when (> (count tasks) max-cards)
                           (str "│ ..." (- (count tasks) max-cards) " more"
                                (apply str (repeat (- col-w 12) " ")) "│"))]
      (str/join "\n" (concat [header title-line separator]
                             task-cards
                             (when more-indicator [more-indicator])
                             [footer]))))

  (render-board [this {:keys [title stats columns]}]
    (let [col-outputs (map #(render-column this (:name %) (:tasks %)) columns)
          col-lines (map str/split-lines col-outputs)
          max-height (apply max (map count col-lines))
          padded-cols (map (fn [lines]
                             (concat lines (repeat (- max-height (count lines))
                                                   (str "│" (apply str (repeat (- column-width 2) " ")) "│"))))
                           col-lines)
          merged-lines (apply map (fn [& rows] (str/join "  " rows)) padded-cols)
          header (str "\n  " title "\n"
                      "  Stats: " (:total stats) " total | "
                      (:todo stats) " todo | "
                      (:in-progress stats) " in-progress | "
                      (:done stats) " done\n")]
      (str header "\n" (str/join "\n" merged-lines) "\n"))))

(defn terminal-renderer
  "Create a terminal ASCII renderer.
   Options:
   - :width - total width (default 120)
   - :column-width - width per column (default 28)
   - :max-cards - max cards per column (default 10)"
  ([] (terminal-renderer {}))
  ([{:keys [width column-width max-cards]
     :or {width 120 column-width 28 max-cards 10}}]
   (->TerminalRenderer width column-width max-cards)))

;; =============================================================================
;; Emacs Buffer Adapter
;; =============================================================================

(defrecord EmacsRenderer [buffer-name use-colors]
  KanbanRenderer

  (render-card [_ task]
    (let [title (or (:title task) "Untitled")
          id (get-in task [:properties :ID] "")
          priority (get-in task [:properties :PRIORITY])]
      (str "  • " title
           (when priority (str " [" priority "]"))
           "\n")))

  (render-column [this column-name tasks]
    (let [header (str "** " column-name " (" (count tasks) ")\n")
          cards (str/join "" (map #(render-card this %) tasks))]
      (str header cards)))

  (render-board [this {:keys [title stats columns]}]
    (let [org-header (str "#+TITLE: " title " - Kanban View\n"
                          "#+STARTUP: showall\n\n"
                          "* Overview\n"
                          "  - Total: " (:total stats) "\n"
                          "  - TODO: " (:todo stats) "\n"
                          "  - In Progress: " (:in-progress stats) "\n"
                          "  - Done: " (:done stats) "\n\n"
                          "* Board\n")
          col-outputs (map #(render-column this (:name %) (:tasks %)) columns)]
      (str org-header (str/join "\n" col-outputs)))))

(defn emacs-renderer
  "Create an Emacs buffer renderer.
   Options:
   - :buffer-name - name of the buffer (default *kanban*)
   - :use-colors - use org-mode faces (default true)"
  ([] (emacs-renderer {}))
  ([{:keys [buffer-name use-colors]
     :or {buffer-name "*kanban*" use-colors true}}]
   (->EmacsRenderer buffer-name use-colors)))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn render-file
  "Render a kanban board from an org file.
   renderer - a KanbanRenderer implementation
   file-path - path to the org file"
  [renderer file-path]
  (let [board-data (load-board file-path)]
    (render-board renderer board-data)))

(defn render-to-terminal
  "Quick function to render org file to terminal."
  [file-path]
  (render-file (terminal-renderer) file-path))

(defn render-to-emacs
  "Quick function to render org file for Emacs buffer."
  [file-path]
  (render-file (emacs-renderer) file-path))

;; =============================================================================
;; REPL Testing
;; =============================================================================

(comment
  ;; Terminal rendering
  (println (render-to-terminal "/home/lages/dotfiles/gitthings/hive-mcp/kanban.org"))

  ;; Emacs rendering
  (println (render-to-emacs "/home/lages/dotfiles/gitthings/hive-mcp/kanban.org"))

  ;; Custom renderer
  (println (render-file (terminal-renderer {:column-width 35 :max-cards 5})
                        "/home/lages/dotfiles/gitthings/hive-mcp/kanban.org")))
