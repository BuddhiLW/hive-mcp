(ns emacs-mcp.tools.org
  "Org-mode tool handlers using native Clojure parser.

   Provides handlers for org file parsing, writing, querying,
   and kanban board operations without elisp dependencies."
  (:require [emacs-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [emacs-mcp.org-clj.parser :as org-parser]
            [emacs-mcp.org-clj.writer :as org-writer]
            [emacs-mcp.org-clj.query :as org-query]
            [emacs-mcp.org-clj.transform :as org-transform]
            [emacs-mcp.org-clj.render :as org-render]
            [clojure.data.json :as json]))

(defn handle-org-clj-parse
  "Parse an org file and return its structure as JSON."
  [{:keys [file_path]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          json-str (json/write-str doc)]
      (mcp-success json-str))
    (catch Exception e
      (mcp-error (str "Error parsing org file: " (.getMessage e))))))

(defn handle-org-clj-write
  "Write an org document structure back to a file."
  [{:keys [file_path document]}]
  (try
    (let [doc (if (string? document)
                (json/read-str document :key-fn keyword)
                document)
          org-text (org-writer/write-document doc)]
      (spit file_path org-text)
      (mcp-success (str "Successfully wrote " (count org-text) " characters to " file_path)))
    (catch Exception e
      (mcp-error (str "Error writing org file: " (.getMessage e))))))

(defn handle-org-clj-query
  "Query headlines in a parsed org document."
  [{:keys [file_path query_type query_value]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          results (case query_type
                    "by_id" [(org-query/find-by-id doc query_value)]
                    "by_vibe_id" [(org-query/find-by-vibe-id doc query_value)]
                    "by_status" (org-query/find-by-status doc query_value)
                    "todo" (org-query/find-todo doc)
                    "done" (org-query/find-done doc)
                    "in_progress" (org-query/find-in-progress doc)
                    "stats" [(org-query/task-stats doc)]
                    (throw (ex-info (str "Unknown query type: " query_type)
                                    {:query_type query_type})))
          ;; Filter out nils
          results (filterv some? results)]
      (mcp-json results))
    (catch Exception e
      (mcp-error (str "Error querying org file: " (.getMessage e))))))

(defn handle-org-kanban-native-status
  "Get kanban status using native Clojure parser (no elisp dependency)."
  [{:keys [file_path]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          stats (org-query/task-stats doc)
          todos (org-query/find-todo doc)
          in-progress (org-query/find-in-progress doc)
          done (org-query/find-done doc)
          result {:stats stats
                  :by_status {:todo (mapv #(select-keys % [:title :properties]) todos)
                              :in_progress (mapv #(select-keys % [:title :properties]) in-progress)
                              :done (mapv #(select-keys % [:title :properties]) done)}
                  :file file_path
                  :backend "org-clj-native"}]
      {:type "text" :text (json/write-str result)})
    (catch Exception e
      {:type "text" :text (str "Error getting kanban status: " (.getMessage e)) :isError true})))

(defn handle-org-kanban-native-move
  "Move a task to a new status using native Clojure parser."
  [{:keys [file_path task_id new_status]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          ;; Check if task exists
          task (org-query/find-by-id doc task_id)]
      (if task
        (let [updated-doc (org-transform/set-status doc task_id new_status)
              org-text (org-writer/write-document updated-doc)]
          (spit file_path org-text)
          (mcp-json {:success true
                     :task_id task_id
                     :old_status (:keyword task)
                     :new_status new_status}))
        (mcp-error (json/write-str {:success false
                                    :error (str "Task not found: " task_id)}))))
    (catch Exception e
      (mcp-error (str "Error moving task: " (.getMessage e))))))

(defn handle-org-kanban-render
  "Render a visual kanban board from an org file."
  [{:keys [file_path format column_width max_cards]}]
  (try
    (let [renderer (case (or format "terminal")
                     "terminal" (org-render/terminal-renderer
                                 {:column-width (or column_width 28)
                                  :max-cards (or max_cards 10)})
                     "emacs" (org-render/emacs-renderer)
                     (throw (ex-info (str "Unknown format: " format) {:format format})))
          output (org-render/render-file renderer file_path)]
      (mcp-success output))
    (catch Exception e
      (mcp-error (str "Error rendering kanban: " (.getMessage e))))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "org_clj_parse"
    :description "Parse an org-mode file and return its structure as JSON. Uses native Clojure parser without elisp dependency."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the org file to parse"}}
                  :required ["file_path"]}
    :handler handle-org-clj-parse}

   {:name "org_clj_write"
    :description "Write an org document structure back to a file. Takes a JSON document and writes org-mode formatted text."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to write the org file"}
                               "document" {:type "string"
                                           :description "JSON string representing the org document structure"}}
                  :required ["file_path" "document"]}
    :handler handle-org-clj-write}

   {:name "org_clj_query"
    :description "Query headlines in an org file. Supports queries by ID, status, or get statistics."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the org file"}
                               "query_type" {:type "string"
                                             :enum ["by_id" "by_vibe_id" "by_status" "todo" "done" "in_progress" "stats"]
                                             :description "Type of query to perform"}
                               "query_value" {:type "string"
                                              :description "Value for the query (ID for by_id, status string for by_status)"}}
                  :required ["file_path" "query_type"]}
    :handler handle-org-clj-query}

   {:name "org_kanban_native_status"
    :description "Get kanban status from an org file using native Clojure parser. Returns task counts and lists by status."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the kanban org file"}}
                  :required ["file_path"]}
    :handler handle-org-kanban-native-status}

   {:name "org_kanban_native_move"
    :description "Move a task to a new status in an org file using native Clojure parser. Updates the file directly."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the kanban org file"}
                               "task_id" {:type "string"
                                          :description "ID of the task to move (from :ID property)"}
                               "new_status" {:type "string"
                                             :description "New TODO status (e.g., TODO, IN-PROGRESS, DONE)"}}
                  :required ["file_path" "task_id" "new_status"]}
    :handler handle-org-kanban-native-move}

   {:name "org_kanban_render"
    :description "Render a visual kanban board from an org file. Supports terminal ASCII art or Emacs org-mode format."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the kanban org file"}
                               "format" {:type "string"
                                         :enum ["terminal" "emacs"]
                                         :description "Output format: 'terminal' for ASCII art, 'emacs' for org-mode (default: terminal)"}
                               "column_width" {:type "integer"
                                               :description "Width of each column in terminal mode (default: 28)"}
                               "max_cards" {:type "integer"
                                            :description "Maximum cards per column in terminal mode (default: 10)"}}
                  :required ["file_path"]}
    :handler handle-org-kanban-render}])
