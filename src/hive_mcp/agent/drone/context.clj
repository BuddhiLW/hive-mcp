(ns hive-mcp.agent.drone.context
  "Smart context injection for drone agents.

   Provides rich file context to maximize drone success rate:
   - Surrounding lines around edit targets
   - Import/require extraction from ns forms
   - Related function signatures via kondo
   - Pre-lint analysis to surface existing issues
   - Relevant memory conventions based on task keywords

   CLARITY-A: Architectural performance via minimal but high-value context"
  (:require [hive-mcp.tools.kondo :as kondo]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; File Reading Utilities
;;; ============================================================

(defn- read-file-lines
  "Read file as vector of lines. Returns nil if file doesn't exist."
  [path]
  (try
    (when (.exists (io/file path))
      (str/split-lines (slurp path)))
    (catch Exception e
      (log/debug "Could not read file:" path (.getMessage e))
      nil)))

(defn read-surrounding-lines
  "Read surrounding lines around a target line number.

   Arguments:
     path        - File path (absolute or relative to project root)
     target-line - Line number to center on (1-indexed, nil = start of file)
     context     - Number of lines before and after (default: 20)

   Returns:
     Map with :content (string), :start-line, :end-line, or nil if file not found."
  [path target-line & [{:keys [context] :or {context 20}}]]
  (when-let [lines (read-file-lines path)]
    (let [total-lines (count lines)
          target (or target-line 1)
          start-line (max 1 (- target context))
          end-line (min total-lines (+ target context))
          ;; Convert to 0-indexed for subvec
          selected (subvec (vec lines) (dec start-line) end-line)
          numbered (map-indexed (fn [i line]
                                  (format "%4d: %s" (+ start-line i) line))
                                selected)]
      {:content (str/join "\n" numbered)
       :start-line start-line
       :end-line end-line
       :target-line target
       :total-lines total-lines})))

;;; ============================================================
;;; Namespace Analysis
;;; ============================================================

(defn- parse-ns-form
  "Parse ns form from file content. Returns the ns form or nil."
  [content]
  (try
    (let [forms (read-string (str "[" content "]"))]
      (first (filter #(and (list? %) (= 'ns (first %))) forms)))
    (catch Exception _
      nil)))

(defn extract-imports
  "Extract require/import clauses from a Clojure file.

   Arguments:
     path - File path

   Returns:
     Map with :namespace, :requires, :imports, :uses"
  [path]
  (when-let [content (try (slurp path) (catch Exception _ nil))]
    (when-let [ns-form (parse-ns-form content)]
      (let [ns-name (second ns-form)
            clauses (drop 2 ns-form)
            extract-clause (fn [kw]
                             (->> clauses
                                  (filter #(and (sequential? %) (= kw (first %))))
                                  (mapcat rest)
                                  (vec)))]
        {:namespace (str ns-name)
         :requires (extract-clause :require)
         :imports (extract-clause :import)
         :uses (extract-clause :use)}))))

(defn format-imports-context
  "Format imports as context string for drone."
  [imports-data]
  (when imports-data
    (let [{:keys [namespace requires]} imports-data]
      (when (seq requires)
        (str "### File Namespace & Dependencies\n"
             "```clojure\n"
             "(ns " namespace "\n"
             "  (:require " (str/join "\n            " (map pr-str requires)) "))\n"
             "```\n")))))

;;; ============================================================
;;; Kondo-Based Analysis
;;; ============================================================

(defn find-related-symbols
  "Find function signatures related to the task using kondo analysis.

   Arguments:
     path - File path to analyze
     task - Task description (used to extract keywords)

   Returns:
     Vector of {:name :arglists :ns :doc-preview} maps"
  [path task]
  (try
    (let [{:keys [analysis]} (kondo/run-analysis path)
          var-defs (:var-definitions analysis)
          ;; Extract potential function names from task
          task-words (-> task
                         str/lower-case
                         (str/replace #"[^a-z0-9\-]" " ")
                         (str/split #"\s+")
                         set)
          ;; Find definitions that match task keywords
          relevant-defs (->> var-defs
                             (filter (fn [vdef]
                                       (let [var-name (str/lower-case (str (:name vdef)))]
                                         (some #(str/includes? var-name %) task-words))))
                             (take 10))]
      (mapv (fn [vdef]
              {:name (str (:name vdef))
               :ns (str (:ns vdef))
               :arglists (:arglist-strs vdef)
               :row (:row vdef)
               :private (:private vdef)})
            relevant-defs))
    (catch Exception e
      (log/debug "Could not analyze file for related symbols:" path (.getMessage e))
      [])))

(defn format-related-symbols
  "Format related symbols as context string."
  [symbols]
  (when (seq symbols)
    (str "### Related Functions in File\n"
         (str/join "\n"
                   (map (fn [{:keys [name arglists private]}]
                          (str "- `" (when private "^:private ") name "` "
                               (when arglists (str ": " (str/join " | " arglists)))))
                        symbols))
         "\n")))

;;; ============================================================
;;; Lint Analysis
;;; ============================================================

(defn get-existing-warnings
  "Get existing lint warnings for a file.

   Arguments:
     path  - File path
     level - Severity level (:error, :warning, :info)

   Returns:
     Vector of {:row :col :level :type :message} maps"
  [path & [{:keys [level] :or {level :warning}}]]
  (try
    (let [{:keys [findings]} (kondo/run-analysis path)]
      (->> findings
           (filter #(case level
                      :error (= (:level %) :error)
                      :warning (#{:error :warning} (:level %))
                      :info true))
           (mapv #(select-keys % [:row :col :level :type :message]))
           (take 15)))  ; Limit to avoid context bloat
    (catch Exception e
      (log/debug "Could not lint file:" path (.getMessage e))
      [])))

(defn format-lint-context
  "Format lint findings as context for drone."
  [findings file-path]
  (when (seq findings)
    (str "### Existing Lint Issues in " (last (str/split file-path #"/")) "\n"
         "NOTE: Fix these if your changes touch affected lines:\n"
         (str/join "\n"
                   (map (fn [{:keys [row level message]}]
                          (format "- Line %d: [%s] %s" row (name level) message))
                        findings))
         "\n")))

;;; ============================================================
;;; Memory Conventions
;;; ============================================================

(defn get-relevant-conventions
  "Query memory for conventions relevant to the task.

   Uses semantic search when available, falls back to tag-based.

   Arguments:
     task       - Task description
     project-id - Project ID for scoping

   Returns:
     Vector of convention content strings"
  [task project-id]
  (try
    ;; Try semantic search first
    (let [results (chroma/search-similar task :type "convention" :limit 5)]
      (if (seq results)
        (->> results
             (map (fn [r]
                    ;; search-similar returns {:metadata {:content ...}}
                    (or (get-in r [:metadata :content])
                        (:document r))))
             (remove nil?)
             (take 3)
             vec)
        ;; Fallback: query all conventions and filter
        (let [all-convs (chroma/query-entries :type "convention"
                                              :project-id project-id
                                              :limit 20)]
          (->> all-convs
               (filter (fn [conv]
                         (let [content (str/lower-case (str (:content conv)))
                               tags (set (map str/lower-case (or (:tags conv) [])))]
                           ;; Basic relevance: check if any task word appears
                           (or (some #(str/includes? content %)
                                     (str/split (str/lower-case task) #"\s+"))
                               (some #(tags %) ["drone" "coding" "clojure"])))))
               (map :content)
               (take 3)
               vec))))
    (catch Exception e
      (log/debug "Could not query conventions:" (.getMessage e))
      [])))

(defn format-conventions-context
  "Format conventions as context string."
  [conventions]
  (when (seq conventions)
    (str "### Coding Conventions to Follow\n"
         (str/join "\n\n---\n\n"
                   (map-indexed (fn [i conv]
                                  (str (inc i) ". "
                                       ;; Truncate long conventions
                                       (if (> (count conv) 500)
                                         (str (subs conv 0 500) "...")
                                         conv)))
                                conventions))
         "\n")))

;;; ============================================================
;;; Knowledge Graph File Context
;;; ============================================================

(defn get-kg-file-knowledge
  "Check KG for existing knowledge about a file before reading it.
   Returns disc state and any knowledge entries grounded from this file.

   Arguments:
     file-path - Absolute file path

   Returns:
     Map with :disc (disc entity or nil), :stale? boolean,
     :staleness-score float, :knowledge-entries (grounded entries)"
  [file-path]
  (try
    (let [disc (kg-disc/get-disc file-path)
          stale? (when disc
                   (let [{:keys [hash exists?]} (kg-disc/file-content-hash file-path)]
                     (or (not exists?)
                         (and hash (:disc/content-hash disc)
                              (not= hash (:disc/content-hash disc))))))
          staleness (when disc (kg-disc/staleness-score disc))
          ;; Find knowledge entries grounded from this file
          ;; Search Chroma for entries with source-file metadata matching this path
          grounded-entries (try
                             (when (chroma/embedding-configured?)
                               (->> (chroma/query-entries :limit 10)
                                    (filter #(= file-path (get-in % [:metadata :source-file])))
                                    (take 5)))
                             (catch Exception _ nil))]
      {:disc disc
       :stale? (boolean stale?)
       :staleness-score (or staleness 1.0)
       :knowledge-entries (vec (or grounded-entries []))
       :has-knowledge? (or (some? disc) (seq grounded-entries))})
    (catch Exception e
      (log/debug "KG file knowledge lookup failed:" (.getMessage e))
      {:disc nil :stale? true :staleness-score 1.0
       :knowledge-entries [] :has-knowledge? false})))

(defn format-kg-file-context
  "Format KG knowledge about a file as context string for drone injection.
   Only includes context when relevant knowledge exists."
  [kg-info file-path]
  (when (:has-knowledge? kg-info)
    (let [{:keys [disc stale? staleness-score knowledge-entries]} kg-info
          sections (cond-> []
                     disc
                     (conj (str "- Disc state: "
                                (if stale? "STALE (file changed)" "fresh")
                                " (staleness: " (format "%.1f" (float staleness-score)) ")"
                                (when (:disc/read-count disc)
                                  (str ", read " (:disc/read-count disc) " times"))
                                (when (:disc/analyzed-at disc)
                                  (str ", last analyzed: " (:disc/analyzed-at disc)))))

                     (seq knowledge-entries)
                     (conj (str "- " (count knowledge-entries) " knowledge entries grounded from this file:"
                                (str/join "\n  "
                                          (map (fn [e]
                                                 (str "  * [" (or (:type e) "note") "] "
                                                      (subs (str (:content e))
                                                            0 (min 100 (count (str (:content e)))))))
                                               knowledge-entries)))))]
      (when (seq sections)
        (str "### KG Knowledge for " (last (str/split file-path #"/")) "\n"
             (str/join "\n" sections)
             "\n")))))

;;; ============================================================
;;; Main Context Builder
;;; ============================================================

(defn build-drone-context
  "Build rich context for drone delegation.

   Arguments:
     file-path    - Target file to modify
     task         - Task description
     project-root - Project root directory
     project-id   - Project ID for memory scoping

   Returns:
     Map with context sections:
       :file-snippet - Surrounding lines around likely edit location
       :imports      - Namespace and require statements
       :related-fns  - Related function signatures
       :lint-issues  - Existing lint warnings
       :conventions  - Relevant coding conventions
       :formatted    - Pre-formatted string for injection
   
   CLARITY-I: Validates paths don't escape project directory."
  [{:keys [file-path task project-root project-id target-line]}]
  ;; SECURITY FIX: Validate path containment using canonical resolution
  (let [file-obj (io/file (if (str/starts-with? file-path "/")
                            file-path
                            (str project-root "/" file-path)))
        abs-path (try
                   (let [canonical (.getCanonicalPath file-obj)
                         root-canonical (.getCanonicalPath (io/file project-root))]
                     (if (str/starts-with? canonical root-canonical)
                       canonical
                       (do
                         (log/warn "Path escapes project directory in build-drone-context"
                                   {:file file-path :canonical canonical :root root-canonical})
                         nil)))
                   (catch Exception e
                     (log/warn "Path validation failed" {:file file-path :error (.getMessage e)})
                     nil))]
    (when-not abs-path
      (log/warn "Skipping context build for invalid path" {:file file-path}))
    (when abs-path
      (let [;; KG-first: Check knowledge graph for existing file knowledge
            kg-info (get-kg-file-knowledge abs-path)
            kg-context-str (format-kg-file-context kg-info abs-path)
            ;; Gather context components
            snippet (read-surrounding-lines abs-path target-line {:context 20})
            imports (extract-imports abs-path)
            related (find-related-symbols abs-path task)
            lint (get-existing-warnings abs-path {:level :warning})
            conventions (get-relevant-conventions task project-id)
            ;; Format each section (KG context injected first for priority)
            formatted (str
                       (when kg-context-str
                         (str kg-context-str "\n"))
                       (format-imports-context imports)
                       "\n"
                       (when snippet
                         (str "### File Context (lines " (:start-line snippet)
                              "-" (:end-line snippet) ")\n"
                              "```clojure\n"
                              (:content snippet)
                              "\n```\n"))
                       "\n"
                       (format-related-symbols related)
                       "\n"
                       (format-lint-context lint file-path)
                       "\n"
                       (format-conventions-context conventions))]
        {:file-snippet snippet
         :imports imports
         :related-fns related
         :lint-issues lint
         :conventions conventions
         :kg-info kg-info
         :formatted formatted}))))

(defn format-full-context
  "Format full drone context for task augmentation.

   Used by drone.clj to inject rich context into task description."
  [context-data]
  (when context-data
    (str "## Smart Context (Auto-Generated)\n"
         (:formatted context-data)
         "\n")))
