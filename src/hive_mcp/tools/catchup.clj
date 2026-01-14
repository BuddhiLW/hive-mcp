(ns hive-mcp.tools.catchup
  "Native Catchup workflow implementation.

   Gathers session context from Chroma memory with project scoping.
   Designed for the /catchup skill to restore context at session start.
   
   Priority Loading:
   - Swarm conventions tagged 'catchup-priority' are surfaced FIRST
   - These help coordinator trust swarm patterns immediately"
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Project Context Helpers
;; =============================================================================

(defn- get-current-project-id
  "Get current project ID from Emacs, or 'global' if not in a project.
   When directory is provided, uses that path to determine project context."
  ([] (get-current-project-id nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--project-id %s)" (pr-str directory))
                   "(hive-mcp-memory--project-id)")
           {:keys [success result]} (ec/eval-elisp elisp)]
       (if (and success result (not= result "nil"))
         (str/replace result #"\"" "")
         "global"))
     (catch Exception _
       "global"))))

(defn- get-current-project-name
  "Get current project name from Emacs.
   When directory is provided, uses that path to determine project context."
  ([] (get-current-project-name nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--get-project-name %s)" (pr-str directory))
                   "(hive-mcp-memory--get-project-name)")
           {:keys [success result]} (ec/eval-elisp elisp)]
       (if (and success result (not= result "nil"))
         (str/replace result #"\"" "")
         nil))
     (catch Exception _
       nil))))

;; =============================================================================
;; Entry Transformation
;; =============================================================================

(defn- entry->catchup-meta
  "Convert a Chroma entry to catchup metadata format.
   Returns map with :id, :type, :preview, :tags."
  [entry preview-len]
  (let [content (:content entry)
        content-str (if (string? content)
                      content
                      (str content))
        preview (subs content-str 0 (min (count content-str) (or preview-len 80)))]
    {:id (:id entry)
     :type (name (or (:type entry) "note"))
     :preview preview
     :tags (vec (or (:tags entry) []))}))

;; =============================================================================
;; Scope Filtering
;; =============================================================================

(defn- matches-project-scope?
  "Check if entry matches project scope (project + global).
   Matches both project-name and project-id for robustness since
   wrap stores with project-name but we may query with project-id."
  [entry project-name project-id]
  (let [tags (set (or (:tags entry) []))
        name-scope (when project-name (str "scope:project:" project-name))
        id-scope (when project-id (str "scope:project:" project-id))]
    (or (and name-scope (contains? tags name-scope))
        (and id-scope (contains? tags id-scope))
        (contains? tags "scope:global"))))

(defn- query-scoped-entries
  "Query Chroma entries filtered by project scope.
   Uses both project-name and project-id for scope matching."
  [entry-type tags project-name project-id limit]
  (when (chroma/embedding-configured?)
    (let [;; Query with over-fetch to allow for filtering
          entries (chroma/query-entries :type entry-type
                                        :limit (* (or limit 20) 5))
          ;; Filter by scope (matches name, id, or global)
          scoped (filter #(matches-project-scope? % project-name project-id) entries)
          ;; Filter by tags if provided
          tag-filtered (if (seq tags)
                         (filter (fn [entry]
                                   (let [entry-tags (set (:tags entry))]
                                     (every? #(contains? entry-tags %) tags)))
                                 scoped)
                         scoped)]
      (take (or limit 20) tag-filtered))))

;; =============================================================================
;; Git Context
;; =============================================================================

(defn- gather-git-info
  "Gather git information from Emacs for the given directory."
  [directory]
  (try
    (let [git-elisp (if directory
                      (format "(let ((default-directory %s))
                                 (json-encode
                                  (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                                        :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                                        :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%%h - %%s' 2>/dev/null || echo 'none'\")))))"
                              (pr-str directory))
                      "(json-encode
                         (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                               :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                               :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%h - %s' 2>/dev/null || echo 'none'\"))))")
          {:keys [success result]} (ec/eval-elisp git-elisp)]
      (when success
        (json/read-str result :key-fn keyword)))
    (catch Exception _
      {:branch "unknown" :uncommitted false :last-commit "unknown"})))

;; =============================================================================
;; Main Catchup Handler
;; =============================================================================

(defn handle-native-catchup
  "Native Clojure catchup implementation that queries Chroma directly.
   Returns structured catchup data with proper project scoping.
   Uses both project-name and project-id for scope matching to ensure
   compatibility with wrap workflow (which stores using project-name).

   Priority conventions (tagged 'catchup-priority') are surfaced first
   to help the coordinator trust swarm patterns immediately.

   Args can contain :directory to specify the project context explicitly,
   which fixes the issue where Emacs's current buffer determines project
   instead of Claude Code's working directory."
  [args]
  (let [directory (:directory args)]
    (log/info "native-catchup: querying Chroma with project scope" {:directory directory})
    (if-not (chroma/embedding-configured?)
      {:type "text"
       :text (json/write-str {:success false
                              :error "Chroma not configured"
                              :message "Memory query requires Chroma with embedding provider"})
       :isError true}
      (try
        (let [project-id (get-current-project-id directory)
              project-name (get-current-project-name directory)
              ;; Include both name and id scopes for display
              scopes (cond-> ["scope:global"]
                       project-name (conj (str "scope:project:" project-name))
                       (and project-id (not= project-id project-name))
                       (conj (str "scope:project:" project-id)))

              ;; PRIORITY: Query swarm conventions with catchup-priority tag FIRST
              ;; These help coordinator trust swarm patterns immediately
              priority-conventions (query-scoped-entries "convention"
                                                         ["catchup-priority"]
                                                         project-name project-id 5)

              ;; Query each type from Chroma with scope filtering (pass both name and id)
              sessions (query-scoped-entries "note" ["session-summary"] project-name project-id 3)
              decisions (query-scoped-entries "decision" nil project-name project-id 10)
              ;; Regular conventions (exclude priority ones to avoid duplicates)
              all-conventions (query-scoped-entries "convention" nil project-name project-id 15)
              priority-ids (set (map :id priority-conventions))
              conventions (remove #(contains? priority-ids (:id %)) all-conventions)
              snippets (query-scoped-entries "snippet" nil project-name project-id 5)

              ;; Query expiring entries (all types, filter later)
              all-expiring (chroma/query-entries :limit 50)
              expiring (->> all-expiring
                            (filter #(matches-project-scope? % project-name project-id))
                            (filter (fn [e]
                                      (when-let [exp (:expires e)]
                                        (let [exp-time (try (java.time.ZonedDateTime/parse exp)
                                                            (catch Exception _ nil))
                                              now (java.time.ZonedDateTime/now)
                                              week-later (.plusDays now 7)]
                                          (and exp-time
                                               (.isBefore exp-time week-later))))))
                            (take 5))

              ;; Get git info from Emacs
              git-info (gather-git-info directory)

              ;; Convert to metadata format - FULL CONTENT for priority conventions
              priority-meta (mapv (fn [e]
                                    {:id (:id e)
                                     :type "convention"
                                     :tags (vec (or (:tags e) []))
                                     :content (:content e)}) ;; Full content!
                                  priority-conventions)
              sessions-meta (mapv #(entry->catchup-meta % 80) sessions)
              decisions-meta (mapv #(entry->catchup-meta % 80) decisions)
              conventions-meta (mapv #(entry->catchup-meta % 80) conventions)
              snippets-meta (mapv #(entry->catchup-meta % 60) snippets)
              expiring-meta (mapv #(entry->catchup-meta % 80) expiring)]

          {:type "text"
           :text (json/write-str
                  {:success true
                   :project (or project-name project-id "global")
                   :scopes scopes
                   :git git-info
                   :counts {:priority-conventions (count priority-meta)
                            :sessions (count sessions-meta)
                            :decisions (count decisions-meta)
                            :conventions (count conventions-meta)
                            :snippets (count snippets-meta)
                            :expiring (count expiring-meta)}
                   ;; Priority conventions at TOP with FULL content
                   :priority-conventions priority-meta
                   :context {:sessions sessions-meta
                             :decisions decisions-meta
                             :conventions conventions-meta
                             :snippets snippets-meta
                             :expiring expiring-meta}
                   :hint "Priority conventions loaded with full content. Use mcp_memory_get_full for other entries."})})
        (catch Exception e
          (log/error e "native-catchup failed")
          {:type "text"
           :text (json/write-str {:success false
                                  :error (.getMessage e)})
           :isError true})))))
