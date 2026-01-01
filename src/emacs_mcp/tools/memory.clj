(ns emacs-mcp.tools.memory
  "MCP tool handlers for memory operations.
   
   Extracted from emacs-mcp.tools for modularity.
   All 14 memory-related handlers for project memory management."
  (:require [emacs-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [emacs-mcp.emacsclient :as ec]
            [emacs-mcp.elisp :as el]
            [emacs-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Helper: Check if emacs-mcp.el is loaded
(defn- emacs-mcp-el-available?
  "Check if emacs-mcp.el is loaded in Emacs."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'emacs-mcp)")]
    (and success (= result "t"))))

(defn handle-mcp-memory-add
  "Add an entry to project memory.
   After successful elisp memory add, auto-indexes in Chroma if configured."
  [{:keys [type content tags duration]}]
  (log/info "mcp-memory-add:" type)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          duration-str (if duration (pr-str duration) "nil")
          elisp (format "(json-encode (emacs-mcp-api-memory-add %s %s %s %s))"
                        (pr-str type)
                        (pr-str content)
                        tags-str
                        duration-str)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (do
          ;; Try to auto-index in Chroma if configured
          (when (chroma/embedding-configured?)
            (try
              (let [parsed-result (json/read-str result :key-fn keyword)
                    entry-id (:id parsed-result)]
                (when entry-id
                  (chroma/index-memory-entry! {:id entry-id
                                               :content content
                                               :type type
                                               :tags tags})
                  (log/info "Auto-indexed memory entry in Chroma:" entry-id)))
              (catch Exception e
                (log/warn "Failed to auto-index memory entry in Chroma:"
                          (ex-message e)))))
          {:type "text" :text result})
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-query
  "Query project memory by type."
  [{:keys [type tags limit duration]}]
  (log/info "mcp-memory-query:" type)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          limit-val (or limit 20)
          duration-str (if duration (pr-str duration) "nil")
          elisp (format "(json-encode (emacs-mcp-api-memory-query %s %s %d %s))"
                        (pr-str type)
                        tags-str
                        limit-val
                        duration-str)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-query-metadata
  "Query project memory by type, returning only metadata (id, type, preview, tags, created).
  Use this for efficient browsing - returns ~10x fewer tokens than full query.
  Follow up with mcp_memory_get_full to fetch specific entries."
  [{:keys [type tags limit]}]
  (log/info "mcp-memory-query-metadata:" type)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          limit-val (or limit 20)
          elisp (format "(json-encode (emacs-mcp-api-memory-query-metadata %s %s %d))"
                        (pr-str type)
                        tags-str
                        limit-val)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-get-full
  "Get full content of a memory entry by ID.
  Use after mcp_memory_query_metadata to fetch specific entries."
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (format "(json-encode (emacs-mcp-api-memory-get-full %s))"
                        (pr-str id))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-search-semantic
  "Search project memory using semantic similarity (vector search).
   Requires Chroma to be configured with an embedding provider."
  [{:keys [query limit type]}]
  (log/info "mcp-memory-search-semantic:" query)
  (let [status (chroma/status)]
    (if-not (:configured? status)
      {:type "text"
       :text (json/write-str
              {:error "Chroma semantic search not configured"
               :message "To enable semantic search, configure Chroma with an embedding provider. See emacs-mcp.chroma namespace."
               :status status})
       :isError true}
      (try
        (let [results (chroma/search-similar query
                                             :limit (or limit 10)
                                             :type type)
              ;; Format results for user-friendly output
              formatted (mapv (fn [{:keys [id document metadata distance]}]
                                {:id id
                                 :type (get metadata :type)
                                 :tags (when-let [t (get metadata :tags)]
                                         (when (not= t "")
                                           (str/split t #",")))
                                 :distance distance
                                 :preview (when document
                                            (subs document 0 (min 200 (count document))))})
                              results)]
          {:type "text"
           :text (json/write-str {:results formatted
                                  :count (count formatted)
                                  :query query})})
        (catch Exception e
          {:type "text"
           :text (json/write-str {:error (str "Semantic search failed: " (.getMessage e))
                                  :status status})
           :isError true})))))

(defn handle-mcp-memory-set-duration
  "Set duration category for a memory entry."
  [{:keys [id duration]}]
  (log/info "mcp-memory-set-duration:" id duration)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-set-duration"
                                          id duration)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-promote
  "Promote memory entry to longer duration."
  [{:keys [id]}]
  (log/info "mcp-memory-promote:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-promote" id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-demote
  "Demote memory entry to shorter duration."
  [{:keys [id]}]
  (log/info "mcp-memory-demote:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-demote" id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-cleanup-expired
  "Remove all expired memory entries."
  [_]
  (log/info "mcp-memory-cleanup-expired")
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-cleanup-expired")
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-expiring-soon
  "List memory entries expiring within N days."
  [{:keys [days]}]
  (log/info "mcp-memory-expiring-soon:" (or days 7))
  (if (emacs-mcp-el-available?)
    (let [days-val (or days 7)
          elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-expiring-soon" days-val)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-log-access
  "Log access to a memory entry.
   Increments access-count and updates last-accessed timestamp."
  [{:keys [id]}]
  (log/info "mcp-memory-log-access:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-memory"
                                          "emacs-mcp-memory-log-access"
                                          id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-feedback
  "Submit helpfulness feedback for a memory entry.
   feedback should be 'helpful' or 'unhelpful'."
  [{:keys [id feedback]}]
  (log/info "mcp-memory-feedback:" id feedback)
  (if (emacs-mcp-el-available?)
    (let [fn-name (case feedback
                    "helpful" "emacs-mcp-memory-mark-helpful"
                    "unhelpful" "emacs-mcp-memory-mark-unhelpful"
                    (throw (ex-info "Invalid feedback type" {:feedback feedback})))
          elisp (el/require-and-call-json "emacs-mcp-memory" fn-name id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-helpfulness-ratio
  "Get helpfulness ratio for a memory entry.
   Returns helpful/(helpful+unhelpful) or null if no feedback."
  [{:keys [id]}]
  (log/info "mcp-memory-helpfulness-ratio:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-memory"
                                          "emacs-mcp-memory-helpfulness-ratio"
                                          id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-check-duplicate
  "Check if content already exists in memory (duplicate detection)."
  [{:keys [type content]}]
  (log/info "mcp-memory-check-duplicate:" type)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api"
                                          "emacs-mcp-api-memory-check-duplicate"
                                          type content)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "mcp_memory_add"
    :description "Add an entry to project memory. Types: note, snippet, convention, decision. Optionally specify duration for TTL: ephemeral (1 day), short (7 days), medium (30 days), long (90 days), permanent (never expires). Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision"]
                                       :description "Type of memory entry"}
                               "content" {:type "string"
                                          :description "Content of the memory entry"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags for categorization"}
                               "duration" {:type "string"
                                           :enum ["ephemeral" "short" "medium" "long" "permanent"]
                                           :description "Duration/TTL category (default: medium)"}}
                  :required ["type" "content"]}
    :handler handle-mcp-memory-add}

   {:name "mcp_memory_query"
    :description "Query project memory by type. Returns stored notes, snippets, conventions, or decisions. Optionally filter by duration category. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision" "conversation"]
                                       :description "Type of memory entries to query"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags to filter by"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results (default: 20)"}
                               "duration" {:type "string"
                                           :enum ["ephemeral" "short" "medium" "long" "permanent"]
                                           :description "Filter by duration category"}}
                  :required ["type"]}
    :handler handle-mcp-memory-query}

   {:name "mcp_memory_query_metadata"
    :description "Query project memory by type, returning only metadata (id, type, preview, tags, created). Use this for efficient browsing - returns ~10x fewer tokens than full query. Follow up with mcp_memory_get_full to fetch specific entries. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision" "conversation"]
                                       :description "Type of memory entries to query"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags to filter by"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results (default: 20)"}}
                  :required ["type"]}
    :handler handle-mcp-memory-query-metadata}

   {:name "mcp_memory_get_full"
    :description "Get full content of a memory entry by ID. Use after mcp_memory_query_metadata to fetch specific entries when you need the full content. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry to retrieve"}}
                  :required ["id"]}
    :handler handle-mcp-memory-get-full}

   {:name "mcp_memory_search_semantic"
    :description "Search project memory using semantic similarity (vector search). Finds conceptually related entries even without exact keyword matches. Requires Chroma to be configured with an embedding provider."
    :inputSchema {:type "object"
                  :properties {"query" {:type "string"
                                        :description "Natural language query to search for semantically similar memory entries"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results to return (default: 10)"}
                               "type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision"]
                                       :description "Optional filter by memory type"}}
                  :required ["query"]}
    :handler handle-mcp-memory-search-semantic}

   {:name "mcp_memory_set_duration"
    :description "Set the duration/TTL category for a memory entry. Use to manually adjust how long a memory persists."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry"}
                               "duration" {:type "string"
                                           :enum ["ephemeral" "short" "medium" "long" "permanent"]
                                           :description "New duration category"}}
                  :required ["id" "duration"]}
    :handler handle-mcp-memory-set-duration}

   {:name "mcp_memory_promote"
    :description "Promote a memory entry to a longer duration category. Moves ephemeral->short->medium->long->permanent."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry to promote"}}
                  :required ["id"]}
    :handler handle-mcp-memory-promote}

   {:name "mcp_memory_demote"
    :description "Demote a memory entry to a shorter duration category. Moves permanent->long->medium->short->ephemeral."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry to demote"}}
                  :required ["id"]}
    :handler handle-mcp-memory-demote}

   {:name "mcp_memory_cleanup_expired"
    :description "Remove all expired memory entries. Call periodically to clean up old ephemeral and short-lived memories."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-memory-cleanup-expired}

   {:name "mcp_memory_expiring_soon"
    :description "List memory entries expiring within the specified number of days. Useful for reviewing and promoting important memories before they expire."
    :inputSchema {:type "object"
                  :properties {"days" {:type "integer"
                                       :description "Number of days to look ahead (default: 7)"}}
                  :required []}
    :handler handle-mcp-memory-expiring-soon}

   {:name "mcp_memory_log_access"
    :description "Log access to a memory entry. Increments access-count and updates last-accessed timestamp. Use when retrieving a memory to track usage patterns."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry accessed"}}
                  :required ["id"]}
    :handler handle-mcp-memory-log-access}

   {:name "mcp_memory_feedback"
    :description "Submit helpfulness feedback for a memory entry. Tracks whether memories are useful for future retrieval decisions."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry"}
                               "feedback" {:type "string"
                                           :enum ["helpful" "unhelpful"]
                                           :description "Whether the memory was helpful"}}
                  :required ["id" "feedback"]}
    :handler handle-mcp-memory-feedback}

   {:name "mcp_memory_helpfulness_ratio"
    :description "Get the helpfulness ratio for a memory entry. Returns helpful/(helpful+unhelpful) or null if no feedback has been submitted."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry"}}
                  :required ["id"]}
    :handler handle-mcp-memory-helpfulness-ratio}])
