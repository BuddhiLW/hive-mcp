(ns hive-mcp.presets
  "Chroma vector database integration for swarm presets.

   Provides:
   - Semantic search over presets (e.g., 'find testing-focused preset')
   - Migration from .md files to Chroma
   - File-based fallback when Chroma unavailable

   Collection Schema:
     id: preset name (e.g., 'tdd', 'clarity')
     content: full markdown content
     metadata:
       - name: human-readable name
       - category: coding, testing, coordination, architecture, workflow
       - tags: comma-separated tags for filtering
       - source: 'file' or 'memory'
       - file-path: original .md file path (if from file)

   Usage:
     ;; Migrate all presets from directory
     (migrate-presets-from-dir! \"/path/to/presets\")

     ;; Semantic search
     (search-presets \"testing discipline\" :limit 3)

     ;; Get specific preset
     (get-preset \"tdd\")"
  (:require [hive-mcp.chroma :as chroma]
            [clojure-chroma-client.api :as chroma-api]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Configuration
;;; ============================================================

(def ^:private collection-name "hive-mcp-presets")

(def ^:private category-keywords
  "Keywords to auto-detect preset category from content."
  {"testing" #"(?i)test|TDD|red.?green|assertion|mock"
   "coding" #"(?i)SOLID|clean.?code|refactor|DRY|KISS"
   "architecture" #"(?i)DDD|domain|layer|boundary|aggregate"
   "coordination" #"(?i)hivemind|swarm|spawn|coordinate|master"
   "workflow" #"(?i)workflow|task|step|process|pipeline"})

;;; ============================================================
;;; Collection Management
;;; ============================================================

(defonce ^:private collection-cache (atom nil))

(defn- get-or-create-collection
  "Get existing presets collection or create new one."
  []
  (if-let [coll @collection-cache]
    coll
    (let [provider (chroma/get-embedding-provider)]
      (when-not provider
        (throw (ex-info "Embedding provider not configured. Call chroma/set-embedding-provider! first."
                        {:type :no-embedding-provider})))
      (let [dim (chroma/embedding-dimension provider)
            existing (try
                       @(chroma-api/get-collection collection-name)
                       (catch Exception _ nil))]
        (if existing
          (do
            (reset! collection-cache existing)
            (log/info "Using existing presets collection:" collection-name)
            existing)
          (let [new-coll @(chroma-api/create-collection
                           collection-name
                           {:metadata {:dimension dim
                                       :created-by "hive-mcp"
                                       :purpose "swarm-presets"}})]
            (reset! collection-cache new-coll)
            (log/info "Created presets collection:" collection-name "dimension:" dim)
            new-coll))))))

(defn reset-collection-cache!
  "Reset the collection cache."
  []
  (reset! collection-cache nil))

;;; ============================================================
;;; Category Detection
;;; ============================================================

(defn- detect-category
  "Auto-detect category from preset content."
  [content]
  (let [matches (for [[cat pattern] category-keywords
                      :when (re-find pattern content)]
                  cat)]
    (or (first matches) "general")))

(defn- extract-title
  "Extract title from markdown content (first H1)."
  [content]
  (when-let [[_ title] (re-find #"^#\s+(.+?)(?:\n|$)" content)]
    (str/trim title)))

(defn- extract-tags-from-content
  "Extract potential tags from markdown content."
  [content name]
  (let [keywords (set (re-seq #"\*\*([A-Za-z-]+)\*\*" content))
        h2-headings (re-seq #"##\s+([A-Za-z ]+)" content)
        base-tags [name (detect-category content)]]
    (->> (concat base-tags
                 (map second keywords)
                 (map (comp str/lower-case str/trim second) h2-headings))
         (map str/lower-case)
         (filter #(< (count %) 30))
         (take 10)
         distinct
         (str/join ","))))

;;; ============================================================
;;; File Operations
;;; ============================================================

(defn- read-preset-file
  "Read a preset .md file and return parsed structure."
  [file-path]
  (let [content (slurp file-path)
        name (-> file-path io/file .getName (str/replace #"\.md$" ""))
        title (or (extract-title content) name)]
    {:id name
     :name name
     :title title
     :content content
     :category (detect-category content)
     :tags (extract-tags-from-content content name)
     :source "file"
     :file-path file-path}))

(defn scan-presets-dir
  "Scan directory for .md preset files."
  [dir-path]
  (let [dir (io/file dir-path)]
    (when (.isDirectory dir)
      (->> (.listFiles dir)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".md")
                         (not= (.getName %) "README.md")))
           (map #(.getAbsolutePath %))
           (map read-preset-file)))))

;;; ============================================================
;;; Indexing
;;; ============================================================

(defn- preset-to-document
  "Convert preset to searchable document string."
  [{:keys [name title content category tags]}]
  (str "Preset: " name "\n"
       "Title: " title "\n"
       "Category: " category "\n"
       "Tags: " tags "\n\n"
       content))

(defn index-preset!
  "Index a single preset in Chroma.
   Returns preset ID on success."
  [{:keys [id name title content category tags source file-path] :as preset}]
  (when-not (chroma/embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        provider (chroma/get-embedding-provider)
        doc-text (preset-to-document preset)
        embedding (chroma/embed-text provider doc-text)]
    @(chroma-api/add coll [{:id id
                            :embedding embedding
                            :document doc-text
                            :metadata {:name name
                                       :title (or title name)
                                       :category category
                                       :tags (or tags "")
                                       :source source
                                       :file-path (or file-path "")}}]
                     :upsert? true)
    (log/debug "Indexed preset:" id)
    id))

(defn index-presets!
  "Index multiple presets in batch."
  [presets]
  (when-not (chroma/embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        provider (chroma/get-embedding-provider)
        docs (mapv preset-to-document presets)
        embeddings (chroma/embed-batch provider docs)
        records (mapv (fn [preset doc emb]
                        {:id (:id preset)
                         :embedding emb
                         :document doc
                         :metadata {:name (:name preset)
                                    :title (or (:title preset) (:name preset))
                                    :category (:category preset)
                                    :tags (or (:tags preset) "")
                                    :source (:source preset)
                                    :file-path (or (:file-path preset) "")}})
                      presets docs embeddings)]
    @(chroma-api/add coll records :upsert? true)
    (log/info "Indexed" (count presets) "presets")
    (mapv :id presets)))

;;; ============================================================
;;; Migration
;;; ============================================================

(defn migrate-presets-from-dir!
  "Migrate all .md preset files from directory to Chroma.
   Returns {:migrated [ids] :failed [{:name :error}]}"
  [dir-path]
  (log/info "Migrating presets from:" dir-path)
  (let [presets (scan-presets-dir dir-path)]
    (if (empty? presets)
      {:migrated [] :failed [] :message "No preset files found"}
      (try
        (let [ids (index-presets! presets)]
          {:migrated ids
           :failed []
           :count (count ids)
           :message (str "Successfully migrated " (count ids) " presets")})
        (catch Exception e
          {:migrated []
           :failed (mapv (fn [p] {:name (:name p) :error (str e)}) presets)
           :message (str "Migration failed: " (.getMessage e))})))))

;;; ============================================================
;;; Semantic Search
;;; ============================================================

(defn search-presets
  "Search presets using semantic similarity.

   Options:
     :limit - Max results (default: 5)
     :category - Filter by category

   Returns seq of {:id, :name, :title, :category, :tags, :distance, :preview}"
  [query-text & {:keys [limit category] :or {limit 5}}]
  (when-not (chroma/embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        provider (chroma/get-embedding-provider)
        query-embedding (chroma/embed-text provider query-text)
        where-clause (when category {:category category})
        results @(chroma-api/query coll query-embedding
                                   :num-results limit
                                   :where where-clause
                                   :include #{:documents :metadatas :distances})]
    (log/debug "Preset search for:" (subs query-text 0 (min 50 (count query-text)))
               "found:" (count results))
    (mapv (fn [{:keys [id document metadata distance]}]
            {:id id
             :name (get metadata :name)
             :title (get metadata :title)
             :category (get metadata :category)
             :tags (when-let [t (get metadata :tags)]
                     (when (not= t "")
                       (str/split t #",")))
             :source (get metadata :source)
             :distance distance
             :preview (when document
                        (subs document 0 (min 300 (count document))))})
          results)))

;;; ============================================================
;;; Retrieval
;;; ============================================================

(defn get-preset
  "Get a specific preset by ID/name from Chroma.
   Returns full content or nil if not found."
  [preset-id]
  (try
    (let [coll (get-or-create-collection)
          results @(chroma-api/get coll :ids [preset-id] :include #{:documents :metadatas})]
      (when-let [{:keys [id document metadata]} (first results)]
        {:id id
         :name (get metadata :name)
         :title (get metadata :title)
         :category (get metadata :category)
         :tags (when-let [t (get metadata :tags)]
                 (when (not= t "")
                   (str/split t #",")))
         :source (get metadata :source)
         :content document}))
    (catch Exception e
      (log/debug "Failed to get preset from Chroma:" preset-id (.getMessage e))
      nil)))

(defn list-presets
  "List all presets in Chroma collection.
   Returns seq of {:id :name :title :category :source}"
  []
  (try
    (let [coll (get-or-create-collection)
          results @(chroma-api/get coll :include [:metadatas])]
      (mapv (fn [{:keys [id metadata]}]
              {:id id
               :name (get metadata :name)
               :title (get metadata :title)
               :category (get metadata :category)
               :source (get metadata :source)})
            results))
    (catch Exception e
      (log/warn "Failed to list presets:" (.getMessage e))
      [])))

;;; ============================================================
;;; Fallback (File-based)
;;; ============================================================

(defn get-preset-from-file
  "Fallback: get preset directly from .md file."
  [preset-dir preset-name]
  (let [file-path (str preset-dir "/" preset-name ".md")]
    (when (.exists (io/file file-path))
      (read-preset-file file-path))))

;;; ============================================================
;;; Status
;;; ============================================================

(defn status
  "Get presets integration status."
  []
  (let [base {:collection collection-name
              :chroma-configured? (chroma/embedding-configured?)}]
    (if (chroma/embedding-configured?)
      (try
        (let [presets (list-presets)]
          (assoc base
                 :count (count presets)
                 :categories (frequencies (map :category presets))
                 :sources (frequencies (map :source presets))))
        (catch Exception e
          (assoc base :error (str e))))
      base)))

(defn delete-preset!
  "Delete a preset from the Chroma index."
  [preset-id]
  (let [coll (get-or-create-collection)]
    @(chroma-api/delete coll :ids [preset-id])
    (log/debug "Deleted preset from Chroma:" preset-id)
    preset-id))
