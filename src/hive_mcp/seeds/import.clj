(ns hive-mcp.seeds.import
  "Import seed memories from markdown files.

   Seed files are markdown documents with YAML frontmatter containing
   metadata for memory entries. They provide a way to bootstrap the
   knowledge base with curated content.

   File structure:
     seeds/
       conventions/
         clojure-style.md
         git-workflow.md
       decisions/
         architecture-choice.md
       snippets/
         common-patterns.md

   Each .md file has YAML frontmatter:
     ---
     title: Clojure Style Guide
     type: convention
     tags: [clojure, style, best-practices]
     duration: permanent
     ---

     Content goes here...

   SOLID: SRP - Single responsibility for seed file parsing and import.
   CLARITY: I - Inputs are guarded at parse boundaries."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-yaml.core :as yaml]
            [hive-mcp.chroma :as chroma]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Configuration
;; ============================================================

(def ^:private default-seeds-dir "seeds")

(defn- get-seeds-dir
  "Get the seeds directory path, using project root detection."
  ([]
   (get-seeds-dir nil))
  ([base-dir]
   (let [dir (or base-dir (System/getProperty "user.dir"))]
     (io/file dir default-seeds-dir))))

;; ============================================================
;; Frontmatter Parsing
;; ============================================================

(defn- parse-frontmatter
  "Parse YAML frontmatter from markdown content.

   Returns {:frontmatter {...} :content \"...\"}
   Returns nil if no valid frontmatter found.

   CLARITY: Y - Yield safe failure on parse errors."
  [content]
  (when-let [[_ yaml-str body] (re-matches #"(?s)^---\n(.*?)\n---\n?(.*)" content)]
    (try
      {:frontmatter (yaml/parse-string yaml-str)
       :content (str/trim body)}
      (catch Exception e
        (log/warn "Failed to parse YAML frontmatter:" (.getMessage e))
        nil))))

(defn parse-seed-file
  "Parse a markdown seed file with YAML frontmatter.

   Returns map with:
     :title    - From frontmatter or filename
     :type     - Memory type (note, convention, decision, snippet)
     :tags     - Vector of tags
     :duration - TTL category
     :content  - Markdown content body
     :source   - Source file path

   Returns nil if file cannot be parsed."
  [path]
  (try
    (let [file (if (instance? java.io.File path) path (io/file path))
          filename (.getName file)
          content (slurp file)]
      (when-let [{:keys [frontmatter content]} (parse-frontmatter content)]
        (let [title (or (:title frontmatter)
                        (-> filename
                            (str/replace #"\.md$" "")
                            (str/replace #"[-_]" " ")
                            str/capitalize))]
          {:title title
           :type (or (:type frontmatter) "note")
           :tags (vec (or (:tags frontmatter) []))
           :duration (or (:duration frontmatter) "permanent")
           :content content
           :source (.getPath file)
           ;; Optional: project scope for multi-project seeds
           :project-id (:project-id frontmatter)})))
    (catch Exception e
      (log/error "Failed to read seed file:" path "-" (.getMessage e))
      nil)))

;; ============================================================
;; File Discovery
;; ============================================================

(defn find-seed-files
  "Find all markdown seed files in the seeds directory.

   Searches recursively through subdirectories.
   Returns sequence of java.io.File objects."
  ([]
   (find-seed-files nil))
  ([base-dir]
   (let [seeds-dir (get-seeds-dir base-dir)]
     (if (.exists seeds-dir)
       (->> (file-seq seeds-dir)
            (filter #(.isFile %))
            (filter #(str/ends-with? (.getName %) ".md"))
            vec)
       (do
         (log/warn "Seeds directory not found:" (.getPath seeds-dir))
         [])))))

;; ============================================================
;; Deduplication
;; ============================================================

(defn- memory-exists?
  "Check if a memory entry with this title already exists.
   Uses semantic search to find potential duplicates.

   Returns the existing entry ID if found, nil otherwise."
  [title type]
  (try
    ;; Search for entries with similar title
    (let [results (chroma/search-similar title :limit 5 :type type)]
      ;; Check if any result has very high similarity (low distance)
      (some (fn [{:keys [id distance metadata]}]
              (when (and (< distance 0.1)  ; Very similar
                         (= (:type metadata) type))
                id))
            results))
    (catch Exception _
      ;; Chroma not available, can't check duplicates
      nil)))

(defn- content-hash-exists?
  "Check if content with this hash already exists."
  [type content]
  (try
    (let [hash (chroma/content-hash content)]
      (chroma/find-duplicate type hash))
    (catch Exception _
      nil)))

;; ============================================================
;; Import Functions
;; ============================================================

(defn import-seed!
  "Import a single parsed seed as a memory entry.

   Arguments:
     seed-data - Map from parse-seed-file

   Options:
     :skip-duplicates? - Skip if similar entry exists (default: true)
     :project-id       - Override project scope

   Returns:
     {:status :imported/:skipped/:error
      :id     entry-id (if imported)
      :reason reason (if skipped/error)}"
  [seed-data & {:keys [skip-duplicates? project-id]
                :or {skip-duplicates? true}}]
  (let [{:keys [title type tags duration content source]} seed-data
        proj-id (or project-id (:project-id seed-data) "global")]
    (log/info "Importing seed:" title "type:" type)
    (try
      ;; Check for duplicates
      (if (and skip-duplicates?
               (or (content-hash-exists? type content)
                   (memory-exists? title type)))
        (do
          (log/info "Skipping duplicate:" title)
          {:status :skipped
           :reason "Duplicate content or title exists"
           :title title
           :source source})
        ;; Import the seed
        (let [full-content (str "# " title "\n\n" content)
              seed-tags (conj tags "seed" (str "seed-source:" source))
              entry-id (chroma/index-memory-entry!
                        {:type type
                         :content full-content
                         :tags seed-tags
                         :duration duration
                         :project-id proj-id
                         :content-hash (chroma/content-hash full-content)})]
          (log/info "Imported seed:" title "-> id:" entry-id)
          {:status :imported
           :id entry-id
           :title title
           :type type
           :source source}))
      (catch Exception e
        (log/error "Failed to import seed:" title "-" (.getMessage e))
        {:status :error
         :title title
         :source source
         :reason (.getMessage e)}))))

(defn import-all-seeds!
  "Import all seed files from the seeds directory.

   Options:
     :base-dir         - Override base directory (default: cwd)
     :skip-duplicates? - Skip existing entries (default: true)
     :project-id       - Override project scope (default: global)
     :dry-run?         - Parse but don't import (default: false)

   Returns:
     {:imported N
      :skipped  M
      :errors   [...]
      :details  [{:status :imported/:skipped/:error ...} ...]}"
  [& {:keys [base-dir skip-duplicates? project-id dry-run?]
      :or {skip-duplicates? true dry-run? false}}]
  (let [files (find-seed-files base-dir)
        _ (log/info "Found" (count files) "seed files")

        ;; Parse all files
        parsed (->> files
                    (map parse-seed-file)
                    (filter some?)
                    vec)
        _ (log/info "Parsed" (count parsed) "seed files successfully")

        ;; Import or dry-run
        results (if dry-run?
                  (mapv (fn [seed]
                          {:status :would-import
                           :title (:title seed)
                           :type (:type seed)
                           :tags (:tags seed)
                           :source (:source seed)})
                        parsed)
                  (mapv #(import-seed! %
                                       :skip-duplicates? skip-duplicates?
                                       :project-id project-id)
                        parsed))

        ;; Aggregate stats
        imported (count (filter #(= (:status %) :imported) results))
        skipped (count (filter #(= (:status %) :skipped) results))
        would-import (count (filter #(= (:status %) :would-import) results))
        errors (filterv #(= (:status %) :error) results)]

    (log/info "Seed import complete:"
              "imported=" imported
              "skipped=" skipped
              "errors=" (count errors)
              (when dry-run? "(dry-run)"))

    {:imported imported
     :skipped skipped
     :would-import would-import
     :errors (count errors)
     :error-details errors
     :details results
     :dry-run? dry-run?}))

;; ============================================================
;; Utility Functions
;; ============================================================

(defn list-seeds
  "List all available seed files with their parsed metadata.

   Returns sequence of maps with:
     :path  - File path
     :title - Parsed title
     :type  - Memory type
     :tags  - Tags vector

   Useful for previewing what would be imported."
  [& {:keys [base-dir]}]
  (let [files (find-seed-files base-dir)]
    (->> files
         (map (fn [f]
                (if-let [parsed (parse-seed-file f)]
                  {:path (.getPath f)
                   :title (:title parsed)
                   :type (:type parsed)
                   :tags (:tags parsed)
                   :duration (:duration parsed)}
                  {:path (.getPath f)
                   :error "Failed to parse"})))
         vec)))
