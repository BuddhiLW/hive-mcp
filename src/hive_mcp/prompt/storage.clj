(ns hive-mcp.prompt.storage
  "Storage adapter for prompt database.
   Abstracts file/database interaction for prompt persistence (CLARITY-L, DIP).

   Provides:
   - PromptStore protocol for storage abstraction
   - OrgPromptStore record for org-mode file storage
   - Serialization between prompt entries and org headlines"
  (:require [clojure.string :as str]
            [hive-mcp.org-clj.parser :as parser]
            [hive-mcp.org-clj.writer :as writer]
            [clojure.java.io :as io])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util UUID]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defn generate-id
  "Generate a unique ID for a prompt entry."
  []
  (let [now (LocalDateTime/now)
        fmt (DateTimeFormatter/ofPattern "yyyyMMddHHmmss")]
    (str (.format now fmt) "-" (subs (str (UUID/randomUUID)) 0 8))))

(defn now-timestamp
  "Get current timestamp in ISO format."
  []
  (let [now (LocalDateTime/now)
        fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss")]
    (.format now fmt)))

;;; ============================================================
;;; Serialization - Org-Mode Format
;;; ============================================================

(defn entry->headline
  "Convert a prompt entry to an org headline structure."
  [{:keys [id prompt accomplishes well-structured improvements
           category tags quality created updated source model context]}]
  {:level 2
   :keyword "NOTE"
   :title (str "[" (name category) "] "
               (if (> (count prompt) 50)
                 (str (subs prompt 0 47) "...")
                 prompt))
   :tags (mapv name tags)
   :properties (cond-> {"ID" id
                        "CATEGORY" (name category)
                        "QUALITY" (name quality)
                        "CREATED" created}
                 updated (assoc "UPDATED" updated)
                 source (assoc "SOURCE" source)
                 model (assoc "MODEL" model))
   ;; Content must be a vector of lines for the org writer
   :content (vec (concat
                  ["*** Prompt"
                   "#+begin_src text"]
                  (str/split-lines prompt)
                  ["#+end_src"
                   ""
                   "*** What It Accomplishes"]
                  (str/split-lines accomplishes)
                  [""
                   "*** Why Well-Structured"]
                  (str/split-lines well-structured)
                  (when improvements
                    (concat [""
                             "*** Improvements"]
                            (str/split-lines improvements)))
                  (when context
                    (concat [""
                             "*** Context"]
                            (str/split-lines context)))))})

(defn headline->entry
  "Convert an org headline back to a prompt entry."
  [{:keys [properties children title tags] :as _headline}]
  ;; Children are sub-headlines like *** Prompt, *** What It Accomplishes, etc.
  (let [get-child-content (fn [child-title]
                            (some->> children
                                     (filter #(= (:title %) child-title))
                                     first
                                     :content
                                     (remove #(or (str/starts-with? % "#+begin_src")
                                                  (str/starts-with? % "#+end_src")))
                                     (str/join "\n")
                                     str/trim))
        ;; Extract tags from title if parser put them there
        title-tags (when (str/includes? (or title "") ":")
                     (let [tag-match (re-find #":([^:]+(?::[^:]+)*):$" title)]
                       (when tag-match
                         (str/split (second tag-match) #":"))))]
    {:id (get properties :ID)
     :prompt (get-child-content "Prompt")
     :accomplishes (get-child-content "What It Accomplishes")
     :well-structured (get-child-content "Why Well-Structured")
     :improvements (get-child-content "Improvements")
     :category (keyword (get properties :CATEGORY "meta"))
     :tags (vec (or title-tags tags []))
     :quality (keyword (get properties :QUALITY "untested"))
     :created (get properties :CREATED)
     :updated (get properties :UPDATED)
     :source (get properties :SOURCE)
     :model (get properties :MODEL)
     :context (get-child-content "Context")}))

;;; ============================================================
;;; Storage Protocol (DIP)
;;; ============================================================

(defprotocol PromptStore
  "Protocol for prompt persistence operations.
   Implementations can use org-mode files, Chroma, SQLite, etc."
  (save-prompt! [this prompt]
    "Save a prompt entry. Returns {:saved true :id ...} on success.")
  (get-prompt [this id]
    "Get a prompt by ID. Returns entry map or nil.")
  (delete-prompt! [this id]
    "Delete a prompt by ID. Returns {:deleted true} on success.")
  (list-prompts [this] [this opts]
    "List prompts with optional filtering. Options: :category, :quality, :tags, :limit")
  (search-prompts [this query] [this query opts]
    "Search prompts by keyword. Options: :limit")
  (update-prompt! [this id updates]
    "Update a prompt entry. Returns updated entry.")
  (get-statistics [this]
    "Get statistics about stored prompts."))

;;; ============================================================
;;; Org-Mode File Implementation
;;; ============================================================

(defn- load-org-file
  "Load prompts from org file. Returns vector of entries."
  [file-path]
  (if (.exists (io/file file-path))
    (let [doc (parser/parse-document (slurp file-path))
          headlines (:headlines doc)]
      (mapv headline->entry headlines))
    []))

(defn- save-org-file!
  "Save prompts to org file."
  [file-path entries]
  (let [dir (io/file (.getParent (io/file file-path)))]
    (when-not (.exists dir)
      (.mkdirs dir)))
  (let [doc {:properties {"TITLE" "Prompt Engineering Knowledge Base"
                          "STARTUP" "overview"
                          "TODO" "NOTE | ARCHIVED"}
             :headlines (mapv entry->headline entries)}
        org-text (writer/write-document doc)]
    (spit file-path org-text)
    {:saved true :count (count entries) :file file-path}))

(defrecord OrgPromptStore [file-path]
  PromptStore

  (save-prompt! [_ prompt]
    (let [existing (load-org-file file-path)
          entry (assoc prompt
                       :id (or (:id prompt) (generate-id))
                       :created (or (:created prompt) (now-timestamp)))
          updated (conj existing entry)]
      (save-org-file! file-path updated)
      {:saved true :id (:id entry) :entry entry}))

  (get-prompt [_ id]
    (let [entries (load-org-file file-path)]
      (first (filter #(= (:id %) id) entries))))

  (delete-prompt! [_ id]
    (let [entries (load-org-file file-path)
          filtered (remove #(= (:id %) id) entries)]
      (save-org-file! file-path (vec filtered))
      {:deleted true :id id}))

  (list-prompts [this]
    (list-prompts this {}))

  (list-prompts [_ {:keys [category quality tags limit] :or {limit 50}}]
    (let [entries (load-org-file file-path)
          filtered (cond->> entries
                     category (filter #(= (:category %) category))
                     quality (filter #(= (:quality %) quality))
                     (seq tags) (filter #(some (set (:tags %)) tags))
                     true (take limit))]
      {:count (count filtered)
       :total (count entries)
       :entries (vec filtered)}))

  (search-prompts [this query]
    (search-prompts this query {}))

  (search-prompts [_ query {:keys [limit] :or {limit 20}}]
    (let [entries (load-org-file file-path)
          query-lower (str/lower-case query)
          matches (filter (fn [e]
                            (or (str/includes? (str/lower-case (:prompt e "")) query-lower)
                                (str/includes? (str/lower-case (:accomplishes e "")) query-lower)))
                          entries)]
      {:count (count matches)
       :query query
       :entries (vec (take limit matches))}))

  (update-prompt! [_ id updates]
    (let [entries (load-org-file file-path)
          updated-entries (mapv (fn [e]
                                  (if (= (:id e) id)
                                    (merge e updates {:updated (now-timestamp)})
                                    e))
                                entries)]
      (save-org-file! file-path updated-entries)
      (first (filter #(= (:id %) id) updated-entries))))

  (get-statistics [_]
    (let [entries (load-org-file file-path)]
      {:total (count entries)
       :by-category (frequencies (map :category entries))
       :by-quality (frequencies (map :quality entries))
       :by-source (frequencies (map :source entries))
       :recent (take 5 (reverse (sort-by :created entries)))})))

;;; ============================================================
;;; Factory Functions
;;; ============================================================

(def default-prompts-file
  "Default location for prompts org file."
  (str (System/getProperty "user.home") "/.emacs.d/hive-mcp/prompts.org"))

(defn create-org-store
  "Create an OrgPromptStore with optional custom file path."
  ([]
   (create-org-store default-prompts-file))
  ([file-path]
   (->OrgPromptStore file-path)))
