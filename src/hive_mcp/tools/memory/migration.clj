(ns hive-mcp.tools.memory.migration
  "Migration handlers for memory project and storage transitions.

   SOLID: SRP - Single responsibility for data migration.
   CLARITY: Y - Yield safe failure with careful migration handling.

   Handlers:
   - migrate-project: Move entries between project IDs
   - import-json: Import from legacy JSON storage to Chroma"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; ============================================================
;; Project Migration Handler
;; ============================================================

(defn handle-migrate-project
  "Migrate memory from one project-id to another (Chroma-only).
   Updates project-id and optionally scope tags for all matching entries."
  [{:keys [old-project-id new-project-id update-scopes]}]
  (log/info "mcp-memory-migrate-project:" old-project-id "->" new-project-id)
  (with-chroma
    (let [entries (chroma/query-entries :project-id old-project-id :limit 10000)
          migrated (atom 0)
          updated-scopes (atom 0)
          old-scope-tag (scope/make-scope-tag old-project-id)
          new-scope-tag (scope/make-scope-tag new-project-id)]
      (doseq [entry entries]
        (let [new-tags (if update-scopes
                         (mapv (fn [tag]
                                 (if (= tag old-scope-tag)
                                   (do (swap! updated-scopes inc)
                                       new-scope-tag)
                                   tag))
                               (:tags entry))
                         (:tags entry))]
          (chroma/update-entry! (:id entry) {:project-id new-project-id
                                             :tags new-tags})
          (swap! migrated inc)))
      {:type "text" :text (json/write-str {:migrated @migrated
                                           :updated-scopes @updated-scopes
                                           :old-project-id old-project-id
                                           :new-project-id new-project-id})})))

;; ============================================================
;; JSON Import Handler
;; ============================================================

(defn- import-entry!
  "Import a single entry to Chroma. Returns true if imported, false if skipped."
  [entry project-id]
  (if (chroma/get-entry-by-id (:id entry))
    false ; Already exists
    (do
      (chroma/index-memory-entry!
       {:id (:id entry)
        :type (:type entry)
        :content (:content entry)
        :tags (if (vector? (:tags entry))
                (vec (:tags entry))
                (:tags entry))
        :content-hash (or (:content-hash entry)
                          (chroma/content-hash (:content entry)))
        :created (:created entry)
        :updated (:updated entry)
        :duration (or (:duration entry) "long")
        :expires (or (:expires entry) "")
        :access-count (or (:access-count entry) 0)
        :helpful-count (or (:helpful-count entry) 0)
        :unhelpful-count (or (:unhelpful-count entry) 0)
        :project-id project-id})
      true)))

(defn handle-import-json
  "Import memory entries from JSON (for migrating from elisp JSON storage to Chroma).
   Reads existing JSON files from Emacs hive-mcp directory and imports to Chroma."
  [{:keys [project-id dry-run]}]
  (log/info "mcp-memory-import-json:" project-id "dry-run:" dry-run)
  (with-chroma
    ;; Get JSON data from elisp
    (let [pid (or project-id (scope/get-current-project-id))
          elisp (format "(json-encode (list :notes (hive-mcp-memory-query 'note nil %s 1000 nil t)
                                            :snippets (hive-mcp-memory-query 'snippet nil %s 1000 nil t)
                                            :conventions (hive-mcp-memory-query 'convention nil %s 1000 nil t)
                                            :decisions (hive-mcp-memory-query 'decision nil %s 1000 nil t)))"
                        (pr-str pid) (pr-str pid) (pr-str pid) (pr-str pid))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if-not success
        {:type "text" :text (json/write-str {:error (str "Failed to read JSON: " error)}) :isError true}
        (let [data (json/read-str result :key-fn keyword)
              all-entries (concat (:notes data) (:snippets data)
                                  (:conventions data) (:decisions data))]
          (if dry-run
            {:type "text" :text (json/write-str {:dry-run true
                                                 :would-import (count all-entries)
                                                 :by-type {:notes (count (:notes data))
                                                           :snippets (count (:snippets data))
                                                           :conventions (count (:conventions data))
                                                           :decisions (count (:decisions data))}})}
            (let [results (map #(import-entry! % pid) all-entries)
                  imported (count (filter identity results))
                  skipped (count (remove identity results))]
              {:type "text" :text (json/write-str {:imported imported
                                                   :skipped skipped
                                                   :project-id pid})})))))))
