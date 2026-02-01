(ns hive-mcp.tools.memory.migration
  "Migration handlers for memory project and storage transitions.

   SOLID: SRP - Single responsibility for data migration.
   CLARITY: Y - Yield safe failure with careful migration handling.

   Handlers:
   - migrate-project: Move entries between project IDs
   - import-json: Import from legacy JSON storage to Chroma
   - detect-orphaned-scopes: Find hash-based scope tags that need migration
   - migrate-scope: Migrate entries from old hash scope to new name scope

   Orphaned Scope Problem:
   Project-id derivation changed from hash-based to name-based:
   - Old: scope:project:d987697ae05f40b1 (hash)
   - New: scope:project:funeraria (name)

   Old entries exist but queries don't find them because scope tags don't match."
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
      (mcp-json {:migrated @migrated
                 :updated-scopes @updated-scopes
                 :old-project-id old-project-id
                 :new-project-id new-project-id}))))

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
        (mcp-json {:error (str "Failed to read JSON: " error)})
        (let [data (json/read-str result :key-fn keyword)
              all-entries (concat (:notes data) (:snippets data)
                                  (:conventions data) (:decisions data))]
          (if dry-run
            (mcp-json {:dry-run true
                       :would-import (count all-entries)
                       :by-type {:notes (count (:notes data))
                                 :snippets (count (:snippets data))
                                 :conventions (count (:conventions data))
                                 :decisions (count (:decisions data))}})
            (let [results (map #(import-entry! % pid) all-entries)
                  imported (count (filter identity results))
                  skipped (count (remove identity results))]
              (mcp-json {:imported imported
                         :skipped skipped
                         :project-id pid}))))))))

;; ============================================================
;; Orphaned Scope Detection and Migration
;; ============================================================

(defn hash-scope?
  "Detect if a scope looks like a hash (orphaned old-style scope).

   Hash-based scopes are hex strings (12+ chars, all hex digits).
   Name-based scopes are readable project names with letters/hyphens/underscores.

   Examples:
     (hash-scope? \"d987697ae05f40b1\") => true
     (hash-scope? \"funeraria\")        => false
     (hash-scope? \"hive-mcp\")         => false
     (hash-scope? \"abc\")              => false (too short)"
  [scope-id]
  (and (string? scope-id)
       (> (count scope-id) 12)
       (boolean (re-matches #"^[a-f0-9]+$" scope-id))))

(defn- extract-scope-id
  "Extract the scope ID from a scope tag.

   Examples:
     (extract-scope-id \"scope:project:abc123\") => \"abc123\"
     (extract-scope-id \"scope:global\")         => nil
     (extract-scope-id \"other-tag\")            => nil"
  [tag]
  (when (and (string? tag) (str/starts-with? tag "scope:project:"))
    (subs tag (count "scope:project:"))))

(defn- orphaned-scope-tag?
  "Check if a tag is an orphaned hash-based scope tag."
  [tag]
  (when-let [scope-id (extract-scope-id tag)]
    (hash-scope? scope-id)))

(defn handle-detect-orphaned
  "Detect orphaned hash-based scope tags in memory.

   Scans all memory entries and identifies hash-based scope tags
   that may be orphaned from the old project-id derivation.

   Returns:
     {:orphaned-scopes [\"d987697ae05f40b1\" ...]
      :count N
      :entries-by-scope {\"d987697ae05f40b1\" 5, ...}}"
  [_args]
  (with-chroma
    (let [;; Query a large sample of entries
          entries (chroma/query-entries :limit 5000 :include-expired? true)
          ;; Extract all scope tags and find hash-based ones
          scope-entries (->> entries
                             (mapcat (fn [entry]
                                       (->> (:tags entry)
                                            (filter orphaned-scope-tag?)
                                            (map (fn [tag]
                                                   {:scope-id (extract-scope-id tag)
                                                    :entry-id (:id entry)})))))
                             (group-by :scope-id))
          orphaned-scopes (keys scope-entries)
          entries-by-scope (into {} (map (fn [[k v]] [k (count v)]) scope-entries))]
      (log/info "Detected" (count orphaned-scopes) "orphaned hash-based scopes")
      (mcp-json {:orphaned-scopes (vec orphaned-scopes)
                 :count (count orphaned-scopes)
                 :entries-by-scope entries-by-scope}))))

(defn- update-scope-tag
  "Replace old scope tag with new scope tag in a tags vector.

   Example:
     (update-scope-tag [\"kanban\" \"scope:project:abc123\"] \"abc123\" \"funeraria\")
     => [\"kanban\" \"scope:project:funeraria\"]"
  [tags old-scope new-scope]
  (let [old-tag (str "scope:project:" old-scope)
        new-tag (str "scope:project:" new-scope)]
    (mapv #(if (= % old-tag) new-tag %) tags)))

(defn handle-migrate-scope
  "Migrate entries from old hash-based scope to new name-based scope.

   Args (from MCP):
     :old_scope - The old hash-based scope to migrate from (e.g., 'd987697ae05f40b1')
     :new_scope - The new name-based scope to migrate to (e.g., 'funeraria')
     :dry_run   - If true, preview changes without modifying (default: true)

   Returns:
     {:migrated N
      :entries [ids...]
      :dry-run boolean
      :old-scope \"...\"
      :new-scope \"...\"}"
  [{:keys [old_scope new_scope dry_run]}]
  (cond
    (str/blank? old_scope)
    (mcp-error "old_scope is required")

    (str/blank? new_scope)
    (mcp-error "new_scope is required")

    (= old_scope new_scope)
    (mcp-error "old_scope and new_scope must be different")

    :else
    (let [dry-run (if (nil? dry_run) true dry_run)]
      (with-chroma
        (let [old-tag (str "scope:project:" old_scope)
              ;; Find all entries with the old scope tag
              entries (chroma/query-entries :limit 5000 :include-expired? true)
              matching (->> entries
                            (filter #(some #{old-tag} (:tags %)))
                            vec)
              entry-ids (mapv :id matching)]
          (log/info "migrate-scope:" (count matching) "entries from" old_scope "to" new_scope
                    (if dry-run "(dry-run)" ""))

          (when-not dry-run
            ;; Actually perform the migration
            (doseq [entry matching]
              (let [new-tags (update-scope-tag (:tags entry) old_scope new_scope)]
                (chroma/update-entry! (:id entry) {:tags new-tags})
                (log/debug "Migrated entry" (:id entry) "tags:" (:tags entry) "->" new-tags))))

          (mcp-json {:migrated (count matching)
                     :entries entry-ids
                     :dry-run (boolean dry-run)
                     :old-scope old_scope
                     :new-scope new_scope}))))))

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  [{:name "mcp_memory_migrate_project"
    :description "Migrate memory entries from one project-id to another. Updates project-id metadata and optionally scope tags."
    :inputSchema {:type "object"
                  :properties {:old-project-id {:type "string"
                                                :description "Current project-id to migrate from"}
                               :new-project-id {:type "string"
                                                :description "New project-id to migrate to"}
                               :update-scopes {:type "boolean"
                                               :description "Also update scope tags (default: false)"
                                               :default false}}
                  :required ["old-project-id" "new-project-id"]}
    :handler handle-migrate-project}

   {:name "mcp_memory_import_json"
    :description "Import memory entries from legacy JSON storage to Chroma. Use dry-run to preview."
    :inputSchema {:type "object"
                  :properties {:project-id {:type "string"
                                            :description "Project ID for imported entries"}
                               :dry-run {:type "boolean"
                                         :description "Preview without importing (default: false)"
                                         :default false}}}
    :handler handle-import-json}

   {:name "mcp_memory_detect_orphaned"
    :description "Detect orphaned hash-based scope tags in memory. Returns list of hash-based scopes that may need migration to name-based scopes. Use before migrate_scope."
    :inputSchema {:type "object"
                  :properties {}}
    :handler handle-detect-orphaned}

   {:name "mcp_memory_migrate_scope"
    :description "Migrate memory entries from old hash-based scope to new name-based scope. Use detect_orphaned first to find orphaned scopes. IMPORTANT: Use dry_run=true first to preview changes."
    :inputSchema {:type "object"
                  :properties {:old_scope {:type "string"
                                           :description "Old hash-based scope ID to migrate FROM (e.g., 'd987697ae05f40b1')"}
                               :new_scope {:type "string"
                                           :description "New name-based scope ID to migrate TO (e.g., 'funeraria')"}
                               :dry_run {:type "boolean"
                                         :description "Preview changes without modifying (default: true)"
                                         :default true}}
                  :required ["old_scope" "new_scope"]}
    :handler handle-migrate-scope}])
