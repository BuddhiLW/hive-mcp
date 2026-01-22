(ns hive-mcp.tools.seeds
  "MCP tool handlers for seed memory import.

   Seeds are curated markdown files with YAML frontmatter that bootstrap
   the knowledge base with foundational content (conventions, decisions,
   snippets).

   Tools:
     - seed_import: Import all seeds from seeds/ directory
     - seed_list:   List available seed files without importing

   SOLID: SRP - Single responsibility for seed tool handlers.
   CLARITY: L - Layers stay pure (delegates to seeds.import)."
  (:require [hive-mcp.seeds.import :as import]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.chroma :as chroma]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Chroma Availability Check
;; ============================================================

(defmacro with-chroma
  "Execute body only if Chroma is available, else return error.

   CLARITY: Y - Yield safe failure (graceful Chroma check)."
  [& body]
  `(if (chroma/chroma-available?)
     (do ~@body)
     (mcp-error "Chroma not available. Seed import requires vector database.")))

;; ============================================================
;; Handler Functions
;; ============================================================

(defn handle-seed-import
  "Import seed memories from seeds/ directory.

   Options:
     :dry-run   - Preview without importing (default: false)
     :directory - Base directory to search for seeds/ (default: cwd)

   Returns summary with imported/skipped/error counts."
  [{:keys [dry_run directory]}]
  (log/info "seed-import:" "dry-run:" dry_run "directory:" directory)
  (with-chroma
    (let [result (import/import-all-seeds!
                  :base-dir directory
                  :dry-run? (boolean dry_run)
                  :skip-duplicates? true)]
      (mcp-json
       {:imported (:imported result)
        :skipped (:skipped result)
        :would_import (:would-import result)
        :errors (:errors result)
        :dry_run (:dry-run? result)
        :details (mapv (fn [{:keys [status title type source reason id]}]
                         {:status (name status)
                          :title title
                          :type type
                          :source source
                          :id id
                          :reason reason})
                       (:details result))}))))

(defn handle-seed-list
  "List available seed files without importing.

   Options:
     :directory - Base directory to search for seeds/ (default: cwd)

   Returns list of seed files with parsed metadata."
  [{:keys [directory]}]
  (log/info "seed-list:" "directory:" directory)
  (let [seeds (import/list-seeds :base-dir directory)]
    (mcp-json
     {:count (count seeds)
      :seeds (mapv (fn [{:keys [path title type tags duration error]}]
                     (if error
                       {:path path :error error}
                       {:path path
                        :title title
                        :type type
                        :tags tags
                        :duration duration}))
                   seeds)})))

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  [{:name "seed_import"
    :description "Import seed memories from seeds/ directory. Run on fresh install to bootstrap knowledge. Seeds are markdown files with YAML frontmatter containing type, tags, and duration metadata."
    :inputSchema {:type "object"
                  :properties {"dry_run" {:type "boolean"
                                          :description "Preview what would be imported without actually importing (default: false)"}
                               "directory" {:type "string"
                                            :description "Base directory containing seeds/ folder (default: current working directory)"}}
                  :required []}
    :handler handle-seed-import}

   {:name "seed_list"
    :description "List available seed files without importing. Shows parsed metadata including title, type, tags, and duration for each seed file."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Base directory containing seeds/ folder (default: current working directory)"}}
                  :required []}
    :handler handle-seed-list}])
