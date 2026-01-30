(ns hive-mcp.agent.drone.kg-context
  "KG-First Context - Consult Knowledge Graph before file reads.

   Drones should check KG for existing knowledge before reading files.
   This reduces redundant file I/O and leverages cached knowledge.

   Flow:
   1. Call kg-first-context on file paths
   2. For :kg-known files → inject KG summary (skip file read)
   3. For :needs-read/:stale → include in files-to-read list

   CLARITY-A: Architectural performance via KG-first lookup."
  (:require [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; KG Summary Builder
;;; ============================================================

(defn- format-disc-info
  "Format disc entity info for injection."
  [disc-info file-path]
  (let [{:keys [disc staleness-score read-count last-read-at]} disc-info
        file-name (last (str/split file-path #"/"))]
    (str "- **" file-name "** (KG-known, staleness: "
         (format "%.1f" (float (or staleness-score 0)))
         ", reads: " (or read-count 0)
         (when-let [analyzed (:disc/analyzed-at disc)]
           (str ", analyzed: " analyzed))
         ")")))

(defn- get-related-knowledge
  "Query for knowledge entries related to a file path.
   Returns memory entries that reference this file as source."
  [file-path]
  (try
    (when (chroma/embedding-configured?)
      (->> (chroma/query-entries :limit 20)
           (filter #(= file-path (get-in % [:metadata :source-file])))
           (take 5)
           vec))
    (catch Exception e
      (log/debug "Could not query related knowledge:" (.getMessage e))
      [])))

(defn- get-kg-edges
  "Get KG edges where this file's entities are involved."
  [file-path]
  (try
    ;; Query edges where source or target mentions this file
    (let [file-name (last (str/split file-path #"/"))
          ;; Use edges/query-by-source if available, else return empty
          source-edges (when (resolve 'hive-mcp.knowledge-graph.edges/query-by-source)
                         ((resolve 'hive-mcp.knowledge-graph.edges/query-by-source)
                          {:source-pattern file-name
                           :limit 5}))]
      (vec (or source-edges [])))
    (catch Exception e
      (log/debug "Could not query KG edges:" (.getMessage e))
      [])))

(defn build-kg-summary
  "Build a summary string from KG knowledge for known files.

   Arguments:
     kg-known - Map of {path -> disc-info} from kg-first-context

   Returns:
     Formatted string summarizing KG knowledge for each file."
  [kg-known]
  (when (seq kg-known)
    (let [entries (for [[path info] kg-known]
                    (let [disc-str (format-disc-info info path)
                          related (get-related-knowledge path)
                          edges (get-kg-edges path)
                          knowledge-str (when (seq related)
                                          (str "\n  Knowledge entries:\n"
                                               (str/join "\n"
                                                         (map (fn [e]
                                                                (str "    - [" (or (:type e) "note") "] "
                                                                     (subs (str (:content e))
                                                                           0 (min 80 (count (str (:content e)))))))
                                                              related))))
                          edges-str (when (seq edges)
                                      (str "\n  KG relationships:\n"
                                           (str/join "\n"
                                                     (map (fn [e]
                                                            (str "    - " (:source e) " --["
                                                                 (:relation e) "]--> " (:target e)))
                                                          edges))))]
                      (str disc-str knowledge-str edges-str)))]
      (str "## KG-Known Files (cached knowledge)\n"
           "These files have fresh KG data - content may be summarized.\n\n"
           (str/join "\n\n" entries)
           "\n"))))

;;; ============================================================
;;; Staleness Warning Builder
;;; ============================================================

(defn build-staleness-warnings
  "Build staleness warnings for stale files.

   Arguments:
     stale-paths - List of file paths that are stale

   Returns:
     {:warnings [warning-maps] :formatted string}"
  [stale-paths]
  (when (seq stale-paths)
    (let [warnings (disc/staleness-warnings stale-paths)
          formatted (disc/format-staleness-warnings warnings)]
      {:warnings warnings
       :formatted (or formatted "")})))

;;; ============================================================
;;; Main Integration: KG-First File Context
;;; ============================================================

(defn- read-file-content
  "Read file content with path validation.
   Returns {:content string} or {:error string}."
  [path project-root]
  (try
    (let [validation (sandbox/validate-path-containment path project-root)]
      (if (:valid? validation)
        {:content (slurp (:canonical-path validation))
         :path (:canonical-path validation)}
        {:error (:error validation)
         :path path}))
    (catch Exception e
      {:error (.getMessage e)
       :path path})))

(defn- format-file-content-block
  "Format a single file's content as a markdown block."
  [path content]
  (str "### " (last (str/split path #"/")) "\n```\n" content "```\n"))

(defn- format-kg-known-file
  "Format a KG-known file's summary instead of full content.

   For fresh KG entries, we inject:
   - Disc metadata (staleness, read count, last analyzed)
   - Related knowledge entries
   - Note that full content can be read if needed"
  [path kg-info]
  (let [file-name (last (str/split path #"/"))
        {:keys [disc staleness-score read-count]} kg-info]
    (str "### " file-name " (KG-cached)\n"
         "**Note:** This file has fresh KG data (staleness: "
         (format "%.1f" (float (or staleness-score 0)))
         "). Full content skipped to save tokens.\n"
         "If you need exact content, request a file read.\n\n"
         "**KG metadata:**\n"
         "- Read count: " (or read-count 0) "\n"
         (when-let [analyzed (:disc/analyzed-at disc)]
           (str "- Last analyzed: " analyzed "\n"))
         (when-let [hash (:disc/content-hash disc)]
           (str "- Content hash: " (subs hash 0 (min 16 (count hash))) "...\n"))
         "\n")))

(defn format-files-with-kg-context
  "Format file contents using KG-first approach.

   1. Consult kg-first-context to classify files
   2. For :kg-known files → inject KG summary (skip file read)
   3. For :needs-read/:stale → read file content

   Arguments:
     files - List of file paths
     opts  - Map with:
       :project-root - Project root for path validation

   Returns:
     {:context  - Formatted context string
      :files-read - List of files that were actually read from disk
      :kg-skipped - List of files where KG summary was used
      :warnings   - Staleness warnings for stale files}"
  [files & [{:keys [project-root]}]]
  (let [effective-root (or project-root "")
        ;; Classify files via KG-first-context
        {:keys [kg-known needs-read stale]} (disc/kg-first-context files)

        ;; Files that need reading: needs-read + stale
        files-to-read (concat needs-read stale)

        ;; Build staleness warnings for stale files
        {:keys [warnings formatted] :as stale-info} (build-staleness-warnings stale)

        ;; Read files that need reading
        read-results (for [f files-to-read]
                       (merge {:path f} (read-file-content f effective-root)))

        ;; Format KG-known files (summaries)
        kg-summaries (for [[path info] kg-known]
                       (format-kg-known-file path info))

        ;; Format read files (full content)
        read-contents (for [{:keys [path content error]} read-results]
                        (if error
                          (str "### " (last (str/split path #"/")) "\n(ERROR: " error ")\n")
                          (format-file-content-block path content)))

        ;; Combine all context
        full-context (str
                      ;; Staleness warnings first (high priority)
                      (when (seq formatted)
                        (str formatted "\n"))

                      ;; KG-known file summaries
                      (when (seq kg-summaries)
                        (str "## Files with KG Knowledge (content cached)\n"
                             (str/join "\n" kg-summaries)
                             "\n"))

                      ;; File contents for files that were read
                      (when (seq read-contents)
                        (str "## Current File Contents\n"
                             "IMPORTANT: Use this EXACT content as old_content in propose_diff.\n"
                             "Do NOT guess or assume file content - use what is provided below.\n\n"
                             (str/join "\n" read-contents))))]

    {:context full-context
     :files-read (vec (keep #(when-not (:error %) (:path %)) read-results))
     :kg-skipped (vec (keys kg-known))
     :warnings (or warnings [])
     :summary {:total (count files)
               :kg-known (count kg-known)
               :needs-read (count needs-read)
               :stale (count stale)
               :actually-read (count files-to-read)}}))

;;; ============================================================
;;; Integration Hook for drone.clj
;;; ============================================================

(defn augment-task-with-kg
  "Augment a drone task using KG-first file context.

   This is the main integration point for drone.clj.
   Replaces direct file reading with KG-aware context building.

   Arguments:
     task         - Task description
     files        - List of files to include
     project-root - Project root for path resolution

   Returns:
     {:augmented-task - Task string with context
      :files-read     - Files actually read from disk
      :kg-skipped     - Files where KG summary was used}"
  [task files & [{:keys [project-root]}]]
  (let [{:keys [context files-read kg-skipped summary]}
        (format-files-with-kg-context files {:project-root project-root})]

    ;; Log KG-first efficiency
    (when (seq kg-skipped)
      (log/info "KG-first context saved file reads"
                {:kg-skipped (count kg-skipped)
                 :files-read (count files-read)
                 :summary summary}))

    {:augmented-task (str "## Task\n" task
                          (when (seq files)
                            (str "\n\n## Files to modify\n"
                                 (str/join "\n" (map #(str "- " %) files))))
                          "\n\n" context)
     :files-read files-read
     :kg-skipped kg-skipped
     :summary summary}))
