(ns hive-mcp.workflows.catchup-ling
  "Lightweight ling-specific catchup using KG + memory reconstruction.

   Produces a compact ~1-3K token context blob for ling spawn injection,
   replacing the heavy coordinator /catchup (~10K+ tokens).

   Pipeline:
   1. Resolve project scope from directory
   2. Query axioms + priority conventions from Chroma
   3. Cache entries in context-store (ephemeral, 5min TTL)
   4. Extract KG seed nodes from kanban task
   5. Reconstruct compressed context via KG pipeline
   6. Append minimal git status + kanban task context

   Key difference from coordinator catchup:
   - NO maintenance (permeation, tree scan, disc decay)
   - NO piggyback enqueuing
   - NO full KG enrichment of all entries
   - ONLY fetches axioms + priority conventions (not all categories)
   - Uses KG reconstruction for ~750 token output

   SOLID-S: Ling context generation only, no coordinator concerns.
   CLARITY-Y: Graceful degradation — returns nil on any failure."
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.catchup.scope :as catchup-scope]
            [hive-mcp.tools.catchup.git :as catchup-git]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.context.reconstruction :as reconstruction]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const ling-context-ttl-ms
  "Context-store TTL for ling catchup entries (5 min).
   Short-lived because ling sessions are ephemeral."
  300000)

(def ^:const max-conventions
  "Maximum priority conventions to include in ling context."
  15)

;; =============================================================================
;; Step 1: Targeted Memory Queries
;; =============================================================================

(defn fetch-ling-memory
  "Fetch only what a ling needs: axioms + priority conventions.

   Unlike coordinator catchup which queries ALL categories
   (axioms, decisions, conventions, sessions, snippets, expiring),
   this fetches only high-signal entries.

   Arguments:
     project-id - Project scope string

   Returns:
     {:axioms [...] :conventions [...]}"
  [project-id]
  (try
    (let [axioms (catchup-scope/query-axioms project-id)
          conventions (catchup-scope/query-scoped-entries
                       "convention" ["catchup-priority"]
                       project-id max-conventions)]
      {:axioms (or axioms [])
       :conventions (or conventions [])})
    (catch Exception e
      (log/debug "fetch-ling-memory failed:" (.getMessage e))
      {:axioms [] :conventions []})))

;; =============================================================================
;; Step 2: Context-Store Caching
;; =============================================================================

(defn- cache-ling-entries!
  "Cache ling memory entries in context-store for reconstruction pipeline.

   The reconstruction pipeline reads from context-store via ctx-refs,
   so we cache entries there with short TTL.

   Arguments:
     memory     - Map from fetch-ling-memory {:axioms [...] :conventions [...]}
     project-id - Project scope string

   Returns:
     ctx-refs map {:axioms ctx-id, :conventions ctx-id} or empty map on failure."
  [{:keys [axioms conventions]} project-id]
  (try
    (let [project-tag (or project-id "global")]
      (cond-> {}
        (seq axioms)
        (assoc :axioms
               (context-store/context-put!
                axioms
                :tags #{"ling-catchup" "axioms" project-tag}
                :ttl-ms ling-context-ttl-ms))
        (seq conventions)
        (assoc :conventions
               (context-store/context-put!
                conventions
                :tags #{"ling-catchup" "conventions" project-tag}
                :ttl-ms ling-context-ttl-ms))))
    (catch Exception e
      (log/debug "cache-ling-entries! failed:" (.getMessage e))
      {})))

;; =============================================================================
;; Step 3: KG Seed Extraction
;; =============================================================================

(defn extract-kg-seeds
  "Extract KG seed node IDs from kanban task for targeted traversal.

   Uses hints/generate-task-hints which traverses KG from the task node
   and returns discovered node IDs (L1 IDs) as seeds for the
   reconstruction pipeline's bounded KG traversal.

   Arguments:
     kanban-task-id - Kanban task memory ID

   Returns:
     Vector of KG node IDs, or empty vector on failure.
     Returns nil when no kanban-task-id provided."
  [kanban-task-id]
  (when kanban-task-id
    (try
      (let [task-hints (hints/generate-task-hints
                        {:task-id kanban-task-id :depth 2})]
        (or (:l1-ids task-hints) []))
      (catch Exception e
        (log/debug "extract-kg-seeds failed:" (.getMessage e))
        []))))

;; =============================================================================
;; Step 4: Git Status (minimal)
;; =============================================================================

(defn- render-git-section
  "Render minimal git status section.

   Arguments:
     directory - Working directory

   Returns:
     Git status markdown string, or nil on failure."
  [directory]
  (try
    (when-let [git-info (catchup-git/gather-git-info directory)]
      (str "\n### Git Status\n"
           "- **Branch**: " (or (:branch git-info) "unknown") "\n"
           (when (:uncommitted git-info)
             "- **Uncommitted changes**: yes\n")
           "- **Last commit**: " (or (:last-commit git-info) "unknown") "\n"))
    (catch Exception e
      (log/debug "render-git-section failed:" (.getMessage e))
      nil)))

;; =============================================================================
;; Step 5: Kanban Task Context
;; =============================================================================

(defn- render-task-section
  "Render kanban task context section with preview and tags.

   Arguments:
     kanban-task-id - Kanban task memory ID

   Returns:
     Task context markdown string, or nil when not available."
  [kanban-task-id]
  (when kanban-task-id
    (try
      (when-let [entry (chroma/get-entry-by-id kanban-task-id)]
        (let [content (or (:content entry) "")
              preview (subs content 0 (min (count content) 300))
              tags (->> (or (:tags entry) [])
                        (remove #(or (str/starts-with? % "scope:")
                                     (str/starts-with? % "agent:")))
                        vec)]
          (str "\n### Assigned Task\n"
               preview
               (when (> (count content) 300) "...")
               "\n"
               (when (seq tags)
                 (str "**Tags**: " (str/join ", " tags) "\n")))))
      (catch Exception e
        (log/debug "render-task-section failed:" (.getMessage e))
        nil))))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn ling-catchup
  "Produce compact context blob for ling spawn injection.

   Replaces the heavy coordinator /catchup with a targeted, compressed
   context reconstruction (~1-3K tokens vs ~10K+).

   Arguments:
     opts - Map with:
            :directory      - Working directory for project scoping (required)
            :task           - Task description string (optional, unused currently)
            :kanban-task-id - Kanban task memory ID (optional, enables context targeting)

   Returns:
     Compact context markdown string (~1-3K tokens), or nil on failure.

   Pipeline:
     1. Check prerequisites (Chroma configured)
     2. Resolve project scope
     3. Fetch axioms + priority conventions
     4. Cache in context-store for reconstruction pipeline
     5. Extract context seeds from kanban task (if provided)
     6. Run compressed reconstruction
     7. Append git status + task context

   CLARITY-Y: Never throws — returns nil on failure."
  [{:keys [directory kanban-task-id] :as _opts}]
  (when (chroma/embedding-configured?)
    (try
      (let [project-id (scope/get-current-project-id directory)

            ;; Step 1: Fetch targeted memory
            memory (fetch-ling-memory project-id)

            ;; Step 2: Cache in context-store
            ctx-refs (cache-ling-entries! memory project-id)

            ;; Step 3: context seeds from kanban task
            kg-seeds (or (extract-kg-seeds kanban-task-id) [])

            ;; Step 4: Reconstruct compressed context
            reconstructed (reconstruction/reconstruct-context
                           ctx-refs kg-seeds project-id)

            ;; Step 5: Append supplementary sections
            git-section (render-git-section directory)
            task-section (render-task-section kanban-task-id)]

        (log/info "ling-catchup: produced context"
                  {:project-id project-id
                   :axioms (count (:axioms memory))
                   :conventions (count (:conventions memory))
                   :kg-seeds (count kg-seeds)
                   :chars (count (or reconstructed ""))})

        (str (or reconstructed "")
             (or git-section "")
             (or task-section "")))

      (catch Exception e
        (log/warn "ling-catchup failed (graceful degradation):" (.getMessage e))
        nil))))
