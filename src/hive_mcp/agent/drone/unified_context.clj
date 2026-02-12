(ns hive-mcp.agent.drone.unified-context
  "Unified context gathering for drone task augmentation.

   Delegates to extension if available. Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry.
   When no extensions are registered, all functions gracefully degrade
   to empty/noop results."
  (:require [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Constants (formatting)
;; =============================================================================

(def ^:const max-content-chars
  "Maximum characters per content entry to avoid token bloat."
  500)

;; =============================================================================
;; Empty Return Shapes (noop fallback)
;; =============================================================================

(def ^:private empty-context
  {:conventions []
   :decisions []
   :snippets []
   :domain []
   :edges []
   :seed-count 0
   :traversal-count 0
   :node-ids #{}})

;; =============================================================================
;; Public API — delegates to extension or returns noop
;; =============================================================================

(defn resolve-seeds
  "Resolve seed node IDs from input parameters.
   Delegates to extension if available."
  [opts]
  (delegate-or-noop :uc/resolve-seeds [] [opts]))

(defn prepare-drone-context
  "Gather context for drone task augmentation.
   Delegates to extension if available."
  [opts]
  (delegate-or-noop :uc/gather empty-context [opts]))

(defn unified-context-available?
  "Check if context gathering is available.
   Delegates to extension if available."
  []
  (delegate-or-noop :uc/available false []))

;; =============================================================================
;; Pure Utilities (no IP — data formatting only)
;; =============================================================================

(defn- truncate-content
  "Truncate content to max-content-chars to prevent token bloat."
  [content]
  (if (and (string? content) (> (count content) max-content-chars))
    (str (subs content 0 max-content-chars) "...")
    content))

(defn- classify-entries
  "Classify fetched entries by memory type into structured map."
  [entries]
  (reduce
   (fn [acc entry]
     (let [entry-type (or (:type entry) "note")
           truncated  (update entry :content truncate-content)
           category   (case entry-type
                        "convention" :conventions
                        "decision"   :decisions
                        "snippet"    :snippets
                        :domain)]
       (update acc category (fnil conj []) truncated)))
   {:conventions [] :decisions [] :snippets [] :domain []}
   entries))

(defn- enrich-context
  "Extension point for context enrichment.
   Delegates to extension if available."
  [context opts]
  (if-let [ext-fn (ext/get-extension :uc/enrich)]
    (try
      (ext-fn context opts)
      (catch Exception e
        (log/debug "Extension :uc/enrich failed, returning unenriched:" (.getMessage e))
        context))
    context))

;; =============================================================================
;; Formatting (pure — no IP)
;; =============================================================================

(defn- format-entries-section
  "Format a category of entries as markdown section."
  [title entries]
  (when (seq entries)
    (str "### " title "\n"
         (str/join "\n---\n"
                   (map-indexed
                    (fn [i {:keys [content]}]
                      (str (inc i) ". " (or content "(no content)")))
                    entries))
         "\n\n")))

(defn- format-edges-section
  "Format KG edges as compact arrow notation."
  [edges]
  (when (seq edges)
    (let [arrow (fn [{:keys [relation]}]
                  (case relation
                    :implements   "-impl->"
                    :supersedes   "-super->"
                    :depends-on   "-dep->"
                    :refines      "-ref->"
                    :contradicts  "-contra->"
                    :derived-from "-from->"
                    :applies-to   "-apply->"
                    (str "-" (name (or relation "?")) "->")))
          truncate-id (fn [id]
                        (if (and (string? id) (> (count id) 8))
                          (subs id 0 8)
                          (str id)))]
      (str "### KG Structure (" (count edges) " edges)\n"
           (str/join "\n"
                     (map (fn [e]
                            (str "  " (truncate-id (:from e))
                                 " " (arrow e) " "
                                 (truncate-id (:to e))))
                          edges))
           "\n\n"))))

(defn format-unified-context
  "Format context as markdown string for prompt injection.
   Delegates to extension if available."
  [context]
  (when (and context
             (or (seq (:conventions context))
                 (seq (:decisions context))
                 (seq (:domain context))
                 (seq (:snippets context))))
    (str "## Unified Project Context (KG-traversed)\n"
         (format-entries-section "Conventions" (:conventions context))
         (format-entries-section "Decisions" (:decisions context))
         (format-entries-section "Domain Knowledge" (:domain context))
         (format-entries-section "Code Snippets" (:snippets context))
         (format-edges-section (:edges context)))))
