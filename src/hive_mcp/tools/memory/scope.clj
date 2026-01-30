(ns hive-mcp.tools.memory.scope
  "Project scope utilities for memory operations.

   SOLID: SRP - Single responsibility for scope management.
   CLARITY: R - Represented intent with clear scope semantics.

   Handles:
   - Project ID detection from Emacs
   - Scope tag injection for memory entries
   - Scope matching for filtering queries

   NOTE: For hierarchical scope resolution (visible-scopes, get-parent-scope),
   see hive-mcp.knowledge-graph.scope which provides full Knowledge Graph
   scope hierarchy support."
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Project ID Detection
;; ============================================================

(defn get-current-project-id
  "Get current project ID from directory path or Emacs context.
   When directory is provided, derives project-id from path (no Emacs call).
   When directory is nil, falls back to Emacs current buffer context."
  ([]
   (get-current-project-id nil))
  ([directory]
   (if directory
     ;; Derive from directory path directly (no emacsclient roundtrip)
     (let [parts (str/split directory #"/")
           project-name (last parts)]
       (if (and project-name (not (str/blank? project-name)))
         project-name
         "global"))
     ;; Fallback to Emacs only when no directory provided
     (try
       (let [elisp "(hive-mcp-memory--project-id)"
             {:keys [success result]} (ec/eval-elisp elisp)]
         (if (and success result (not= result "nil"))
           (str/replace result #"\"" "")
           "global"))
       (catch Exception _
         "global")))))

;; ============================================================
;; Scope Tag Management
;; ============================================================

(defn inject-project-scope
  "Add project scope tag if not already present.
   Returns tags vector with scope:project:<id> or scope:global added."
  [tags project-id]
  (let [has-scope? (some #(str/starts-with? % "scope:") tags)]
    (if has-scope?
      tags
      (if (= project-id "global")
        (conj (vec tags) "scope:global")
        (conj (vec tags) (str "scope:project:" project-id))))))

(defn make-scope-tag
  "Create a scope tag for a project-id.
   Returns 'scope:global' for 'global' project-id,
   otherwise 'scope:project:<project-id>'."
  [project-id]
  (if (= project-id "global")
    "scope:global"
    (str "scope:project:" project-id)))

;; ============================================================
;; Scope Filtering
;; ============================================================

(defn matches-scope?
  "Check if entry matches the given scope filter.

   Filter behavior:
   - nil or 'all': match everything
   - 'global': only match scope:global entries
   - specific scope tag: match that scope OR global entries"
  [entry scope-filter]
  (let [tags (or (:tags entry) [])]
    (cond
      ;; No filter or "all" - match everything
      (or (nil? scope-filter) (= scope-filter "all"))
      true

      ;; "global" - only global scope
      (= scope-filter "global")
      (some #(= % "scope:global") tags)

      ;; Specific scope tag - match scope or global
      :else
      (or (some #(= % scope-filter) tags)
          (some #(= % "scope:global") tags)))))

(defn derive-scope-filter
  "Derive scope filter from scope parameter and project-id.

   Returns:
   - nil if scope is 'all' (no filtering)
   - the scope value if explicitly provided
   - 'scope:project:<project-id>' if scope is nil (auto mode)"
  [scope project-id]
  (cond
    (= scope "all") nil
    (some? scope) scope
    :else (make-scope-tag project-id)))

;; ============================================================
;; Hierarchical Scope Support
;; ============================================================

(defn- _extract-project-hierarchy
  "Extract project hierarchy from a scope tag.
   'scope:project:funeraria:sisf-sync:sisf-caixa-fe' -> ['funeraria' 'sisf-sync' 'sisf-caixa-fe']"
  [scope-tag]
  (when (and scope-tag (str/starts-with? scope-tag "scope:project:"))
    (-> scope-tag
        (str/replace #"^scope:project:" "")
        (str/split #":"))))

(defn- _build-ancestor-scopes
  "Build all ancestor scope tags from a project hierarchy.
   ['funeraria' 'sisf-sync' 'sisf-caixa-fe'] ->
   ['scope:project:funeraria'
    'scope:project:funeraria:sisf-sync'
    'scope:project:funeraria:sisf-sync:sisf-caixa-fe']"
  [hierarchy]
  (when (seq hierarchy)
    (loop [parts hierarchy
           acc []
           path []]
      (if (empty? parts)
        acc
        (let [new-path (conj path (first parts))
              scope-tag (str "scope:project:" (str/join ":" new-path))]
          (recur (rest parts) (conj acc scope-tag) new-path))))))

(defn derive-hierarchy-scope-filter
  "Derive hierarchical scope filter that includes ancestors.

   Returns a set of valid scope tags:
   - nil if scope is 'all' (no filtering)
   - set including scope + ancestors + global if hierarchical
   - single scope set if not hierarchical

   NOTE: This now delegates to hive-mcp.knowledge-graph.scope for full
   hierarchical support including explicit parent-id from .hive-project.edn.
   Falls back to string-based inference for backward compatibility."
  [scope]
  (cond
    (= scope "all") nil
    (nil? scope) nil  ;; Let caller handle nil -> auto mode
    (= scope "global") #{"scope:global"}
    :else
    ;; Delegate to knowledge-graph.scope for hierarchical resolution
    ;; This supports both explicit parent-id and string-based inference
    (kg-scope/visible-scope-tags scope)))

(defn matches-hierarchy-scopes?
  "Check if entry matches any of the hierarchical scope filters.

   Entry matches if it has:
   - scope:global tag (always matches)
   - Any scope tag in the valid-scopes set
   - An ancestor scope (entries at parent level are visible to children)"
  [entry valid-scopes]
  (if (nil? valid-scopes)
    true  ;; No filter = match all
    (let [tags (set (or (:tags entry) []))]
      (some tags valid-scopes))))
