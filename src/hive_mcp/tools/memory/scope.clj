(ns hive-mcp.tools.memory.scope
  "Project scope utilities for memory operations.

   SOLID: SRP - Single responsibility for scope management.
   CLARITY: R - Represented intent with clear scope semantics.

   Handles:
   - Project ID detection from Emacs
   - Scope tag injection for memory entries
   - Scope matching for filtering queries"
  (:require [hive-mcp.emacsclient :as ec]
            [clojure.string :as str]))

;; ============================================================
;; Project ID Detection
;; ============================================================

(defn get-current-project-id
  "Get current project ID from Emacs, or 'global' if not in a project.
   When directory is provided, uses that path to determine project context
   instead of relying on Emacs's current buffer."
  ([]
   (get-current-project-id nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--project-id %s)" (pr-str directory))
                   "(hive-mcp-memory--project-id)")
           {:keys [success result]} (ec/eval-elisp elisp)]
       (if (and success result (not= result "nil"))
         (str/replace result #"\"" "")
         "global"))
     (catch Exception _
       "global"))))

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

(defn- extract-project-hierarchy
  "Extract project hierarchy from a scope tag.
   'scope:project:funeraria:sisf-sync:sisf-caixa-fe' -> ['funeraria' 'sisf-sync' 'sisf-caixa-fe']"
  [scope-tag]
  (when (and scope-tag (str/starts-with? scope-tag "scope:project:"))
    (-> scope-tag
        (str/replace #"^scope:project:" "")
        (str/split #":"))))

(defn- build-ancestor-scopes
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
   - single scope set if not hierarchical"
  [scope]
  (cond
    (= scope "all") nil
    (nil? scope) nil  ;; Let caller handle nil -> auto mode
    (= scope "global") #{"scope:global"}
    :else
    (let [hierarchy (extract-project-hierarchy scope)
          ancestors (build-ancestor-scopes hierarchy)]
      (if (seq ancestors)
        (set (conj ancestors "scope:global"))
        #{scope "scope:global"}))))

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
