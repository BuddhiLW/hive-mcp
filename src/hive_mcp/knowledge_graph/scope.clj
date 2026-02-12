(ns hive-mcp.knowledge-graph.scope
  "Scope hierarchy for Knowledge Graph.

   - S: Single responsibility - scope hierarchy management
   - O: Open for extension via new project config fields
   - L: Layers pure - no I/O in core functions, only in config loading
   - R: Represented intent - clear scope semantics
   - I: Inputs guarded - validates scope strings

   Inheritance Rules:
   - Down (parent→child): Automatic - child sees parent knowledge
   - Up (child→parent): NOT automatic - requires explicit promotion
   - Across (sibling→sibling): Via common ancestor only

   Scope Resolution Priority:
   1. Explicit parent-id from .hive-project.edn
   2. Inferred from colon-delimited scope string (e.g., 'hive-mcp:agora' -> 'hive-mcp')
   3. Global scope as root"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Project Configuration Loading
;; ============================================================

(defn- read-hive-project-config
  "Read .hive-project.edn from a directory.
   Returns the parsed config map or nil on failure."
  [dir]
  (try
    (let [config-file (io/file dir ".hive-project.edn")]
      (when (.exists config-file)
        (-> config-file slurp edn/read-string)))
    (catch Exception e
      (log/debug "Failed to read .hive-project.edn:" (.getMessage e))
      nil)))

(def ^:private config-cache
  "Cache for project configs to avoid repeated file reads.
   Key: directory path, Value: {:config ... :timestamp ...}"
  (atom {}))

(def ^:private cache-ttl-ms
  "Cache TTL in milliseconds (5 minutes)"
  (* 5 60 1000))

(defn- get-cached-config
  "Get project config with caching."
  [dir]
  (let [dir-path (.getAbsolutePath (io/file dir))
        now (System/currentTimeMillis)
        cached (get @config-cache dir-path)]
    (if (and cached (< (- now (:timestamp cached)) cache-ttl-ms))
      (:config cached)
      (let [config (read-hive-project-config dir)]
        (swap! config-cache assoc dir-path {:config config :timestamp now})
        config))))

(defn read-direct-project-config
  "Read .hive-project.edn from exactly the given directory (no parent walk).
   Returns the config map or nil if no .hive-project.edn exists in this dir."
  [directory]
  (get-cached-config (io/file directory)))

;; NOTE: clear-config-cache! is defined after the Project Config Registry section
;; to avoid forward references to project-configs and reverse-alias-index atoms.

;; ============================================================
;; Scope Parsing Utilities
;; ============================================================

(defn- normalize-scope
  "Normalize a scope string:
   - nil -> nil
   - empty string -> nil
   - 'scope:global' -> 'global'
   - 'scope:project:foo' -> 'foo'
   - 'foo' -> 'foo' (unchanged)"
  [scope]
  (when (and scope (not (str/blank? scope)))
    (cond
      (= scope "global") "global"
      (= scope "scope:global") "global"
      (str/starts-with? scope "scope:project:")
      (subs scope (count "scope:project:"))
      :else scope)))

(defn- infer-parent-from-string
  "Infer parent scope from a colon-delimited scope string.
   'hive-mcp:agora:feature' -> 'hive-mcp:agora'
   'hive-mcp:agora' -> 'hive-mcp'
   'hive-mcp' -> nil (no inferred parent, will fall back to global)
   'global' -> nil"
  [scope]
  (when (and scope (not= scope "global"))
    (let [parts (str/split scope #":")]
      (when (> (count parts) 1)
        (str/join ":" (butlast parts))))))

;; ============================================================
;; Project Config Registry
;; ============================================================

(def ^:private project-configs
  "Registry of known project configs by project-id.
   Built up as configs are discovered."
  (atom {}))

(def ^:private reverse-alias-index
  "Reverse index from alias -> canonical project-id.
   Enables O(1) lookup when a query uses an old/aliased project name.
   Built up by register-project-config! when configs have :aliases."
  (atom {}))

(defn resolve-project-id
  "Resolve a project-id that may be an alias to its canonical project-id.
   Returns the canonical project-id if the input is a known alias,
   otherwise returns the input unchanged.

   Examples:
     (resolve-project-id \"emacs-mcp\")   => \"hive-mcp\"  (if aliased)
     (resolve-project-id \"hive-mcp\")    => \"hive-mcp\"  (canonical, unchanged)
     (resolve-project-id \"unknown\")     => \"unknown\"   (not found, pass-through)
     (resolve-project-id nil)            => nil"
  [project-id]
  (when project-id
    (or (get @reverse-alias-index project-id)
        project-id)))

(defn register-project-config!
  "Register a project config for later parent-id lookups.
   Called when loading .hive-project.edn files.

   Also registers alias mappings from the config's :aliases vector
   into the reverse-alias-index for O(1) alias resolution."
  [project-id config]
  (when project-id
    (swap! project-configs assoc project-id config)
    ;; Register alias mappings: each alias -> canonical project-id
    (when-let [aliases (seq (:aliases config))]
      (doseq [alias-id aliases]
        (when (and alias-id (string? alias-id) (not= alias-id project-id))
          (swap! reverse-alias-index assoc alias-id project-id))))))

(defn deregister-project-config!
  "Remove a project config and its alias mappings.
   Useful for testing and project cleanup."
  [project-id]
  (when project-id
    (let [config (get @project-configs project-id)
          aliases (:aliases config)]
      ;; Remove alias mappings
      (when (seq aliases)
        (doseq [alias-id aliases]
          (swap! reverse-alias-index dissoc alias-id)))
      ;; Remove config
      (swap! project-configs dissoc project-id))))

(defn get-project-config
  "Get a registered project config by project-id.
   Also resolves aliases: if project-id is a known alias,
   returns the config for the canonical project."
  [project-id]
  (or (get @project-configs project-id)
      (when-let [canonical (get @reverse-alias-index project-id)]
        (get @project-configs canonical))))

(defn get-alias-index
  "Return the current reverse alias index (alias -> canonical-id).
   Read-only snapshot for inspection/debugging."
  []
  @reverse-alias-index)

(defn clear-config-cache!
  "Clear the config cache, project configs, and alias index. Useful for testing."
  []
  (reset! config-cache {})
  (reset! project-configs {})
  (reset! reverse-alias-index {}))

;; ============================================================
;; Core Scope Functions
;; ============================================================

(defn get-parent-scope
  "Get parent scope from project config or infer from scope string.

   Resolution order:
   0. Resolve aliases first (e.g., 'emacs-mcp' -> 'hive-mcp')
   1. Explicit :parent-id in registered project config
   2. Explicit :parent in registered project config (legacy)
   3. Inferred from colon-delimited scope string
   4. nil if no parent (scope is root or global)

   Examples:
     (get-parent-scope 'hive-mcp:agora') -> 'hive-mcp' (inferred)
     (get-parent-scope 'hive-mcp') -> 'global' (if no explicit parent)
     (get-parent-scope 'emacs-mcp') -> 'global' (alias resolved to hive-mcp)
     (get-parent-scope 'global') -> nil"
  [scope]
  (let [scope (normalize-scope scope)
        ;; Resolve alias to canonical project-id
        scope (when scope (resolve-project-id scope))]
    (cond
      ;; nil or global has no parent
      (or (nil? scope) (= scope "global"))
      nil

      :else
      (let [;; Check for explicit parent in registered config
            config (get-project-config scope)
            explicit-parent (or (:parent-id config) (:parent config))]
        (cond
          ;; Explicit parent from config
          (some? explicit-parent)
          (normalize-scope (str explicit-parent))

          ;; Try to infer from string (colon-delimited)
          :else
          (or (infer-parent-from-string scope)
              ;; Root project - parent is global
              "global"))))))

(defn visible-scopes
  "Return all scopes visible from given scope (inclusive).
   Walks up hierarchy to global. Resolves aliases first.

   Examples:
     (visible-scopes 'hive-mcp:agora')
     => ['hive-mcp:agora' 'hive-mcp' 'global']

     (visible-scopes 'hive-mcp')
     => ['hive-mcp' 'global']

     (visible-scopes 'emacs-mcp')
     => ['hive-mcp' 'global']  ;; alias resolved

     (visible-scopes 'global')
     => ['global']

     (visible-scopes nil)
     => ['global']"
  [scope]
  (let [scope (normalize-scope scope)
        ;; Resolve alias to canonical project-id
        scope (when scope (resolve-project-id scope))]
    (if (or (nil? scope) (= scope "global"))
      ["global"]
      (loop [s scope
             acc [scope]]
        (if-let [parent (get-parent-scope s)]
          (if (= parent "global")
            (conj acc "global")
            (recur parent (conj acc parent)))
          ;; No more parents
          (conj acc "global"))))))

(defn scope-contains?
  "Check if child-scope is within or equal to parent-scope.
   Used for inheritance checks. Resolves aliases first.

   Examples:
     (scope-contains? 'hive-mcp' 'hive-mcp:agora') -> true
     (scope-contains? 'hive-mcp:agora' 'hive-mcp') -> false
     (scope-contains? 'global' 'hive-mcp') -> true (global contains all)
     (scope-contains? 'hive-mcp' 'hive-mcp') -> true (equal scopes)
     (scope-contains? 'hive-mcp' 'emacs-mcp') -> true (alias resolved)"
  [parent-scope child-scope]
  (let [parent-scope (normalize-scope parent-scope)
        child-scope (normalize-scope child-scope)
        ;; Resolve aliases
        parent-scope (when parent-scope (resolve-project-id parent-scope))
        child-scope (when child-scope (resolve-project-id child-scope))]
    (cond
      ;; Same scope (after alias resolution)
      (= parent-scope child-scope)
      true

      ;; Global contains everything
      (= parent-scope "global")
      true

      ;; nil parent contains nothing (except nil child)
      (nil? parent-scope)
      (nil? child-scope)

      ;; Check if child's visible scopes include parent
      :else
      (let [child-visible (set (visible-scopes child-scope))]
        (contains? child-visible parent-scope)))))

;; ============================================================
;; Path-based Scope Inference
;; ============================================================

(defn- find-nearest-hive-project
  "Walk up from file-path finding nearest .hive-project.edn.
   Returns [directory config] or nil if not found."
  [file-path]
  (let [start-file (io/file file-path)
        start-dir (if (.isDirectory start-file)
                    start-file
                    (.getParentFile start-file))
        home-dir (System/getProperty "user.home")]
    (loop [current start-dir]
      (cond
        ;; Reached root
        (nil? current)
        nil

        ;; Don't traverse above home
        (= (.getAbsolutePath current) home-dir)
        (when-let [config (get-cached-config current)]
          [(.getAbsolutePath current) config])

        :else
        (if-let [config (get-cached-config current)]
          (do
            ;; Register config for later lookups
            (when-let [project-id (:project-id config)]
              (register-project-config! project-id config))
            [(.getAbsolutePath current) config])
          (recur (.getParentFile current)))))))

(defn infer-scope-from-path
  "Infer scope from file path by finding nearest .hive-project.edn.
   Returns the project-id from the config, or 'global' if not found.

   Also registers the project config for future parent-id lookups."
  [file-path]
  (if-let [[_dir config] (find-nearest-hive-project file-path)]
    (let [project-id (:project-id config)]
      (when project-id
        (register-project-config! project-id config))
      (or project-id "global"))
    "global"))

;; ============================================================
;; Scope Tag Utilities
;; ============================================================

(defn scope->tag
  "Convert a scope to a scope tag for Chroma filtering.
   'hive-mcp' -> 'scope:project:hive-mcp'
   'global' -> 'scope:global'"
  [scope]
  (let [scope (normalize-scope scope)]
    (if (or (nil? scope) (= scope "global"))
      "scope:global"
      (str "scope:project:" scope))))

(defn visible-scope-tags
  "Return all scope tags visible from given scope.
   Suitable for Chroma metadata filtering.

   (visible-scope-tags 'hive-mcp:agora')
   => #{'scope:project:hive-mcp:agora' 'scope:project:hive-mcp' 'scope:global'}"
  [scope]
  (set (map scope->tag (visible-scopes scope))))

;; ============================================================
;; HCR Wave 3: Descendant Scope Tags
;; ============================================================

(defn descendant-scope-tags
  "Return all scope tags for descendant projects (children, grandchildren, etc).
   HCR Wave 3: Enables memory queries to include child project memories.

   This is the inverse of visible-scope-tags:
   - visible-scope-tags: goes UP to ancestors (child sees parent)
   - descendant-scope-tags: goes DOWN to children (parent queries child)

   Uses project tree from hive-mcp.project.tree (loaded lazily to avoid
   circular dependency).

   Args:
     scope - Project ID or scope string

   Returns:
     Set of scope tags like #{\"scope:project:child1\" \"scope:project:child2\"}
     Returns empty set if project has no children or tree not scanned.

   Example:
     (descendant-scope-tags \"hive-mcp\")
     => #{\"scope:project:hive-mcp:agora\" \"scope:project:hive-mcp:memory\"}"
  [scope]
  (let [scope (normalize-scope scope)]
    (when (and scope (not= scope "global"))
      (try
        ;; Lazy require to avoid circular dependency (tree.clj requires scope.clj)
        (require 'hive-mcp.project.tree)
        (let [get-tags (resolve 'hive-mcp.project.tree/get-descendant-scope-tags)]
          (or (get-tags scope) #{}))
        (catch Exception e
          (log/debug "Failed to get descendant scope tags:" (.getMessage e))
          #{})))))

(defn descendant-scopes
  "Return all descendant project IDs (children, grandchildren, etc).
   Lower-level function - use descendant-scope-tags for memory filtering.

   Example:
     (descendant-scopes \"hive-mcp\")
     => [\"hive-mcp:agora\" \"hive-mcp:memory\"]"
  [scope]
  (let [scope (normalize-scope scope)]
    (when (and scope (not= scope "global"))
      (try
        (require 'hive-mcp.project.tree)
        (let [get-descendants (resolve 'hive-mcp.project.tree/get-descendant-scopes)]
          (or (get-descendants scope) []))
        (catch Exception e
          (log/debug "Failed to get descendant scopes:" (.getMessage e))
          [])))))

(defn full-hierarchy-scope-tags
  "Return scope tags for full hierarchy: self + ancestors + descendants.
   HCR Wave 3: Use this for bidirectional memory queries.

   Combines:
   - visible-scope-tags (upward: ancestors + global)
   - descendant-scope-tags (downward: children)

   Example:
     (full-hierarchy-scope-tags \"hive-mcp\")
     => #{\"scope:project:hive-mcp\"      ; self
          \"scope:global\"                 ; ancestor
          \"scope:project:hive-mcp:agora\" ; child
          \"scope:project:hive-mcp:memory\"}"
  [scope]
  (let [scope (normalize-scope scope)]
    (if (or (nil? scope) (= scope "global"))
      #{"scope:global"}
      (clojure.set/union
       (visible-scope-tags scope)
       (descendant-scope-tags scope)))))

;; ============================================================
;; Backward Compatibility
;; ============================================================

(defn derive-hierarchy-scope-filter
  "Derive hierarchical scope filter that includes ancestors.
   Compatible with existing memory/scope.clj API.

   Returns a set of valid scope tags:
   - nil if scope is 'all' (no filtering)
   - set including scope + ancestors + global if hierarchical"
  [scope]
  (cond
    (= scope "all") nil
    (nil? scope) nil  ;; Let caller handle nil -> auto mode
    :else (visible-scope-tags scope)))