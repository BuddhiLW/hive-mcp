(ns hive-mcp.knowledge-graph.scope
  "Scope hierarchy for Knowledge Graph.

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
  "Cache for project configs to avoid repeated file reads."
  (atom {}))

(def ^:private cache-ttl-ms
  "Cache TTL in milliseconds (5 minutes)"
  (* 5 60 1000))

(defn- cache-fresh?
  "True if cached entry is within TTL. CC-free: when-let only."
  [entry now]
  (when-let [ts (:timestamp entry)]
    (< (- now ts) cache-ttl-ms)))

(defn- get-cached-config
  "Get project config with caching."
  [dir]
  (let [dir-path (.getAbsolutePath (io/file dir))
        now (System/currentTimeMillis)
        entry (get @config-cache dir-path)]
    (if-let [_ (cache-fresh? entry now)]
      (:config entry)
      (let [config (read-hive-project-config dir)]
        (swap! config-cache assoc dir-path {:config config :timestamp now})
        config))))

(defn read-direct-project-config
  "Read .hive-project.edn from exactly the given directory (no parent walk).
   Returns the config map or nil if no .hive-project.edn exists in this dir."
  [directory]
  (get-cached-config (io/file directory)))

;; ============================================================
;; Scope Parsing Utilities
;; ============================================================

(def ^:private scope-prefix "scope:project:")
(def ^:private scope-prefix-len (count scope-prefix))

(defn- normalize-scope
  "Normalize a scope string to canonical form.
   nil/blank -> nil, 'scope:global' -> 'global', 'scope:project:foo' -> 'foo'."
  [scope]
  (when-not (str/blank? (str scope))
    (case scope
      "global"       "global"
      "scope:global" "global"
      (cond-> scope
        (str/starts-with? scope scope-prefix)
        (subs scope-prefix-len)))))

(defn- infer-parent-from-string
  "Infer parent scope from colon-delimited string.
   'a:b:c' -> 'a:b', 'a:b' -> 'a', 'a' -> nil, 'global' -> nil."
  [scope]
  (when-let [parts (when-not (= scope "global")
                     (let [p (str/split scope #":")]
                       (when (> (count p) 1) p)))]
    (str/join ":" (butlast parts))))

;; ============================================================
;; Shared Predicates (CC-free)
;; ============================================================

(def ^:private global-set
  "Set for O(1) global-or-nil? checks."
  #{nil "global"})

(defn- global-or-nil?
  "True if scope is nil or 'global'. CC-free: uses set contains?."
  [scope]
  (contains? global-set scope))

;; ============================================================
;; Project Config Registry
;; ============================================================

(def ^:private project-configs
  "Registry of known project configs by project-id."
  (atom {}))

(def ^:private reverse-alias-index
  "Reverse index: alias -> canonical project-id."
  (atom {}))

(defn resolve-project-id
  "Resolve a project-id that may be an alias to its canonical project-id.
   Returns the canonical project-id if the input is a known alias,
   otherwise returns the input unchanged."
  [project-id]
  (when-let [pid project-id]
    (get @reverse-alias-index pid pid)))

(defn- valid-alias?
  "True if alias-id is registerable (string, non-nil, different from canonical)."
  [alias-id project-id]
  (and (string? alias-id) (not= alias-id project-id)))

(defn register-project-config!
  "Register a project config for later parent-id lookups.
   Also registers alias mappings from the config's :aliases vector."
  [project-id config]
  (when-let [pid project-id]
    (swap! project-configs assoc pid config)
    (run! (fn [a]
            (when (valid-alias? a pid)
              (swap! reverse-alias-index assoc a pid)))
          (:aliases config))))

(defn deregister-project-config!
  "Remove a project config and its alias mappings."
  [project-id]
  (when-let [pid project-id]
    (when-let [aliases (seq (:aliases (get @project-configs pid)))]
      (run! #(swap! reverse-alias-index dissoc %) aliases))
    (swap! project-configs dissoc pid)))

(defn get-project-config
  "Get a registered project config by project-id.
   Also resolves aliases."
  [project-id]
  (when-let [pid project-id]
    (or (get @project-configs pid)
        (when-let [canonical (get @reverse-alias-index pid)]
          (get @project-configs canonical)))))

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

(defn- explicit-parent
  "Get explicit parent from config (:parent-id or legacy :parent). CC-free."
  [config]
  (when-let [cfg config]
    (or (:parent-id cfg) (:parent cfg))))

(defn get-parent-scope
  "Get parent scope from project config or infer from scope string.

   Resolution order:
   0. Normalize + resolve aliases
   1. Explicit :parent-id in registered project config
   2. Explicit :parent in registered project config (legacy)
   3. Inferred from colon-delimited scope string
   4. 'global' if scope is root (no parent inferred)"
  [scope]
  (let [scope (some-> scope normalize-scope resolve-project-id)]
    (when-not (global-or-nil? scope)
      (if-let [ep (explicit-parent (get-project-config scope))]
        (normalize-scope (str ep))
        (if-let [inferred (infer-parent-from-string scope)]
          inferred
          "global")))))

(defn visible-scopes
  "Return all scopes visible from given scope (inclusive).
   Walks up hierarchy to global using iterate + take-while.

   Examples:
     (visible-scopes 'hive-mcp:agora')
     => ['hive-mcp:agora' 'hive-mcp' 'global']

     (visible-scopes nil) => ['global']"
  [scope]
  (let [scope (some-> scope normalize-scope resolve-project-id)]
    (if (global-or-nil? scope)
      ["global"]
      (vec (take-while some? (iterate get-parent-scope scope))))))

(defn scope-contains?
  "Check if child-scope is within or equal to parent-scope.
   Resolves aliases first.

   Examples:
     (scope-contains? 'global' 'hive-mcp') -> true
     (scope-contains? 'hive-mcp' 'hive-mcp:agora') -> true
     (scope-contains? nil nil) -> true"
  [parent-scope child-scope]
  (let [ps (some-> parent-scope normalize-scope resolve-project-id)
        cs (some-> child-scope normalize-scope resolve-project-id)]
    (cond
      (= ps cs) true
      (= ps "global") true
      (nil? ps) (nil? cs)
      :else (contains? (set (visible-scopes cs)) ps))))

;; ============================================================
;; Path-based Scope Inference
;; ============================================================

(defn- at-home-boundary?
  "True if directory is at user.home. CC-free via when-let."
  [^java.io.File current]
  (when-let [home (System/getProperty "user.home")]
    (= (.getAbsolutePath current) home)))

(defn- try-register-config!
  "Try to load config from dir. If found, register and return [dir config]."
  [^java.io.File dir]
  (when-let [config (get-cached-config dir)]
    (when-let [pid (:project-id config)]
      (register-project-config! pid config))
    [(.getAbsolutePath dir) config]))

(defn- find-nearest-hive-project
  "Walk up from file-path finding nearest .hive-project.edn.
   Returns [directory config] or nil if not found."
  [file-path]
  (let [start-file (io/file file-path)
        start-dir  (cond-> start-file
                     (not (.isDirectory start-file)) (.getParentFile))]
    (loop [current start-dir]
      (when-let [dir current]
        (if-let [result (try-register-config! dir)]
          result
          (when-not (at-home-boundary? dir)
            (recur (.getParentFile dir))))))))

(defn infer-scope-from-path
  "Infer scope from file path by finding nearest .hive-project.edn.
   Returns the project-id from the config, or 'global' if not found."
  [file-path]
  (if-let [[_dir config] (find-nearest-hive-project file-path)]
    (if-let [pid (:project-id config)]
      (do (register-project-config! pid config) pid)
      "global")
    "global"))

;; ============================================================
;; Scope Tag Utilities
;; ============================================================

(defn scope->tag
  "Convert a scope to a scope tag for Chroma filtering.
   'hive-mcp' -> 'scope:project:hive-mcp'
   'global'/nil -> 'scope:global'"
  [scope]
  (let [scope (normalize-scope scope)]
    (if (global-or-nil? scope)
      "scope:global"
      (str scope-prefix scope))))

(defn visible-scope-tags
  "Return all scope tags visible from given scope.
   Suitable for Chroma metadata filtering."
  [scope]
  (set (map scope->tag (visible-scopes scope))))

;; ============================================================
;; HCR Wave 3: Descendant Scope Tags
;; ============================================================

(defn- resolve-tree-fn
  "Lazy-resolve a function from project.tree namespace. Returns fn or nil."
  [sym-name]
  (try
    (require 'hive-mcp.project.tree)
    (resolve (symbol "hive-mcp.project.tree" sym-name))
    (catch Exception e
      (log/debug "Failed to resolve tree fn:" sym-name (.getMessage e))
      nil)))

(defn descendant-scope-tags
  "Return all scope tags for descendant projects (children, grandchildren, etc).
   Inverse of visible-scope-tags: goes DOWN instead of UP.
   Returns nil for nil/global scopes, empty set if no children."
  [scope]
  (let [scope (normalize-scope scope)]
    (when-not (global-or-nil? scope)
      (if-let [get-tags (resolve-tree-fn "get-descendant-scope-tags")]
        (if-let [tags (get-tags scope)] tags #{})
        #{}))))

(defn descendant-scopes
  "Return all descendant project IDs (children, grandchildren, etc).
   Returns nil for nil/global scopes, empty vector if no children."
  [scope]
  (let [scope (normalize-scope scope)]
    (when-not (global-or-nil? scope)
      (if-let [get-descendants (resolve-tree-fn "get-descendant-scopes")]
        (if-let [desc (get-descendants scope)] desc [])
        []))))

(defn full-hierarchy-scope-tags
  "Return scope tags for full hierarchy: self + ancestors + descendants."
  [scope]
  (let [scope (normalize-scope scope)]
    (if (global-or-nil? scope)
      #{"scope:global"}
      (clojure.set/union
       (visible-scope-tags scope)
       (descendant-scope-tags scope)))))

;; ============================================================
;; Backward Compatibility
;; ============================================================

(defn derive-hierarchy-scope-filter
  "Derive hierarchical scope filter that includes ancestors.
   Returns nil for 'all' or nil scope (no filtering),
   set of scope tags otherwise."
  [scope]
  (case scope
    (nil "all") nil
    (visible-scope-tags scope)))
