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
... (truncated)