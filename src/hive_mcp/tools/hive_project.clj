(ns hive-mcp.tools.hive-project
  "MCP tool for auto-generating .hive-project.edn configuration.

   CLARITY Framework:
   - C: Composition - builds on projectile_info for detection
   - L: Layers pure - inference logic separated from I/O
   - I: Inputs guarded - validates directory exists
   - Y: Yield safe failure - graceful errors on write failure

   Generates project-specific config:
   - :project-id - stable identifier (survives directory renames)
   - :project-type - detected project type as keyword
   - :watch-dirs - directories to watch for hot-reload
   - :hot-reload - enabled by default for Clojure
   - :presets-path - project-local presets directory"
  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.elisp :as el]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.security MessageDigest]
           [java.time Instant]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Project Type Inference
;; =============================================================================

(def ^:private type->watch-dirs
  "Map of project types to watch directories for hot-reload.
   Includes test/dev dirs where applicable for Clojure projects."
  {"clojure"     ["src" "test" "dev"]
   "lein"        ["src" "test" "dev"]
   "deps.edn"    ["src" "test" "dev"]
   "clojure-cli" ["src" "test" "dev"]
   "shadow-cljs" ["src" "test" "dev"]
   "npm"         ["src" "lib"]
   "yarn"        ["src" "lib"]
   "pnpm"        ["src" "lib"]
   "cargo"       ["src"]
   "go-mod"      ["cmd" "pkg" "internal"]
   "maven"       ["src/main/java" "src/main/resources"]
   "gradle"      ["src/main/java" "src/main/kotlin"]
   "python"      ["src" "lib"]
   "poetry"      ["src" "lib"]
   "pipenv"      ["src" "lib"]
   "mix"         ["lib"]
   "rebar3"      ["src"]
   "cmake"       ["src"]
   "meson"       ["src"]
   "make"        ["src"]
   "generic"     ["src"]})

(def ^:private clojure-types
  "Project types that benefit from hot-reload."
  #{"clojure" "clojure-cli" "lein" "deps.edn" "shadow-cljs"})

(defn- infer-watch-dirs
  "Infer watch directories based on project type.
   Returns directories to watch for hot-reload."
  [project-type]
  (get type->watch-dirs project-type ["src"]))

(defn- infer-hot-reload?
  "Determine if hot-reload should be enabled by default."
  [project-type]
  (contains? clojure-types project-type))

;; =============================================================================
;; Project ID Generation
;; =============================================================================

(defn- generate-base-project-id
  "Generate a base project ID from project name.
   Format: <name>-<short-hash>
   The hash is based on name + creation timestamp for uniqueness."
  [project-name]
  (let [timestamp (.toString (Instant/now))
        raw-str (str project-name "-" timestamp)
        md (MessageDigest/getInstance "SHA-1")
        hash-bytes (.digest md (.getBytes raw-str "UTF-8"))
        hash-hex (apply str (map #(format "%02x" %) (take 4 hash-bytes)))]
    (str (str/lower-case (str/replace project-name #"[^a-zA-Z0-9]+" "-"))
         "-" hash-hex)))

;; =============================================================================
;; Parent Project Discovery
;; =============================================================================

(defn- read-parent-project-id
  "Read :project-id from a .hive-project.edn file.
   Returns the project-id string or nil on failure."
  [hive-project-file]
  (try
    (let [content (slurp hive-project-file)
          config (read-string content)]
      (:project-id config))
    (catch Exception e
      (log/debug "Failed to read parent .hive-project.edn:" (.getMessage e))
      nil)))

(defn- find-parent-project-id
  "Walk up directories from given path looking for parent .hive-project.edn.
   Stops at filesystem root or user home directory.
   Returns the parent's :project-id if found, nil otherwise."
  [project-dir]
  (let [home-dir (System/getProperty "user.home")
        start-dir (io/file project-dir)]
    (loop [current (.getParentFile start-dir)]
      (cond
        ;; No more parents - reached root
        (nil? current)
        nil

        ;; Stop at home directory (don't traverse above)
        (= (.getAbsolutePath current) home-dir)
        (let [hive-file (io/file current ".hive-project.edn")]
          (when (.exists hive-file)
            (read-parent-project-id hive-file)))

        ;; Check for .hive-project.edn in this directory
        :else
        (let [hive-file (io/file current ".hive-project.edn")]
          (if (.exists hive-file)
            (read-parent-project-id hive-file)
            (recur (.getParentFile current))))))))

(defn- generate-project-id
  "Generate a hierarchical project ID.
   If a parent project exists, returns <parent-id>:<child-id>.
   Otherwise returns just <child-id>."
  [project-name project-dir]
  (let [base-id (generate-base-project-id project-name)
        parent-id (find-parent-project-id project-dir)]
    (if parent-id
      (str parent-id ":" base-id)
      base-id)))

;; =============================================================================
;; EDN Generation
;; =============================================================================

(defn- format-edn-value
  "Format a value for EDN output."
  [v]
  (cond
    (string? v) (str "\"" v "\"")
    (keyword? v) (str v)
    (boolean? v) (if v "true" "false")
    (vector? v) (str "[" (str/join " " (map format-edn-value v)) "]")
    (map? v) (str "{"
                  (str/join "\n "
                            (map (fn [[k vv]]
                                   (str (format-edn-value k) " " (format-edn-value vv)))
                                 v))
                  "}")
    :else (pr-str v)))

(defn- generate-edn-content
  "Generate .hive-project.edn content as formatted string.
   Includes both :parent-id (preferred) and :parent (legacy) for compatibility."
  [{:keys [project-id parent project-type watch-dirs hot-reload presets-path aliases]}]
  (let [lines [(str ";; hive-mcp project configuration")
               (str ";; Generated: " (.toString (Instant/now)))
               (str ";; See: https://github.com/BuddhiLW/hive-mcp")
               ""
               (str "{:project-id \"" project-id "\"")
               ""
               (str " ;; Parent project ID (for hierarchical scoping in Knowledge Graph)")
               (str " ;; Knowledge Graph uses this for scope inheritance: child sees parent knowledge")
               (str " :parent-id " (if parent
                                     (format-edn-value parent)
                                     "nil"))
               ""
               (str " ;; Legacy field (deprecated - use :parent-id)")
               (str " :parent " (if parent
                                  (format-edn-value parent)
                                  "nil"))
               ""
               (str " ;; Detected project type")
               (str " :project-type " (format-edn-value project-type))
               ""
               (str " ;; Watch directories for hot-reload")
               (str " :watch-dirs " (format-edn-value watch-dirs))
               ""
               (str " ;; Enable/disable hot-reload")
               (str " :hot-reload " (if hot-reload "true" "false"))
               ""
               (str " ;; Project-local presets directory (optional)")
               (str " :presets-path " (if presets-path
                                        (format-edn-value presets-path)
                                        "nil"))]]
    (str (str/join "\n" lines)
         (when (seq aliases)
           (str "\n\n ;; Previous project IDs for migration\n"
                " :aliases " (format-edn-value aliases)))
         "}\n")))

;; =============================================================================
;; Projectile Integration
;; =============================================================================

(defn- get-projectile-info
  "Get project info from Projectile via Emacs.
   Returns {:name \"...\" :root \"...\" :type \"...\"} or nil on error."
  [directory]
  (let [elisp (if directory
                (format "(let ((default-directory %s))
                          (require 'hive-mcp-projectile)
                          (hive-mcp-projectile-api-project-info))"
                        (pr-str (str directory "/")))
                (el/require-and-call-json 'hive-mcp-projectile
                                          'hive-mcp-projectile-api-project-info))
        {:keys [success result]} (ec/eval-elisp elisp)]
    (when success
      (try
        (json/read-str result :key-fn keyword)
        (catch Exception e
          (log/warn "Failed to parse projectile info:" (.getMessage e))
          nil)))))

;; =============================================================================
;; Handler
;; =============================================================================

(defn handle-generate-hive-project
  "Generate .hive-project.edn based on Projectile detection.

   Parameters:
   - directory: Optional project directory (uses current project if nil)
   - force: Overwrite existing file (default: false)
   - project-id: Custom project ID (auto-generated if nil)

   Returns generated config or error."
  [{:keys [directory force project_id]}]
  (log/info "generate-hive-project" {:directory directory :force force})

  ;; Get projectile info
  (if-let [proj-info (get-projectile-info directory)]
    (let [{:keys [name root type]} proj-info
          project-root (or root directory)
          config-path (str project-root "/.hive-project.edn")
          existing? (.exists (io/file config-path))]

      ;; Check for existing file
      (if (and existing? (not force))
        (mcp-json {:error "File already exists. Use force=true to overwrite."
                   :path config-path
                   :hint "Existing .hive-project.edn found"})

        ;; Generate config
        (let [project-type-str (or type "generic")
              project-type-kw (keyword project-type-str)
              parent-id (find-parent-project-id project-root)
              generated-id (or project_id (generate-project-id name project-root))
              config {:project-id generated-id
                      :parent parent-id
                      :project-type project-type-kw
                      :watch-dirs (infer-watch-dirs project-type-str)
                      :hot-reload (infer-hot-reload? project-type-str)
                      :presets-path (when (infer-hot-reload? project-type-str)
                                      ".hive/presets")}
              edn-content (generate-edn-content config)]

          ;; Write file
          (try
            (spit config-path edn-content)
            (log/info "Generated .hive-project.edn at:" config-path)
            (mcp-json {:success true
                       :path config-path
                       :config config
                       :project-type project-type-str
                       :project-name name
                       :parent parent-id
                       :hierarchical (boolean parent-id)})
            (catch Exception e
              (log/error e "Failed to write .hive-project.edn")
              (mcp-error (str "Failed to write config: " (.getMessage e))))))))

    ;; No projectile info available
    (mcp-error "Could not detect project. Ensure directory is a valid project root.")))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tools
  "Tool definitions for hive-project generation."
  [{:name "generate_hive_project"
    :description "Generate .hive-project.edn from Projectile detection. Creates project-specific config with stable ID, project type, inferred watch directories, and hot-reload settings. Clojure projects get [src test dev], Node gets [src lib], Go gets [cmd pkg internal]."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Project directory (uses current project if not specified)"}
                               "force" {:type "boolean"
                                        :description "Overwrite existing .hive-project.edn (default: false)"}
                               "project_id" {:type "string"
                                             :description "Custom project ID (auto-generated if not specified)"}}
                  :required []}
    :handler handle-generate-hive-project}])
