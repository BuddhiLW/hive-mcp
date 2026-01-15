(ns hive-mcp.tools.hive-project
  "MCP tool for auto-generating .hive-project.edn configuration.

   CLARITY Framework:
   - C: Composition - builds on projectile_info for detection
   - L: Layers pure - inference logic separated from I/O
   - I: Inputs guarded - validates directory exists
   - Y: Yield safe failure - graceful errors on write failure

   Generates project-specific config:
   - :project-id - stable identifier (survives directory renames)
   - :src-dirs - inferred from project type
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

;; =============================================================================
;; Project Type Inference
;; =============================================================================

(def ^:private type->src-dirs
  "Map of project types to typical source directories."
  {"clojure"   ["src"]
   "lein"      ["src"]
   "deps.edn"  ["src"]
   "shadow-cljs" ["src"]
   "npm"       ["src" "lib"]
   "yarn"      ["src" "lib"]
   "pnpm"      ["src" "lib"]
   "cargo"     ["src"]
   "go-mod"    ["." "cmd" "internal" "pkg"]
   "maven"     ["src/main/java" "src/main/resources"]
   "gradle"    ["src/main/java" "src/main/kotlin"]
   "python"    ["src" "."]
   "poetry"    ["src" "."]
   "pipenv"    ["src" "."]
   "mix"       ["lib"]
   "rebar3"    ["src"]
   "cmake"     ["src"]
   "meson"     ["src"]
   "make"      ["src"]
   "generic"   ["src" "lib"]})

(def ^:private clojure-types
  "Project types that benefit from hot-reload."
  #{"clojure" "lein" "deps.edn" "shadow-cljs"})

(defn- infer-src-dirs
  "Infer source directories based on project type."
  [project-type]
  (get type->src-dirs project-type ["src"]))

(defn- infer-hot-reload?
  "Determine if hot-reload should be enabled by default."
  [project-type]
  (contains? clojure-types project-type))

;; =============================================================================
;; Project ID Generation
;; =============================================================================

(defn- generate-project-id
  "Generate a stable project ID from project name.
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
  "Generate .hive-project.edn content as formatted string."
  [{:keys [project-id src-dirs hot-reload presets-path aliases]}]
  (let [lines [(str ";; hive-mcp project configuration")
               (str ";; Generated: " (.toString (Instant/now)))
               (str ";; See: https://github.com/BuddhiLW/hive-mcp")
               ""
               (str "{:project-id \"" project-id "\"")
               ""
               (str " ;; Source directories for hot-reload watching")
               (str " :src-dirs " (format-edn-value src-dirs))
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
        {:keys [success result error]} (ec/eval-elisp elisp)]
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
        (let [project-type (or type "generic")
              config {:project-id (or project_id (generate-project-id name))
                      :src-dirs (infer-src-dirs project-type)
                      :hot-reload (infer-hot-reload? project-type)
                      :presets-path (when (infer-hot-reload? project-type)
                                      ".hive/presets")}
              edn-content (generate-edn-content config)]

          ;; Write file
          (try
            (spit config-path edn-content)
            (log/info "Generated .hive-project.edn at:" config-path)
            (mcp-json {:success true
                       :path config-path
                       :config config
                       :project-type project-type
                       :project-name name})
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
    :description "Generate .hive-project.edn from Projectile detection. Creates project-specific config with stable ID, inferred source directories, and hot-reload settings based on project type (Clojure, npm, Go, etc.)."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Project directory (uses current project if not specified)"}
                               "force" {:type "boolean"
                                        :description "Overwrite existing .hive-project.edn (default: false)"}
                               "project_id" {:type "string"
                                             :description "Custom project ID (auto-generated if not specified)"}}
                  :required []}
    :handler handle-generate-hive-project}])
