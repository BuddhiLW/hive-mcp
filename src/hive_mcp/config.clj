(ns hive-mcp.config
  "Global configuration loader for ~/.config/hive-mcp/config.edn.

   Bounded context: Global user-level configuration for hive-mcp.

   SOLID:
   - S: Single responsibility — load+cache global config
   - O: Open for extension — new config keys can be added without code changes
   - L: Layers pure — core functions are pure, I/O isolated to load-global-config!
   - I: Inputs guarded — missing file returns defaults, malformed file logged

   CLARITY:
   - R: Represented intent — explicit defaults merged with user overrides
   - Y: Yield safe failure — missing/malformed config uses defaults

   Schema (~/.config/hive-mcp/config.edn):
     {:project-roots  [\"path1\" \"path2\"]
      :defaults       {:kg-backend :datahike :hot-reload false}
      :project-overrides {\"proj\" {:hot-reload true}}
      :parent-rules   [{:path-prefix \"/path/prefix/\" :parent-id \"parent-proj\"}]
      :embeddings     {:ollama {:host \"http://localhost:11434\" :model \"nomic-embed-text\"}
                       :openrouter {:model \"qwen/qwen3-embedding-8b\"}}
      :services       {:chroma {:mode :local :host \"localhost\" :port 8000}
                       :ollama {:mode :local :host \"http://localhost:11434\" :model \"nomic-embed-text\"}
                       :datahike {:mode :local :path \"data/kg\"}
                       :kg {:mode :local :backend :datalevin}
                       :project {:mode :local :id nil :dir nil :src-dirs [\"src\"]}
                       :drone {:mode :local :default-model \"devstral-small:24b\" :default-backend :openrouter}
                       :scheduler {:mode :local :enabled true :interval-minutes 60
                                   :memory-limit 50 :edge-limit 100 :disc-enabled true}}
      ;; :mode values: :local (localhost defaults) or :remote (use :host/:port as-is)
      :secrets        {:openrouter-api-key nil :openai-api-key nil}}

   Usage:
     (load-global-config!)       ;; Load from disk, cache in atom
     (get-global-config)         ;; Return cached config (or defaults)
     (get-project-roots)         ;; Shortcut for :project-roots
     (get-defaults)              ;; Shortcut for :defaults
     (get-project-overrides k)   ;; Overrides for specific project
     (get-parent-for-path p)     ;; Resolve parent-id via :parent-rules
     (get-service-config :chroma) ;; Service-specific config map
     (get-secret :openrouter-api-key) ;; Secret with env var fallback
     (get-config-value \"embeddings.ollama.host\") ;; Dotted key path access
     (set-config-value! \"embeddings.ollama.host\" \"http://new:11434\") ;; Write + persist"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Defaults
;; =============================================================================

(def ^:private default-config
  "Default configuration when ~/.config/hive-mcp/config.edn is missing or incomplete."
  {:project-roots []
   :defaults {:kg-backend :datahike
              :hot-reload false
              :presets-path nil}
   :project-overrides {}
   :parent-rules []
   :embeddings {:ollama {:host "http://localhost:11434"
                         :model "nomic-embed-text"}
                :openrouter {:model "qwen/qwen3-embedding-8b"}}
   :services {:chroma {:mode :local :host "localhost" :port 8000}
              :ollama {:mode :local :host "http://localhost:11434" :model "nomic-embed-text"}
              :datahike {:mode :local :path "data/kg"}
              :nrepl {:mode :local :port 7910}
              :prometheus {:mode :local :url "http://localhost:9090"}
              :loki {:mode :local :url "http://localhost:3100"}
              :websocket {:mode :local :enabled false :port nil :project-dir nil}
              :ws-channel {:mode :local :port 9999}
              :channel {:mode :local :port 9998}
              :olympus {:mode :local :ws-port 7911}
              :overarch {:mode :local :jar nil}
              :presets {:mode :local :dir nil}
              :kg {:mode :local :backend :datalevin}
              :project {:mode :local :id nil :dir nil :src-dirs ["src"]}
              :drone {:mode :local :default-model "devstral-small:24b" :default-backend :openrouter}
              :scheduler {:mode :local :enabled true :interval-minutes 60
                          :memory-limit 50 :edge-limit 100 :disc-enabled true}}
   :secrets {:openrouter-api-key nil
             :openai-api-key nil}})

(def ^:private legacy-config-path
  "Legacy path for backward compatibility migration."
  (str (System/getProperty "user.home") "/.config/hive.edn"))

(def ^:private config-path
  "Canonical path for global hive config."
  (str (System/getProperty "user.home") "/.config/hive-mcp/config.edn"))

;; =============================================================================
;; State
;; =============================================================================

;; Cached global configuration atom.
;; nil = not loaded yet, map = loaded config.
(defonce ^:private global-config (atom nil))

;; =============================================================================
;; Loading
;; =============================================================================

(defn- read-config-file
  "Read and parse an EDN config file. Returns parsed map or nil on failure.
   CLARITY-Y: Yield safe failure — logs and returns nil on any error."
  [path]
  (try
    (let [f (io/file path)]
      (when (.exists f)
        (let [content (slurp f)
              parsed (edn/read-string content)]
          (when (map? parsed)
            parsed))))
    (catch Exception e
      (log/warn "Failed to read config file" path ":" (.getMessage e))
      nil)))

(defn- merge-config
  "Deep-merge user config with defaults.
   User values override defaults at each level.
   Nested maps are merged recursively; other values replaced."
  [defaults user-config]
  (merge-with
   (fn [default-val user-val]
     (if (and (map? default-val) (map? user-val))
       (merge default-val user-val)
       user-val))
   defaults
   user-config))

(defn load-global-config!
  "Load global config from ~/.config/hive-mcp/config.edn, merge with defaults, cache in atom.

   Falls back to legacy ~/.config/hive.edn if new path not found (migration).

   Safe to call multiple times — always re-reads from disk and updates cache.
   Returns the merged config map.

   CLARITY-Y: If file is missing or malformed, returns defaults (never throws)."
  ([]
   (load-global-config! config-path))
  ([path]
   (let [;; Try new path first, fall back to legacy
         user-config (or (read-config-file path)
                         (when (= path config-path)
                           (when-let [legacy (read-config-file legacy-config-path)]
                             (log/info "Migrating config: found legacy" legacy-config-path
                                       "→ please move to" config-path)
                             legacy)))
         merged (if user-config
                  (merge-config default-config user-config)
                  default-config)]
     (reset! global-config merged)
     (log/info "Global config loaded from" path
               (if user-config "(user config found)" "(using defaults)"))
     merged)))

;; =============================================================================
;; Accessors (all return defaults if not yet loaded)
;; =============================================================================

(defn get-global-config
  "Return the cached global config, or defaults if not yet loaded.
   Does NOT trigger a disk read — call load-global-config! first."
  []
  (or @global-config default-config))

(defn get-project-roots
  "Return the :project-roots vector from global config.
   These are root directories to scan for projects."
  []
  (:project-roots (get-global-config)))

(defn get-defaults
  "Return the :defaults map from global config.
   Contains default settings for new/unconfigured projects."
  []
  (:defaults (get-global-config)))

(defn get-project-overrides
  "Return overrides for a specific project-id, or nil if none.
   Overrides are merged with :defaults by the caller."
  [project-id]
  (get-in (get-global-config) [:project-overrides project-id]))

(defn get-project-config
  "Return the effective config for a project-id.
   Merges :defaults with :project-overrides for that project.
   Returns just :defaults if no overrides exist."
  [project-id]
  (let [defaults (get-defaults)
        overrides (get-project-overrides project-id)]
    (if overrides
      (merge defaults overrides)
      defaults)))

(defn get-parent-rules
  "Return the :parent-rules vector from global config.
   Each rule is {:path-prefix \"...\" :parent-id \"...\"}."
  []
  (:parent-rules (get-global-config)))

(defn get-parent-for-path
  "Resolve parent-id for a directory path via :parent-rules.

   Iterates through rules in order. First rule whose :path-prefix
   matches the start of the given path wins.

   Returns parent-id string or nil if no rule matches.

   Example:
     With rule {:path-prefix \"/home/user/PP/hive/\" :parent-id \"hive-mcp\"}
     (get-parent-for-path \"/home/user/PP/hive/hive-hot\") => \"hive-mcp\""
  [directory-path]
  (when directory-path
    (let [rules (get-parent-rules)
          ;; Normalize path to end with / for consistent prefix matching
          norm-path (if (.endsWith (str directory-path) "/")
                      (str directory-path)
                      (str directory-path "/"))]
      (->> rules
           (filter (fn [{:keys [path-prefix]}]
                     (and path-prefix
                          (.startsWith norm-path path-prefix))))
           first
           :parent-id))))

;; =============================================================================
;; Service & Secret Accessors
;; =============================================================================

(defn get-service-config
  "Return config map for a specific service (e.g., :chroma, :ollama, :datahike).
   Returns the service-specific config map, or nil if not configured.
   Each service includes a :mode key (:local or :remote).

   Examples:
     (get-service-config :chroma)   => {:mode :local :host \"localhost\" :port 8000}
     (get-service-config :ollama)   => {:mode :local :host \"http://localhost:11434\" :model \"nomic-embed-text\"}
     (get-service-config :datahike) => {:mode :local :path \"data/kg\"}"
  [service-key]
  (get-in (get-global-config) [:services service-key]))

(defn get-service-mode
  "Return the :mode for a service (:local or :remote, defaults to :local).

   Examples:
     (get-service-mode :chroma)   => :local
     (get-service-mode :unknown)  => :local"
  [service-key]
  (get-in (get-global-config) [:services service-key :mode] :local))

(defn get-service-value
  "Get a specific field from service config with mode-aware host/port resolution.

   Each service supports a :mode key (:local or :remote, default :local).
   When :mode is :remote, the service's :host/:port come from config as-is
   (pointing to a remote machine). When :local, localhost defaults apply.

   Priority: config.edn :services > env var > default value.

   Options:
     :env     - env var name for fallback (e.g., \"HIVE_MCP_NREPL_PORT\")
     :parse   - parse fn for env var string (e.g., parse-long)
     :default - fallback value if not found anywhere

   Examples:
     (get-service-value :nrepl :port :env \"HIVE_MCP_NREPL_PORT\" :parse parse-long :default 7910)
     (get-service-value :prometheus :url :env \"PROMETHEUS_URL\" :default \"http://localhost:9090\")
     ;; With :mode :remote, :host/:port are used from config directly
     ;; With :mode :local (default), behavior is unchanged"
  [service-key field-key & {:keys [env parse default]}]
  (let [svc-cfg (get-in (get-global-config) [:services service-key])
        mode (get svc-cfg :mode :local)
        config-val (get svc-cfg field-key)
        ;; When :mode is :local and requesting :host, prefer localhost defaults
        ;; When :mode is :remote, use the configured :host/:port as-is
        effective-val (if (and (= mode :local)
                               (#{:host :url} field-key)
                               (nil? config-val))
                        ;; :local mode without explicit host — let env/default handle it
                        nil
                        config-val)
        env-val (when env
                  (when-let [raw (System/getenv env)]
                    (if parse (parse raw) raw)))]
    (or effective-val env-val default)))

(defn get-secret
  "Return a secret value, checking config.edn first, then env var fallback.

   The env var name is derived from the secret key by uppercasing and
   replacing hyphens with underscores:
     :openrouter-api-key => OPENROUTER_API_KEY
     :openai-api-key     => OPENAI_API_KEY

   Returns nil if not found in either location.

   CLARITY-Y: Yield safe — never throws, returns nil on missing."
  [secret-key]
  (let [config-val (get-in (get-global-config) [:secrets secret-key])
        env-name (-> (name secret-key)
                     (str/replace "-" "_")
                     (str/upper-case))]
    (or config-val (System/getenv env-name))))

;; =============================================================================
;; Dotted Key Path Access
;; =============================================================================

;; Parse a dotted key string like "embeddings.ollama.host" into keyword path
;; [:embeddings :ollama :host].
(defn parse-key-path
  [key-str]
  (when (and key-str (not (str/blank? key-str)))
    (mapv keyword (str/split key-str #"\."))))

;; Read a value at a dotted key path from the cached config.
;; Returns nil if the path does not exist.
;;
;; Examples:
;;   (get-config-value "embeddings.ollama.host") => "http://localhost:11434"
;;   (get-config-value "defaults.kg-backend")    => :datahike
(defn get-config-value
  [key-str]
  (let [path (parse-key-path key-str)]
    (get-in (get-global-config) path)))

;; =============================================================================
;; Write Config to Disk
;; =============================================================================

;; Write the current cached config map to disk as pretty-printed EDN.
;; Creates parent directories if they don't exist.
;; CLARITY-I: Ensures parent dir exists before write.
(defn- write-config!
  ([] (write-config! config-path))
  ([path]
   (let [f (io/file path)
         parent (.getParentFile f)]
     (when (and parent (not (.exists parent)))
       (.mkdirs parent))
     (spit f (pr-str (get-global-config)))
     (log/info "Config written to" path))))

;; =============================================================================
;; Set Config Value
;; =============================================================================

;; Update a value at a dotted key path in the config atom AND persist to disk.
;;
;; Examples:
;;   (set-config-value! "embeddings.ollama.host" "http://ollama.k8s:11434")
;;   (set-config-value! "defaults.hot-reload" true)
;;
;; Returns the updated config map.
;; CLARITY-Y: Throws on invalid key path (empty/nil).
(defn set-config-value!
  ([key-str value] (set-config-value! key-str value config-path))
  ([key-str value path]
   (let [kp (parse-key-path key-str)]
     (when (empty? kp)
       (throw (ex-info "Invalid config key path" {:key key-str})))
     ;; Ensure config is loaded first
     (when-not @global-config
       (load-global-config! path))
     (let [updated (swap! global-config assoc-in kp value)]
       (write-config! path)
       (log/info "Config updated:" key-str "=" value)
       updated))))

;; =============================================================================
;; Reset (for testing)
;; =============================================================================

(defn reset-config!
  "Reset the cached config to nil. Useful for testing."
  []
  (reset! global-config nil))
