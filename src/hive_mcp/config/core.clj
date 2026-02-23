(ns hive-mcp.config.core
  "Global configuration loader for ~/.config/hive-mcp/config.edn.
   Canonical implementation — hive-mcp.config is the public facade."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Defaults
;; =============================================================================

(def ^:private default-config
  "Default configuration when config file is missing."
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
              :kg {:mode :local :backend :datalevin
                   :writer {:backend :self}}
              :project {:mode :local :id nil :dir nil :src-dirs ["src"]}
              :forge {:mode :local :legacy false :budget-routing false
                      ;; Max ms to wait for a ling to register in DataScript + pass CLI
                      ;; check before dispatch is attempted. Claude CLI can take 10-30s
                      ;; to start, so 60s is the safe default. Configurable via:
                      ;;   {:services {:forge {:readiness-timeout-ms 90000}}}
                      :readiness-timeout-ms 60000}
              :drone {:mode :local :default-model "devstral-small:24b" :default-backend :openrouter}
              :nats {:mode :local
                     :enabled false
                     :url "nats://localhost:4222"
                     :connection-timeout 5000
                     :max-reconnects 5
                     :reconnect-wait 1000}
              :scheduler {:mode :local :enabled true :interval-minutes 60
                          :memory-limit 50 :edge-limit 100 :disc-enabled true}}
   :secrets {:openrouter-api-key nil
             :openai-api-key nil}
   :models {:task-models {:coding "x-ai/grok-code-fast-1"
                          :coding-alt "deepseek/deepseek-v3.2"
                          :testing "x-ai/grok-code-fast-1"
                          :bugfix "x-ai/grok-code-fast-1"
                          :general "x-ai/grok-code-fast-1"
                          :arch "deepseek/deepseek-v3.2"
                          :docs "deepseek/deepseek-v3.2"}
            :routing {:testing {:primary "x-ai/grok-code-fast-1"
                                :secondary "deepseek/deepseek-v3.2"}
                      :refactoring {:primary "x-ai/grok-code-fast-1"
                                    :secondary "deepseek/deepseek-v3.2"}
                      :implementation {:primary "x-ai/grok-code-fast-1"
                                       :secondary "deepseek/deepseek-v3.2"}
                      :bugfix {:primary "x-ai/grok-code-fast-1"
                               :secondary "deepseek/deepseek-v3.2"}
                      :documentation {:primary "deepseek/deepseek-v3.2"
                                      :secondary "x-ai/grok-code-fast-1"}
                      :general {:primary "x-ai/grok-code-fast-1"
                                :secondary "deepseek/deepseek-v3.2"}}
            :default-model "x-ai/grok-code-fast-1"}})

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
  "Read and parse an EDN config file."
  [path]
  (rescue nil
          (let [f (io/file path)]
            (when (.exists f)
              (let [content (slurp f)
                    parsed (edn/read-string content)]
                (when (map? parsed)
                  parsed))))))

(defn- merge-config
  "Deep-merge user config with defaults."
  [defaults user-config]
  (merge-with
   (fn [default-val user-val]
     (if (and (map? default-val) (map? user-val))
       (merge default-val user-val)
       user-val))
   defaults
   user-config))

(defn load-global-config!
  "Load global config from disk, merge with defaults, cache in atom."
  ([]
   (load-global-config! config-path))
  ([path]
   (let [;; Try new path first, fall back to legacy
         user-config (or (read-config-file path)
                         (when (= path config-path)
                           (when-let [legacy (read-config-file legacy-config-path)]
                             (log/info "Migrating config: found legacy" legacy-config-path
                                       "-> please move to" config-path)
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
  "Return the cached global config, or defaults if not yet loaded."
  []
  (or @global-config default-config))

(defn get-project-roots
  "Return the :project-roots vector from global config."
  []
  (:project-roots (get-global-config)))

(defn get-defaults
  "Return the :defaults map from global config."
  []
  (:defaults (get-global-config)))

(defn get-project-overrides
  "Return overrides for a specific project-id."
  [project-id]
  (get-in (get-global-config) [:project-overrides project-id]))

(defn get-project-config
  "Return the effective config for a project-id."
  [project-id]
  (let [defaults (get-defaults)
        overrides (get-project-overrides project-id)]
    (if overrides
      (merge defaults overrides)
      defaults)))

(defn get-parent-rules
  "Return the :parent-rules vector from global config."
  []
  (:parent-rules (get-global-config)))

(defn get-parent-for-path
  "Resolve parent-id for a directory path via :parent-rules."
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
  "Return config map for a specific service."
  [service-key]
  (get-in (get-global-config) [:services service-key]))

(defn get-service-mode
  "Return the :mode for a service."
  [service-key]
  (get-in (get-global-config) [:services service-key :mode] :local))

(defn get-service-value
  "Get a field from service config with mode-aware resolution and env fallback."
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
  "Return a secret from config or env var fallback."
  [secret-key]
  (let [config-val (get-in (get-global-config) [:secrets secret-key])
        env-name (-> (name secret-key)
                     (str/replace "-" "_")
                     (str/upper-case))]
    (or config-val (System/getenv env-name))))

;; =============================================================================
;; Drone Defaults (Convenience Accessors)
;; =============================================================================

(defn default-drone-model
  "Resolve default drone model from config, env, or hardcoded fallback."
  []
  (get-service-value :drone :default-model
                     :env "DRONE_DEFAULT_MODEL"
                     :default "devstral-small:24b"))

(defn default-drone-backend
  "Resolve default drone backend from config, env, or hardcoded fallback."
  []
  (get-service-value :drone :default-backend
                     :env "DRONE_DEFAULT_BACKEND"
                     :default :openrouter))

;; =============================================================================
;; Dotted Key Path Access
;; =============================================================================

(defn parse-key-path
  "Parse a dotted key string into a keyword path vector."
  [key-str]
  (when (and key-str (not (str/blank? key-str)))
    (mapv keyword (str/split key-str #"\."))))

(defn get-config-value
  "Read a value at a dotted key path from the cached config."
  [key-str]
  (let [path (parse-key-path key-str)]
    (get-in (get-global-config) path)))

;; =============================================================================
;; Write Config to Disk
;; =============================================================================

(defn- write-config!
  "Write cached config to disk as EDN."
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

(defn set-config-value!
  "Update a value at a dotted key path and persist to disk."
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
