(ns hive-mcp.addons.core
  "Plugin architecture for hive-mcp addons."
  (:require [clojure.set]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IAddon
  "Protocol for hive-mcp addons."

  (addon-name [this]
    "Return unique keyword identifier for this addon.")

  (addon-version [this]
    "Return semantic version string.")

  (addon-info [this]
    "Return metadata map about this addon.")

  (init! [this opts]
    "Initialize the addon with config options.")

  (shutdown! [this]
    "Shutdown the addon and release resources.")

  (addon-tools [this]
    "Return MCP tool definitions contributed by this addon.")

  (addon-capabilities [this]
    "Return set of capability keywords this addon provides."))

(defonce ^:private addon-registry (atom {}))

(defn register-addon!
  "Register an addon in the global registry."
  [addon]
  {:pre [(satisfies? IAddon addon)]}
  (let [name-kw (addon-name addon)]
    (if (contains? @addon-registry name-kw)
      (do
        (log/warn "Addon already registered" {:addon name-kw})
        {:success? false
         :addon-name name-kw
         :errors [(str "Addon " name-kw " is already registered")]})
      (do
        (swap! addon-registry assoc name-kw
               {:addon addon
                :state :registered
                :registered-at (java.time.Instant/now)
                :init-time nil
                :init-result nil})
        (log/info "Addon registered" {:addon name-kw
                                      :version (addon-version addon)
                                      :capabilities (addon-capabilities addon)})
        {:success? true
         :addon-name name-kw}))))

(defn get-addon
  "Get addon by name from the registry."
  [name-kw]
  (get-in @addon-registry [name-kw :addon]))

(defn get-addon-entry
  "Get full addon registry entry with state metadata."
  [name-kw]
  (get @addon-registry name-kw))

(defn addon-registered?
  "Check if an addon is registered."
  [name-kw]
  (contains? @addon-registry name-kw))

(defn list-addons
  "List all registered addons with their state."
  []
  (->> @addon-registry
       (mapv (fn [[name-kw {:keys [addon state registered-at init-time]}]]
               {:name name-kw
                :version (addon-version addon)
                :state state
                :registered-at registered-at
                :init-time init-time
                :capabilities (addon-capabilities addon)
                :info (addon-info addon)}))))

(defn unregister-addon!
  "Unregister an addon, calling shutdown! first if active."
  [name-kw]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (do
      (when (= state :active)
        (try
          (let [result (shutdown! addon)]
            (when-not (:success? result)
              (log/warn "Addon shutdown had errors during unregister"
                        {:addon name-kw :errors (:errors result)})))
          (catch Exception e
            (log/error e "Addon shutdown failed during unregister" {:addon name-kw}))))
      (swap! addon-registry dissoc name-kw)
      (log/info "Addon unregistered" {:addon name-kw})
      {:success? true :addon-name name-kw})
    (do
      (log/warn "Addon not found for unregister" {:addon name-kw})
      {:success? false
       :addon-name name-kw
       :errors [(str "Addon " name-kw " is not registered")]})))

(defn init-addon!
  "Initialize a registered addon."
  [name-kw & [opts]]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (if (= state :active)
      (do
        (log/info "Addon already active, skipping init" {:addon name-kw})
        {:success? true :addon-name name-kw :already-active? true})
      (try
        (let [start-time (System/nanoTime)
              result (init! addon (or opts {}))
              elapsed-ms (/ (- (System/nanoTime) start-time) 1e6)]
          (if (:success? result)
            (do
              (swap! addon-registry assoc-in [name-kw :state] :active)
              (swap! addon-registry assoc-in [name-kw :init-time]
                     (java.time.Instant/now))
              (swap! addon-registry assoc-in [name-kw :init-result] result)
              (log/info "Addon initialized" {:addon name-kw
                                             :elapsed-ms elapsed-ms})
              (assoc result :addon-name name-kw :elapsed-ms elapsed-ms))
            (do
              (swap! addon-registry assoc-in [name-kw :state] :error)
              (swap! addon-registry assoc-in [name-kw :init-result] result)
              (log/warn "Addon init failed" {:addon name-kw
                                             :errors (:errors result)})
              (assoc result :addon-name name-kw))))
        (catch Exception e
          (swap! addon-registry assoc-in [name-kw :state] :error)
          (log/error e "Addon init threw exception" {:addon name-kw})
          {:success? false
           :addon-name name-kw
           :errors [(.getMessage e)]})))
    {:success? false
     :addon-name name-kw
     :errors [(str "Addon " name-kw " is not registered")]}))

(defn shutdown-addon!
  "Shutdown an active addon."
  [name-kw]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (if (not= state :active)
      (do
        (log/info "Addon not active, skipping shutdown" {:addon name-kw :state state})
        {:success? true :addon-name name-kw :already-inactive? true})
      (try
        (let [result (shutdown! addon)]
          (swap! addon-registry assoc-in [name-kw :state] :registered)
          (swap! addon-registry assoc-in [name-kw :init-time] nil)
          (log/info "Addon shut down" {:addon name-kw})
          (assoc result :addon-name name-kw))
        (catch Exception e
          (swap! addon-registry assoc-in [name-kw :state] :error)
          (log/error e "Addon shutdown threw exception" {:addon name-kw})
          {:success? false
           :addon-name name-kw
           :errors [(.getMessage e)]})))
    {:success? false
     :addon-name name-kw
     :errors [(str "Addon " name-kw " is not registered")]}))

(defn init-all!
  "Initialize all registered addons that are not yet active."
  [& [opts]]
  (->> @addon-registry
       (filter (fn [[_name {:keys [state]}]] (not= state :active)))
       (map (fn [[name-kw _]]
              [name-kw (init-addon! name-kw opts)]))
       (into {})))

(defn shutdown-all!
  "Shutdown all active addons."
  []
  (->> @addon-registry
       (filter (fn [[_name {:keys [state]}]] (= state :active)))
       (map (fn [[name-kw _]]
              [name-kw (shutdown-addon! name-kw)]))
       (into {})))

(defn active-addon-tools
  "Get all MCP tools from active addons."
  []
  (->> @addon-registry
       (filter (fn [[_name {:keys [state addon]}]]
                 (and (= state :active)
                      (contains? (addon-capabilities addon) :tools))))
       (mapcat (fn [[name-kw {:keys [addon]}]]
                 (->> (addon-tools addon)
                      (map #(assoc % :addon-source name-kw)))))
       vec))

(defn addon-tools-by-name
  "Get MCP tools contributed by a specific addon."
  [name-kw]
  (if-let [{:keys [addon state]} (get-addon-entry name-kw)]
    (if (and (= state :active)
             (contains? (addon-capabilities addon) :tools))
      (vec (addon-tools addon))
      [])
    []))

(defn addons-with-capability
  "Find all active addons providing a specific capability."
  [capability]
  (->> @addon-registry
       (filter (fn [[_name {:keys [state addon]}]]
                 (and (= state :active)
                      (contains? (addon-capabilities addon) capability))))
       (mapv first)))

(defn all-capabilities
  "Get capability summary from all active addons."
  []
  (let [active (->> @addon-registry
                    (filter (fn [[_name {:keys [state]}]] (= state :active))))]
    (->> active
         (mapcat (fn [[name-kw {:keys [addon]}]]
                   (map (fn [cap] [cap name-kw]) (addon-capabilities addon))))
         (reduce (fn [acc [cap name-kw]]
                   (update acc cap (fnil conj []) name-kw))
                 {}))))

(defn check-dependencies
  "Check if all dependencies for an addon are satisfied."
  [name-kw]
  (if-let [addon (get-addon name-kw)]
    (let [deps (or (:dependencies (addon-info addon)) #{})
          available (set (keys @addon-registry))
          missing (clojure.set/difference deps available)]
      {:satisfied? (empty? missing)
       :missing missing
       :available (clojure.set/intersection deps available)})
    {:satisfied? false
     :missing #{name-kw}
     :available #{}}))

(defn reset-registry!
  "Reset the addon registry, shutting down active addons first."
  []
  (let [shutdown-results (shutdown-all!)]
    (reset! addon-registry {})
    (log/info "Addon registry reset")
    shutdown-results))

(defn registry-status
  "Get comprehensive status of the addon registry."
  []
  (let [entries (vals @addon-registry)
        by-state (group-by :state entries)]
    {:total (count entries)
     :active (count (:active by-state))
     :registered (count (:registered by-state))
     :error (count (:error by-state))
     :tool-count (count (active-addon-tools))
     :capabilities (all-capabilities)
     :addons (list-addons)}))

(defrecord NoopAddon [id version-str]
  IAddon

  (addon-name [_] id)

  (addon-version [_] version-str)

  (addon-info [_]
    {:name id
     :version version-str
     :description "No-operation addon for testing and development"
     :author "hive-mcp"
     :license "AGPL-3.0-or-later"
     :dependencies #{}
     :capabilities #{}})

  (init! [_ _opts]
    {:success? true
     :errors []
     :metadata {:noop true}})

  (shutdown! [_]
    {:success? true
     :errors []})

  (addon-tools [_]
    [])

  (addon-capabilities [_]
    #{}))

(defn ->noop-addon
  "Create a NoopAddon for testing."
  ([] (->noop-addon :noop "0.0.0"))
  ([id] (->noop-addon id "0.0.0"))
  ([id version] (->NoopAddon id version)))

(defrecord ExampleAddon [id state]
  IAddon

  (addon-name [_] id)

  (addon-version [_] "1.0.0")

  (addon-info [_]
    {:name id
     :version "1.0.0"
     :description "Example addon demonstrating tool contribution"
     :author "hive-mcp"
     :license "AGPL-3.0-or-later"
     :dependencies #{}
     :capabilities #{:tools}})

  (init! [_ opts]
    (reset! state {:initialized true :config (:config opts)})
    {:success? true
     :errors []
     :metadata {:config-keys (keys (:config opts))}})

  (shutdown! [_]
    (reset! state nil)
    {:success? true
     :errors []})

  (addon-tools [_]
    [{:name (str (name id) "_ping")
      :description (str "Ping tool from " (name id) " addon")
      :inputSchema {:type "object"
                    :properties {"message" {:type "string"
                                            :description "Message to echo"}}
                    :required ["message"]}
      :handler (fn [{:keys [message]}]
                 {:type "text"
                  :text (str "pong: " message " (from " (name id) ")")})}])

  (addon-capabilities [_]
    #{:tools}))

(defn ->example-addon
  "Create an ExampleAddon for testing."
  ([] (->example-addon :example))
  ([id] (->ExampleAddon id (atom nil))))
