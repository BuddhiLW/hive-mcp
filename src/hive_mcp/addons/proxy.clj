(ns hive-mcp.addons.proxy
  "Auto-generate proxy tool-defs from remote tool definitions with middleware.

   Decouples proxy generation from IMcpBridge protocol. Works with any
   call-fn: (fn [tool-name params] -> result-map).

   Two middleware modes:
   1. Declarative: {:middleware [:strip-params :stringify :logging :timeout]}
   2. Explicit:    {:middleware [(wrap-proxy-timeout 30000)]}

   Generated tool-defs flow through server.routes/make-tool like any native
   tool, getting the full hive-mcp middleware chain (piggyback, context,
   compress, etc.).

   Usage:
     ;; Generic call function
     (auto-proxy-tool-defs
       {:call-fn    (fn [tool-name params] (http-call url tool-name params))
        :prefix     \"my-api\"
        :tools      [{:name \"search\" :description \"...\" :inputSchema {...}}]
        :middleware  [:strip-params :stringify :logging]})

     ;; From an IMcpBridge instance
     (auto-proxy-tool-defs (from-bridge my-bridge))

     ;; Auto-register in extension registry
     (auto-register!
       {:call-fn call-fn :prefix \"svc\" :tools tools :middleware [:stringify]})

   See also:
   - hive-mcp.addons.mcp-bridge — IMcpBridge protocol + existing middleware fns
   - hive-mcp.addons.protocol  — IAddon multiplexer protocol
   - hive-mcp.extensions.registry — Dynamic tool registration"
  (:require [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.dns.result :as r]
            [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Base Handler Construction
;; =============================================================================

(defn make-call-handler
  "Create base proxy handler from a generic call function.

   call-fn: (fn [tool-name params] -> result-map)

   The call-fn should return MCP-compatible results:
     {:content [{:type \"text\" :text \"...\"}]}         — success
     {:isError true :content [{:type \"text\" :text ...}]} — error

   Raw strings and maps with :type are also accepted (auto-normalized).

   Returns: (fn [params] -> content-item)"
  [call-fn tool-name]
  {:pre [(ifn? call-fn) (string? tool-name)]}
  (fn [params]
    (let [effect (r/try-effect* :proxy/call-failed (call-fn tool-name params))]
      (if (r/err? effect)
        {:type "text"
         :text (str "Proxy call failed: " (:message effect))}
        (let [result (:ok effect)]
          (cond
            ;; Error response from remote
            (:isError result)
            {:type "text"
             :text (str "Remote tool error: "
                        (->> (:content result)
                             (keep :text)
                             (str/join "\n")))}

            ;; Multi-content response — return seq (normalize-content handles)
            (and (sequential? (:content result))
                 (> (count (:content result)) 1))
            (:content result)

            ;; Single content response
            (:content result)
            {:type "text"
             :text (or (-> (:content result) first :text)
                       (pr-str result))}

            ;; Raw string result
            (string? result)
            {:type "text" :text result}

            ;; Raw map with :type (already MCP content item)
            (and (map? result) (:type result))
            result

            ;; Fallback: pr-str anything else
            :else
            {:type "text" :text (pr-str result)}))))))

;; =============================================================================
;; Declarative Middleware Resolution
;; =============================================================================

(def ^:private middleware-factories
  "Registry mapping middleware keywords to factory functions.

   Each factory: (fn [opts tool-context] -> middleware-fn)
   middleware-fn: (fn [handler] -> handler')

   tool-context: {:prefix \"svc\" :tool-name \"search\" :tool-desc \"...\"}

   Factories receive the full opts map plus per-tool context, allowing
   middleware that depends on tool identity (e.g. logging)."
  {:strip-params
   (fn [opts _ctx]
     (bridge/wrap-proxy-strip-params
      (or (:strip-keys opts) bridge/default-strip-params)))

   :stringify
   (fn [_opts _ctx]
     (bridge/wrap-proxy-stringify-keys))

   :logging
   (fn [_opts ctx]
     (bridge/wrap-proxy-logging (:prefix ctx) (:tool-name ctx)))

   :timeout
   (fn [opts _ctx]
     (bridge/wrap-proxy-timeout (or (:timeout-ms opts) 30000)))

   :retry
   (fn [opts _ctx]
     (bridge/wrap-proxy-retry
      (or (:max-retries opts) 3)
      (or (:retry-delay-ms opts) 500)))

   :inject-params
   (fn [opts _ctx]
     (bridge/wrap-proxy-params (or (:extra-params opts) {})))})

(defn middleware-keys
  "Return set of available declarative middleware keywords."
  []
  (set (keys middleware-factories)))

(defn- resolve-one-middleware
  "Resolve a single middleware entry (keyword or fn) into a middleware fn.

   Keywords are looked up in middleware-factories.
   Functions are passed through.
   Anything else throws ex-info."
  [entry opts tool-context]
  (if-let [factory (when (keyword? entry)
                     (get middleware-factories entry))]
    (factory opts tool-context)
    (if (fn? entry)
      entry
      (throw (ex-info "Unknown middleware entry"
                      {:entry entry
                       :available (middleware-keys)})))))

(defn resolve-middleware-stack
  "Resolve a declarative middleware spec into a vector of middleware fns.

   spec: Vector of keyword or fn entries.
     [:strip-params :stringify :logging]
     [:strip-params (wrap-proxy-timeout 5000)]

   opts:         Full options map (factories may read :timeout-ms, etc.)
   tool-context: {:prefix \"svc\" :tool-name \"search\" :tool-desc \"...\"}

   Returns: Vector of (fn [handler] -> handler')"
  [spec opts tool-context]
  (mapv #(resolve-one-middleware % opts tool-context) spec))

;; =============================================================================
;; Proxy Tool-Def Generation
;; =============================================================================

(defn proxy-tool-def
  "Generate a proxy tool-def from a remote tool definition.

   Arguments:
     call-fn     - (fn [tool-name params] -> result-map)
     prefix      - String namespace prefix for tool isolation
     remote-tool - {:name :description :inputSchema}
     opts        - Options map:
       :middleware      - Vector of keyword/fn middleware entries
                          Default: [:strip-params :stringify]
       :skip-defaults?  - If true, skip default middleware (default: false)
       :timeout-ms      - Timeout for :timeout middleware (default: 30000)
       :max-retries     - Retries for :retry middleware (default: 3)
       :retry-delay-ms  - Delay between retries (default: 500)
       :strip-keys      - Set of keys to strip (default: bridge/default-strip-params)
       :extra-params    - Map of params to inject via :inject-params
       :name-xf         - Custom naming: (fn [name prefix] -> string)
       :desc-xf         - Custom description: (fn [desc prefix] -> string)
       :schema-xf       - Transform inputSchema: (fn [schema] -> schema')

   Returns:
     {:name         \"prefix:tool-name\"
      :description  \"desc [via prefix]\"
      :inputSchema  {...}
      :handler      <fn>
      :proxy-source prefix}"
  ([call-fn prefix remote-tool]
   (proxy-tool-def call-fn prefix remote-tool {}))
  ([call-fn prefix {:keys [name description inputSchema]} opts]
   {:pre [(ifn? call-fn) (string? prefix) (string? name)]}
   (let [namespaced-name (if-let [xf (:name-xf opts)]
                           (xf name prefix)
                           (str prefix ":" name))
         final-desc      (if-let [xf (:desc-xf opts)]
                           (xf description prefix)
                           (str (or description "") " [via " prefix "]"))
         base-schema     (or inputSchema {:type "object" :properties {}})
         final-schema    (if-let [xf (:schema-xf opts)]
                           (xf base-schema)
                           base-schema)
         ;; Build base handler
         base-handler    (make-call-handler call-fn name)
         ;; Build middleware stack (declarative + explicit)
         tool-ctx        {:prefix prefix :tool-name name :tool-desc description}
         default-mw      (when-not (:skip-defaults? opts)
                           [:stringify :strip-params])
         mw-spec         (into (vec default-mw) (or (:middleware opts) []))
         mw-fns          (when (seq mw-spec)
                           (resolve-middleware-stack mw-spec opts tool-ctx))
         final-handler   (if (seq mw-fns)
                           (bridge/compose-middleware base-handler mw-fns)
                           base-handler)]
     {:name         namespaced-name
      :description  final-desc
      :inputSchema  final-schema
      :handler      final-handler
      :proxy-source prefix})))

(defn auto-proxy-tool-defs
  "Auto-generate proxy tool-defs from a sequence of remote tool definitions.

   Accepts a config map:
     {:call-fn     (fn [tool-name params] -> result)     ;; required
      :prefix      \"service-name\"                       ;; required
      :tools       [{:name \"x\" :description \"...\" :inputSchema {...}}]
      :tool-filter (fn [tool-def] -> bool)               ;; optional
      :middleware   [:strip-params :stringify :logging]   ;; optional
      ...opts}

   Or positional args:
     (auto-proxy-tool-defs call-fn prefix tools)
     (auto-proxy-tool-defs call-fn prefix tools opts)

   Returns: Vector of tool-def maps."
  ([config]
   {:pre [(ifn? (:call-fn config))
          (string? (:prefix config))]}
   (let [{:keys [call-fn prefix tools tool-filter]} config
         opts     (dissoc config :call-fn :prefix :tools :tool-filter)
         filtered (cond->> tools tool-filter (filter tool-filter))]
     (mapv #(proxy-tool-def call-fn prefix % opts) filtered)))
  ([call-fn prefix tools]
   (auto-proxy-tool-defs {:call-fn call-fn :prefix prefix :tools tools}))
  ([call-fn prefix tools opts]
   (auto-proxy-tool-defs (merge opts {:call-fn call-fn :prefix prefix :tools tools}))))

;; =============================================================================
;; Bridge Adapter
;; =============================================================================

(defn from-bridge
  "Create auto-proxy config from an IMcpBridge instance.

   Discovers remote tools via list-remote-tools and builds a config map
   suitable for auto-proxy-tool-defs or auto-register!.

   Arguments:
     bridge - IMcpBridge implementation (must also satisfy IAddon)
     opts   - Additional options to merge into config

   Returns config map:
     {:call-fn fn :prefix str :tools [...] ...opts}"
  ([bridge]
   (from-bridge bridge {}))
  ([bridge opts]
   {:pre [(bridge/bridge? bridge)]}
   (let [name-kw (r/guard Exception :unknown
                          (if-let [addon-id-fn (requiring-resolve
                                                'hive-mcp.addons.protocol/addon-id)]
                            (addon-id-fn bridge)
                            :unknown))
         prefix  (clojure.core/name name-kw)
         tools   (let [t (r/guard Exception [] (bridge/list-remote-tools bridge))]
                   (when-let [err (:hive-dsl.result/error (meta t))]
                     (log/warn "Failed to discover remote tools from bridge"
                               {:bridge prefix :error (:message err)}))
                   t)
         call-fn (fn [tool-name params]
                   (bridge/call-tool bridge tool-name params))]
     (merge {:call-fn call-fn
             :prefix  prefix
             :tools   tools}
            opts))))

;; =============================================================================
;; Auto-Registration
;; =============================================================================

(defn auto-register!
  "Generate proxy tool-defs and register them in the extension registry.

   Accepts same arguments as auto-proxy-tool-defs.
   Each generated tool is registered via ext/register-tool!.

   Returns:
     {:registered [tool-name-strings]
      :count      N
      :prefix     prefix}"
  ([config]
   (let [tool-defs (auto-proxy-tool-defs config)
         names     (mapv (fn [td]
                           (ext/register-tool! td)
                           (:name td))
                         tool-defs)]
     (log/info "Auto-registered" (count names) "proxy tools"
               {:prefix (:prefix config) :tools names})
     {:registered names
      :count      (count names)
      :prefix     (:prefix config)}))
  ([call-fn prefix tools]
   (auto-register! {:call-fn call-fn :prefix prefix :tools tools}))
  ([call-fn prefix tools opts]
   (auto-register! (merge opts {:call-fn call-fn :prefix prefix :tools tools}))))

(defn deregister!
  "Deregister all proxy tools with the given prefix from extension registry.

   Removes all tools whose name starts with \"prefix:\".

   Returns: Vector of deregistered tool names."
  [prefix]
  {:pre [(string? prefix)]}
  (let [pattern   (str prefix ":")
        all-tools (ext/get-registered-tools)
        matching  (filterv #(str/starts-with? (:name %) pattern) all-tools)
        names     (mapv (fn [td]
                          (ext/deregister-tool! (:name td))
                          (:name td))
                        matching)]
    (when (seq names)
      (log/info "Deregistered" (count names) "proxy tools"
                {:prefix prefix :tools names}))
    names))

;; =============================================================================
;; Convenience: One-Shot Bridge Registration
;; =============================================================================

(defn register-bridge-tools!
  "Discover remote tools from a bridge and auto-register them.

   Combines from-bridge + auto-register! in one call.

   Arguments:
     bridge - IMcpBridge implementation
     opts   - Options for proxy generation:
       :middleware [:strip-params :stringify :logging :timeout]
       :timeout-ms 30000
       ...

   Returns:
     {:registered [tool-names] :count N :prefix prefix}"
  ([bridge]
   (register-bridge-tools! bridge {}))
  ([bridge opts]
   (auto-register! (from-bridge bridge opts))))

(comment
  ;; === Development REPL examples ===

  ;; 1. Generic call-fn (e.g. wrapping an HTTP API)
  (def my-tools
    [{:name "search"
      :description "Search documents"
      :inputSchema {:type "object"
                    :properties {"query" {:type "string"}}
                    :required ["query"]}}
     {:name "convert"
      :description "Convert document format"
      :inputSchema {:type "object"
                    :properties {"file" {:type "string"}
                                 "format" {:type "string"}}
                    :required ["file" "format"]}}])

  (def my-call-fn
    (fn [tool-name params]
      {:content [{:type "text" :text (str "Called " tool-name " with " (pr-str params))}]}))

  ;; Generate proxy defs
  (auto-proxy-tool-defs
   {:call-fn    my-call-fn
    :prefix     "my-api"
    :tools      my-tools
    :middleware [:strip-params :stringify :logging]})

  ;; Auto-register
  (auto-register!
   {:call-fn    my-call-fn
    :prefix     "my-api"
    :tools      my-tools
    :middleware [:stringify]})

  ;; Check registered tools
  (ext/get-registered-tools)

  ;; Deregister
  (deregister! "my-api")

  ;; 2. From an existing bridge
  ;; (register-bridge-tools! my-bridge {:middleware [:logging :timeout]})

  ;; 3. Available middleware keywords
  (middleware-keys)
  ;; => #{:strip-params :stringify :logging :timeout :retry :inject-params}
  )
