(ns {{top/ns}}.{{main/ns}}.addon
  "{{description}}

   Implements IAddon protocol for the hive-mcp multiplexer.
   Discovered via META-INF/hive-addons/{{artifact/id}}.edn classpath manifest.

   Lifecycle:
     init-as-addon! → creates addon record → registered in addon-core
     initialize!    → start services, open connections
     shutdown!      → release resources, close connections

   See: hive-mcp.addons.protocol/IAddon"
  (:require [hive-mcp.addons.protocol :as proto]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Addon State
;; =============================================================================

(defonce ^:private addon-state (atom nil))

;; =============================================================================
;; Tool Definitions
;; =============================================================================
;;
;; Each tool-def is a map with :name :description :inputSchema :handler.
;; Tools are namespaced by the multiplexer at registration time
;; (e.g. "{{artifact/id}}:my-tool"). Return unnamespaced names here.

(defn- handle-hello
  "Example tool handler. Replace with your own logic.

   Params come as keyword maps (keywordized by make-tool middleware).
   Return {:type \"text\" :text \"...\"} for single-text responses."
  [{:keys [name]}]
  {:type "text"
   :text (str "Hello from {{artifact/id}}, " (or name "world") "!")})

(def ^:private tool-defs
  "MCP tool definitions contributed by this addon.
   Add your tools here — each will be auto-registered when the addon initializes."
  [{:name        "hello"
    :description "Greeting tool from {{artifact/id}}"
    :inputSchema {:type       "object"
                  :properties {"name" {:type        "string"
                                       :description "Name to greet"}}
                  :required   []}
    :handler     handle-hello}])

;; =============================================================================
;; IAddon Implementation
;; =============================================================================

(defrecord {{main/ns}}Addon [id config]
  proto/IAddon

  (addon-id [_] id)

  (addon-type [_] :native)

  (capabilities [_] #{:tools})

  (initialize! [_ cfg]
    (if @addon-state
      {:success? true :already-initialized? true}
      (try
        (log/info "Initializing addon" {:addon id})
        ;; TODO: Start your services, open connections here
        (reset! addon-state {:initialized-at (java.time.Instant/now)
                             :config         (merge config cfg)})
        {:success? true
         :errors   []
         :metadata {}}
        (catch Exception e
          (log/error e "Addon initialization failed" {:addon id})
          {:success? false
           :errors   [(.getMessage e)]}))))

  (shutdown! [_]
    (log/info "Shutting down addon" {:addon id})
    ;; TODO: Release resources, close connections here
    (reset! addon-state nil)
    {:success? true :errors []})

  (tools [_] tool-defs)

  (schema-extensions [_]
    ;; Return DataScript schema additions if your addon needs persistent entities.
    ;; Namespace with your addon prefix to avoid collisions.
    ;; Example:
    ;;   {:addon.{{artifact/id}}/doc-id {:db/valueType   :db.type/string
    ;;                                 :db/cardinality :db.cardinality/one}}
    {})

  (health [_]
    (if @addon-state
      {:status  :ok
       :details {:version    "0.1.0"
                 :uptime-ms  (- (System/currentTimeMillis)
                                (.toEpochMilli ^java.time.Instant
                                  (:initialized-at @addon-state)))}}
      {:status  :down
       :details {:version "0.1.0"}})))

;; =============================================================================
;; Constructor & Entry Point
;; =============================================================================

(defn ->addon
  "Create an addon instance.

   Arguments:
     id     - String identifier (e.g. \"{{artifact/id}}\")
     config - Optional config map (merged with manifest config)"
  ([]      (->addon "{{artifact/id}}" {}))
  ([id]    (->addon id {}))
  ([id config] (->{{main/ns}}Addon id config)))

(defn init-as-addon!
  "Entry point called by hive-mcp extension loader.

   Discovered via META-INF/hive-addons/{{artifact/id}}.edn manifest.
   Creates the addon record and returns it for registration.
   The loader then calls (register-addon!) and (init-addon!) on it."
  ([] (init-as-addon! {}))
  ([config]
   (let [addon (->addon "{{artifact/id}}" config)]
     (log/info "{{artifact/id}} addon created via init-as-addon!")
     addon)))

;; =============================================================================
;; REPL Development
;; =============================================================================

(comment
  ;; Create addon instance
  (def a (->addon))

  ;; Check protocol satisfaction
  (satisfies? proto/IAddon a)    ;=> true
  (proto/addon-id a)             ;=> "{{artifact/id}}"
  (proto/addon-type a)           ;=> :native
  (proto/capabilities a)         ;=> #{:tools}

  ;; Initialize
  (proto/initialize! a {})       ;=> {:success? true ...}

  ;; List tools
  (proto/tools a)                ;=> [{:name "hello" ...}]

  ;; Call a tool handler directly
  (handle-hello {:name "Hive"})  ;=> {:type "text" :text "Hello from ..."}

  ;; Health check
  (proto/health a)               ;=> {:status :ok :details {...}}

  ;; Shutdown
  (proto/shutdown! a)            ;=> {:success? true ...}

  ;; Full lifecycle with addon registry
  (require '[hive-mcp.addons.core :as addon-core])
  (addon-core/register-addon! a)
  (addon-core/init-addon! "{{artifact/id}}")
  (addon-core/active-addon-tools)
  (addon-core/shutdown-addon! "{{artifact/id}}")
  (addon-core/unregister-addon! "{{artifact/id}}")
  )
