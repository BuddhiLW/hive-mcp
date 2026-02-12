(ns modex-bb.mcp.tools
  "Tool definition macros and utilities for Babashka MCP servers."
  (:require [modex-bb.mcp.schema :as schema]
            [clojure.string :as string]
            [modex-bb.mcp.json-rpc :as json-rpc]
            [modex-bb.mcp.log :as log]))

(defrecord Parameter [name doc type required default])

(defprotocol ITool
  (required-args [this])
  (input-schema [this]))

(defn tool-arg->property
  [^Parameter tool-arg]
  (select-keys tool-arg [:type :doc :required]))

(defn tool-args->input-schema [args]
  (into {}
        (for [tool-arg args]
          [(:name tool-arg) (tool-arg->property tool-arg)])))

(defrecord Tool [name doc args handler]
  ITool
  (required-args [this] (->> (filter :required args)
                             (mapv :name)))
  (input-schema [^Tool this]
    {:type       "object"
     :required   (required-args this)
     :properties (tool-args->input-schema args)}))

(defn missing-elements
  "Returns seq of elements in required that are not present in input."
  [required input]
  (remove (set input) (set required)))

(defn validate-arg-types
  "Validates argument types based on tool parameter definitions"
  [args arg-map]
  (let [type-errors (reduce (fn [errors arg]
                              (let [arg-name  (:name arg)
                                    arg-type  (:type arg)
                                    arg-value (get arg-map arg-name)]
                                (cond
                                  (nil? arg-value) errors
                                  (and (= :number arg-type) (not (number? arg-value)))
                                  (conj errors {:parameter arg-name
                                                :expected  :number
                                                :got       (type arg-value)})
                                  (and (= :string arg-type) (not (string? arg-value)))
                                  (conj errors {:parameter arg-name
                                                :expected  :string
                                                :got       (type arg-value)})
                                  (and (= :text arg-type) (not (string? arg-value)))
                                  (conj errors {:parameter arg-name
                                                :expected  :text
                                                :got       (type arg-value)})
                                  :else errors)))
                            [] args)]
    (when (seq type-errors)
      {:type-validation-errors type-errors})))

(defn missing-tool-args
  "Returns a seq of missing args or nil, for args with :required true."
  [tool-args arg-map]
  (let [required-args    (filter #(true? (:required %)) tool-args)
        required-key-set (set (map :name required-args))]
    (missing-elements required-key-set (keys arg-map))))

(defn invoke-handler
  "Invokes tool handler & arg-map.
   Returns result or throws on exception."
  [handler arg-map]
  (try
    (handler arg-map)
    (catch Exception ex
      (log/error "Tool handler exception:" (ex-message ex))
      (throw (ex-info (ex-message ex)
                      (assoc (ex-data ex) :cause :tool/exception))))))

(defn invoke-tool
  "1. Validates tool input parameters (missing args or wrong types)
   2. Calls invoke-tool-handler, gathers result
   3. Returns {:keys [success results errors]}"
  [^Tool {:as tool :keys [handler args]}
   arg-map]
  (let [required-args    (filter #(true? (:required %)) args)
        required-key-set (set (map :name required-args))
        missing-args     (missing-elements required-key-set (keys arg-map))
        type-errors      (when (empty? missing-args)
                           (validate-arg-types args arg-map))]
    (cond
      ;; Check for missing required arguments
      (seq missing-args)
      (throw (ex-info (str "Missing tool parameters: " (string/join ", " missing-args))
                      {:code          schema/error-invalid-params
                       :cause         :tool.exception/missing-parameters
                       :provided-args (keys arg-map)
                       :required-args required-key-set}))

      ;; Validate argument types (tool-level error)
      (seq type-errors)
      {:success false
       :errors   type-errors}

      ;; Validation passed, invoke handler:
      :else
      (try
        (let [results (invoke-handler handler arg-map)]
          {:success true
           :results results})
        (catch Exception ex
          (log/error "Exception during tool handler invocation for" (:name tool) ":" (ex-message ex))
          {:success false
           :errors  [(ex-message ex)]})))))

(def mcp-meta-keys [:type :doc :required])

(defn extract-mcp-meta
  "Extracts MCP tool metadata from a {:keys [...]} map."
  [m]
  {:pre [(map? m)]}
  (select-keys m mcp-meta-keys))

(defn argmap
  "Takes a {:keys [...]} argument map and extracts :type, :doc & :required to metadata."
  [m]
  {:pre [(map? m)]}
  (let [mcp-meta (select-keys m mcp-meta-keys)]
    (with-meta (apply dissoc m mcp-meta-keys) mcp-meta)))

(defmacro handler
  "Like fn but returns a fn with :mcp metadata for MCP Tool construction."
  [[map-arg] & body]
  {:pre [(map? map-arg)]}
  (let [mcp-meta     (extract-mcp-meta map-arg)
        let-map-arg# (apply dissoc map-arg mcp-meta-keys)
        map-sym#     (gensym "arg-map-")]
    `(do (assert (every? #{:string :number} (vals (:type '~mcp-meta))) ":type must be one of :number or :string.")
         (with-meta (fn [~map-sym#]
                      (let [~let-map-arg# ~map-sym#]
                        ~@body))
           {:mcp '~mcp-meta}))))

(defmacro tool-v1
  "Deprecated. Superseded by tool-v2-argmap, but still supported via tool."
  [[tool-name & tool-body]]
  (let [tool-key# (keyword tool-name)

        [docstring# rest-body#] (if (string? (first tool-body))
                                  [(first tool-body) (rest tool-body)]
                                  [(str tool-name) tool-body])

        args-vec# (first rest-body#)
        fn-body#  (rest rest-body#)

        arg-keywords# (mapv (fn [arg] `(keyword '~arg)) args-vec#)]

    `(let [arg-info# (vec (for [arg# '~args-vec#]
                            (let [m# (meta arg#)]
                              (map->Parameter
                               {:name     (keyword arg#)
                                :doc      (get m# :doc (str arg#))
                                :type     (get m# :type :string)
                                :required (get m# :required true)}))))

           v1-to-map-handler# (fn [arg-map#]
                                (let [arg-values# (map #(get arg-map# %) ~arg-keywords#)]
                                  (apply
                                   (fn ~args-vec# ~@fn-body#)
                                   arg-values#)))]
       (->Tool ~tool-key# ~docstring# arg-info# v1-to-map-handler#))))

(defmacro tool-v2-argmap
  "Primary tool definition macro using map destructuring."
  [[tool-name & tool-body]]
  (let [tool-key#  (keyword tool-name)

        [docstring# rest-body#] (if (string? (first tool-body))
                                  [(first tool-body) (rest tool-body)]
                                  [(str tool-name) tool-body])

        args-vec#  (first rest-body#)
        fn-body#   (rest rest-body#)

        args-map#  (first args-vec#)

        keys-vec#  (get args-map# :keys [])

        type-map#  (get args-map# :type {})
        doc-map#   (get args-map# :doc {})
        or-map#    (get args-map# :or {})
        or-keyset# (set (keys or-map#))]

    `(let [arg-info#   (vec (for [k# '~keys-vec#]
                              (map->Parameter
                               (cond-> {:name     (keyword k#)
                                        :doc      (get '~doc-map# k# (str k#))
                                        :type     (get '~type-map# k# :string)
                                        :required (let [has-default?# (get '~or-keyset# k#)]
                                                    (boolean (not has-default?#)))
                                        :default  (get '~or-map# k#)}))))

           handler-fn# (handler ~args-vec# ~@fn-body#)]
       (->Tool ~tool-key# ~docstring# arg-info# handler-fn#))))

(defmacro tool [[tool-name & tool-body]]
  (let [[_docstring# rest-body#] (if (string? (first tool-body))
                                   [(first tool-body) (rest tool-body)]
                                   [(str tool-name) tool-body])
        args# (first rest-body#)]
    (if (map? (first args#))
      `(tool-v2-argmap [~tool-name ~@tool-body])
      `(tool-v1 [~tool-name ~@tool-body]))))

(defmacro tools
  "Returns a map of tool name => Tool."
  [& tool-defs]
  `(let [tools#    (vector ~@(map (fn [tool-def] `(tool ~tool-def)) tool-defs))
         tool-map# (into {} (for [tool# tools#]
                              [(:name tool#) tool#]))]
     tool-map#))

(defmacro deftools
  "Calls tools and binds to a symbol via def."
  [name & tool-defs]
  `(def ~name (tools ~@tool-defs)))

(defn tool->json-schema
  "Builds MCP-compatible {:keys [name description inputSchema]}."
  [^Tool {:as tool :keys [name doc]}]
  {:name        name
   :description doc
   :inputSchema (input-schema tool)})
