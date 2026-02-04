(ns hive-mcp.migration.adapter
  "Protocol and registry for migration adapters.

   Adapters transform data for export/import to external systems:
   - Logseq (markdown + EDN properties)
   - Obsidian (markdown + YAML frontmatter)
   - Notion (JSON API format)
   - Custom (user-defined transforms)

   CLARITY-L: Clean protocol boundary for extensibility."
  (:require [clojure.walk :as walk]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Adapter Protocol
;; =============================================================================

(defprotocol IAdapter
  "Protocol for data transformation adapters."

  (adapter-id [this]
    "Return unique keyword identifier for this adapter.")

  (adapter-info [this]
    "Return map with :name, :description, :version, :formats.")

  (export-transform [this data opts]
    "Transform internal data format to external format.
     Returns transformed data structure.")

  (import-transform [this data opts]
    "Transform external format to internal data format.
     Returns data ready for import.")

  (validate-external [this data]
    "Validate external data before import.
     Returns {:valid? boolean, :errors [...]}"))

;; =============================================================================
;; Adapter Registry
;; =============================================================================

(defonce ^:private adapter-registry (atom {}))

(defn register-adapter!
  "Register an adapter implementation.

   Arguments:
     adapter - Implementation of IAdapter protocol

   Returns the adapter."
  [adapter]
  (let [id (adapter-id adapter)]
    (log/info "Registering migration adapter" {:id id})
    (swap! adapter-registry assoc id adapter)
    adapter))

(defn get-adapter
  "Get adapter by ID.

   Arguments:
     id - Adapter keyword (:logseq, :obsidian, :notion, etc.)

   Returns adapter or nil."
  [id]
  (get @adapter-registry id))

(defn list-adapters
  "List all registered adapters.

   Returns vector of {:id :name :description :formats}."
  []
  (->> @adapter-registry
       vals
       (mapv adapter-info)))

;; =============================================================================
;; Built-in Adapters
;; =============================================================================

;; Identity adapter (EDN passthrough)
(defrecord IdentityAdapter []
  IAdapter
  (adapter-id [_] :edn)
  (adapter-info [_]
    {:id :edn
     :name "EDN Passthrough"
     :description "Native EDN format, no transformation"
     :version "1.0.0"
     :formats #{:edn}})
  (export-transform [_ data _opts] data)
  (import-transform [_ data _opts] data)
  (validate-external [_ data]
    {:valid? (map? data)
     :errors (when-not (map? data) ["Expected map"])}))

;; JSON adapter (for interop)
(defrecord JsonAdapter []
  IAdapter
  (adapter-id [_] :json)
  (adapter-info [_]
    {:id :json
     :name "JSON Format"
     :description "JSON export/import with keyword conversion"
     :version "1.0.0"
     :formats #{:json}})

  (export-transform [_ data _opts]
    ;; Convert keywords to strings for JSON compat
    (walk/postwalk
     (fn [x]
       (cond
         (keyword? x) (str (namespace x) "/" (name x))
         :else x))
     data))

  (import-transform [_ data _opts]
    ;; Convert namespaced strings back to keywords
    (walk/postwalk
     (fn [x]
       (if (and (string? x) (re-matches #"\w+/\w+" x))
         (keyword x)
         x))
     data))

  (validate-external [_ data]
    {:valid? (or (map? data) (sequential? data))
     :errors (when-not (or (map? data) (sequential? data))
               ["Expected map or collection"])}))

;; Register built-in adapters
(register-adapter! (->IdentityAdapter))
(register-adapter! (->JsonAdapter))

;; =============================================================================
;; Adapter Helpers
;; =============================================================================

(defn transform-export
  "Apply adapter transformation for export.

   Arguments:
     adapter-id - Keyword (:edn, :json, :logseq, etc.)
     data       - Data to transform
     opts       - Adapter-specific options

   Returns transformed data or throws if adapter not found."
  [adapter-id data & [opts]]
  (if-let [adapter (get-adapter adapter-id)]
    (export-transform adapter data opts)
    (throw (ex-info "Unknown adapter" {:adapter adapter-id
                                       :available (keys @adapter-registry)}))))

(defn transform-import
  "Apply adapter transformation for import.

   Arguments:
     adapter-id - Keyword (:edn, :json, :logseq, etc.)
     data       - External data to transform
     opts       - Adapter-specific options

   Returns internal format data or throws if adapter not found."
  [adapter-id data & [opts]]
  (if-let [adapter (get-adapter adapter-id)]
    (import-transform adapter data opts)
    (throw (ex-info "Unknown adapter" {:adapter adapter-id
                                       :available (keys @adapter-registry)}))))
