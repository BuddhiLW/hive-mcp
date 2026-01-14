(ns hive-mcp.agent.registry
  "Tool registry for agent delegation.
   
   Manages tool registration and schema retrieval.
   Tools are registered with :name, :description, :inputSchema, and :handler."
  (:require [taoensso.timbre :as log]))

;;; ============================================================
;;; Tool Registry
;;; ============================================================

(defonce registry (atom {}))

(declare register!)

(defonce ^:private initialized? (atom false))

(defn ensure-registered!
  "Lazily initialize tool registry on first use.
   This allows delegate! to work from REPL without manual registration."
  []
  (when (and (empty? @registry)
             (not @initialized?))
    (reset! initialized? true)
    (try
      (log/info "Auto-registering tools for agent delegation...")
      (require 'hive-mcp.tools)
      (let [tools-var (resolve 'hive-mcp.tools/tools)]
        (when tools-var
          (register! @tools-var)))
      (catch Exception e
        (log/warn "Failed to auto-register tools:" (ex-message e))))))

(defn register!
  "Register tools for agent use. Takes a seq of tool maps with :name and :handler."
  [tools]
  (doseq [{:keys [name handler] :as tool} tools]
    (swap! registry assoc name (assoc tool :handler handler)))
  (log/info "Registered" (count tools) "tools for agent delegation"))

(defn get-schemas
  "Get tool schemas for specified tool names (or all if nil)."
  [tool-names]
  (let [all-tools @registry
        selected (if tool-names
                   (select-keys all-tools tool-names)
                   all-tools)]
    (mapv #(dissoc % :handler) (vals selected))))

(defn get-tool
  "Get a tool by name from the registry."
  [tool-name]
  (get @registry tool-name))

(defn list-tools
  "List all registered tool names."
  []
  (keys @registry))
