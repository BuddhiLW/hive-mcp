(ns hive-mcp.docs
  "MCP tools for Emacs documentation lookup.
   Leverages hive-mcp-docs.el addon for introspection."
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.tools :refer [mcp-success mcp-error mcp-json]]
            [taoensso.timbre :as log]))

;;; Helpers

(defn docs-addon-available?
  "Check if hive-mcp-docs addon is loaded."
  []
  (try
    (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-docs)")]
      (and success (= "t" (clojure.string/trim (or result "")))))
    (catch Exception _ false)))

;;; MCP Tool Handlers

(defn handle-describe-function
  "Get documentation for an Emacs function."
  [{:keys [function_name functionName]}]
  (let [fname (or function_name functionName)]
    (if-not (docs-addon-available?)
      (mcp-error "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs.")
      (try
        (let [elisp (format "(json-encode (hive-mcp-docs-describe-function '%s))" fname)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            (mcp-success result)
            (mcp-error (str "Elisp error: " error))))
        (catch Exception e
          (mcp-error (str "Failed to describe function: " (.getMessage e))))))))

(defn handle-describe-variable
  "Get documentation for an Emacs variable."
  [{:keys [variable_name variableName]}]
  (let [vname (or variable_name variableName)]
    (if-not (docs-addon-available?)
      (mcp-error "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs.")
      (try
        (let [elisp (format "(json-encode (hive-mcp-docs-describe-variable '%s))" vname)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            (mcp-success result)
            (mcp-error (str "Elisp error: " error))))
        (catch Exception e
          (mcp-error (str "Failed to describe variable: " (.getMessage e))))))))

(defn handle-apropos
  "Search for Emacs symbols matching a pattern."
  [{:keys [pattern type]}]
  (if-not (docs-addon-available?)
    (mcp-error "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs.")
    (try
      (let [elisp (if type
                    (format "(json-encode (hive-mcp-docs-apropos \"%s\" \"%s\"))"
                            pattern type)
                    (format "(json-encode (hive-mcp-docs-apropos \"%s\"))"
                            pattern))
            {:keys [success result error]} (ec/eval-elisp elisp)]
        (if success
          (mcp-success result)
          (mcp-error (str "Elisp error: " error))))
      (catch Exception e
        (mcp-error (str "Failed apropos search: " (.getMessage e)))))))

(defn handle-package-functions
  "List all functions in a package/prefix."
  [{:keys [package_or_prefix packageOrPrefix]}]
  (let [prefix (or package_or_prefix packageOrPrefix)]
    (if-not (docs-addon-available?)
      (mcp-error "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs.")
      (try
        (let [elisp (format "(json-encode (hive-mcp-docs-package-functions \"%s\"))" prefix)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            (mcp-success result)
            (mcp-error (str "Elisp error: " error))))
        (catch Exception e
          (mcp-error (str "Failed to list package functions: " (.getMessage e))))))))

(defn handle-find-keybindings
  "Find keybindings for an Emacs command."
  [{:keys [command]}]
  (if-not (docs-addon-available?)
    (mcp-error "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs.")
    (try
      (let [elisp (format "(json-encode (hive-mcp-docs-find-keybindings '%s))"
                          command)
            {:keys [success result error]} (ec/eval-elisp elisp)]
        (if success
          (mcp-success result)
          (mcp-error (str "Elisp error: " error))))
      (catch Exception e
        (mcp-error (str "Failed to find keybindings: " (.getMessage e)))))))

(defn handle-package-commentary
  "Get the Commentary section from a package."
  [{:keys [package_name packageName]}]
  (let [pname (or package_name packageName)]
    (if-not (docs-addon-available?)
      (mcp-error "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs.")
      (try
        (let [elisp (format "(json-encode (hive-mcp-docs-package-commentary '%s))" pname)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            (mcp-success result)
            (mcp-error (str "Elisp error: " error))))
        (catch Exception e
          (mcp-error (str "Failed to get package commentary: " (.getMessage e))))))))

(defn handle-list-packages
  "List all loaded Emacs features/packages."
  [_]
  (if-not (docs-addon-available?)
    (mcp-error "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs.")
    (try
      (let [elisp "(json-encode (hive-mcp-docs-list-packages))"
            {:keys [success result error]} (ec/eval-elisp elisp)]
        (if success
          (mcp-success result)
          (mcp-error (str "Elisp error: " error))))
      (catch Exception e
        (mcp-error (str "Failed to list packages: " (.getMessage e)))))))

;;; Tool Definitions

(def docs-tools
  "Documentation MCP tools."
  [{:name "mcp_describe_function"
    :description "Get documentation for an Emacs function including signature, docstring, and source file location."
    :inputSchema {:type "object"
                  :properties {:function_name {:type "string"
                                               :description "Name of the function to describe"}}
                  :required ["function_name"]}
    :handler #'handle-describe-function}

   {:name "mcp_describe_variable"
    :description "Get documentation for an Emacs variable including current value, docstring, and file location."
    :inputSchema {:type "object"
                  :properties {:variable_name {:type "string"
                                               :description "Name of the variable to describe"}}
                  :required ["variable_name"]}
    :handler #'handle-describe-variable}

   {:name "mcp_apropos"
    :description "Search for Emacs symbols matching a pattern. Returns functions, variables, commands, or faces."
    :inputSchema {:type "object"
                  :properties {:pattern {:type "string"
                                         :description "Pattern to search for (regex supported)"}
                               :type {:type "string"
                                      :description "Filter by type: 'function', 'variable', 'command', 'face', or nil for all"
                                      :enum ["function" "variable" "command" "face"]}}
                  :required ["pattern"]}
    :handler #'handle-apropos}

   {:name "mcp_package_functions"
    :description "List all functions defined by a package or matching a prefix."
    :inputSchema {:type "object"
                  :properties {:package_or_prefix {:type "string"
                                                   :description "Package name or function prefix (e.g., 'org-', 'magit-')"}}
                  :required ["package_or_prefix"]}
    :handler #'handle-package-functions}

   {:name "mcp_find_keybindings"
    :description "Find all keybindings for an Emacs command."
    :inputSchema {:type "object"
                  :properties {:command {:type "string"
                                         :description "Name of the command to find keybindings for"}}
                  :required ["command"]}
    :handler #'handle-find-keybindings}

   {:name "mcp_package_commentary"
    :description "Get the Commentary section from a package's source file."
    :inputSchema {:type "object"
                  :properties {:package_name {:type "string"
                                              :description "Name of the package or feature"}}
                  :required ["package_name"]}
    :handler #'handle-package-commentary}

   {:name "mcp_list_packages"
    :description "List all loaded Emacs features (packages) with their source files."
    :inputSchema {:type "object"
                  :properties {}}
    :handler #'handle-list-packages}])
