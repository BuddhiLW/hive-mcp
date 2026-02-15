(ns hive-mcp.tools.docs
  "MCP tools for Emacs documentation lookup.
   Leverages hive-mcp-docs.el addon for introspection."
  (:require [clojure.string :as str]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dns.result :as result :refer [rescue]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Helpers

(defn docs-addon-available?
  "Check if hive-mcp-docs addon is loaded."
  []
  (rescue false
          (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-docs)")]
            (and success (= "t" (str/trim (or result "")))))))

(defn- eval-docs*
  "Evaluate docs elisp, returning Result. Checks addon availability first."
  [elisp-expr]
  (if-not (docs-addon-available?)
    (result/err :docs/addon-not-loaded
                {:message "hive-mcp-docs addon not loaded. Run (require 'hive-mcp-docs) in Emacs."})
    (let [{:keys [success result error]} (ec/eval-elisp elisp-expr)]
      (if success
        (result/ok result)
        (result/err :docs/elisp-error {:message (str "Elisp error: " error)})))))

;;; MCP Tool Handlers

(defn handle-describe-function
  "Get documentation for an Emacs function."
  [{:keys [function_name functionName]}]
  (rb/result->mcp-text
   (rb/try-result :docs/describe-function
                  #(eval-docs* (format "(json-encode (hive-mcp-docs-describe-function '%s))"
                                       (or function_name functionName))))))

(defn handle-describe-variable
  "Get documentation for an Emacs variable."
  [{:keys [variable_name variableName]}]
  (rb/result->mcp-text
   (rb/try-result :docs/describe-variable
                  #(eval-docs* (format "(json-encode (hive-mcp-docs-describe-variable '%s))"
                                       (or variable_name variableName))))))

(defn handle-apropos
  "Search for Emacs symbols matching a pattern."
  [{:keys [pattern type]}]
  (rb/result->mcp-text
   (rb/try-result :docs/apropos
                  #(eval-docs* (if type
                                 (format "(json-encode (hive-mcp-docs-apropos \"%s\" \"%s\"))"
                                         pattern type)
                                 (format "(json-encode (hive-mcp-docs-apropos \"%s\"))"
                                         pattern))))))

(defn handle-package-functions
  "List all functions in a package/prefix."
  [{:keys [package_or_prefix packageOrPrefix]}]
  (rb/result->mcp-text
   (rb/try-result :docs/package-functions
                  #(eval-docs* (format "(json-encode (hive-mcp-docs-package-functions \"%s\"))"
                                       (or package_or_prefix packageOrPrefix))))))

(defn handle-find-keybindings
  "Find keybindings for an Emacs command."
  [{:keys [command]}]
  (rb/result->mcp-text
   (rb/try-result :docs/find-keybindings
                  #(eval-docs* (format "(json-encode (hive-mcp-docs-find-keybindings '%s))" command)))))

(defn handle-package-commentary
  "Get the Commentary section from a package."
  [{:keys [package_name packageName]}]
  (rb/result->mcp-text
   (rb/try-result :docs/package-commentary
                  #(eval-docs* (format "(json-encode (hive-mcp-docs-package-commentary '%s))"
                                       (or package_name packageName))))))

(defn handle-list-packages
  "List all loaded Emacs features/packages."
  [_]
  (rb/result->mcp-text
   (rb/try-result :docs/list-packages
                  #(eval-docs* "(json-encode (hive-mcp-docs-list-packages))"))))

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
