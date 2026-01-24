(ns hive-mcp.workflows.router
  "Pure workflow routing decisions.
   
   Decides WHERE a workflow executes (native Clojure vs elisp).
   No side effects - just routing decisions.
   
   SOLID: Single Responsibility - routing logic only
   CLARITY: L - Layers stay pure (domain logic separate from I/O)
   CLARITY: R - Represented intent (clear naming)")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Configuration: which workflows have native Clojure implementations
(def ^:private native-workflows
  "Workflows that bypass elisp and use native Clojure implementation.
   These typically need direct access to Clojure-side resources (Chroma, DataScript)."
  #{"catchup" "wrap"})

(defn route-workflow
  "Determine where a workflow should execute.
   
   Returns:
   - :native  - Execute via native Clojure implementation
   - :elisp   - Execute via elisp (hive-mcp.el)
   
   Arguments:
   - workflow-name: String name of the workflow
   
   Examples:
   ```clojure
   (route-workflow \"catchup\")  ;; => :native
   (route-workflow \"wrap\")     ;; => :native
   ```"
  [workflow-name]
  (if (contains? native-workflows workflow-name)
    :native
    :elisp))

(defn native-workflow?
  "Predicate: does this workflow have a native implementation?"
  [workflow-name]
  (= :native (route-workflow workflow-name)))

(defn elisp-workflow?
  "Predicate: should this workflow go through elisp?"
  [workflow-name]
  (= :elisp (route-workflow workflow-name)))

(defn get-native-workflows
  "Return the set of workflows with native implementations."
  []
  native-workflows)
