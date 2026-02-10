(ns hive-mcp.agent.drone.tools
  "Drone tool selection — full hive capabilities for headless agents.

   Drones are headless agents with the same power as lings.
   They get full hive-mcp capabilities: memory, KG, CIDER, analysis, search.
   The sandbox layer (sandbox.clj + tool_allowlist.clj) handles security.

   CLARITY-I: Security is enforced at sandbox/allowlist layer, not here."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Tool Sets
;;; ============================================================

(def ^:private core-tools
  "Tools always included — drone lifecycle essentials."
  #{"propose_diff" "hivemind_shout"})

(def ^:private file-tools
  "File operations — read, write, search."
  #{"read_file" "file_write" "grep" "glob_files"})

(def ^:private clojure-tools
  "Clojure development — nREPL eval, docs, static analysis.
   Enables TDD: write code, eval it, fix mistakes."
  #{"cider_eval_silent" "cider_doc" "cider_info"
    "kondo_lint" "kondo_analyze" "clojure_eval"})

(def ^:private git-tools
  "Git inspection (read-only)."
  #{"magit_status" "magit_diff" "magit_log" "magit_branches"})

(def ^:private hive-tools
  "Hive-MCP capabilities — memory, KG, analysis.
   Makes drones comparable to lings for context-aware work."
  #{"memory_query" "memory_search" "memory_add"
    "kg_traverse" "kg_context" "kg_impact"
    "analysis_lint" "analysis_hotspots" "analysis_callers"
    "analysis_scc" "analysis_file" "analysis_compare"
    "bash"})

(def full-toolset
  "Full drone toolset — all hive capabilities.
   Drones are headless agents, not second-class citizens."
  (reduce into #{} [core-tools file-tools clojure-tools git-tools hive-tools]))

;;; ============================================================
;;; Tool Profiles
;;; ============================================================

(def tool-profiles
  "Task-specific tool sets. All profiles get full capabilities by default.
   Only :documentation is restricted (no mutation tools needed).

   Profiles exist for logging/metrics, not for restricting power."
  {:coding        full-toolset
   :testing       full-toolset
   :refactoring   full-toolset
   :bugfix        full-toolset
   :documentation (into core-tools file-tools)
   :general       full-toolset})

;; Backward compat alias
(def legacy-allowed-tools full-toolset)

;;; ============================================================
;;; Tool Selection
;;; ============================================================

(defn filter-tools-for-task
  "Select tools for task type. Returns full toolset for all task types
   except :documentation."
  [task-type]
  (let [tools (vec (get tool-profiles task-type full-toolset))]
    (log/debug "Drone tools:" {:task-type task-type :count (count tools)})
    tools))

;;; ============================================================
;;; File Scope Instructions
;;; ============================================================

(defn scope-file-access
  "Create file access restriction instructions for the drone."
  [allowed-files]
  (when (seq allowed-files)
    (str "\n## FILE ACCESS RESTRICTIONS\n"
         "You are ONLY permitted to read/modify these files:\n"
         (str/join "\n" (map #(str "- " %) allowed-files))
         "\n\nDo NOT attempt to read or modify any other files.\n"
         "If you need information from other files, report this limitation.\n\n")))

;;; ============================================================
;;; Public API
;;; ============================================================

(defn get-tools-for-drone
  "Get tools for a drone. All drones get full hive capabilities."
  [task-type files]
  (let [tools (filter-tools-for-task task-type)]
    (log/debug "Drone tool selection:"
               {:task-type task-type
                :tool-count (count tools)
                :file-count (count (or files []))})
    tools))

(defn tool-reduction-summary
  "Summary of tool selection for logging."
  [task-type]
  (let [selected (count (get tool-profiles task-type full-toolset))
        total (count full-toolset)]
    {:task-type task-type
     :selected-count selected
     :total-count total}))
