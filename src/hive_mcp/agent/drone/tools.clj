(ns hive-mcp.agent.drone.tools
  "Drone tool selection for headless agents with full hive capabilities."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private core-tools
  "Tools always included for drone lifecycle."
  #{"propose_diff" "hivemind_shout"})

(def ^:private file-tools
  "File operations."
  #{"read_file" "file_write" "grep" "glob_files"})

(def ^:private clojure-tools
  "Clojure development tools."
  #{"cider_eval_silent" "cider_doc" "cider_info"
    "kondo_lint" "kondo_analyze" "clojure_eval"})

(def ^:private git-tools
  "Git inspection (read-only)."
  #{"magit_status" "magit_diff" "magit_log" "magit_branches"})

(def ^:private hive-tools
  "Hive-MCP capabilities."
  #{"memory_query" "memory_search" "memory_add"
    "kg_traverse" "kg_context" "kg_impact"
    "analysis_lint" "analysis_hotspots" "analysis_callers"
    "analysis_scc" "analysis_file" "analysis_compare"
    "bash"})

(def full-toolset
  "Full drone toolset with all hive capabilities."
  (reduce into #{} [core-tools file-tools clojure-tools git-tools hive-tools]))

(def tool-profiles
  "Task-specific tool sets."
  {:coding        full-toolset
   :testing       full-toolset
   :refactoring   full-toolset
   :bugfix        full-toolset
   :documentation (into core-tools file-tools)
   :general       full-toolset})

;; Backward compat alias
(def legacy-allowed-tools full-toolset)

(defn filter-tools-for-task
  "Select tools for task type."
  [task-type]
  (let [tools (vec (get tool-profiles task-type full-toolset))]
    (log/debug "Drone tools:" {:task-type task-type :count (count tools)})
    tools))

(defn scope-file-access
  "Create file access restriction instructions for the drone."
  [allowed-files]
  (when (seq allowed-files)
    (str "\n## FILE ACCESS RESTRICTIONS\n"
         "You are ONLY permitted to read/modify these files:\n"
         (str/join "\n" (map #(str "- " %) allowed-files))
         "\n\nDo NOT attempt to read or modify any other files.\n"
         "If you need information from other files, report this limitation.\n\n")))

(defn get-tools-for-drone
  "Get tools for a drone."
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
