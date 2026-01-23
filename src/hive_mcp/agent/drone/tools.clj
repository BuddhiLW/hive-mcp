(ns hive-mcp.agent.drone.tools
  "Drone tool minimization - reduce attack surface by task type.

   Gives drones minimal tools for their specific task. Less is more.

   CLARITY-I: Inputs are guarded - minimize what drones can do
   CLARITY-Y: Yield safe failure - fewer tools = fewer failure modes"
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Tool Profiles - Minimal Attack Surface
;;; ============================================================

(def ^:private core-tools
  "Tools always included for basic drone operation (cannot be removed).
   These are the minimum tools every drone needs to function."
  #{"propose_diff" "hivemind_shout"})

(def tool-profiles
  "Task-specific tool sets for drones. Each profile contains the MINIMUM
   tools needed for that task type. Less tools = smaller attack surface.

   Profiles are additive: core-tools + profile tools.

   Task Types (aligned with preset.clj):
   - :testing       - Writing/running tests (needs eval for assertions)
   - :refactoring   - Code restructuring (needs search tools)
   - :bugfix        - Bug fixing (needs read + lint)
   - :documentation - Docs only (most restrictive)
   - :general       - Default file modifications"
  {:testing       #{:read_file :kondo_lint :cider_eval_silent}
   :refactoring   #{:read_file :grep :glob_files :kondo_lint :kondo_analyze}
   :bugfix        #{:read_file :kondo_lint :magit_diff}
   :documentation #{:read_file}
   :general       #{:read_file :kondo_lint}})

(def legacy-allowed-tools
  "Full tool set for backward compatibility when no task-type specified.
   Used as fallback - prefer explicit task-type for minimal attack surface.

   Note on Clojure tools:
   - cider_eval_silent: Fast nREPL evaluation (use for routine code inspection)
   - kondo_analyze/kondo_lint: Static analysis without nREPL (preferred for analysis)"
  #{"read_file" "grep" "glob_files"
    ;; Clojure tools (nREPL-based)
    "cider_eval_silent" "cider_doc" "cider_info"
    ;; Static analysis (no nREPL required)
    "kondo_lint" "kondo_analyze"
    ;; Git inspection
    "magit_status" "magit_diff" "magit_log" "magit_branches"
    ;; Drone-specific (core)
    "propose_diff" "hivemind_shout"})

;;; ============================================================
;;; Tool Filtering
;;; ============================================================

(defn- profile->tool-names
  "Convert a tool profile keyword to a set of tool name strings.
   Always includes core tools."
  [task-type]
  (let [profile-tools (get tool-profiles task-type #{})]
    (into core-tools (map name profile-tools))))

(defn filter-tools-for-task
  "Select minimal tool set based on task type.

   Arguments:
     task-type - Keyword from tool-profiles (:testing, :refactoring, etc.)
                 If nil, returns legacy full tool set for backward compat.

   Returns vector of tool name strings."
  [task-type]
  (if task-type
    (let [tools (vec (profile->tool-names task-type))]
      (log/info "Tool minimization:" {:task-type task-type
                                      :tool-count (count tools)
                                      :tools tools})
      tools)
    (do
      (log/debug "No task-type specified, using legacy tool set")
      (vec legacy-allowed-tools))))

;;; ============================================================
;;; File Scope Instructions
;;; ============================================================

(defn scope-file-access
  "Create file access restriction instructions for the drone.

   Instead of complex tool wrapping, we inject clear instructions
   that restrict which files the drone should access.

   Arguments:
     allowed-files - Set of files the drone is permitted to access

   Returns instruction string to prepend to task."
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
  "Get the minimal tool set for a drone based on task type.

   Arguments:
     task-type - Keyword (:testing, :refactoring, :bugfix, :documentation, :general)
                 Can be nil for backward compat (uses full legacy set)
     files     - Optional list of files (for logging/metrics)

   Returns vector of tool name strings.

   Example:
     (get-tools-for-drone :testing nil)
     ;; => [\"propose_diff\" \"hivemind_shout\" \"read_file\" \"kondo_lint\" \"cider_eval_silent\"]

     (get-tools-for-drone :documentation nil)
     ;; => [\"propose_diff\" \"hivemind_shout\" \"read_file\"]"
  [task-type files]
  (let [tools (filter-tools-for-task task-type)
        file-count (count (or files []))]
    (log/debug "Drone tool selection:"
               {:task-type task-type
                :tool-count (count tools)
                :file-count file-count})
    tools))

(defn tool-reduction-summary
  "Generate a summary of tool reduction for logging/metrics.

   Arguments:
     task-type - The task type used for tool selection

   Returns map with reduction statistics."
  [task-type]
  (let [minimized (count (profile->tool-names task-type))
        legacy (count legacy-allowed-tools)
        reduction-pct (if (pos? legacy)
                        (* 100 (/ (- legacy minimized) legacy))
                        0)]
    {:task-type task-type
     :minimized-count minimized
     :legacy-count legacy
     :reduction-pct (int reduction-pct)
     :tools-removed (- legacy minimized)}))
