(ns hive-mcp.agent.drone.tool-allowlist
  "Tool allowlist enforcement for drone execution.

   Provides a configurable allowlist mechanism that rejects tool calls
   not on the allowed list. Works with executor.clj to enforce at
   execution time, not just tool selection time.

   Three resolution levels:
   1. Explicit :tool-allowlist (per-drone/per-wave override)
   2. Task-type based profile (from drone-tools)
   3. Default allowlist (safety net)

   CLARITY-I: Inputs guarded at tool execution boundary.
   CLARITY-Y: Clear error messages on rejection."
  (:require [hive-mcp.agent.drone.tools :as drone-tools]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Default Allowlist
;;; ============================================================

(def default-allowlist
  "Default set of tools that drones are allowed to call.
   This is the safety net — even if tool selection is broader,
   the executor will reject anything not on this list.

   Includes:
   - File read/write (core operations)
   - Search (grep, glob)
   - Shell (bash — further restricted by sandbox)
   - Clojure eval (read-only, silent mode)
   - Diff proposal (safe mutation path)
   - Static analysis (lint, analyze)
   - Status reporting (hivemind shout)"
  #{"read_file"
    "file_write"
    "grep"
    "glob_files"
    "bash"
    "clojure_eval"
    "cider_eval_silent"
    "cider_doc"
    "cider_info"
    ;; Drone-specific core tools
    "propose_diff"
    "hivemind_shout"
    ;; Static analysis
    "kondo_lint"
    "kondo_analyze"
    ;; Git inspection (read-only)
    "magit_status"
    "magit_diff"
    "magit_log"
    "magit_branches"})

;;; ============================================================
;;; Allowlist Resolution
;;; ============================================================

(defn resolve-allowlist
  "Resolve the effective allowlist for a drone execution.

   Priority (highest to lowest):
   1. Explicit :tool-allowlist in options (per-drone/per-wave override)
   2. Task-type based profile from drone-tools
   3. Default allowlist

   Arguments:
     opts - Map with optional keys:
       :tool-allowlist - Explicit set/coll of allowed tool names (overrides all)
       :task-type      - Task type keyword for profile-based allowlist

   Returns:
     Set of allowed tool name strings."
  [{:keys [tool-allowlist task-type]}]
  (cond
    ;; 1. Explicit override takes priority
    (seq tool-allowlist)
    (let [al (set tool-allowlist)]
      (log/debug "Using explicit tool allowlist" {:count (count al)})
      al)

    ;; 2. Task-type profile
    task-type
    (let [profile-tools (set (drone-tools/filter-tools-for-task task-type))]
      (log/debug "Using task-type allowlist" {:task-type task-type
                                               :count (count profile-tools)})
      profile-tools)

    ;; 3. Default
    :else
    (do
      (log/debug "Using default tool allowlist" {:count (count default-allowlist)})
      default-allowlist)))

;;; ============================================================
;;; Enforcement
;;; ============================================================

(defn tool-allowed?
  "Check if a tool is on the allowlist.

   Arguments:
     tool-name - String name of the tool
     allowlist - Set of allowed tool name strings

   Returns:
     true if allowed, false if rejected."
  [tool-name allowlist]
  (contains? allowlist tool-name))

(defn reject-tool-call
  "Create a rejection result for a disallowed tool call.

   Arguments:
     call-id   - The tool call ID
     tool-name - The rejected tool name
     allowlist - The active allowlist (for error message)

   Returns:
     Tool result message map indicating rejection."
  [call-id tool-name allowlist]
  (let [msg (str "TOOL REJECTED: '" tool-name "' is not on the drone's tool allowlist. "
                 "Allowed tools: " (pr-str (sort allowlist)) ". "
                 "If you need this tool, ask the parent ling to grant access.")]
    (log/warn "Tool call rejected by allowlist"
              {:tool tool-name
               :allowlist-count (count allowlist)})
    {:role "tool"
     :tool_call_id call-id
     :name tool-name
     :content (str "Error: " msg)}))

(defn enforce-allowlist
  "Enforce tool allowlist on a batch of tool calls.

   Partitions calls into allowed and rejected. Returns a map with:
   - :allowed   - Vector of tool calls that pass the allowlist
   - :rejected  - Vector of pre-formatted rejection results

   Arguments:
     tool-calls - Sequence of tool call maps with :id, :name, :arguments
     allowlist  - Set of allowed tool name strings

   Returns:
     {:allowed [...] :rejected [...]}"
  [tool-calls allowlist]
  (reduce
   (fn [acc {:keys [id name] :as call}]
     (if (tool-allowed? name allowlist)
       (update acc :allowed conj call)
       (update acc :rejected conj (reject-tool-call id name allowlist))))
   {:allowed [] :rejected []}
   tool-calls))
