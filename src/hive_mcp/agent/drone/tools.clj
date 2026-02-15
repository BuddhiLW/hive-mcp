(ns hive-mcp.agent.drone.tools
  "Drone tool selection with dynamic discovery and blacklist-based safety filtering.

   Drones pull available tools from the agent registry at spawn time,
   filtered through a blacklist that prevents destructive/recursive operations.
   This gives drones access to addon-contributed tools (kondo, scc, lsp, etc.)
   automatically without manual allowlist updates."
  (:require [clojure.string :as str]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Blacklist Definitions
;; =============================================================================

(def ^:private blacklisted-tools
  "Consolidated tool names that drones MUST NOT have access to.
   Prevents recursive spawning, resource amplification, and destructive ops."
  #{"agent"      ;; spawns/kills lings and drones -> recursive self-call
    "wave"       ;; dispatches drone waves -> resource amplification
    "workflow"   ;; forge-strike spawns lings -> recursive self-call
    "multi"      ;; meta-facade routes to excluded tools -> bypass vector
    "delegate"   ;; drone delegation -> resource amplification
    "olympus"    ;; Emacs grid control -> coordinator-only
    "emacs"      ;; direct Emacs operations -> coordinator-only
    "migration"  ;; KG/memory backend migration -> destructive
    "config"})   ;; system config mutation -> coordinator-only

(def ^:private blacklisted-tool-names
  "Specific tool names (non-consolidated) that drones must not access."
  #{"magit_commit"            ;; git history modification
    "magit_push"              ;; remote state change
    "preset_delete"           ;; config removal
    "session_complete"        ;; session lifecycle -> coordinator
    "delegate_drone"          ;; deprecated but still callable
    "dispatch_validated_wave" ;; wave dispatch
    "swarm_spawn"             ;; agent spawn (deprecated)
    "swarm_kill"              ;; agent kill (deprecated)
    "swarm_dispatch"})        ;; agent dispatch (deprecated)

(def ^:private blacklisted-prefixes
  "Name prefixes that map to dangerous deprecated shims."
  #{"swarm_" "delegate_" "lings_"})

;; =============================================================================
;; Bash Safety Guard
;; =============================================================================

(def ^:private bash-blocked-patterns
  "Regex patterns that drones are not allowed to execute via bash."
  [#"rm\s+-rf\s+/"          ;; rm -rf /
   #"rm\s+-rf\s+\."         ;; rm -rf . (current dir nuke)
   #"rm\s+.*\.git"          ;; deleting .git
   #"git\s+push"            ;; git push (use magit which is blacklisted)
   #"git\s+reset\s+--hard"  ;; destructive git reset
   #"chmod\s+.*777"         ;; world-writable perms
   #">\s*/dev/sd"           ;; writing to block devices
   #"mkfs\."                ;; formatting filesystems
   #"dd\s+if="              ;; raw disk operations
   #":\(\)\s*\{"            ;; fork bomb pattern
   #"curl.*\|\s*sh"         ;; pipe-to-shell
   #"wget.*\|\s*sh"])        ;; pipe-to-shell

(defn validate-bash-command
  "Check a bash command against drone safety patterns.
   Returns {:allowed? bool :reason string}."
  [command]
  (if-let [blocked (first (filter #(re-find % command) bash-blocked-patterns))]
    {:allowed? false
     :reason (str "Drone bash command blocked by safety pattern: " blocked)}
    {:allowed? true}))

;; =============================================================================
;; Dynamic Tool Discovery
;; =============================================================================

(defonce ^:private discovered-tools (atom nil))

(def ^:private drone-extra-tools
  "Tools available to drones that are not in the agent/extension registries.
   bash is provided by the drone tool proxy, not registered in any tool registry."
  #{"bash"})

(defn discover-drone-tools
  "Discover available tools for drone from agent registry + extension registry.
   Returns set of tool-name strings, filtered through blacklist.

   Sources:
   1. Agent registry (base + consolidated + compat shim tools)
   2. Extensions registry (addon-contributed tools: kondo, scc, lsp, file ops)
   3. drone-extra-tools (bash — provided by drone tool proxy)

   Then removes:
   1. Consolidated tools on the blacklist (agent, wave, workflow, etc.)
   2. Specific tool names on the blacklist (magit_commit, swarm_spawn, etc.)
   3. Deprecated shim prefixes (swarm_*, delegate_*, lings_*)"
  []
  (registry/ensure-registered!)
  (let [agent-tools   (registry/list-tools)
        ext-tools     (map :name (ext/get-registered-tools))
        all-tools     (concat agent-tools ext-tools drone-extra-tools)
        filtered      (->> all-tools
                           (remove #(or (contains? blacklisted-tools %)
                                        (contains? blacklisted-tool-names %)
                                        (some (fn [prefix]
                                                (str/starts-with? % prefix))
                                              blacklisted-prefixes))))]
    (set filtered)))

(defn full-toolset
  "Full drone toolset with dynamic discovery and blacklist filtering.
   Cached after first call; use invalidate-tool-cache! to refresh."
  []
  (or @discovered-tools
      (let [tools (discover-drone-tools)]
        (reset! discovered-tools tools)
        (log/info "Drone toolset discovered:" (count tools) "tools"
                  "(blacklisted:" (- (count (registry/list-tools)) (count tools)) ")")
        tools)))

(defn invalidate-tool-cache!
  "Clear the cached drone toolset. Next call to full-toolset will re-discover."
  []
  (reset! discovered-tools nil)
  (log/info "Drone tool cache invalidated"))

;; =============================================================================
;; Task-Specific Tool Profiles
;; =============================================================================

(def ^:private documentation-only-tools
  "Minimal tools for documentation-only tasks."
  #{"propose_diff" "hivemind_shout" "read_file" "file_write" "grep" "glob_files"})

(defn- get-tool-profiles
  "Task-specific tool sets. Full toolset for most tasks, minimal for docs."
  []
  (let [ts (full-toolset)]
    {:coding        ts
     :testing       ts
     :refactoring   ts
     :bugfix        ts
     :documentation documentation-only-tools
     :general       ts}))

;; Backward compat alias — callers should use (full-toolset) instead
(def legacy-allowed-tools
  "Backward compat alias. Use (full-toolset) for the dynamic set."
  full-toolset)

(defn filter-tools-for-task
  "Select tools for task type."
  [task-type]
  (let [profiles (get-tool-profiles)
        tools (vec (get profiles task-type (full-toolset)))]
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
  (let [profiles (get-tool-profiles)
        selected (count (get profiles task-type (full-toolset)))
        total (count (full-toolset))]
    {:task-type task-type
     :selected-count selected
     :total-count total}))
