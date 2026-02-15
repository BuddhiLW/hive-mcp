(ns hive-mcp.agent.drone.tool-allowlist
  "Tool allowlist enforcement for drone execution.
   Uses dynamic tool discovery from drone.tools (blacklist-based filtering)
   and adds bash command safety interception."
  (:require [hive-mcp.agent.drone.tools :as drone-tools]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn default-allowlist
  "Default allowlist matching the dynamically discovered drone toolset."
  []
  (drone-tools/full-toolset))

(defn resolve-allowlist
  "Resolve the effective allowlist for a drone execution."
  [{:keys [tool-allowlist task-type]}]
  (cond
    (seq tool-allowlist)
    (let [al (set tool-allowlist)]
      (log/debug "Using explicit tool allowlist" {:count (count al)})
      al)

    task-type
    (let [profile-tools (set (drone-tools/filter-tools-for-task task-type))]
      (log/debug "Using task-type allowlist" {:task-type task-type
                                              :count (count profile-tools)})
      profile-tools)

    :else
    (let [al (default-allowlist)]
      (log/debug "Using default tool allowlist" {:count (count al)})
      al)))

(defn tool-allowed?
  "Check if a tool is on the allowlist."
  [tool-name allowlist]
  (contains? allowlist tool-name))

(defn reject-tool-call
  "Create a rejection result for a disallowed tool call."
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

(defn- reject-bash-command
  "Create a rejection result for a blocked bash command."
  [call-id reason]
  {:role "tool"
   :tool_call_id call-id
   :name "bash"
   :content (str "Error: BASH COMMAND BLOCKED: " reason)})

(defn enforce-allowlist
  "Enforce tool allowlist on a batch of tool calls.
   Additionally intercepts bash commands against drone safety patterns."
  [tool-calls allowlist]
  (reduce
   (fn [acc {:keys [id name arguments] :as call}]
     (cond
       ;; Not on allowlist -> reject
       (not (tool-allowed? name allowlist))
       (update acc :rejected conj (reject-tool-call id name allowlist))

       ;; Bash tool -> additional command safety check
       (= name "bash")
       (let [command (or (get arguments "command") (get arguments :command) "")
             validation (drone-tools/validate-bash-command command)]
         (if (:allowed? validation)
           (update acc :allowed conj call)
           (do
             (log/warn "Bash command rejected by safety guard"
                       {:command (subs command 0 (min 80 (count command)))
                        :reason (:reason validation)})
             (update acc :rejected conj (reject-bash-command id (:reason validation))))))

       ;; All other allowed tools -> pass through
       :else
       (update acc :allowed conj call)))
   {:allowed [] :rejected []}
   tool-calls))
