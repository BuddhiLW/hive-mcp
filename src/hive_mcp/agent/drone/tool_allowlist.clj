(ns hive-mcp.agent.drone.tool-allowlist
  "Tool allowlist enforcement for drone execution."
  (:require [hive-mcp.agent.drone.tools :as drone-tools]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-allowlist
  "Default allowlist matching the full drone toolset."
  drone-tools/full-toolset)

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
    (do
      (log/debug "Using default tool allowlist" {:count (count default-allowlist)})
      default-allowlist)))

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

(defn enforce-allowlist
  "Enforce tool allowlist on a batch of tool calls."
  [tool-calls allowlist]
  (reduce
   (fn [acc {:keys [id name] :as call}]
     (if (tool-allowed? name allowlist)
       (update acc :allowed conj call)
       (update acc :rejected conj (reject-tool-call id name allowlist))))
   {:allowed [] :rejected []}
   tool-calls))
