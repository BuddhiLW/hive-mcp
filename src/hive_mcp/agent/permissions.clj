(ns hive-mcp.agent.permissions
  "Permission policies and enforcement for agent tool calls."
  (:require [hive-mcp.agent.drone.tool-allowlist :as allowlist]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.protocols.agent-bridge :as bridge]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def tool-categories
  "Map of tool names to their permission category."
  {;; Read-only tools
   "read_file"      :read-only
   "grep"           :read-only
   "glob_files"     :read-only
   "cider_doc"      :read-only
   "cider_info"     :read-only
   "cider_complete" :read-only
   "magit_status"   :read-only
   "magit_diff"     :read-only
   "magit_log"      :read-only
   "magit_branches" :read-only
   "kondo_lint"     :read-only
   "kondo_analyze"  :read-only
   "scc"            :read-only

   ;; Safe write tools
   "file_write"     :safe-write
   "propose_diff"   :safe-write
   "magit_stage"    :safe-write
   "magit_commit"   :safe-write

   ;; Destructive tools
   "bash"           :destructive
   "magit_push"     :destructive
   "magit_pull"     :destructive
   "magit_fetch"    :destructive

   ;; Coordination tools
   "hivemind_shout" :coordination
   "memory"         :coordination
   "kanban"         :coordination
   "kg"             :coordination
   "session"        :coordination

   ;; Eval tools
   "clojure_eval"      :eval
   "cider_eval_silent" :eval
   "cider_eval"        :eval})

(defn categorize-tool
  "Return the permission category for a tool name, or :unknown."
  [tool-name]
  (get tool-categories tool-name :unknown))

(def permission-levels
  "Permission level definitions mapping tool categories to actions (:allow, :deny, :prompt)."
  {:allow-all
   {:read-only    :allow
    :safe-write   :allow
    :destructive  :allow
    :coordination :allow
    :eval         :allow
    :unknown      :allow}

   :allow-safe
   {:read-only    :allow
    :safe-write   :allow
    :destructive  :prompt
    :coordination :allow
    :eval         :allow
    :unknown      :prompt}

   :prompt-user
   {:read-only    :allow
    :safe-write   :prompt
    :destructive  :prompt
    :coordination :allow
    :eval         :prompt
    :unknown      :prompt}

   :deny-all
   {:read-only    :deny
    :safe-write   :deny
    :destructive  :deny
    :coordination :deny
    :eval         :deny
    :unknown      :deny}})

(def ^:private valid-levels (set (keys permission-levels)))

(defn valid-permission-level?
  "Check if a keyword is a recognized permission level."
  [level]
  (contains? valid-levels level))

(defn- category-action
  "Resolve the action for a tool category under a permission level."
  [level category]
  (get-in permission-levels [level category] :deny))

(defn check-permission
  "Check whether a tool call is permitted under the given policy."
  [{:keys [level handler-fn]} tool-name input context]
  (let [category (categorize-tool tool-name)
        action   (category-action level category)]
    (case action
      :allow
      (do (log/trace "Permission granted" {:tool tool-name :category category :level level})
          {:action :allow})

      :deny
      (let [msg (str "Permission denied: tool '" tool-name "' (category: " (name category)
                     ") not allowed under policy '" (name level) "'")]
        (log/info "Permission denied" {:tool tool-name :category category :level level})
        {:action :deny :message msg})

      :prompt
      (if handler-fn
        (try
          (let [result (handler-fn tool-name input context)]
            (log/debug "Permission handler result" {:tool tool-name :result result})
            result)
          (catch Exception e
            (log/warn "Permission handler threw exception, denying" {:tool tool-name :error (.getMessage e)})
            {:action :deny
             :message (str "Permission handler error: " (.getMessage e))}))
        (do
          (log/info "Permission prompt with no handler, denying" {:tool tool-name :category category})
          {:action :deny
           :message (str "Tool '" tool-name "' requires approval but no permission handler is configured")})))))

(defn ->policy
  "Create a permission policy map from a level and optional handler-fn."
  ([level]
   (->policy level nil))
  ([level handler-fn]
   {:pre [(valid-permission-level? level)]}
   {:level      level
    :handler-fn handler-fn}))

(def default-policy
  "Default permission policy: allow-safe (read + safe writes permitted)."
  (->policy :allow-safe))

(defn logging-handler
  "Create a permission handler that logs and auto-allows all prompted tools."
  []
  (fn [tool-name _input context]
    (log/info "Permission prompt (auto-allowed)"
              {:tool tool-name
               :agent-id (:agent-id context)})
    {:action :allow}))

(defn allowlist-handler
  "Create a permission handler that checks against a tool allowlist."
  [allowlist-opts]
  (let [al (allowlist/resolve-allowlist allowlist-opts)]
    (fn [tool-name _input _context]
      (if (allowlist/tool-allowed? tool-name al)
        {:action :allow}
        {:action :deny
         :message (str "Tool '" tool-name "' not on allowlist. "
                       "Allowed: " (pr-str (sort al)))}))))

(defn callback-handler
  "Create a permission handler that invokes a callback returning boolean or result map."
  [callback-fn]
  (fn [tool-name input context]
    (let [result (callback-fn tool-name input context)]
      (cond
        (true? result)  {:action :allow}
        (false? result) {:action :deny :message (str "Callback denied tool: " tool-name)}
        (map? result)   result
        :else           {:action :deny :message (str "Unexpected callback result for: " tool-name)}))))

(defn composite-handler
  "Create a handler that chains multiple handlers; first non-allow result wins."
  [handlers]
  (fn [tool-name input context]
    (reduce
     (fn [_acc handler]
       (let [result (handler tool-name input context)]
         (if (= :allow (:action result))
           result
           (reduced result))))
     {:action :allow}
     handlers)))

(defn mode->policy
  "Convert IAgentPermissions mode keyword to an internal policy."
  [mode]
  (case mode
    :default      (->policy :prompt-user)
    :accept-edits (->policy :allow-safe)
    :bypass       (->policy :allow-all)
    (do (log/warn "Unknown permission mode, using default" {:mode mode})
        (->policy :prompt-user))))

(defn mode->sdk-string
  "Convert internal permission mode keyword to Claude SDK string value."
  [mode]
  (case mode
    :default      "default"
    :accept-edits "acceptEdits"
    :bypass       "bypassPermissions"
    (do (log/warn "Unknown mode for SDK string, using default" {:mode mode})
        "default")))

(defn ling-policy
  "Permission policy for ling agents (allow-safe)."
  []
  (->policy :allow-safe))

(defn drone-policy
  "Permission policy for drone agents with allowlist enforcement."
  [opts]
  (->policy :allow-safe (allowlist-handler opts)))

(defn coordinator-policy
  "Permission policy for the coordinator (allow-all)."
  []
  (->policy :allow-all))

(defn- resolve-budget-guardrail-handler
  "Dynamically resolve the budget guardrail handler factory."
  []
  (rescue nil
          (when-let [factory-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/budget-guardrail-handler)]
            (factory-fn))))

(defn ling-policy-with-budget
  "Permission policy for ling agents with optional budget guardrail."
  ([] (ling-policy-with-budget {}))
  ([opts]
   (if-let [budget-handler (resolve-budget-guardrail-handler)]
     (do
       (when (and (:agent-id opts) (:max-budget-usd opts))
         (rescue nil
                 (let [register-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/register-budget!)]
                   (register-fn (:agent-id opts)
                                (:max-budget-usd opts)
                                {:model (:model opts)}))))
       (->policy :allow-safe
                 (composite-handler [(logging-handler) budget-handler])))
     (do
       (log/debug "Budget module unavailable, using plain ling-policy")
       (ling-policy)))))

(defn drone-policy-with-budget
  "Permission policy for drone agents with allowlist and budget guardrail."
  [opts]
  (let [al-handler (allowlist-handler opts)
        budget-handler (resolve-budget-guardrail-handler)]
    (when (and (:agent-id opts) (:max-budget-usd opts) budget-handler)
      (rescue nil
              (let [register-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/register-budget!)]
                (register-fn (:agent-id opts)
                             (:max-budget-usd opts)
                             {:model (:model opts)}))))
    (if budget-handler
      (->policy :allow-safe
                (composite-handler [al-handler budget-handler]))
      (->policy :allow-safe al-handler))))

(defrecord PermissionManager [policy-atom]
  bridge/IAgentPermissions

  (set-permission-mode! [_ _session mode]
    (let [new-policy (mode->policy mode)]
      (reset! policy-atom new-policy)
      (log/info "Permission mode set" {:mode mode :level (:level new-policy)})
      {:success? true}))

  (set-permission-handler! [_ _session handler-fn]
    (swap! policy-atom assoc :handler-fn handler-fn)
    (log/info "Permission handler set" {:has-handler? true})
    {:success? true}))

(defn ->permission-manager
  "Create a new PermissionManager with the given initial policy."
  ([] (->permission-manager default-policy))
  ([policy]
   (->PermissionManager (atom policy))))

(defn check-tool-permission
  "Check permission on a PermissionManager instance."
  [^PermissionManager manager tool-name input context]
  (check-permission @(.policy-atom manager) tool-name input context))

(defn wrap-permission-check
  "Middleware that wraps a tool handler with permission enforcement."
  [handler manager context]
  (fn [tool-name input]
    (let [result (check-tool-permission manager tool-name input context)]
      (case (:action result)
        :allow
        (handler tool-name (or (:updated-input result) input))

        :deny
        {:error true
         :message (:message result)
         :tool tool-name}

        ;; Shouldn't reach here, but safe fallback
        {:error true
         :message (str "Unexpected permission result for tool: " tool-name)
         :tool tool-name}))))
