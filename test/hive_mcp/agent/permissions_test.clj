(ns hive-mcp.agent.permissions-test
  "Tests for IAgentPermissions implementation + hooks bridge."
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.agent.permissions :as perm]
            [hive-mcp.protocols.agent-bridge :as bridge]))

;;; ============================================================================
;;; Tool Categorization Tests
;;; ============================================================================

(deftest test-categorize-tool
  (testing "Read-only tools"
    (are [tool] (= :read-only (perm/categorize-tool tool))
      "read_file"
      "grep"
      "glob_files"
      "cider_doc"
      "magit_status"
      "kondo_lint"
      "scc"))

  (testing "Safe-write tools"
    (are [tool] (= :safe-write (perm/categorize-tool tool))
      "file_write"
      "propose_diff"
      "magit_stage"
      "magit_commit"))

  (testing "Destructive tools"
    (are [tool] (= :destructive (perm/categorize-tool tool))
      "bash"
      "magit_push"
      "magit_pull"))

  (testing "Coordination tools"
    (are [tool] (= :coordination (perm/categorize-tool tool))
      "hivemind_shout"
      "memory"
      "kanban"))

  (testing "Eval tools"
    (are [tool] (= :eval (perm/categorize-tool tool))
      "clojure_eval"
      "cider_eval_silent"))

  (testing "Unknown tools default to :unknown"
    (is (= :unknown (perm/categorize-tool "nonexistent_tool")))
    (is (= :unknown (perm/categorize-tool "some_custom_tool")))))

;;; ============================================================================
;;; Permission Level Validation Tests
;;; ============================================================================

(deftest test-valid-permission-level
  (testing "Valid levels"
    (are [level] (perm/valid-permission-level? level)
      :allow-all
      :allow-safe
      :prompt-user
      :deny-all))

  (testing "Invalid levels"
    (are [level] (not (perm/valid-permission-level? level))
      :invalid
      :read-only
      nil
      "allow-all")))

;;; ============================================================================
;;; Policy Construction Tests
;;; ============================================================================

(deftest test-policy-construction
  (testing "Basic policy"
    (let [policy (perm/->policy :allow-safe)]
      (is (= :allow-safe (:level policy)))
      (is (nil? (:handler-fn policy)))))

  (testing "Policy with handler"
    (let [handler (fn [_ _ _] {:action :allow})
          policy  (perm/->policy :prompt-user handler)]
      (is (= :prompt-user (:level policy)))
      (is (= handler (:handler-fn policy)))))

  (testing "Invalid level throws assertion"
    (is (thrown? AssertionError (perm/->policy :bogus)))))

(deftest test-default-policy
  (testing "Default policy is :allow-safe"
    (is (= :allow-safe (:level perm/default-policy)))
    (is (nil? (:handler-fn perm/default-policy)))))

;;; ============================================================================
;;; Permission Decision Tests
;;; ============================================================================

(deftest test-check-permission-allow-all
  (let [policy (perm/->policy :allow-all)]
    (testing "All tool categories allowed"
      (are [tool] (= :allow (:action (perm/check-permission policy tool {} {})))
        "read_file"
        "file_write"
        "bash"
        "hivemind_shout"
        "clojure_eval"
        "unknown_tool"))))

(deftest test-check-permission-deny-all
  (let [policy (perm/->policy :deny-all)]
    (testing "All tool categories denied"
      (are [tool] (= :deny (:action (perm/check-permission policy tool {} {})))
        "read_file"
        "file_write"
        "bash"
        "hivemind_shout"
        "clojure_eval"))))

(deftest test-check-permission-allow-safe
  (let [policy (perm/->policy :allow-safe)]
    (testing "Read-only tools allowed"
      (is (= :allow (:action (perm/check-permission policy "read_file" {} {})))))

    (testing "Safe-write tools allowed"
      (is (= :allow (:action (perm/check-permission policy "file_write" {} {})))))

    (testing "Coordination tools allowed"
      (is (= :allow (:action (perm/check-permission policy "hivemind_shout" {} {})))))

    (testing "Eval tools allowed"
      (is (= :allow (:action (perm/check-permission policy "cider_eval_silent" {} {})))))

    (testing "Destructive tools prompt → deny (no handler)"
      (let [result (perm/check-permission policy "bash" {} {})]
        (is (= :deny (:action result)))
        (is (string? (:message result)))))))

(deftest test-check-permission-prompt-user
  (let [policy (perm/->policy :prompt-user)]
    (testing "Read-only tools still allowed"
      (is (= :allow (:action (perm/check-permission policy "read_file" {} {})))))

    (testing "Coordination tools still allowed"
      (is (= :allow (:action (perm/check-permission policy "hivemind_shout" {} {})))))

    (testing "Safe-write tools prompt → deny (no handler)"
      (let [result (perm/check-permission policy "file_write" {} {})]
        (is (= :deny (:action result)))))

    (testing "Destructive tools prompt → deny (no handler)"
      (let [result (perm/check-permission policy "bash" {} {})]
        (is (= :deny (:action result)))))))

(deftest test-check-permission-with-handler
  (testing "Handler allows prompted tool"
    (let [handler (fn [_tool _input _ctx] {:action :allow})
          policy  (perm/->policy :allow-safe handler)
          result  (perm/check-permission policy "bash" {} {})]
      (is (= :allow (:action result)))))

  (testing "Handler denies prompted tool"
    (let [handler (fn [_tool _input _ctx] {:action :deny :message "Nope"})
          policy  (perm/->policy :allow-safe handler)
          result  (perm/check-permission policy "bash" {} {})]
      (is (= :deny (:action result)))
      (is (= "Nope" (:message result)))))

  (testing "Handler can update input"
    (let [handler (fn [_tool _input _ctx]
                    {:action :allow :updated-input {:sanitized true}})
          policy  (perm/->policy :allow-safe handler)
          result  (perm/check-permission policy "bash" {:cmd "rm -rf /"} {})]
      (is (= :allow (:action result)))
      (is (= {:sanitized true} (:updated-input result)))))

  (testing "Handler exception → deny (safe failure)"
    (let [handler (fn [_tool _input _ctx] (throw (ex-info "boom" {})))
          policy  (perm/->policy :allow-safe handler)
          result  (perm/check-permission policy "bash" {} {})]
      (is (= :deny (:action result)))
      (is (clojure.string/includes? (:message result) "boom")))))

;;; ============================================================================
;;; Handler Factory Tests
;;; ============================================================================

(deftest test-logging-handler
  (let [handler (perm/logging-handler)]
    (testing "Always allows"
      (is (= :allow (:action (handler "bash" {} {:agent-id "test"}))))
      (is (= :allow (:action (handler "file_write" {} {})))))))

(deftest test-allowlist-handler
  (testing "Explicit allowlist"
    (let [handler (perm/allowlist-handler {:tool-allowlist #{"bash" "read_file"}})]
      (is (= :allow (:action (handler "bash" {} {}))))
      (is (= :allow (:action (handler "read_file" {} {}))))
      (is (= :deny (:action (handler "magit_push" {} {}))))))

  (testing "Deny message includes tool name"
    (let [handler (perm/allowlist-handler {:tool-allowlist #{"read_file"}})
          result  (handler "bash" {} {})]
      (is (= :deny (:action result)))
      (is (clojure.string/includes? (:message result) "bash")))))

(deftest test-callback-handler
  (testing "Boolean true → allow"
    (let [handler (perm/callback-handler (constantly true))]
      (is (= :allow (:action (handler "bash" {} {}))))))

  (testing "Boolean false → deny"
    (let [handler (perm/callback-handler (constantly false))]
      (is (= :deny (:action (handler "bash" {} {}))))))

  (testing "Map passthrough"
    (let [handler (perm/callback-handler
                   (constantly {:action :deny :message "custom" :interrupt? true}))]
      (let [result (handler "bash" {} {})]
        (is (= :deny (:action result)))
        (is (= "custom" (:message result)))
        (is (true? (:interrupt? result)))))))

(deftest test-composite-handler
  (testing "All allow → allow"
    (let [h1 (constantly {:action :allow})
          h2 (constantly {:action :allow})
          handler (perm/composite-handler [h1 h2])]
      (is (= :allow (:action (handler "bash" {} {}))))))

  (testing "First deny wins"
    (let [h1 (constantly {:action :deny :message "blocked by h1"})
          h2 (constantly {:action :allow})
          handler (perm/composite-handler [h1 h2])]
      (let [result (handler "bash" {} {})]
        (is (= :deny (:action result)))
        (is (= "blocked by h1" (:message result))))))

  (testing "Second deny wins when first allows"
    (let [h1 (constantly {:action :allow})
          h2 (constantly {:action :deny :message "blocked by h2"})
          handler (perm/composite-handler [h1 h2])]
      (let [result (handler "bash" {} {})]
        (is (= :deny (:action result)))
        (is (= "blocked by h2" (:message result)))))))

;;; ============================================================================
;;; Mode Bridge Tests
;;; ============================================================================

(deftest test-mode->policy
  (testing "Protocol modes map correctly"
    (is (= :prompt-user (:level (perm/mode->policy :default))))
    (is (= :allow-safe (:level (perm/mode->policy :accept-edits))))
    (is (= :allow-all (:level (perm/mode->policy :bypass)))))

  (testing "Unknown mode falls back to prompt-user"
    (is (= :prompt-user (:level (perm/mode->policy :unknown-mode))))))

(deftest test-mode->sdk-string
  (testing "SDK string conversion"
    (is (= "default" (perm/mode->sdk-string :default)))
    (is (= "acceptEdits" (perm/mode->sdk-string :accept-edits)))
    (is (= "bypassPermissions" (perm/mode->sdk-string :bypass))))

  (testing "Unknown mode fallback"
    (is (= "default" (perm/mode->sdk-string :bogus)))))

;;; ============================================================================
;;; Agent Policy Preset Tests
;;; ============================================================================

(deftest test-ling-policy
  (let [policy (perm/ling-policy)]
    (is (= :allow-safe (:level policy)))
    (is (nil? (:handler-fn policy)))))

(deftest test-drone-policy
  (let [policy (perm/drone-policy {:tool-allowlist #{"read_file" "propose_diff"}})]
    (is (= :allow-safe (:level policy)))
    (is (fn? (:handler-fn policy)))

    (testing "Allowlist handler integrated"
      ;; Destructive tool hits :prompt → handler checks allowlist
      (let [result (perm/check-permission policy "bash" {} {})]
        (is (= :deny (:action result))))

      ;; read_file is :allow-safe → :allow (doesn't hit handler)
      (let [result (perm/check-permission policy "read_file" {} {})]
        (is (= :allow (:action result)))))))

(deftest test-coordinator-policy
  (let [policy (perm/coordinator-policy)]
    (is (= :allow-all (:level policy)))))

;;; ============================================================================
;;; PermissionManager (IAgentPermissions) Tests
;;; ============================================================================

(deftest test-permission-manager-creation
  (testing "Default manager"
    (let [mgr (perm/->permission-manager)]
      (is (satisfies? bridge/IAgentPermissions mgr))
      (is (= :allow-safe (:level @(.policy-atom mgr))))))

  (testing "Custom policy manager"
    (let [policy (perm/->policy :deny-all)
          mgr    (perm/->permission-manager policy)]
      (is (= :deny-all (:level @(.policy-atom mgr)))))))

(deftest test-permission-manager-set-mode
  (let [mgr (perm/->permission-manager)]
    (testing "Set mode changes policy"
      (let [result (bridge/set-permission-mode! mgr nil :bypass)]
        (is (:success? result))
        (is (= :allow-all (:level @(.policy-atom mgr))))))

    (testing "Set mode back to default"
      (bridge/set-permission-mode! mgr nil :default)
      (is (= :prompt-user (:level @(.policy-atom mgr)))))))

(deftest test-permission-manager-set-handler
  (let [mgr     (perm/->permission-manager)
        handler (fn [_ _ _] {:action :allow})]
    (testing "Set handler updates policy"
      (let [result (bridge/set-permission-handler! mgr nil handler)]
        (is (:success? result))
        (is (= handler (:handler-fn @(.policy-atom mgr))))))))

(deftest test-check-tool-permission
  (let [mgr (perm/->permission-manager)]
    (testing "Check via manager convenience fn"
      (is (= :allow (:action (perm/check-tool-permission mgr "read_file" {} {}))))
      ;; bash is destructive, no handler → deny
      (is (= :deny (:action (perm/check-tool-permission mgr "bash" {} {})))))))

;;; ============================================================================
;;; Enforcement Middleware Tests
;;; ============================================================================

(deftest test-wrap-permission-check
  (let [mgr     (perm/->permission-manager (perm/->policy :allow-safe))
        calls   (atom [])
        handler (fn [tool-name input]
                  (swap! calls conj {:tool tool-name :input input})
                  {:result "ok"})
        wrapped (perm/wrap-permission-check handler mgr {:agent-id "test-ling"})]

    (testing "Allowed tool passes through to handler"
      (let [result (wrapped "read_file" {:path "/foo"})]
        (is (= {:result "ok"} result))
        (is (= 1 (count @calls)))
        (is (= "read_file" (:tool (first @calls))))))

    (testing "Denied tool returns error, handler not called"
      (reset! calls [])
      (let [result (wrapped "bash" {:cmd "rm -rf /"})]
        (is (:error result))
        (is (= "bash" (:tool result)))
        (is (empty? @calls))))))

(deftest test-wrap-permission-check-with-updated-input
  (let [handler-fn (fn [_tool _input _ctx]
                     {:action :allow :updated-input {:sanitized true}})
        mgr        (perm/->permission-manager (perm/->policy :allow-safe handler-fn))
        calls      (atom [])
        handler    (fn [tool-name input]
                     (swap! calls conj {:tool tool-name :input input})
                     {:result "ok"})
        wrapped    (perm/wrap-permission-check handler mgr {:agent-id "test"})]

    (testing "Handler receives updated input from permission handler"
      ;; bash is :destructive → :prompt → handler-fn returns updated-input
      (wrapped "bash" {:cmd "rm -rf /"})
      (is (= 1 (count @calls)))
      (is (= {:sanitized true} (:input (first @calls)))))))
