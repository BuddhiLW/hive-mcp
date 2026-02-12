(ns hive-mcp.transport.olympus-server-test
  "Tests for Olympus WebSocket server command handling.

   Tests the command routing, authentication, and message processing
   without requiring a running Aleph server (pure function tests).

   Uses with-redefs to mock snapshot builders so tests don't depend
   on DataScript/project-tree infrastructure.

   TDD: Tests written alongside implementation.
   CLARITY: I - Tests validate input handling at boundaries."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.transport.olympus-server :as server]
            [hive-mcp.transport.olympus :as olympus]
            [hive-mcp.agent.headless :as headless]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:private mock-snapshot
  "Mock snapshot data for tests."
  {:type :init-snapshot
   :timestamp 1234567890
   :data {:agents [{:id "ling-1" :status "working"}]
          :waves {"wave-1" {:id "wave-1" :status "running"}}
          :kg {:entries [] :edges [] :entry-count 0}
          :project-tree {:projects [] :roots [] :total 0}}})

(defn reset-fixture
  "Reset server state between tests."
  [f]
  ;; Reset auth handler to nil (dev mode - all authorized)
  (server/set-auth-handler! nil)
  (f))

(use-fixtures :each reset-fixture)

;; =============================================================================
;; Helper: Run with mocked snapshots
;; =============================================================================

(defmacro with-mock-snapshots
  "Execute body with olympus/build-full-snapshot mocked."
  [& body]
  `(with-redefs [olympus/build-full-snapshot (fn [] mock-snapshot)]
     ~@body))

;; =============================================================================
;; process-incoming-message Tests
;; =============================================================================

(deftest process-ping-message
  (testing "Ping message returns pong"
    (let [result (server/process-incoming-message
                  "{\"type\":\"ping\"}" {:client-id "test"})]
      (is (= :pong (:type result)))
      (is (number? (:timestamp result))))))

(deftest process-ping-json-wrapped
  (testing "JSON-wrapped ping message returns pong"
    (let [result (server/process-incoming-message
                  (json/write-str {:type "ping"}) {:client-id "test"})]
      (is (= :pong (:type result))))))

(deftest process-subscribe-message
  (testing "Subscribe message returns ack"
    (let [result (server/process-incoming-message
                  (json/write-str {:type "subscribe" :views ["agents" "waves"]})
                  {:client-id "test"})]
      (is (= :subscribe-ack (:type result)))
      ;; JSON roundtrip preserves strings but doesn't keywordize array elements
      (is (= ["agents" "waves"] (:views result))))))

(deftest process-unknown-message-type
  (testing "Unknown message type returns nil (ignored)"
    (let [result (server/process-incoming-message
                  (json/write-str {:type "unknown-type"}) {:client-id "test"})]
      (is (nil? result)))))

(deftest process-malformed-json
  (testing "Malformed JSON returns error response"
    (let [result (server/process-incoming-message
                  "not valid json{{{" {:client-id "test"})]
      (is (= :error (:type result)))
      (is (string? (:error result))))))

(deftest process-request-snapshot-agents
  (testing "Request snapshot for agents view"
    (with-mock-snapshots
      (let [result (server/process-incoming-message
                    (json/write-str {:type "request-snapshot" :view "agents"})
                    {:client-id "test"})]
        (is (= :snapshot-response (:type result)))
        (is (= :agents (:view result)))
        (is (number? (:timestamp result)))
        ;; Data should be extracted from mock snapshot
        (is (= {:type :agents :data [{:id "ling-1" :status "working"}]}
               (:data result)))))))

(deftest process-request-snapshot-full
  (testing "Request full snapshot"
    (with-mock-snapshots
      (let [result (server/process-incoming-message
                    (json/write-str {:type "request-snapshot" :view "full"})
                    {:client-id "test"})]
        (is (= :snapshot-response (:type result)))
        (is (= :full (:view result)))
        (is (= mock-snapshot (:data result)))))))

;; =============================================================================
;; Command Message Tests
;; =============================================================================

(deftest process-command-message-basic
  (testing "Command message gets routed and returns response"
    (with-mock-snapshots
      (let [result (server/process-incoming-message
                    (json/write-str {:type "command"
                                     :command "snapshot/full"
                                     :params {}})
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= "snapshot/full" (:command result)))
        (is (number? (:timestamp result)))))))

(deftest process-command-with-request-id
  (testing "Command response includes request-id for correlation"
    (with-mock-snapshots
      (let [result (server/process-incoming-message
                    (json/write-str {:type "command"
                                     :command "snapshot/agents"
                                     :params {}
                                     :request-id "req-42"})
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= "req-42" (:request-id result)))))))

(deftest process-command-unknown-namespace
  (testing "Unknown command namespace returns error"
    (let [result (server/handle-command-message
                  {:command "nonexistent/action" :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (string? (:error result)))
      (is (.contains (:error result) "Unknown command namespace")))))

;; =============================================================================
;; handle-command-message Tests (Snapshot commands with mocks)
;; =============================================================================

(deftest handle-command-snapshot-full
  (testing "Snapshot/full command returns full snapshot"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/full" :params {}}
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= true (:success result)))
        (is (map? (:data result)))
        (is (= :init-snapshot (:type (:data result))))))))

(deftest handle-command-snapshot-agents
  (testing "Snapshot/agents command returns agents data"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/agents" :params {}}
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= true (:success result)))
        (is (= :agents (get-in result [:data :type])))
        (is (= [{:id "ling-1" :status "working"}]
               (get-in result [:data :data])))))))

(deftest handle-command-snapshot-waves
  (testing "Snapshot/waves command returns waves data"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/waves" :params {}}
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= true (:success result)))
        (is (= :waves (get-in result [:data :type])))))))

(deftest handle-command-snapshot-kg
  (testing "Snapshot/kg command returns KG data"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/kg" :params {}}
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= true (:success result)))
        (is (= :kg-snapshot (get-in result [:data :type])))))))

(deftest handle-command-snapshot-project-tree
  (testing "Snapshot/project-tree command returns project tree data"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/project-tree" :params {}}
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= true (:success result)))
        (is (= :project-tree (get-in result [:data :type])))))))

(deftest handle-command-snapshot-unknown-view
  (testing "Unknown snapshot view returns error"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/nonexistent" :params {}}
                    {:client-id "test"})]
        (is (= :command-response (:type result)))
        (is (= false (:success result)))
        (is (.contains (:error result) "Unknown snapshot command"))))))

;; =============================================================================
;; Authentication Tests
;; =============================================================================

(deftest auth-default-allows-all
  (testing "Without auth handler, all commands are authorized"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/full" :params {}}
                    {:client-id "test"})]
        (is (= true (:success result)))))))

(deftest auth-handler-blocks-unauthorized
  (testing "Auth handler can deny commands"
    (server/set-auth-handler!
     (fn [_client-info _command _params]
       {:authorized? false :reason "Invalid token"}))
    (let [result (server/handle-command-message
                  {:command "snapshot/full" :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (.contains (:error result) "Unauthorized"))
      (is (.contains (:error result) "Invalid token")))))

(deftest auth-handler-allows-authorized
  (testing "Auth handler can authorize commands"
    (server/set-auth-handler!
     (fn [_client-info _command _params]
       {:authorized? true}))
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/full" :params {}}
                    {:client-id "test"})]
        (is (= true (:success result)))))))

(deftest auth-handler-selective-authorization
  (testing "Auth handler can selectively authorize commands"
    (server/set-auth-handler!
     (fn [_client-info command _params]
       (if (= command :snapshot/full)
         {:authorized? true}
         {:authorized? false :reason "Read-only access"})))
    ;; Snapshot should work
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/full" :params {}}
                    {:client-id "test"})]
        (is (= true (:success result)))))
    ;; Agent command should be denied
    (let [result (server/handle-command-message
                  {:command "agent/list" :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (.contains (:error result) "Read-only access")))))

(deftest auth-handler-receives-client-info
  (testing "Auth handler receives client-info"
    (let [captured (atom nil)]
      (server/set-auth-handler!
       (fn [client-info _command _params]
         (reset! captured client-info)
         {:authorized? true}))
      (with-mock-snapshots
        (server/handle-command-message
         {:command "snapshot/full" :params {}}
         {:client-id "web-42" :ip "127.0.0.1"}))
      (is (= {:client-id "web-42" :ip "127.0.0.1"} @captured)))))

(deftest auth-handler-error-denies
  (testing "Auth handler exception results in denial"
    (server/set-auth-handler!
     (fn [_ _ _]
       (throw (Exception. "Auth service down"))))
    (let [result (server/handle-command-message
                  {:command "snapshot/full" :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (.contains (:error result) "Unauthorized")))))

;; =============================================================================
;; Custom Command Registration Tests
;; =============================================================================

(deftest register-custom-command
  (testing "Custom commands can be registered and executed"
    (server/register-command! "custom"
                              (fn [action params]
                                {:success true
                                 :command (str "custom/" (name action))
                                 :result {:echo (:message params)}}))
    (let [result (server/handle-command-message
                  {:command "custom/echo"
                   :params {:message "hello"}}
                  {:client-id "test"})]
      (is (= true (:success result)))
      (is (= {:echo "hello"} (:data result))))))

(deftest custom-command-multiple-actions
  (testing "Custom namespace supports multiple actions"
    (server/register-command! "myapp"
                              (fn [action params]
                                (case action
                                  :greet {:success true :result (str "Hello " (:name params))}
                                  :count {:success true :result (count (:items params))}
                                  {:success false :error (str "Unknown action: " (name action))})))
    (let [greet (server/handle-command-message
                 {:command "myapp/greet" :params {:name "World"}}
                 {:client-id "test"})
          count-r (server/handle-command-message
                   {:command "myapp/count" :params {:items [1 2 3]}}
                   {:client-id "test"})]
      (is (= true (:success greet)))
      (is (= "Hello World" (:data greet)))
      (is (= true (:success count-r)))
      (is (= 3 (:data count-r))))))

(deftest custom-command-appears-in-status
  (testing "Custom commands appear in status"
    (server/register-command! "myns"
                              (fn [_ _] {:success true}))
    (let [sts (server/status)]
      (is (some #(= "myns" %) (:command-namespaces sts))))))

(deftest custom-command-appears-in-list-commands
  (testing "Custom commands appear in list-commands"
    (server/register-command! "test-ns"
                              (fn [_ _] {:success true}))
    (let [cmds (server/list-commands)]
      (is (contains? (:custom cmds) "test-ns")))))

;; =============================================================================
;; list-commands Tests
;; =============================================================================

(deftest list-commands-has-builtin-agent
  (testing "list-commands returns agent command descriptions"
    (let [cmds (server/list-commands)]
      (is (map? (:builtin cmds)))
      (is (contains? (:builtin cmds) :agent))
      (is (contains? (get-in cmds [:builtin :agent]) :spawn))
      (is (contains? (get-in cmds [:builtin :agent]) :kill))
      (is (contains? (get-in cmds [:builtin :agent]) :dispatch))
      (is (contains? (get-in cmds [:builtin :agent]) :status))
      (is (contains? (get-in cmds [:builtin :agent]) :list))
      (is (contains? (get-in cmds [:builtin :agent]) :claims))
      (is (contains? (get-in cmds [:builtin :agent]) :cleanup)))))

(deftest list-commands-has-builtin-kanban
  (testing "list-commands returns kanban command descriptions"
    (let [cmds (server/list-commands)]
      (is (contains? (:builtin cmds) :kanban))
      (is (contains? (get-in cmds [:builtin :kanban]) :list))
      (is (contains? (get-in cmds [:builtin :kanban]) :create))
      (is (contains? (get-in cmds [:builtin :kanban]) :move))
      (is (contains? (get-in cmds [:builtin :kanban]) :status)))))

(deftest list-commands-has-builtin-hivemind
  (testing "list-commands returns hivemind command descriptions"
    (let [cmds (server/list-commands)]
      (is (contains? (:builtin cmds) :hivemind))
      (is (contains? (get-in cmds [:builtin :hivemind]) :shout))
      (is (contains? (get-in cmds [:builtin :hivemind]) :status)))))

(deftest list-commands-has-builtin-snapshot
  (testing "list-commands returns snapshot command descriptions"
    (let [cmds (server/list-commands)]
      (is (contains? (:builtin cmds) :snapshot))
      (is (contains? (get-in cmds [:builtin :snapshot]) :full))
      (is (contains? (get-in cmds [:builtin :snapshot]) :agents))
      (is (contains? (get-in cmds [:builtin :snapshot]) :waves))
      (is (contains? (get-in cmds [:builtin :snapshot]) :kg))
      (is (contains? (get-in cmds [:builtin :snapshot]) :project-tree)))))

;; =============================================================================
;; Status Tests
;; =============================================================================

(deftest status-includes-command-namespaces
  (testing "Status includes all command namespaces"
    (let [sts (server/status)]
      (is (vector? (:command-namespaces sts)))
      (is (some #(= "agent" %) (:command-namespaces sts)))
      (is (some #(= "kanban" %) (:command-namespaces sts)))
      (is (some #(= "hivemind" %) (:command-namespaces sts)))
      (is (some #(= "snapshot" %) (:command-namespaces sts))))))

(deftest status-auth-flag-default
  (testing "Status shows no auth configured by default"
    (is (= false (:auth-configured? (server/status))))))

(deftest status-auth-flag-after-set
  (testing "Status shows auth configured after setting handler"
    (server/set-auth-handler! (fn [_ _ _] {:authorized? true}))
    (is (= true (:auth-configured? (server/status))))))

(deftest status-includes-base-olympus-status
  (testing "Status inherits from olympus/status"
    (let [sts (server/status)]
      ;; Should have :running? from base status
      (is (contains? sts :running?))
      (is (contains? sts :clients)))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest command-without-slash
  (testing "Command without slash uses namespace as both ns and action"
    (let [result (server/handle-command-message
                  {:command "snapshot" :params {}}
                  {:client-id "test"})]
      ;; Should attempt to call snapshot handler with :snapshot action
      ;; which isn't a valid sub-command, so it returns success=false
      (is (= :command-response (:type result))))))

(deftest command-with-nil-params
  (testing "Command with nil params uses empty map"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command "snapshot/full" :params nil}
                    {:client-id "test"})]
        (is (= true (:success result)))))))

(deftest command-as-keyword
  (testing "Command as keyword works same as string"
    (with-mock-snapshots
      (let [result (server/handle-command-message
                    {:command :snapshot/full :params {}}
                    {:client-id "test"})]
        (is (= true (:success result)))))))

(deftest process-map-input
  (testing "process-incoming-message accepts map directly (not just string)"
    (let [result (server/process-incoming-message
                  {:type "ping"} {:client-id "test"})]
      (is (= :pong (:type result))))))

;; =============================================================================
;; Integration: Command -> Response flow
;; =============================================================================

(deftest full-command-flow-snapshot
  (testing "Full flow: JSON message -> parse -> route -> response"
    (with-mock-snapshots
      (let [msg (json/write-str {:type "command"
                                 :command "snapshot/full"
                                 :params {}
                                 :request-id "test-123"})
            result (server/process-incoming-message msg {:client-id "web-1"})]
        (is (= :command-response (:type result)))
        (is (= true (:success result)))
        (is (= "test-123" (:request-id result)))
        (is (= "snapshot/full" (:command result)))
        ;; Verify snapshot data structure
        (let [data (:data result)]
          (is (map? data))
          (is (= :init-snapshot (:type data))))))))

(deftest full-command-flow-with-auth-denied
  (testing "Full flow with auth: denied command returns error"
    (server/set-auth-handler!
     (fn [_ _ _] {:authorized? false :reason "No token"}))
    (let [msg (json/write-str {:type "command"
                               :command "agent/spawn"
                               :params {:type "ling" :cwd "/tmp"}
                               :request-id "auth-test"})
          result (server/process-incoming-message msg {:client-id "web-1"})]
      (is (= :command-response (:type result)))
      (is (= false (:success result)))
      (is (= "auth-test" (:request-id result)))
      (is (.contains (:error result) "No token")))))

(deftest full-command-flow-custom-command
  (testing "Full flow with custom command"
    (server/register-command! "test-flow"
                              (fn [action params]
                                {:success true
                                 :result {:action (name action)
                                          :received params}}))
    (let [msg (json/write-str {:type "command"
                               :command "test-flow/execute"
                               :params {:key "value"}
                               :request-id "flow-1"})
          result (server/process-incoming-message msg {:client-id "web-1"})]
      (is (= :command-response (:type result)))
      (is (= true (:success result)))
      (is (= "flow-1" (:request-id result)))
      (is (= "execute" (get-in result [:data :action]))))))

;; =============================================================================
;; Ling Output Command Tests
;; =============================================================================

(defn- gen-test-ling-id []
  (str "test-ling-output-" (java.util.UUID/randomUUID)))

(deftest ling-output-get-basic
  (testing "ling-output/get returns stdout from headless ling"
    (let [ling-id (gen-test-ling-id)]
      (try
        ;; Spawn a cat process (echoes stdin to stdout)
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        ;; Send some lines
        (headless/dispatch-via-stdin! ling-id "hello world")
        (headless/dispatch-via-stdin! ling-id "test output")
        (Thread/sleep 300)
        ;; Request via command handler
        (let [result (server/handle-command-message
                      {:command "ling-output/get"
                       :params {:ling_id ling-id}}
                      {:client-id "test"})]
          (is (= :command-response (:type result)))
          (is (= true (:success result)))
          (is (vector? (get-in result [:data :lines])))
          (is (some #(= "hello world" %) (get-in result [:data :lines])))
          (is (some #(= "test output" %) (get-in result [:data :lines])))
          (is (= ling-id (get-in result [:data :ling_id])))
          (is (pos? (get-in result [:data :count]))))
        (finally
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))

(deftest ling-output-get-with-last-n
  (testing "ling-output/get with last_n limits output"
    (let [ling-id (gen-test-ling-id)]
      (try
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        (doseq [i (range 5)]
          (headless/dispatch-via-stdin! ling-id (str "line-" i)))
        (Thread/sleep 300)
        (let [result (server/handle-command-message
                      {:command "ling-output/get"
                       :params {:ling_id ling-id :last_n 2}}
                      {:client-id "test"})]
          (is (= true (:success result)))
          (is (= 2 (count (get-in result [:data :lines]))))
          (is (= "line-4" (last (get-in result [:data :lines])))))
        (finally
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))

(deftest ling-output-get-with-since-timestamp
  (testing "ling-output/get with since returns only newer lines"
    (let [ling-id (gen-test-ling-id)]
      (try
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        ;; Send first batch
        (headless/dispatch-via-stdin! ling-id "old-line-1")
        (headless/dispatch-via-stdin! ling-id "old-line-2")
        (Thread/sleep 200)
        ;; Record timestamp between batches
        (let [mid-ts (System/currentTimeMillis)]
          (Thread/sleep 50)
          ;; Send second batch
          (headless/dispatch-via-stdin! ling-id "new-line-1")
          (headless/dispatch-via-stdin! ling-id "new-line-2")
          (Thread/sleep 200)
          ;; Get only lines since mid-ts
          (let [result (server/handle-command-message
                        {:command "ling-output/get"
                         :params {:ling_id ling-id :since mid-ts}}
                        {:client-id "test"})]
            (is (= true (:success result)))
            ;; Should only have the new lines
            (let [lines (get-in result [:data :lines])]
              (is (pos? (count lines)))
              (is (some #(= "new-line-1" %) lines))
              (is (some #(= "new-line-2" %) lines))
              ;; Old lines should NOT be present
              (is (not (some #(= "old-line-1" %) lines))))
            ;; Should have a cursor for next poll
            (is (pos? (get-in result [:data :cursor])))))
        (finally
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))

(deftest ling-output-get-missing-ling-id
  (testing "ling-output/get without ling_id returns error"
    (let [result (server/handle-command-message
                  {:command "ling-output/get"
                   :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (.contains (:error result) "ling_id")))))

(deftest ling-output-get-nonexistent-ling
  (testing "ling-output/get with nonexistent ling returns error"
    (let [result (server/handle-command-message
                  {:command "ling-output/get"
                   :params {:ling_id "nonexistent-ling-xyz"}}
                  {:client-id "test"})]
      (is (= false (:success result))))))

(deftest ling-output-list-headless
  (testing "ling-output/list returns available headless lings"
    (let [ling-id (gen-test-ling-id)]
      (try
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        (let [result (server/handle-command-message
                      {:command "ling-output/list" :params {}}
                      {:client-id "test"})]
          (is (= true (:success result)))
          (is (vector? (get-in result [:data :headless-lings])))
          (is (some #(= ling-id (:ling-id %))
                    (get-in result [:data :headless-lings]))))
        (finally
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))

(deftest ling-output-subscribe-basic
  (testing "ling-output/subscribe creates subscription"
    (let [ling-id (gen-test-ling-id)]
      (try
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        (let [result (server/handle-command-message
                      {:command "ling-output/subscribe"
                       :params {:ling_id ling-id}}
                      {:client-id "test"})]
          (is (= true (:success result)))
          (is (= true (get-in result [:data :subscribed])))
          (is (= ling-id (get-in result [:data :ling_id])))
          (is (string? (get-in result [:data :subscription-id]))))
        ;; Cleanup subscription
        (server/cleanup-ling-subscriptions!)
        (finally
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))

(deftest ling-output-subscribe-missing-ling-id
  (testing "ling-output/subscribe without ling_id returns error"
    (let [result (server/handle-command-message
                  {:command "ling-output/subscribe"
                   :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (.contains (:error result) "ling_id")))))

(deftest ling-output-subscribe-nonexistent-ling
  (testing "ling-output/subscribe for nonexistent ling returns error"
    (let [result (server/handle-command-message
                  {:command "ling-output/subscribe"
                   :params {:ling_id "nonexistent-ling-xyz"}}
                  {:client-id "test"})]
      (is (= false (:success result))))))

(deftest ling-output-unsubscribe
  (testing "ling-output/unsubscribe removes subscription"
    (let [ling-id (gen-test-ling-id)]
      (try
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        ;; Subscribe first
        (server/handle-command-message
         {:command "ling-output/subscribe" :params {:ling_id ling-id}}
         {:client-id "test"})
        ;; Then unsubscribe
        (let [result (server/handle-command-message
                      {:command "ling-output/unsubscribe"
                       :params {:ling_id ling-id}}
                      {:client-id "test"})]
          (is (= true (:success result)))
          (is (= true (get-in result [:data :unsubscribed]))))
        (finally
          (server/cleanup-ling-subscriptions!)
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))

(deftest ling-output-unsubscribe-missing-ling-id
  (testing "ling-output/unsubscribe without ling_id returns error"
    (let [result (server/handle-command-message
                  {:command "ling-output/unsubscribe"
                   :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (.contains (:error result) "ling_id")))))

(deftest ling-output-unknown-subcommand
  (testing "ling-output with unknown sub-command returns error"
    (let [result (server/handle-command-message
                  {:command "ling-output/bogus"
                   :params {}}
                  {:client-id "test"})]
      (is (= false (:success result)))
      (is (.contains (:error result) "Unknown ling-output command")))))

(deftest ling-output-push-streaming
  (testing "Subscribed clients receive push output when new lines appear"
    (let [ling-id (gen-test-ling-id)
          received (atom [])]
      (try
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        ;; Subscribe with a custom send-fn that captures events
        (server/subscribe-ling-output! ling-id
                                       (fn [event] (swap! received conj event)))
        ;; Send a line — should trigger push
        (headless/dispatch-via-stdin! ling-id "streamed-line")
        (Thread/sleep 500)
        ;; Verify we received the push event
        (is (pos? (count @received)))
        (let [event (first @received)]
          (is (= :ling-output-stream (:type event)))
          (is (= ling-id (:ling_id event)))
          (is (seq (:entries event)))
          (is (some #(= "streamed-line" (:text %)) (:entries event)))
          (is (pos? (:cursor event))))
        (finally
          (server/cleanup-ling-subscriptions!)
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))

(deftest ling-output-in-list-commands
  (testing "list-commands includes ling-output namespace"
    (let [cmds (server/list-commands)]
      (is (contains? (:builtin cmds) :ling-output))
      (is (contains? (get-in cmds [:builtin :ling-output]) :get))
      (is (contains? (get-in cmds [:builtin :ling-output]) :subscribe))
      (is (contains? (get-in cmds [:builtin :ling-output]) :unsubscribe))
      (is (contains? (get-in cmds [:builtin :ling-output]) :list)))))

(deftest ling-output-in-status-namespaces
  (testing "Status includes ling-output in command namespaces"
    (let [sts (server/status)]
      (is (some #(= "ling-output" %) (:command-namespaces sts))))))

(deftest ling-output-full-flow-via-process-incoming-message
  (testing "Full flow: JSON command -> ling-output/get -> response"
    (let [ling-id (gen-test-ling-id)]
      (try
        (headless/spawn-headless! ling-id {:cwd "/tmp" :claude-cmd "cat"
                                           :buffer-capacity 100})
        (Thread/sleep 100)
        (headless/dispatch-via-stdin! ling-id "full-flow-test")
        (Thread/sleep 300)
        (let [msg (json/write-str {:type "command"
                                   :command "ling-output/get"
                                   :params {:ling_id ling-id}
                                   :request-id "lo-123"})
              result (server/process-incoming-message msg {:client-id "web-1"})]
          (is (= :command-response (:type result)))
          (is (= true (:success result)))
          (is (= "lo-123" (:request-id result)))
          (is (= "ling-output/get" (:command result)))
          (is (some #(= "full-flow-test" %) (get-in result [:data :lines]))))
        (finally
          (try (headless/kill-headless! ling-id {:force? true}) (catch Exception _)))))))
