(ns hive-mcp.addons.mcp-bridge-test
  "Tests for the MCP bridge subsystem.

   Covers:
   - IMcpBridge protocol satisfaction
   - NoopMcpBridge behavior
   - proxy-tool-def generation
   - Bridge registry operations
   - StdioBridge full lifecycle (spawn → discover → proxy → shutdown)
     via scripts/echo-mcp-server.py"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.addons.core :as addon]
            [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.addons.stdio-bridge :as stdio]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn reset-fixture
  "Reset addon + bridge registries before/after each test."
  [f]
  (addon/reset-registry!)
  (f)
  ;; Cleanup any lingering bridges
  (doseq [{:keys [name]} (bridge/list-bridges)]
    (bridge/unregister-bridge! name))
  (addon/reset-registry!))

(use-fixtures :each reset-fixture)

;; =============================================================================
;; Echo Server Path
;; =============================================================================

(def echo-server-command
  "Command to launch the echo MCP server.
   Resolved relative to project root (where tests run from)."
  ["python3" "scripts/echo-mcp-server.py"])

;; =============================================================================
;; IMcpBridge Protocol Tests
;; =============================================================================

(deftest test-noop-bridge-protocol
  (let [b (bridge/->noop-bridge :bridge/test)]
    (testing "satisfies both protocols"
      (is (bridge/bridge? b))
      (is (bridge/bridge-addon? b)))

    (testing "transport-type is :stdio"
      (is (= :stdio (bridge/transport-type b))))

    (testing "start-bridge! returns success"
      (let [result (bridge/start-bridge! b {})]
        (is (true? (:success? result)))))

    (testing "stop-bridge! returns success"
      (let [result (bridge/stop-bridge! b)]
        (is (true? (:success? result)))))

    (testing "bridge-status shows disconnected"
      (let [status (bridge/bridge-status b)]
        (is (false? (:connected? status)))
        (is (= :stdio (:transport status)))
        (is (= 0 (:remote-tool-count status)))))

    (testing "call-tool returns error"
      (let [result (bridge/call-tool b "anything" {})]
        (is (true? (:isError result)))
        (is (string? (-> result :content first :text)))))

    (testing "list-remote-tools returns empty"
      (is (= [] (bridge/list-remote-tools b))))))

(deftest test-noop-bridge-defaults
  (testing "zero-arg constructor"
    (let [b (bridge/->noop-bridge)]
      (is (= :bridge/noop (addon/addon-name b)))))

  (testing "one-arg constructor"
    (let [b (bridge/->noop-bridge :bridge/custom)]
      (is (= :bridge/custom (addon/addon-name b))))))

;; =============================================================================
;; Proxy Tool Generation Tests
;; =============================================================================

(deftest test-proxy-tool-def
  (let [b (bridge/->noop-bridge :bridge/test)
        remote-tool {:name        "search"
                     :description "Search documents"
                     :inputSchema {:type       "object"
                                   :properties {"query" {:type "string"}}
                                   :required   ["query"]}}
        proxy (bridge/proxy-tool-def b "test" remote-tool)]

    (testing "namespaced name"
      (is (= "test:search" (:name proxy))))

    (testing "description includes bridge prefix"
      (is (str/includes? (:description proxy) "[via test]")))

    (testing "schema preserved"
      (is (= (:inputSchema remote-tool) (:inputSchema proxy))))

    (testing "handler is a function"
      (is (fn? (:handler proxy))))

    (testing "bridge-source set"
      (is (= "test" (:bridge-source proxy))))

    (testing "handler proxies call (noop returns error)"
      (let [result ((:handler proxy) {"query" "hello"})]
        (is (= "text" (:type result)))
        (is (str/includes? (:text result) "Remote tool error"))))))

(deftest test-proxy-tool-def-missing-schema
  (testing "nil inputSchema gets default"
    (let [b (bridge/->noop-bridge :bridge/x)
          proxy (bridge/proxy-tool-def b "x" {:name "bare" :description "bare tool"})]
      (is (= {:type "object" :properties {}} (:inputSchema proxy))))))

(deftest test-proxy-tool-defs-empty
  (testing "noop bridge produces empty tool defs"
    (let [b (bridge/->noop-bridge :bridge/test)]
      (is (= [] (bridge/proxy-tool-defs b "test"))))))

;; =============================================================================
;; Bridge Registry Tests
;; =============================================================================

(deftest test-bridge-registry-operations
  (let [b (bridge/->noop-bridge :bridge/reg-test)]
    (testing "register-bridge! succeeds"
      (let [result (bridge/register-bridge! b {})]
        (is (true? (:success? result)))
        (is (= "reg-test" (:bridge-prefix result)))))

    (testing "get-bridge returns bridge"
      (is (some? (bridge/get-bridge :bridge/reg-test)))
      (is (bridge/bridge? (bridge/get-bridge :bridge/reg-test))))

    (testing "list-bridges includes registered bridge"
      (let [bridges (bridge/list-bridges)]
        (is (= 1 (count bridges)))
        (is (= :bridge/reg-test (:name (first bridges))))
        (is (= "reg-test" (:prefix (first bridges))))
        (is (= :stdio (:transport (first bridges))))))

    (testing "bridge-connected? returns false for noop"
      (is (false? (bridge/bridge-connected? :bridge/reg-test))))

    (testing "unregister-bridge! removes bridge"
      (bridge/unregister-bridge! :bridge/reg-test)
      (is (nil? (bridge/get-bridge :bridge/reg-test)))
      (is (= [] (bridge/list-bridges))))))

(deftest test-bridge-register-validates
  (testing "non-bridge throws assertion"
    (is (thrown? AssertionError
                 (bridge/register-bridge! (addon/->noop-addon :not-bridge))))))

;; =============================================================================
;; StdioBridge Protocol Tests
;; =============================================================================

(deftest test-stdio-bridge-protocols
  (let [b (stdio/->stdio-bridge :bridge/proto)]
    (testing "satisfies IMcpBridge"
      (is (bridge/bridge? b)))

    (testing "satisfies IAddon"
      (is (satisfies? addon/IAddon b)))

    (testing "bridge-addon? true"
      (is (bridge/bridge-addon? b)))

    (testing "transport-type is :stdio"
      (is (= :stdio (bridge/transport-type b))))

    (testing "addon-name correct"
      (is (= :bridge/proto (addon/addon-name b))))

    (testing "addon-capabilities include :mcp-bridge and :tools"
      (is (contains? (addon/addon-capabilities b) :mcp-bridge))
      (is (contains? (addon/addon-capabilities b) :tools)))))

;; =============================================================================
;; StdioBridge Integration Tests — Full Lifecycle
;; =============================================================================

(deftest ^:integration test-stdio-bridge-lifecycle
  (let [b (stdio/->stdio-bridge :bridge/echo)]

    (testing "start-bridge! spawns echo server and completes handshake"
      (let [result (bridge/start-bridge! b {:command echo-server-command})]
        (is (true? (:success? result))
            (str "start-bridge! failed: " (:errors result)))
        (is (= "echo-mcp" (get-in result [:metadata :server-info :name])))))

    (testing "bridge-status shows connected"
      (let [status (bridge/bridge-status b)]
        (is (true? (:connected? status)))
        (is (= :stdio (:transport status)))
        (is (pos? (:uptime-ms status)))))

    (testing "list-remote-tools discovers echo and add tools"
      (let [tools (bridge/list-remote-tools b)]
        (is (= 2 (count tools)))
        (is (= #{"echo" "add"} (set (map :name tools))))
        ;; Verify schema structure
        (let [echo-tool (first (filter #(= "echo" (:name %)) tools))]
          (is (= "Echo back the input message" (:description echo-tool)))
          (is (= "object" (get-in echo-tool [:inputSchema :type]))))))

    (testing "call-tool echo returns input"
      (let [result (bridge/call-tool b "echo" {"message" "hello bridge"})]
        (is (not (:isError result)))
        (is (= "hello bridge" (-> result :content first :text)))))

    (testing "call-tool add returns sum"
      (let [result (bridge/call-tool b "add" {"a" 17 "b" 25})]
        (is (not (:isError result)))
        (is (= "42" (-> result :content first :text)))))

    (testing "call-tool unknown tool returns error"
      (let [result (bridge/call-tool b "nonexistent" {})]
        (is (true? (:isError result)))
        (is (str/includes? (-> result :content first :text) "Unknown tool"))))

    (testing "stop-bridge! shuts down cleanly"
      (let [result (bridge/stop-bridge! b)]
        (is (true? (:success? result)))))

    (testing "bridge-status shows disconnected after stop"
      (let [status (bridge/bridge-status b)]
        (is (false? (:connected? status)))))))

(deftest ^:integration test-stdio-bridge-proxy-tools
  (let [b (stdio/->stdio-bridge :bridge/proxy-test)]
    ;; Start bridge
    (bridge/start-bridge! b {:command echo-server-command})

    (testing "addon-tools generates proxy tool-defs"
      (let [tools (addon/addon-tools b)]
        (is (= 2 (count tools)))
        (is (= #{"proxy-test:echo" "proxy-test:add"}
               (set (map :name tools))))
        ;; Each has a handler
        (is (every? fn? (map :handler tools)))
        ;; Each has bridge-source
        (is (every? #(= "proxy-test" (:bridge-source %)) tools))))

    (testing "proxy handler for echo works"
      (let [echo-tool (first (filter #(= "proxy-test:echo" (:name %))
                                     (addon/addon-tools b)))
            result    ((:handler echo-tool) {"message" "proxied!"})]
        (is (= "text" (:type result)))
        (is (= "proxied!" (:text result)))))

    (testing "proxy handler for add works"
      (let [add-tool (first (filter #(= "proxy-test:add" (:name %))
                                    (addon/addon-tools b)))
            result   ((:handler add-tool) {"a" 100 "b" 200})]
        (is (= "text" (:type result)))
        (is (= "300" (:text result)))))

    ;; Cleanup
    (bridge/stop-bridge! b)))

(deftest ^:integration test-stdio-bridge-register-bridge
  (let [b (stdio/->stdio-bridge :bridge/full)]
    (testing "register-bridge! does full lifecycle"
      (let [result (bridge/register-bridge! b {:command echo-server-command})]
        (is (true? (:success? result)))
        (is (= "full" (:bridge-prefix result)))))

    (testing "bridge appears in list-bridges"
      (let [bridges (bridge/list-bridges)]
        (is (= 1 (count bridges)))
        (is (= :bridge/full (:name (first bridges))))
        (is (true? (get-in (first bridges) [:status :connected?])))))

    (testing "bridge-connected? is true"
      (is (true? (bridge/bridge-connected? :bridge/full))))

    (testing "can call tool via get-bridge"
      (let [result (bridge/call-tool (bridge/get-bridge :bridge/full)
                                     "echo" {"message" "via registry"})]
        (is (= "via registry" (-> result :content first :text)))))

    (testing "unregister-bridge! cleans up"
      (bridge/unregister-bridge! :bridge/full)
      (is (nil? (bridge/get-bridge :bridge/full)))
      (is (= [] (bridge/list-bridges))))))

(deftest ^:integration test-stdio-bridge-bad-command
  (let [b (stdio/->stdio-bridge :bridge/bad)]
    (testing "start-bridge! with invalid command returns error"
      (let [result (bridge/start-bridge! b {:command ["nonexistent-binary-xyz"]})]
        (is (false? (:success? result)))
        (is (seq (:errors result)))))))
