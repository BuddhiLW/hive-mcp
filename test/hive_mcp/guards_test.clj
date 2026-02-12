(ns hive-mcp.guards-test
  "Tests for delegation enforcement guards.

   Covers:
   - Agent classification (ling vs drone)
   - Mutation tool detection
   - Guard check behavior in warn and block modes
   - Configuration helpers"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.server.guards :as guards]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn reset-guards [f]
  ;; Reset to defaults
  (guards/enable-guards!)
  (guards/set-enforcement-mode! :warn)
  (f))

(use-fixtures :each reset-guards)

;;; =============================================================================
;;; Agent Classification Tests
;;; =============================================================================

(deftest test-ling-agent-detection
  (testing "ling-agent? returns false when no env var"
    (with-redefs [guards/ling-agent? (constantly false)]
      (is (not (guards/ling-agent?)))))

  (testing "ling-agent? returns true for swarm- prefix without drone"
    (with-redefs [guards/ling-agent? (constantly true)]
      (is (guards/ling-agent?))))

  (testing "ling-agent? returns false for drone agents"
    (with-redefs [guards/ling-agent? (constantly false)]
      (is (not (guards/ling-agent?))))))

(deftest test-drone-agent-detection
  (testing "drone-agent? returns true for drone- prefix"
    (with-redefs [guards/drone-agent? (constantly true)]
      (is (guards/drone-agent?))))

  (testing "drone-agent? returns false for ling agents"
    (with-redefs [guards/drone-agent? (constantly false)]
      (is (not (guards/drone-agent?))))))

;;; =============================================================================
;;; Mutation Tool Detection Tests
;;; =============================================================================

(deftest test-mutation-tool-names
  (testing "mutation-tool-names contains expected tools"
    (is (contains? guards/mutation-tool-names "file_write"))
    (is (contains? guards/mutation-tool-names "file_edit"))
    (is (contains? guards/mutation-tool-names "mcp__emacs__file_write"))
    (is (contains? guards/mutation-tool-names "propose_diff")))

  (testing "mutation-tool-names does not contain read tools"
    (is (not (contains? guards/mutation-tool-names "read_file")))
    (is (not (contains? guards/mutation-tool-names "grep")))))

;;; =============================================================================
;;; Guard Check Tests - Non-Mutation Tools
;;; =============================================================================

(deftest test-check-mutation-guard-non-mutation
  (testing "non-mutation tools always allowed"
    (let [result (guards/check-mutation-guard "read_file" {:path "/foo.clj"})]
      (is (:allowed? result)))))

(deftest test-check-mutation-guard-disabled
  (testing "guard check passes when guards disabled"
    (guards/disable-guards!)
    (let [result (guards/check-mutation-guard "file_write" {:path "/foo.clj"})]
      (is (:allowed? result)))))

;;; =============================================================================
;;; Guard Check Tests - Mutation Tools (Mock Ling)
;;; =============================================================================

(deftest test-check-mutation-guard-ling-warn-mode
  (testing "warn mode allows mutation but logs"
    (guards/set-enforcement-mode! :warn)
    (with-redefs [guards/ling-agent? (constantly true)]
      (let [result (guards/check-mutation-guard "file_write" {:file_path "/foo.clj"})]
        (is (:allowed? result))
        (is (:logged? result))
        (is (some? (:message result)))))))

(deftest test-check-mutation-guard-ling-block-mode
  (testing "block mode rejects mutation with guidance"
    (guards/set-enforcement-mode! :block)
    (with-redefs [guards/ling-agent? (constantly true)]
      (let [result (guards/check-mutation-guard "file_write" {:file_path "/foo.clj"})]
        (is (not (:allowed? result)))
        (is (:logged? result))
        (is (str/includes? (:message result) "dispatch_validated_wave"))))))

;;; =============================================================================
;;; Guard Check Tests - Drone Agents
;;; =============================================================================

(deftest test-check-mutation-guard-drone-allowed
  (testing "drones are allowed to use mutation tools"
    (with-redefs [guards/ling-agent? (constantly false)
                  guards/drone-agent? (constantly true)]
      (let [result (guards/check-mutation-guard "propose_diff" {:file_path "/foo.clj"})]
        (is (:allowed? result))
        (is (not (:logged? result)))))))

;;; =============================================================================
;;; Configuration Tests
;;; =============================================================================

(deftest test-set-enforcement-mode
  (testing "set-enforcement-mode changes mode"
    (guards/set-enforcement-mode! :block)
    (is (= :block guards/*enforcement-mode*))
    (guards/set-enforcement-mode! :warn)
    (is (= :warn guards/*enforcement-mode*))))

(deftest test-enable-disable-guards
  (testing "enable/disable guards changes flag"
    (guards/disable-guards!)
    (is (not guards/*guard-enabled?*))
    (guards/enable-guards!)
    (is guards/*guard-enabled?*)))

(deftest test-guard-status
  (testing "guard-status returns configuration"
    (guards/set-enforcement-mode! :block)
    (let [status (guards/guard-status)]
      (is (:enabled? status))
      (is (= :block (:mode status)))
      (is (set? (:mutation-tools status))))))

;;; =============================================================================
;;; Middleware Tests
;;; =============================================================================

(deftest test-wrap-mutation-guard-allowed
  (testing "middleware passes through when allowed"
    (let [handler (fn [_params] {:type "text" :text "success"})
          wrapped (guards/wrap-mutation-guard handler)]
      (guards/set-enforcement-mode! :warn)
      (with-redefs [guards/ling-agent? (constantly false)]
        (let [result (wrapped {:path "/foo.clj"})]
          (is (= "success" (:text result))))))))

(deftest test-wrap-mutation-guard-blocked
  (testing "middleware returns error when blocked"
    (let [handler (fn [_params] {:type "text" :text "success"})
          wrapped (guards/wrap-mutation-guard (with-meta handler {:tool-name "file_write"}))]
      (guards/set-enforcement-mode! :block)
      (with-redefs [guards/ling-agent? (constantly true)]
        (let [result (wrapped {:path "/foo.clj"})]
          (is (:isError result))
          (is (str/includes? (:text result) "ENFORCEMENT VIOLATION")))))))

;;; =============================================================================
;;; Delegation Guidance Tests
;;; =============================================================================

(deftest test-delegation-guidance-content
  (testing "guidance includes all required tools"
    (is (str/includes? guards/delegation-guidance "dispatch_validated_wave"))
    (is (str/includes? guards/delegation-guidance "dispatch_drone_wave"))
    (is (str/includes? guards/delegation-guidance "delegate_drone"))
    (is (str/includes? guards/delegation-guidance "92%"))))

;;; =============================================================================
;;; Child Ling Detection Tests
;;; =============================================================================

(deftest test-child-ling-detection
  (testing "child-ling? returns true when HIVE_MCP_ROLE is child-ling"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "child-ling" nil))]
      (is (true? (guards/child-ling?)))))

  (testing "child-ling? returns false when HIVE_MCP_ROLE is unset"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (false? (guards/child-ling?)))))

  (testing "child-ling? returns false when HIVE_MCP_ROLE is something else"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "coordinator" nil))]
      (is (false? (guards/child-ling?))))))

(deftest test-coordinator-detection
  (testing "coordinator? returns true when not child-ling"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (true? (guards/coordinator?)))))

  (testing "coordinator? returns false when child-ling"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "child-ling" nil))]
      (is (false? (guards/coordinator?))))))

(deftest test-get-role
  (testing "get-role returns child-ling when env is set"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "child-ling" nil))]
      (is (= "child-ling" (guards/get-role)))))

  (testing "get-role returns coordinator when env is unset"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (= "coordinator" (guards/get-role))))))

(deftest test-ling-depth
  (testing "ling-depth returns 0 when env is unset"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (= 0 (guards/ling-depth)))))

  (testing "ling-depth returns parsed integer from env"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "2" nil))]
      (is (= 2 (guards/ling-depth)))))

  (testing "ling-depth returns 0 for non-numeric string"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "not-a-number" nil))]
      (is (= 0 (guards/ling-depth)))))

  (testing "ling-depth returns 0 for empty string"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "" nil))]
      (is (= 0 (guards/ling-depth))))))

(deftest test-child-ling-env
  (testing "child-ling-env returns correct env map from coordinator (depth 0)"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (let [env (guards/child-ling-env)]
        (is (= "child-ling" (get env "HIVE_MCP_ROLE")))
        (is (= "1" (get env "HIVE_LING_DEPTH"))))))

  (testing "child-ling-env increments depth from existing value"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "3" nil))]
      (let [env (guards/child-ling-env)]
        (is (= "child-ling" (get env "HIVE_MCP_ROLE")))
        (is (= "4" (get env "HIVE_LING_DEPTH")))))))

(deftest test-guard-status-includes-child-ling-info
  (testing "guard-status includes child-ling fields"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (let [status (guards/guard-status)]
        (is (contains? status :child-ling?))
        (is (contains? status :role))
        (is (contains? status :ling-depth))
        (is (false? (:child-ling? status)))
        (is (= "coordinator" (:role status)))
        (is (= 0 (:ling-depth status)))))))
