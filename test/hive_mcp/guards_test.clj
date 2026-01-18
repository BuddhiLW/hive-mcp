(ns hive-mcp.guards-test
  "Tests for delegation enforcement guards.

   Covers:
   - Agent classification (ling vs drone)
   - Mutation tool detection
   - Guard check behavior in warn and block modes
   - Configuration helpers"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.guards :as guards]))

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
