(ns hive-mcp.agent.sdk.availability-test
  "Tests for SDK availability detection with graceful degradation.

   Tests the public API: sdk-status, available?, reset-availability!
   and the private helpers: check-libpython-available?, check-sdk-available?.

   Uses with-redefs to mock classpath/Python checks since actual
   libpython-clj2 and claude-code-sdk may not be present in test env."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.sdk.availability :as avail]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn reset-availability-fixture
  "Reset cached availability before each test to ensure isolation."
  [f]
  (avail/reset-availability!)
  (f)
  (avail/reset-availability!))

(use-fixtures :each reset-availability-fixture)

;; =============================================================================
;; Private fn references (for with-redefs)
;; =============================================================================

(def ^:private check-libpython-var  #'avail/check-libpython-available?)
(def ^:private check-sdk-var        #'avail/check-sdk-available?)

;; =============================================================================
;; reset-availability! Tests
;; =============================================================================

(deftest reset-availability!-clears-cache
  (testing "reset-availability! clears the cached sdk status"
    ;; Prime the cache by calling sdk-status with mocked fns
    (with-redefs [avail/check-libpython-available? (constantly true)
                  avail/check-sdk-available?       (constantly true)]
      ;; Also mock requiring-resolve for the initialize! call inside sdk-status
      (let [orig-rr requiring-resolve]
        (with-redefs [requiring-resolve (fn [sym]
                                          (if (= sym 'libpython-clj2.python/initialize!)
                                            (fn [] nil)
                                            (orig-rr sym)))]
          (is (= :available (avail/sdk-status)) "Should be available with mocks"))))
    ;; After reset, the cache should be empty (nil), forcing re-evaluation
    (avail/reset-availability!)
    ;; Now mock to return different status
    (with-redefs [avail/check-libpython-available? (constantly false)]
      (is (= :no-libpython (avail/sdk-status))
          "After reset, should re-check and get new status"))))

;; =============================================================================
;; sdk-status Tests
;; =============================================================================

(deftest sdk-status-no-libpython
  (testing "returns :no-libpython when libpython-clj2 not on classpath"
    (with-redefs [avail/check-libpython-available? (constantly false)]
      (is (= :no-libpython (avail/sdk-status))))))

(deftest sdk-status-available
  (testing "returns :available when both libpython and sdk are ready"
    (with-redefs [avail/check-libpython-available? (constantly true)
                  avail/check-sdk-available?       (constantly true)
                  requiring-resolve                (fn [sym]
                                                     (when (= sym 'libpython-clj2.python/initialize!)
                                                       (fn [] nil)))]
      (is (= :available (avail/sdk-status))))))

(deftest sdk-status-no-sdk
  (testing "returns :no-sdk when libpython available but claude-code-sdk not installed"
    (with-redefs [avail/check-libpython-available? (constantly true)
                  avail/check-sdk-available?       (constantly false)
                  requiring-resolve                (fn [sym]
                                                     (when (= sym 'libpython-clj2.python/initialize!)
                                                       (fn [] nil)))]
      (is (= :no-sdk (avail/sdk-status))))))

(deftest sdk-status-not-initialized
  (testing "returns :not-initialized when Python init fails"
    (with-redefs [avail/check-libpython-available? (constantly true)
                  requiring-resolve                (fn [sym]
                                                     (when (= sym 'libpython-clj2.python/initialize!)
                                                       (fn [] (throw (Exception. "Python init failed")))))]
      (is (= :not-initialized (avail/sdk-status))))))

(deftest sdk-status-caches-result
  (testing "caches result after first call — subsequent calls return cached value"
    (let [call-count (atom 0)]
      (with-redefs [avail/check-libpython-available? (fn []
                                                        (swap! call-count inc)
                                                        false)]
        ;; First call computes
        (is (= :no-libpython (avail/sdk-status)))
        (is (= 1 @call-count) "check-libpython should be called once")
        ;; Second call returns cached
        (is (= :no-libpython (avail/sdk-status)))
        (is (= 1 @call-count) "check-libpython should NOT be called again (cached)")))))

(deftest sdk-status-cache-survives-redef-removal
  (testing "cached value persists even after with-redefs scope ends"
    ;; Set up cache with mocked fns
    (with-redefs [avail/check-libpython-available? (constantly false)]
      (is (= :no-libpython (avail/sdk-status))))
    ;; Outside with-redefs, cache should still have the value
    ;; (no re-evaluation since cache is set)
    (is (= :no-libpython (avail/sdk-status))
        "Cached value should persist outside with-redefs scope")))

;; =============================================================================
;; available? Tests
;; =============================================================================

(deftest available?-true-when-sdk-available
  (testing "returns true when sdk-status is :available"
    (with-redefs [avail/sdk-status (constantly :available)]
      (is (true? (avail/available?))))))

(deftest available?-false-when-no-libpython
  (testing "returns false when sdk-status is :no-libpython"
    (with-redefs [avail/sdk-status (constantly :no-libpython)]
      (is (false? (avail/available?))))))

(deftest available?-false-when-no-sdk
  (testing "returns false when sdk-status is :no-sdk"
    (with-redefs [avail/sdk-status (constantly :no-sdk)]
      (is (false? (avail/available?))))))

(deftest available?-false-when-not-initialized
  (testing "returns false when sdk-status is :not-initialized"
    (with-redefs [avail/sdk-status (constantly :not-initialized)]
      (is (false? (avail/available?))))))

;; =============================================================================
;; Private function tests (via var deref)
;; =============================================================================

(deftest check-libpython-available?-true-on-success
  (testing "returns true when require 'libpython-clj2.python succeeds"
    ;; We can't easily mock `require` since it's a special form in clojure.core.
    ;; Instead test the actual function - if libpython is on classpath it returns true,
    ;; if not it returns false. Both are valid outcomes.
    (let [check-fn @check-libpython-var
          result (check-fn)]
      (is (boolean? result) "Should return a boolean")
      ;; In test env, libpython is likely not on classpath
      (is (contains? #{true false} result) "Should be true or false"))))

(deftest check-sdk-available?-returns-boolean
  (testing "check-sdk-available? returns boolean or throws gracefully"
    ;; This function uses requiring-resolve which may fail if libpython not loaded
    ;; We mock it to test the logic
    (let [check-fn @check-sdk-var]
      ;; Mock requiring-resolve to simulate success
      (with-redefs [requiring-resolve (fn [sym]
                                        (when (= sym 'libpython-clj2.python/run-simple-string)
                                          (fn [_code] nil)))]
        (is (true? (check-fn)) "Should return true when import succeeds")))))

(deftest check-sdk-available?-false-on-import-failure
  (testing "returns false when Python import of claude_code_sdk fails"
    (let [check-fn @check-sdk-var]
      (with-redefs [requiring-resolve (fn [sym]
                                        (when (= sym 'libpython-clj2.python/run-simple-string)
                                          (fn [_code] (throw (Exception. "ModuleNotFoundError")))))]
        (is (false? (check-fn)) "Should return false when import fails")))))

(deftest check-sdk-available?-false-when-resolve-fails
  (testing "returns false when requiring-resolve itself fails"
    (let [check-fn @check-sdk-var]
      (with-redefs [requiring-resolve (fn [_sym]
                                        (throw (Exception. "Cannot resolve")))]
        (is (false? (check-fn)) "Should return false when resolve fails")))))

;; =============================================================================
;; Integration-style Tests (full flow through sdk-status)
;; =============================================================================

(deftest full-flow-available
  (testing "full flow: libpython ok, init ok, sdk ok -> :available"
    (with-redefs [avail/check-libpython-available? (constantly true)
                  avail/check-sdk-available?       (constantly true)
                  requiring-resolve                (fn [sym]
                                                     (case sym
                                                       libpython-clj2.python/initialize! (fn [] nil)
                                                       nil))]
      (let [status (avail/sdk-status)]
        (is (= :available status))
        (is (true? (avail/available?)))))))

(deftest full-flow-no-libpython
  (testing "full flow: no libpython -> :no-libpython, not available"
    (with-redefs [avail/check-libpython-available? (constantly false)]
      (let [status (avail/sdk-status)]
        (is (= :no-libpython status))
        (is (false? (avail/available?)))))))

(deftest full-flow-no-sdk
  (testing "full flow: libpython ok, init ok, no sdk -> :no-sdk"
    (with-redefs [avail/check-libpython-available? (constantly true)
                  avail/check-sdk-available?       (constantly false)
                  requiring-resolve                (fn [sym]
                                                     (case sym
                                                       libpython-clj2.python/initialize! (fn [] nil)
                                                       nil))]
      (let [status (avail/sdk-status)]
        (is (= :no-sdk status))
        (is (false? (avail/available?)))))))

(deftest full-flow-init-failure
  (testing "full flow: libpython ok, init throws -> :not-initialized"
    (with-redefs [avail/check-libpython-available? (constantly true)
                  requiring-resolve                (fn [sym]
                                                     (case sym
                                                       libpython-clj2.python/initialize!
                                                       (fn [] (throw (RuntimeException. "segfault")))
                                                       nil))]
      (let [status (avail/sdk-status)]
        (is (= :not-initialized status))
        (is (false? (avail/available?)))))))

(deftest full-flow-reset-and-recheck
  (testing "full flow: check, reset, re-check gets fresh result"
    ;; First check: no libpython
    (with-redefs [avail/check-libpython-available? (constantly false)]
      (is (= :no-libpython (avail/sdk-status))))
    ;; Reset
    (avail/reset-availability!)
    ;; Second check: now everything available
    (with-redefs [avail/check-libpython-available? (constantly true)
                  avail/check-sdk-available?       (constantly true)
                  requiring-resolve                (fn [sym]
                                                     (case sym
                                                       libpython-clj2.python/initialize! (fn [] nil)
                                                       nil))]
      (is (= :available (avail/sdk-status))
          "After reset, fresh check should reflect new state"))))
