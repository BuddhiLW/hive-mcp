(ns {{top/ns}}.{{main/ns}}.addon-test
  "Tests for {{raw-name}} addon.

   Covers: protocol satisfaction, lifecycle (init/shutdown), tools,
   health reporting, idempotency, and error resilience."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [{{top/ns}}.{{main/ns}}.addon :as addon]
            [hive-mcp.addons.protocol :as proto]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn fresh-state-fixture
  "Reset addon state before each test."
  [f]
  ;; Ensure clean state — shutdown any previously initialized addon
  (let [a (addon/->addon)]
    (try (proto/shutdown! a) (catch Exception _)))
  (f))

(use-fixtures :each fresh-state-fixture)

;; =============================================================================
;; Protocol Satisfaction
;; =============================================================================

(deftest test-protocol-satisfaction
  (let [a (addon/->addon)]
    (is (satisfies? proto/IAddon a)
        "Addon must satisfy IAddon protocol")))

(deftest test-addon-identity
  (let [a (addon/->addon)]
    (is (= "{{artifact/id}}" (proto/addon-id a))
        "addon-id must match manifest ID")
    (is (= :native (proto/addon-type a))
        "addon-type must be :native")
    (is (contains? (proto/capabilities a) :tools)
        "capabilities must include :tools")))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(deftest test-initialize-success
  (let [a      (addon/->addon)
        result (proto/initialize! a {})]
    (is (:success? result)
        "initialize! must return {:success? true}")
    (is (empty? (:errors result))
        "No errors on successful init")))

(deftest test-initialize-idempotent
  (let [a (addon/->addon)]
    (proto/initialize! a {})
    (let [result (proto/initialize! a {})]
      (is (:success? result)
          "Re-init must succeed")
      (is (:already-initialized? result)
          "Re-init must flag already-initialized?"))))

(deftest test-shutdown-success
  (let [a (addon/->addon)]
    (proto/initialize! a {})
    (let [result (proto/shutdown! a)]
      (is (:success? result)
          "shutdown! must return {:success? true}"))))

(deftest test-shutdown-idempotent
  (let [a (addon/->addon)]
    ;; Shutdown without init should not throw
    (let [result (proto/shutdown! a)]
      (is (:success? result)
          "shutdown! on uninitialized addon must succeed"))))

;; =============================================================================
;; Tools
;; =============================================================================

(deftest test-tools-non-empty
  (let [a     (addon/->addon)
        tools (proto/tools a)]
    (is (seq tools)
        "tools must return at least one tool-def")
    (is (every? :name tools)
        "Each tool-def must have :name")
    (is (every? :handler tools)
        "Each tool-def must have :handler")
    (is (every? :inputSchema tools)
        "Each tool-def must have :inputSchema")))

(deftest test-hello-tool-handler
  (let [a       (addon/->addon)
        tools   (proto/tools a)
        hello   (first (filter #(= "hello" (:name %)) tools))
        handler (:handler hello)]
    (is (some? hello) "hello tool must exist")

    (testing "with name param"
      (let [result (handler {:name "Hive"})]
        (is (= "text" (:type result)))
        (is (clojure.string/includes? (:text result) "Hive"))))

    (testing "without name param"
      (let [result (handler {})]
        (is (= "text" (:type result)))
        (is (clojure.string/includes? (:text result) "world"))))))

;; =============================================================================
;; Health
;; =============================================================================

(deftest test-health-when-down
  (let [a      (addon/->addon)
        health (proto/health a)]
    (is (= :down (:status health))
        "Health must be :down before init")))

(deftest test-health-when-ok
  (let [a (addon/->addon)]
    (proto/initialize! a {})
    (let [health (proto/health a)]
      (is (= :ok (:status health))
          "Health must be :ok after init")
      (is (number? (get-in health [:details :uptime-ms]))
          "Uptime must be reported"))))

;; =============================================================================
;; Schema Extensions
;; =============================================================================

(deftest test-schema-extensions
  (let [a (addon/->addon)]
    (is (map? (proto/schema-extensions a))
        "schema-extensions must return a map (possibly empty)")))

;; =============================================================================
;; Constructor Variants
;; =============================================================================

(deftest test-constructor-variants
  (testing "zero-arg"
    (let [a (addon/->addon)]
      (is (= "{{artifact/id}}" (proto/addon-id a)))))

  (testing "custom id"
    (let [a (addon/->addon "custom.id")]
      (is (= "custom.id" (proto/addon-id a)))))

  (testing "custom id + config"
    (let [a (addon/->addon "custom.id" {:api-key "test"})]
      (is (= "custom.id" (proto/addon-id a))))))

;; =============================================================================
;; init-as-addon! Entry Point
;; =============================================================================

(deftest test-init-as-addon-entry-point
  (let [result (addon/init-as-addon!)]
    (is (satisfies? proto/IAddon result)
        "init-as-addon! must return an IAddon instance")
    (is (= "{{artifact/id}}" (proto/addon-id result))
        "Returned addon must have correct ID")))
