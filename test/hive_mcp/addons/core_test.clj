(ns hive-mcp.addons.core-test
  "Tests for the addon plugin architecture.

   Covers:
   - IAddon protocol implementation
   - Registry operations (register, get, list, unregister)
   - Lifecycle management (init, shutdown)
   - Tool aggregation from active addons
   - Capability queries
   - Dependency checking
   - Edge cases (double-register, shutdown-inactive, etc.)
   - NoopAddon and ExampleAddon reference implementations"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string]
            [hive-mcp.addons.core :as addons]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-registry-fixture
  "Reset the addon registry before each test."
  [f]
  (addons/reset-registry!)
  (f)
  (addons/reset-registry!))

(use-fixtures :each reset-registry-fixture)

;; =============================================================================
;; Custom Test Addon
;; =============================================================================

(defrecord TestAddon [id version-str init-count shutdown-count state deps]
  addons/IAddon

  (addon-name [_] id)
  (addon-version [_] version-str)

  (addon-info [_]
    {:name id
     :version version-str
     :description (str "Test addon: " (name id))
     :author "test"
     :license "MIT"
     :dependencies deps
     :capabilities #{:tools :memory-store}})

  (init! [_ opts]
    (swap! init-count inc)
    (reset! state {:initialized true :opts opts})
    {:success? true
     :errors []
     :metadata {:init-count @init-count}})

  (shutdown! [_]
    (swap! shutdown-count inc)
    (reset! state nil)
    {:success? true
     :errors []})

  (addon-tools [_]
    [{:name (str (name id) "_search")
      :description (str "Search from " (name id))
      :inputSchema {:type "object"
                    :properties {"query" {:type "string"}}
                    :required ["query"]}
      :handler (fn [{:keys [query]}]
                 {:type "text" :text (str "result for: " query)})}
     {:name (str (name id) "_index")
      :description (str "Index from " (name id))
      :inputSchema {:type "object"
                    :properties {"path" {:type "string"}}
                    :required ["path"]}
      :handler (fn [{:keys [path]}]
                 {:type "text" :text (str "indexed: " path)})}])

  (addon-capabilities [_]
    #{:tools :memory-store}))

(defn ->test-addon
  "Create a test addon with tracking atoms."
  ([id] (->test-addon id "1.0.0" #{}))
  ([id version] (->test-addon id version #{}))
  ([id version deps]
   (->TestAddon id version (atom 0) (atom 0) (atom nil) deps)))

;; Addon that fails on init
(defrecord FailingAddon [id]
  addons/IAddon
  (addon-name [_] id)
  (addon-version [_] "0.1.0")
  (addon-info [_]
    {:name id :version "0.1.0" :description "Addon that fails"
     :author "test" :license "MIT" :dependencies #{} :capabilities #{}})
  (init! [_ _opts]
    {:success? false :errors ["Intentional init failure"]})
  (shutdown! [_]
    {:success? true :errors []})
  (addon-tools [_] [])
  (addon-capabilities [_] #{}))

;; Addon that throws on init
(defrecord ThrowingAddon [id]
  addons/IAddon
  (addon-name [_] id)
  (addon-version [_] "0.1.0")
  (addon-info [_]
    {:name id :version "0.1.0" :description "Addon that throws"
     :author "test" :license "MIT" :dependencies #{} :capabilities #{}})
  (init! [_ _opts]
    (throw (ex-info "Boom!" {:addon id})))
  (shutdown! [_]
    {:success? true :errors []})
  (addon-tools [_] [])
  (addon-capabilities [_] #{}))

;; =============================================================================
;; IAddon Protocol Tests
;; =============================================================================

(deftest test-iaddon-protocol-satisfaction
  (testing "NoopAddon satisfies IAddon"
    (is (satisfies? addons/IAddon (addons/->noop-addon))))

  (testing "ExampleAddon satisfies IAddon"
    (is (satisfies? addons/IAddon (addons/->example-addon))))

  (testing "TestAddon satisfies IAddon"
    (is (satisfies? addons/IAddon (->test-addon :test)))))

(deftest test-noop-addon
  (let [addon (addons/->noop-addon :my-noop "2.0.0")]
    (testing "name returns constructor arg"
      (is (= :my-noop (addons/addon-name addon))))

    (testing "version returns constructor arg"
      (is (= "2.0.0" (addons/addon-version addon))))

    (testing "info returns correct map"
      (let [info (addons/addon-info addon)]
        (is (= :my-noop (:name info)))
        (is (= "2.0.0" (:version info)))
        (is (string? (:description info)))
        (is (= #{} (:capabilities info)))))

    (testing "init succeeds"
      (let [result (addons/init! addon {})]
        (is (true? (:success? result)))
        (is (empty? (:errors result)))))

    (testing "shutdown succeeds"
      (let [result (addons/shutdown! addon)]
        (is (true? (:success? result)))
        (is (empty? (:errors result)))))

    (testing "tools returns empty vector"
      (is (= [] (addons/addon-tools addon))))

    (testing "capabilities returns empty set"
      (is (= #{} (addons/addon-capabilities addon))))))

(deftest test-noop-addon-defaults
  (testing "zero-arg constructor defaults"
    (let [addon (addons/->noop-addon)]
      (is (= :noop (addons/addon-name addon)))
      (is (= "0.0.0" (addons/addon-version addon)))))

  (testing "one-arg constructor"
    (let [addon (addons/->noop-addon :custom)]
      (is (= :custom (addons/addon-name addon)))
      (is (= "0.0.0" (addons/addon-version addon))))))

(deftest test-example-addon
  (let [addon (addons/->example-addon :my-example)]
    (testing "basic info"
      (is (= :my-example (addons/addon-name addon)))
      (is (= "1.0.0" (addons/addon-version addon)))
      (is (= #{:tools} (addons/addon-capabilities addon))))

    (testing "contributes one tool"
      (let [tools (addons/addon-tools addon)]
        (is (= 1 (count tools)))
        (is (= "my-example_ping" (:name (first tools))))
        (is (string? (:description (first tools))))
        (is (map? (:inputSchema (first tools))))
        (is (fn? (:handler (first tools))))))

    (testing "tool handler works"
      (let [handler (:handler (first (addons/addon-tools addon)))
            result (handler {:message "hello"})]
        (is (= "text" (:type result)))
        (is (clojure.string/includes? (:text result) "hello"))
        (is (clojure.string/includes? (:text result) "my-example"))))

    (testing "lifecycle works"
      (let [init-result (addons/init! addon {:config {:key "val"}})]
        (is (true? (:success? init-result)))
        (let [shutdown-result (addons/shutdown! addon)]
          (is (true? (:success? shutdown-result))))))))

;; =============================================================================
;; Registry Tests
;; =============================================================================

(deftest test-register-addon
  (let [addon (addons/->noop-addon :test-reg)]
    (testing "register succeeds"
      (let [result (addons/register-addon! addon)]
        (is (true? (:success? result)))
        (is (= :test-reg (:addon-name result)))))

    (testing "addon is registered"
      (is (true? (addons/addon-registered? :test-reg))))

    (testing "can retrieve addon"
      (is (some? (addons/get-addon :test-reg)))
      (is (= :test-reg (addons/addon-name (addons/get-addon :test-reg)))))

    (testing "double register fails"
      (let [result (addons/register-addon! (addons/->noop-addon :test-reg))]
        (is (false? (:success? result)))
        (is (seq (:errors result)))))))

(deftest test-register-validates-protocol
  (testing "non-IAddon throws assertion error"
    (is (thrown? AssertionError
                 (addons/register-addon! {:not "an addon"})))))

(deftest test-get-addon-entry
  (let [addon (addons/->noop-addon :entry-test)]
    (addons/register-addon! addon)

    (testing "returns full entry with metadata"
      (let [entry (addons/get-addon-entry :entry-test)]
        (is (some? entry))
        (is (= addon (:addon entry)))
        (is (= :registered (:state entry)))
        (is (instance? java.time.Instant (:registered-at entry)))
        (is (nil? (:init-time entry)))))

    (testing "returns nil for unknown addon"
      (is (nil? (addons/get-addon-entry :nonexistent))))))

(deftest test-list-addons
  (testing "empty registry returns empty vector"
    (is (= [] (addons/list-addons))))

  (testing "lists all registered addons"
    (addons/register-addon! (addons/->noop-addon :addon-a "1.0.0"))
    (addons/register-addon! (addons/->noop-addon :addon-b "2.0.0"))

    (let [addons-list (addons/list-addons)]
      (is (= 2 (count addons-list)))
      (is (= #{:addon-a :addon-b} (set (map :name addons-list))))
      (is (every? #(= :registered (:state %)) addons-list))
      (is (every? #(instance? java.time.Instant (:registered-at %)) addons-list)))))

(deftest test-unregister-addon
  (let [addon (->test-addon :unreg-test)]
    (addons/register-addon! addon)

    (testing "unregister succeeds for registered addon"
      (let [result (addons/unregister-addon! :unreg-test)]
        (is (true? (:success? result)))
        (is (false? (addons/addon-registered? :unreg-test)))))

    (testing "unregister fails for unknown addon"
      (let [result (addons/unregister-addon! :nonexistent)]
        (is (false? (:success? result)))
        (is (seq (:errors result)))))))

(deftest test-unregister-calls-shutdown-if-active
  (let [addon (->test-addon :unreg-active)]
    (addons/register-addon! addon)
    (addons/init-addon! :unreg-active)

    (testing "addon is active before unregister"
      (is (= :active (:state (addons/get-addon-entry :unreg-active)))))

    (addons/unregister-addon! :unreg-active)

    (testing "shutdown was called"
      (is (= 1 @(.shutdown-count addon))))

    (testing "addon is removed"
      (is (false? (addons/addon-registered? :unreg-active))))))

;; =============================================================================
;; Lifecycle Tests
;; =============================================================================

(deftest test-init-addon
  (let [addon (->test-addon :init-test)]
    (addons/register-addon! addon)

    (testing "init succeeds"
      (let [result (addons/init-addon! :init-test {:config {:db "test"}})]
        (is (true? (:success? result)))
        (is (= :init-test (:addon-name result)))
        (is (number? (:elapsed-ms result)))))

    (testing "state transitions to active"
      (is (= :active (:state (addons/get-addon-entry :init-test)))))

    (testing "init-time is set"
      (is (instance? java.time.Instant
                     (:init-time (addons/get-addon-entry :init-test)))))

    (testing "init! was called with opts"
      (is (= 1 @(.init-count addon)))
      (is (= {:db "test"} (get-in @(.state addon) [:opts :config]))))))

(deftest test-init-idempotent
  (let [addon (->test-addon :idempotent-init)]
    (addons/register-addon! addon)
    (addons/init-addon! :idempotent-init)

    (testing "second init returns already-active"
      (let [result (addons/init-addon! :idempotent-init)]
        (is (true? (:success? result)))
        (is (true? (:already-active? result)))))

    (testing "init! was only called once"
      (is (= 1 @(.init-count addon))))))

(deftest test-init-unregistered
  (testing "init for unknown addon returns error"
    (let [result (addons/init-addon! :unknown)]
      (is (false? (:success? result)))
      (is (seq (:errors result))))))

(deftest test-init-failing-addon
  (let [addon (->FailingAddon :fail-init)]
    (addons/register-addon! addon)

    (testing "init returns failure from addon"
      (let [result (addons/init-addon! :fail-init)]
        (is (false? (:success? result)))
        (is (= ["Intentional init failure"] (:errors result)))))

    (testing "state transitions to error"
      (is (= :error (:state (addons/get-addon-entry :fail-init)))))))

(deftest test-init-throwing-addon
  (let [addon (->ThrowingAddon :throw-init)]
    (addons/register-addon! addon)

    (testing "init catches exception and returns error"
      (let [result (addons/init-addon! :throw-init)]
        (is (false? (:success? result)))
        (is (seq (:errors result)))))

    (testing "state transitions to error"
      (is (= :error (:state (addons/get-addon-entry :throw-init)))))))

(deftest test-shutdown-addon
  (let [addon (->test-addon :shutdown-test)]
    (addons/register-addon! addon)
    (addons/init-addon! :shutdown-test)

    (testing "shutdown succeeds"
      (let [result (addons/shutdown-addon! :shutdown-test)]
        (is (true? (:success? result)))
        (is (= :shutdown-test (:addon-name result)))))

    (testing "state transitions to registered"
      (is (= :registered (:state (addons/get-addon-entry :shutdown-test)))))

    (testing "shutdown! was called"
      (is (= 1 @(.shutdown-count addon))))))

(deftest test-shutdown-inactive
  (let [addon (addons/->noop-addon :inactive-shutdown)]
    (addons/register-addon! addon)

    (testing "shutdown on non-active addon returns already-inactive"
      (let [result (addons/shutdown-addon! :inactive-shutdown)]
        (is (true? (:success? result)))
        (is (true? (:already-inactive? result)))))))

(deftest test-shutdown-unregistered
  (testing "shutdown for unknown addon returns error"
    (let [result (addons/shutdown-addon! :unknown)]
      (is (false? (:success? result)))
      (is (seq (:errors result))))))

(deftest test-init-all
  (addons/register-addon! (->test-addon :all-a "1.0.0"))
  (addons/register-addon! (->test-addon :all-b "2.0.0"))

  (testing "init-all initializes all registered addons"
    (let [results (addons/init-all!)]
      (is (= 2 (count results)))
      (is (true? (get-in results [:all-a :success?])))
      (is (true? (get-in results [:all-b :success?])))))

  (testing "all addons are now active"
    (is (every? #(= :active (:state %))
                (map #(addons/get-addon-entry %) [:all-a :all-b])))))

(deftest test-init-all-skips-active
  (addons/register-addon! (->test-addon :skip-a))
  (addons/register-addon! (->test-addon :skip-b))
  (addons/init-addon! :skip-a)

  (testing "init-all skips already-active addons"
    (let [results (addons/init-all!)]
      ;; Only skip-b should be in results (skip-a was already active)
      (is (= 1 (count results)))
      (is (contains? results :skip-b)))))

(deftest test-shutdown-all
  (addons/register-addon! (->test-addon :shut-a))
  (addons/register-addon! (->test-addon :shut-b))
  (addons/init-all!)

  (testing "shutdown-all shuts down all active addons"
    (let [results (addons/shutdown-all!)]
      (is (= 2 (count results)))
      (is (true? (get-in results [:shut-a :success?])))
      (is (true? (get-in results [:shut-b :success?])))))

  (testing "all addons are now registered (not active)"
    (is (every? #(= :registered (:state %))
                (map #(addons/get-addon-entry %) [:shut-a :shut-b])))))

;; =============================================================================
;; Tool Aggregation Tests
;; =============================================================================

(deftest test-active-addon-tools
  (testing "no tools when no addons"
    (is (= [] (addons/active-addon-tools))))

  (testing "no tools when addon is registered but not active"
    (addons/register-addon! (->test-addon :inactive-tools))
    (is (= [] (addons/active-addon-tools))))

  (testing "returns tools from active addon"
    (addons/init-addon! :inactive-tools)
    (let [tools (addons/active-addon-tools)]
      (is (= 2 (count tools)))
      (is (= "inactive-tools_search" (:name (first tools))))
      (is (= "inactive-tools_index" (:name (second tools))))))

  (testing "tools annotated with :addon-source"
    (let [tools (addons/active-addon-tools)]
      (is (every? #(= :inactive-tools (:addon-source %)) tools)))))

(deftest test-addon-tools-multiple-addons
  (addons/register-addon! (->test-addon :tools-a))
  (addons/register-addon! (->test-addon :tools-b))
  (addons/init-all!)

  (testing "aggregates tools from all active addons"
    (let [tools (addons/active-addon-tools)]
      (is (= 4 (count tools)))
      (let [tool-names (set (map :name tools))]
        (is (contains? tool-names "tools-a_search"))
        (is (contains? tool-names "tools-a_index"))
        (is (contains? tool-names "tools-b_search"))
        (is (contains? tool-names "tools-b_index"))))))

(deftest test-addon-tools-by-name
  (addons/register-addon! (->test-addon :by-name))
  (addons/init-addon! :by-name)

  (testing "returns tools for specific addon"
    (let [tools (addons/addon-tools-by-name :by-name)]
      (is (= 2 (count tools)))))

  (testing "returns empty for unknown addon"
    (is (= [] (addons/addon-tools-by-name :nonexistent))))

  (testing "returns empty for non-active addon"
    (addons/register-addon! (addons/->noop-addon :not-active))
    (is (= [] (addons/addon-tools-by-name :not-active)))))

(deftest test-noop-addon-no-tools-capability
  (let [addon (addons/->noop-addon :no-cap)]
    (addons/register-addon! addon)
    (addons/init-addon! :no-cap)

    (testing "noop addon has no :tools capability, excluded from active-addon-tools"
      ;; NoopAddon capabilities is #{}, so it doesn't have :tools
      (is (= [] (addons/active-addon-tools))))))

;; =============================================================================
;; Capability Query Tests
;; =============================================================================

(deftest test-addons-with-capability
  (addons/register-addon! (->test-addon :cap-a))  ;; Has :tools, :memory-store
  (addons/register-addon! (addons/->noop-addon :cap-b))  ;; Has #{}
  (addons/init-all!)

  (testing "finds addons with :tools capability"
    (is (= [:cap-a] (addons/addons-with-capability :tools))))

  (testing "finds addons with :memory-store capability"
    (is (= [:cap-a] (addons/addons-with-capability :memory-store))))

  (testing "no addons for :kg-store capability"
    (is (= [] (addons/addons-with-capability :kg-store)))))

(deftest test-all-capabilities
  (addons/register-addon! (->test-addon :allcap-a))
  (addons/register-addon! (addons/->example-addon :allcap-b))
  (addons/init-all!)

  (testing "aggregates capabilities across addons"
    (let [caps (addons/all-capabilities)]
      (is (= [:allcap-a] (get caps :memory-store)))
      (is (= #{:allcap-a :allcap-b} (set (get caps :tools)))))))

;; =============================================================================
;; Dependency Tests
;; =============================================================================

(deftest test-check-dependencies
  (addons/register-addon! (addons/->noop-addon :dep-base))
  (addons/register-addon! (->test-addon :dep-child "1.0.0" #{:dep-base}))

  (testing "satisfied when all deps present"
    (let [result (addons/check-dependencies :dep-child)]
      (is (true? (:satisfied? result)))
      (is (empty? (:missing result)))
      (is (= #{:dep-base} (:available result)))))

  (testing "unsatisfied when dep missing"
    (addons/register-addon! (->test-addon :missing-dep "1.0.0" #{:nonexistent}))
    (let [result (addons/check-dependencies :missing-dep)]
      (is (false? (:satisfied? result)))
      (is (= #{:nonexistent} (:missing result))))))

(deftest test-check-dependencies-unknown-addon
  (testing "unknown addon returns not-satisfied"
    (let [result (addons/check-dependencies :totally-unknown)]
      (is (false? (:satisfied? result))))))

;; =============================================================================
;; Registry Management Tests
;; =============================================================================

(deftest test-reset-registry
  (addons/register-addon! (->test-addon :reset-a))
  (addons/register-addon! (->test-addon :reset-b))
  (addons/init-all!)

  (testing "reset shuts down and clears all"
    (let [results (addons/reset-registry!)]
      (is (= 2 (count results)))
      (is (= [] (addons/list-addons))))))

(deftest test-registry-status
  (testing "empty registry status"
    (let [status (addons/registry-status)]
      (is (= 0 (:total status)))
      (is (= 0 (:active status)))
      (is (= 0 (:tool-count status)))))

  (addons/register-addon! (->test-addon :status-a))
  (addons/register-addon! (->test-addon :status-b))
  (addons/init-addon! :status-a)

  (testing "mixed registry status"
    (let [status (addons/registry-status)]
      (is (= 2 (:total status)))
      (is (= 1 (:active status)))
      (is (= 1 (:registered status)))
      (is (= 0 (:error status)))
      (is (= 2 (:tool-count status)))  ;; Only status-a active, 2 tools
      (is (map? (:capabilities status)))
      (is (= 2 (count (:addons status)))))))

;; =============================================================================
;; Edge Case Tests
;; =============================================================================

(deftest test-re-init-after-error
  (let [addon (->FailingAddon :re-init)]
    (addons/register-addon! addon)
    (addons/init-addon! :re-init)

    (testing "addon is in error state"
      (is (= :error (:state (addons/get-addon-entry :re-init)))))

    (testing "can re-init after error (not blocked by active check)"
      (let [result (addons/init-addon! :re-init)]
        ;; Will still fail because FailingAddon always fails
        (is (false? (:success? result)))))))

(deftest test-multiple-register-unregister-cycles
  (testing "can register, unregister, and re-register"
    (let [addon (addons/->noop-addon :cycle)]
      (is (true? (:success? (addons/register-addon! addon))))
      (is (true? (:success? (addons/unregister-addon! :cycle))))
      (is (false? (addons/addon-registered? :cycle)))
      ;; Re-register with new instance
      (is (true? (:success? (addons/register-addon! (addons/->noop-addon :cycle))))))))

(deftest test-get-addon-returns-nil-for-unknown
  (is (nil? (addons/get-addon :nonexistent))))

(deftest test-addon-registered-returns-false-for-unknown
  (is (false? (addons/addon-registered? :nonexistent))))
