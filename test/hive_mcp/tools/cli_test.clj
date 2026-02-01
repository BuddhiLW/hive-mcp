(ns hive-mcp.tools.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.tools.cli :as cli]))

;; =============================================================================
;; format-help tests
;; =============================================================================

(deftest format-help-test
  (testing "formats help text with all command names"
    (let [handlers {:foo (fn [_] nil)
                    :bar (fn [_] nil)
                    :baz (fn [_] nil)}
          help-text (cli/format-help handlers)]
      (is (str/includes? help-text "Available commands:"))
      (is (str/includes? help-text "foo"))
      (is (str/includes? help-text "bar"))
      (is (str/includes? help-text "baz"))))

  (testing "handles empty handlers map"
    (let [help-text (cli/format-help {})]
      (is (str/includes? help-text "Available commands:")))))

;; =============================================================================
;; make-cli-handler dispatcher tests
;; =============================================================================

(deftest make-cli-handler-dispatches-correctly
  (let [handlers {:status (fn [_] {:ok true})
                  :spawn (fn [{:keys [name]}] {:spawned name})}
        cli-handler (cli/make-cli-handler handlers)]

    (testing "dispatches to status command"
      (is (= {:ok true} (cli-handler {:command "status"}))))

    (testing "dispatches to spawn with params"
      (is (= {:spawned "foo"} (cli-handler {:command "spawn" :name "foo"}))))

    (testing "dispatches with keyword command"
      (is (= {:ok true} (cli-handler {:command :status}))))

    (testing "passes all params through to handler"
      (let [handlers {:echo (fn [params] params)}
            handler (cli/make-cli-handler handlers)
            params {:command "echo" :a 1 :b 2 :nested {:x "y"}}]
        (is (= params (handler params)))))))

(deftest make-cli-handler-returns-handler-result
  (testing "returns exact result from handler"
    (let [expected {:type "text" :text "result" :meta {:id 123}}
          handlers {:cmd (fn [_] expected)}
          handler (cli/make-cli-handler handlers)]
      (is (= expected (handler {:command "cmd"})))))

  (testing "returns nil if handler returns nil"
    (let [handlers {:noop (fn [_] nil)}
          handler (cli/make-cli-handler handlers)]
      (is (nil? (handler {:command "noop"}))))))

;; =============================================================================
;; Unknown command handling tests
;; =============================================================================

(deftest unknown-command-returns-error
  (let [handlers {:status (fn [_] :status)
                  :spawn (fn [_] :spawn)
                  :kill (fn [_] :kill)}
        cli-handler (cli/make-cli-handler handlers)]

    (testing "unknown command returns error map"
      (let [result (cli-handler {:command "bogus"})]
        (is (:isError result) "should have :isError flag")
        (is (string? (:text result)) "should have error text")))

    (testing "error mentions the unknown command"
      (let [result (cli-handler {:command "nonexistent"})]
        (is (str/includes? (:text result) "nonexistent"))))

    (testing "error lists available commands"
      (let [result (cli-handler {:command "bad"})]
        (is (str/includes? (:text result) "status"))
        (is (str/includes? (:text result) "spawn"))
        (is (str/includes? (:text result) "kill"))))

    (testing "doesn't throw exceptions for unknown commands"
      (is (map? (cli-handler {:command ""})))
      (is (map? (cli-handler {:command nil})))
      (is (map? (cli-handler {:command "!@#$%"}))))))

;; =============================================================================
;; Help command generation tests
;; =============================================================================

(deftest help-command-generation
  (let [handlers {:status (fn [_] nil)
                  :spawn (fn [_] nil)
                  :collect (fn [_] nil)}
        cli-handler (cli/make-cli-handler handlers)]

    (testing "help command returns text type"
      (let [result (cli-handler {:command "help"})]
        (is (= "text" (:type result)))))

    (testing "help lists all available subcommands"
      (let [result (cli-handler {:command "help"})]
        (is (str/includes? (:text result) "status"))
        (is (str/includes? (:text result) "spawn"))
        (is (str/includes? (:text result) "collect"))))

    (testing "help works with keyword command"
      (let [result (cli-handler {:command :help})]
        (is (= "text" (:type result)))
        (is (str/includes? (:text result) "Available commands:"))))))
