(ns hive-mcp.addons.tool-invoke-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.addons.runtime-ports :as ports]))

(deftest hidden-registered-tools-remain-callable
  (let [calls (atom [])
        catalog (fn [] [{:name "hidden" :deprecated true
                         :handler (fn [args] (swap! calls conj args) {:ok args})}])]
    (is (= {:ok {:request_id "r"}} (ports/invoke-registered-tool! catalog "hidden" {:request_id "r"})))
    (is (= [{:request_id "r"}] @calls))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ports/invoke-registered-tool! catalog "missing" {})))
    (is (= 1 (count @calls)))))

(deftest catalog-is-resolved-per-invocation
  (let [current (atom [{:name "tool" :handler (fn [_] :first)}])
        catalog (fn [] @current)]
    (is (= :first (ports/invoke-registered-tool! catalog "tool" {})))
    (reset! current [{:name "tool" :handler (fn [_] :second)}])
    (is (= :second (ports/invoke-registered-tool! catalog "tool" {})))
    (reset! current [{:name "tool"}])
    (is (thrown? clojure.lang.ExceptionInfo
                 (ports/invoke-registered-tool! catalog "tool" {})))))
