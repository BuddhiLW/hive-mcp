(ns hive-mcp.tools.catchup.persona-buckets-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.catchup.bundle :as bundle]
            [hive-mcp.tools.catchup.bundle-cache :as cache]
            [hive-mcp.tools.catchup.hydration :as hydration]))

(deftest personas-select-different-memory-types-before-hydration
  (let [entries {"axiom" [{:id "law"}]
                 "decision" [{:id "decision-1"} {:id "decision-2"}]
                 "snippet" [{:id "code-1"}]
                 "convention" [{:id "convention-1"}]}
        hydrated (atom [])]
    (with-redefs-fn {#'bundle/query-all-scoped (fn [_] {:by-type entries :all []})
                    #'hydration/hydrate-buckets (fn [b] (swap! hydrated conj b) b)
                    #'cache/cached-bundle (fn [& _] (throw (Exception. "persona must not share cache")))}
      (fn []
        (let [explorer (bundle/query-catchup-bundle "project"
                         {:caps {:decisions 2 :snippets 0 :conventions 0 :axioms 0}})
              implementer (bundle/query-catchup-bundle "project"
                            {:caps {:decisions 0 :snippets 1 :conventions 1 :axioms 0}})]
          (is (= ["decision-1" "decision-2"] (mapv :id (:decisions explorer))))
          (is (empty? (:snippets explorer)))
          (is (empty? (:decisions implementer)))
          (is (= ["code-1"] (mapv :id (:snippets implementer))))
          (is (= ["convention-1"] (mapv :id (:conventions implementer))))
          (is (= ["law"] (mapv :id (:axioms explorer)) (mapv :id (:axioms implementer))))
          (is (= [explorer implementer] @hydrated)))))))

(deftest disabling-priority-principles-does-not-hide-ordinary-principles
  (let [result (#'bundle/split-by-type
                 {"principle" [{:id "mandatory-priority" :tags ["catchup-priority"]}
                               {:id "ordinary"}]} []
                 {:priority-principles 0 :principles 5})]
    (is (empty? (:priority-principles result)))
    (is (= ["ordinary"] (mapv :id (:principles result))))))
