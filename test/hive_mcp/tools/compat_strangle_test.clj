(ns hive-mcp.tools.compat-strangle-test
  "Tests for Phase 2 strangle: deprecated tools hidden from tools/list."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.tools.compat :as compat]))

;; =============================================================================
;; Phase 2 Strangle Tests
;; =============================================================================

(deftest deprecated-tools-have-metadata
  (testing "All deprecated tools have :deprecated true"
    (doseq [tool compat/tools]
      (is (:deprecated tool)
          (str "Tool " (:name tool) " should have :deprecated true"))))

  (testing "All deprecated tools have sunset-date"
    (doseq [tool compat/tools]
      (is (:sunset-date tool)
          (str "Tool " (:name tool) " should have :sunset-date"))))

  (testing "Sunset date is 2026-04-01"
    (doseq [tool compat/tools]
      (is (= "2026-04-01" (:sunset-date tool))
          (str "Tool " (:name tool) " should have sunset-date 2026-04-01")))))

(deftest deprecated-tool-count
  (testing "Should have 50 deprecated tools"
    (is (= 50 (count compat/tools))
        "Expected 50 deprecated tools")))

(deftest shim-handlers-are-callable
  (testing "Shim handlers are functions"
    (doseq [[name handler] compat/shims]
      (is (fn? handler)
          (str "Handler for " name " should be a function")))))

(deftest filtering-removes-deprecated
  (testing "filterv with :deprecated removes deprecated tools"
    (let [sample-tools [{:name "visible" :handler identity}
                        {:name "deprecated" :deprecated true :handler identity}]
          visible (filterv #(not (:deprecated %)) sample-tools)]
      (is (= 1 (count visible)))
      (is (= "visible" (:name (first visible)))))))
