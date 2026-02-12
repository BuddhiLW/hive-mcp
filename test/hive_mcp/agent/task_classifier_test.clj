(ns hive-mcp.agent.task-classifier-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.task-classifier :as tc]
            [hive-mcp.extensions.registry :as ext]))

;;; ─── Test setup ──────────────────────────────────────────────────

(use-fixtures :each
  (fn [f]
    (ext/clear-all!)
    (f)
    (ext/clear-all!)))

;;; ─── Signal scorer noop fallbacks ────────────────────────────────

(deftest score-file-count-noop-test
  (testing "returns noop signal shape when no extension"
    (let [result (tc/score-file-count ["src/foo.clj"])]
      (is (= 0.5 (:score result)))
      (is (= :noop (:signal result)))
      (is (string? (:detail result)))))

  (testing "noop with nil files"
    (let [result (tc/score-file-count nil)]
      (is (number? (:score result)))
      (is (keyword? (:signal result)))
      (is (string? (:detail result)))))

  (testing "noop with empty files"
    (let [result (tc/score-file-count [])]
      (is (number? (:score result)))
      (is (keyword? (:signal result)))
      (is (string? (:detail result)))))

  (testing "noop with multiple files"
    (let [result (tc/score-file-count ["a.clj" "b.clj" "c.clj"])]
      (is (number? (:score result)))
      (is (keyword? (:signal result)))
      (is (string? (:detail result))))))

(deftest score-complexity-noop-test
  (testing "returns noop signal shape when no extension"
    (let [result (tc/score-complexity "fix bug" ["src/foo.clj"])]
      (is (= 0.5 (:score result)))
      (is (= :noop (:signal result)))
      (is (string? (:detail result)))))

  (testing "noop with nil files"
    (let [result (tc/score-complexity "some task" nil)]
      (is (number? (:score result)))
      (is (keyword? (:signal result)))
      (is (string? (:detail result))))))

(deftest score-task-type-noop-test
  (testing "returns noop signal shape when no extension"
    (let [result (tc/score-task-type "write unit tests" ["test/foo_test.clj"])]
      (is (= 0.5 (:score result)))
      (is (= :noop (:signal result)))
      (is (string? (:detail result))))))

(deftest score-keywords-noop-test
  (testing "returns noop signal shape when no extension"
    (let [result (tc/score-keywords "design the architecture")]
      (is (= 0.5 (:score result)))
      (is (= :noop (:signal result)))
      (is (string? (:detail result)))))

  (testing "noop regardless of keyword content"
    (is (= 0.5 (:score (tc/score-keywords "add a new function"))))
    (is (= 0.5 (:score (tc/score-keywords "investigate and analyze"))))))

;;; ─── classify-task noop fallback ─────────────────────────────────

(deftest classify-task-noop-test
  (testing "returns ling route with 0.0 confidence when no extension"
    (let [result (tc/classify-task {:title "fix null pointer in foo.clj"
                                    :files ["src/foo.clj"]})]
      (is (= :ling (:route result)))
      (is (= 0.0 (:confidence result)))
      (is (string? (:reason result)))
      (is (map? (:signals result))))))

(deftest classify-task-no-files-noop-test
  (testing "handles missing files gracefully"
    (let [result (tc/classify-task {:title "do something"})]
      (is (= :ling (:route result)))
      (is (= 0.0 (:confidence result))))))

(deftest classify-task-description-noop-test
  (testing "accepts description without error"
    (let [result (tc/classify-task {:title "task"
                                    :description "fix the bug in parser"
                                    :files ["src/parser.clj"]})]
      (is (= :ling (:route result)))
      (is (= 0.0 (:confidence result))))))

(deftest classify-task-empty-input-noop-test
  (testing "handles empty/nil fields gracefully"
    (let [result (tc/classify-task {:title "" :files []})]
      (is (= :ling (:route result)))
      (is (= 0.0 (:confidence result))))
    (let [result (tc/classify-task {:title nil})]
      (is (= :ling (:route result)))
      (is (= 0.0 (:confidence result))))))

;;; ─── classify-for-routing wrapper ────────────────────────────────

(deftest classify-for-routing-noop-test
  (testing "string API returns same shape as map API"
    (let [map-result (tc/classify-task {:title "fix bug" :files ["src/foo.clj"]})
          str-result (tc/classify-for-routing "fix bug" ["src/foo.clj"])]
      (is (= (:route map-result) (:route str-result)))
      (is (= (:confidence map-result) (:confidence str-result)))
      (is (string? (:reason str-result)))
      (is (map? (:signals str-result)))))

  (testing "accepts optional opts"
    (let [result (tc/classify-for-routing "fix bug" ["src/foo.clj"] {:threshold 0.5})]
      (is (= :ling (:route result))))))

;;; ─── drone-eligible? predicate ───────────────────────────────────

(deftest drone-eligible-noop-test
  (testing "always returns false when no extension"
    (is (false? (tc/drone-eligible? {:title "fix bug" :files ["src/foo.clj"]})))
    (is (false? (tc/drone-eligible? {:title "write test" :files ["test/foo.clj"]})))
    (is (false? (tc/drone-eligible? {:title "do something"})))))

;;; ─── Return shape validation ─────────────────────────────────────

(deftest return-shape-test
  (testing "result has all required keys"
    (let [result (tc/classify-task {:title "fix bug" :files ["src/foo.clj"]})]
      (is (contains? result :route))
      (is (contains? result :confidence))
      (is (contains? result :reason))
      (is (contains? result :signals))
      (is (#{:drone :ling} (:route result)))
      (is (<= 0.0 (:confidence result) 1.0))
      (is (string? (:reason result)))
      (is (map? (:signals result)))))

  (testing "each signal has score, signal, detail when extension provides them"
    ;; With noop, signals is empty map — shape validation still holds
    (let [result (tc/classify-task {:title "fix bug" :files ["src/foo.clj"]})]
      (is (map? (:signals result))))))

;;; ─── Confidence bounds ──────────────────────────────────────────

(deftest confidence-bounds-noop-test
  (testing "confidence always in [0.0, 1.0] with noop"
    (doseq [task [{:title "fix bug" :files ["src/foo.clj"]}
                  {:title "design architecture" :files ["a" "b" "c" "d" "e"]}
                  {:title "" :files []}
                  {:title nil}]]
      (let [result (tc/classify-task task)]
        (is (<= 0.0 (:confidence result) 1.0)
            (str "Out of bounds for: " task))))))

;;; ─── Extension delegation ───────────────────────────────────────

(deftest extension-delegation-classify-test
  (testing "classify-task delegates to extension when registered"
    (ext/register! :tc/run
                   (fn [_task]
                     {:route      :drone
                      :confidence 0.95
                      :reason     "Extension classified"
                      :signals    {:ext {:score 0.95 :signal :ext :detail "From extension"}}}))
    (let [result (tc/classify-task {:title "fix bug" :files ["src/foo.clj"]})]
      (is (= :drone (:route result)))
      (is (= 0.95 (:confidence result)))
      (is (= "Extension classified" (:reason result))))))

(deftest extension-delegation-scorer-test
  (testing "scorer delegates to extension when registered"
    (ext/register! :tc/s1
                   (fn [_files]
                     {:score 1.0 :signal :single-file :detail "Single file mutation"}))
    (let [result (tc/score-file-count ["src/foo.clj"])]
      (is (= 1.0 (:score result)))
      (is (= :single-file (:signal result)))))

  (testing "complexity scorer delegates to extension when registered"
    (ext/register! :tc/s2
                   (fn [_task-text _files]
                     {:score 0.8 :signal :simple :detail "Simple complexity"}))
    (let [result (tc/score-complexity "fix bug" ["src/foo.clj"])]
      (is (= 0.8 (:score result)))
      (is (= :simple (:signal result)))))

  (testing "task type scorer delegates to extension when registered"
    (ext/register! :tc/s3
                   (fn [_task-text _files]
                     {:score 0.9 :signal :testing :detail "Test writing"}))
    (let [result (tc/score-task-type "write tests" ["test/foo.clj"])]
      (is (= 0.9 (:score result)))))

  (testing "keyword scorer delegates to extension when registered"
    (ext/register! :tc/s4
                   (fn [_task-text]
                     {:score 0.0 :signal :ling-keywords :detail "1 ling keyword"}))
    (let [result (tc/score-keywords "design the architecture")]
      (is (= 0.0 (:score result)))
      (is (= :ling-keywords (:signal result))))))

(deftest extension-drone-eligible-test
  (testing "drone-eligible? delegates through classify-task extension"
    (ext/register! :tc/run
                   (fn [_task]
                     {:route      :drone
                      :confidence 0.95
                      :reason     "Extension classified"
                      :signals    {}}))
    (is (true? (tc/drone-eligible? {:title "fix bug" :files ["src/foo.clj"]})))))
