(ns hive-mcp.tools.consolidated.workflow.spawn-property-test
  "Property tests for spawn pure functions: compute-route-batches,
   build-spark-response."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.consolidated.workflow.spawn]))

;; Access private functions via var deref
(def ^:private compute-route-batches
  @(resolve 'hive-mcp.tools.consolidated.workflow.spawn/compute-route-batches))

(def ^:private build-spark-response
  @(resolve 'hive-mcp.tools.consolidated.workflow.spawn/build-spark-response))

;; ── Generators ─────────────────────────────────────────────────────────────

(def gen-task
  (gen/hash-map :id (gen/fmap #(str "task-" %) (gen/choose 1 10000))
                :title gen/string-alphanumeric))

(def gen-task-list
  (gen/vector gen-task 0 15))

(def gen-spawn-mode
  (gen/elements [:claude :vterm :headless :agent-sdk :openrouter :mixed]))

(def gen-active-counts
  (gen/let [vt (gen/choose 0 5)
            hl (gen/choose 0 10)]
    {:active-vterm vt :active-headless hl :active-total (+ vt hl)}))

(def gen-max-slots
  (gen/choose 1 20))

;; ── compute-route-batches ──────────────────────────────────────────────────

(defspec route-batches-total-never-exceeds-tasks 200
  (prop/for-all [tasks gen-task-list
                 mode gen-spawn-mode
                 max-slots gen-max-slots
                 active gen-active-counts]
                (let [[vt hl] (compute-route-batches {:effective-spawn-mode mode
                                                      :ling-tasks tasks
                                                      :max-slots max-slots
                                                      :active-counts active})]
                  (<= (+ (count vt) (count hl)) (count tasks)))))

(defspec route-batches-total-never-exceeds-available-slots 200
  (prop/for-all [tasks gen-task-list
                 mode gen-spawn-mode
                 max-slots gen-max-slots
                 active gen-active-counts]
                (let [[vt hl] (compute-route-batches {:effective-spawn-mode mode
                                                      :ling-tasks tasks
                                                      :max-slots max-slots
                                                      :active-counts active})
                      total-batched (+ (count vt) (count hl))]
      ;; Total batched + active should not exceed max-slots
      ;; (vterm has its own cap of 5)
                  (<= total-batched (max 0 (- (max max-slots 1) (:active-total active)))))))

(defspec vterm-route-only-produces-vterm-batch 200
  (prop/for-all [tasks gen-task-list
                 max-slots gen-max-slots
                 active gen-active-counts]
                (let [[vt hl] (compute-route-batches {:effective-spawn-mode :vterm
                                                      :ling-tasks tasks
                                                      :max-slots max-slots
                                                      :active-counts active})]
                  (empty? hl))))

(defspec headless-route-only-produces-headless-batch 200
  (prop/for-all [tasks gen-task-list
                 max-slots gen-max-slots
                 active gen-active-counts]
                (let [[vt hl] (compute-route-batches {:effective-spawn-mode :headless
                                                      :ling-tasks tasks
                                                      :max-slots max-slots
                                                      :active-counts active})]
                  (empty? vt))))

(defspec route-batches-return-vectors 200
  (prop/for-all [tasks gen-task-list
                 mode gen-spawn-mode
                 max-slots gen-max-slots
                 active gen-active-counts]
                (let [[vt hl] (compute-route-batches {:effective-spawn-mode mode
                                                      :ling-tasks tasks
                                                      :max-slots max-slots
                                                      :active-counts active})]
                  (and (vector? vt) (vector? hl)))))

(defspec route-batches-preserve-task-identity 200
  (prop/for-all [tasks gen-task-list
                 mode gen-spawn-mode
                 max-slots gen-max-slots
                 active gen-active-counts]
                (let [[vt hl] (compute-route-batches {:effective-spawn-mode mode
                                                      :ling-tasks tasks
                                                      :max-slots max-slots
                                                      :active-counts active})
                      all-batched (concat vt hl)]
      ;; All batched tasks must come from original tasks
                  (every? (set tasks) all-batched))))

(defspec empty-tasks-produce-empty-batches 200
  (prop/for-all [mode gen-spawn-mode
                 max-slots gen-max-slots
                 active gen-active-counts]
                (let [[vt hl] (compute-route-batches {:effective-spawn-mode mode
                                                      :ling-tasks []
                                                      :max-slots max-slots
                                                      :active-counts active})]
                  (and (empty? vt) (empty? hl)))))

;; ── build-spark-response ───────────────────────────────────────────────────

(def gen-spawn-result
  (gen/let [spawned gen/boolean
            id (gen/fmap #(str "agent-" %) (gen/choose 1 1000))
            title gen/string-alphanumeric]
    {:agent-id id :task-title title :spawned spawned :route :claude}))

(def gen-spawn-results
  (gen/vector gen-spawn-result 0 5))

(defspec spark-response-count-matches-spawned 200
  (prop/for-all [vt-results gen-spawn-results
                 hl-results gen-spawn-results]
                (let [resp (build-spark-response {:vt-results vt-results
                                                  :hl-results hl-results
                                                  :drone-result nil
                                                  :active-counts {:active-vterm 0 :active-headless 0 :active-total 0}
                                                  :max-slots 10})]
                  (= (:count resp)
                     (count (filter :spawned (concat vt-results hl-results)))))))

(defspec spark-response-has-required-keys 200
  (prop/for-all [vt-results gen-spawn-results
                 hl-results gen-spawn-results]
                (let [resp (build-spark-response {:vt-results vt-results
                                                  :hl-results hl-results
                                                  :drone-result nil
                                                  :active-counts {:active-vterm 0 :active-headless 0 :active-total 0}
                                                  :max-slots 10})]
                  (and (contains? resp :spawned)
                       (contains? resp :failed)
                       (contains? resp :count)
                       (contains? resp :routes)
                       (contains? resp :slots-used)))))

(defspec spark-response-spawned-plus-failed-equals-total 200
  (prop/for-all [vt-results gen-spawn-results
                 hl-results gen-spawn-results]
                (let [resp (build-spark-response {:vt-results vt-results
                                                  :hl-results hl-results
                                                  :drone-result nil
                                                  :active-counts {:active-vterm 0 :active-headless 0 :active-total 0}
                                                  :max-slots 10})
                      all-ling (concat vt-results hl-results)]
                  (= (count all-ling)
                     (+ (count (:spawned resp)) (count (:failed resp)))))))

;; ── Unit Tests ─────────────────────────────────────────────────────────────

(deftest compute-route-batches-saturated
  (testing "fully saturated slots produce empty batches"
    (let [[vt hl] (compute-route-batches {:effective-spawn-mode :mixed
                                          :ling-tasks [{:id "t1"} {:id "t2"}]
                                          :max-slots 3
                                          :active-counts {:active-vterm 2 :active-headless 1 :active-total 3}})]
      (is (empty? vt))
      (is (empty? hl)))))

(deftest build-spark-response-with-drone-result
  (testing "drone results merge into response"
    (let [resp (build-spark-response {:vt-results []
                                      :hl-results []
                                      :drone-result {:spawned [{:spawned true :route :drone}]
                                                     :count 1
                                                     :wave-id "w-1"}
                                      :active-counts {:active-vterm 0 :active-headless 0 :active-total 0}
                                      :max-slots 10})]
      (is (= 1 (:count resp)))
      (is (= 1 (get-in resp [:routes :drone :count]))))))
