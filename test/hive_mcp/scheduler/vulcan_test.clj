(ns hive-mcp.scheduler.vulcan-test
  "Tests for vulcan-mode task prioritization.

   All tests use with-redefs to mock KG edge queries and Chroma lookups,
   keeping tests fast and isolated (FIRST: Fast, Isolated)."
  (:require [clojure.test :refer [deftest is testing are]]
            [hive-mcp.scheduler.vulcan :as vulcan]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def task-a {:id "20260210-task-a" :title "Task A" :priority "high" :status "todo"})
(def task-b {:id "20260210-task-b" :title "Task B" :priority "high" :status "todo"})
(def task-c {:id "20260210-task-c" :title "Task C" :priority "medium" :status "todo"})
(def task-d {:id "20260210-task-d" :title "Task D" :priority "low" :status "todo"})
(def task-e {:id "20260210-task-e" :title "Task E" :priority "high" :status "todo"})

;; Dependency graph:
;; A (no deps) → wave 0
;; B depends on A → wave 1
;; C depends on A → wave 1
;; D depends on B and C → wave 2
;; E (no deps) → wave 0

(def mock-deps
  {"20260210-task-a" #{}
   "20260210-task-b" #{"20260210-task-a"}
   "20260210-task-c" #{"20260210-task-a"}
   "20260210-task-d" #{"20260210-task-b" "20260210-task-c"}
   "20260210-task-e" #{}})

(defn mock-deps-fn [task-id]
  (get mock-deps task-id #{}))

;; =============================================================================
;; dep-satisfied? tests
;; =============================================================================

(deftest test-dep-satisfied?
  (testing "dep in completed set is satisfied"
    (is (true? (vulcan/dep-satisfied? "task-1" #{"task-1"} (constantly true)))))

  (testing "dep not in completed but missing from store (deleted=done)"
    (is (true? (vulcan/dep-satisfied? "task-1" #{} (constantly false)))))

  (testing "dep not completed and still exists in store (not satisfied)"
    (is (false? (vulcan/dep-satisfied? "task-1" #{} (constantly true))))))

;; =============================================================================
;; task-ready? tests
;; =============================================================================

(deftest test-task-ready?
  (testing "task with no deps is always ready"
    (is (true? (vulcan/task-ready? "20260210-task-a" #{} mock-deps-fn (constantly true)))))

  (testing "task with all deps completed is ready"
    (is (true? (vulcan/task-ready? "20260210-task-b"
                                   #{"20260210-task-a"}
                                   mock-deps-fn
                                   (constantly true)))))

  (testing "task with unmet deps is NOT ready"
    (is (false? (vulcan/task-ready? "20260210-task-b"
                                    #{}
                                    mock-deps-fn
                                    (constantly true)))))

  (testing "task with deps deleted from store (done) is ready"
    ;; exists-fn returns false = task was deleted = completed
    (is (true? (vulcan/task-ready? "20260210-task-b"
                                   #{}
                                   mock-deps-fn
                                   (constantly false)))))

  (testing "task with partial deps met is NOT ready"
    ;; D depends on B and C. Only B completed.
    (let [exists-fn (fn [id] (= id "20260210-task-c"))]  ;; C still exists (not done)
      (is (false? (vulcan/task-ready? "20260210-task-d"
                                      #{"20260210-task-b"}
                                      mock-deps-fn
                                      exists-fn))))))

;; =============================================================================
;; filter-ready-tasks tests
;; =============================================================================

(deftest test-filter-ready-tasks
  (let [all-tasks [task-a task-b task-c task-d task-e]]

    (testing "with no completions, only tasks with no deps are ready"
      (let [ready (vulcan/filter-ready-tasks all-tasks #{} mock-deps-fn (constantly true))]
        (is (= 2 (count ready)))
        (is (= #{"20260210-task-a" "20260210-task-e"}
               (set (map :id ready))))))

    (testing "after A completes, B and C become ready too"
      (let [ready (vulcan/filter-ready-tasks all-tasks
                                             #{"20260210-task-a"}
                                             mock-deps-fn
                                             (constantly true))]
        (is (= 4 (count ready)))
        (is (= #{"20260210-task-a" "20260210-task-b" "20260210-task-c" "20260210-task-e"}
               (set (map :id ready))))))

    (testing "after A,B,C complete, D becomes ready"
      (let [ready (vulcan/filter-ready-tasks all-tasks
                                             #{"20260210-task-a" "20260210-task-b" "20260210-task-c"}
                                             mock-deps-fn
                                             (constantly true))]
        (is (= 5 (count ready)))))))

;; =============================================================================
;; compute-wave-number tests
;; =============================================================================

(deftest test-compute-wave-number
  (testing "task with no deps = wave 0"
    (is (= 0 (vulcan/compute-wave-number "20260210-task-a" mock-deps-fn))))

  (testing "task depending on wave-0 = wave 1"
    (is (= 1 (vulcan/compute-wave-number "20260210-task-b" mock-deps-fn))))

  (testing "task depending on wave-1 tasks = wave 2"
    (is (= 2 (vulcan/compute-wave-number "20260210-task-d" mock-deps-fn))))

  (testing "cycle detection returns 0 (no infinite loop)"
    (let [cyclic-deps {"x" #{"y"} "y" #{"x"}}
          cyclic-fn (fn [id] (get cyclic-deps id #{}))]
      (is (integer? (vulcan/compute-wave-number "x" cyclic-fn))))))

;; =============================================================================
;; enrich-with-wave-numbers tests
;; =============================================================================

(deftest test-enrich-with-wave-numbers
  (let [enriched (vulcan/enrich-with-wave-numbers
                  [task-a task-b task-d task-e]
                  mock-deps-fn)]
    (testing "all tasks get :wave-number"
      (is (every? :wave-number enriched)))

    (testing "wave numbers are correct"
      (let [by-id (into {} (map (juxt :id :wave-number) enriched))]
        (is (= 0 (get by-id "20260210-task-a")))
        (is (= 1 (get by-id "20260210-task-b")))
        (is (= 2 (get by-id "20260210-task-d")))
        (is (= 0 (get by-id "20260210-task-e")))))))

;; =============================================================================
;; sort-vulcan tests
;; =============================================================================

(deftest test-sort-vulcan
  (testing "primary sort by priority (high > medium > low)"
    (let [tasks [{:id "1" :priority "low" :wave-number 0}
                 {:id "2" :priority "high" :wave-number 0}
                 {:id "3" :priority "medium" :wave-number 0}]
          sorted (vulcan/sort-vulcan tasks)]
      (is (= ["2" "3" "1"] (mapv :id sorted)))))

  (testing "secondary sort by wave-number (lower wave first)"
    (let [tasks [{:id "1" :priority "high" :wave-number 2}
                 {:id "2" :priority "high" :wave-number 0}
                 {:id "3" :priority "high" :wave-number 1}]
          sorted (vulcan/sort-vulcan tasks)]
      (is (= ["2" "3" "1"] (mapv :id sorted)))))

  (testing "tertiary sort by creation date (id encodes timestamp)"
    (let [tasks [{:id "20260210-c" :priority "high" :wave-number 0}
                 {:id "20260210-a" :priority "high" :wave-number 0}
                 {:id "20260210-b" :priority "high" :wave-number 0}]
          sorted (vulcan/sort-vulcan tasks)]
      (is (= ["20260210-a" "20260210-b" "20260210-c"] (mapv :id sorted)))))

  (testing "combined: priority > wave > creation"
    (let [tasks [{:id "20260210-z" :priority "medium" :wave-number 0}
                 {:id "20260210-a" :priority "high" :wave-number 1}
                 {:id "20260210-b" :priority "high" :wave-number 0}
                 {:id "20260210-c" :priority "low" :wave-number 0}]
          sorted (vulcan/sort-vulcan tasks)]
      ;; high-wave0 first, then high-wave1, then medium-wave0, then low-wave0
      (is (= ["20260210-b" "20260210-a" "20260210-z" "20260210-c"]
             (mapv :id sorted))))))

;; =============================================================================
;; prioritize-tasks (high-level API) tests
;; =============================================================================

(deftest test-prioritize-tasks
  (let [all-tasks [task-a task-b task-c task-d task-e]]

    (testing "with no completions: only frontier tasks, sorted correctly"
      (let [result (vulcan/prioritize-tasks all-tasks #{}
                                            {:deps-fn mock-deps-fn
                                             :exists-fn (constantly true)})]
        (is (= 2 (:count result)))
        (is (= 3 (:blocked-count result)))
        ;; Both are high priority, wave 0 — sorted by id
        (is (= ["20260210-task-a" "20260210-task-e"]
               (mapv :id (:tasks result))))))

    (testing "after A completes: B,C,E ready; D still blocked"
      (let [result (vulcan/prioritize-tasks all-tasks
                                            #{"20260210-task-a"}
                                            {:deps-fn mock-deps-fn
                                             :exists-fn (constantly true)})]
        (is (= 4 (:count result)))
        (is (= 1 (:blocked-count result)))
        ;; A (high, wave0), E (high, wave0), B (high, wave1), C (medium, wave1)
        ;; Note: A is in completed-ids but still in the todo list input
        ;; filter-ready-tasks doesn't remove completed tasks from input — that's caller's job
        (let [ids (mapv :id (:tasks result))]
          ;; high-wave0 before high-wave1 before medium-wave1
          (is (= "20260210-task-c" (last ids)))  ;; medium is last
          )))))
