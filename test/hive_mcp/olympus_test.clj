(ns hive-mcp.olympus-test
  "Tests for Olympus grid layout calculation.

   TDD: Tests written FIRST, implementation follows.

   Layout algorithm:
   n=1: {:rows 1 :cols 1}           ; full screen
   n=2: {:rows 1 :cols 2}           ; side-by-side
   n=3: {:rows 2 :cols 2 :empty #{[1 1]}}  ; 2+1
   n=4: {:rows 2 :cols 2}           ; 2x2
   n=5+: {:tabs (ceil n/4) :per-tab 4}  ; tabbed"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.olympus :as olympus]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn test-fixture [f]
  ;; No state to reset for pure functions
  (f))

(use-fixtures :each test-fixture)

;;; =============================================================================
;;; calculate-layout Tests
;;; =============================================================================

(deftest calculate-layout-single-ling
  (testing "n=1 returns full screen layout"
    (let [layout (olympus/calculate-layout 1)]
      (is (= 1 (:rows layout)))
      (is (= 1 (:cols layout)))
      (is (nil? (:empty-cells layout)))
      (is (nil? (:tabs layout))))))

(deftest calculate-layout-two-lings
  (testing "n=2 returns side-by-side layout"
    (let [layout (olympus/calculate-layout 2)]
      (is (= 1 (:rows layout)))
      (is (= 2 (:cols layout)))
      (is (nil? (:empty-cells layout)))
      (is (nil? (:tabs layout))))))

(deftest calculate-layout-three-lings
  (testing "n=3 returns 2x2 with one empty cell"
    (let [layout (olympus/calculate-layout 3)]
      (is (= 2 (:rows layout)))
      (is (= 2 (:cols layout)))
      (is (= #{[1 1]} (:empty-cells layout)))
      (is (nil? (:tabs layout))))))

(deftest calculate-layout-four-lings
  (testing "n=4 returns perfect 2x2 grid"
    (let [layout (olympus/calculate-layout 4)]
      (is (= 2 (:rows layout)))
      (is (= 2 (:cols layout)))
      (is (nil? (:empty-cells layout)))
      (is (nil? (:tabs layout))))))

(deftest calculate-layout-five-plus-lings
  (testing "n=5 starts tabbed layout"
    (let [layout (olympus/calculate-layout 5)]
      (is (= 2 (:tabs layout)))
      (is (= 4 (:per-tab layout)))))

  (testing "n=8 fits in 2 tabs"
    (let [layout (olympus/calculate-layout 8)]
      (is (= 2 (:tabs layout)))
      (is (= 4 (:per-tab layout)))))

  (testing "n=9 requires 3 tabs"
    (let [layout (olympus/calculate-layout 9)]
      (is (= 3 (:tabs layout)))
      (is (= 4 (:per-tab layout))))))

(deftest calculate-layout-edge-cases
  (testing "n=0 returns empty layout"
    (let [layout (olympus/calculate-layout 0)]
      (is (= 0 (:rows layout)))
      (is (= 0 (:cols layout)))))

  (testing "Negative n treated as 0"
    (let [layout (olympus/calculate-layout -1)]
      (is (= 0 (:rows layout))))))

;;; =============================================================================
;;; assign-positions Tests
;;; =============================================================================

(deftest assign-positions-single-ling
  (testing "Single ling gets full screen position"
    (let [lings [{:slave/id "ling-1" :slave/name "worker"}]
          layout {:rows 1 :cols 1}
          positions (olympus/assign-positions lings layout)]
      (is (= 1 (count positions)))
      (is (= {:row 0 :col 0 :tab nil}
             (get positions "ling-1"))))))

(deftest assign-positions-two-lings
  (testing "Two lings get side-by-side positions"
    (let [lings [{:slave/id "ling-1" :slave/name "left"}
                 {:slave/id "ling-2" :slave/name "right"}]
          layout {:rows 1 :cols 2}
          positions (olympus/assign-positions lings layout)]
      (is (= 2 (count positions)))
      (is (= {:row 0 :col 0 :tab nil} (get positions "ling-1")))
      (is (= {:row 0 :col 1 :tab nil} (get positions "ling-2"))))))

(deftest assign-positions-three-lings
  (testing "Three lings skip empty cell"
    (let [lings [{:slave/id "ling-1"} {:slave/id "ling-2"} {:slave/id "ling-3"}]
          layout {:rows 2 :cols 2 :empty-cells #{[1 1]}}
          positions (olympus/assign-positions lings layout)]
      (is (= 3 (count positions)))
      ;; Should use [0,0], [0,1], [1,0] - skip [1,1]
      (is (= {:row 0 :col 0 :tab nil} (get positions "ling-1")))
      (is (= {:row 0 :col 1 :tab nil} (get positions "ling-2")))
      (is (= {:row 1 :col 0 :tab nil} (get positions "ling-3"))))))

(deftest assign-positions-tabbed
  (testing "Five lings get tabbed positions"
    (let [lings (mapv #(hash-map :slave/id (str "ling-" %)) (range 1 6))
          layout {:tabs 2 :per-tab 4}
          positions (olympus/assign-positions lings layout)]
      (is (= 5 (count positions)))
      ;; First 4 on tab 0, 5th on tab 1
      (is (= 0 (:tab (get positions "ling-1"))))
      (is (= 0 (:tab (get positions "ling-4"))))
      (is (= 1 (:tab (get positions "ling-5")))))))

(deftest assign-positions-empty-lings
  (testing "Empty lings list returns empty positions"
    (let [positions (olympus/assign-positions [] {:rows 2 :cols 2})]
      (is (= {} positions)))))

;;; =============================================================================
;;; Integration: Layout + Positions
;;; =============================================================================

(deftest full-layout-flow
  (testing "End-to-end layout calculation"
    (let [lings (mapv #(hash-map :slave/id (str "ling-" %)
                                 :slave/name (str "worker-" %))
                      (range 1 5))
          layout (olympus/calculate-layout 4)
          positions (olympus/assign-positions lings layout)]
      (is (= 2 (:rows layout)))
      (is (= 2 (:cols layout)))
      (is (= 4 (count positions)))
      ;; All 4 cells filled
      (is (some #(= {:row 0 :col 0 :tab nil} %) (vals positions)))
      (is (some #(= {:row 0 :col 1 :tab nil} %) (vals positions)))
      (is (some #(= {:row 1 :col 0 :tab nil} %) (vals positions)))
      (is (some #(= {:row 1 :col 1 :tab nil} %) (vals positions))))))
