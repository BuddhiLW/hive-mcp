(ns hive-mcp.tools.memory.lifecycle-property-test
  "Property-based tests for lifecycle refactoring.
   Tests: walk-tiers, build-decay-plan, build-xpoll-plan."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.memory.promotion :as promo]
            [hive-mcp.tools.memory.decay :as decay]
            [hive-mcp.tools.memory.duration :as dur]))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-duration
  (gen/elements dur/duration-order))

(def gen-tiers
  (gen/choose 0 10))

(def gen-memory-type-kw
  (gen/elements [:axiom :principle :decision :convention :snippet :note :plan
                 :doc :todo :question :answer :warning :error
                 :pattern :lesson :rule :guideline :workflow :recipe]))

(def gen-staleness-beta
  (gen/double* {:min 0.0 :max 100.0 :NaN? false :infinite? false}))

(def gen-entry-id
  (gen/fmap #(str "test-" %) (gen/choose 1000 9999)))

(def gen-mock-entry
  (gen/let [id gen-entry-id
            type-kw gen-memory-type-kw
            duration gen-duration
            beta gen-staleness-beta
            access (gen/choose 0 50)]
    {:id id
     :type (name type-kw)
     :duration duration
     :staleness-beta beta
     :access-count access}))

;; =============================================================================
;; walk-tiers Properties
;; =============================================================================

(defspec P1-walk-tiers-zero-tiers-is-identity 200
  (prop/for-all [dur gen-duration]
    (let [{:keys [final-duration tiers-promoted]} (promo/walk-tiers dur 0)]
      (and (= final-duration dur)
           (zero? tiers-promoted)))))

(defspec P2-walk-tiers-monotonic 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
    (let [{:keys [final-duration tiers-promoted]} (promo/walk-tiers dur tiers)
          start-idx (.indexOf dur/duration-order dur)
          end-idx (.indexOf dur/duration-order final-duration)]
      (and (>= end-idx start-idx)
           (<= tiers-promoted tiers)
           (>= tiers-promoted 0)))))

(defspec P3-walk-tiers-bounded-by-permanent 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
    (let [{:keys [final-duration]} (promo/walk-tiers dur tiers)
          idx (.indexOf dur/duration-order final-duration)]
      (and (>= idx 0)
           (< idx (count dur/duration-order))))))

(defspec P4-walk-tiers-permanent-is-ceiling 200
  (prop/for-all [tiers (gen/choose 1 20)]
    (let [{:keys [final-duration tiers-promoted]} (promo/walk-tiers "permanent" tiers)]
      (and (= final-duration "permanent")
           (zero? tiers-promoted)))))

(defspec P5-walk-tiers-composable 200
  (prop/for-all [dur gen-duration
                 t1 gen-tiers
                 t2 gen-tiers]
    (let [result1 (promo/walk-tiers dur (+ t1 t2))
          intermediate (promo/walk-tiers dur t1)
          result2 (promo/walk-tiers (:final-duration intermediate) t2)]
      (= (:final-duration result1) (:final-duration result2)))))

(defspec P6-walk-tiers-promoted-count-accurate 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
    (let [{:keys [final-duration tiers-promoted]} (promo/walk-tiers dur tiers)
          start-idx (.indexOf dur/duration-order dur)
          end-idx (.indexOf dur/duration-order final-duration)]
      (= tiers-promoted (- end-idx start-idx)))))

(defspec P7-walk-tiers-never-negative 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
    (let [{:keys [tiers-promoted]} (promo/walk-tiers dur tiers)]
      (nat-int? tiers-promoted))))

;; =============================================================================
;; build-decay-plan Properties
;; =============================================================================

(defspec P8-build-decay-plan-structure-valid 200
  (prop/for-all [entry gen-mock-entry]
    (let [plan (decay/build-decay-plan entry {:access-threshold 0 :recency-days 0})]
      (or (nil? plan)
          (and (contains? plan :id)
               (contains? plan :delta)
               (contains? plan :old-beta)
               (contains? plan :new-beta))))))

(defspec P9-build-decay-plan-preserves-id 200
  (prop/for-all [entry gen-mock-entry]
    (let [plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
      (or (nil? plan)
          (= (:id plan) (:id entry))))))

(defspec P10-build-decay-plan-new-beta-ge-old 200
  (prop/for-all [entry gen-mock-entry]
    (let [plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
      (or (nil? plan)
          (>= (:new-beta plan) (:old-beta plan))))))

(defspec P11-build-decay-plan-delta-positive 200
  (prop/for-all [entry gen-mock-entry]
    (let [plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
      (or (nil? plan)
          (pos? (:delta plan))))))

;; =============================================================================
;; build-xpoll-plan Properties
;; =============================================================================

(defspec P12-build-xpoll-plan-has-required-keys 200
  (prop/for-all [entry gen-mock-entry]
    (let [plan (promo/build-xpoll-plan entry)]
      (and (contains? plan :id)
           (contains? plan :type)
           (contains? plan :duration)
           (contains? plan :cross_projects)
           (contains? plan :cross_project_count)
           (contains? plan :tiers_to_promote)
           (contains? plan :next_duration)))))

(defspec P13-build-xpoll-plan-preserves-identity 200
  (prop/for-all [entry gen-mock-entry]
    (let [plan (promo/build-xpoll-plan entry)]
      (and (= (:id plan) (:id entry))
           (= (:type plan) (:type entry))
           (= (:duration plan) (:duration entry))))))

(defspec P14-build-xpoll-plan-non-negative-counts 200
  (prop/for-all [entry gen-mock-entry]
    (let [plan (promo/build-xpoll-plan entry)]
      (and (nat-int? (:cross_project_count plan))
           (nat-int? (:tiers_to_promote plan))
           (vector? (:cross_projects plan))))))

;; =============================================================================
;; Deterministic Unit Tests
;; =============================================================================

(deftest walk-tiers-specific-cases
  (testing "ephemeral +1 = short"
    (is (= {:final-duration "short" :tiers-promoted 1}
           (promo/walk-tiers "ephemeral" 1))))
  (testing "short +2 = long"
    (is (= {:final-duration "long" :tiers-promoted 2}
           (promo/walk-tiers "short" 2))))
  (testing "long +5 = permanent (capped at 1 tier)"
    (is (= {:final-duration "permanent" :tiers-promoted 1}
           (promo/walk-tiers "long" 5))))
  (testing "permanent +0 = permanent"
    (is (= {:final-duration "permanent" :tiers-promoted 0}
           (promo/walk-tiers "permanent" 0))))
  (testing "ephemeral +4 = permanent (all tiers)"
    (is (= {:final-duration "permanent" :tiers-promoted 4}
           (promo/walk-tiers "ephemeral" 4))))
  (testing "medium +0 = identity"
    (is (= {:final-duration "medium" :tiers-promoted 0}
           (promo/walk-tiers "medium" 0)))))

(deftest duration-order-consistency
  (testing "walk-tiers follows duration-order"
    (doseq [[i dur] (map-indexed vector dur/duration-order)]
      (let [{:keys [final-duration tiers-promoted]} (promo/walk-tiers dur 1)]
        (if (= dur "permanent")
          (do (is (= final-duration "permanent"))
              (is (zero? tiers-promoted)))
          (do (is (= final-duration (nth dur/duration-order (inc i))))
              (is (= tiers-promoted 1))))))))

(deftest build-decay-plan-structure
  (testing "plan with mock entry has correct structure"
    (let [entry {:id "test-1" :type "note" :duration "short"
                 :staleness-beta 1.0 :access-count 0}
          plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
      (when plan
        (is (= "test-1" (:id plan)))
        (is (= "note" (:type plan)))
        (is (pos? (:delta plan)))
        (is (>= (:new-beta plan) (:old-beta plan)))))))
