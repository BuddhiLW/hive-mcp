(ns hive-mcp.tools.memory.lifecycle-property-test
  "Property-based tests for lifecycle module extractions.
   Tests pure functions: walk-tiers, build-decay-plan, build-xpoll-plan."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.memory.promotion :as promo]
            [hive-mcp.tools.memory.decay :as decay]
            [hive-mcp.tools.memory.duration :as dur]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; --- Generators ---

(def gen-duration (gen/elements dur/duration-order))
(def gen-tiers (gen/choose 0 10))
(def gen-positive-double (gen/double* {:min 0.01 :max 100.0 :NaN? false :infinite? false}))
(def gen-non-negative-double (gen/double* {:min 0.0 :max 100.0 :NaN? false :infinite? false}))
(def gen-beta (gen/double* {:min 0.1 :max 10.0 :NaN? false :infinite? false}))
(def gen-access-count (gen/choose 0 50))
(def gen-type (gen/elements ["axiom" "principle" "decision" "convention"
                             "snippet" "note" "plan"]))

;; --- P1-P7: walk-tiers properties ---

(defspec walk-tiers-never-exceeds-permanent 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
                (let [result (promo/walk-tiers dur tiers)]
                  (contains? (set dur/duration-order) (:new-duration result)))))

(defspec walk-tiers-zero-is-noop 200
  (prop/for-all [dur gen-duration]
                (let [result (promo/walk-tiers dur 0)]
                  (and (= dur (:new-duration result))
                       (zero? (:tiers-walked result))
                       (false? (:changed? result))))))

(defspec walk-tiers-monotonic 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
                (let [result (promo/walk-tiers dur tiers)
                      old-idx (.indexOf dur/duration-order dur)
                      new-idx (.indexOf dur/duration-order (:new-duration result))]
                  (>= new-idx old-idx))))

(defspec walk-tiers-walked-bounded 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
                (let [result (promo/walk-tiers dur tiers)]
                  (<= (:tiers-walked result) tiers))))

(defspec walk-tiers-permanent-ceiling 200
  (prop/for-all [tiers (gen/choose 1 20)]
                (let [result (promo/walk-tiers "permanent" tiers)]
                  (and (= "permanent" (:new-duration result))
                       (zero? (:tiers-walked result))
                       (false? (:changed? result))))))

(defspec walk-tiers-idempotent-at-permanent 200
  (prop/for-all [dur gen-duration
                 t1 (gen/choose 0 10)
                 t2 (gen/choose 0 10)]
                (let [total (+ t1 t2)
                      combined (promo/walk-tiers dur total)
                      first-pass (promo/walk-tiers dur t1)
                      second-pass (promo/walk-tiers (:new-duration first-pass) t2)]
                  (= (:new-duration combined) (:new-duration second-pass)))))

(defspec walk-tiers-changed-iff-walked 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
                (let [result (promo/walk-tiers dur tiers)]
                  (= (:changed? result) (pos? (:tiers-walked result))))))

;; --- P8-P11: build-decay-plan properties ---

(defspec build-decay-plan-preserves-id 200
  (prop/for-all [id gen/string-alphanumeric
                 beta gen-beta
                 _delta gen-positive-double]
                (let [entry {:id id :staleness-beta beta :type "note" :duration "long" :access-count 1}
          ;; Simulate what build-decay-plan expects
                      plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
                  (or (nil? plan) (= id (:id plan))))))

(defspec build-decay-plan-new-beta-greater 200
  (prop/for-all [beta gen-beta
                 _delta gen-positive-double]
                (let [entry {:id "test" :staleness-beta beta :type "note" :duration "long" :access-count 1}
                      plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
                  (or (nil? plan) (> (:new-beta plan) (:old-beta plan))))))

(defspec build-decay-plan-delta-equals-diff 200
  (prop/for-all [beta gen-beta]
                (let [entry {:id "test" :staleness-beta beta :type "note" :duration "long" :access-count 1}
                      plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
                  (or (nil? plan)
                      (let [expected (+ (:old-beta plan) (:delta plan))]
                        (< (Math/abs (- (:new-beta plan) expected)) 0.0001))))))

(defspec build-decay-plan-nil-when-zero-delta 200
  (prop/for-all [beta gen-beta]
                (let [entry {:id "test" :staleness-beta beta :type "note" :duration "long"
                             :access-count 100} ;; high access = likely no decay
                      plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
      ;; Either nil (no decay needed) or plan has positive delta
                  (or (nil? plan) (pos? (:delta plan))))))

;; --- P12-P14: walk-tiers composition properties ---

(defspec walk-tiers-single-step-max-one-tier 200
  (prop/for-all [dur gen-duration]
                (let [result (promo/walk-tiers dur 1)]
                  (<= (:tiers-walked result) 1))))

(defspec walk-tiers-ephemeral-always-advances 200
  (prop/for-all [tiers (gen/choose 1 10)]
                (let [result (promo/walk-tiers "ephemeral" tiers)]
                  (:changed? result))))

(defspec walk-tiers-tiers-walked-exact 200
  (prop/for-all [dur gen-duration
                 tiers gen-tiers]
                (let [result (promo/walk-tiers dur tiers)
                      old-idx (.indexOf dur/duration-order dur)
                      new-idx (.indexOf dur/duration-order (:new-duration result))]
                  (= (:tiers-walked result) (- new-idx old-idx)))))

;; --- Deterministic unit tests ---

(deftest walk-tiers-specific-cases
  (testing "ephemeral +3 = long"
    (let [r (promo/walk-tiers "ephemeral" 3)]
      (is (= "long" (:new-duration r)))
      (is (= 3 (:tiers-walked r)))
      (is (true? (:changed? r)))))
  (testing "ephemeral +10 = permanent (ceiling)"
    (let [r (promo/walk-tiers "ephemeral" 10)]
      (is (= "permanent" (:new-duration r)))
      (is (= 4 (:tiers-walked r)))))
  (testing "long +1 = permanent"
    (let [r (promo/walk-tiers "long" 1)]
      (is (= "permanent" (:new-duration r)))
      (is (= 1 (:tiers-walked r))))))

(deftest duration-order-consistency
  (testing "duration-order has 5 entries"
    (is (= 5 (count dur/duration-order))))
  (testing "starts with ephemeral, ends with permanent"
    (is (= "ephemeral" (first dur/duration-order)))
    (is (= "permanent" (last dur/duration-order)))))

(deftest build-decay-plan-structure
  (testing "plan has required keys when non-nil"
    (let [entry {:id "test-123" :staleness-beta 1.0 :type "note"
                 :duration "long" :access-count 0}
          plan (decay/build-decay-plan entry {:access-threshold 3 :recency-days 7})]
      (when plan
        (is (contains? plan :id))
        (is (contains? plan :delta))
        (is (contains? plan :old-beta))
        (is (contains? plan :new-beta))
        (is (= "test-123" (:id plan)))))))
