;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.channel.blocks-test
  "Blocks as an open set.

   The assertion that carries the design is `an-unknown-emitter-renders-with-no-
   host-change`: a tag this namespace has never heard of reaches the response
   because an addon registered it. If that ever needs a host edit, the seam is
   gone."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.blocks :as blocks]
            [hive-mcp.extensions.registry :as ext]))

(defn- clear-blocks! []
  (doseq [k (blocks/emitter-keys)] (ext/deregister! k)))

(use-fixtures :each (fn [t] (clear-blocks!) (t) (clear-blocks!)))

(def ctx {:tool-name "code" :cues #{"carto"} :caller-id "c1"})

(deftest an-unknown-emitter-renders-with-no-host-change
  (testing "the host names no block; a new one is a register! by an addon"
    (ext/register! :block/weather (fn [_] "sunny"))
    (is (= [["WEATHER" "sunny"]] (blocks/render ctx)))))

(deftest the-key-name-becomes-the-tag
  (is (= "FRONTIER" (blocks/tag-of :block/frontier)))
  (is (= "MY-THING" (blocks/tag-of :block/my-thing))))

(deftest only-block-namespaced-keys-are-emitters
  (ext/register! :block/yes (fn [_] "y"))
  (ext/register! :memory/activation (fn [_] "should not render"))
  (is (= [:block/yes] (blocks/emitter-keys)))
  (is (= [["YES" "y"]] (blocks/render ctx))))

(deftest the-emitter-receives-the-request-ctx
  (testing "this is what the buffered caller-id-only channel could not offer"
    (ext/register! :block/echo (fn [c] (pr-str (select-keys c [:tool-name :caller-id]))))
    (is (= [["ECHO" (pr-str {:tool-name "code" :caller-id "c1"})]]
           (blocks/render ctx)))))

(deftest order-is-deterministic-not-registration-order
  (ext/register! :block/zebra (fn [_] "z"))
  (ext/register! :block/alpha (fn [_] "a"))
  (is (= ["ALPHA" "ZEBRA"] (mapv first (blocks/render ctx)))))

;; =============================================================================
;; Nothing to say costs nothing
;; =============================================================================

(deftest a-nil-or-blank-body-renders-no-block
  (ext/register! :block/quiet (fn [_] nil))
  (ext/register! :block/blank (fn [_] "   "))
  (ext/register! :block/empty-coll (fn [_] nil))
  (is (empty? (blocks/render ctx))))

(deftest no-emitters-is-an-empty-vector
  (is (= [] (blocks/render ctx))))

;; =============================================================================
;; Budget lives in the loop, not in each emitter's good manners
;; =============================================================================

(deftest a-long-body-is-truncated-and-SAYS-so
  (testing "an unmarked cut reads as a complete answer that ends oddly"
    (ext/register! :block/big (fn [_] (apply str (repeat 9000 "z"))))
    (let [[[_ body]] (blocks/render ctx)]
      (is (< (count body) 9000))
      (is (clojure.string/ends-with? body "...[truncated]")))))

(deftest the-block-count-is-capped
  (doseq [i (range (+ 3 blocks/max-blocks))]
    (ext/register! (keyword "block" (str "b" i)) (fn [_] "x")))
  (is (= blocks/max-blocks (count (blocks/render ctx)))))

;; =============================================================================
;; Totality — one bad emitter must not cost the response
;; =============================================================================

(deftest a-throwing-emitter-costs-its-own-block-only
  (ext/register! :block/boom (fn [_] (throw (ex-info "boom" {}))))
  (ext/register! :block/fine (fn [_] "still here"))
  (is (= [["FINE" "still here"]] (blocks/render ctx))))

(deftest a-non-string-body-is-printed
  (ext/register! :block/data (fn [_] [{:id "a"}]))
  (is (= [["DATA" (pr-str [{:id "a"}])]] (blocks/render ctx))))
