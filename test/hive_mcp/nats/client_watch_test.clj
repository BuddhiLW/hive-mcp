(ns hive-mcp.nats.client-watch-test
  "A stop!/start! cycle replaces the connection object, drops the dispatcher and
   clears every tracked subscription, so a subscriber armed before the cycle is
   silently gone afterwards while `connected?` reports healthy again. The
   connection atom is the only thing that moves across such a cycle, so it is
   what long-lived subscribers watch to re-subscribe. These tests drive the
   private atom directly: no NATS server is contacted."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.nats.client :as client]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; the atom itself, not its var: it is a defonce, so deref-at-load is stable
(def ^:private connection @#'client/connection)

(def ^:private test-key ::probe)

(defn- reset-world! []
  (client/remove-connection-watch! test-key)
  (reset! connection nil))

(use-fixtures :each (fn [t] (reset-world!) (t) (reset-world!)))

(deftest watch-fires-when-the-connection-object-is-replaced
  (let [seen (atom [])]
    (client/add-connection-watch! test-key (fn [old new] (swap! seen conj [old new])))
    (reset! connection ::conn-1)
    (is (= [[nil ::conn-1]] @seen) "a fresh connect must reach the subscriber")
    (reset! connection nil)
    (is (= [[nil ::conn-1] [::conn-1 nil]] @seen)
        "a disconnect is reported too, so a subscriber can stand down")))

(deftest an-unchanged-value-does-not-fire
  (let [calls (atom 0)]
    (reset! connection ::conn-1)
    (client/add-connection-watch! test-key (fn [_ _] (swap! calls inc)))
    (reset! connection ::conn-1)
    (is (zero? @calls)
        "re-writing the same connection is not a lifecycle event; firing on it
         would make every subscriber re-subscribe for nothing")))

(deftest removing-the-watch-stops-delivery
  (let [calls (atom 0)]
    (client/add-connection-watch! test-key (fn [_ _] (swap! calls inc)))
    (reset! connection ::conn-1)
    (is (= 1 @calls))
    (client/remove-connection-watch! test-key)
    (reset! connection ::conn-2)
    (is (= 1 @calls) "a stopped subscriber must not keep receiving")
    (testing "removing an absent key is safe"
      (is (nil? (client/remove-connection-watch! ::never-registered))))))

(deftest re-registering-a-key-replaces-its-callback
  (let [first-calls (atom 0)
        second-calls (atom 0)]
    (client/add-connection-watch! test-key (fn [_ _] (swap! first-calls inc)))
    (client/add-connection-watch! test-key (fn [_ _] (swap! second-calls inc)))
    (reset! connection ::conn-1)
    (is (= 0 @first-calls) "the superseded callback is gone, not stacked")
    (is (= 1 @second-calls))))

(deftest a-throwing-subscriber-cannot-break-the-others
  (let [survivor (atom 0)]
    (client/add-connection-watch! ::boom (fn [_ _] (throw (ex-info "boom" {}))))
    (client/add-connection-watch! test-key (fn [_ _] (swap! survivor inc)))
    (try
      (reset! connection ::conn-1)
      (is (= 1 @survivor)
          "one bad subscriber must not take down the connection lifecycle")
      (finally (client/remove-connection-watch! ::boom)))))
