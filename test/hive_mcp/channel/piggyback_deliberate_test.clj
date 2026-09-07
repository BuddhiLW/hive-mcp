(ns hive-mcp.channel.piggyback-deliberate-test
  "A shout an agent made DELIBERATELY (through the hivemind tool) reaches the
   reader even when it sits inside a burst of the runtime's per-turn progress
   telemetry, on both the local and the backbone path — and the marker that
   protects it never leaks into the row a reader sees.

   Measured 2026-09-07: two wave members each called `swarm hivemind shout`
   with \"probe hello from a wave member\"; the digest folded that row into the
   `bb-ling turn 2` telemetry row and the coordinator never saw it."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.piggyback :as pb]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- clean-pb-state
  "Empty cursors + backbone buffer around each test, and put back the message
   source the JVM had BEFORE (hivemind registers the real one at load time).
   Restoring a stub instead would leave every later namespace in the same JVM
   reading an empty source; the e2e piggyback suite measures exactly that."
  [f]
  (let [original-source @pb/message-source-fn]
    (pb/reset-all-cursors!)
    (pb/clear-backbone-buffer!)
    (pb/register-message-source! (constantly []))
    (try (f)
         (finally
           (pb/reset-all-cursors!)
           (pb/clear-backbone-buffer!)
           (pb/register-message-source! original-source)))))

(use-fixtures :each clean-pb-state)

(def ^:private telemetry-then-speech
  [{:agent-id "m0" :event-type :progress :message "bb-ling turn 1 — tool call"
    :timestamp 1000 :project-id "global"}
   {:agent-id "m0" :event-type :progress :message "probe hello"
    :timestamp 1001 :project-id "global" :deliberate? true}
   {:agent-id "m0" :event-type :progress :message "bb-ling turn 2 — text response"
    :timestamp 1002 :project-id "global"}])

(deftest a-deliberate-shout-survives-the-digest-test
  (pb/register-message-source! (constantly telemetry-then-speech))
  (let [out (pb/get-messages "coordinator" :project-id "global")]
    (testing "the member's own words reach the reader; the telemetry still rolls up"
      (is (= ["probe hello" "bb-ling turn 2 — text response"] (mapv :m out)))
      (is (= 2 (:n (second out)))))
    (testing "the marker is for the digest, not for the reader"
      (is (not-any? #(contains? % :deliberate?) out)))))

(deftest the-backbone-path-keeps-the-marker-test
  (testing "a shout arriving over NATS is spared exactly as a local one is"
    (pb/buffer-backbone-event! {:agent-id "m1" :event-type "progress" :message "hello"
                                :timestamp 2000 :project-id "global"
                                :deliberate? true :shout-id "s1"})
    (pb/buffer-backbone-event! {:agent-id "m1" :event-type "progress" :message "turn 3"
                                :timestamp 2001 :project-id "global" :shout-id "s2"})
    (let [out (pb/get-messages "coordinator" :project-id "global")]
      (is (= ["hello" "turn 3"] (mapv :m out)))
      (is (not-any? #(contains? % :deliberate?) out)))))
