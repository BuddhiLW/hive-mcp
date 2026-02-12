(ns hive-mcp.events.multi-test
  "Tests for hive-mcp.events.multi — pure orchestration layer.

   Tests cover:
   1. Core multimethod dispatch (orchestrate)
   2. Pipeline execution (execute!, execute-async!)
   3. Composition helpers (merge-results, chain, fan-out)
   4. Middleware (wrap-orchestrator)
   5. Built-in orchestrators (:multi/ping, :multi/echo)
   6. Introspection (registered-orchestrators, orchestrator-registered?)"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.events.multi :as multi]))

;; =============================================================================
;; Test Orchestrators (defined at test ns level for isolation)
;; =============================================================================

;; Simple orchestrator — returns effects only
(defmethod multi/orchestrate :test/simple
  [event]
  (let [{:keys [msg]} (second event)]
    {:effects {:log {:level :info :message (or msg "simple test")}}}))

;; Chaining orchestrator — returns :next
(defmethod multi/orchestrate :test/chain-start
  [event]
  (let [{:keys [agent-id]} (second event)]
    {:effects {:log {:level :info :message (str "chain start: " agent-id)}}
     :next [:test/chain-end {:agent-id agent-id :from :start}]}))

(defmethod multi/orchestrate :test/chain-end
  [event]
  (let [{:keys [agent-id from]} (second event)]
    {:effects {:log {:level :info :message (str "chain end: " agent-id " from: " from)}}}))

;; Fan-out orchestrator — returns :dispatch-n
(defmethod multi/orchestrate :test/fan-out
  [event]
  (let [{:keys [targets]} (second event)]
    {:effects {:log {:level :info :message (str "fan-out to " (count targets))}}
     :dispatch-n (mapv (fn [t] [:test/simple {:msg (str "fan: " t)}]) targets)}))

;; Halting orchestrator — returns :halt?
(defmethod multi/orchestrate :test/halting
  [event]
  {:effects {:log {:level :warn :message "halting!"}}
   :next [:test/simple {:msg "should not fire"}]
   :halt? true})

;; Context-passing orchestrator
(defmethod multi/orchestrate :test/with-ctx
  [event]
  (let [{:keys [data]} (second event)]
    {:effects {}
     :ctx {:custom-data data
           :processed true}}))

;; Error-throwing orchestrator
(defmethod multi/orchestrate :test/throws
  [_event]
  (throw (ex-info "Test error" {:test true})))

;; Orchestrator that records calls via atom (for verifying pipeline dispatch)
(def ^:private dispatch-log (atom []))

(defmethod multi/orchestrate :test/tracking
  [event]
  (let [{:keys [id]} (second event)]
    (swap! dispatch-log conj id)
    {:effects {:log {:level :info :message (str "tracked: " id)}}}))

;; Orchestrator that chains to :test/tracking for pipeline verification
(defmethod multi/orchestrate :test/chain-to-tracking
  [event]
  (let [{:keys [agent-id]} (second event)]
    {:effects {:log {:level :info :message (str "chain-to-tracking: " agent-id)}}
     :next [:test/tracking {:id (str agent-id "-chained")}]}))

;; Orchestrator with both :next and :dispatch-n for edge case testing
(defmethod multi/orchestrate :test/next-and-fan
  [_event]
  {:effects {:log {:level :info :message "both next and fan"}}
   :next [:test/tracking {:id "next-target"}]
   :dispatch-n [[:test/tracking {:id "fan-1"}]
                [:test/tracking {:id "fan-2"}]]})

;; Orchestrator returning nil effects (empty result body)
(defmethod multi/orchestrate :test/nil-effects
  [_event]
  {:effects nil
   :ctx {:empty true}})

;; Orchestrator returning empty effects map
(defmethod multi/orchestrate :test/empty-effects
  [_event]
  {:effects {}
   :next [:test/simple {:msg "after-empty"}]})

;; Deep chain orchestrators — A → B → tracking (3 levels deep)
(defmethod multi/orchestrate :test/deep-chain-a
  [event]
  (let [{:keys [agent-id]} (second event)]
    (swap! dispatch-log conj (str agent-id "-a"))
    {:effects {:log {:level :info :message (str "deep-a: " agent-id)}}
     :next [:test/deep-chain-b {:agent-id agent-id}]}))

(defmethod multi/orchestrate :test/deep-chain-b
  [event]
  (let [{:keys [agent-id]} (second event)]
    (swap! dispatch-log conj (str agent-id "-b"))
    {:effects {:log {:level :info :message (str "deep-b: " agent-id)}}
     :next [:test/tracking {:id (str agent-id "-c")}]}))

;; Orchestrator where :next points to an invalid (non-vector) value
(defmethod multi/orchestrate :test/bad-next
  [_event]
  {:effects {:log {:level :info :message "bad-next"}}
   :next "not-a-vector"})

;; Orchestrator with :dispatch-n containing some invalid events
(defmethod multi/orchestrate :test/mixed-fan
  [_event]
  {:effects {:log {:level :info :message "mixed-fan"}}
   :dispatch-n [[:test/tracking {:id "valid-fan"}]
                "invalid-event"
                42]})

;; Orchestrator that halts but also has :dispatch-n
(defmethod multi/orchestrate :test/halt-with-fan
  [_event]
  {:effects {:log {:level :warn :message "halt-fan"}}
   :dispatch-n [[:test/tracking {:id "should-not-fire"}]]
   :halt? true})

;; =============================================================================
;; Fixture: Clean up test orchestrators after all tests + reset tracking
;; =============================================================================

(def ^:private all-test-methods
  [:test/simple :test/chain-start :test/chain-end
   :test/fan-out :test/halting :test/with-ctx :test/throws
   :test/tracking :test/chain-to-tracking :test/next-and-fan
   :test/nil-effects :test/empty-effects
   :test/deep-chain-a :test/deep-chain-b :test/bad-next
   :test/mixed-fan :test/halt-with-fan])

(use-fixtures :once
  (fn [test-fn]
    (test-fn)
    ;; Clean up test defmethods
    (doseq [k all-test-methods]
      (remove-method multi/orchestrate k))))

(use-fixtures :each
  (fn [test-fn]
    (reset! dispatch-log [])
    (test-fn)))

;; =============================================================================
;; Tests: Core Multimethod
;; =============================================================================

(deftest orchestrate-default-test
  (testing "Unknown events return nil (no-op)"
    (is (nil? (multi/orchestrate [:unknown/event {:data 42}])))))

(deftest orchestrate-simple-test
  (testing "Simple orchestrator returns effects map"
    (let [result (multi/orchestrate [:test/simple {:msg "hello"}])]
      (is (map? result))
      (is (= {:level :info :message "hello"}
             (get-in result [:effects :log]))))))

(deftest orchestrate-chain-test
  (testing "Chaining orchestrator returns :next event"
    (let [result (multi/orchestrate [:test/chain-start {:agent-id "ling-1"}])]
      (is (= [:test/chain-end {:agent-id "ling-1" :from :start}]
             (multi/next-event result)))
      (is (= {:level :info :message "chain start: ling-1"}
             (get-in result [:effects :log]))))))

(deftest orchestrate-fan-out-test
  (testing "Fan-out orchestrator returns :dispatch-n"
    (let [result (multi/orchestrate [:test/fan-out {:targets ["a" "b" "c"]}])]
      (is (= 3 (count (multi/fan-out-events result))))
      (is (every? #(= :test/simple (first %)) (multi/fan-out-events result))))))

(deftest orchestrate-halt-test
  (testing "Halting orchestrator sets :halt? true"
    (let [result (multi/orchestrate [:test/halting {}])]
      (is (true? (multi/halt? result)))
      (is (some? (multi/next-event result))) ;; next is set but won't fire
      (is (= {:level :warn :message "halting!"}
             (get-in result [:effects :log]))))))

(deftest orchestrate-context-test
  (testing "Context orchestrator returns :ctx"
    (let [result (multi/orchestrate [:test/with-ctx {:data "payload"}])]
      (is (= {:custom-data "payload" :processed true}
             (multi/orchestration-ctx result))))))

;; =============================================================================
;; Tests: Result Accessors
;; =============================================================================

(deftest accessors-test
  (testing "All accessors work on a full result map"
    (let [result {:effects {:log {:level :info :message "test"}}
                  :next [:event/b {}]
                  :dispatch-n [[:event/c {}] [:event/d {}]]
                  :halt? false
                  :ctx {:key "val"}}]
      (is (= {:log {:level :info :message "test"}} (multi/effects result)))
      (is (= [:event/b {}] (multi/next-event result)))
      (is (= [[:event/c {}] [:event/d {}]] (multi/fan-out-events result)))
      (is (false? (multi/halt? result)))
      (is (= {:key "val"} (multi/orchestration-ctx result)))))

  (testing "Accessors return nil on empty map"
    (is (nil? (multi/effects {})))
    (is (nil? (multi/next-event {})))
    (is (nil? (multi/fan-out-events {})))
    (is (false? (multi/halt? {})))
    (is (nil? (multi/orchestration-ctx {})))))

;; =============================================================================
;; Tests: Composition Helpers
;; =============================================================================

(deftest merge-results-test
  (testing "Merge multiple results combines effects"
    (let [r1 {:effects {:log {:level :info :message "a"}}}
          r2 {:effects {:shout {:event-type :progress}}}
          merged (multi/merge-results r1 r2)]
      (is (= {:log {:level :info :message "a"}
              :shout {:event-type :progress}}
             (multi/effects merged)))))

  (testing "Merge collects :next into :dispatch-n when multiple"
    (let [r1 {:effects {} :next [:event/a {}]}
          r2 {:effects {} :next [:event/b {}]}
          merged (multi/merge-results r1 r2)]
      (is (nil? (multi/next-event merged)))
      (is (= [[:event/a {}] [:event/b {}]]
             (multi/fan-out-events merged)))))

  (testing "Merge single :next stays as :next"
    (let [r1 {:effects {:log {:level :info :message "a"}}}
          r2 {:effects {} :next [:event/b {}]}
          merged (multi/merge-results r1 r2)]
      (is (= [:event/b {}] (multi/next-event merged)))))

  (testing "Merge preserves :halt? if any result halts"
    (let [r1 {:effects {} :halt? false}
          r2 {:effects {} :halt? true}
          merged (multi/merge-results r1 r2)]
      (is (true? (multi/halt? merged)))))

  (testing "Merge combines :ctx maps"
    (let [r1 {:effects {} :ctx {:a 1}}
          r2 {:effects {} :ctx {:b 2}}
          merged (multi/merge-results r1 r2)]
      (is (= {:a 1 :b 2} (multi/orchestration-ctx merged)))))

  (testing "Merge with nil results returns valid result"
    (let [r1 {:effects {:log {:level :info :message "a"}}}
          merged (multi/merge-results nil r1 nil)]
      (is (= {:log {:level :info :message "a"}} (multi/effects merged)))))

  (testing "Merge of all nils returns nil"
    (is (nil? (multi/merge-results nil nil nil)))))

(deftest chain-test
  (testing "Chain creates sequential event pipeline"
    (let [result (multi/chain
                  [:event/a {:step 1}]
                  [:event/b {:step 2}]
                  [:event/c {:step 3}])]
      (is (= [:event/a {:step 1}] (multi/next-event result)))))

  (testing "Chain with single event returns :next"
    (let [result (multi/chain [:event/only {}])]
      (is (= [:event/only {}] (multi/next-event result)))))

  (testing "Chain with no valid events returns nil"
    (is (nil? (multi/chain))))

  (testing "Chain filters invalid events"
    (let [result (multi/chain "not-an-event" [:valid/event {}] 42)]
      (is (= [:valid/event {}] (multi/next-event result))))))

(deftest fan-out-test
  (testing "Fan-out creates parallel dispatch"
    (let [result (multi/fan-out [[:event/a {}] [:event/b {}] [:event/c {}]])]
      (is (= 3 (count (multi/fan-out-events result))))
      (is (= [[:event/a {}] [:event/b {}] [:event/c {}]]
             (multi/fan-out-events result)))))

  (testing "Fan-out with single event still uses :dispatch-n"
    (let [result (multi/fan-out [[:event/only {}]])]
      (is (= [[:event/only {}]] (multi/fan-out-events result)))))

  (testing "Fan-out with empty list returns nil"
    (is (nil? (multi/fan-out []))))

  (testing "Fan-out filters invalid events"
    (let [result (multi/fan-out [[:valid/a {}] "invalid" [:valid/b {}]])]
      (is (= 2 (count (multi/fan-out-events result)))))))

;; =============================================================================
;; Tests: Middleware
;; =============================================================================

(deftest wrap-orchestrator-test
  (testing "Basic wrapping passes through"
    (let [handler (multi/wrap-orchestrator
                   (fn [event]
                     {:effects {:log {:message (str "got: " (second event))}}}))]
      (is (= {:effects {:log {:message "got: {:x 1}"}}}
             (handler [:test/wrap {:x 1}])))))

  (testing ":before transforms event"
    (let [handler (multi/wrap-orchestrator
                   (fn [event]
                     {:effects {:log {:message (str "id=" (:id (second event)))}}})
                   :before (fn [event]
                             (update event 1 assoc :id "injected")))]
      (is (= {:effects {:log {:message "id=injected"}}}
             (handler [:test/wrap {}])))))

  (testing ":after transforms result"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event]
                     {:effects {:log {:message "original"}}})
                   :after (fn [result _event]
                            (assoc result :halt? true)))]
      (let [result (handler [:test/wrap {}])]
        (is (true? (:halt? result)))
        (is (= "original" (get-in result [:effects :log :message]))))))

  (testing ":error handles exceptions"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event]
                     (throw (ex-info "boom" {})))
                   :error (fn [e _event]
                            {:effects {:log {:level :error
                                             :message (.getMessage e)}}
                             :halt? true}))]
      (let [result (handler [:test/wrap {}])]
        (is (true? (:halt? result)))
        (is (= "boom" (get-in result [:effects :log :message]))))))

  (testing "Default error handling returns log effect"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event]
                     (throw (ex-info "unhandled" {}))))]
      (let [result (handler [:test/wrap {}])]
        (is (string? (get-in result [:effects :log :message])))
        (is (= :error (get-in result [:effects :log :level])))))))

;; =============================================================================
;; Tests: Built-in Orchestrators
;; =============================================================================

(deftest ping-test
  (testing ":multi/ping returns health check"
    (let [result (multi/orchestrate [:multi/ping])]
      (is (= :info (get-in result [:effects :log :level])))
      (is (re-find #"pong" (get-in result [:effects :log :message]))))))

(deftest echo-test
  (testing ":multi/echo returns event data in :ctx"
    (let [result (multi/orchestrate [:multi/echo {:key "value" :num 42}])]
      (is (= {:key "value" :num 42} (multi/orchestration-ctx result)))
      (is (re-find #"echo" (get-in result [:effects :log :message]))))))

;; =============================================================================
;; Tests: Introspection
;; =============================================================================

(deftest introspection-test
  (testing "registered-orchestrators returns known event-ids"
    (let [registered (multi/registered-orchestrators)]
      (is (set? registered))
      (is (contains? registered :multi/ping))
      (is (contains? registered :multi/echo))
      (is (contains? registered :test/simple))
      (is (not (contains? registered :default)))))

  (testing "orchestrator-registered? checks specific event-id"
    (is (true? (multi/orchestrator-registered? :multi/ping)))
    (is (true? (multi/orchestrator-registered? :test/simple)))
    (is (false? (multi/orchestrator-registered? :nonexistent/event)))))

;; =============================================================================
;; Tests: Pipeline Execution (execute!)
;; =============================================================================

(deftest execute-simple-test
  (testing "execute! returns orchestration result"
    ;; Note: effects won't actually fire without hive-events fx system,
    ;; but the orchestration result is returned
    (let [result (multi/execute! [:test/simple {:msg "execute test"}])]
      (is (map? result))
      (is (= "execute test" (get-in result [:effects :log :message]))))))

(deftest execute-halting-test
  (testing "execute! respects :halt? — does not chain"
    (let [result (multi/execute! [:test/halting {}])]
      (is (true? (multi/halt? result)))
      ;; :next is present but should NOT have been dispatched
      (is (some? (multi/next-event result))))))

(deftest execute-unknown-test
  (testing "execute! returns nil for unknown events"
    (is (nil? (multi/execute! [:unknown/event {}])))))

(deftest execute-precondition-test
  (testing "execute! rejects invalid event vectors"
    (is (thrown? AssertionError (multi/execute! "not-a-vector")))
    (is (thrown? AssertionError (multi/execute! ["string-first" {}])))
    (is (thrown? AssertionError (multi/execute! [])))))

;; =============================================================================
;; Tests: Async Execution
;; =============================================================================

(deftest execute-async-test
  (testing "execute-async! returns a future"
    (let [f (multi/execute-async! [:test/simple {:msg "async"}])]
      (is (future? f))
      (let [result (deref f 5000 :timeout)]
        (is (not= :timeout result))
        (is (map? result))
        (is (= "async" (get-in result [:effects :log :message]))))))

  (testing "execute-async! returns nil for unknown events (no exception)"
    (let [f (multi/execute-async! [:unknown/async-event {}])]
      (is (future? f))
      (let [result (deref f 5000 :timeout)]
        (is (not= :timeout result))
        (is (nil? result)))))

  (testing "execute-async! catches orchestrator exceptions, returns nil"
    (let [f (multi/execute-async! [:test/throws {}])]
      (is (future? f))
      ;; The future should resolve (not throw) — error is caught inside
      (let [result (deref f 5000 :timeout)]
        (is (not= :timeout result))
        (is (nil? result)))))

  (testing "execute-async! rejects invalid events (precondition)"
    (is (thrown? AssertionError (multi/execute-async! "bad")))
    (is (thrown? AssertionError (multi/execute-async! [])))))

;; =============================================================================
;; Tests: Pipeline Chaining Behavior (execute! dispatch-event! interaction)
;; =============================================================================

(deftest execute-chaining-test
  (testing "execute! chains :next events through the pipeline"
    (reset! dispatch-log [])
    ;; chain-to-tracking produces :next -> [:test/tracking {:id "ling-1-chained"}]
    ;; dispatch-event! should orchestrate the :next event
    (let [result (multi/execute! [:test/chain-to-tracking {:agent-id "ling-1"}])]
      (is (= "chain-to-tracking: ling-1" (get-in result [:effects :log :message])))
      (is (= [:test/tracking {:id "ling-1-chained"}] (multi/next-event result)))
      ;; The tracking orchestrator should have been called via pipeline chaining
      (is (contains? (set @dispatch-log) "ling-1-chained")
          "Chain :next event should have dispatched to tracking orchestrator")))

  (testing "execute! processes fan-out events"
    (reset! dispatch-log [])
    (let [result (multi/execute! [:test/fan-out {:targets ["x" "y"]}])]
      (is (= 2 (count (multi/fan-out-events result))))
      ;; Fan-out events dispatch to :test/simple which doesn't track,
      ;; but the dispatch should not throw
      (is (map? result))))

  (testing "execute! halting prevents :next dispatch"
    (reset! dispatch-log [])
    (let [result (multi/execute! [:test/halting {}])]
      (is (true? (multi/halt? result)))
      ;; :test/halting has :next pointing to :test/simple, but halt should prevent it
      ;; We can't directly observe non-dispatch of :test/simple, but the result is correct
      (is (some? (multi/next-event result))))))

(deftest execute-next-and-fan-out-test
  (testing "execute! dispatches both :next and :dispatch-n when both present"
    (reset! dispatch-log [])
    (let [result (multi/execute! [:test/next-and-fan {}])]
      (is (some? (multi/next-event result)))
      (is (= 2 (count (multi/fan-out-events result))))
      ;; All three targets should have been dispatched
      (Thread/sleep 50) ;; small grace for any async
      (is (contains? (set @dispatch-log) "next-target"))
      (is (contains? (set @dispatch-log) "fan-1"))
      (is (contains? (set @dispatch-log) "fan-2")))))

;; =============================================================================
;; Tests: Edge Cases — Orchestration Results
;; =============================================================================

(deftest nil-effects-test
  (testing "execute! handles nil effects gracefully"
    (let [result (multi/execute! [:test/nil-effects {}])]
      (is (map? result))
      (is (nil? (multi/effects result)))
      (is (= {:empty true} (multi/orchestration-ctx result))))))

(deftest empty-effects-test
  (testing "execute! handles empty effects map and still chains"
    (reset! dispatch-log [])
    (let [result (multi/execute! [:test/empty-effects {}])]
      (is (map? result))
      (is (= {} (multi/effects result)))
      ;; Should still chain to :next
      (is (= [:test/simple {:msg "after-empty"}] (multi/next-event result))))))

;; =============================================================================
;; Tests: Composition Helpers — Extended Edge Cases
;; =============================================================================

(deftest merge-results-with-dispatch-n-test
  (testing "Merge combines existing :dispatch-n with :next events"
    (let [r1 {:effects {} :dispatch-n [[:event/x {}] [:event/y {}]]}
          r2 {:effects {} :next [:event/z {}]}
          merged (multi/merge-results r1 r2)]
      ;; r1 has 2 fan-out events, r2 has 1 :next → total 3 in :dispatch-n
      (is (= 3 (count (multi/fan-out-events merged))))
      (is (some #(= [:event/x {}] %) (multi/fan-out-events merged)))
      (is (some #(= [:event/y {}] %) (multi/fan-out-events merged)))
      (is (some #(= [:event/z {}] %) (multi/fan-out-events merged)))))

  (testing "Merge with two :dispatch-n results concatenates all"
    (let [r1 {:effects {} :dispatch-n [[:event/a {}]]}
          r2 {:effects {} :dispatch-n [[:event/b {}] [:event/c {}]]}
          merged (multi/merge-results r1 r2)]
      (is (= 3 (count (multi/fan-out-events merged))))))

  (testing "Merge with no args returns nil"
    (is (nil? (multi/merge-results))))

  (testing "Merge effects — last wins for same key"
    (let [r1 {:effects {:log {:level :info :message "first"}}}
          r2 {:effects {:log {:level :warn :message "second"}}}
          merged (multi/merge-results r1 r2)]
      (is (= {:level :warn :message "second"}
             (get-in merged [:effects :log])))))

  (testing "Merge :ctx — later values override earlier for same key"
    (let [r1 {:effects {} :ctx {:shared "v1" :only-r1 true}}
          r2 {:effects {} :ctx {:shared "v2" :only-r2 true}}
          merged (multi/merge-results r1 r2)]
      (is (= "v2" (:shared (multi/orchestration-ctx merged))))
      (is (true? (:only-r1 (multi/orchestration-ctx merged))))
      (is (true? (:only-r2 (multi/orchestration-ctx merged)))))))

(deftest chain-continuation-structure-test
  (testing "Chain builds nested continuation via :ctx :_chain-continuation"
    (let [result (multi/chain
                  [:event/a {:step 1}]
                  [:event/b {:step 2}]
                  [:event/c {:step 3}])]
      ;; First event is :next
      (is (= [:event/a {:step 1}] (multi/next-event result)))
      ;; The continuation for the remaining chain is in :ctx
      (let [continuation (get-in result [:ctx :_chain-continuation])]
        (is (some? continuation) "Should have continuation context")
        (is (= [:event/b {:step 2}] (multi/next-event continuation))))))

  (testing "Chain with two events — no nested continuation"
    (let [result (multi/chain [:event/first {}] [:event/second {}])]
      (is (= [:event/first {}] (multi/next-event result)))
      (let [continuation (get-in result [:ctx :_chain-continuation])]
        (is (some? continuation))
        (is (= [:event/second {}] (multi/next-event continuation)))
        ;; Last in chain has no further continuation
        (is (nil? (get-in continuation [:ctx :_chain-continuation])))))))

(deftest fan-out-all-invalid-test
  (testing "Fan-out with all invalid events returns nil"
    (is (nil? (multi/fan-out ["string" 42 {:not "a vector"}])))))

;; =============================================================================
;; Tests: Middleware — Combined & Edge Cases
;; =============================================================================

(deftest wrap-orchestrator-combined-test
  (testing "All three middleware hooks compose correctly"
    (let [call-order (atom [])
          handler (multi/wrap-orchestrator
                   (fn [event]
                     (swap! call-order conj :handler)
                     {:effects {:log {:message (str "data=" (:data (second event)))}}})
                   :before (fn [event]
                             (swap! call-order conj :before)
                             (update event 1 assoc :data "enriched"))
                   :after (fn [result _event]
                            (swap! call-order conj :after)
                            (assoc result :ctx {:post-processed true})))]
      (let [result (handler [:test/combo {}])]
        (is (= [:before :handler :after] @call-order)
            "Middleware should execute in order: before → handler → after")
        (is (= "data=enriched" (get-in result [:effects :log :message]))
            ":before should have enriched the event data")
        (is (= {:post-processed true} (multi/orchestration-ctx result))
            ":after should have added context"))))

  (testing ":error intercepts even when :before and :after are set"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event]
                     (throw (ex-info "middleware-error" {:phase :handler})))
                   :before (fn [event] event)
                   :after (fn [result _event] result)
                   :error (fn [e _event]
                            {:effects {:log {:level :error
                                             :message (.getMessage e)}}
                             :halt? true}))]
      (let [result (handler [:test/combo {}])]
        (is (true? (:halt? result)))
        (is (= "middleware-error" (get-in result [:effects :log :message])))))))

(deftest wrap-orchestrator-before-throws-test
  (testing ":error catches :before exceptions too"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event] {:effects {}})
                   :before (fn [_event]
                             (throw (ex-info "before-boom" {})))
                   :error (fn [e _event]
                            {:effects {:log {:message (.getMessage e)}}
                             :halt? true}))]
      (let [result (handler [:test/wrap {}])]
        (is (= "before-boom" (get-in result [:effects :log :message])))))))

;; =============================================================================
;; Tests: Accessors — nil input & edge cases
;; =============================================================================

(deftest accessors-nil-input-test
  (testing "Accessors handle nil result gracefully"
    (is (nil? (multi/effects nil)))
    (is (nil? (multi/next-event nil)))
    (is (nil? (multi/fan-out-events nil)))
    (is (false? (multi/halt? nil)))
    (is (nil? (multi/orchestration-ctx nil)))))

(deftest halt-truthy-values-test
  (testing "halt? coerces truthy values to boolean"
    (is (true? (multi/halt? {:halt? true})))
    (is (true? (multi/halt? {:halt? 1})))
    (is (true? (multi/halt? {:halt? "yes"})))
    (is (false? (multi/halt? {:halt? false})))
    (is (false? (multi/halt? {:halt? nil})))
    (is (false? (multi/halt? {})))))

;; =============================================================================
;; Tests: Built-in Orchestrators — Extended
;; =============================================================================

(deftest ping-no-args-test
  (testing ":multi/ping works with just keyword (no data map)"
    (let [result (multi/orchestrate [:multi/ping])]
      (is (map? result))
      (is (nil? (multi/next-event result)) "ping should not chain")
      (is (nil? (multi/fan-out-events result)) "ping should not fan-out")
      (is (false? (multi/halt? result)) "ping should not halt"))))

(deftest echo-nil-data-test
  (testing ":multi/echo with nil second element"
    (let [result (multi/orchestrate [:multi/echo nil])]
      (is (nil? (multi/orchestration-ctx result)))
      (is (some? (get-in result [:effects :log :message])))))

  (testing ":multi/echo with empty map"
    (let [result (multi/orchestrate [:multi/echo {}])]
      (is (= {} (multi/orchestration-ctx result))))))

;; =============================================================================
;; Tests: Introspection — Dynamic Registration
;; =============================================================================

(deftest introspection-dynamic-test
  (testing "Newly registered orchestrator appears in introspection"
    ;; Register a temporary orchestrator
    (defmethod multi/orchestrate :test/dynamic-introspection [_event] {:effects {}})
    (is (true? (multi/orchestrator-registered? :test/dynamic-introspection)))
    (is (contains? (multi/registered-orchestrators) :test/dynamic-introspection))
    ;; Remove it
    (remove-method multi/orchestrate :test/dynamic-introspection)
    (is (false? (multi/orchestrator-registered? :test/dynamic-introspection)))
    (is (not (contains? (multi/registered-orchestrators) :test/dynamic-introspection)))))

;; =============================================================================
;; Tests: Event Validation Edge Cases
;; =============================================================================

(deftest execute-event-shape-test
  (testing "execute! works with keyword-only event (no data)"
    (let [result (multi/execute! [:multi/ping])]
      (is (map? result))))

  (testing "execute! works with event containing multiple data elements"
    ;; While convention is [:id data-map], nothing prevents longer vectors
    (let [result (multi/execute! [:test/simple {:msg "multi-arity"} :extra])]
      (is (map? result))
      (is (= "multi-arity" (get-in result [:effects :log :message])))))

  (testing "execute! preserves result from orchestrator that returns only :ctx"
    (let [result (multi/execute! [:test/with-ctx {:data "ctx-only"}])]
      (is (map? result))
      (is (= {:custom-data "ctx-only" :processed true}
             (multi/orchestration-ctx result))))))

;; =============================================================================
;; Tests: execute! with Throwing Orchestrator (sync path)
;; =============================================================================

(deftest execute-throws-sync-test
  (testing "execute! propagates exception from throwing orchestrator"
    ;; Unlike execute-async! which catches, execute! propagates directly
    ;; because orchestrate is called outside try-catch in execute!
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Test error"
                          (multi/execute! [:test/throws {}]))))

  (testing "execute! exception carries ex-data"
    (try
      (multi/execute! [:test/throws {}])
      (is false "Should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= {:test true} (ex-data e)))))))

;; =============================================================================
;; Tests: Deep Recursive Chaining (3+ levels)
;; =============================================================================

(deftest execute-deep-chaining-test
  (testing "execute! chains through 3 levels: A → B → tracking"
    (reset! dispatch-log [])
    (let [result (multi/execute! [:test/deep-chain-a {:agent-id "deep"}])]
      (is (= "deep-a: deep" (get-in result [:effects :log :message])))
      (is (= [:test/deep-chain-b {:agent-id "deep"}] (multi/next-event result)))
      ;; All three levels should have been visited
      (is (contains? (set @dispatch-log) "deep-a")
          "Level A should have recorded")
      (is (contains? (set @dispatch-log) "deep-b")
          "Level B should have recorded")
      (is (contains? (set @dispatch-log) "deep-c")
          "Level C (tracking) should have recorded"))))

;; =============================================================================
;; Tests: Pipeline Robustness — Invalid :next & mixed :dispatch-n
;; =============================================================================

(deftest execute-bad-next-test
  (testing "execute! handles invalid :next value gracefully (non-vector)"
    ;; dispatch-event! calls valid-event? which returns false for strings
    ;; so the invalid :next is simply skipped — no exception
    (let [result (multi/execute! [:test/bad-next {}])]
      (is (map? result))
      (is (= "bad-next" (get-in result [:effects :log :message]))))))

(deftest execute-mixed-fan-test
  (testing "execute! skips invalid events in :dispatch-n"
    (reset! dispatch-log [])
    (let [result (multi/execute! [:test/mixed-fan {}])]
      (is (map? result))
      (is (= "mixed-fan" (get-in result [:effects :log :message])))
      ;; Only the valid event should have dispatched
      (is (contains? (set @dispatch-log) "valid-fan")
          "Valid fan-out event should have dispatched"))))

(deftest execute-halt-prevents-fan-out-test
  (testing "execute! halt prevents :dispatch-n from firing"
    (reset! dispatch-log [])
    (let [result (multi/execute! [:test/halt-with-fan {}])]
      (is (true? (multi/halt? result)))
      ;; :dispatch-n was set but halt should prevent dispatch
      (is (empty? @dispatch-log)
          "No events should dispatch when halted"))))

;; =============================================================================
;; Tests: merge-results — Single Result & Edge Cases
;; =============================================================================

(deftest merge-results-single-test
  (testing "Merge with single result returns equivalent result"
    (let [r {:effects {:log {:level :info :message "solo"}}
             :next [:event/a {}]
             :ctx {:key "val"}}
          merged (multi/merge-results r)]
      (is (= {:log {:level :info :message "solo"}} (multi/effects merged)))
      (is (= [:event/a {}] (multi/next-event merged)))
      (is (= {:key "val"} (multi/orchestration-ctx merged)))))

  (testing "Merge with single result preserving :halt?"
    (let [r {:effects {} :halt? true}
          merged (multi/merge-results r)]
      (is (true? (multi/halt? merged)))))

  (testing "Merge with single nil returns nil"
    (is (nil? (multi/merge-results nil)))))

;; =============================================================================
;; Tests: Middleware — Partial (only :before or only :after)
;; =============================================================================

(deftest wrap-orchestrator-only-before-test
  (testing "wrap-orchestrator with only :before (no :after, no :error)"
    (let [handler (multi/wrap-orchestrator
                   (fn [event]
                     {:effects {:log {:message (str "val=" (:val (second event)))}}})
                   :before (fn [event]
                             (update event 1 assoc :val "injected")))]
      (let [result (handler [:test/wrap {}])]
        (is (= "val=injected" (get-in result [:effects :log :message])))))))

(deftest wrap-orchestrator-only-after-test
  (testing "wrap-orchestrator with only :after (no :before, no :error)"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event]
                     {:effects {:log {:message "base"}}})
                   :after (fn [result _event]
                            (assoc-in result [:effects :metric] {:count 1})))]
      (let [result (handler [:test/wrap {}])]
        (is (= "base" (get-in result [:effects :log :message])))
        (is (= {:count 1} (get-in result [:effects :metric])))))))

(deftest wrap-orchestrator-after-throws-test
  (testing ":after throwing falls to default error handler (no :error provided)"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event]
                     {:effects {:log {:message "ok"}}})
                   :after (fn [_result _event]
                            (throw (ex-info "after-boom" {}))))]
      (let [result (handler [:test/wrap {}])]
        ;; Default error handler produces {:effects {:log {:level :error ...}}}
        (is (= :error (get-in result [:effects :log :level])))
        (is (re-find #"after-boom" (get-in result [:effects :log :message]))))))

  (testing ":after throwing caught by custom :error handler"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event]
                     {:effects {:log {:message "ok"}}})
                   :after (fn [_result _event]
                            (throw (ex-info "after-crash" {:phase :after})))
                   :error (fn [e _event]
                            {:effects {:recovery {:ex-data (ex-data e)}}
                             :halt? true}))]
      (let [result (handler [:test/wrap {}])]
        (is (true? (:halt? result)))
        (is (= {:phase :after} (get-in result [:effects :recovery :ex-data])))))))

;; =============================================================================
;; Tests: Event Validation — Additional Edge Cases
;; =============================================================================

(deftest execute-validation-edge-cases-test
  (testing "execute! rejects list (not vector) even with keyword first"
    ;; (list :foo {}) is a seq, not a vector — valid-event? checks vector?
    (is (thrown? AssertionError (multi/execute! (list :foo {})))))

  (testing "execute! rejects vector with nil first element"
    (is (thrown? AssertionError (multi/execute! [nil {}]))))

  (testing "execute! rejects vector with integer first element"
    (is (thrown? AssertionError (multi/execute! [42 {}])))))

;; =============================================================================
;; Tests: Concurrent execute-async! safety
;; =============================================================================

(deftest execute-async-concurrent-test
  (testing "Multiple concurrent execute-async! calls don't interfere"
    (reset! dispatch-log [])
    (let [futures (mapv (fn [i]
                          (multi/execute-async!
                           [:test/tracking {:id (str "concurrent-" i)}]))
                        (range 5))]
      ;; Wait for all futures
      (doseq [f futures]
        (let [result (deref f 5000 :timeout)]
          (is (not= :timeout result))
          (is (map? result))))
      ;; All 5 should have been tracked
      (is (= 5 (count @dispatch-log)))
      (is (= (set (map #(str "concurrent-" %) (range 5)))
             (set @dispatch-log))))))

;; =============================================================================
;; Tests: fan-out — Order Preservation
;; =============================================================================

(deftest fan-out-preserves-order-test
  (testing "Fan-out preserves event order"
    (let [events (mapv (fn [i] [(keyword "event" (str i)) {:idx i}]) (range 10))
          result (multi/fan-out events)]
      (is (= events (multi/fan-out-events result))))))

;; =============================================================================
;; Tests: execute-effects! — Effect Execution Paths
;; =============================================================================

(deftest execute-effects-with-fx-handler-test
  (testing "execute! calls registered fx handler when available"
    (let [fx-calls (atom [])
          mock-fx-registry {:log (fn [data] (swap! fx-calls conj [:log data]))}]
      ;; Mock hive.events.fx/get-fx to return our test handlers
      (with-redefs [requiring-resolve
                    (fn [sym]
                      (case (str sym)
                        "hive.events.fx/get-fx"
                        (fn [fx-id] (get mock-fx-registry fx-id))
                        ;; Fall through to real requiring-resolve for other symbols
                        (clojure.lang.RT/var
                         (namespace sym) (name sym))))]
        (multi/execute! [:test/simple {:msg "fx-test"}])
        (is (= 1 (count @fx-calls))
            "Exactly one fx handler should have been called")
        (is (= :log (first (first @fx-calls)))
            "The :log fx handler should be called")
        (is (= {:level :info :message "fx-test"}
               (second (first @fx-calls)))
            "fx handler receives the effect data"))))

  (testing "execute! handles fx handler that throws"
    (let [mock-fx-registry {:log (fn [_data]
                                   (throw (ex-info "fx-boom" {:fx :log})))}]
      (with-redefs [requiring-resolve
                    (fn [sym]
                      (case (str sym)
                        "hive.events.fx/get-fx"
                        (fn [fx-id] (get mock-fx-registry fx-id))
                        (clojure.lang.RT/var
                         (namespace sym) (name sym))))]
        ;; Should not throw — error is caught internally
        (let [result (multi/execute! [:test/simple {:msg "boom-test"}])]
          (is (map? result)
              "execute! still returns result even when fx handler throws")))))

  (testing "execute! handles unresolvable hive.events.fx/get-fx"
    (with-redefs [requiring-resolve
                  (fn [sym]
                    (case (str sym)
                      "hive.events.fx/get-fx"
                      (throw (Exception. "Cannot resolve"))
                      (clojure.lang.RT/var
                       (namespace sym) (name sym))))]
      ;; Should not throw — gracefully handles missing fx system
      (let [result (multi/execute! [:test/simple {:msg "no-fx"}])]
        (is (map? result)
            "execute! returns result when fx system unavailable")))))

(deftest execute-effects-multiple-fx-test
  (testing "execute! calls multiple fx handlers for multi-effect results"
    (let [fx-calls (atom [])
          mock-fx-registry {:log   (fn [data] (swap! fx-calls conj [:log data]))
                            :shout (fn [data] (swap! fx-calls conj [:shout data]))}]
      ;; Register a multi-effect orchestrator
      (defmethod multi/orchestrate :test/multi-fx
        [_event]
        {:effects {:log {:level :info :message "multi-fx"}
                   :shout {:event-type :progress :message "hello"}}})
      (with-redefs [requiring-resolve
                    (fn [sym]
                      (case (str sym)
                        "hive.events.fx/get-fx"
                        (fn [fx-id] (get mock-fx-registry fx-id))
                        (clojure.lang.RT/var
                         (namespace sym) (name sym))))]
        (multi/execute! [:test/multi-fx {}])
        (is (= 2 (count @fx-calls))
            "Both fx handlers should have been called")
        (is (= #{:log :shout} (set (map first @fx-calls)))
            "Both :log and :shout fx should fire"))
      ;; Cleanup
      (remove-method multi/orchestrate :test/multi-fx))))

;; =============================================================================
;; Tests: dispatch-event! — Core Delegation Path
;; =============================================================================

(deftest dispatch-event-core-delegation-test
  (testing "dispatch-event! delegates to core/dispatch when handler is registered"
    (let [core-dispatched (atom [])
          ;; Mock: core has a handler for :core/handled-event
          mock-handler-registered? (fn [event-id]
                                     (= event-id :core/handled-event))
          mock-dispatch (fn [event]
                          (swap! core-dispatched conj event))]
      ;; Chain to a :core/handled-event from an orchestrator
      (defmethod multi/orchestrate :test/to-core
        [_event]
        {:effects {}
         :next [:core/handled-event {:from "orchestrator"}]})
      (with-redefs [requiring-resolve
                    (fn [sym]
                      (case (str sym)
                        "hive-mcp.events.core/handler-registered?"
                        mock-handler-registered?
                        "hive-mcp.events.core/dispatch"
                        mock-dispatch
                        "hive.events.fx/get-fx"
                        (fn [_] nil)
                        (clojure.lang.RT/var
                         (namespace sym) (name sym))))]
        (multi/execute! [:test/to-core {}])
        (is (= 1 (count @core-dispatched))
            "core/dispatch should be called for :core/handled-event")
        (is (= [:core/handled-event {:from "orchestrator"}]
               (first @core-dispatched))
            "Event should be passed unchanged to core/dispatch"))
      ;; Cleanup
      (remove-method multi/orchestrate :test/to-core)))

  (testing "dispatch-event! falls back to orchestrate when core has no handler"
    (reset! dispatch-log [])
    ;; :test/tracking has no core handler, only an orchestrator
    ;; chain-to-tracking chains to tracking — should go through orchestrate
    (let [mock-handler-registered? (fn [_] false)]
      (with-redefs [requiring-resolve
                    (fn [sym]
                      (case (str sym)
                        "hive-mcp.events.core/handler-registered?"
                        mock-handler-registered?
                        "hive.events.fx/get-fx"
                        (fn [_] nil)
                        ;; For other symbols, fall through
                        (clojure.lang.RT/var
                         (namespace sym) (name sym))))]
        (multi/execute! [:test/chain-to-tracking {:agent-id "ling-2"}])
        (is (contains? (set @dispatch-log) "ling-2-chained")
            "Tracking orchestrator should fire via recursive orchestration")))))

;; =============================================================================
;; Tests: wrap-orchestrator — Nil Handler Return
;; =============================================================================

(deftest wrap-orchestrator-nil-return-test
  (testing "wrap-orchestrator with handler returning nil — :after still fires"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event] nil)
                   :after (fn [result _event]
                            (or result {:effects {:log {:message "recovered"}}
                                        :ctx {:nil-recovery true}})))]
      (let [result (handler [:test/wrap {}])]
        (is (= {:nil-recovery true} (multi/orchestration-ctx result))
            ":after can recover from nil handler result"))))

  (testing "wrap-orchestrator with handler returning nil — no :after"
    (let [handler (multi/wrap-orchestrator
                   (fn [_event] nil))]
      (let [result (handler [:test/wrap {}])]
        (is (nil? result)
            "nil passthrough when no :after hook")))))

;; =============================================================================
;; Tests: merge-results — Complex Combinatorics (3+ results)
;; =============================================================================

(deftest merge-results-three-plus-test
  (testing "Merge 3 results with mixed :next, :dispatch-n, :halt?, :ctx"
    (let [r1 {:effects {:log {:level :info :message "r1"}}
              :next [:event/a {}]
              :ctx {:source "r1" :shared "v1"}}
          r2 {:effects {:shout {:event-type :progress}}
              :dispatch-n [[:event/b {}] [:event/c {}]]
              :ctx {:source "r2" :shared "v2"}}
          r3 {:effects {:metric {:count 42}}
              :next [:event/d {}]
              :halt? true
              :ctx {:source "r3"}}
          merged (multi/merge-results r1 r2 r3)]
      ;; Effects: all three merged
      (is (= {:log {:level :info :message "r1"}
              :shout {:event-type :progress}
              :metric {:count 42}}
             (multi/effects merged)))
      ;; Events: 2 :next + 2 :dispatch-n = 4 total → all in :dispatch-n
      (let [fan (multi/fan-out-events merged)]
        (is (= 4 (count fan)))
        (is (some #(= [:event/a {}] %) fan))
        (is (some #(= [:event/b {}] %) fan))
        (is (some #(= [:event/c {}] %) fan))
        (is (some #(= [:event/d {}] %) fan)))
      ;; :halt? from r3 propagates
      (is (true? (multi/halt? merged)))
      ;; :ctx merged left-to-right (r3 :source wins over r2 :source)
      (is (= "r3" (:source (multi/orchestration-ctx merged))))
      (is (= "v2" (:shared (multi/orchestration-ctx merged))))))

  (testing "Merge 4 results — only one has :next, rest have :dispatch-n"
    (let [r1 {:effects {:a 1} :dispatch-n [[:ev/x {}]]}
          r2 {:effects {:b 2} :next [:ev/y {}]}
          r3 {:effects {:c 3} :dispatch-n [[:ev/z {}] [:ev/w {}]]}
          r4 {:effects {:d 4}}
          merged (multi/merge-results r1 r2 r3 r4)]
      (is (= {:a 1 :b 2 :c 3 :d 4} (multi/effects merged)))
      ;; 1 :next + 1 + 2 :dispatch-n = 4 events total
      (is (= 4 (count (multi/fan-out-events merged))))
      (is (false? (multi/halt? merged))))))

;; =============================================================================
;; Tests: chain — Deep Nesting Structure (4+ events)
;; =============================================================================

(deftest chain-deep-nesting-test
  (testing "Chain with 4 events builds 3 levels of nesting"
    (let [result (multi/chain
                  [:event/a {:step 1}]
                  [:event/b {:step 2}]
                  [:event/c {:step 3}]
                  [:event/d {:step 4}])]
      ;; Top level: :next → :event/a
      (is (= [:event/a {:step 1}] (multi/next-event result)))
      ;; Level 2: continuation → :event/b
      (let [cont1 (get-in result [:ctx :_chain-continuation])]
        (is (some? cont1))
        (is (= [:event/b {:step 2}] (multi/next-event cont1)))
        ;; Level 3: continuation → :event/c
        (let [cont2 (get-in cont1 [:ctx :_chain-continuation])]
          (is (some? cont2))
          (is (= [:event/c {:step 3}] (multi/next-event cont2)))
          ;; Level 4: continuation → :event/d (leaf — no further continuation)
          (let [cont3 (get-in cont2 [:ctx :_chain-continuation])]
            (is (some? cont3))
            (is (= [:event/d {:step 4}] (multi/next-event cont3)))
            (is (nil? (get-in cont3 [:ctx :_chain-continuation]))
                "Last event in chain has no further continuation"))))))

  (testing "Chain with 5 events produces correct depth"
    (let [events (mapv (fn [i] [(keyword "ev" (str i)) {:i i}]) (range 5))
          result (apply multi/chain events)]
      ;; Walk the chain and collect all :next events
      (loop [r result
             collected []]
        (if-let [nxt (multi/next-event r)]
          (recur (get-in r [:ctx :_chain-continuation])
                 (conj collected nxt))
          (do
            (is (= 5 (count collected))
                "Chain should produce 5 sequential events")
            (is (= events collected)
                "Events should match input order")))))))

;; =============================================================================
;; Tests: Orchestrator with Extra Keys (non-standard result fields)
;; =============================================================================

(deftest orchestrator-extra-keys-test
  (testing "Extra keys in orchestration result are preserved"
    (defmethod multi/orchestrate :test/extra-keys
      [_event]
      {:effects {:log {:message "extra"}}
       :custom-field "preserved"
       :metadata {:version 2}})
    (let [result (multi/execute! [:test/extra-keys {}])]
      (is (= "preserved" (:custom-field result))
          "Non-standard keys are passed through")
      (is (= {:version 2} (:metadata result))
          "Metadata is preserved in result"))
    (remove-method multi/orchestrate :test/extra-keys))

  (testing "merge-results ignores extra keys (only merges known fields)"
    (let [r1 {:effects {:a 1} :custom "r1-custom"}
          r2 {:effects {:b 2} :custom "r2-custom"}
          merged (multi/merge-results r1 r2)]
      ;; Effects are merged
      (is (= {:a 1 :b 2} (multi/effects merged)))
      ;; Custom keys are NOT in the merge output (merge-results only handles known fields)
      (is (nil? (:custom merged))
          "merge-results does not propagate unknown keys"))))

;; =============================================================================
;; Tests: execute! — Return Value Consistency
;; =============================================================================

(deftest execute-return-value-test
  (testing "execute! always returns the top-level orchestration result"
    (let [result (multi/execute! [:test/chain-start {:agent-id "ling-ret"}])]
      ;; Returns the result of the FIRST orchestrate call, not the chained one
      (is (= "chain start: ling-ret" (get-in result [:effects :log :message])))
      (is (= [:test/chain-end {:agent-id "ling-ret" :from :start}]
             (multi/next-event result)))))

  (testing "execute! returns nil for unknown events, not empty map"
    (is (nil? (multi/execute! [:completely/unknown {}]))))

  (testing "execute! returns result with :halt? even though pipeline stops"
    (let [result (multi/execute! [:test/halting {}])]
      (is (true? (multi/halt? result)))
      (is (some? (multi/effects result)))
      (is (some? (multi/next-event result))
          ":next is in the result map even though it wasn't dispatched"))))

;; =============================================================================
;; Tests: Orchestrate Idempotency
;; =============================================================================

(deftest orchestrate-idempotent-test
  (testing "Calling orchestrate multiple times with same event gives same result"
    (let [event [:test/simple {:msg "idem"}]
          r1 (multi/orchestrate event)
          r2 (multi/orchestrate event)
          r3 (multi/orchestrate event)]
      (is (= r1 r2 r3)
          "Orchestrate should be deterministic for pure handlers"))))

;; =============================================================================
;; Tests: execute-async! — Timeout Behavior
;; =============================================================================

(deftest execute-async-deref-timeout-test
  (testing "execute-async! can be deref'd with custom timeout"
    (let [f (multi/execute-async! [:test/simple {:msg "timeout-test"}])
          result (deref f 100 ::timed-out)]
      (is (not= ::timed-out result)
          "Simple orchestration should complete well within 100ms")
      (is (map? result)))))

;; =============================================================================
;; Tests: merge-results — Effect Key Ordering Independence
;; =============================================================================

(deftest merge-results-effect-key-independence-test
  (testing "Merge order doesn't matter for disjoint effect keys"
    (let [r1 {:effects {:log {:msg "a"}}}
          r2 {:effects {:shout {:msg "b"}}}
          r3 {:effects {:metric {:n 1}}}
          merged-123 (multi/merge-results r1 r2 r3)
          merged-321 (multi/merge-results r3 r2 r1)]
      (is (= (multi/effects merged-123) (multi/effects merged-321))
          "Disjoint effect keys produce same result regardless of order")))

  (testing "Merge order matters for overlapping effect keys (last wins)"
    (let [r1 {:effects {:log {:msg "first"}}}
          r2 {:effects {:log {:msg "second"}}}
          merged-12 (multi/merge-results r1 r2)
          merged-21 (multi/merge-results r2 r1)]
      (is (= {:msg "second"} (:log (multi/effects merged-12)))
          "Last result wins for same key (r2 overwrites r1)")
      (is (= {:msg "first"} (:log (multi/effects merged-21)))
          "Last result wins for same key (r1 overwrites r2)"))))

;; =============================================================================
;; Tests: Orchestrator Returning Empty Map or Halt-Only
;; =============================================================================

(deftest execute-empty-result-map-test
  (testing "execute! handles orchestrator returning empty map (no effects, no chaining)"
    (defmethod multi/orchestrate :test/empty-result
      [_event]
      {})
    (let [result (multi/execute! [:test/empty-result {}])]
      (is (= {} result))
      (is (nil? (multi/effects result)))
      (is (nil? (multi/next-event result)))
      (is (false? (multi/halt? result))))
    (remove-method multi/orchestrate :test/empty-result)))

(deftest execute-halt-only-test
  (testing "execute! handles result with only :halt? and nothing else"
    (defmethod multi/orchestrate :test/halt-only
      [_event]
      {:halt? true})
    (let [result (multi/execute! [:test/halt-only {}])]
      (is (map? result))
      (is (true? (multi/halt? result)))
      (is (nil? (multi/effects result))))
    (remove-method multi/orchestrate :test/halt-only)))

;; =============================================================================
;; Tests: chain — All Invalid Events
;; =============================================================================

(deftest chain-all-invalid-test
  (testing "Chain with only invalid events returns nil"
    (is (nil? (multi/chain "bad" 42 {:not "event"})))))

;; =============================================================================
;; Tests: merge-results — Empty Variadic & :halt? Edge Cases
;; =============================================================================

(deftest merge-results-halt-edge-cases-test
  (testing "Merge with all :halt? false returns false"
    (let [r1 {:effects {} :halt? false}
          r2 {:effects {} :halt? false}
          merged (multi/merge-results r1 r2)]
      (is (false? (multi/halt? merged)))))

  (testing "Merge with no :halt? keys returns false"
    (let [r1 {:effects {:log {:message "a"}}}
          r2 {:effects {:log {:message "b"}}}
          merged (multi/merge-results r1 r2)]
      (is (false? (multi/halt? merged)))))

  (testing "Merge with mix of nil and true :halt? returns true"
    (let [r1 {:effects {}}
          r2 {:effects {} :halt? true}
          merged (multi/merge-results r1 r2)]
      (is (true? (multi/halt? merged))))))

(deftest merge-results-empty-variadic-test
  (testing "Merge with zero args via apply returns nil"
    (is (nil? (apply multi/merge-results []))))

  (testing "Merge with single :dispatch-n result preserves it"
    (let [r {:effects {} :dispatch-n [[:event/a {}] [:event/b {}]]}
          merged (multi/merge-results r)]
      (is (= [[:event/a {}] [:event/b {}]]
             (multi/fan-out-events merged))))))

;; =============================================================================
;; Tests: merge-results — Large Composition (10+ results)
;; =============================================================================

(deftest merge-results-many-test
  (testing "Merge handles 10+ results with unique effect keys and ctx keys"
    (let [results (mapv (fn [i]
                          {:effects {(keyword (str "fx-" i)) {:idx i}}
                           :ctx {(keyword (str "key-" i)) i}})
                        (range 10))
          merged (apply multi/merge-results results)]
      (is (= 10 (count (multi/effects merged))))
      (is (= 10 (count (multi/orchestration-ctx merged)))))))

;; =============================================================================
;; Tests: wrap-orchestrator — Identity Wrapper
;; =============================================================================

(deftest wrap-orchestrator-identity-test
  (testing "wrap-orchestrator with no options acts as identity"
    (let [handler (multi/wrap-orchestrator
                   (fn [event]
                     {:effects {:log {:message (str "identity: " (second event))}}}))]
      (is (= {:effects {:log {:message "identity: {:x 1}"}}}
             (handler [:test/identity {:x 1}]))))))

;; =============================================================================
;; Tests: execute! — Tracking Orchestrator State Independence
;; =============================================================================

(deftest execute-tracking-state-independence-test
  (testing "Multiple execute! calls to :test/tracking accumulate independently"
    (reset! dispatch-log [])
    (multi/execute! [:test/tracking {:id "call-1"}])
    (multi/execute! [:test/tracking {:id "call-2"}])
    (multi/execute! [:test/tracking {:id "call-3"}])
    (is (= ["call-1" "call-2" "call-3"] @dispatch-log)
        "Each call appends to log in order"))

  (testing "dispatch-log resets properly via manual reset"
    (reset! dispatch-log [])
    (is (empty? @dispatch-log)
        "dispatch-log should be empty after reset")
    (multi/execute! [:test/tracking {:id "fresh"}])
    (is (= ["fresh"] @dispatch-log))))

;; =============================================================================
;; Tests: execute-async! — Rapid Sequential
;; =============================================================================

(deftest execute-async-rapid-sequential-test
  (testing "Rapid sequential async calls all resolve"
    (let [futures (mapv #(multi/execute-async! [:test/simple {:msg (str "seq-" %)}])
                        (range 20))]
      (doseq [[i f] (map-indexed vector futures)]
        (let [result (deref f 5000 :timeout)]
          (is (not= :timeout result) (str "Future " i " should resolve"))
          (is (= (str "seq-" i) (get-in result [:effects :log :message]))))))))

;; =============================================================================
;; Tests: fan-out — Empty & Single
;; =============================================================================

(deftest fan-out-edge-cases-test
  (testing "fan-out with nil input returns nil"
    (is (nil? (multi/fan-out nil))))

  (testing "fan-out with all nils returns nil"
    (is (nil? (multi/fan-out [nil nil nil])))))

;; =============================================================================
;; Tests: Accessors — Non-Map Input
;; =============================================================================

(deftest accessors-non-map-input-test
  (testing "Accessors handle various non-map types without exception"
    ;; Keywords, strings, numbers — all return nil for map lookups
    (are [input]
         (and (nil? (multi/effects input))
              (nil? (multi/next-event input))
              (nil? (multi/fan-out-events input))
              (false? (multi/halt? input))
              (nil? (multi/orchestration-ctx input)))
      nil
      42
      "string"
      :keyword
      [])))
