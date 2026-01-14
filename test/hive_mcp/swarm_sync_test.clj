(ns hive-mcp.swarm-sync-test
  "Tests for ADR-001 Phase 2: Event-driven sync between elisp and Clojure registries.

   These tests verify that channel events properly synchronize the lings-registry
   (now backed by DataScript - ADR-002) without requiring manual registration."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.channel :as ch]
            [clojure.core.async :as async :refer [<!! timeout alts!!]]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn with-clean-state [f]
  "Ensure clean registry state before and after each test.
   ADR-002: Reset both DataScript and deprecated atom."
  ;; Reset DataScript (primary - ADR-002)
  (ds/reset-conn!)
  ;; Reset deprecated atom
  (reset! swarm/lings-registry {})
  (ch/stop-server!)
  (Thread/sleep 50)
  (f)
  (ch/stop-server!)
  ;; Cleanup
  (ds/reset-conn!)
  (reset! swarm/lings-registry {})
  (Thread/sleep 50))

(use-fixtures :each with-clean-state)

;; =============================================================================
;; Unit Tests: Registry Functions
;; =============================================================================

(deftest register-ling-test
  (testing "register-ling! adds ling to registry with all fields"
    (swarm/register-ling! "test-slave-123"
                          {:name "worker1"
                           :presets ["tdd" "clarity"]
                           :cwd "/home/user/project"})
    (let [lings @swarm/lings-registry
          ling (get lings "test-slave-123")]
      (is (= 1 (count lings)))
      (is (some? ling))
      (is (= "worker1" (:name ling)))
      (is (= ["tdd" "clarity"] (:presets ling)))
      (is (= "/home/user/project" (:cwd ling)))
      (is (number? (:spawned-at ling))))))

(deftest unregister-ling-test
  (testing "unregister-ling! removes ling from registry"
    ;; Setup: add a ling
    (swarm/register-ling! "test-slave-456" {:name "temp" :presets [] :cwd "/"})
    (is (= 1 (count @swarm/lings-registry)))

    ;; Act: unregister
    (swarm/unregister-ling! "test-slave-456")

    ;; Assert: removed
    (is (= 0 (count @swarm/lings-registry)))
    (is (nil? (get @swarm/lings-registry "test-slave-456")))))

(deftest unregister-nonexistent-ling-test
  (testing "unregister-ling! handles non-existent ling gracefully"
    (is (= 0 (count @swarm/lings-registry)))
    ;; Should not throw
    (swarm/unregister-ling! "nonexistent-slave")
    (is (= 0 (count @swarm/lings-registry)))))

;; =============================================================================
;; Event Handler Tests (Mocked Channel)
;; =============================================================================

(deftest handle-ling-registered-event-test
  (testing "slave-spawned event triggers register-ling!"
    ;; Start channel server
    (ch/start-server! {:type :unix :path "/tmp/hive-mcp-sync-test1.sock"})
    (Thread/sleep 100)

    ;; Start registry sync subscriptions
    (swarm/start-registry-sync!)
    (Thread/sleep 100)

    ;; Emit the registration event (simulating elisp -> clojure)
    (ch/emit-event! :slave-spawned
                    {:slave-id "spawned-via-event-1"
                     :name "test-worker"
                     :presets ["hivemind"]
                     :cwd "/tmp/test-project"})

    ;; Wait for event processing
    (Thread/sleep 200)

    ;; Verify ling was registered
    (let [ling (get @swarm/lings-registry "spawned-via-event-1")]
      (is (some? ling) "Ling should be registered via event")
      (is (= "test-worker" (:name ling)))
      (is (= ["hivemind"] (:presets ling)))
      (is (= "/tmp/test-project" (:cwd ling))))

    ;; Cleanup
    (swarm/stop-registry-sync!)))

(deftest handle-ling-unregistered-event-test
  (testing "slave-killed event triggers unregister-ling!"
    ;; Setup: pre-register a ling
    (swarm/register-ling! "to-be-killed-1"
                          {:name "doomed" :presets [] :cwd "/"})
    (is (some? (get @swarm/lings-registry "to-be-killed-1")))

    ;; Start channel server
    (ch/start-server! {:type :unix :path "/tmp/hive-mcp-sync-test2.sock"})
    (Thread/sleep 100)

    ;; Start registry sync subscriptions
    (swarm/start-registry-sync!)
    (Thread/sleep 100)

    ;; Emit the unregistration event (simulating elisp -> clojure)
    (ch/emit-event! :slave-killed
                    {:slave-id "to-be-killed-1"})

    ;; Wait for event processing
    (Thread/sleep 200)

    ;; Verify ling was unregistered
    (is (nil? (get @swarm/lings-registry "to-be-killed-1"))
        "Ling should be unregistered via event")

    ;; Cleanup
    (swarm/stop-registry-sync!)))

;; =============================================================================
;; Integration Tests: Full Event Flow
;; =============================================================================

(deftest spawn-kill-sync-lifecycle-test
  (testing "Full spawn/kill lifecycle syncs via events"
    ;; Start channel infrastructure
    (ch/start-server! {:type :unix :path "/tmp/hive-mcp-sync-test3.sock"})
    (Thread/sleep 100)
    (swarm/start-registry-sync!)
    (Thread/sleep 100)

    ;; Simulate spawn event from elisp
    (ch/emit-event! :slave-spawned
                    {:slave-id "lifecycle-test-1"
                     :name "lifecycle-worker"
                     :presets ["tdd"]
                     :cwd "/tmp/lifecycle"})
    (Thread/sleep 200)

    ;; Verify registered
    (is (some? (get @swarm/lings-registry "lifecycle-test-1")))
    (is (= 1 (count @swarm/lings-registry)))

    ;; Simulate another spawn
    (ch/emit-event! :slave-spawned
                    {:slave-id "lifecycle-test-2"
                     :name "another-worker"
                     :presets []
                     :cwd "/tmp/lifecycle2"})
    (Thread/sleep 200)

    ;; Verify both registered
    (is (= 2 (count @swarm/lings-registry)))

    ;; Simulate kill event for first ling
    (ch/emit-event! :slave-killed
                    {:slave-id "lifecycle-test-1"})
    (Thread/sleep 200)

    ;; Verify first unregistered, second still there
    (is (nil? (get @swarm/lings-registry "lifecycle-test-1")))
    (is (some? (get @swarm/lings-registry "lifecycle-test-2")))
    (is (= 1 (count @swarm/lings-registry)))

    ;; Cleanup
    (swarm/stop-registry-sync!)))

(deftest event-with-missing-fields-test
  (testing "Events with missing optional fields handled gracefully"
    ;; Start channel infrastructure
    (ch/start-server! {:type :unix :path "/tmp/hive-mcp-sync-test4.sock"})
    (Thread/sleep 100)
    (swarm/start-registry-sync!)
    (Thread/sleep 100)

    ;; Emit event with minimal fields (only slave-id is truly required)
    (ch/emit-event! :slave-spawned
                    {:slave-id "minimal-ling"
                     :name "minimal"})
    ;; Note: presets and cwd may be nil
    (Thread/sleep 200)

    ;; Should still register with what we have
    (let [ling (get @swarm/lings-registry "minimal-ling")]
      (is (some? ling))
      (is (= "minimal" (:name ling))))

    ;; Cleanup
    (swarm/stop-registry-sync!)))

;; =============================================================================
;; CLARITY: Safe Failure Tests
;; =============================================================================

(deftest sync-handles-channel-down-gracefully-test
  (testing "start-registry-sync! handles channel not available"
    ;; Don't start channel server - it should handle gracefully
    ;; This should not throw
    (swarm/start-registry-sync!)
    ;; Should be ok to call stop even if start didn't fully succeed
    (swarm/stop-registry-sync!)))

(comment
  ;; Run tests
  (clojure.test/run-tests 'hive-mcp.swarm-sync-test)

  ;; Run single test
  (clojure.test/test-vars [#'handle-ling-registered-event-test]))
