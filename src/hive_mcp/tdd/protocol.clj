(ns hive-mcp.tdd.protocol
  "TDD Loop Participant Protocol - Autonomous drone orchestration.

   Implements ADR 20260126163927-77493f81: Ling TDD Loop.

   Architecture (SOLID):
   - ITDDParticipant protocol: Open for extension (O)
   - ImplDrone: Implementation drone - writes feature code
   - TestDrone: Test runner drone - executes tests, parses failures
   - FixDrone: Fix drone - fixes failing assertions

   Key Axiom: 'Drones get ONE shot - no internal retry loop.'
   The TDD loop handles retries EXTERNALLY via ling orchestration.

   SOLID-O: Open for extension via ITDDParticipant protocol
   SOLID-S: SRP - Participant contract only
   CLARITY-L: Layer separation from loop.clj orchestration"
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; ITDDParticipant Protocol (SOLID-O: Open for Extension)
;; =============================================================================

(defprotocol ITDDParticipant
  "Protocol for TDD loop participants. Implement this to add new participant types.

   Participants execute atomic tasks in the TDD loop. Each participant
   gets ONE shot at their task - retries are handled by the loop orchestration.

   Examples:
   - ImplDrone: Implements feature code from spec
   - TestDrone: Runs tests and parses failures
   - FixDrone: Fixes failing assertions
   - MockParticipant: For testing"

  (participant-id [this]
    "Return unique identifier for this participant.")

  (participant-type [this]
    "Return participant type keyword (:impl-drone, :test-drone, :fix-drone).")

  (execute-task! [this task]
    "Execute a single task atomically. NO internal retries.

     Arguments:
       task - Map with task context:
              :prompt      - Task description
              :files       - Target files
              :context     - Additional context (test output, errors, etc.)

     Returns:
       {:status  :completed|:failed
        :result  string     ; Output/result
        :error   string}    ; Error message if failed

     AXIOM: Drones get ONE shot. Tasks must be atomic.")

  (get-preset [this]
    "Return the preset name this participant uses for delegation."))

;; =============================================================================
;; Participant Type Constants
;; =============================================================================

(def participant-types
  "Valid participant types and their roles."
  {:impl-drone  {:role "Implements feature code"
                 :default-preset "drone-worker"}
   :test-drone  {:role "Runs tests and parses failures"
                 :default-preset "tester"}
   :fix-drone   {:role "Fixes failing assertions"
                 :default-preset "fixer"}})

;; =============================================================================
;; ImplDrone Record - Implementation Participant
;; =============================================================================

(defrecord ImplDrone [id preset model]
  ITDDParticipant

  (participant-id [_] id)

  (participant-type [_] :impl-drone)

  (execute-task! [_ task]
    (require 'hive-mcp.agent.core)
    (let [delegate-fn (resolve 'hive-mcp.agent.core/delegate-drone!)]
      (try
        (let [result (delegate-fn {:task (:prompt task)
                                   :preset (or preset "drone-worker")
                                   :files (:files task)
                                   :trace false})]
          (if (= (:status result) :completed)
            {:status :completed :result (:result result)}
            {:status :failed :error (or (:message result) "Implementation failed")}))
        (catch Exception e
          (log/error e "ImplDrone task execution failed")
          {:status :failed :error (str "Execution error: " (.getMessage e))}))))

  (get-preset [_] (or preset "drone-worker")))

(defn create-impl-drone
  "Factory function for creating ImplDrone.

   Arguments:
   - opts: Optional {:model 'model-id' :id 'custom-id' :preset 'preset-name'}

   Returns: ImplDrone record"
  [& [{:keys [model id preset]}]]
  (->ImplDrone
   (or id (str "impl-drone-" (System/currentTimeMillis) "-" (rand-int 1000)))
   (or preset "drone-worker")
   model))

;; =============================================================================
;; TestDrone Record - Test Runner Participant
;; =============================================================================

(defrecord TestDrone [id preset model]
  ITDDParticipant

  (participant-id [_] id)

  (participant-type [_] :test-drone)

  (execute-task! [_ task]
    ;; TestDrone executes test commands and parses output
    (require 'hive-mcp.agent.core)
    (let [delegate-fn (resolve 'hive-mcp.agent.core/delegate-drone!)]
      (try
        (let [result (delegate-fn {:task (str "Run tests and report results.\n\n"
                                              "Test command: " (:prompt task) "\n\n"
                                              "Output a JSON object with:\n"
                                              "- status: 'pass' or 'fail'\n"
                                              "- failures: array of {file, line, message}\n"
                                              "- summary: brief summary string")
                                   :preset (or preset "tester")
                                   :files (:files task)
                                   :trace false})]
          (if (= (:status result) :completed)
            {:status :completed :result (:result result)}
            {:status :failed :error (or (:message result) "Test execution failed")}))
        (catch Exception e
          (log/error e "TestDrone task execution failed")
          {:status :failed :error (str "Test execution error: " (.getMessage e))}))))

  (get-preset [_] (or preset "tester")))

(defn create-test-drone
  "Factory function for creating TestDrone.

   Arguments:
   - opts: Optional {:model 'model-id' :id 'custom-id' :preset 'preset-name'}

   Returns: TestDrone record"
  [& [{:keys [model id preset]}]]
  (->TestDrone
   (or id (str "test-drone-" (System/currentTimeMillis) "-" (rand-int 1000)))
   (or preset "tester")
   model))

;; =============================================================================
;; FixDrone Record - Fix Implementation Participant
;; =============================================================================

(defrecord FixDrone [id preset model]
  ITDDParticipant

  (participant-id [_] id)

  (participant-type [_] :fix-drone)

  (execute-task! [_ task]
    (require 'hive-mcp.agent.core)
    (let [delegate-fn (resolve 'hive-mcp.agent.core/delegate-drone!)]
      (try
        (let [;; Include test failure context in fix prompt
              fix-prompt (str "Fix the following test failures:\n\n"
                              (:context task)
                              "\n\nOriginal task: " (:prompt task))
              result (delegate-fn {:task fix-prompt
                                   :preset (or preset "fixer")
                                   :files (:files task)
                                   :trace false})]
          (if (= (:status result) :completed)
            {:status :completed :result (:result result)}
            {:status :failed :error (or (:message result) "Fix failed")}))
        (catch Exception e
          (log/error e "FixDrone task execution failed")
          {:status :failed :error (str "Fix execution error: " (.getMessage e))}))))

  (get-preset [_] (or preset "fixer")))

(defn create-fix-drone
  "Factory function for creating FixDrone.

   Arguments:
   - opts: Optional {:model 'model-id' :id 'custom-id' :preset 'preset-name'}

   Returns: FixDrone record"
  [& [{:keys [model id preset]}]]
  (->FixDrone
   (or id (str "fix-drone-" (System/currentTimeMillis) "-" (rand-int 1000)))
   (or preset "fixer")
   model))

;; =============================================================================
;; MockParticipant for Testing
;; =============================================================================

(defrecord MockParticipant [id participant-type-kw preset responses response-idx]
  ITDDParticipant

  (participant-id [_] id)

  (participant-type [_] participant-type-kw)

  (execute-task! [_ _task]
    (let [idx @response-idx
          response (get responses idx {:status :completed :result "Mock result"})]
      (swap! response-idx inc)
      response))

  (get-preset [_] preset))

(defn create-mock-participant
  "Factory for MockParticipant (testing).

   Arguments:
   - ptype: Participant type (:impl-drone, :test-drone, :fix-drone)
   - responses: Vector of response maps [{:status :completed|:failed :result|:error}]

   Returns: MockParticipant record"
  [ptype responses]
  (->MockParticipant
   (str "mock-" (name ptype) "-" (rand-int 10000))
   ptype
   "mock-preset"
   (vec responses)
   (atom 0)))

;; =============================================================================
;; Validation Helpers
;; =============================================================================

(defn valid-participant?
  "Check if x satisfies ITDDParticipant protocol."
  [x]
  (satisfies? ITDDParticipant x))

(defn participant-summary
  "Get a summary map of a participant for logging/debugging."
  [participant]
  (when (valid-participant? participant)
    {:id (participant-id participant)
     :type (participant-type participant)
     :preset (get-preset participant)}))
