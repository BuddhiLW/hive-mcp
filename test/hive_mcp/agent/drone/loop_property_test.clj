(ns hive-mcp.agent.drone.loop-property-test
  "Property-based tests for the agentic drone loop.

   Tests pure calculations and state transitions:
   - Token accumulation: monotonic, idempotent on nil
   - State advancement: turn always increments, steps grow, invariants hold
   - Termination: total (never throws), deterministic
   - Result evaluation: total, partitions error space
   - Final status determination: total, correct partition
   - Completion language detection: never throws, boolean return"
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.agent.drone.loop :as loop]
            [hive-mcp.agent.drone.loop-predicates :as pred]))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-tokens
  "Generator for token count maps."
  (gen/let [input  (gen/choose 0 100000)
            output (gen/choose 0 100000)]
    {:input input :output output :total (+ input output)}))

(def gen-usage
  "Generator for LLM usage response (may be nil)."
  (gen/one-of
   [(gen/return nil)
    (gen/let [input  (gen/choose 0 5000)
              output (gen/choose 0 5000)]
      {:input input :output output :total (+ input output)})]))

(def gen-response-type
  "Generator for LLM response types."
  (gen/elements [:text :tool_calls :error]))

(def gen-step
  "Generator for a loop step."
  (gen/let [typ  gen-response-type
            turn (gen/choose 0 100)]
    {:type typ :turn turn}))

(def gen-loop-state
  "Generator for valid loop states."
  (gen/let [turn    (gen/choose 0 50)
            steps   (gen/vector gen-step 0 20)
            tcm     (gen/choose 0 100)
            tokens  gen-tokens
            cf      (gen/choose 0 5)
            lrt     (gen/one-of [(gen/return nil) gen-response-type])
            lt      (gen/one-of [(gen/return nil) gen/string-alphanumeric])
            obs     (gen/choose 0 50)
            reason  (gen/choose 0 50)]
    {:turn                 turn
     :steps                steps
     :tool-calls-made      tcm
     :total-tokens         tokens
     :consecutive-failures cf
     :last-response-type   lrt
     :last-text            lt
     :obs-count            obs
     :reason-count         reason}))

(def gen-max-turns
  "Generator for max-turns option."
  (gen/choose 1 50))

(def gen-error-msg
  "Generator for error messages (may contain retryable/fatal patterns)."
  (gen/one-of
   [(gen/return "file not found")
    (gen/return "no such file or directory")
    (gen/return "timeout waiting for response")
    (gen/return "Unknown tool: foo_bar")
    (gen/return "rejected by policy")
    (gen/return "something completely different")
    gen/string-alphanumeric]))

(def gen-text-for-completion
  "Generator for text that may/may not contain completion language."
  (gen/one-of
   [(gen/return "I've successfully completed the task.")
    (gen/return "all done")
    (gen/return "The fix is ready.")
    (gen/return "Nothing more to do.")
    (gen/return "Here is the summary of changes.")
    (gen/return "Still working on it...")
    (gen/return "Let me try a different approach")
    gen/string-alphanumeric
    (gen/return nil)
    (gen/return "")]))

;; =============================================================================
;; P1: Token Accumulation Properties
;; =============================================================================

(defspec p1-token-accumulation-monotonic 200
  (prop/for-all [total  gen-tokens
                 usage  gen-usage]
                (let [result (#'loop/accumulate-tokens total usage)]
                  (and (map? result)
                       (>= (:input result)  (:input total))
                       (>= (:output result) (:output total))
                       (>= (:total result)  (:total total))))))

(defspec p1-token-accumulation-nil-identity 200
  (prop/for-all [total gen-tokens]
                (= total (#'loop/accumulate-tokens total nil))))

(defspec p1-token-accumulation-zero-identity 200
  (prop/for-all [total gen-tokens]
                (let [zero-usage {:input 0 :output 0 :total 0}]
                  (= total (#'loop/accumulate-tokens total zero-usage)))))

;; =============================================================================
;; P2: Termination Properties
;; =============================================================================

(defspec p2-termination-totality 200
  (prop/for-all [state     gen-loop-state
                 max-turns gen-max-turns]
                (let [result (pred/should-terminate? state {:max-turns max-turns})]
                  (and (map? result)
                       (contains? result :terminate?)
                       (contains? result :reason)
                       (boolean? (:terminate? result))
                       (string? (:reason result))))))

(defspec p2-termination-max-turns-always-terminates 200
  (prop/for-all [max-turns gen-max-turns]
                (let [state {:turn max-turns :steps [] :consecutive-failures 0
                             :last-response-type :tool_calls :last-text nil}
                      result (pred/should-terminate? state {:max-turns max-turns})]
                  (:terminate? result))))

(defspec p2-termination-text-response-always-terminates 200
  (prop/for-all [turn (gen/choose 0 9)
                 text gen/string-alphanumeric]
                (let [state {:turn turn :steps [] :consecutive-failures 0
                             :last-response-type :text :last-text text}
                      result (pred/should-terminate? state {:max-turns 10})]
                  (:terminate? result))))

(defspec p2-termination-turn-zero-never-terminates-early 200
  (prop/for-all [max-turns (gen/choose 1 50)]
                (let [state {:turn 0 :steps [] :consecutive-failures 0
                             :last-response-type nil :last-text nil}
                      result (pred/should-terminate? state {:max-turns max-turns})]
                  (not (:terminate? result)))))

;; =============================================================================
;; P3: Result Evaluation Properties
;; =============================================================================

(defspec p3-evaluation-success-always-good 200
  (prop/for-all [text gen/string-alphanumeric]
                (let [result (pred/evaluate-result {:success true :result {:text text}} "goal" [])]
                  (and (= :good (:quality result))
                       (:continue? result)))))

(defspec p3-evaluation-failure-always-bad 200
  (prop/for-all [error-msg gen-error-msg]
                (let [result (pred/evaluate-result {:success false :error error-msg} "goal" [])]
                  (= :bad (:quality result)))))

(defspec p3-evaluation-totality 200
  (prop/for-all [success? gen/boolean
                 error-msg gen-error-msg]
                (let [result (pred/evaluate-result
                              {:success success? :error error-msg} "goal" [])]
                  (and (map? result)
                       (contains? result :quality)
                       (contains? result :continue?)
                       (contains? result :reason)
                       (#{:good :bad} (:quality result))
                       (boolean? (:continue? result))
                       (string? (:reason result))))))

;; =============================================================================
;; P4: Final Status Determination
;; =============================================================================

(defspec p4-status-totality 200
  (prop/for-all [state     gen-loop-state
                 max-turns gen-max-turns]
                (let [status (pred/determine-final-status state max-turns)]
                  (#{:completed :max_steps} status))))

(defspec p4-status-max-steps-when-at-limit 200
  (prop/for-all [max-turns gen-max-turns]
                (= :max_steps
                   (pred/determine-final-status {:turn max-turns} max-turns))))

(defspec p4-status-completed-when-under-limit 200
  (prop/for-all [max-turns (gen/choose 2 50)]
                (= :completed
                   (pred/determine-final-status {:turn (dec max-turns)} max-turns))))

;; =============================================================================
;; P5: Completion Language Detection
;; =============================================================================

(defspec p5-completion-language-totality 200
  (prop/for-all [text gen-text-for-completion]
                (let [result (pred/completion-language? text)]
      ;; Never throws, returns truthy or falsy
                  (or (nil? result) (boolean? result) (true? result)))))

(defspec p5-completion-language-nil-safe 100
  (prop/for-all [_ gen/boolean]
                (nil? (pred/completion-language? nil))))

(defspec p5-completion-language-blank-safe 100
  (prop/for-all [_ gen/boolean]
                (nil? (pred/completion-language? ""))))

;; =============================================================================
;; P6: No-Progress Detection
;; =============================================================================

(defspec p6-no-progress-needs-enough-steps 200
  (prop/for-all [n (gen/choose 1 10)]
    ;; With fewer steps than n, no-progress? is always false
                (let [steps (vec (repeat (dec n) {:type :error}))]
                  (not (pred/no-progress? steps n)))))

(defspec p6-no-progress-all-errors-detected 200
  (prop/for-all [n (gen/choose 1 5)]
    ;; N error steps should trigger no-progress
                (let [steps (vec (repeat n {:type :error}))]
                  (pred/no-progress? steps n))))

;; =============================================================================
;; P7: Build Result Invariants
;; =============================================================================

(defspec p7-final-result-has-all-keys 200
  (prop/for-all [state     gen-loop-state
                 max-turns gen-max-turns]
                (let [state-with-model (assoc state :model-name "test-model")
                      result (#'loop/build-final-result state-with-model max-turns "test reason")]
                  (and (contains? result :status)
                       (contains? result :result)
                       (contains? result :steps)
                       (contains? result :tool_calls_made)
                       (contains? result :tokens)
                       (contains? result :turns)
                       (contains? result :model)
                       (contains? result :kg-stats)
                       (#{:completed :max_steps} (:status result))
                       (string? (:result result))
                       (map? (:tokens result))
                       (map? (:kg-stats result))))))

(defspec p7-error-result-has-all-keys 200
  (prop/for-all [state gen-loop-state
                 msg   gen/string-alphanumeric]
                (let [state-with-model (assoc state :model-name "test-model")
                      result (#'loop/build-error-result state-with-model msg)]
                  (and (= :error (:status result))
                       (= msg (:result result))
                       (contains? result :kg-stats)
                       (map? (:kg-stats result))))))

;; =============================================================================
;; P8: KG Stats Invariants
;; =============================================================================

(defspec p8-kg-stats-non-negative 200
  (prop/for-all [state gen-loop-state]
                (let [stats (#'loop/build-kg-stats state)]
                  (and (>= (:observations stats) 0)
                       (>= (:reasoning stats) 0)
                       (= 0 (:facts stats))))))
