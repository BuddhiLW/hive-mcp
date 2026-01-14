(ns hive-mcp.specs.hooks-test
  "Tests for hooks domain specs - TDD approach.

   Tests validate:
   - Hook event enums
   - Hook context and payload structures
   - Hook registry entries
   - Helper validation functions
   - Generators for property-based testing"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hive-mcp.specs.hooks :as hooks]))

;; ============================================================
;; Hook Event Tests
;; ============================================================

(deftest test-hook-event-spec
  (testing "Valid hook events"
    (is (s/valid? ::hooks/hook-event :task-start))
    (is (s/valid? ::hooks/hook-event :task-complete))
    (is (s/valid? ::hooks/hook-event :session-start))
    (is (s/valid? ::hooks/hook-event :session-end))
    (is (s/valid? ::hooks/hook-event :file-modified))
    (is (s/valid? ::hooks/hook-event :error))
    (is (s/valid? ::hooks/hook-event :ling-spawn))
    (is (s/valid? ::hooks/hook-event :ling-terminate)))

  (testing "Invalid hook events"
    (is (not (s/valid? ::hooks/hook-event :invalid-event)))
    (is (not (s/valid? ::hooks/hook-event "task-start")))
    (is (not (s/valid? ::hooks/hook-event nil)))))

(deftest test-hook-event-string-spec
  (testing "Valid hook event strings"
    (is (s/valid? ::hooks/hook-event-string "task-start"))
    (is (s/valid? ::hooks/hook-event-string "task-complete"))
    (is (s/valid? ::hooks/hook-event-string "ling-spawn")))

  (testing "Invalid hook event strings"
    (is (not (s/valid? ::hooks/hook-event-string "invalid")))
    (is (not (s/valid? ::hooks/hook-event-string :task-start)))))

(deftest test-hook-event-any-spec
  (testing "Hook event accepts both keywords and strings"
    (is (s/valid? ::hooks/hook-event-any :task-start))
    (is (s/valid? ::hooks/hook-event-any "task-start"))
    (is (s/valid? ::hooks/hook-event-any :ling-terminate))
    (is (s/valid? ::hooks/hook-event-any "ling-terminate")))

  (testing "Invalid hook-event-any"
    (is (not (s/valid? ::hooks/hook-event-any :invalid)))
    (is (not (s/valid? ::hooks/hook-event-any "invalid")))))

;; ============================================================
;; Hook Context Tests
;; ============================================================

(deftest test-hook-context-spec
  (testing "Minimal valid context (all optional)"
    (is (s/valid? ::hooks/hook-context {})))

  (testing "Context with event"
    (is (s/valid? ::hooks/hook-context {:event :task-start}))
    (is (s/valid? ::hooks/hook-context {:event "task-complete"})))

  (testing "Context with slave-id"
    (is (s/valid? ::hooks/hook-context {:slave-id "agent-1"})))

  (testing "Context with task"
    (is (s/valid? ::hooks/hook-context {:task "Implement feature X"})))

  (testing "Context with files"
    (is (s/valid? ::hooks/hook-context {:files ["file1.clj" "file2.clj"]})))

  (testing "Context with error"
    (is (s/valid? ::hooks/hook-context {:error "Something went wrong"}))
    (is (s/valid? ::hooks/hook-context {:error {:type :validation :msg "bad input"}}))
    (is (s/valid? ::hooks/hook-context {:error (ex-info "test" {})}))
    (is (s/valid? ::hooks/hook-context {:error nil})))

  (testing "Full context"
    (is (s/valid? ::hooks/hook-context
                  {:event :task-complete
                   :slave-id "agent-1"
                   :task "Implement specs"
                   :files ["specs.clj"]
                   :data {:duration 1234}
                   :message "Task completed successfully"})))

  (testing "Invalid context - bad event"
    (is (not (s/valid? ::hooks/hook-context {:event :invalid-event}))))

  (testing "Invalid context - empty slave-id"
    (is (not (s/valid? ::hooks/hook-context {:slave-id ""})))))

;; ============================================================
;; Hook Payload Tests
;; ============================================================

(deftest test-hook-payload-spec
  (testing "Minimal valid payload"
    (is (s/valid? ::hooks/hook-payload {:hook-type :kanban-done})))

  (testing "Payload with files"
    (is (s/valid? ::hooks/hook-payload {:hook-type :file-modified
                                        :files ["a.clj" "b.clj"]})))

  (testing "Payload with message"
    (is (s/valid? ::hooks/hook-payload {:hook-type :task-completed
                                        :message "All tests passed"})))

  (testing "Payload with data"
    (is (s/valid? ::hooks/hook-payload {:hook-type :session-wrap
                                        :data {:commits 5 :tasks 3}})))

  (testing "Full payload"
    (is (s/valid? ::hooks/hook-payload
                  {:hook-type :kanban-done
                   :files ["task.org"]
                   :message "Task moved to done"
                   :data {:task-id "123" :priority "high"}})))

  (testing "Invalid payload - missing hook-type"
    (is (not (s/valid? ::hooks/hook-payload {}))))

  (testing "Invalid payload - hook-type must be keyword"
    (is (not (s/valid? ::hooks/hook-payload {:hook-type "kanban-done"})))))

;; ============================================================
;; Hook Entry Tests
;; ============================================================

(deftest test-hook-entry-spec
  (testing "Minimal valid entry"
    (is (s/valid? ::hooks/hook-entry {:hook-event :task-start
                                      :hook-fn identity})))

  (testing "Entry with priority"
    (is (s/valid? ::hooks/hook-entry {:hook-event :task-complete
                                      :hook-fn println
                                      :priority 50})))

  (testing "Entry with enabled flag"
    (is (s/valid? ::hooks/hook-entry {:hook-event :error
                                      :hook-fn (fn [_] nil)
                                      :enabled true})))

  (testing "Full entry"
    (is (s/valid? ::hooks/hook-entry {:hook-event :ling-spawn
                                      :hook-fn identity
                                      :priority 10
                                      :enabled false})))

  (testing "Invalid entry - missing hook-event"
    (is (not (s/valid? ::hooks/hook-entry {:hook-fn identity}))))

  (testing "Invalid entry - missing hook-fn"
    (is (not (s/valid? ::hooks/hook-entry {:hook-event :task-start}))))

  (testing "Invalid entry - priority out of range"
    (is (not (s/valid? ::hooks/hook-entry {:hook-event :task-start
                                           :hook-fn identity
                                           :priority -1})))
    (is (not (s/valid? ::hooks/hook-entry {:hook-event :task-start
                                           :hook-fn identity
                                           :priority 101}))))

  (testing "Invalid entry - hook-fn must be invocable"
    (is (not (s/valid? ::hooks/hook-entry {:hook-event :task-start
                                           :hook-fn "not a function"})))))

;; ============================================================
;; Hook Registry Tests
;; ============================================================

(deftest test-hook-registry-spec
  (testing "Empty registry"
    (is (s/valid? ::hooks/hook-registry {})))

  (testing "Registry with single entry"
    (is (s/valid? ::hooks/hook-registry
                  {"hook-1" {:hook-event :task-start
                             :hook-fn identity}})))

  (testing "Registry with multiple entries"
    (is (s/valid? ::hooks/hook-registry
                  {"hook-1" {:hook-event :task-start :hook-fn identity}
                   "hook-2" {:hook-event :task-complete :hook-fn println}
                   "hook-3" {:hook-event :error :hook-fn (fn [_] nil)}})))

  (testing "Invalid registry - empty key"
    (is (not (s/valid? ::hooks/hook-registry
                       {"" {:hook-event :task-start :hook-fn identity}})))))

;; ============================================================
;; Helper Function Tests
;; ============================================================

(deftest test-valid-hook-event?
  (testing "Valid events return true"
    (is (true? (hooks/valid-hook-event? :task-start)))
    (is (true? (hooks/valid-hook-event? :ling-terminate))))

  (testing "Invalid events return false"
    (is (false? (hooks/valid-hook-event? :invalid)))
    (is (false? (hooks/valid-hook-event? "task-start")))
    (is (false? (hooks/valid-hook-event? nil)))))

(deftest test-valid-hook-payload?
  (testing "Valid payloads return true"
    (is (true? (hooks/valid-hook-payload? {:hook-type :kanban-done})))
    (is (true? (hooks/valid-hook-payload? {:hook-type :test :message "hi"}))))

  (testing "Invalid payloads return false"
    (is (false? (hooks/valid-hook-payload? {})))
    (is (false? (hooks/valid-hook-payload? {:hook-type "string"})))))

(deftest test-explain-hook-payload
  (testing "Valid payload returns nil"
    (is (nil? (hooks/explain-hook-payload {:hook-type :test}))))

  (testing "Invalid payload returns explain-data"
    (let [result (hooks/explain-hook-payload {})]
      (is (some? result))
      (is (contains? result ::s/problems)))))

(deftest test-valid-hook-context?
  (testing "Valid contexts return true"
    (is (true? (hooks/valid-hook-context? {})))
    (is (true? (hooks/valid-hook-context? {:event :task-start})))
    (is (true? (hooks/valid-hook-context? {:slave-id "agent-1" :task "test"}))))

  (testing "Invalid contexts return false"
    (is (false? (hooks/valid-hook-context? {:event :invalid})))))

(deftest test-explain-hook-context
  (testing "Valid context returns nil"
    (is (nil? (hooks/explain-hook-context {}))))

  (testing "Invalid context returns explain-data"
    (let [result (hooks/explain-hook-context {:event :invalid})]
      (is (some? result))
      (is (contains? result ::s/problems)))))

(deftest test-valid-hook-entry?
  (testing "Valid entries return true"
    (is (true? (hooks/valid-hook-entry? {:hook-event :task-start :hook-fn identity}))))

  (testing "Invalid entries return false"
    (is (false? (hooks/valid-hook-entry? {})))
    (is (false? (hooks/valid-hook-entry? {:hook-event :task-start})))))

(deftest test-explain-hook-entry
  (testing "Valid entry returns nil"
    (is (nil? (hooks/explain-hook-entry {:hook-event :task-start :hook-fn identity}))))

  (testing "Invalid entry returns explain-data"
    (let [result (hooks/explain-hook-entry {})]
      (is (some? result))
      (is (contains? result ::s/problems)))))

;; ============================================================
;; Generator Tests
;; ============================================================

(deftest test-hook-event-gen
  (testing "Generator produces valid hook events"
    (let [events (gen/sample (hooks/hook-event-gen) 20)]
      (is (every? #(s/valid? ::hooks/hook-event %) events)))))

(deftest test-hook-type-gen
  (testing "Generator produces valid hook types"
    (let [types (gen/sample (hooks/hook-type-gen) 20)]
      (is (every? keyword? types)))))

(deftest test-slave-id-gen
  (testing "Generator produces valid slave IDs"
    (let [ids (gen/sample (hooks/slave-id-gen) 20)]
      (is (every? string? ids))
      (is (every? #(clojure.string/starts-with? % "slave-") ids)))))

(deftest test-hook-context-gen
  (testing "Generator produces valid hook contexts"
    (let [contexts (gen/sample (hooks/hook-context-gen) 10)]
      (is (every? #(s/valid? ::hooks/hook-context %) contexts)))))

(deftest test-hook-payload-gen
  (testing "Generator produces valid hook payloads"
    (let [payloads (gen/sample (hooks/hook-payload-gen) 10)]
      (is (every? #(s/valid? ::hooks/hook-payload %) payloads)))))

(deftest test-hook-entry-gen
  (testing "Generator produces valid hook entries"
    (let [entries (gen/sample (hooks/hook-entry-gen) 10)]
      (is (every? #(s/valid? ::hooks/hook-entry %) entries)))))
