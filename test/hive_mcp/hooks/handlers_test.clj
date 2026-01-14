(ns hive-mcp.hooks.handlers-test
  "Tests for built-in hook handlers.

   TDD: Tests written first to define handler contract.
   Handlers are pure functions: (event, context) -> result-map"
  (:require [clojure.test :refer :all]
            [hive-mcp.hooks.handlers :as handlers]))

;; =============================================================================
;; Handler Contract Tests
;; =============================================================================

(deftest handlers-are-functions
  (testing "All exported handlers are functions"
    (is (fn? handlers/shout-completion))
    (is (fn? handlers/commit-if-files-modified))
    (is (fn? handlers/run-wrap))
    (is (fn? handlers/sync-kanban))
    (is (fn? handlers/shout-error))))

;; =============================================================================
;; :task-complete Handlers
;; =============================================================================

(deftest shout-completion-returns-action-map
  (testing "shout-completion returns a map with :action :shout"
    (let [event {:type :task-complete :task-id "task-123"}
          context {:agent-id "agent-1" :project "hive-mcp"}
          result (handlers/shout-completion event context)]
      (is (map? result))
      (is (= :shout (:action result)))
      (is (= :completed (:event-type result)))
      (is (string? (:message result)))
      (is (= "task-123" (get-in result [:data :task-id]))))))

(deftest shout-completion-includes-task-details
  (testing "shout-completion includes task title when available"
    (let [event {:type :task-complete
                 :task-id "task-456"
                 :title "Implement feature X"}
          context {:agent-id "agent-2"}
          result (handlers/shout-completion event context)]
      (is (re-find #"Implement feature X" (:message result))))))

(deftest commit-if-files-modified-returns-nil-when-no-files
  (testing "commit-if-files-modified returns nil when no files modified"
    (let [event {:type :task-complete :task-id "task-123"}
          context {:modified-files []}
          result (handlers/commit-if-files-modified event context)]
      (is (nil? result)))))

(deftest commit-if-files-modified-returns-commit-action
  (testing "commit-if-files-modified returns commit action when files modified"
    (let [event {:type :task-complete
                 :task-id "task-123"
                 :title "Add login feature"}
          context {:modified-files ["src/auth.clj" "test/auth_test.clj"]}
          result (handlers/commit-if-files-modified event context)]
      (is (map? result))
      (is (= :git-commit (:action result)))
      (is (vector? (:files result)))
      (is (= 2 (count (:files result))))
      (is (string? (:message result))))))

(deftest commit-if-files-modified-message-includes-task-title
  (testing "commit message is based on task title"
    (let [event {:type :task-complete
                 :task-id "task-789"
                 :title "Fix authentication bug"}
          context {:modified-files ["src/auth.clj"]}
          result (handlers/commit-if-files-modified event context)]
      (is (re-find #"Fix authentication bug" (:message result))))))

(deftest commit-if-files-modified-handles-missing-title
  (testing "commit-if-files-modified generates message when title missing"
    (let [event {:type :task-complete :task-id "task-abc"}
          context {:modified-files ["src/core.clj"]}
          result (handlers/commit-if-files-modified event context)]
      (is (map? result))
      (is (string? (:message result)))
      (is (not (empty? (:message result)))))))

;; =============================================================================
;; :session-end Handlers
;; =============================================================================

(deftest run-wrap-returns-wrap-action
  (testing "run-wrap returns action to trigger wrap workflow"
    (let [event {:type :session-end}
          context {:session-id "session-123" :project "hive-mcp"}
          result (handlers/run-wrap event context)]
      (is (map? result))
      (is (= :run-workflow (:action result)))
      (is (= :wrap (:workflow result)))
      (is (= "session-123" (get-in result [:params :session-id]))))))

(deftest run-wrap-includes-session-context
  (testing "run-wrap passes session context to wrap workflow"
    (let [event {:type :session-end}
          context {:session-id "sess-456"
                   :project "my-project"
                   :start-time "2026-01-12T10:00:00"}
          result (handlers/run-wrap event context)]
      (is (= "sess-456" (get-in result [:params :session-id])))
      (is (= "my-project" (get-in result [:params :project]))))))

(deftest sync-kanban-returns-sync-action
  (testing "sync-kanban returns action to sync kanban boards"
    (let [event {:type :session-end}
          context {:project "hive-mcp"}
          result (handlers/sync-kanban event context)]
      (is (map? result))
      (is (= :kanban-sync (:action result)))
      (is (= "hive-mcp" (:project result))))))

(deftest sync-kanban-includes-direction
  (testing "sync-kanban specifies bidirectional sync by default"
    (let [event {:type :session-end}
          context {:project "test-proj"}
          result (handlers/sync-kanban event context)]
      (is (= :bidirectional (:direction result))))))

;; =============================================================================
;; :error Handlers
;; =============================================================================

(deftest shout-error-returns-error-shout
  (testing "shout-error returns a shout action with error type"
    (let [event {:type :error-occurred
                 :error "Connection timeout"
                 :task-id "task-err"}
          context {:agent-id "agent-3"}
          result (handlers/shout-error event context)]
      (is (map? result))
      (is (= :shout (:action result)))
      (is (= :error (:event-type result)))
      (is (re-find #"Connection timeout" (:message result))))))

(deftest shout-error-includes-error-details
  (testing "shout-error includes error details in data"
    (let [event {:type :error-occurred
                 :error "File not found"
                 :task-id "task-404"
                 :file "missing.clj"}
          context {:agent-id "agent-4"}
          result (handlers/shout-error event context)]
      (is (= "File not found" (get-in result [:data :error])))
      (is (= "task-404" (get-in result [:data :task-id]))))))

(deftest shout-error-handles-exception-objects
  (testing "shout-error extracts message from Exception objects"
    (let [ex (ex-info "Something went wrong" {:code 500})
          event {:type :error-occurred
                 :error ex
                 :task-id "task-ex"}
          context {:agent-id "agent-5"}
          result (handlers/shout-error event context)]
      (is (re-find #"Something went wrong" (:message result))))))

;; =============================================================================
;; Handler Collection Tests
;; =============================================================================

(deftest builtin-handlers-map-exists
  (testing "builtin-handlers map contains all handlers by event type"
    (is (map? handlers/builtin-handlers))
    (is (contains? handlers/builtin-handlers :task-complete))
    (is (contains? handlers/builtin-handlers :session-end))
    (is (contains? handlers/builtin-handlers :error-occurred))))

(deftest builtin-handlers-are-vectors
  (testing "Each event type maps to a vector of handlers"
    (is (vector? (:task-complete handlers/builtin-handlers)))
    (is (vector? (:session-end handlers/builtin-handlers)))
    (is (vector? (:error-occurred handlers/builtin-handlers)))))

(deftest builtin-handlers-contain-correct-handlers
  (testing ":task-complete contains shout-completion and commit-if-files-modified"
    (let [tc-handlers (:task-complete handlers/builtin-handlers)]
      (is (some #(= % handlers/shout-completion) tc-handlers))
      (is (some #(= % handlers/commit-if-files-modified) tc-handlers))))

  (testing ":session-end contains run-wrap and sync-kanban"
    (let [se-handlers (:session-end handlers/builtin-handlers)]
      (is (some #(= % handlers/run-wrap) se-handlers))
      (is (some #(= % handlers/sync-kanban) se-handlers))))

  (testing ":error-occurred contains shout-error"
    (let [err-handlers (:error-occurred handlers/builtin-handlers)]
      (is (some #(= % handlers/shout-error) err-handlers)))))

;; =============================================================================
;; Registration Helper Tests
;; =============================================================================

(deftest register-builtins!-applies-handlers
  (testing "register-builtins! registers all built-in handlers to registry"
    (let [registry (atom {})]
      ;; Mock the register-hook function behavior
      (handlers/register-builtins! registry
                                   (fn [reg event handler]
                                     (swap! reg update event (fnil conj []) handler)))
      ;; Verify handlers were registered
      (is (>= (count (:task-complete @registry)) 2))
      (is (>= (count (:session-end @registry)) 2))
      (is (>= (count (:error-occurred @registry)) 1)))))
