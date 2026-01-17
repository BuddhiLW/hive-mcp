(ns hive-mcp.session-complete-test
  "Unit tests for session_complete MCP tool.

   Tests the session completion workflow that:
   1. Commits git changes
   2. Moves kanban tasks to done
   3. Runs wrap/crystallize
   4. Shouts completion to hivemind

   TDD: Tests written first, implementation follows.

   CLARITY Framework:
   - C: Composition - builds on existing git, kanban, crystal infrastructure
   - L: Layers pure - handler orchestrates, effects execute
   - I: Inputs guarded - validates commit_msg required
   - T: Telemetry first - shouts completion for coordinator"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.tools.session-complete :as sc]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.events.handlers :as handlers]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.test-fixtures :as fixtures]))

;; =============================================================================
;; Test Fixtures and Helpers
;; =============================================================================

(def ^:dynamic *test-collection* "hive-mcp-test-session-complete")

(def ^:dynamic *effects-log* (atom []))

(defn with-mock-effects
  "Fixture that captures effect calls for verification."
  [f]
  (reset! *effects-log* [])
  ;; Reset event system for clean state
  (ev/reset-all!)
  (handlers/reset-registration!)
  (effects/reset-registration!)
  (sc/reset-registration!)
  ;; Register handlers and effects
  (effects/register-effects!)
  (handlers/register-handlers!)
  ;; Register session-complete handler
  (sc/register-handler!)
  ;; Override effects to capture calls
  (ev/reg-fx :git-commit
             (fn [data]
               (swap! *effects-log* conj [:git-commit data])))
  (ev/reg-fx :kanban-move-done
             (fn [data]
               (swap! *effects-log* conj [:kanban-move-done data])))
  (ev/reg-fx :wrap-crystallize
             (fn [data]
               (swap! *effects-log* conj [:wrap-crystallize data])))
  (ev/reg-fx :shout
             (fn [data]
               (swap! *effects-log* conj [:shout data])))
  (try
    (f)
    (finally
      (reset! *effects-log* []))))

(defn with-mock-embedder
  "Fixture that sets up mock embedder for testing."
  [f]
  (let [original-provider @@#'chroma/embedding-provider]
    (chroma/set-embedding-provider! (fixtures/->MockEmbedder 384))
    (chroma/configure! {:host "localhost"
                        :port 8000
                        :collection-name *test-collection*})
    (try
      (f)
      (finally
        (chroma/reset-collection-cache!)
        (reset! @#'chroma/embedding-provider original-provider)))))

(defn combined-fixture [f]
  (with-mock-embedder
    (fn []
      (with-mock-effects f))))

(use-fixtures :each combined-fixture)

(defn parse-mcp-response
  "Parse MCP response - extracts :text and parses as JSON."
  [response]
  (-> response :text (json/read-str :key-fn keyword)))

(defn find-effect
  "Find an effect in the log by type."
  [effect-type]
  (first (filter #(= (first %) effect-type) @*effects-log*)))

;; =============================================================================
;; Handler Tests
;; =============================================================================

(deftest handle-session-complete-basic
  (testing "Basic session complete triggers all effects"
    (let [result (sc/handle-session-complete
                  {:commit_msg "feat: implement session complete"
                   :agent_id "ling-test-123"})
          parsed (parse-mcp-response result)]
      ;; Should succeed
      (is (not (:error parsed)) "Should not return error")
      (is (= "ok" (:status parsed)) "Should return ok status")

      ;; Wait for async effects
      (Thread/sleep 100)

      ;; Verify git-commit effect was triggered
      (let [[_ git-data] (find-effect :git-commit)]
        (is (some? git-data) "Should trigger git-commit effect")
        (is (= "feat: implement session complete" (:message git-data))
            "Should pass commit message"))

      ;; Verify shout effect was triggered
      (let [[_ shout-data] (find-effect :shout)]
        (is (some? shout-data) "Should trigger shout effect")
        (is (= :completed (:event-type shout-data))
            "Should shout completed event")))))

(deftest handle-session-complete-with-task-ids
  (testing "Session complete moves kanban tasks to done"
    (let [result (sc/handle-session-complete
                  {:commit_msg "feat: complete tasks"
                   :task_ids ["task-1" "task-2"]
                   :agent_id "ling-test-456"})
          parsed (parse-mcp-response result)]
      (is (not (:error parsed)) "Should succeed")

      ;; Wait for async effects
      (Thread/sleep 100)

      ;; Verify kanban-move-done effect
      (let [[_ kanban-data] (find-effect :kanban-move-done)]
        (is (some? kanban-data) "Should trigger kanban-move-done effect")
        (is (= ["task-1" "task-2"] (:task-ids kanban-data))
            "Should pass task IDs")))))

(deftest handle-session-complete-validation
  (testing "Missing commit_msg returns error"
    (let [result (sc/handle-session-complete {:task_ids ["task-1"]})
          parsed (parse-mcp-response result)]
      (is (:error parsed) "Should return error")
      (is (str/includes? (:error parsed) "commit_msg")
          "Error should mention commit_msg")))

  (testing "Empty commit_msg returns error"
    (let [result (sc/handle-session-complete {:commit_msg ""})
          parsed (parse-mcp-response result)]
      (is (:error parsed) "Should return error for empty message"))))

(deftest handle-session-complete-wrap-crystallize
  (testing "Session complete triggers wrap crystallize"
    (let [result (sc/handle-session-complete
                  {:commit_msg "feat: session end"
                   :agent_id "ling-test-789"})
          _parsed (parse-mcp-response result)]

      ;; Wait for async effects
      (Thread/sleep 100)

      ;; Verify wrap-crystallize effect
      (let [[_ wrap-data] (find-effect :wrap-crystallize)]
        (is (some? wrap-data) "Should trigger wrap-crystallize effect")
        (is (= "ling-test-789" (:agent-id wrap-data))
            "Should pass agent ID to wrap")))))

(deftest handle-session-complete-agent-id-detection
  (testing "Agent ID defaults to env var when not provided"
    ;; This test verifies the fallback behavior
    (let [result (sc/handle-session-complete
                  {:commit_msg "feat: test agent detection"})
          parsed (parse-mcp-response result)]
      (is (not (:error parsed)) "Should succeed without explicit agent_id"))))

(deftest handle-session-complete-directory-scoping
  (testing "Directory is passed for project scoping"
    (let [result (sc/handle-session-complete
                  {:commit_msg "feat: scoped commit"
                   :directory "/project/path"
                   :agent_id "ling-scoped"})
          _parsed (parse-mcp-response result)]

      ;; Wait for async effects
      (Thread/sleep 100)

      ;; Git commit should receive directory
      (let [[_ git-data] (find-effect :git-commit)]
        (is (= "/project/path" (:cwd git-data))
            "Should pass directory as cwd for git")))))

;; =============================================================================
;; Event Handler Tests
;; =============================================================================

(deftest ling-session-complete-event-handler
  (testing "Event handler is registered"
    (is (ev/handler-registered? :ling/session-complete)
        "Handler should be registered")))

;; =============================================================================
;; Tool Definition Tests
;; =============================================================================

(deftest tool-definition-structure
  (testing "Tool has correct schema structure"
    (let [tool (first sc/tools)]
      (is (= "session_complete" (:name tool)))
      (is (string? (:description tool)))
      (is (fn? (:handler tool)))
      (let [schema (:inputSchema tool)
            props (:properties schema)]
        (is (= "object" (:type schema)))
        (is (contains? props "commit_msg"))
        (is (contains? props "task_ids"))
        (is (contains? props "agent_id"))
        (is (contains? props "directory"))
        (is (= ["commit_msg"] (:required schema)))))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest session-complete-full-flow
  (testing "Full session complete flow executes all steps"
    (let [result (sc/handle-session-complete
                  {:commit_msg "feat(session): complete sprint tasks"
                   :task_ids ["kanban-task-1" "kanban-task-2"]
                   :agent_id "ling-sprint-worker"
                   :directory "/home/lages/project"})
          parsed (parse-mcp-response result)]

      (is (= "ok" (:status parsed)) "Should succeed")
      (is (= "ling-sprint-worker" (:agent_id parsed))
          "Should return agent_id")

      ;; Wait for async effects
      (Thread/sleep 150)

      ;; Verify all effects were triggered
      (is (some? (find-effect :git-commit)) "git-commit triggered")
      (is (some? (find-effect :kanban-move-done)) "kanban-move-done triggered")
      (is (some? (find-effect :wrap-crystallize)) "wrap-crystallize triggered")
      (is (some? (find-effect :shout)) "shout triggered"))))
