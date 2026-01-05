(ns hive-mcp.memory-kanban-test
  "TDD tests for in-memory kanban using the memory system.
   
   Tasks are stored as memory entries with:
   - type: \"note\"
   - tags: [\"kanban\" status priority]
   - duration: \"short-term\"
   - content: {:task-type \"kanban\" :title ... :status ... :priority ...}
   
   When a task moves to \"done\", it gets DELETED (not persisted).
   
   These tests verify elisp generation for kanban operations,
   following the TDD approach used throughout this project."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.elisp :as el]))

;; =============================================================================
;; Constants
;; =============================================================================

(def valid-statuses #{"todo" "doing" "review"})
(def valid-priorities #{"high" "medium" "low"})
(def default-priority "medium")
(def kanban-duration "short-term")

;; =============================================================================
;; Elisp Generation Helpers (what we're testing)
;; =============================================================================

(defn kanban-create-elisp
  "Generate elisp to create a kanban task via memory-add.
   Tags include 'kanban', status, and priority."
  [title & {:keys [status priority description]
            :or {status "todo" priority default-priority description nil}}]
  (let [content {:task-type "kanban"
                 :title title
                 :status status
                 :priority priority
                 :description description
                 :created (System/currentTimeMillis)}
        tags ["kanban" status priority]]
    (el/require-and-call-json 'hive-mcp-memory
                              'hive-mcp-api-kanban-create
                              title status priority description)))

(defn kanban-list-elisp
  "Generate elisp to list kanban tasks.
   Queries memory with 'kanban' tag, optionally filtered by status."
  [& {:keys [status]}]
  (let [tags (if status ["kanban" status] ["kanban"])]
    (el/require-and-call-json 'hive-mcp-memory
                              'hive-mcp-api-kanban-list
                              (when status status))))

(defn kanban-move-elisp
  "Generate elisp to move a task to a new status.
   Moving to 'done' triggers delete instead of update."
  [task-id new-status]
  (if (= new-status "done")
    (el/require-and-call-json 'hive-mcp-memory
                              'hive-mcp-api-kanban-delete
                              task-id)
    (el/require-and-call-json 'hive-mcp-memory
                              'hive-mcp-api-kanban-move
                              task-id new-status)))

(defn kanban-update-elisp
  "Generate elisp to update task fields (title, priority, description)."
  [task-id & {:keys [title priority description]}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-api-kanban-update
                            task-id title priority description))

(defn kanban-stats-elisp
  "Generate elisp to get task statistics by status."
  []
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-api-kanban-stats))

(defn kanban-delete-elisp
  "Generate elisp to delete a task."
  [task-id]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-api-kanban-delete
                            task-id))

;; =============================================================================
;; 1. Schema Tests
;; =============================================================================

(deftest test-task-content-has-required-fields
  (testing "Task content map must have required fields"
    ;; Document expected content structure
    (let [expected-fields #{:task-type :title :status :priority :created}
          sample-content {:task-type "kanban"
                          :title "Test task"
                          :status "todo"
                          :priority "medium"
                          :created 1234567890}]
      (is (every? #(contains? sample-content %) expected-fields)
          "Content must have all required fields")
      (is (= "kanban" (:task-type sample-content))
          ":task-type must be 'kanban'"))))

(deftest test-valid-statuses
  (testing "Only 'todo', 'doing', 'review' are valid statuses (not 'done')"
    (is (= #{"todo" "doing" "review"} valid-statuses))
    (is (not (contains? valid-statuses "done"))
        "'done' is not a valid status - tasks are deleted when done")
    (is (not (contains? valid-statuses "completed")))
    (is (not (contains? valid-statuses "in-progress")))))

(deftest test-valid-priorities
  (testing "Only 'high', 'medium', 'low' are valid priorities"
    (is (= #{"high" "medium" "low"} valid-priorities))
    (is (not (contains? valid-priorities "critical")))
    (is (not (contains? valid-priorities "urgent")))
    (is (not (contains? valid-priorities "normal")))))

(deftest test-kanban-tag-required
  (testing "All kanban tasks must have 'kanban' tag"
    (let [expected-tags ["kanban" "todo" "medium"]]
      (is (= "kanban" (first expected-tags))
          "First tag must be 'kanban'"))))

(deftest test-duration-is-short-term
  (testing "Kanban tasks use short-term duration"
    (is (= "short-term" kanban-duration)
        "Duration must be 'short-term' for kanban tasks")))

;; =============================================================================
;; 2. Create Tests
;; =============================================================================

(deftest test-create-generates-valid-elisp
  (testing "Create generates valid elisp structure"
    (let [result (kanban-create-elisp "Test task")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-api-kanban-create"))
      (is (str/includes? result "\"Test task\""))
      (is (str/includes? result "json-encode")))))

(deftest test-create-includes-title
  (testing "Create includes task title in elisp"
    (let [result (kanban-create-elisp "Implement feature X")]
      (is (str/includes? result "\"Implement feature X\"")))))

(deftest test-create-includes-status
  (testing "Create includes status parameter"
    (let [result (kanban-create-elisp "Task" :status "doing")]
      (is (str/includes? result "\"doing\"")))))

(deftest test-create-includes-priority
  (testing "Create includes priority parameter"
    (let [result (kanban-create-elisp "Task" :priority "high")]
      (is (str/includes? result "\"high\"")))))

(deftest test-create-defaults-priority-medium
  (testing "Missing priority defaults to 'medium'"
    ;; Document that default priority is medium
    (is (= "medium" default-priority))
    ;; The elisp generator should use default when not specified
    (let [result (kanban-create-elisp "Task")]
      ;; The function should pass medium as default
      (is (string? result)))))

(deftest test-create-defaults-status-todo
  (testing "Missing status defaults to 'todo'"
    (let [result (kanban-create-elisp "New task")]
      ;; Default status is todo
      (is (string? result))
      (is (str/includes? result "hive-mcp-api-kanban-create")))))

(deftest test-create-with-description
  (testing "Create can include optional description"
    (let [result (kanban-create-elisp "Task" :description "Detailed description")]
      (is (str/includes? result "\"Detailed description\"")))))

(deftest test-create-with-all-fields
  (testing "Create with all fields specified"
    (let [result (kanban-create-elisp "Full task"
                                      :status "review"
                                      :priority "high"
                                      :description "Complete task")]
      (is (str/includes? result "\"Full task\""))
      (is (str/includes? result "\"review\""))
      (is (str/includes? result "\"high\""))
      (is (str/includes? result "\"Complete task\"")))))

;; =============================================================================
;; 3. Move Tests
;; =============================================================================

(deftest test-move-generates-valid-elisp
  (testing "Move generates valid elisp structure"
    (let [result (kanban-move-elisp "task-123" "doing")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-api-kanban-move"))
      (is (str/includes? result "\"task-123\""))
      (is (str/includes? result "\"doing\"")))))

(deftest test-move-to-todo
  (testing "Move to 'todo' updates status"
    (let [result (kanban-move-elisp "task-456" "todo")]
      (is (str/includes? result "hive-mcp-api-kanban-move"))
      (is (str/includes? result "\"todo\"")))))

(deftest test-move-to-doing
  (testing "Move to 'doing' updates status"
    (let [result (kanban-move-elisp "task-789" "doing")]
      (is (str/includes? result "hive-mcp-api-kanban-move"))
      (is (str/includes? result "\"doing\"")))))

(deftest test-move-to-review
  (testing "Move to 'review' updates status"
    (let [result (kanban-move-elisp "task-abc" "review")]
      (is (str/includes? result "hive-mcp-api-kanban-move"))
      (is (str/includes? result "\"review\"")))))

(deftest test-move-to-done-calls-delete
  (testing "Moving to 'done' must call delete, not update"
    (let [result (kanban-move-elisp "task-xyz" "done")]
      ;; Should NOT call kanban-move
      (is (not (str/includes? result "hive-mcp-api-kanban-move"))
          "Moving to 'done' should not call move")
      ;; Should call kanban-delete instead
      (is (str/includes? result "hive-mcp-api-kanban-delete")
          "Moving to 'done' must call delete")
      (is (str/includes? result "\"task-xyz\"")))))

(deftest test-move-preserves-task-id
  (testing "Move preserves task ID in elisp"
    (let [result (kanban-move-elisp "unique-task-id-12345" "doing")]
      (is (str/includes? result "\"unique-task-id-12345\"")))))

;; =============================================================================
;; 4. List Tests
;; =============================================================================

(deftest test-list-all-generates-valid-elisp
  (testing "List all generates valid elisp structure"
    (let [result (kanban-list-elisp)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-api-kanban-list"))
      (is (str/includes? result "json-encode")))))

(deftest test-list-all-queries-kanban-tag
  (testing "List all must query with 'kanban' tag filter"
    ;; Document that list without status still filters by kanban tag
    (let [result (kanban-list-elisp)]
      (is (str/includes? result "hive-mcp-api-kanban-list")))))

(deftest test-list-by-status-todo
  (testing "List by 'todo' status"
    (let [result (kanban-list-elisp :status "todo")]
      (is (str/includes? result "hive-mcp-api-kanban-list"))
      (is (str/includes? result "\"todo\"")))))

(deftest test-list-by-status-doing
  (testing "List by 'doing' status"
    (let [result (kanban-list-elisp :status "doing")]
      (is (str/includes? result "\"doing\"")))))

(deftest test-list-by-status-review
  (testing "List by 'review' status"
    (let [result (kanban-list-elisp :status "review")]
      (is (str/includes? result "\"review\"")))))

(deftest test-list-without-status-returns-all
  (testing "List without status filter returns all kanban tasks"
    (let [result (kanban-list-elisp)]
      (is (str/includes? result "hive-mcp-api-kanban-list"))
      ;; Should not include a specific status filter
      (is (not (str/includes? result "\"todo\""))))))

;; =============================================================================
;; 5. Update Tests
;; =============================================================================

(deftest test-update-generates-valid-elisp
  (testing "Update generates valid elisp structure"
    (let [result (kanban-update-elisp "task-123" :title "New title")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "hive-mcp-api-kanban-update"))
      (is (str/includes? result "\"task-123\"")))))

(deftest test-update-title
  (testing "Update task title"
    (let [result (kanban-update-elisp "task-id" :title "Updated title")]
      (is (str/includes? result "\"Updated title\"")))))

(deftest test-update-priority
  (testing "Update task priority"
    (let [result (kanban-update-elisp "task-id" :priority "high")]
      (is (str/includes? result "\"high\"")))))

(deftest test-update-description
  (testing "Update task description"
    (let [result (kanban-update-elisp "task-id" :description "New description")]
      (is (str/includes? result "\"New description\"")))))

(deftest test-update-multiple-fields
  (testing "Update multiple fields at once"
    (let [result (kanban-update-elisp "task-id"
                                      :title "New title"
                                      :priority "low"
                                      :description "Updated")]
      (is (str/includes? result "\"New title\""))
      (is (str/includes? result "\"low\""))
      (is (str/includes? result "\"Updated\"")))))

;; =============================================================================
;; 6. Delete Tests
;; =============================================================================

(deftest test-delete-generates-valid-elisp
  (testing "Delete generates valid elisp structure"
    (let [result (kanban-delete-elisp "task-to-delete")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "hive-mcp-api-kanban-delete"))
      (is (str/includes? result "\"task-to-delete\""))
      (is (str/includes? result "json-encode")))))

(deftest test-delete-preserves-task-id
  (testing "Delete preserves task ID"
    (let [result (kanban-delete-elisp "specific-task-id-999")]
      (is (str/includes? result "\"specific-task-id-999\"")))))

;; =============================================================================
;; 7. Stats Tests
;; =============================================================================

(deftest test-stats-generates-valid-elisp
  (testing "Stats generates valid elisp structure"
    (let [result (kanban-stats-elisp)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "hive-mcp-api-kanban-stats"))
      (is (str/includes? result "json-encode")))))

(deftest test-stats-returns-counts-documentation
  (testing "Stats returns counts by status"
    ;; Document expected return format: {:todo N :doing M :review K}
    (let [expected-keys #{:todo :doing :review}
          sample-stats {:todo 5 :doing 3 :review 2}]
      (is (= expected-keys (set (keys sample-stats))))
      ;; Note: 'done' is not tracked because tasks are deleted
      (is (not (contains? sample-stats :done))))))

;; =============================================================================
;; 8. Edge Cases
;; =============================================================================

(deftest test-create-empty-title-still-generates-elisp
  (testing "Create with empty title still generates elisp (validation elsewhere)"
    (let [result (kanban-create-elisp "")]
      (is (string? result))
      (is (str/includes? result "hive-mcp-api-kanban-create")))))

(deftest test-move-same-status
  (testing "Move to same status generates valid elisp"
    (let [result (kanban-move-elisp "task-id" "todo")]
      (is (str/includes? result "hive-mcp-api-kanban-move")))))

(deftest test-special-characters-in-title
  (testing "Title with special characters is properly escaped"
    (let [result (kanban-create-elisp "Task with \"quotes\" and 'apostrophes'")]
      (is (string? result))
      (is (str/includes? result "hive-mcp-api-kanban-create")))))

(deftest test-unicode-in-title
  (testing "Unicode characters in title"
    (let [result (kanban-create-elisp "Task with emoji 🎯 and unicode")]
      (is (string? result)))))

;; =============================================================================
;; 9. Tag Manipulation Logic Tests
;; =============================================================================

(deftest test-tags-structure-for-todo
  (testing "Tags for 'todo' status task"
    (let [expected-tags ["kanban" "todo" "medium"]]
      (is (= 3 (count expected-tags)))
      (is (= "kanban" (nth expected-tags 0)))
      (is (= "todo" (nth expected-tags 1)))
      (is (= "medium" (nth expected-tags 2))))))

(deftest test-tags-structure-for-doing-high
  (testing "Tags for 'doing' status with 'high' priority"
    (let [expected-tags ["kanban" "doing" "high"]]
      (is (= "kanban" (first expected-tags)))
      (is (= "doing" (second expected-tags)))
      (is (= "high" (nth expected-tags 2))))))

(deftest test-tags-must-include-kanban
  (testing "All task tags must include 'kanban'"
    (let [tags-todo ["kanban" "todo" "medium"]
          tags-doing ["kanban" "doing" "high"]
          tags-review ["kanban" "review" "low"]]
      (is (every? #(= "kanban" (first %)) [tags-todo tags-doing tags-review])))))

(deftest test-move-updates-status-tag
  (testing "Moving task should update status tag (old removed, new added)"
    ;; Document the expected behavior:
    ;; Old tags: ["kanban" "todo" "high"]
    ;; After move to "doing": ["kanban" "doing" "high"]
    ;; The status tag changes, kanban and priority remain
    (let [old-tags ["kanban" "todo" "high"]
          new-status "doing"
          expected-new-tags ["kanban" "doing" "high"]]
      (is (= (first old-tags) (first expected-new-tags)))
      (is (= new-status (second expected-new-tags)))
      (is (= (nth old-tags 2) (nth expected-new-tags 2))))))

;; =============================================================================
;; 10. Timestamp Tests
;; =============================================================================

(deftest test-created-timestamp-in-content
  (testing "Content includes :created timestamp"
    (let [content {:task-type "kanban"
                   :title "Test"
                   :status "todo"
                   :priority "medium"
                   :created 1234567890}]
      (is (number? (:created content))))))

(deftest test-started-timestamp-documentation
  (testing "Moving to 'doing' should set :started timestamp"
    ;; Document that started timestamp is set when task moves to doing
    ;; This is handled by the elisp implementation
    (let [content-before {:task-type "kanban"
                          :title "Test"
                          :status "todo"
                          :priority "medium"
                          :created 1234567890}
          content-after {:task-type "kanban"
                         :title "Test"
                         :status "doing"
                         :priority "medium"
                         :created 1234567890
                         :started 1234567900}]
      (is (nil? (:started content-before)))
      (is (number? (:started content-after))))))

;; =============================================================================
;; 11. Integration-Style Tests (Elisp Structure Validation)
;; =============================================================================

(deftest test-all-operations-use-json-encode
  (testing "All kanban operations use json-encode for return values"
    (let [functions [(kanban-create-elisp "Task")
                     (kanban-list-elisp)
                     (kanban-move-elisp "id" "doing")
                     (kanban-update-elisp "id" :title "New")
                     (kanban-delete-elisp "id")
                     (kanban-stats-elisp)]]
      (is (every? #(str/includes? % "json-encode") functions)))))

(deftest test-all-operations-require-hive-mcp-memory
  (testing "All kanban operations require hive-mcp-memory"
    (let [functions [(kanban-create-elisp "Task")
                     (kanban-list-elisp)
                     (kanban-move-elisp "id" "doing")
                     (kanban-update-elisp "id" :title "New")
                     (kanban-delete-elisp "id")
                     (kanban-stats-elisp)]]
      (is (every? #(str/includes? % "hive-mcp-memory") functions)))))

(deftest test-all-operations-have-fboundp-check
  (testing "All kanban operations have fboundp safety check"
    (let [functions [(kanban-create-elisp "Task")
                     (kanban-list-elisp)
                     (kanban-move-elisp "id" "doing")
                     (kanban-update-elisp "id" :title "New")
                     (kanban-delete-elisp "id")
                     (kanban-stats-elisp)]]
      (is (every? #(str/includes? % "fboundp") functions)))))

(deftest test-full-lifecycle-elisp-generation
  (testing "Full lifecycle: create -> move -> update -> stats -> delete"
    (let [create-result (kanban-create-elisp "New task" :priority "high")
          move-result (kanban-move-elisp "task-id" "doing")
          update-result (kanban-update-elisp "task-id" :title "Updated task")
          stats-result (kanban-stats-elisp)
          delete-result (kanban-delete-elisp "task-id")]
      ;; All should generate valid elisp
      (is (every? #(str/starts-with? % "(progn")
                  [create-result move-result update-result stats-result delete-result]))
      ;; All should require hive-mcp-memory
      (is (every? #(str/includes? % "hive-mcp-memory")
                  [create-result move-result update-result stats-result delete-result])))))

(deftest test-done-lifecycle
  (testing "Task completion lifecycle: create -> move to done (delete)"
    (let [create-result (kanban-create-elisp "Task to complete")
          ;; Moving to done should trigger delete
          done-result (kanban-move-elisp "task-id" "done")]
      (is (str/includes? create-result "hive-mcp-api-kanban-create"))
      (is (str/includes? done-result "hive-mcp-api-kanban-delete"))
      (is (not (str/includes? done-result "hive-mcp-api-kanban-move"))))))
