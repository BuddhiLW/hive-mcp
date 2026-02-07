(ns hive-mcp.agent.drone.domain
  "Domain value objects for drone execution.

   Extracted from drone.clj to provide type-safe, immutable data structures
   with validated constructors (SOLID-O, CLARITY-I, CLARITY-R).

   Records:
   - TaskSpec: Immutable task configuration
   - ExecutionContext: Runtime execution state
   - ExecutionResult: Execution outcome with metrics

   All constructors validate inputs (CLARITY-I: Inputs guarded)."
  (:require [clojure.string :as str]
            [hive-mcp.protocols.kg :as kg]))

;;; ============================================================
;;; TaskSpec - Immutable Task Configuration
;;; ============================================================

(defrecord TaskSpec
           [task files task-type preset cwd parent-id wave-id options]
  ;; task      - Task description string (required)
  ;; files     - Vector of file paths to modify
  ;; task-type - Keyword (:testing, :refactoring, :bugfix, :documentation, :general)
  ;; preset    - Drone preset name
  ;; cwd       - Working directory (project root)
  ;; parent-id - Parent ling's slave-id
  ;; wave-id   - Wave identifier for batch operations
  ;; options   - Additional options map (:trace, :skip-auto-apply, etc.)
  )

(defn ->task-spec
  "Create a validated TaskSpec.

   Arguments:
     opts - Map with:
       :task      - Task description (required, non-blank string)
       :files     - File paths (default: [])
       :task-type - Task type keyword (default: :general)
       :preset    - Preset name (optional)
       :cwd       - Working directory (optional)
       :parent-id - Parent ling ID (optional)
       :wave-id   - Wave ID (optional)
       :trace     - Enable tracing (default: true)
       :skip-auto-apply - Don't auto-apply diffs (default: false)

   Returns:
     TaskSpec record

   Throws:
     ex-info if :task is missing or blank (CLARITY-I)"
  [{:keys [task files task-type preset cwd parent-id wave-id
           trace skip-auto-apply]
    :or {files []
         task-type :general
         trace true
         skip-auto-apply false}}]
  (when (or (nil? task) (and (string? task) (str/blank? task)))
    (throw (ex-info "TaskSpec requires non-blank :task"
                    {:error-type :validation
                     :field :task
                     :value task})))
  (map->TaskSpec {:task task
                  :files (vec files)
                  :task-type (or task-type :general)
                  :preset preset
                  :cwd cwd
                  :parent-id parent-id
                  :wave-id wave-id
                  :options {:trace trace
                            :skip-auto-apply skip-auto-apply}}))

(defn task-spec?
  "Check if value is a TaskSpec."
  [x]
  (instance? TaskSpec x))

;;; ============================================================
;;; ExecutionContext - Runtime Execution State
;;; ============================================================

(def ^:dynamic *drone-kg-store*
  "Dynamic var for per-drone KG store binding.
   Alternative to threading kg-store through ExecutionContext.
   Bind via (binding [*drone-kg-store* my-store] ...) in code paths
   that don't have access to the execution context.
   nil = use global store."
  nil)

(defrecord ExecutionContext
           [drone-id task-id parent-id model sandbox start-time
            pre-validation file-contents-before project-root kg-store]
  ;; drone-id             - Unique drone identifier
  ;; task-id              - Task identifier for claims
  ;; parent-id            - Parent ling ID
  ;; model                - Selected model for execution
  ;; sandbox              - Sandbox configuration map
  ;; start-time           - Execution start timestamp (millis)
  ;; pre-validation       - Pre-execution validation results
  ;; file-contents-before - File contents snapshot for post-validation
  ;; project-root         - Resolved project root path
  ;; kg-store             - Optional IKGStore instance for per-drone KG isolation (nil = use global)
  )

(defn ->execution-context
  "Create an ExecutionContext.

   Arguments:
     opts - Map with:
       :drone-id     - Drone identifier (required)
       :task-id      - Task identifier (default: \"task-{drone-id}\")
       :parent-id    - Parent ling ID
       :model        - Selected model
       :sandbox      - Sandbox config map
       :project-root - Project root path
       :kg-store     - Optional IKGStore for per-drone KG isolation (nil = use global)

   Returns:
     ExecutionContext record

   Throws:
     ex-info if :drone-id is missing"
  [{:keys [drone-id task-id parent-id model sandbox project-root kg-store]}]
  (when (str/blank? drone-id)
    (throw (ex-info "ExecutionContext requires :drone-id"
                    {:error-type :validation
                     :field :drone-id})))
  (map->ExecutionContext
   {:drone-id drone-id
    :task-id (or task-id (str "task-" drone-id))
    :parent-id parent-id
    :model model
    :sandbox sandbox
    :start-time (System/currentTimeMillis)
    :pre-validation nil
    :file-contents-before nil
    :project-root project-root
    :kg-store kg-store}))

(defn with-pre-validation
  "Add pre-validation results to execution context."
  [ctx validation]
  (assoc ctx :pre-validation validation))

(defn with-file-contents-before
  "Add file contents snapshot to execution context."
  [ctx contents]
  (assoc ctx :file-contents-before contents))

(defn execution-context?
  "Check if value is an ExecutionContext."
  [x]
  (instance? ExecutionContext x))

(defn elapsed-ms
  "Calculate elapsed milliseconds since context start."
  [ctx]
  (- (System/currentTimeMillis) (:start-time ctx)))

(defn get-kg-store
  "Get the KG store for a drone execution context.

   Resolution order:
   1. :kg-store from ExecutionContext (explicit per-drone store)
   2. *drone-kg-store* dynamic var (bound in current thread)
   3. Global store via kg/get-store (singleton fallback)

   This allows per-drone KG isolation when needed while
   maintaining backward compatibility (nil = global behavior).

   Arguments:
     ctx - ExecutionContext (or nil for dynamic/global fallback)

   Returns:
     IKGStore instance"
  [ctx]
  (or (:kg-store ctx)
      *drone-kg-store*
      (kg/get-store)))

;;; ============================================================
;;; ExecutionResult - Execution Outcome
;;; ============================================================

(defrecord ExecutionResult
           [status agent-id task-id parent-id
            files-modified files-failed proposed-diff-ids
            duration-ms validation result error-info]
  ;; status           - :completed | :failed | :timeout | :cancelled
  ;; agent-id         - Drone ID that executed
  ;; task-id          - Task identifier
  ;; parent-id        - Parent ling ID
  ;; files-modified   - Vector of successfully modified files
  ;; files-failed     - Vector of {:file :error} for failures
  ;; proposed-diff-ids - Vector of diff IDs for review mode
  ;; duration-ms      - Execution duration
  ;; validation       - Validation summary map
  ;; result           - Raw execution result
  ;; error-info       - Error details if failed
  )

(defn ->execution-result
  "Create an ExecutionResult.

   Arguments:
     ctx    - ExecutionContext
     status - :completed or :failed
     opts   - Additional result data:
       :result         - Raw execution result
       :diff-results   - DiffResults record
       :validation     - Validation summary
       :error-info     - Error details (for failures)"
  [ctx status {:keys [result diff-results validation error-info]}]
  (map->ExecutionResult
   {:status status
    :agent-id (:drone-id ctx)
    :task-id (:task-id ctx)
    :parent-id (:parent-id ctx)
    :files-modified (or (:applied diff-results) [])
    :files-failed (or (:failed diff-results) [])
    :proposed-diff-ids (or (:proposed diff-results) [])
    :duration-ms (elapsed-ms ctx)
    :validation validation
    :result result
    :error-info error-info}))

(defn success-result
  "Create a successful ExecutionResult."
  [ctx opts]
  (->execution-result ctx :completed opts))

(defn failure-result
  "Create a failed ExecutionResult."
  [ctx error-info]
  (->execution-result ctx :failed {:error-info error-info}))

(defn execution-result?
  "Check if value is an ExecutionResult."
  [x]
  (instance? ExecutionResult x))

(defn completed?
  "Check if result status is :completed."
  [result]
  (= :completed (:status result)))

(defn failed?
  "Check if result status is :failed."
  [result]
  (= :failed (:status result)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defn generate-drone-id
  "Generate a unique drone ID using UUID.

   Returns:
     String in format \"drone-{uuid}\""
  []
  (str "drone-" (java.util.UUID/randomUUID)))

(defn generate-task-id
  "Generate a task ID from drone ID.

   Arguments:
     drone-id - The drone identifier

   Returns:
     String in format \"task-{drone-id}\""
  [drone-id]
  (str "task-" drone-id))
