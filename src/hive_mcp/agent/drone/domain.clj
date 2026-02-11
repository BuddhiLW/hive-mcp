(ns hive-mcp.agent.drone.domain
  "Domain value objects for drone execution: TaskSpec, ExecutionContext, ExecutionResult."
  (:require [clojure.string :as str]
            [hive-mcp.protocols.kg :as kg]))

(defrecord TaskSpec
           [task files task-type preset cwd parent-id wave-id options])

(defn ->task-spec
  "Create a validated TaskSpec from an options map."
  [{:keys [task files task-type preset cwd parent-id wave-id
           trace skip-auto-apply backend model seeds ctx-refs kg-node-ids]
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
                  :options (cond-> {:trace trace
                                    :skip-auto-apply skip-auto-apply}
                             backend          (assoc :backend backend)
                             model            (assoc :model model)
                             (seq seeds)      (assoc :seeds (vec seeds))
                             (seq ctx-refs)   (assoc :ctx-refs ctx-refs)
                             (seq kg-node-ids) (assoc :kg-node-ids (vec kg-node-ids)))}))

(defn task-spec?
  "Check if value is a TaskSpec."
  [x]
  (instance? TaskSpec x))

(def ^:dynamic *drone-kg-store*
  "Dynamic var for per-drone KG store binding (nil = use global store)."
  nil)

(defrecord ExecutionContext
           [drone-id task-id parent-id model sandbox start-time
            pre-validation file-contents-before project-root kg-store])

(defn ->execution-context
  "Create an ExecutionContext from an options map."
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
  "Get the KG store for a drone, resolving ctx > dynamic > global."
  [ctx]
  (or (:kg-store ctx)
      *drone-kg-store*
      (kg/get-store)))

(defrecord ExecutionResult
           [status agent-id task-id parent-id
            files-modified files-failed proposed-diff-ids
            duration-ms validation result error-info])

(defn ->execution-result
  "Create an ExecutionResult from context, status, and options."
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

(defn generate-drone-id
  "Generate a unique drone ID using UUID."
  []
  (str "drone-" (java.util.UUID/randomUUID)))

(defn generate-task-id
  "Generate a task ID from drone ID."
  [drone-id]
  (str "task-" drone-id))
