(ns hive-mcp.agent.drone.execution
  "Phase-based drone execution orchestration.

   Phases: prepare -> register -> validate -> execute -> finalize -> cleanup

   Heavy logic is delegated to sub-namespaces:
   - execution.env      — environment preparation, sandbox, backend resolution
   - execution.finalize — diff handling, validation, metrics, error handling

   This file is a thin orchestrator: wire phases, share lifecycle."
  (:require [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.agent.drone.diff-mgmt :as diff-mgmt]
            [hive-mcp.agent.drone.tools :as drone-tools]
            [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.agent.drone.decompose :as decompose]
            [hive-mcp.agent.drone.validation :as validation]
            [hive-mcp.agent.drone.kg-factory :as kg-factory]
            [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.backend :as backend]
            [hive-mcp.agent.drone.execution.env :as exec-env]
            [hive-mcp.agent.drone.execution.finalize :as exec-fin]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.dns.result :as result]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.events.core :as ev]
            [hive-mcp.tools.diff :as diff]
            [taoensso.timbre :as log]))

;;; ---------------------------------------------------------------------------
;;; Phase: Prepare
;;; ---------------------------------------------------------------------------

(defn phase:prepare
  "Prepare execution configuration including task type, tools, preset, and model."
  [task-spec]
  (let [{:keys [task files task-type preset cwd options]} task-spec
        model-override (:model options)
        effective-task-type (or task-type (preset/get-task-type task files))
        minimal-tools (drone-tools/get-tools-for-drone effective-task-type files)
        effective-preset (or preset (preset/select-drone-preset task files))
        model-selection (when-not model-override
                          (routing/route-and-select task files {:directory cwd}))
        effective-model (or model-override (:model model-selection))
        step-budget (decompose/get-step-budget task files)]

    (log/info "Drone configuration prepared:"
              {:task-type effective-task-type
               :preset effective-preset
               :model effective-model
               :model-override? (some? model-override)
               :model-reason (if model-override :wave-override (:reason model-selection))
               :tool-count (count minimal-tools)
               :max-steps step-budget})

    {:task-type effective-task-type
     :tools minimal-tools
     :preset effective-preset
     :model effective-model
     :model-fallback (when model-selection (:fallback model-selection))
     :step-budget step-budget
     :model-selection model-selection}))

;;; ---------------------------------------------------------------------------
;;; Phase: Register
;;; ---------------------------------------------------------------------------

(defn phase:register!
  "Register drone in DataScript and acquire file locks."
  [ctx task-spec]
  (let [{:keys [drone-id task-id parent-id]} ctx
        {:keys [files]} task-spec
        tx-result (ds/add-slave! drone-id {:status :spawning
                                           :name "drone"
                                           :depth 2
                                           :parent parent-id})]

    (when-not (and tx-result (seq (:tx-data tx-result)))
      (log/error {:event :drone/registration-failed
                  :drone-id drone-id
                  :tx-result tx-result})
      (throw (ex-info "Failed to register drone in DataScript"
                      {:drone-id drone-id})))

    (when (seq files)
      (let [claim (coordinator/atomic-claim-files! task-id drone-id files)]
        (when-not (:acquired? claim)
          (result/rescue nil (ds/remove-slave! drone-id))
          (log/error {:event :drone/error
                      :error-type :conflict
                      :drone-id drone-id
                      :task-id task-id
                      :files files
                      :conflicts (:conflicts claim)})
          (ev/dispatch [:drone/failed {:drone-id drone-id
                                       :task-id task-id
                                       :parent-id parent-id
                                       :error "File conflicts detected"
                                       :error-type :conflict
                                       :files files}])
          (throw (ex-info "File conflicts detected - files locked by another drone"
                          {:conflicts (:conflicts claim)
                           :drone-id drone-id
                           :files files})))
        (log/info "Drone acquired file locks:" drone-id "(" (:files-claimed claim) "files)")))

    ctx))

;;; ---------------------------------------------------------------------------
;;; Phase: Validate
;;; ---------------------------------------------------------------------------

(defn phase:validate
  "Pre-execution file validation."
  [ctx task-spec]
  (let [{:keys [drone-id]} ctx
        {:keys [files]} task-spec]
    (if (empty? files)
      ctx
      (let [pre-validation (validation/validate-files-pre files (:task-id ctx))
            _ (when-not (validation/all-valid? pre-validation :pre)
                (log/warn {:event :drone/pre-validation-failed
                           :drone-id drone-id
                           :invalid-files (->> pre-validation
                                               (filter (comp not :pre-valid? val))
                                               (map key))}))
            file-contents-before (into {}
                                       (for [f files]
                                         [f (result/rescue nil (slurp f))]))]
        (-> ctx
            (domain/with-pre-validation pre-validation)
            (domain/with-file-contents-before file-contents-before))))))

;;; ---------------------------------------------------------------------------
;;; Phase: Execute
;;; ---------------------------------------------------------------------------

(defn phase:execute!
  "Execute task via delegate-fn."
  [ctx task-spec config delegate-fn]
  (let [{:keys [options]} task-spec
        {:keys [augmented-task drone-sandbox]} (exec-env/prepare-execution-env ctx task-spec config)]
    (exec-env/emit-execution-started! ctx task-spec :delegate)
    (let [task-context (-> (exec-env/build-task-context ctx task-spec config augmented-task drone-sandbox)
                           (assoc :trace (:trace options true)))]
      (binding [domain/*drone-kg-store* (:kg-store ctx)]
        (delegate-fn task-context)))))

(defn phase:execute-agentic!
  "Execute task via IDroneExecutionBackend dispatch."
  [ctx task-spec config]
  (let [{:keys [options]} task-spec
        {:keys [augmented-task drone-sandbox]} (exec-env/prepare-execution-env ctx task-spec config)]
    (exec-env/emit-execution-started! ctx task-spec :agentic)
    (let [task-context (exec-env/build-task-context ctx task-spec config augmented-task drone-sandbox)
          resolved-backend (exec-env/resolve-backend task-context (:backend options))]
      (log/info {:event    :drone/backend-resolved
                 :drone-id (:drone-id ctx)
                 :backend  (backend/backend-type resolved-backend)})
      (binding [domain/*drone-kg-store* (:kg-store ctx)]
        (backend/execute-drone resolved-backend task-context)))))

;;; ---------------------------------------------------------------------------
;;; Phase: Cleanup
;;; ---------------------------------------------------------------------------

(defn phase:cleanup!
  "Release resources: file claims, KG store, DataScript registration."
  [ctx task-spec]
  (let [{:keys [drone-id task-id]} ctx
        {:keys [files]} task-spec]
    (when (seq files)
      (coordinator/release-task-claims! task-id)
      (log/info "Drone released file locks:" drone-id))
    (when (:kg-store ctx)
      (result/rescue nil
                     (kg-factory/close-drone-store! drone-id)
                     (log/debug "Drone KG store closed:" drone-id)))
    (ds/remove-slave! drone-id)))

;;; ---------------------------------------------------------------------------
;;; Orchestration: shared lifecycle for delegate and agentic modes
;;; ---------------------------------------------------------------------------

(defn- create-kg-store
  "Create KG store appropriate for execution mode."
  [mode drone-id]
  (if (= mode :agentic)
    (or (result/rescue nil (session-kg/create-session-kg! drone-id))
        (result/rescue nil (kg-factory/create-drone-store drone-id)))
    (result/rescue nil (kg-factory/create-drone-store drone-id))))

(defn- build-initial-context
  "Create drone ID, task ID, parent ID, and execution context."
  [task-spec mode]
  (let [drone-id (domain/generate-drone-id)
        task-id (domain/generate-task-id drone-id)
        parent-id (or (:parent-id task-spec)
                      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        cwd (or (:cwd task-spec) (diff/get-project-root))
        kg-store (create-kg-store mode drone-id)]
    {:ctx (domain/->execution-context
           {:drone-id drone-id
            :task-id task-id
            :parent-id parent-id
            :project-root cwd
            :kg-store kg-store})
     :kg-store kg-store}))

(defn- run-orchestrated!
  "Shared orchestration for both delegate and agentic execution modes.
   execute-fn receives (ctx, task-spec, config) and returns raw-result."
  [task-spec mode execute-fn]
  (let [{:keys [ctx kg-store]} (build-initial-context task-spec mode)
        config (phase:prepare task-spec)
        ctx (assoc ctx :model (:model config))]
    (try
      (let [ctx (phase:register! ctx task-spec)
            diffs-before (diff-mgmt/capture-diffs-before)
            ctx (phase:validate ctx task-spec)]
        (try
          (let [raw-result (execute-fn ctx task-spec config)]
            (exec-fin/phase:finalize! ctx task-spec config raw-result diffs-before))
          (catch Throwable e
            (exec-fin/phase:handle-error! ctx task-spec config e)
            (throw e))))
      (finally
        (when (and (= mode :agentic) kg-store)
          (result/rescue nil (session-kg/close-session-kg! kg-store (:drone-id ctx))))
        (phase:cleanup! ctx task-spec)))))

;;; ---------------------------------------------------------------------------
;;; Public entry points
;;; ---------------------------------------------------------------------------

(defn run-execution!
  "Orchestrate drone execution through all phases with proper error handling and cleanup."
  [task-spec delegate-fn]
  (run-orchestrated! task-spec :delegate
                     (fn [ctx task-spec' config]
                       (phase:execute! ctx task-spec' config delegate-fn))))

(defn run-agentic-execution!
  "Orchestrate agentic drone execution through all phases."
  [task-spec]
  (run-orchestrated! task-spec :agentic
                     phase:execute-agentic!))
