(ns hive-mcp.agent.drone.execution
  "Phase-based drone execution orchestration."
  (:require [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.agent.drone.diff-mgmt :as diff-mgmt]
            [hive-mcp.agent.drone.augment :as augment]
            [hive-mcp.agent.drone.backend :as backend]
            [hive-mcp.agent.drone.ext-router :as ext-router]
            [hive-mcp.agent.drone.kg-factory :as kg-factory]
            [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.tools :as drone-tools]
            [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.agent.drone.decompose :as decompose]
            [hive-mcp.agent.drone.validation :as validation]
            [hive-mcp.agent.drone.errors :as errors]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.agent.cost :as cost]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hivemind.core :as hivemind]
            [hive-mcp.events.core :as ev]
            [hive-mcp.telemetry.prometheus :as prom]
            [hive-mcp.tools.diff :as diff]
            [taoensso.timbre :as log]))

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

(defn phase:register!
  "Register drone in DataScript and acquire file locks."
  [ctx task-spec]
  (let [{:keys [drone-id task-id parent-id]} ctx
        {:keys [files]} task-spec]

    (let [tx-result (ds/add-slave! drone-id {:status :spawning
                                             :name "drone"
                                             :depth 2
                                             :parent parent-id})]
      (when-not (and tx-result (seq (:tx-data tx-result)))
        (log/error {:event :drone/registration-failed
                    :drone-id drone-id
                    :tx-result tx-result})
        (throw (ex-info "Failed to register drone in DataScript"
                        {:drone-id drone-id}))))

    (when (seq files)
      (let [result (coordinator/atomic-claim-files! task-id drone-id files)]
        (when-not (:acquired? result)
          (try (ds/remove-slave! drone-id) (catch Exception _ nil))

          (log/error {:event :drone/error
                      :error-type :conflict
                      :drone-id drone-id
                      :task-id task-id
                      :files files
                      :conflicts (:conflicts result)})

          (ev/dispatch [:drone/failed {:drone-id drone-id
                                       :task-id task-id
                                       :parent-id parent-id
                                       :error "File conflicts detected"
                                       :error-type :conflict
                                       :files files}])

          (throw (ex-info "File conflicts detected - files locked by another drone"
                          {:conflicts (:conflicts result)
                           :drone-id drone-id
                           :files files})))
        (log/info "Drone acquired file locks:" drone-id "(" (:files-claimed result) "files)")))

    ctx))

(defn phase:validate
  "Pre-execution file validation."
  [ctx task-spec]
  (let [{:keys [drone-id task-id]} ctx
        {:keys [files]} task-spec]

    (if (empty? files)
      ctx
      (let [pre-validation (validation/validate-files-pre files task-id)
            _ (when-not (validation/all-valid? pre-validation :pre)
                (let [invalid-files (->> pre-validation
                                         (filter (comp not :pre-valid? val))
                                         (map key))]
                  (log/warn {:event :drone/pre-validation-failed
                             :drone-id drone-id
                             :invalid-files invalid-files})))
            file-contents-before (into {}
                                       (for [f files]
                                         [f (try (slurp f) (catch Exception _ nil))]))]

        (-> ctx
            (domain/with-pre-validation pre-validation)
            (domain/with-file-contents-before file-contents-before))))))

(defn- prepare-execution-env
  "Prepare execution environment: augment task, create sandbox, security check."
  [ctx task-spec config]
  (let [{:keys [drone-id project-root]} ctx
        {:keys [task files options]} task-spec
        {:keys [model step-budget]} config
        cwd (or (:cwd task-spec) project-root)

        augmented-task (augment/augment-task task files
                                             (cond-> {:project-root cwd}
                                               (seq (:seeds options))        (assoc :seeds (:seeds options))
                                               (seq (:ctx-refs options))     (assoc :ctx-refs (:ctx-refs options))
                                               (seq (:kg-node-ids options))  (assoc :kg-node-ids (:kg-node-ids options))))

        effective-root (or cwd (diff/get-project-root))
        drone-sandbox (sandbox/create-sandbox (or files []) effective-root)]

    (when (seq (:rejected-files drone-sandbox))
      (log/error {:event :drone/path-escape-blocked
                  :drone-id drone-id
                  :rejected-files (:rejected-files drone-sandbox)})
      (throw (ex-info "File paths escape project directory"
                      {:error-type :path-escape
                       :rejected-files (:rejected-files drone-sandbox)})))

    (log/info "Drone sandbox created"
              {:drone-id drone-id
               :allowed-files (count (:allowed-files drone-sandbox))
               :blocked-tools (count (:blocked-tools drone-sandbox))
               :max-turns step-budget
               :model model})

    {:augmented-task augmented-task
     :drone-sandbox drone-sandbox
     :cwd cwd}))

(defn- emit-execution-started!
  "Emit :drone/started event and shout to parent ling."
  [ctx task-spec mode]
  (let [{:keys [drone-id task-id parent-id pre-validation]} ctx
        {:keys [task files]} task-spec
        mode-prefix (if (= mode :agentic) "Agentic drone" "Drone")]

    (ev/dispatch [:drone/started (cond-> {:drone-id drone-id
                                          :task-id task-id
                                          :parent-id parent-id
                                          :files files
                                          :task task
                                          :pre-validation (validation/summarize-validation (or pre-validation {}))}
                                   (= mode :agentic) (assoc :mode :agentic))])

    (when parent-id
      (hivemind/shout! parent-id :started
                       {:task (str mode-prefix ": " (subs task 0 (min 80 (count task))))
                        :message (format "%s %s working" mode-prefix drone-id)}))))

(defn- build-task-context
  "Build the task-context map for IDroneExecutionBackend dispatch."
  [ctx task-spec config augmented-task drone-sandbox]
  (let [{:keys [drone-id]} ctx
        {:keys [tools preset model step-budget]} config
        cwd (or (:cwd task-spec) (:project-root ctx))]
    {:task       augmented-task
     :model      model
     :preset     preset
     :tools      tools
     :max-steps  (or step-budget 20)
     :drone-id   drone-id
     :cwd        cwd
     :files      (vec (or (:files task-spec) []))
     :sandbox    {:allowed-files    (:allowed-files drone-sandbox)
                  :allowed-dirs     (:allowed-dirs drone-sandbox)
                  :blocked-patterns (mapv str (:blocked-patterns drone-sandbox))
                  :blocked-tools    (:blocked-tools drone-sandbox)}}))

(defn- resolve-backend
  "Resolve the IDroneExecutionBackend for execution."
  [task-context explicit-backend]
  (ext-router/load-available-backends!)
  (if explicit-backend
    (do
      (log/info {:event   :drone/explicit-backend-requested
                 :backend explicit-backend})
      (backend/resolve-backend (assoc task-context :backend explicit-backend)))
    (let [best (ext-router/best-backend)]
      (try
        (backend/resolve-backend (assoc task-context :backend best))
        (catch Exception e
          (log/warn {:event   :drone/preferred-backend-unavailable
                     :preferred best
                     :error   (ex-message e)
                     :message "Falling back to :legacy-loop backend"})
          (backend/resolve-backend (assoc task-context :backend :legacy-loop)))))))

(defn- dispatch-to-backend!
  "Dispatch task-context to a resolved IDroneExecutionBackend."
  [ctx task-context resolved-backend]
  (let [{:keys [drone-id]} ctx]
    (log/info {:event    :drone/backend-resolved
               :drone-id drone-id
               :backend  (backend/backend-type resolved-backend)})

    (binding [domain/*drone-kg-store* (:kg-store ctx)]
      (backend/execute-drone resolved-backend task-context))))

(defn phase:execute!
  "Execute task via delegate-fn wrapped as IDroneExecutionBackend."
  [ctx task-spec config delegate-fn]
  (let [{:keys [options]} task-spec
        {:keys [augmented-task drone-sandbox]} (prepare-execution-env ctx task-spec config)]

    (emit-execution-started! ctx task-spec :delegate)

    (let [task-context (-> (build-task-context ctx task-spec config augmented-task drone-sandbox)
                           (assoc :trace (:trace options true)))]

      (binding [domain/*drone-kg-store* (:kg-store ctx)]
        (delegate-fn task-context)))))

(defn phase:execute-agentic!
  "Execute task via IDroneExecutionBackend dispatch."
  [ctx task-spec config]
  (let [{:keys [options]} task-spec
        {:keys [augmented-task drone-sandbox]} (prepare-execution-env ctx task-spec config)]

    (emit-execution-started! ctx task-spec :agentic)

    (let [task-context (build-task-context ctx task-spec config
                                           augmented-task drone-sandbox)
          explicit-backend (:backend options)
          resolved-backend (resolve-backend task-context explicit-backend)]

      (dispatch-to-backend! ctx task-context resolved-backend))))

(defn phase:finalize!
  "Handle diffs, validate, record metrics, and build execution result."
  [ctx task-spec config raw-result diffs-before]
  (let [{:keys [drone-id task-id parent-id pre-validation file-contents-before]} ctx
        {:keys [task files options]} task-spec
        {:keys [task-type model]} config
        {:keys [wave-id skip-auto-apply]} options

        diffs-after (diff-mgmt/capture-diffs-before)
        new-diff-ids (diff-mgmt/get-new-diff-ids diffs-before diffs-after)

        _ (when (and (= :completed (:status raw-result))
                     (empty? new-diff-ids)
                     (seq files))
            (let [result-text (str (:result raw-result))
                  has-code-blocks? (re-find #"```" result-text)]
              (log/warn {:event :drone/zero-diff-completion
                         :drone-id drone-id
                         :files-expected (count files)
                         :has-code-blocks? (boolean has-code-blocks?)
                         :result-preview (subs result-text 0 (min 200 (count result-text)))
                         :message (str "Drone completed with 0 diffs but " (count files)
                                       " files expected. Model likely returned code as text "
                                       "instead of calling propose_diff."
                                       (when has-code-blocks?
                                         " CODE BLOCKS DETECTED in text response."))})))

        diff-results (diff-mgmt/handle-diff-results!
                      drone-id new-diff-ids
                      {:wave-id wave-id :skip-auto-apply skip-auto-apply})

        duration-ms (domain/elapsed-ms ctx)

        post-validation (when (and (seq files)
                                   (= :completed (:status raw-result))
                                   (not skip-auto-apply))
                          (validation/validate-files-post
                           file-contents-before
                           (or pre-validation {})
                           {:lint-level :error
                            :require-modification false}))

        validation-summary (validation/summarize-validation
                            (merge (or pre-validation {}) (or post-validation {})))]

    (when (and post-validation (not (validation/all-valid? post-validation :post)))
      (log/warn {:event :drone/post-validation-warnings
                 :drone-id drone-id
                 :summary validation-summary}))

    (when parent-id
      (if (= :completed (:status raw-result))
        (hivemind/shout! parent-id :completed
                         {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                          :message (format "Drone %s completed. Files: %s"
                                           drone-id
                                           (diff-mgmt/summarize-diff-results diff-results))})
        (hivemind/shout! parent-id :error
                         {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                          :message (format "Drone %s failed: %s" drone-id (:result raw-result))})))

    (if (= :completed (:status raw-result))
      (ev/dispatch [:drone/completed {:drone-id drone-id
                                      :task-id task-id
                                      :parent-id parent-id
                                      :files-modified (:applied diff-results)
                                      :files-failed (:failed diff-results)
                                      :proposed-diff-ids (:proposed diff-results)
                                      :duration-ms duration-ms
                                      :validation validation-summary}])
      (ev/dispatch [:drone/failed {:drone-id drone-id
                                   :task-id task-id
                                   :parent-id parent-id
                                   :error (str (:result raw-result))
                                   :error-type :execution
                                   :files files}]))

    (let [model-name (or (:model raw-result) model)
          tokens (:tokens raw-result)
          input-tokens (or (:input-tokens tokens)
                           (cost/count-tokens (:task task-spec)))
          output-tokens (or (:output-tokens tokens)
                            (cost/count-tokens (str (:result raw-result))))]

      (prom/record-drone-result! {:model model-name
                                  :task-type (name task-type)
                                  :success? (= :completed (:status raw-result))
                                  :duration-ms duration-ms
                                  :tokens tokens})

      (cost/track-drone-usage! drone-id
                               {:input-tokens input-tokens
                                :output-tokens output-tokens
                                :task-preview task
                                :wave-id wave-id})

      (routing/report-execution! task-type model-name raw-result
                                 {:duration-ms duration-ms
                                  :directory (:cwd task-spec)
                                  :agent-id drone-id}))

    (hivemind/record-ling-result! drone-id
                                  {:task task
                                   :files files
                                   :result raw-result
                                   :diff-results diff-results
                                   :validation validation-summary
                                   :parent-id parent-id
                                   :timestamp (System/currentTimeMillis)})

    (when-let [drone-store (:kg-store ctx)]
      (try
        (when (kg/store-set?)
          (let [merge-result (kg-factory/merge-drone-results! drone-store (kg/get-store))]
            (log/info {:event :drone/kg-merge
                       :drone-id drone-id
                       :edges-found (:edges-found merge-result)
                       :edges-merged (:edges-merged merge-result)})))
        (catch Exception e
          (log/warn {:event :drone/kg-merge-failed
                     :drone-id drone-id
                     :error (.getMessage e)}))))

    (domain/success-result ctx {:result raw-result
                                :diff-results diff-results
                                :validation validation-summary})))

(defn phase:cleanup!
  "Release resources: file claims, KG store, DataScript registration."
  [ctx task-spec]
  (let [{:keys [drone-id task-id]} ctx
        {:keys [files]} task-spec]
    (when (seq files)
      (coordinator/release-task-claims! task-id)
      (log/info "Drone released file locks:" drone-id))
    (when (:kg-store ctx)
      (try
        (kg-factory/close-drone-store! drone-id)
        (log/debug "Drone KG store closed:" drone-id)
        (catch Exception e
          (log/warn {:event :drone/kg-cleanup-failed
                     :drone-id drone-id
                     :error (.getMessage e)}))))
    (ds/remove-slave! drone-id)))

(defn phase:handle-error!
  "Handle execution error with proper logging and metrics."
  [ctx task-spec config exception]
  (let [{:keys [drone-id task-id parent-id]} ctx
        {:keys [files]} task-spec
        {:keys [model task-type]} config
        duration-ms (domain/elapsed-ms ctx)
        structured (errors/structure-error exception)]

    (log/error {:event :drone/error
                :error-type (:error-type structured)
                :drone-id drone-id
                :task-id task-id
                :parent-id parent-id
                :model model
                :files files
                :duration-ms duration-ms
                :message (:message structured)
                :stacktrace (subs (or (:stacktrace structured) "") 0
                                  (min 500 (count (or (:stacktrace structured) ""))))})

    (ev/dispatch [:drone/failed {:drone-id drone-id
                                 :task-id task-id
                                 :parent-id parent-id
                                 :error (:message structured)
                                 :error-type (:error-type structured)
                                 :stacktrace (:stacktrace structured)
                                 :files files}])

    (prom/record-drone-result! {:model model
                                :task-type (name task-type)
                                :success? false
                                :duration-ms duration-ms
                                :retry? (= :timeout (:error-type structured))
                                :retry-reason (:error-type structured)})

    (routing/report-execution! task-type model
                               {:status :failed :error (:message structured)}
                               {:duration-ms duration-ms
                                :directory (:cwd task-spec)
                                :agent-id drone-id})))

(defn run-execution!
  "Orchestrate drone execution through all phases with proper error handling and cleanup."
  [task-spec delegate-fn]
  (let [drone-id (domain/generate-drone-id)
        task-id (domain/generate-task-id drone-id)
        parent-id (or (:parent-id task-spec)
                      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        cwd (or (:cwd task-spec) (diff/get-project-root))

        drone-kg-store (try
                         (kg-factory/create-drone-store drone-id)
                         (catch Exception e
                           (log/warn {:event :drone/kg-store-creation-failed
                                      :drone-id drone-id
                                      :error (.getMessage e)})
                           nil))

        ctx (domain/->execution-context
             {:drone-id drone-id
              :task-id task-id
              :parent-id parent-id
              :project-root cwd
              :kg-store drone-kg-store})

        config (phase:prepare task-spec)
        ctx (assoc ctx :model (:model config))]

    (try
      (let [ctx (phase:register! ctx task-spec)
            diffs-before (diff-mgmt/capture-diffs-before)]

        (let [ctx (phase:validate ctx task-spec)]

          (try
            (let [raw-result (phase:execute! ctx task-spec config delegate-fn)]

              (phase:finalize! ctx task-spec config raw-result diffs-before))

            (catch Throwable e
              (phase:handle-error! ctx task-spec config e)
              (throw e)))))

      (finally
        (phase:cleanup! ctx task-spec)))))

(defn run-agentic-execution!
  "Orchestrate agentic drone execution through all phases."
  [task-spec]
  (let [drone-id (domain/generate-drone-id)
        task-id (domain/generate-task-id drone-id)
        parent-id (or (:parent-id task-spec)
                      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        cwd (or (:cwd task-spec) (diff/get-project-root))

        session-kg-store (try
                           (session-kg/create-session-kg! drone-id)
                           (catch Exception e
                             (log/warn {:event :drone/session-kg-creation-failed
                                        :drone-id drone-id
                                        :error (.getMessage e)})
                             (try
                               (kg-factory/create-drone-store drone-id)
                               (catch Exception e2
                                 (log/warn {:event :drone/kg-store-fallback-failed
                                            :drone-id drone-id
                                            :error (.getMessage e2)})
                                 nil))))

        ctx (domain/->execution-context
             {:drone-id drone-id
              :task-id task-id
              :parent-id parent-id
              :project-root cwd
              :kg-store session-kg-store})

        config (phase:prepare task-spec)
        ctx (assoc ctx :model (:model config))]

    (try
      (let [ctx (phase:register! ctx task-spec)
            diffs-before (diff-mgmt/capture-diffs-before)]

        (let [ctx (phase:validate ctx task-spec)]

          (try
            (let [raw-result (phase:execute-agentic! ctx task-spec config)]

              (phase:finalize! ctx task-spec config raw-result diffs-before))

            (catch Throwable e
              (phase:handle-error! ctx task-spec config e)
              (throw e)))))

      (finally
        (when session-kg-store
          (try
            (session-kg/close-session-kg! session-kg-store drone-id)
            (catch Exception e
              (log/warn {:event :drone/session-kg-cleanup-failed
                         :drone-id drone-id
                         :error (.getMessage e)}))))
        (phase:cleanup! ctx task-spec)))))
