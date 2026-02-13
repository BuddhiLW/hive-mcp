(ns hive-mcp.tools.crystal
  "Crystal/wrap workflow tools for session crystallization.

   Handles progressive crystallization of session data into long-term memory.

   DDD: Application service layer exposing crystal domain functionality.

   Result DSL: Internal logic returns Result maps ({:ok val} or {:error category}).
   Single try-result boundary at each handler level. Zero nested try-catch."
  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Result DSL Helpers (boundary pattern)
;;; =============================================================================

(defn- try-result
  "Execute thunk f returning Result; catch unexpected exceptions as error Result.
   Unlike try-effect, expects f to return a Result map directly."
  [category f]
  (try
    (f)
    (catch Exception e
      (log/error e (str (name category) " failed"))
      (result/err category {:message (.getMessage e)}))))

(defn- result->mcp
  "Convert Result to MCP response.
   {:ok data} -> (mcp-json data), {:error ...} -> (mcp-error message)."
  [r]
  (if (result/ok? r)
    (mcp-json (:ok r))
    (mcp-error (or (:message r) (:cause r) (str (:error r))))))

;;; =============================================================================
;;; Shared Helpers (pure calculations)
;;; =============================================================================

(defn- hive-mcp-el-available?
  "Check if hive-mcp.el is loaded in Emacs."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp)")]
    (and success (= result "t"))))

(defn- extract-source-ids
  "Extract memory entry IDs from harvested session data.

   Sources include:
   - progress-notes: ephemeral notes created during session
   - completed-tasks: kanban tasks that moved to DONE
   - memory-ids-created: IDs tracked by crystal/recall created-ids-buffer (step 2/4)

   Returns a vector of non-nil IDs suitable for KG edge creation.
   Used as :source-ids-fn in the wrap FSM."
  [{:keys [progress-notes completed-tasks memory-ids-created]}]
  (->> (concat (keep :id progress-notes)
               (keep :id completed-tasks)
               (keep :id memory-ids-created))
       (filter some?)
       (distinct)
       (vec)))

(defn- resolve-agent
  "Resolve effective agent-id with fallback chain. Logs warnings for fallbacks."
  [{:keys [agent_id]}]
  (let [ctx-id (ctx/current-agent-id)
        env-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")]
    (when (and (nil? agent_id) (nil? ctx-id))
      (log/warn "wrap-crystallize: agent_id not passed explicitly."
                (if env-id
                  (str "Falling back to env var " env-id)
                  "Defaulting to 'coordinator'. Lings should pass agent_id explicitly.")))
    (or agent_id ctx-id env-id "coordinator")))

;;; =============================================================================
;;; Side-Effect Helpers (isolated actions)
;;; =============================================================================

(defn- fetch-elisp-data
  "Fetch wrap data from elisp side. Returns data map or nil."
  [effective-dir]
  (when (hive-mcp-el-available?)
    (let [elisp-call (if effective-dir
                       (format "(json-encode (hive-mcp-api-wrap-gather \"%s\"))" effective-dir)
                       "(json-encode (hive-mcp-api-wrap-gather))")
          {:keys [success result]} (ec/eval-elisp elisp-call)]
      (when success
        (try (json/read-str result :key-fn keyword)
             (catch Exception _ nil))))))

;;; =============================================================================
;;; Auto-KG Edge Creation — FOSS default with extension override
;;; =============================================================================

(def ^:private ^:const max-derived-from-edges
  "Maximum :derived-from edges per crystal summary.
   Prevents quadratic explosion on large sessions."
  50)

(defn- foss-build-crystal-edges
  "FOSS default for :ck/a — creates :derived-from and :co-accessed KG edges.

   Creates basic KG edges linking a crystal summary to its source entries.
   This is the L1/L2 (AGPL) implementation using existing kg-edges CRUD.
   The proprietary extension may add scoring, similarity, or advanced logic.

   Input:  {:summary-id str, :harvested map, :project-id str, :agent-id str}
   Output: {:derived-from {:created-count N, :edge-ids [...]},
            :co-accessed  {:count M},
            :total-edges  T,
            :capped?      bool}"
  [{:keys [summary-id harvested project-id agent-id]}]
  (let [source-ids (extract-source-ids harvested)
        capped?    (> (count source-ids) max-derived-from-edges)
        limited-ids (if capped?
                      (vec (take max-derived-from-edges source-ids))
                      source-ids)
        ;; Create :derived-from edges (summary → each source)
        derived-result
        (when (and summary-id (seq limited-ids))
          (reduce
           (fn [acc source-id]
             (try
               (let [edge-id (kg-edges/add-edge!
                              {:from        summary-id
                               :to          source-id
                               :relation    :derived-from
                               :scope       project-id
                               :created-by  agent-id
                               :source-type :automated})]
                 (-> acc
                     (update :created-count inc)
                     (update :edge-ids conj edge-id)))
               (catch Exception e
                 (log/debug "Failed to create derived-from edge to" source-id
                            ":" (.getMessage e))
                 acc)))
           {:created-count 0 :edge-ids []}
           limited-ids))

        ;; Create :co-accessed edges between source entries
        co-count (when (>= (count limited-ids) 2)
                   (kg-edges/record-co-access!
                    limited-ids
                    {:scope      project-id
                     :created-by agent-id}))

        derived-count (or (:created-count derived-result) 0)
        co-count-val  (or co-count 0)]
    {:derived-from derived-result
     :co-accessed  {:count co-count-val}
     :total-edges  (+ derived-count co-count-val)
     :capped?      capped?}))

(defn- ck-a
  "Delegates to extension if available, falls back to FOSS implementation.
   OCP: positional core args + & opts for extension-specific keys."
  [summary-id harvested project-id agent-id & {:as opts}]
  (let [args (merge {:summary-id summary-id
                     :harvested  harvested
                     :project-id project-id
                     :agent-id   agent-id}
                    opts)]
    (if-let [f (ext/get-extension :ck/a)]
      (f args)
      (do
        (log/info "Using FOSS crystal edge builder for" summary-id)
        (foss-build-crystal-edges args)))))

;;; =============================================================================
;;; Harvest / Crystallize Helpers
;;; =============================================================================

(defn- harvest
  "Harvest session data. Returns Result."
  [effective-dir]
  (result/try-effect* :crystal/harvest-failed
                      (crystal-hooks/harvest-all {:directory effective-dir})))

(defn- crystallize-session-result
  "Run crystallize-session, handling domain-level :error. Returns Result."
  [harvested project-id]
  (try
    (let [r (crystal-hooks/crystallize-session harvested)]
      (if (:error r)
        (result/err :crystal/crystallize-failed
                    {:message (:error r) :project-id project-id})
        (result/ok r)))
    (catch Exception e
      (result/err :crystal/crystallize-failed
                  {:message (.getMessage e)}))))

(defn- emit-wrap-notify!
  "Best-effort event dispatch for wrap notification."
  [agent-id cr-result project-id stats]
  (try
    (ev/dispatch [:crystal/wrap-notify
                  {:agent-id agent-id
                   :session-id (:session cr-result)
                   :project-id project-id
                   :created-ids (some-> (:summary-id cr-result) vector)
                   :stats stats}])
    (log/info "wrap-crystallize: emitted wrap_notify for" agent-id "project:" project-id
              "summary-id:" (:summary-id cr-result))
    (catch Exception e
      (log/warn "wrap-crystallize: failed to emit wrap_notify:" (.getMessage e)))))

;;; =============================================================================
;;; Pure Logic Functions (Result-returning)
;;; =============================================================================

(defn- gather*
  "Gather session data. Returns Result with combined crystal + elisp data."
  [{:keys [directory]}]
  (let [effective-dir (or directory (ctx/current-directory))]
    (log/info "wrap-gather with crystal harvesting" (str "directory:" effective-dir))
    (result/let-ok [harvested (harvest effective-dir)]
                   (let [elisp-result (fetch-elisp-data effective-dir)]
                     (result/ok {:crystal harvested
                                 :elisp elisp-result
                                 :session (:session harvested)
                                 :summary (merge (:summary harvested)
                                                 {:has-elisp-data (some? elisp-result)})})))))

(defn- crystallize*
  "Crystallize session data into long-term memory. Returns Result."
  [{:keys [directory] :as params}]
  (let [effective-dir (or directory (ctx/current-directory))
        effective-agent (resolve-agent params)
        project-id (scope/get-current-project-id effective-dir)]
    (log/info "wrap-crystallize" (str "agent-id:" effective-agent " directory:" effective-dir))
    (result/let-ok [harvested (harvest effective-dir)
                    cr-result (crystallize-session-result harvested project-id)]
                   (let [safe-stats (or (when (map? (:stats cr-result)) (:stats cr-result)) {})
                         summary-id (:summary-id cr-result)
                         ;; Auto-KG edges — delegates to extension
                         kg-result (when summary-id
                                     (ck-a summary-id harvested project-id effective-agent))]
                     (emit-wrap-notify! effective-agent cr-result project-id safe-stats)
                     (result/ok (cond-> (assoc cr-result :project-id project-id)
                                  kg-result (assoc :kg-edges kg-result)))))))

(defn- permeate*
  "Process wrap queue entries. Returns Result."
  [{:keys [directory include_children]}]
  (let [effective-dir (or directory (ctx/current-directory))
        project-id (scope/get-current-project-id effective-dir)
        include-children? (not (false? include_children))]
    (log/info "permeate-crystals: processing wrap queue for project:" project-id
              "include-children:" include-children?)
    (let [queue-items (if include-children?
                        (ds/get-unprocessed-wraps-for-hierarchy project-id)
                        (ds/get-unprocessed-wraps-for-project project-id))
          agent-ids (mapv :wrap-queue/agent-id queue-items)
          project-ids (distinct (map :wrap-queue/project-id queue-items))]
      (doseq [item queue-items]
        (ds/mark-wrap-processed! (:wrap-queue/id item)))
      (result/ok {:processed (count queue-items)
                  :agent-ids agent-ids
                  :project-id project-id
                  :project-ids-matched (vec project-ids)
                  :include-children include-children?
                  :status "ok"}))))

;;; =============================================================================
;;; Public Handlers (thin boundary: try-result + result->mcp)
;;; =============================================================================

(defn handle-wrap-gather
  "Gather session data for wrap workflow without storing.

   Uses progressive crystallization to harvest:
   - Session progress notes (from kanban completions)
   - Completed tasks (ephemeral notes tagged session-progress)
   - Git commits since session start
   - Recall patterns (for smart promotion)

   Params:
   - directory: Working directory for git operations.

   Returns gathered data for confirmation before crystallization."
  [params]
  (result->mcp (try-result :crystal/gather-failed #(gather* params))))

(defn handle-wrap-crystallize
  "Crystallize session data into long-term memory.

   Takes harvested data (from wrap-gather) and:
   1. Creates session summary (short-term duration)
   2. Promotes entries that meet score threshold
   3. Flushes recall buffer
   4. Emits wrap_notify event for hivemind permeation

   Params:
   - agent_id (REQUIRED for lings): Ling's slave-id.
   - directory: Working directory for project scoping.

   Call after wrap-gather when ready to persist."
  [params]
  (result->mcp (try-result :crystal/crystallize-failed #(crystallize* params))))

(defn handle-permeate-crystals
  "Process wrap queue entries from ling sessions.

   Coordinator calls this to process wrap notifications from lings:
   1. Gets unprocessed wraps from DataScript queue (filtered by project)
   2. Marks each as processed
   3. Returns stats about what was permeated

   Params:
   - directory: Working directory for project scoping.
   - include_children: When true (default), includes wraps from child projects."
  [params]
  (result->mcp (try-result :crystal/permeate-failed #(permeate* params))))

;; Tool definitions
(def tools
  [{:name "wrap_gather"
    :description "Gather session data for wrap workflow. Returns recent notes, git commits, kanban activity without storing. Use before wrap to preview/confirm data."
    :inputSchema {:type "object"
                  :properties {:directory {:type "string"
                                           :description "Working directory for git operations. Pass your cwd to ensure git context comes from your project, not the MCP server's directory."}}
                  :required []}
    :handler handle-wrap-gather}

   {:name "wrap_crystallize"
    :description "Crystallize session data into long-term memory. Creates session summary, promotes entries meeting score threshold, and flushes recall buffer. Call after wrap_gather to persist. CRITICAL FOR LINGS: You MUST pass your CLAUDE_SWARM_SLAVE_ID as agent_id parameter - the MCP server runs in coordinator's JVM so env vars won't work. Without explicit agent_id, wrap_notify will show 'coordinator' instead of your ling ID."
    :inputSchema {:type "object"
                  :properties {:agent_id {:type "string"
                                          :description "REQUIRED FOR LINGS: Your CLAUDE_SWARM_SLAVE_ID. Without this, wrap attribution shows 'coordinator' instead of your ling ID. MCP server runs in coordinator's JVM - System.getenv reads coordinator's env, not yours."}
                               :directory {:type "string"
                                           :description "Working directory for project scoping. Pass your cwd to ensure wrap is tagged with correct project-id for scoped permeation."}}
                  :required []}
    :handler handle-wrap-crystallize}

   {:name "mcp_permeate_crystals"
    :description "Process wrap queue from ling sessions. Coordinator calls this to permeate ling session data. Returns stats about processed wraps including agent IDs and counts. IMPORTANT: Pass directory to scope permeation to current project only."
    :inputSchema {:type "object"
                  :properties {:directory {:type "string"
                                           :description "Working directory for project scoping. Pass your cwd to ensure only wraps from the current project are permeated, preventing cross-project contamination."}
                               :include_children {:type "boolean"
                                                  :description "When true (default), includes wraps from child projects using hierarchical prefix matching. E.g., 'myproject' also matches 'myproject:submodule'. Set to false for exact project matching only."}}
                  :required []}
    :handler handle-permeate-crystals}])
