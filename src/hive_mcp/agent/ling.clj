(ns hive-mcp.agent.ling
  "Ling agent implementation - Claude Code instances with tool chaining.

   Lings are persistent agents that:
   - Run as Claude Code subprocesses
   - Can chain multiple tool calls
   - Maintain session context
   - Coordinate via hivemind
   - Delegate to drones for file mutations

   Implements IAgent protocol for unified lifecycle management."
  (:require [hive-mcp.agent.protocol :refer [IAgent]]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.swarm.datascript.claims :as ds-claims]
            [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.emacsclient :as ec]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Ling Record - IAgent Implementation
;;; =============================================================================

;; Forward declare for use in spawn! method (creates new ling for task dispatch)
(declare ->ling)

(defrecord Ling [id cwd presets project-id]
  IAgent

  (spawn! [this opts]
    "Spawn a ling via elisp hive-mcp-swarm-api-spawn.

     Uses emacsclient to invoke the Emacs-side spawn function which:
     - Creates a new Claude Code process
     - Registers it in the swarm
     - Returns the slave-id

     Options:
       :task      - Initial task to dispatch (optional)
       :presets   - Override presets (optional)
       :depth     - Hierarchy depth (default: 1)
       :parent    - Parent slave-id (optional)"
    (let [{:keys [task depth parent kanban-task-id terminal]
           :or {depth 1}} opts
          ;; FIX: Use format-elisp-list to produce proper elisp list syntax '("a" "b")
          ;; instead of pr-str which produces Clojure vector syntax ["a" "b"]
          preset-list (or (:presets opts) presets)
          preset-str (or (swarm-core/format-elisp-list preset-list pr-str) "nil")
          ;; Elisp API: (name presets &optional cwd terminal kanban-task-id context-file)
          ;; Note: depth and parent are tracked in DataScript, not passed to elisp
          ;; Note: preset-str already includes quote prefix from format-elisp-list
          elisp-code (format "(hive-mcp-swarm-api-spawn \"%s\" %s \"%s\" %s %s nil)"
                             id
                             preset-str
                             (or cwd "")
                             (if terminal (format "\"%s\"" terminal) "nil")
                             (if kanban-task-id (format "\"%s\"" kanban-task-id) "nil"))
          result (ec/eval-elisp-with-timeout elisp-code 10000)]
      (if (:success result)
        (let [;; Elisp returns the actual slave-id (may differ from requested id)
              elisp-slave-id (str/trim (or (:result result) id))]
          (log/info "Ling spawned via elisp" {:requested-id id
                                              :elisp-slave-id elisp-slave-id})
          ;; Register with ELISP slave-id for proper dispatch routing
          (ds-lings/add-slave! elisp-slave-id {:status :idle
                                               :depth depth
                                               :parent parent
                                               :presets (or (:presets opts) presets)
                                               :cwd cwd
                                               :project-id project-id
                                               :kanban-task-id kanban-task-id
                                               :requested-id id})
          ;; Dispatch initial task if provided (uses elisp-slave-id)
          (when task
            ;; Create a new ling instance with the correct elisp ID for dispatch
            (let [elisp-ling (->ling elisp-slave-id {:cwd cwd
                                                     :presets (or (:presets opts) presets)
                                                     :project-id project-id})]
              (.dispatch! elisp-ling {:task task})))
          elisp-slave-id)
        (do
          (log/error "Failed to spawn ling via elisp" {:id id :error (:error result)})
          (throw (ex-info "Failed to spawn ling"
                          {:id id :error (:error result)}))))))

  (dispatch! [this task-opts]
    "Dispatch a task to this ling via elisp.

     Sends the prompt to the Claude Code instance via emacsclient.
     Also handles:
     - Task queuing when ling is busy
     - Conflict detection for file claims
     - DataScript tracking

     Options:
       :task       - Task description (required)
       :files      - Files to claim (optional)
       :priority   - Task priority (default: :normal)
       :timeout-ms - Elisp dispatch timeout (default: 60000)"
    (let [{:keys [task files priority timeout-ms]
           :or {priority :normal
                timeout-ms 60000}} task-opts
          task-id (str "task-" (System/currentTimeMillis) "-" (subs id 0 8))]
      ;; Update status to :working
      (ds-lings/update-slave! id {:slave/status :working})
      ;; Add task to DataScript
      (ds-lings/add-task! task-id id {:status :dispatched
                                      :prompt task
                                      :files files})
      ;; Claim files if specified
      (when (seq files)
        (.claim-files! this files task-id))

      ;; Dispatch to ling via elisp (actually sends prompt to Claude Code)
      (let [escaped-prompt (-> task
                               (str/replace "\\" "\\\\")
                               (str/replace "\"" "\\\"")
                               (str/replace "\n" "\\n"))
            elisp-code (format "(hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %d)"
                               id
                               escaped-prompt
                               timeout-ms)
            result (ec/eval-elisp-with-timeout elisp-code timeout-ms)]
        (if (:success result)
          (do
            (log/info "Task dispatched to ling via elisp"
                      {:ling-id id :task-id task-id :files files})
            task-id)
          (do
            (log/error "Failed to dispatch to ling via elisp"
                       {:ling-id id :task-id task-id :error (:error result)})
            ;; Update task status to failed
            (ds-lings/update-task! task-id {:status :failed
                                            :error (:error result)})
            (throw (ex-info "Failed to dispatch to ling"
                            {:ling-id id
                             :task-id task-id
                             :error (:error result)})))))))

  (status [this]
    "Get current ling status from DataScript with elisp fallback.

     Returns merged status from:
     1. DataScript slave entity (primary source)
     2. Elisp process status (fallback for liveness)"
    (let [ds-status (ds-queries/get-slave id)
          ;; Fallback to elisp if not in DataScript or for liveness check
          elisp-result (when (or (nil? ds-status)
                                 (= :unknown (:slave/status ds-status)))
                         (ec/eval-elisp-with-timeout
                          (format "(hive-mcp-swarm-get-slave-status \"%s\")" id)
                          3000))]
      (if ds-status
        ;; Merge elisp liveness into DS status
        (cond-> ds-status
          (and (:success elisp-result)
               (not= (:result elisp-result) "nil"))
          (assoc :elisp-alive? true))
        ;; Fallback to elisp-only status
        (when (:success elisp-result)
          {:slave/id id
           :slave/status (if (= (:result elisp-result) "nil")
                           :dead
                           :unknown)
           :elisp-raw (:result elisp-result)}))))

  (kill! [this]
    "Terminate the ling and release resources.

     Checks ownership and critical operations before killing:
     1. Verify no critical ops in progress (wrap, commit, dispatch)
     2. Release all file claims
     3. Kill elisp process
     4. Remove from DataScript"
    (let [{:keys [can-kill? blocking-ops]} (ds-lings/can-kill? id)]
      (if can-kill?
        (do
          ;; Release claims first
          (.release-claims! this)
          ;; Kill elisp process
          (let [elisp-result (ec/eval-elisp-with-timeout
                              (format "(hive-mcp-swarm-slaves-kill \"%s\")" id)
                              5000)]
            (when-not (:success elisp-result)
              (log/warn "Elisp kill may have failed" {:id id :error (:error elisp-result)})))
          ;; Remove from DataScript
          (ds-lings/remove-slave! id)
          (log/info "Ling killed" {:id id})
          {:killed? true :id id})
        (do
          (log/warn "Cannot kill ling - critical ops in progress"
                    {:id id :blocking-ops blocking-ops})
          {:killed? false
           :reason :critical-ops-blocking
           :blocking-ops blocking-ops}))))

  (agent-type [_]
    :ling)

  (can-chain-tools? [_]
    "Lings can chain multiple tool calls in a single turn."
    true)

  (claims [this]
    "Get list of files currently claimed by this ling.

     Queries DataScript for all claims where :claim/slave = this ling."
    (let [all-claims (ds-queries/get-all-claims)]
      (->> all-claims
           (filter #(= id (:slave-id %)))
           (map :file)
           vec)))

  (claim-files! [this files task-id]
    "Claim files for exclusive access during task.

     Claims prevent other agents from modifying the same files.
     Uses DataScript claims with TTL for stale detection."
    (when (seq files)
      (doseq [f files]
        ;; Check for conflicts first
        (let [{:keys [conflict? held-by]} (ds-queries/has-conflict? f id)]
          (if conflict?
            (do
              (log/warn "File already claimed by another agent"
                        {:file f :held-by held-by :requesting id})
              ;; Add to wait queue instead of claiming
              (ds-claims/add-to-wait-queue! id f))
            ;; No conflict - claim the file
            (ds-lings/claim-file! f id task-id))))
      (log/info "Files claimed" {:ling-id id :count (count files)})))

  (release-claims! [this]
    "Release all file claims held by this ling."
    (let [released-count (ds-lings/release-claims-for-slave! id)]
      (log/info "Released claims" {:ling-id id :count released-count})
      released-count))

  (upgrade! [_]
    "No-op for lings - they already have full capabilities."
    nil))

;;; =============================================================================
;;; Factory Functions
;;; =============================================================================

(defn ->ling
  "Create a new Ling agent instance.

   Arguments:
     id   - Unique identifier for this ling
     opts - Map with optional keys:
            :cwd        - Working directory
            :presets    - Collection of preset names
            :project-id - Project ID for scoping

   Returns:
     Ling record implementing IAgent protocol

   Example:
     (->ling \"ling-123\" {:cwd \"/project\"
                          :presets [\"coordinator\"]
                          :project-id \"hive-mcp\"})"
  [id opts]
  (map->Ling {:id id
              :cwd (:cwd opts)
              :presets (:presets opts [])
              :project-id (:project-id opts)}))

(defn create-ling!
  "Create and spawn a new ling agent.

   Convenience function that creates the Ling record and spawns it.

   Arguments:
     id   - Unique identifier
     opts - Spawn options (see spawn! and ->ling)

   Returns:
     The ling ID on success, throws on failure"
  [id opts]
  (let [ling (->ling id opts)]
    (.spawn! ling opts)))

;;; =============================================================================
;;; Ling Query Functions
;;; =============================================================================

(defn get-ling
  "Get a ling by ID as a Ling record.

   Reconstitutes the Ling record from DataScript state.

   Returns:
     Ling record or nil if not found"
  [id]
  (when-let [slave (ds-queries/get-slave id)]
    (->ling id {:cwd (:slave/cwd slave)
                :presets (:slave/presets slave)
                :project-id (:slave/project-id slave)})))

(defn list-lings
  "List all lings, optionally filtered by project-id.

   Arguments:
     project-id - Optional project ID filter

   Returns:
     Seq of Ling records"
  [& [project-id]]
  (let [slaves (if project-id
                 (ds-queries/get-slaves-by-project project-id)
                 (ds-queries/get-all-slaves))]
    (->> slaves
         ;; Filter to depth 1 (lings, not drones)
         (filter #(= 1 (:slave/depth %)))
         (map (fn [s]
                (->ling (:slave/id s)
                        {:cwd (:slave/cwd s)
                         :presets (:slave/presets s)
                         :project-id (:slave/project-id s)}))))))

(defn get-ling-for-task
  "Get the ling assigned to a kanban task.

   Arguments:
     kanban-task-id - Kanban task ID

   Returns:
     Ling record or nil"
  [kanban-task-id]
  (when-let [slave (ds-queries/get-slave-by-kanban-task kanban-task-id)]
    (->ling (:slave/id slave)
            {:cwd (:slave/cwd slave)
             :presets (:slave/presets slave)
             :project-id (:slave/project-id slave)})))

;;; =============================================================================
;;; Critical Operations Guard (Delegating to ds-lings)
;;; =============================================================================

(defn with-critical-op
  "Execute body while holding a critical operation guard.

   Prevents swarm_kill from terminating the ling during critical ops.
   Wraps ds-lings/with-critical-op.

   Usage:
     (with-critical-op ling-id :wrap
       (do-wrap-stuff))"
  [ling-id op-type body-fn]
  (ds-lings/with-critical-op ling-id op-type
    (body-fn)))

(comment
  ;; Usage examples

  ;; Create a ling
  (def my-ling (->ling "ling-001" {:cwd "/home/user/project"
                                   :presets ["coordinator"]
                                   :project-id "hive-mcp"}))

  ;; Spawn it (requires Emacs running)
  ;; (.spawn! my-ling {:task "Explore the codebase"})

  ;; Check status
  ;; (.status my-ling)

  ;; Dispatch a task
  ;; (.dispatch! my-ling {:task "Find all test files" :files ["test/"]})

  ;; Get claims
  ;; (.claims my-ling)

  ;; Kill when done
  ;; (.kill! my-ling)

  ;; Query functions
  ;; (get-ling "ling-001")
  ;; (list-lings "hive-mcp")
  )
