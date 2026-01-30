(ns hive-mcp.tools.swarm.lifecycle
  "Swarm lifecycle handlers - spawn and kill operations.

   ADR-001 Phase 2: Registration handled by event-driven sync.
   The slave-spawned event from elisp triggers register-ling! via
   channel subscription (see registry/start-registry-sync!).

   ADR-003: Kill guard prevents termination during critical operations
   (wrap, commit, dispatch). Uses datascript critical-ops tracking.

   Project-scoped kill: When slave_id='all' and directory is provided,
   only kills lings belonging to that project (not all lings globally).

   SOLID: SRP - Single responsibility for spawn/kill operations.
   CLARITY: Y - Yield safe failure with timeout handling."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.catchup :as catchup]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.telemetry.prometheus :as prom]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Spawn Context Injection
;; ============================================================

(defn- write-spawn-context-file
  "Write spawn context to a temporary file for elisp consumption.
   Returns absolute file path, or nil on failure.

   Architecture > LLM behavior: context injected at spawn, not
   left to LLM /catchup compliance."
  [context-str]
  (when context-str
    (try
      (let [tmp-file (java.io.File/createTempFile "hive-spawn-ctx-" ".md")]
        (spit tmp-file context-str)
        (.getAbsolutePath tmp-file))
      (catch Exception e
        (log/warn "Failed to write spawn context file:" (.getMessage e))
        nil))))

;; ============================================================
;; Spawn Handler
;; ============================================================

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance.
   Uses timeout to prevent MCP blocking.

   ADR-001 Phase 2: Registration now handled by event-driven sync.
   The slave-spawned event from elisp triggers register-ling! via
   channel subscription (see registry/start-registry-sync!).

   Architecture > LLM behavior: Generates lightweight catchup context
   and injects it at spawn time via temp file. Lings receive axioms,
   priority conventions, and active decisions without needing /catchup.

   Parameters:
   - name: Name for the slave (required)
   - presets: List of preset names to apply
   - cwd: Working directory for the slave
   - role: Predefined role
   - terminal: Terminal type (vterm/eat)
   - kanban_task_id: Optional kanban task ID to link this ling with

   CLARITY: I - Inputs validated (name required)
   CLARITY: C - Composes spawn-context injection without modifying catchup
   SOLID: SRP - Only handles spawn, not registration"
  [{:keys [name presets cwd _role terminal kanban_task_id]}]
  (core/with-swarm
    (let [;; CLARITY-I: Validate cwd is a proper string path (fix for stringp bug)
          ;; MCP may pass non-string values - coerce to nil if invalid
          validated-cwd (when (and cwd (string? cwd) (not (str/blank? cwd)))
                          cwd)
          ;; Context injection re-enabled with validated directory
          spawn-ctx (try
                      (catchup/spawn-context validated-cwd)
                      (catch Exception e
                        (log/warn "spawn-context generation failed (non-fatal):" (.getMessage e))
                        nil))
          ctx-file (write-spawn-context-file spawn-ctx)
          presets-str (when (seq presets)
                        (format "'(%s)" (str/join " " (map #(format "\"%s\"" %) presets))))
          ;; Include kanban_task_id and context-file in elisp call
          elisp (format "(json-encode (hive-mcp-swarm-api-spawn \"%s\" %s %s %s %s %s))"
                        (v/escape-elisp-string (or name "slave"))
                        (or presets-str "nil")
                        (if validated-cwd (format "\"%s\"" (v/escape-elisp-string validated-cwd)) "nil")
                        (if terminal (format "\"%s\"" terminal) "nil")
                        (if kanban_task_id (format "\"%s\"" (v/escape-elisp-string kanban_task_id)) "nil")
                        (if ctx-file (format "\"%s\"" (v/escape-elisp-string ctx-file)) "nil"))
          ;; Use 10s timeout for spawn as it may take longer
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
      (when spawn-ctx
        (log/info "spawn-context injected for ling" name
                  {:chars (count spawn-ctx) :file ctx-file}))
      (cond
        timed-out
        (core/mcp-timeout-error "Spawn operation" :extra-data {:slave_name name})

        success
        ;; ADR-001 Phase 2: No manual registration here.
        ;; Registration is handled by event-driven sync when elisp emits
        ;; slave-spawned event via channel (see registry/start-registry-sync!).
        (do
          ;; CLARITY-T: Update Prometheus gauge (estimate +1 until registry sync)
          (let [current-count (count (registry/get-available-lings))]
            (prom/set-lings-active! (inc current-count)))
          (core/mcp-success result))

        :else
        (core/mcp-error (str "Error: " error))))))

;; ============================================================
;; Kill Handler
;; ============================================================

(defn- format-blocking-ops
  "Format blocking operations for error message."
  [ops]
  (str/join ", " (map name ops)))

;; ============================================================
;; Ownership Validation (Cross-Project Kill Prevention)
;; ============================================================

(defn- can-kill-ownership?
  "Check if caller can kill target ling based on project ownership.

   Rules:
   - Force flag bypasses ownership check (explicit cross-project kill)
   - Caller without project context (coordinator) can kill anything
   - Legacy lings without project-id can be killed by anyone
   - Otherwise, caller's project-id must match target's project-id

   Arguments:
     caller-project-id    - Project ID of the caller (nil for coordinator)
     target-slave-id      - ID of the slave to check
     force-cross-project? - If true, bypass ownership check (default: false)

   Returns:
     {:can-kill? bool :reason string :caller-project string :target-project string}"
  ([caller-project-id target-slave-id]
   (can-kill-ownership? caller-project-id target-slave-id false))
  ([caller-project-id target-slave-id force-cross-project?]
   (let [target-slave (ds/get-slave target-slave-id)
         target-project-id (:slave/project-id target-slave)]
     (cond
       ;; Force flag explicitly bypasses ownership check
       force-cross-project?
       {:can-kill? true :reason "force-cross-project"}

       ;; Caller without project context (coordinator) can kill anything
       (nil? caller-project-id)
       {:can-kill? true :reason "coordinator-context"}

       ;; Legacy lings without project-id can be killed by anyone
       (nil? target-project-id)
       {:can-kill? true :reason "legacy-ling"}

       ;; Same project - allowed
       (= caller-project-id target-project-id)
       {:can-kill? true :reason "same-project"}

       ;; Cross-project kill attempt - denied
       :else
       {:can-kill? false
        :reason "cross-project"
        :caller-project caller-project-id
        :target-project target-project-id}))))

(defn- check-any-critical-ops
  "Check if any slaves have critical operations blocking kill-all.
   Returns {:can-kill? bool :blocked-slaves [{:id :ops}...]}"
  ([]
   (check-any-critical-ops nil))
  ([slave-ids]
   (let [ids-to-check (or slave-ids (keys (registry/get-available-lings)))
         blocked (->> ids-to-check
                      (map (fn [slave-id]
                             (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
                               (when-not can-kill?
                                 {:id slave-id :ops blocking-ops}))))
                      (filter some?))]
     (if (seq blocked)
       {:can-kill? false :blocked-slaves blocked}
       {:can-kill? true :blocked-slaves []}))))

(defn- kill-single-slave!
  "Kill a single slave by ID. Returns MCP response.

   Checks both critical ops guard AND project ownership.
   Pass caller-project-id as nil for coordinator (no project context).
   Pass force-cross-project? as true to bypass ownership check.

   CLARITY: SRP - Extracted from handle-swarm-kill for reuse.
   CLARITY: I - Inputs guarded (ownership + critical ops)"
  ([slave_id]
   (kill-single-slave! slave_id nil false))
  ([slave_id caller-project-id]
   (kill-single-slave! slave_id caller-project-id false))
  ([slave_id caller-project-id force-cross-project?]
   ;; First check ownership (cross-project kill prevention)
   (let [{:keys [can-kill? reason caller-project target-project]} (can-kill-ownership? caller-project-id slave_id force-cross-project?)]
     (if-not can-kill?
       ;; Blocked by ownership mismatch
       {:success false
        :error (format "cross-project kill denied: caller=%s target=%s" caller-project target-project)
        :slave-id slave_id
        :reason reason}
       ;; Ownership OK, now check critical ops
       (let [{crit-can-kill? :can-kill? :keys [blocking-ops]} (ds/can-kill? slave_id)]
         (if crit-can-kill?
           ;; Safe to kill
           (let [elisp (format "(json-encode (hive-mcp-swarm-api-kill \"%s\"))"
                               (v/escape-elisp-string slave_id))
                 {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
             (cond
               timed-out
               {:success false :error "timeout" :slave-id slave_id}

               success
               (do
                 ;; Clear from both registries: DataScript + hivemind agent-registry
                 (hivemind/clear-agent! slave_id)
                 {:success true :result result :slave-id slave_id})

               :else
               {:success false :error error :slave-id slave_id}))
           ;; Blocked by critical ops
           {:success false
            :error (format "critical ops: %s" (format-blocking-ops blocking-ops))
            :slave-id slave_id}))))))

(defn handle-swarm-kill
  "Kill a slave or all slaves.
   Uses short timeout (3s) as kill should be fast.
   Removes killed lings from registry.

   KILL GUARD (ADR-003): Blocks kill during critical operations.
   Critical ops: :wrap (session crystallization), :commit (git),
   :dispatch (task dispatch in progress).

   OWNERSHIP GUARD: Prevents cross-project kills.
   - Caller with directory context can only kill lings from same project
   - Caller without directory (coordinator) can kill any ling
   - Legacy lings without project-id can be killed by anyone
   - force_cross_project: true bypasses ownership check (explicit override)

   PROJECT-SCOPED KILL: When slave_id='all' and directory is provided,
   only kills lings belonging to that project (not all lings globally).

   Parameters:
   - slave_id: ID of slave to kill, or \"all\" to kill all
   - directory: Working directory to scope kill (optional).
                For single kills, enforces ownership check.
                For 'all', filters to project-owned lings only.
   - force_cross_project: If true, allows killing lings from other projects.
                          Use with caution - bypasses security guard.

   CLARITY: Y - Yield safe failure with timeout handling
   CLARITY: I - Inputs guarded (ownership + critical ops check)
   SOLID: SRP - Only handles kill operation"
  [{:keys [slave_id directory force_cross_project]}]
  (core/with-swarm
    ;; Derive caller's project-id for ownership checks
    (let [caller-project-id (when directory (scope/get-current-project-id directory))
          force? (boolean force_cross_project)]
      (if (= slave_id "all")
        ;; Kill all (optionally project-scoped)
        (let [;; Get slave IDs to kill (project-scoped or all)
              slave-ids (if caller-project-id
                          (ds/get-slave-ids-by-project caller-project-id)
                          (keys (registry/get-available-lings)))
              {:keys [can-kill? blocked-slaves]} (check-any-critical-ops slave-ids)]
          (cond
            ;; No lings to kill
            (empty? slave-ids)
            (core/mcp-success {:killed 0
                               :project-id caller-project-id
                               :message (if caller-project-id
                                          (format "No lings found for project '%s'" caller-project-id)
                                          "No lings to kill")})

            ;; Blocked by critical ops
            (not can-kill?)
            (core/mcp-error
             (format "KILL BLOCKED: Cannot kill slaves with critical operations in progress. Blocked slaves: %s"
                     (str/join ", " (map (fn [{:keys [id ops]}]
                                           (format "%s (%s)" id (format-blocking-ops ops)))
                                         blocked-slaves))))

            ;; Project-scoped kill: kill each ling individually (with ownership check)
            caller-project-id
            (let [results (mapv #(kill-single-slave! % caller-project-id force?) slave-ids)
                  killed (filter :success results)
                  failed (remove :success results)]
              ;; Update Prometheus gauge
              (let [current-count (count (registry/get-available-lings))]
                (prom/set-lings-active! (max 0 (- current-count (count killed)))))
              (core/mcp-success {:killed (count killed)
                                 :failed (count failed)
                                 :project-id caller-project-id
                                 :details {:killed (mapv :slave-id killed)
                                           :failed (mapv #(select-keys % [:slave-id :error]) failed)}}))

            ;; Global kill-all (no directory provided - coordinator context)
            :else
            (let [elisp "(json-encode (hive-mcp-swarm-api-kill-all))"
                  {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
              (cond
                timed-out
                (core/mcp-timeout-error "Kill operation" :extra-data {:slave_id slave_id})

                success
                (do
                  ;; Clear all registries: DataScript + hivemind agent-registry
                  ;; Bug fix (task 9871bcf4): hivemind_status showed stale entries
                  (registry/clear-registry!)
                  (reset! hivemind/agent-registry {})
                  ;; CLARITY-T: Reset Prometheus gauge to 0
                  (prom/set-lings-active! 0)
                  (core/mcp-success result))

                :else
                (core/mcp-error (str "Error: " error))))))
        ;; Single slave kill - use ownership-aware kill-single-slave!
        (let [{:keys [success error] :as result} (kill-single-slave! slave_id caller-project-id force?)]
          (if success
            (do
              ;; CLARITY-T: Decrement Prometheus gauge (estimate current - 1)
              (let [current-count (count (registry/get-available-lings))]
                (prom/set-lings-active! (max 0 (dec current-count))))
              (core/mcp-success (:result result)))
            ;; Return appropriate error (ownership or critical-ops)
            (core/mcp-error
             (format "KILL BLOCKED: Cannot kill slave '%s' - %s"
                     slave_id error))))))))
