(ns hive-mcp.agent.drone
  "Drone delegation - token-optimized leaf agents.
   
   Drones are lightweight agents that:
   - Use OpenRouter free-tier models
   - Pre-inject file contents (no read tool calls needed)
   - Use propose_diff instead of direct file writes
   - Auto-apply proposed diffs on completion
   - Report status to parent lings for swarm sync"
  (:require [hive-mcp.agent.registry :as registry]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.hivemind :as hivemind]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.set]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Configuration
;;; ============================================================

(def allowed-tools
  "Safe tools for drone agents. Drones cannot write files directly - they must use propose_diff."
  ["read_file" "grep" "glob_files" "clojure_eval" "clojure_inspect_project"
   "magit_status" "magit_diff" "magit_log" "magit_branches"
   "propose_diff" "hivemind_shout"])

;;; ============================================================
;;; Context Preparation
;;; ============================================================

(defn- prepare-context
  "Prepare context for drone delegation by gathering catchup data."
  []
  (try
    (let [catchup-handler (registry/get-tool "mcp_get_context")
          context (when (:handler catchup-handler)
                    ((:handler catchup-handler) {}))]
      (if (and context (:text context))
        (let [parsed (json/read-str (:text context) :key-fn keyword)]
          {:conventions (get-in parsed [:memory :conventions] [])
           :decisions (get-in parsed [:memory :decisions] [])
           :snippets (get-in parsed [:memory :snippets] [])
           :project (get parsed :project {})})
        {}))
    (catch Exception e
      (log/warn e "Failed to gather ling context")
      {})))

(defn- format-context-str
  "Format context data as string for task augmentation."
  [context]
  (when (seq context)
    (str "## Project Context\n"
         (when (seq (:conventions context))
           (str "### Conventions\n"
                (str/join "\n" (map :content (:conventions context)))
                "\n\n"))
         (when (seq (:decisions context))
           (str "### Decisions\n"
                (str/join "\n" (map :content (:decisions context)))
                "\n\n")))))

(defn- format-file-contents
  "Pre-read file contents so drone has exact content for propose_diff."
  [files]
  (when (seq files)
    (let [project-root (or (diff/get-project-root) "")
          contents (for [f files]
                     (let [abs-path (if (str/starts-with? f "/")
                                      f
                                      (str project-root "/" f))]
                       (try
                         (let [content (slurp abs-path)]
                           (str "### " f "\n```\n" content "```\n"))
                         (catch Exception e
                           (str "### " f "\n(File not found or unreadable: " (.getMessage e) ")\n")))))]
      (str "## Current File Contents\n"
           "IMPORTANT: Use this EXACT content as old_content in propose_diff.\n"
           "Do NOT guess or assume file content - use what is provided below.\n\n"
           (str/join "\n" contents)))))

(defn- augment-task
  "Augment task with context and file contents."
  [task files]
  (let [context (prepare-context)
        context-str (format-context-str context)
        file-contents-str (format-file-contents files)]
    (str context-str
         "## Task\n" task
         (when (seq files)
           (str "\n\n## Files to modify\n"
                (str/join "\n" (map #(str "- " %) files))))
         (when file-contents-str
           (str "\n\n" file-contents-str)))))

;;; ============================================================
;;; Diff Management
;;; ============================================================

(defn- auto-apply-diffs
  "Auto-apply diffs proposed during drone execution."
  [drone-id new-diff-ids]
  (when (seq new-diff-ids)
    (let [results (for [diff-id new-diff-ids]
                    (let [diff-info (get @diff/pending-diffs diff-id)
                          response (diff/handle-apply-diff {:diff_id diff-id})
                          parsed (try (json/read-str (:text response) :key-fn keyword)
                                      (catch Exception _ nil))]
                      (if (:isError response)
                        {:status :failed :file (:file-path diff-info) :error (:error parsed)}
                        {:status :applied :file (:file-path diff-info)})))
          {applied :applied failed :failed} (group-by :status results)]
      (when (seq applied)
        (log/info "Auto-applied drone diffs" {:drone drone-id :files (mapv :file applied)}))
      (when (seq failed)
        (log/warn "Some drone diffs failed to apply" {:drone drone-id :failures failed}))
      {:applied (mapv :file applied)
       :failed (mapv #(select-keys % [:file :error]) failed)})))

;;; ============================================================
;;; Public API
;;; ============================================================

(defn delegate!
  "Delegate a task to a drone (token-optimized leaf agent).
   
   Automatically:
   - Pre-injects file contents (drone doesn't need to read)
   - Injects catchup context (conventions, decisions, snippets)
   - Uses drone-worker preset for OpenRouter
   - Auto-applies any diffs proposed by the drone
   - Records results to hivemind for review
   - Reports status to parent ling (if parent-id provided) for swarm state sync
   
   Options:
     :task      - Task description (required)
     :files     - List of files the drone will modify (contents pre-injected)
     :preset    - Override preset (default: drone-worker)
     :trace     - Enable progress events (default: true)
     :parent-id - Parent ling's slave-id (for swarm status sync)
   
   Returns result map with :status, :result, :agent-id, :files-modified"
  [{:keys [task files preset trace parent-id]
    :or {preset "drone-worker"
         trace true}}
   delegate-fn]
  (let [effective-parent-id (or parent-id
                                (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        augmented-task (augment-task task files)
        agent-id (str "drone-" (System/currentTimeMillis))
        diffs-before (set (keys @diff/pending-diffs))]

    ;; Shout started to parent ling
    (when effective-parent-id
      (hivemind/shout! effective-parent-id :started
                       {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                        :message (format "Delegated drone %s working" agent-id)}))

    (let [result (delegate-fn {:backend :openrouter
                               :preset preset
                               :task augmented-task
                               :tools allowed-tools
                               :trace trace})
          diffs-after (set (keys @diff/pending-diffs))
          new-diff-ids (clojure.set/difference diffs-after diffs-before)
          diff-results (auto-apply-diffs agent-id new-diff-ids)]

      ;; Shout completion to parent ling
      (when effective-parent-id
        (if (= :completed (:status result))
          (hivemind/shout! effective-parent-id :completed
                           {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                            :message (format "Drone %s completed. Files: %s"
                                             agent-id
                                             (str/join ", " (or (:applied diff-results) [])))})
          (hivemind/shout! effective-parent-id :error
                           {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                            :message (format "Drone %s failed: %s" agent-id (:result result))})))

      ;; Record result for coordinator review
      (hivemind/record-ling-result! agent-id
                                    {:task task
                                     :files files
                                     :result result
                                     :diff-results diff-results
                                     :parent-id effective-parent-id
                                     :timestamp (System/currentTimeMillis)})
      (assoc result
             :agent-id agent-id
             :parent-id effective-parent-id
             :files-modified (:applied diff-results)
             :files-failed (:failed diff-results)))))
