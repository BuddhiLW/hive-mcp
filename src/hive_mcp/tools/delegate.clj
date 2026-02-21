(ns hive-mcp.tools.delegate
  "Unified delegate API for drone and batch execution.

   Consolidates three existing tools into one polymorphic interface:
   - delegate_drone (single task)
   - dispatch_drone_wave (batch)
   - dispatch_validated_wave (batch + validation)

   ADR: 20260123161700-6d447e27 - Unified Delegate API

   Usage:
     ;; Single task mode
     (delegate {:task \"Fix the bug in auth.clj\" :files [\"src/auth.clj\"]})

     ;; Batch mode
     (delegate {:tasks [{:file \"src/a.clj\" :task \"Add docstrings\"}
                        {:file \"src/b.clj\" :task \"Add docstrings\"}]})

     ;; Validated batch mode
     (delegate {:tasks [...] :validate true :max_retries 3})

     ;; Review-before-apply mode
     (delegate {:tasks [...] :review_mode true})"

  (:require [hive-mcp.agent.core :as agent]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.tools.swarm.wave :as wave]
            [hive-mcp.tools.swarm.validated-wave :as validated-wave]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Handler
;;; ============================================================

(defn handle-delegate
  "Unified handler for drone delegation with polymorphic dispatch.

   Routing Logic:
   1. :task present → single drone mode (delegate-drone!)
   2. :tasks + :validate/:review_mode → validated wave
   3. :tasks alone → batch wave

   Parameters:
     task        - Single task description (string)
     tasks       - Batch tasks array [{:file :task}]
     files       - Files for single mode (array)
     validate    - Run lint validation after batch (bool)
     review_mode - Propose diffs for review before apply (bool)
     max_retries - Max validation retries (int)
     lint_level  - Lint severity: error, warning, info (string)
     preset      - Drone preset (string)
     trace       - Emit progress events (bool)
     cwd         - Working directory override (string)
     parent_id   - Parent ling's slave-id (string)
     concurrency - Max concurrent drones for batch (int)"
  [{:keys [task tasks files validate review_mode max_retries lint_level
           preset trace cwd parent_id _concurrency]}]
  (log/info {:event :delegate/unified-api-call
             :mode (cond task :single tasks :batch :else :invalid)
             :validate validate
             :review_mode review_mode})

  (cond
    ;; ERROR: Both task and tasks provided
    (and task tasks)
    (do
      (log/warn {:event :delegate/invalid-params
                 :error "Cannot specify both :task and :tasks"})
      (mcp-error "Cannot specify both :task and :tasks. Use :task for single drone, :tasks for batch."))

    ;; ERROR: Neither task nor tasks provided
    (and (nil? task) (nil? tasks))
    (do
      (log/warn {:event :delegate/invalid-params
                 :error "Must specify either :task or :tasks"})
      (mcp-error "Must specify either :task or :tasks."))

    ;; SINGLE MODE: task present
    task
    (do
      (log/info {:event :delegate/routing
                 :mode :single
                 :file-count (count (or files []))})
      (let [;; file-level: Proactively surface staleness warnings for claimed files
            disc-warnings (try (kg-disc/staleness-warnings files) (catch Exception _ nil))
            disc-notice (kg-disc/format-staleness-warnings disc-warnings)
            augmented-task (if disc-notice (str disc-notice task) task)
            result (agent/delegate-drone!
                    {:task augmented-task
                     :files files
                     :preset (or preset "drone-worker")
                     :trace (if (nil? trace) true trace)
                     :parent-id parent_id
                     :cwd cwd})]
        (mcp-json (assoc result
                         :delegate-mode :single
                         :message (or (:message result)
                                      (if (= :completed (:status result))
                                        "Single drone completed"
                                        "Single drone finished"))))))

    ;; BATCH MODE with validation or review
    (and tasks (or validate review_mode))
    (do
      (log/info {:event :delegate/routing
                 :mode :validated-batch
                 :task-count (count tasks)
                 :validate validate
                 :review_mode review_mode})
      ;; Delegate to validated-wave handler (handles both lint and review modes)
      (validated-wave/handle-dispatch-validated-wave
       {:tasks tasks
        :review_mode review_mode
        :validate (if (nil? validate) true validate)
        :max_retries max_retries
        :lint_level lint_level
        :preset preset
        :trace trace
        :cwd cwd}))

    ;; BATCH MODE without validation
    tasks
    (do
      (log/info {:event :delegate/routing
                 :mode :batch
                 :task-count (count tasks)})
      ;; Delegate to wave handler
      (wave/handle-dispatch-drone-wave
       {:tasks tasks
        :preset preset
        :trace trace
        :cwd cwd}))

    ;; Fallback (should not reach here)
    :else
    (mcp-error "Invalid delegate parameters")))

;;; ============================================================
;;; Tool Definition
;;; ============================================================

(def tools
  "REMOVED: Flat delegate tool no longer exposed. Use consolidated `wave` tool."
  [])
