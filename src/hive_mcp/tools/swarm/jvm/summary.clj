(ns hive-mcp.tools.swarm.jvm.summary
  "Summary builders for JVM management operations.

   Pattern: Pure functions that transform process data into summary maps.
   No I/O, no side effects - just data transformation.

   CLARITY: Represented intent - separate data shaping from business logic."
  (:require [hive-mcp.tools.swarm.jvm.memory :as memory]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; ============================================================
;;; Cleanup Summary Builder
;;; ============================================================

(defn- swarm-summary
  "Build swarm-specific summary from classified processes."
  [classified]
  (let [swarm-procs (filter :swarm-spawned classified)
        by-slave (group-by :swarm-slave-id swarm-procs)]
    {:total-swarm-spawned (count swarm-procs)
     :by-slave (into {}
                     (map (fn [[k v]]
                            [(or k "unknown")
                             {:count (count v)
                              :pids (map :pid v)}])
                          by-slave))}))

(defn- orphan-detection-summary
  "Build orphan detection summary."
  [all-classified true-orphans-only]
  (let [managed (filter :parent-is-claude all-classified)]
    {:mode (if true-orphans-only "true-orphans" "age-based")
     :truly-orphaned-count (count (filter :truly-orphaned all-classified))
     :managed-by-claude (count managed)}))

(defn- process-details
  "Extract relevant details from classified processes."
  [all-classified]
  (map #(select-keys % [:pid :ppid :type :etime :orphan :reason :age-minutes
                        :truly-orphaned :parent-alive :parent-comm :parent-is-claude
                        :swarm-spawned :swarm-slave-id :swarm-master-id :swarm-depth])
       all-classified))

(defn build-cleanup-summary
  "Build summary map from classified processes and cleanup results.

   Parameters:
     all-procs - Raw JVM processes found
     classified - Processes after classification
     all-classified - Processes after orphan identification
     orphans - Filtered orphan processes
     killed-pids - PIDs that were killed (or nil if dry-run)
     options - Map with :dry-run, :swarm-only, :min-age, :true-orphans-only"
  [all-procs classified all-classified orphans killed-pids
   {:keys [dry-run swarm-only min-age true-orphans-only]}]
  (let [by-type (group-by :type classified)]
    {:total-jvm-processes (count all-procs)
     :by-type (into {} (map (fn [[k v]] [(name k) (count v)]) by-type))
     :swarm (swarm-summary classified)
     :orphan-detection (orphan-detection-summary all-classified true-orphans-only)
     :orphans-found (count orphans)
     :orphan-pids (map :pid orphans)
     :dry-run dry-run
     :swarm-only-mode swarm-only
     :killed (if dry-run [] (or killed-pids []))
     :min-age-threshold min-age
     :details (process-details all-classified)}))

;;; ============================================================
;;; Resource Guard Summary Builder
;;; ============================================================

(defn- determine-status
  "Determine status keyword based on memory state before/after cleanup."
  [initial-high? final-high?]
  (cond
    (not initial-high?) :healthy
    (and initial-high? (not final-high?)) :recovered-after-cleanup
    :else :capacity-reached))

(defn build-resource-guard-summary
  "Build summary map for resource guard response.

   Parameters:
     can-spawn - Whether spawning is allowed
     initial-mem - Memory state before cleanup
     final-mem - Memory state after cleanup
     threshold - Percent threshold used
     min-available - Minimum available MB required
     initial-high? - Was memory high initially?
     final-high? - Is memory high after cleanup?
     cleanup-config - Map with :auto-clean, :cleanup-dry
     cleanup-data - Parsed cleanup result (or nil)"
  [can-spawn initial-mem final-mem threshold min-available
   initial-high? final-high? {:keys [auto-clean cleanup-dry]} cleanup-data]
  (let [orphans-killed (when cleanup-data (count (:killed cleanup-data)))]
    {:can-spawn can-spawn
     :memory {:initial initial-mem
              :final final-mem
              :threshold-percent threshold
              :min-available-mb min-available}
     :status (determine-status initial-high? final-high?)
     :cleanup (when cleanup-data
                {:ran true
                 :dry-run cleanup-dry
                 :orphans-found (:orphans-found cleanup-data)
                 :killed orphans-killed})
     :recommendation (memory/spawn-recommendation can-spawn auto-clean cleanup-dry)}))
