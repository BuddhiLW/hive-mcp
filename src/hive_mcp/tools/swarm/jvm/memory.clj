(ns hive-mcp.tools.swarm.jvm.memory
  "Memory usage monitoring for resource guard.

   Pattern: Pure functions for memory reading and threshold checking.
   Separates I/O (shell calls) from policy (threshold logic).

   CLARITY: Layers stay pure - memory logic separated from orchestration."
  (:require [clojure.java.shell :as shell]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; ============================================================
;;; Memory Reading
;;; ============================================================

(defn get-memory-usage
  "Get current RAM usage from /proc/meminfo.

   Returns:
     {:total-mb :used-mb :available-mb :percent-used}
   or
     {:error \"message\"}

   Uses shell command instead of slurp due to JVM issues with procfs."
  []
  (try
    (let [{:keys [exit out]} (shell/sh "cat" "/proc/meminfo")]
      (if (zero? exit)
        (let [meminfo out
              parse-kb (fn [pattern]
                         (when-let [m (re-find (re-pattern (str pattern ":\\s+(\\d+)")) meminfo)]
                           (Long/parseLong (second m))))
              total-kb (parse-kb "MemTotal")
              available-kb (parse-kb "MemAvailable")
              used-kb (- total-kb available-kb)
              percent-used (double (* 100 (/ used-kb total-kb)))]
          {:total-mb (quot total-kb 1024)
           :used-mb (quot used-kb 1024)
           :available-mb (quot available-kb 1024)
           :percent-used (Math/round percent-used)})
        {:error "Failed to read /proc/meminfo"}))
    (catch Exception e
      {:error (.getMessage e)})))

;;; ============================================================
;;; Threshold Checking (Pure Functions)
;;; ============================================================

(defn memory-high?
  "Check if memory usage exceeds thresholds.

   Parameters:
     mem-info - Result from get-memory-usage
     threshold-percent - Max allowed percent used (e.g., 80)
     min-available-mb - Min required available MB (e.g., 2048)

   Returns true if EITHER condition is violated."
  [mem-info threshold-percent min-available-mb]
  (or (>= (:percent-used mem-info) threshold-percent)
      (< (:available-mb mem-info) min-available-mb)))

(defn memory-status
  "Determine memory status keyword based on thresholds.

   Returns :healthy, :warning, or :critical"
  [mem-info threshold-percent min-available-mb]
  (let [percent (:percent-used mem-info)
        warning-threshold (* threshold-percent 0.9)] ;; 90% of threshold
    (cond
      (< percent warning-threshold) :healthy
      (memory-high? mem-info threshold-percent min-available-mb) :critical
      :else :warning)))

(defn spawn-recommendation
  "Generate human-readable recommendation for spawn decision.

   Parameters:
     can-spawn - Boolean from spawn decision
     auto-cleanup - Whether auto cleanup is enabled
     cleanup-dry - Whether cleanup was dry-run only

   Returns recommendation string."
  [can-spawn auto-cleanup cleanup-dry]
  (cond
    can-spawn "Safe to spawn new processes"
    (not auto-cleanup) "Memory high - consider enabling auto_cleanup"
    cleanup-dry "Memory high - set cleanup_dry_run=false to actually kill orphans"
    :else "Capacity reached - wait for running tasks to complete"))
