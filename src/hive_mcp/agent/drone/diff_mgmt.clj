(ns hive-mcp.agent.drone.diff-mgmt
  "Diff management for drone execution.

   Extracted from drone.clj to reduce complexity (SOLID-S: Single Responsibility).
   This namespace handles diff lifecycle during drone execution:
   - Auto-applying proposed diffs after execution
   - Tagging diffs with wave-id for batch review
   - Tracking diff results (applied/failed/proposed)

   CLARITY-T: All diff operations are logged for observability."
  (:require [hive-mcp.tools.diff :as diff]
            [clojure.data.json :as json]
            [clojure.set]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Diff Result Record
;;; ============================================================

(defrecord DiffResults
           [applied failed proposed]
  ;; applied  - vector of file paths successfully applied
  ;; failed   - vector of {:file :error} for failed applications
  ;; proposed - vector of diff-ids for review mode
  )

(defn ->diff-results
  "Create a DiffResults record.

   Arguments:
     applied  - vector of file paths
     failed   - vector of {:file :error} maps
     proposed - vector of diff-ids (for skip-auto-apply mode)"
  [applied failed proposed]
  (->DiffResults (vec applied) (vec failed) (vec proposed)))

(defn empty-diff-results
  "Create an empty DiffResults."
  []
  (->diff-results [] [] []))

;;; ============================================================
;;; Diff Operations
;;; ============================================================

(defn auto-apply-diffs!
  "Auto-apply diffs proposed during drone execution.

   Arguments:
     drone-id     - Drone identifier for logging
     new-diff-ids - Set of diff IDs to apply

   Returns:
     DiffResults record with :applied, :failed, and :proposed vectors.

   CLARITY-T: Logs operational details directly for debugging."
  [drone-id new-diff-ids]
  (if (empty? new-diff-ids)
    (empty-diff-results)
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
      (->diff-results
       (mapv :file applied)
       (mapv #(select-keys % [:file :error]) failed)
       []))))

(defn tag-diffs-with-wave!
  "Tag newly proposed diffs with wave-id for batch review tracking.

   Used by validated wave execution to associate diffs with their wave
   so they can be reviewed together before applying.

   Arguments:
     new-diff-ids - Set/seq of diff IDs to tag
     wave-id      - Wave identifier to associate

   CLARITY-T: Logs tagging operation for traceability."
  [new-diff-ids wave-id]
  (when (and (seq new-diff-ids) wave-id)
    (doseq [diff-id new-diff-ids]
      (swap! diff/pending-diffs update diff-id assoc :wave-id wave-id))
    (log/debug "Tagged diffs with wave-id" {:wave-id wave-id :count (count new-diff-ids)})))

(defn get-new-diff-ids
  "Calculate the set of new diff IDs after drone execution.

   Arguments:
     diffs-before - Set of diff IDs before execution
     diffs-after  - Set of diff IDs after execution

   Returns:
     Set of new diff IDs."
  [diffs-before diffs-after]
  (clojure.set/difference diffs-after diffs-before))

(defn capture-diffs-before
  "Capture the current set of pending diff IDs.

   Returns:
     Set of current diff IDs."
  []
  (set (keys @diff/pending-diffs)))

(defn handle-diff-results!
  "Handle diff application based on execution mode.

   Arguments:
     drone-id       - Drone identifier
     new-diff-ids   - Set of new diff IDs
     wave-id        - Optional wave ID for batch review
     skip-auto-apply - When true, don't apply diffs

   Returns:
     DiffResults record."
  [drone-id new-diff-ids {:keys [wave-id skip-auto-apply]}]
  (when (and wave-id (seq new-diff-ids))
    (tag-diffs-with-wave! new-diff-ids wave-id))

  (if skip-auto-apply
    ;; In validated mode, don't apply - just track proposed diffs
    (->diff-results [] [] (vec new-diff-ids))
    ;; Normal mode - auto-apply diffs
    (auto-apply-diffs! drone-id new-diff-ids)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defn summarize-diff-results
  "Create a human-readable summary of diff results.

   Arguments:
     results - DiffResults record

   Returns:
     String summary for logging/reporting."
  [results]
  (cond
    (seq (:proposed results))
    (str (count (:proposed results)) " diffs proposed for review")

    (and (seq (:applied results)) (empty? (:failed results)))
    (str (count (:applied results)) " diffs applied successfully")

    (and (seq (:applied results)) (seq (:failed results)))
    (str (count (:applied results)) " applied, " (count (:failed results)) " failed")

    (seq (:failed results))
    (str (count (:failed results)) " diffs failed")

    :else
    "No diffs"))
