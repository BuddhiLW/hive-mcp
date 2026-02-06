(ns hive-mcp.emacs.daemon-selection
  "Daemon selection logic for multi-daemon ling distribution.

   Selects the healthiest daemon with capacity for new ling spawns.
   This is about PROCESS DISTRIBUTION, not data isolation — DataScript
   is unified across all daemons (ADR-010).

   Selection criteria (ranked):
   1. Health score (higher = better) — :emacs-daemon/health-score 0-100
   2. Capacity (fewer lings = better) — max 5 lings per daemon
   3. Status filter — only :active daemons considered
   4. Project affinity (optional) — prefer daemons already hosting same-project lings

   SOLID-S: Single Responsibility — daemon selection logic only.
   SOLID-O: Open for extension — scoring functions can be overridden.
   DDD: Domain Service — stateless selection algorithm."
  (:require [hive-mcp.emacs.daemon :as daemon]
            [hive-mcp.swarm.datascript.connection :as conn]
            [datascript.core :as d]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Constants
;;; =============================================================================

(def ^:const max-lings-per-daemon
  "Maximum number of lings a single daemon should host.
   Beyond this, Emacs process stability degrades significantly."
  5)

(def ^:const default-health-score
  "Default health score for daemons without an explicit score.
   Daemons start healthy until proven otherwise."
  100)

;;; =============================================================================
;;; Health Classification (mirrors schema/daemon-health-levels)
;;; =============================================================================

(defn health-level
  "Classify a health score into a health level.

   Arguments:
     score - Health score 0-100 (nil treated as default-health-score)

   Returns:
     :healthy (70-100), :degraded (30-69), or :unhealthy (0-29)"
  [score]
  (let [s (or score default-health-score)]
    (cond
      (>= s 70) :healthy
      (>= s 30) :degraded
      :else     :unhealthy)))

(defn healthy?
  "Is the daemon healthy enough for new ling spawns?"
  [score]
  (not= :unhealthy (health-level score)))

;;; =============================================================================
;;; Daemon Metrics (Pure Query Functions)
;;; =============================================================================

(defn daemon-ling-count
  "Count the number of lings currently bound to a daemon.

   Arguments:
     daemon - Daemon map (from get-daemon or get-all-daemons)

   Returns:
     Integer count of bound lings"
  [daemon]
  (count (or (:emacs-daemon/lings daemon) #{})))

(defn daemon-has-capacity?
  "Check if a daemon can accept more lings.

   Arguments:
     daemon    - Daemon map
     max-lings - Optional max lings override (default: max-lings-per-daemon)

   Returns:
     true if daemon has room for at least one more ling"
  ([daemon]
   (daemon-has-capacity? daemon max-lings-per-daemon))
  ([daemon max-lings]
   (< (daemon-ling-count daemon) max-lings)))

(defn daemon-project-affinity
  "Calculate project affinity score for a daemon.

   If the daemon already hosts lings for the same project-id,
   it gets a bonus score (reduces cross-daemon project scatter).

   Arguments:
     daemon     - Daemon map
     project-id - Target project ID (may be nil)

   Returns:
     Affinity score (0-10). Higher = better affinity."
  [daemon project-id]
  (if (nil? project-id)
    0  ;; No project = no affinity preference
    (let [ling-ids (or (:emacs-daemon/lings daemon) #{})
          c (conn/ensure-conn)
          db @c
          ;; Count how many of this daemon's lings share the target project
          same-project-count
          (count
           (d/q '[:find [?e ...]
                  :in $ ?project-id [?ling-id ...]
                  :where
                  [?e :slave/id ?ling-id]
                  [?e :slave/project-id ?project-id]]
                db project-id (vec ling-ids)))]
      ;; Scale: 0 lings = 0, 1+ lings = 5, 3+ lings = 10
      (cond
        (>= same-project-count 3) 10
        (pos? same-project-count)  5
        :else                      0))))

;;; =============================================================================
;;; Composite Scoring
;;; =============================================================================

(defn score-daemon
  "Compute a composite selection score for a daemon.

   Higher score = better candidate for new ling spawn.

   Scoring formula:
     health_score (0-100)
     + capacity_bonus (0-50): fewer lings = more bonus
     + affinity_bonus (0-10): same-project preference

   Arguments:
     daemon     - Daemon map
     project-id - Target project ID (may be nil)

   Returns:
     Map with :daemon-id, :score, :breakdown, :disqualified?"
  [daemon project-id]
  (let [daemon-id  (:emacs-daemon/id daemon)
        status     (:emacs-daemon/status daemon)
        health     (or (:emacs-daemon/health-score daemon) default-health-score)
        ling-count (daemon-ling-count daemon)
        has-cap    (< ling-count max-lings-per-daemon)

        ;; Disqualification checks
        not-active (not= :active status)
        at-capacity (not has-cap)
        too-sick   (= :unhealthy (health-level health))
        disqualified? (or not-active at-capacity too-sick)

        ;; Scoring components (only meaningful if not disqualified)
        health-score    health
        capacity-bonus  (* (- max-lings-per-daemon ling-count) 10) ;; 10 points per free slot
        affinity-bonus  (daemon-project-affinity daemon project-id)
        total-score     (if disqualified?
                          -1
                          (+ health-score capacity-bonus affinity-bonus))]
    {:daemon-id     daemon-id
     :score         total-score
     :ling-count    ling-count
     :health-score  health
     :health-level  (health-level health)
     :disqualified? disqualified?
     :disqualify-reasons (cond-> []
                           not-active  (conj :not-active)
                           at-capacity (conj :at-capacity)
                           too-sick    (conj :unhealthy))
     :breakdown     {:health   health-score
                     :capacity capacity-bonus
                     :affinity affinity-bonus}}))

;;; =============================================================================
;;; Selection Algorithm
;;; =============================================================================

(defn select-daemon
  "Select the best daemon for a new ling spawn.

   Queries all registered daemons, scores each one, and returns the
   best candidate. Falls back to default daemon if no better option exists.

   Arguments:
     store      - IEmacsDaemon store instance
     opts       - Optional map with:
                  :project-id   - Target project for affinity scoring
                  :default-id   - Fallback daemon ID (default: env-based)

   Returns:
     Map with :daemon-id (selected daemon) and :selection-details
     Returns {:daemon-id default-id :reason :no-daemons} if none available.
     Returns {:daemon-id default-id :reason :all-disqualified} if all disqualified."
  [store & [{:keys [project-id default-id]}]]
  (let [all-daemons (daemon/get-all-daemons store)
        default-id  (or default-id
                        (System/getenv "EMACS_SOCKET_NAME")
                        "server")]
    (if (empty? all-daemons)
      ;; No daemons registered — fall back to default
      (do
        (log/warn "No daemons registered, using default:" default-id)
        {:daemon-id default-id
         :reason    :no-daemons
         :scored    []})

      ;; Score and rank all daemons
      (let [scored    (->> all-daemons
                           (map #(score-daemon % project-id))
                           (sort-by :score #(compare %2 %1))) ;; highest first
            qualified (remove :disqualified? scored)
            best      (first qualified)]

        (if best
          (do
            (log/info "Selected daemon:" (:daemon-id best)
                      "score:" (:score best)
                      "lings:" (:ling-count best) "/" max-lings-per-daemon
                      "health:" (:health-level best))
            {:daemon-id (:daemon-id best)
             :reason    :selected
             :scored    scored})

          ;; All disqualified — fall back to default with warning
          (do
            (log/warn "All daemons disqualified! Falling back to default:" default-id
                      "Reasons:" (mapv #(select-keys % [:daemon-id :disqualify-reasons]) scored))
            {:daemon-id default-id
             :reason    :all-disqualified
             :scored    scored}))))))

;;; =============================================================================
;;; Health Score Update
;;; =============================================================================

(defn update-health-score!
  "Update a daemon's health score in DataScript.

   Called by health monitoring to reflect daemon health changes.

   Arguments:
     daemon-id - Daemon to update
     score     - New health score (0-100, clamped)

   Returns:
     Transaction report or nil if daemon not found"
  [daemon-id score]
  {:pre [(string? daemon-id)
         (number? score)]}
  (let [clamped (max 0 (min 100 (int score)))
        c       (conn/ensure-conn)
        db      @c]
    (when-let [eid (:db/id (d/entity db [:emacs-daemon/id daemon-id]))]
      (log/debug "Updating daemon health:" daemon-id "→" clamped (health-level clamped))
      (d/transact! c [{:db/id eid
                       :emacs-daemon/health-score clamped}]))))

;;; =============================================================================
;;; Health Degradation Logic (EWMA-based)
;;; =============================================================================

(def ^:const ewma-alpha
  "EWMA smoothing factor. Higher = more weight on latest measurement.
   0.3 means 70% historical, 30% new — smooths transient spikes."
  0.3)

(def ^:const latency-healthy-ms
  "Response time threshold for full health. Below this = no penalty."
  500)

(def ^:const latency-degraded-ms
  "Response time threshold for degraded health. Above this = heavy penalty."
  2000)

(def ^:const latency-max-penalty
  "Maximum health points deducted for latency (scales linearly)."
  40)

(def ^:const error-penalty
  "Health points deducted per consecutive error (capped at error-penalty-max)."
  15)

(def ^:const error-penalty-max
  "Maximum cumulative error penalty."
  50)

(def ^:const ling-load-penalty-per
  "Health points deducted per ling beyond the first.
   Daemon with 5 lings = 4 * 2 = 8 points penalty."
  2)

(def ^:const recovery-bonus
  "Health points recovered per successful heartbeat after errors.
   Allows gradual recovery from error state."
  5)

(defn latency-score-delta
  "Compute health score delta from response latency.

   Arguments:
     latency-ms - Response time in milliseconds

   Returns:
     Negative number (penalty) or 0 (no penalty).
     Fast responses (< 500ms): 0 penalty
     Slow responses (500-2000ms): linear penalty up to -40
     Very slow (>2000ms): full -40 penalty"
  [latency-ms]
  (cond
    (nil? latency-ms)              (- latency-max-penalty) ;; No measurement = assume worst
    (<= latency-ms latency-healthy-ms) 0
    (>= latency-ms latency-degraded-ms) (- latency-max-penalty)
    :else
    (let [range-ms  (- latency-degraded-ms latency-healthy-ms)
          excess-ms (- latency-ms latency-healthy-ms)
          ratio     (/ (double excess-ms) range-ms)]
      (- (int (* ratio latency-max-penalty))))))

(defn error-score-delta
  "Compute health score delta from consecutive error count.

   Arguments:
     consecutive-errors - Number of consecutive heartbeat failures

   Returns:
     Negative number (penalty) capped at error-penalty-max."
  [consecutive-errors]
  (- (min error-penalty-max
          (* (or consecutive-errors 0) error-penalty))))

(defn ling-load-delta
  "Compute health score delta from ling count (load pressure).

   Arguments:
     ling-count - Number of lings bound to daemon

   Returns:
     Negative number (penalty). First ling is free."
  [ling-count]
  (- (* (max 0 (dec (or ling-count 0))) ling-load-penalty-per)))

(defn compute-health-score
  "Compute a new health score using EWMA blending of current and previous scores.

   The raw score is 100 + penalties from latency, errors, and load.
   Then blended with the previous score using EWMA for smoothing.

   Arguments:
     prev-score         - Previous health score (0-100, nil = 100)
     latency-ms         - Response time of latest heartbeat (nil = failed)
     consecutive-errors - Number of consecutive heartbeat failures
     ling-count         - Number of lings bound to daemon

   Returns:
     New health score (0-100, integer)"
  [prev-score latency-ms consecutive-errors ling-count]
  (let [prev     (or prev-score default-health-score)
        ;; Raw instantaneous score from latest measurements
        raw      (max 0 (min 100
                             (+ 100
                                (latency-score-delta latency-ms)
                                (error-score-delta consecutive-errors)
                                (ling-load-delta ling-count))))
        ;; EWMA blend: smoothed = α * new + (1-α) * old
        blended  (+ (* ewma-alpha raw)
                    (* (- 1.0 ewma-alpha) prev))]
    (max 0 (min 100 (int blended)))))

;;; =============================================================================
;;; Heartbeat with Health Scoring
;;; =============================================================================

(defn heartbeat!
  "Execute a heartbeat for a daemon: ping Emacs, measure latency, update health.

   This is the core W2 heartbeat function that:
   1. Pings the Emacs daemon with a lightweight elisp eval
   2. Measures response time
   3. Computes new health score based on latency + errors + load
   4. Updates both heartbeat timestamp and health score in DataScript

   Arguments:
     daemon-id  - Daemon to heartbeat
     ping-fn    - Function that pings Emacs, returns {:success bool :duration-ms int}
                  (default: uses emacsclient eval-elisp-with-timeout)

   Returns:
     Map with :healthy? :health-score :latency-ms :health-level :consecutive-errors
     or nil if daemon not found"
  ([daemon-id]
   (heartbeat! daemon-id nil))
  ([daemon-id ping-fn]
   (let [c       (conn/ensure-conn)
         db      @c
         eid     (:db/id (d/entity db [:emacs-daemon/id daemon-id]))]
     (when eid
       (let [entity       (d/entity db eid)
             prev-score   (or (:emacs-daemon/health-score entity) default-health-score)
             prev-errors  (or (:emacs-daemon/error-count entity) 0)
             ling-count   (count (or (:emacs-daemon/lings entity) #{}))
             ;; Ping the daemon
             ping-result  (try
                            (if ping-fn
                              (ping-fn daemon-id)
                              ;; Default: use emacsclient with 3s timeout
                              (let [ec-fn (requiring-resolve 'hive-mcp.emacsclient/eval-elisp-with-timeout)]
                                (ec-fn "t" 3000)))
                            (catch Exception e
                              {:success false :error (.getMessage e)}))
             success?     (:success ping-result)
             latency-ms   (when success? (:duration-ms ping-result))
             ;; Consecutive errors: reset on success, increment on failure
             new-errors   (if success? 0 (inc prev-errors))
             ;; Compute new health score
             new-score    (compute-health-score prev-score latency-ms new-errors ling-count)
             ;; Apply recovery bonus if successful after errors
             final-score  (if (and success? (pos? prev-errors))
                            (min 100 (+ new-score recovery-bonus))
                            new-score)]
         ;; Update DataScript: heartbeat + health score + error count
         (d/transact! c
                      (cond-> [{:db/id eid
                                :emacs-daemon/health-score final-score
                                :emacs-daemon/error-count new-errors}]
                        success?
                        (conj {:db/id eid
                               :emacs-daemon/heartbeat-at (java.util.Date.)
                               :emacs-daemon/status :active})
                        (not success?)
                        (conj {:db/id eid
                               :emacs-daemon/status (if (>= new-errors 3) :error :active)})))

         (let [level (health-level final-score)]
           (if success?
             (log/debug "Heartbeat OK:" daemon-id
                        "latency:" latency-ms "ms"
                        "health:" final-score (name level)
                        "lings:" ling-count)
             (log/warn "Heartbeat FAILED:" daemon-id
                       "errors:" new-errors
                       "health:" final-score (name level)))
           {:healthy?           (not= :unhealthy level)
            :health-score       final-score
            :health-level       level
            :latency-ms         latency-ms
            :consecutive-errors new-errors
            :ling-count         ling-count
            :success?           success?}))))))

;;; =============================================================================
;;; W4: Ling Redistribution
;;; =============================================================================
;;
;; Redistributes lings from overloaded/degraded daemons to healthier ones.
;; Unlike W3 (autoheal) which handles DEAD daemons, redistribution handles
;; ALIVE but stressed daemons — the Emacs process is still running but is
;; degraded or overloaded.
;;
;; Redistribution is conservative:
;; 1. Only moves IDLE lings (never interrupts working lings)
;; 2. Only migrates when the target daemon is significantly better
;; 3. Caps migrations per cycle to avoid thundering herd
;; 4. Respects project affinity (prefers same-project daemons)

(def ^:const redistribution-score-threshold
  "Minimum score improvement to justify a migration.
   Prevents churn from marginal improvements."
  20)

(def ^:const max-migrations-per-cycle
  "Maximum number of lings to redistribute in a single cycle.
   Prevents thundering herd when many lings need to move."
  2)

(def ^:const overloaded-ling-threshold
  "A daemon is considered overloaded if it has this many or more lings.
   Set to max-lings-per-daemon - 1 to trigger before hard cap."
  4)

(defn find-overloaded-daemons
  "Find active daemons that are degraded or overloaded.

   A daemon qualifies for redistribution when:
   - Status is :active (still alive — dead daemons are handled by W3 autoheal)
   - AND either:
     a) Health level is :degraded (score 30-69)
     b) Ling count >= overloaded-ling-threshold (near capacity)

   Arguments:
     store - IEmacsDaemon store instance

   Returns:
     Seq of daemon maps that are candidates for offloading, or nil if none."
  [store]
  (let [all-daemons (daemon/get-all-daemons store)
        active (filter #(= :active (:emacs-daemon/status %)) all-daemons)
        overloaded (filter
                    (fn [d]
                      (let [health (or (:emacs-daemon/health-score d) default-health-score)
                            level (health-level health)
                            ling-count (daemon-ling-count d)]
                        (and (pos? ling-count) ;; Must have lings to offload
                             (or (= :degraded level)
                                 (>= ling-count overloaded-ling-threshold)))))
                    active)]
    (seq overloaded)))

(defn find-migration-candidates
  "Find idle lings on an overloaded daemon that can be migrated.

   Only idle lings are candidates — working/blocked lings are left alone
   to avoid task interruption.

   Arguments:
     daemon - Daemon map (from get-all-daemons)

   Returns:
     Seq of maps with :ling-id, :project-id, :ling-status.
     Empty if no eligible lings."
  [daemon]
  (let [ling-ids (or (:emacs-daemon/lings daemon) #{})
        daemon-id (:emacs-daemon/id daemon)
        c (conn/ensure-conn)
        db @c]
    (->> ling-ids
         (map (fn [ling-id]
                (when-let [e (d/entity db [:slave/id ling-id])]
                  {:ling-id    ling-id
                   :daemon-id  daemon-id
                   :project-id (:slave/project-id e)
                   :ling-status (:slave/status e)})))
         (filter some?)
         (filter #(= :idle (:ling-status %))))))

(defn plan-redistribution
  "Create a redistribution plan: which lings move where.

   For each overloaded daemon, finds idle lings and scores potential
   target daemons. Only plans migrations that improve by at least
   `redistribution-score-threshold` points.

   Arguments:
     store - IEmacsDaemon store instance

   Returns:
     Seq of migration plan maps:
       {:ling-id, :source-daemon, :target-daemon, :source-score, :target-score, :improvement}
     Or nil if no beneficial migrations exist."
  [store]
  (when-let [overloaded (find-overloaded-daemons store)]
    (let [all-daemons (daemon/get-all-daemons store)
          plans (for [source-d overloaded
                      :let [source-id (:emacs-daemon/id source-d)
                            source-score (:score (score-daemon source-d nil))
                            candidates (find-migration-candidates source-d)]
                      candidate candidates
                      :let [project-id (:project-id candidate)
                            ;; Score all other active daemons as targets
                            targets (->> all-daemons
                                         (remove #(= source-id (:emacs-daemon/id %)))
                                         (map #(score-daemon % project-id))
                                         (remove :disqualified?)
                                         (sort-by :score #(compare %2 %1)))
                            best-target (first targets)]
                      :when best-target
                      :let [improvement (- (:score best-target) (max 0 source-score))]
                      :when (>= improvement redistribution-score-threshold)]
                  {:ling-id       (:ling-id candidate)
                   :project-id    project-id
                   :source-daemon source-id
                   :target-daemon (:daemon-id best-target)
                   :source-score  source-score
                   :target-score  (:score best-target)
                   :improvement   improvement})]
      ;; Cap at max-migrations-per-cycle, prioritize highest improvement
      (when (seq plans)
        (->> plans
             (sort-by :improvement #(compare %2 %1))
             (take max-migrations-per-cycle)
             vec)))))

(defn migrate-ling!
  "Execute a single ling migration from one daemon to another.

   Steps:
   1. Verify ling is still idle (guard against race conditions)
   2. Unbind from source daemon
   3. Bind to target daemon
   4. Log the migration

   Arguments:
     store     - IEmacsDaemon store instance
     migration - Migration plan map from plan-redistribution

   Returns:
     Map with :success?, :ling-id, :source-daemon, :target-daemon, :reason"
  [store {:keys [ling-id source-daemon target-daemon]}]
  (let [c (conn/ensure-conn)
        db @c]
    ;; Guard: verify ling is still idle (race condition protection)
    (if-let [e (d/entity db [:slave/id ling-id])]
      (if (= :idle (:slave/status e))
        (do
          (daemon/unbind-ling! store source-daemon ling-id)
          (daemon/bind-ling! store target-daemon ling-id)
          (log/info "W4 redistribute: Migrated ling" ling-id
                    "from" source-daemon "to" target-daemon)
          {:success?      true
           :ling-id       ling-id
           :source-daemon source-daemon
           :target-daemon target-daemon})
        ;; Ling is no longer idle — skip migration
        (do
          (log/debug "W4 redistribute: Skipping" ling-id
                     "- status changed to" (:slave/status e))
          {:success?      false
           :ling-id       ling-id
           :source-daemon source-daemon
           :target-daemon target-daemon
           :reason        :no-longer-idle}))
      ;; Ling not found — already killed?
      {:success?      false
       :ling-id       ling-id
       :source-daemon source-daemon
       :target-daemon target-daemon
       :reason        :ling-not-found})))

(defn redistribute-lings!
  "Main entry point: plan and execute ling redistribution.

   Called from the heartbeat loop to proactively rebalance lings
   across daemons when some become degraded or overloaded.

   Arguments:
     store - IEmacsDaemon store instance

   Returns:
     Map with :migrations-planned, :migrations-executed, :results
     Or nil if no redistribution needed."
  [store]
  (when-let [plan (plan-redistribution store)]
    (log/info "W4 redistribute: Planning" (count plan) "migrations"
              (mapv #(select-keys % [:ling-id :source-daemon :target-daemon :improvement]) plan))
    (let [results (mapv #(migrate-ling! store %) plan)
          executed (count (filter :success? results))
          failed (- (count results) executed)]
      ;; Emit event for Olympus visibility
      (try
        (when-let [emit-fn (requiring-resolve 'hive-mcp.transport.olympus/emit-agent-event!)]
          (emit-fn :daemon/lings-redistributed
                   {:planned (count plan)
                    :executed executed
                    :failed failed
                    :details (mapv #(select-keys % [:ling-id :source-daemon :target-daemon :success?])
                                   results)}))
        (catch Exception _ nil))
      (when (pos? executed)
        (log/info "W4 redistribute: Executed" executed "of" (count plan) "migrations"
                  (when (pos? failed) (str "(" failed " failed)"))))
      {:migrations-planned  (count plan)
       :migrations-executed executed
       :migrations-failed   failed
       :results             results})))

(defn redistribution-status
  "Get current redistribution status without executing. Monitoring endpoint.

   Arguments:
     store - IEmacsDaemon store instance

   Returns:
     Map with :overloaded-daemons, :planned-migrations, or nil if balanced."
  [store]
  (let [overloaded (find-overloaded-daemons store)
        plan (when overloaded (plan-redistribution store))]
    (when overloaded
      {:overloaded-count (count overloaded)
       :overloaded-daemons (mapv (fn [d]
                                   {:daemon-id (:emacs-daemon/id d)
                                    :health-score (or (:emacs-daemon/health-score d)
                                                      default-health-score)
                                    :health-level (health-level
                                                   (or (:emacs-daemon/health-score d)
                                                       default-health-score))
                                    :ling-count (daemon-ling-count d)})
                                 overloaded)
       :planned-migrations (or plan [])
       :migrations-available (count (or plan []))})))

(comment
  ;; Usage examples

  ;; Score a daemon
  ;; (score-daemon {:emacs-daemon/id "server"
  ;;                :emacs-daemon/status :active
  ;;                :emacs-daemon/health-score 85
  ;;                :emacs-daemon/lings #{"ling-1" "ling-2"}}
  ;;               "hive-mcp")

  ;; Select best daemon
  ;; (select-daemon (daemon-store/get-store)
  ;;                {:project-id "hive-mcp"})

  ;; Update health manually
  ;; (update-health-score! "server" 75)

  ;; Execute heartbeat with health scoring
  ;; (heartbeat! "server")

  ;; Heartbeat with custom ping function (for testing)
  ;; (heartbeat! "server" (fn [_] {:success true :duration-ms 100}))

  ;; Compute health score
  ;; (compute-health-score 80 150 0 2)  ;; => ~96 (fast, no errors, 2 lings)
  ;; (compute-health-score 80 3000 2 4) ;; => ~26 (slow, errors, loaded)

  ;; W4: Check redistribution status
  ;; (redistribution-status (daemon-store/get-store))

  ;; W4: Execute redistribution
  ;; (redistribute-lings! (daemon-store/get-store))
  )
