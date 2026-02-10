(ns hive-mcp.knowledge-graph.disc.volatility
  "Pure volatility classification, Bayesian certainty, and staleness scoring
   functions for disc entities.

   Extracted from disc.clj (Sprint 1 - SAA refactoring).
   All functions in this namespace are PURE (no side effects, no DataScript).

   Domains covered:
   - Volatility classification (file path → :stable/:moderate/:volatile)
   - Bayesian certainty (Beta distribution alpha/beta updates)
   - Staleness scoring (disc map → numeric score)
   - Entry staleness (Chroma entry staleness scoring)
   - Staleness formatting (warnings → text)

   CLARITY-L: Pure layer - zero side effects, zero I/O.
   CLARITY-I: All inputs are validated data maps."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Volatility Classification
;; =============================================================================

(def ^:private volatility-patterns
  "File patterns for volatility classification.
   Used to determine appropriate decay rates for certainty."
  {:stable   #{#"deps\.edn$" #"project\.clj$" #"pom\.xml$" #"\.gitignore$"}
   :volatile #{#"\.log$" #"\.tmp$" #"target/" #"\.nrepl-port$"}})

(defn classify-volatility
  "Classify file volatility based on path patterns.
   Returns :stable, :moderate, or :volatile"
  [path]
  (cond
    (some #(re-find % path) (:stable volatility-patterns)) :stable
    (some #(re-find % path) (:volatile volatility-patterns)) :volatile
    :else :moderate))

(def decay-rates
  "Daily certainty decay rates by volatility class.
   Higher values = faster certainty decay."
  {:stable 0.01    ;; 1% per day - config files rarely change
   :moderate 0.05  ;; 5% per day - typical source files
   :volatile 0.15}) ;; 15% per day - logs, temps, build artifacts

;; =============================================================================
;; L1-P2 Transitive Staleness Propagation Constants
;; =============================================================================

(def staleness-decay-factor
  "Decay factor per hop in staleness propagation.
   Each hop multiplies staleness by this factor."
  0.5)

(def staleness-min-threshold
  "Minimum staleness to propagate (stop propagation below this)."
  0.3)

(def staleness-max-depth
  "Maximum depth for staleness propagation."
  5)

(def base-staleness-values
  "Base staleness values by source event type."
  {:hash-mismatch 5.0
   :git-commit 2.0
   :time-decay 0.5})

;; =============================================================================
;; Initial Priors by Volatility
;; =============================================================================

(def initial-alpha-by-volatility
  "Initial alpha priors by volatility class.
   Higher alpha = more confident starting certainty.
   Stable files start more confident since they rarely change."
  {:stable   7.0   ;; 7/(7+2) = 0.78 initial certainty
   :moderate 5.0   ;; 5/(5+2) = 0.71 initial certainty
   :volatile 3.0}) ;; 3/(3+2) = 0.60 initial certainty

;; =============================================================================
;; Bayesian Certainty Functions (Beta Distribution)
;; =============================================================================
;;
;; Probabilistic certainty model using Beta distribution:
;; - alpha: pseudo-count of "confirming" observations (reads that verified)
;; - beta: pseudo-count of "refuting" observations (hash mismatches, git changes)
;;
;; Expected certainty = alpha / (alpha + beta)
;; Higher alpha → more confident the file content is accurate
;; Higher beta → more evidence that content has changed

(defn current-certainty
  "Expected certainty: alpha / (alpha + beta).
   Returns float in [0, 1] representing how confident we are that
   this disc entity's knowledge is still accurate.

   Default priors: alpha=5.0 (mildly confident), beta=2.0 (some uncertainty)
   This gives initial certainty of ~0.71"
  [disc]
  (let [a (or (:disc/certainty-alpha disc) 5.0)
        b (or (:disc/certainty-beta disc) 2.0)]
    (/ a (+ a b))))

(defn beta-lower-bound
  "Lower bound of 95% credible interval for certainty.
   Uses normal approximation: mean - 2*sqrt(variance)

   Variance of Beta(a,b) = ab / ((a+b)^2 * (a+b+1))

   This gives a conservative estimate - if the lower bound is low,
   we're uncertain about our certainty."
  [disc]
  (let [a (or (:disc/certainty-alpha disc) 5.0)
        b (or (:disc/certainty-beta disc) 2.0)
        sum (+ a b)
        mean (/ a sum)
        variance (/ (* a b) (* sum sum (+ sum 1)))
        std-dev (Math/sqrt variance)]
    (max 0.0 (- mean (* 2 std-dev)))))

(defn needs-read?
  "True if certainty below threshold or credible interval too wide.

   Triggers re-read when:
   1. Expected certainty falls below threshold
   2. Lower bound of credible interval < 50% of threshold (high uncertainty)

   Arguments:
     disc      - Disc entity map
     threshold - Certainty threshold (default 0.7)

   Returns:
     true if file should be re-read to update knowledge"
  ([disc] (needs-read? disc 0.7))
  ([disc threshold]
   (or (< (current-certainty disc) threshold)
       (< (beta-lower-bound disc) (* 0.5 threshold)))))

(defn update-certainty
  "Update certainty based on observation event.

   Events and their effects:
   - :read-confirmed  → alpha += 3 (strong evidence content is accurate)
   - :hash-mismatch   → beta += 5  (strong evidence content changed)
   - :git-commit-touched → beta += 2 (moderate evidence of change)
   - :time-decay      → beta += 0.5 (mild uncertainty from time passing)

   Returns updated disc map (does not persist - call update-disc! separately)."
  [disc event]
  (let [a (or (:disc/certainty-alpha disc) 5.0)
        b (or (:disc/certainty-beta disc) 2.0)
        [new-a new-b] (case event
                        :read-confirmed     [(+ a 3) b]
                        :hash-mismatch      [a (+ b 5)]
                        :git-commit-touched [a (+ b 2)]
                        :time-decay         [a (+ b 0.5)]
                        ;; Unknown event - no change
                        [a b])]
    (assoc disc
           :disc/certainty-alpha new-a
           :disc/certainty-beta new-b)))

(defn apply-time-decay
  "Apply time-based decay to disc certainty.

   Decay rate depends on volatility class (from decay-rates map):
   - :stable   → 0.01/day (config files, deps)
   - :moderate → 0.05/day (typical source files)
   - :volatile → 0.15/day (logs, temps, build artifacts)

   The decay is proportional to days elapsed since last observation.
   Updates beta parameter: beta += rate * days_elapsed

   Arguments:
     disc - Disc entity map with Bayesian certainty fields

   Returns:
     Updated disc map with adjusted beta and refreshed last-observation.
     Does not persist - call update-disc! separately."
  [disc]
  (let [now (java.time.Instant/now)
        volatility (or (:disc/volatility-class disc) :moderate)
        rate (get decay-rates volatility 0.05)
        last-obs (:disc/last-observation disc)
        ;; Calculate days elapsed since last observation
        days-elapsed (if last-obs
                       (/ (- (.toEpochMilli now)
                             (.toEpochMilli (.toInstant ^java.util.Date last-obs)))
                          86400000.0)
                       0.0)
        ;; Only decay if time has passed
        decay-amount (* rate days-elapsed)
        current-beta (or (:disc/certainty-beta disc) 2.0)]
    (-> disc
        (assoc :disc/certainty-beta (+ current-beta decay-amount))
        (assoc :disc/last-observation (java.util.Date/from now)))))

;; =============================================================================
;; Staleness Scoring (Pure — takes pre-computed hash result)
;; =============================================================================

(defn staleness-score
  "Compute staleness score for a disc entity.
   Score ranges from 0.0 (fresh) to 1.0 (very stale).

   Factors:
   - Hash mismatch (content changed since last analysis): +0.5
   - Time since last read (>7 days: +0.3, >30 days: +0.5)
   - Never analyzed: +0.2

   Arguments:
     disc        - Disc entity map
     hash-result - Result of file-content-hash {:hash \"..\" :exists? bool}
                   Pass nil to skip hash check.

   Returns:
     Float score 0.0-1.0"
  [disc hash-result]
  (let [now-ms (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        ;; Check hash staleness
        hash-stale? (and (:exists? hash-result)
                         (:hash hash-result)
                         (:disc/content-hash disc)
                         (not= (:hash hash-result) (:disc/content-hash disc)))
        ;; Check time since last read
        last-read (:disc/last-read-at disc)
        days-since-read (when last-read
                          (/ (- now-ms (.getTime ^java.util.Date last-read)) day-ms))
        ;; Check if ever analyzed
        never-analyzed? (nil? (:disc/analyzed-at disc))]
    (min 1.0
         (+ (if hash-stale? 0.5 0.0)
            (cond
              (nil? days-since-read) 0.3
              (> days-since-read 30) 0.5
              (> days-since-read 7) 0.3
              :else 0.0)
            (if never-analyzed? 0.2 0.0)))))

(defn staleness-report
  "Compute staleness score and diagnostic info for a disc entity in one pass.

   Arguments:
     disc        - Disc entity map
     hash-result - Result of file-content-hash {:hash \"..\" :exists? bool}
                   Pass nil to skip hash check.

   Returns {:score :days-since-read :hash-mismatch? :never-analyzed?}"
  [disc hash-result]
  (let [now-ms (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        hash-mismatch? (boolean
                        (and (:exists? hash-result)
                             (:hash hash-result)
                             (:disc/content-hash disc)
                             (not= (:hash hash-result) (:disc/content-hash disc))))
        last-read (:disc/last-read-at disc)
        days-since-read (when last-read
                          (long (/ (- now-ms (.getTime ^java.util.Date last-read))
                                   day-ms)))
        never-analyzed? (nil? (:disc/analyzed-at disc))
        score (min 1.0
                   (+ (if hash-mismatch? 0.5 0.0)
                      (cond
                        (nil? days-since-read) 0.3
                        (> days-since-read 30) 0.5
                        (> days-since-read 7) 0.3
                        :else 0.0)
                      (if never-analyzed? 0.2 0.0)))]
    {:score score
     :days-since-read days-since-read
     :hash-mismatch? hash-mismatch?
     :never-analyzed? never-analyzed?}))

(defn format-staleness-warnings
  "Format staleness warnings as a text block for injection into task prompts.
   Returns nil if no warnings (zero noise for fresh files)."
  [warnings]
  (when (seq warnings)
    (str "## L1 Disc Staleness Warnings\n"
         (str/join "\n" (map :message warnings))
         "\n\n")))

;; =============================================================================
;; L1-P2 Chroma Entry Staleness (Pure)
;; =============================================================================

(defn entry-staleness-score
  "Compute staleness score for a Chroma entry based on Bayesian staleness fields.

   Uses staleness-beta to compute a score:
   - score = 1 - (alpha / (alpha + beta))
   - Higher beta = more stale

   Arguments:
     entry - Chroma entry map with :staleness-alpha, :staleness-beta

   Returns:
     Float score 0.0 (fresh) to 1.0 (very stale)"
  [entry]
  (let [alpha (or (:staleness-alpha entry) 1.0)
        beta (or (:staleness-beta entry) 1.0)]
    (- 1.0 (/ alpha (+ alpha beta)))))

(defn entry-staleness-report
  "Generate staleness report for a Chroma entry.

   Returns:
     {:id entry-id
      :score Float 0-1
      :alpha Bayesian alpha
      :beta Bayesian beta
      :source Staleness source keyword
      :depth Propagation depth
      :grounded-from Disc path if grounded}"
  [entry]
  (let [alpha (or (:staleness-alpha entry) 1.0)
        beta (or (:staleness-beta entry) 1.0)]
    {:id (:id entry)
     :score (entry-staleness-score entry)
     :alpha alpha
     :beta beta
     :source (:staleness-source entry)
     :depth (:staleness-depth entry)
     :grounded-from (:grounded-from entry)}))
