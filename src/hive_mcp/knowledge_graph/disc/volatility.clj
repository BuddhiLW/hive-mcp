(ns hive-mcp.knowledge-graph.disc.volatility
  "Pure volatility classification and staleness scoring for disc entities."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private volatility-patterns
  "File patterns for volatility classification."
  {:stable   #{#"deps\.edn$" #"project\.clj$" #"pom\.xml$" #"\.gitignore$"}
   :volatile #{#"\.log$" #"\.tmp$" #"target/" #"\.nrepl-port$"}})

(defn classify-volatility
  "Classify file volatility based on path patterns."
  [path]
  (cond
    (some #(re-find % path) (:stable volatility-patterns)) :stable
    (some #(re-find % path) (:volatile volatility-patterns)) :volatile
    :else :moderate))

(def decay-rates
  "Daily certainty decay rates by volatility class."
  {:stable 0.01
   :moderate 0.05
   :volatile 0.15})

(def staleness-decay-factor
  "Decay factor per hop in staleness propagation."
  0.5)

(def staleness-min-threshold
  "Minimum staleness to propagate."
  0.3)

(def staleness-max-depth
  "Maximum depth for staleness propagation."
  5)

(def base-staleness-values
  "Base staleness values by source event type."
  {:hash-mismatch 5.0
   :git-commit 2.0
   :time-decay 0.5})

(def initial-alpha-by-volatility
  "Initial alpha priors by volatility class."
  {:stable   7.0
   :moderate 5.0
   :volatile 3.0})

(defn current-certainty
  "Compute expected certainty from Beta distribution parameters."
  [disc]
  (let [a (or (:disc/certainty-alpha disc) 5.0)
        b (or (:disc/certainty-beta disc) 2.0)]
    (/ a (+ a b))))

(defn beta-lower-bound
  "Lower bound of 95% credible interval for certainty."
  [disc]
  (let [a (or (:disc/certainty-alpha disc) 5.0)
        b (or (:disc/certainty-beta disc) 2.0)
        sum (+ a b)
        mean (/ a sum)
        variance (/ (* a b) (* sum sum (+ sum 1)))
        std-dev (Math/sqrt variance)]
    (max 0.0 (- mean (* 2 std-dev)))))

(defn needs-read?
  "True if certainty below threshold or credible interval too wide."
  ([disc] (needs-read? disc 0.7))
  ([disc threshold]
   (or (< (current-certainty disc) threshold)
       (< (beta-lower-bound disc) (* 0.5 threshold)))))

(defn update-certainty
  "Update certainty based on an observation event."
  [disc event]
  (let [a (or (:disc/certainty-alpha disc) 5.0)
        b (or (:disc/certainty-beta disc) 2.0)
        [new-a new-b] (case event
                        :read-confirmed     [(+ a 3) b]
                        :hash-mismatch      [a (+ b 5)]
                        :git-commit-touched [a (+ b 2)]
                        :time-decay         [a (+ b 0.5)]
                        [a b])]
    (assoc disc
           :disc/certainty-alpha new-a
           :disc/certainty-beta new-b)))

(defn apply-time-decay
  "Apply time-based decay to disc certainty using volatility-appropriate rates."
  [disc]
  (let [now (java.time.Instant/now)
        volatility (or (:disc/volatility-class disc) :moderate)
        rate (get decay-rates volatility 0.05)
        last-obs (:disc/last-observation disc)
        days-elapsed (if last-obs
                       (/ (- (.toEpochMilli now)
                             (.toEpochMilli (.toInstant ^java.util.Date last-obs)))
                          86400000.0)
                       0.0)
        decay-amount (* rate days-elapsed)
        current-beta (or (:disc/certainty-beta disc) 2.0)]
    (-> disc
        (assoc :disc/certainty-beta (+ current-beta decay-amount))
        (assoc :disc/last-observation (java.util.Date/from now)))))

(defn staleness-score
  "Compute staleness score for a disc entity (0.0-1.0)."
  [disc hash-result]
  (let [now-ms (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        hash-stale? (and (:exists? hash-result)
                         (:hash hash-result)
                         (:disc/content-hash disc)
                         (not= (:hash hash-result) (:disc/content-hash disc)))
        last-read (:disc/last-read-at disc)
        days-since-read (when last-read
                          (/ (- now-ms (.getTime ^java.util.Date last-read)) day-ms))
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
  "Compute staleness score and diagnostic info for a disc entity."
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
  "Format staleness warnings as a text block for task prompts."
  [warnings]
  (when (seq warnings)
    (str "## file-level Staleness Warnings\n"
         (str/join "\n" (map :message warnings))
         "\n\n")))

(defn entry-staleness-score
  "Compute staleness score for a Chroma entry from Bayesian fields."
  [entry]
  (let [alpha (or (:staleness-alpha entry) 1.0)
        beta (or (:staleness-beta entry) 1.0)]
    (- 1.0 (/ alpha (+ alpha beta)))))

(defn entry-staleness-report
  "Generate staleness report for a Chroma entry."
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
