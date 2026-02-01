(ns hive-mcp.knowledge-graph.disc
  "Disc entity management for L1 (file) abstraction level.

   Disc entities track the actual state of files on disk, enabling:
   - Grounding verification without re-reading files
   - Change detection via content hash comparison
   - Git commit tracking for provenance
   - Bayesian certainty tracking with automatic event wiring

   CLARITY-Y: Graceful failure with status codes instead of exceptions."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.chroma :as chroma]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.security MessageDigest]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Forward declarations for mutually-referencing functions
(declare propagate-staleness!)

;; =============================================================================
;; Hash Utilities
;; =============================================================================

(defn compute-hash
  "Compute SHA-256 hash of content string.
   Returns hex string."
  [content]
  (let [md (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest md (.getBytes (str content) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) hash-bytes))))

(defn file-content-hash
  "Read file and compute content hash.
   Returns {:hash \"..\" :exists? true} or {:exists? false}."
  [path]
  (try
    (let [file (io/file path)]
      (if (.exists file)
        {:hash (compute-hash (slurp file)) :exists? true}
        {:exists? false}))
    (catch Exception e
      (log/warn "Failed to hash file" {:path path :error (.getMessage e)})
      {:exists? false :error (.getMessage e)})))

;; =============================================================================
;; Volatility Classification
;; =============================================================================

(def volatility-patterns
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

(def propagation-relations
  "KG edge types that should propagate staleness transitively."
  #{:depends-on :implements :derived-from :refines})

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
;; Disc Entity CRUD
;; =============================================================================

(def ^:private initial-alpha-by-volatility
  "Initial alpha priors by volatility class.
   Higher alpha = more confident starting certainty.
   Stable files start more confident since they rarely change."
  {:stable   7.0   ;; 7/(7+2) = 0.78 initial certainty
   :moderate 5.0   ;; 5/(5+2) = 0.71 initial certainty
   :volatile 3.0}) ;; 3/(3+2) = 0.60 initial certainty

(defn add-disc!
  "Create or update a disc entity for a file path.

   Initializes Bayesian certainty fields based on file volatility:
   - Classifies volatility from path patterns (:stable/:moderate/:volatile)
   - Sets initial alpha/beta priors (higher alpha for stable files)
   - Sets last-observation to now

   Arguments:
     opts - Map with:
       :path         - File path (required, unique identity)
       :content-hash - SHA256 of file content
       :analyzed-at  - Timestamp of analysis (defaults to now)
       :git-commit   - Git commit hash when analyzed
       :project-id   - Project scope

   Returns the entity ID."
  [{:keys [path content-hash analyzed-at git-commit project-id]}]
  {:pre [(string? path) (seq path)]}
  (let [now (java.util.Date.)
        volatility (classify-volatility path)
        initial-alpha (get initial-alpha-by-volatility volatility 5.0)
        tx-data [{:disc/path path
                  :disc/content-hash (or content-hash "")
                  :disc/analyzed-at (or analyzed-at now)
                  :disc/git-commit (or git-commit "")
                  :disc/project-id (or project-id "global")
                  ;; Initialize Bayesian certainty fields
                  :disc/certainty-alpha initial-alpha
                  :disc/certainty-beta 2.0
                  :disc/volatility-class volatility
                  :disc/last-observation now}]
        result (conn/transact! tx-data)]
    (log/debug "Added/updated disc entity" {:path path :volatility volatility
                                            :initial-certainty (/ initial-alpha (+ initial-alpha 2.0))})
    ;; Return the entity ID
    (-> result :tx-data first :e)))

(defn get-disc
  "Get disc entity by file path.
   Returns entity map or nil if not found."
  [path]
  {:pre [(string? path)]}
  (let [result (conn/query '[:find (pull ?e [*])
                             :in $ ?path
                             :where [?e :disc/path ?path]]
                           path)]
    (ffirst result)))

(defn get-disc-by-id
  "Get disc entity by entity ID.
   Returns entity map or nil if not found."
  [eid]
  (when-let [e (conn/entity eid)]
    (when (:disc/path e)
      (into {} e))))

(defn disc-exists?
  "Check if a disc entity exists for the given path."
  [path]
  (some? (get-disc path)))

(defn update-disc!
  "Update a disc entity.
   Path is used to find the entity; other fields are updated."
  [path updates]
  {:pre [(string? path)]}
  (when-let [_existing (get-disc path)]
    (let [tx-data [(merge {:disc/path path} updates)]
          _ (conn/transact! tx-data)]
      (log/debug "Updated disc entity" {:path path :updates (keys updates)})
      (get-disc path))))

(defn remove-disc!
  "Remove a disc entity by path.
   Returns true if removed, nil if not found."
  [path]
  {:pre [(string? path)]}
  (when-let [disc (get-disc path)]
    (let [eid (:db/id disc)]
      (conn/transact! [[:db.fn/retractEntity eid]])
      (log/debug "Removed disc entity" {:path path})
      true)))

;; =============================================================================
;; Disc Queries
;; =============================================================================

(defn get-all-discs
  "Get all disc entities.
   Optional project-id filter."
  [& {:keys [project-id]}]
  (let [results (if project-id
                  (conn/query '[:find (pull ?e [*])
                                :in $ ?pid
                                :where
                                [?e :disc/path _]
                                [?e :disc/project-id ?pid]]
                              project-id)
                  (conn/query '[:find (pull ?e [*])
                                :where [?e :disc/path _]]))]
    (map first results)))

(defn get-stale-discs
  "Get disc entities with content hash mismatch.
   Computes current file hash and compares with stored hash.
   Returns seq of {:disc ... :current-hash ... :stale? true/false}."
  [& {:keys [project-id]}]
  (let [discs (get-all-discs :project-id project-id)]
    (->> discs
         (map (fn [disc]
                (let [path (:disc/path disc)
                      stored-hash (:disc/content-hash disc)
                      {:keys [hash exists?]} (file-content-hash path)
                      stale? (or (not exists?)
                                 (and hash stored-hash (not= hash stored-hash)))]
                  {:disc disc
                   :current-hash hash
                   :exists? exists?
                   :stale? stale?})))
         (filter :stale?)
         vec)))

(defn refresh-disc!
  "Refresh a disc entity by re-reading the file.
   Updates content-hash and analyzed-at.
   Returns {:status :refreshed|:not-found|:file-missing :disc ...}."
  [path & {:keys [git-commit]}]
  {:pre [(string? path)]}
  (let [{:keys [hash exists?]} (file-content-hash path)]
    (cond
      (not exists?)
      {:status :file-missing :path path}

      :else
      (let [updates {:disc/content-hash hash
                     :disc/analyzed-at (java.util.Date.)}
            updates (if git-commit
                      (assoc updates :disc/git-commit git-commit)
                      updates)]
        (if (disc-exists? path)
          (do (update-disc! path updates)
              {:status :refreshed :disc (get-disc path)})
          (do (add-disc! (merge {:path path :content-hash hash} updates))
              {:status :created :disc (get-disc path)}))))))

;; =============================================================================
;; Read Tracking
;; =============================================================================

(defn touch-disc!
  "Record that a file was read by an agent.
   Creates the disc entity if it doesn't exist, updates last-read-at and
   increments read-count. Returns the updated disc entity.

   Arguments:
     path       - File path (required)
     project-id - Project scope (optional, defaults to 'global')"
  [path & {:keys [project-id]}]
  {:pre [(string? path) (seq path)]}
  (let [now (java.util.Date.)]
    (if (disc-exists? path)
      ;; Update existing: bump last-read-at and read-count
      (let [existing (get-disc path)
            current-count (or (:disc/read-count existing) 0)]
        (update-disc! path {:disc/last-read-at now
                            :disc/read-count (inc current-count)})
        (get-disc path))
      ;; Create new: also compute content hash for fresh tracking
      (let [{:keys [hash]} (file-content-hash path)]
        (add-disc! {:path path
                    :content-hash (or hash "")
                    :project-id (or project-id "global")})
        (update-disc! path {:disc/last-read-at now
                            :disc/read-count 1})
        (get-disc path)))))

(defn staleness-score
  "Compute staleness score for a disc entity.
   Score ranges from 0.0 (fresh) to 1.0 (very stale).

   Factors:
   - Hash mismatch (content changed since last analysis): +0.5
   - Time since last read (>7 days: +0.3, >30 days: +0.5)
   - Never analyzed: +0.2

   Arguments:
     disc - Disc entity map

   Returns:
     Float score 0.0-1.0"
  [disc]
  (let [now-ms (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        ;; Check hash staleness
        path (:disc/path disc)
        {:keys [hash exists?]} (when path (file-content-hash path))
        hash-stale? (and exists? hash (:disc/content-hash disc)
                         (not= hash (:disc/content-hash disc)))
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

;; =============================================================================
;; L1 Disc Surfacing — Proactive Staleness Reporting
;; =============================================================================

(defn staleness-report
  "Compute staleness score and diagnostic info for a disc entity in one pass.
   Avoids redundant file I/O compared to calling staleness-score separately.

   Returns {:score :days-since-read :hash-mismatch? :never-analyzed?}"
  [disc]
  (let [now-ms (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        path (:disc/path disc)
        {:keys [hash exists?]} (when path (file-content-hash path))
        hash-mismatch? (boolean
                        (and exists? hash (:disc/content-hash disc)
                             (not= hash (:disc/content-hash disc))))
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

(defn staleness-warnings
  "Generate staleness warnings for a collection of file paths.
   Only returns warnings for files with existing disc entities that are stale.
   Fresh files and files without disc entities produce no output (zero noise).

   Arguments:
     paths - Collection of file path strings

   Returns:
     Vector of {:path :staleness :message} for stale files only (staleness > 0.3)"
  [paths]
  (when (seq paths)
    (->> paths
         (keep (fn [path]
                 (when-let [disc (get-disc path)]
                   (let [{:keys [score days-since-read hash-mismatch?]}
                         (staleness-report disc)]
                     (when (> score 0.3)
                       {:path path
                        :staleness score
                        :message (format "NOTE: file %s is stale (staleness: %.1f%s%s). Re-read carefully."
                                         path
                                         (float score)
                                         (if days-since-read
                                           (format ", last read %dd ago" days-since-read)
                                           ", never read")
                                         (if hash-mismatch?
                                           ", hash mismatch"
                                           ""))})))))
         vec)))

(defn format-staleness-warnings
  "Format staleness warnings as a text block for injection into task prompts.
   Returns nil if no warnings (zero noise for fresh files)."
  [warnings]
  (when (seq warnings)
    (str "## L1 Disc Staleness Warnings\n"
         (str/join "\n" (map :message warnings))
         "\n\n")))

(defn top-stale-files
  "Query top-N most stale disc entities.
   Returns vector of {:path :score :days-since-read :hash-mismatch?}
   sorted by staleness score descending.

   Arguments:
     n          - Max entries to return (default: 5)
     project-id - Optional project filter
     threshold  - Minimum staleness score (default: 0.5)"
  [& {:keys [n project-id threshold] :or {n 5 threshold 0.5}}]
  (let [discs (get-all-discs :project-id project-id)]
    (->> discs
         (map (fn [disc]
                (let [report (staleness-report disc)]
                  (assoc report :path (:disc/path disc)))))
         (filter #(> (:score %) threshold))
         (sort-by :score >)
         (take n)
         vec)))

;; =============================================================================
;; KG-First Context — Read the Map Before the Territory
;; =============================================================================

(defn- classify-disc
  "Classify a single file path's KG status.
   Returns a map with :path, :disc, :status (:fresh | :stale | :missing),
   and diagnostic fields.

   Staleness threshold: files with score <= threshold are considered fresh.
   Default threshold: 0.3 (matches staleness-warnings threshold)."
  [path staleness-threshold]
  (try
    (if-let [disc (get-disc path)]
      (let [{:keys [score days-since-read hash-mismatch? never-analyzed?]}
            (staleness-report disc)]
        {:path path
         :disc disc
         :status (if (<= score staleness-threshold) :fresh :stale)
         :staleness-score score
         :days-since-read days-since-read
         :hash-mismatch? hash-mismatch?
         :never-analyzed? never-analyzed?
         :read-count (or (:disc/read-count disc) 0)
         :last-read-at (:disc/last-read-at disc)})
      ;; No disc entity — file has never been tracked
      {:path path
       :disc nil
       :status :missing
       :staleness-score 1.0
       :days-since-read nil
       :hash-mismatch? false
       :never-analyzed? true
       :read-count 0
       :last-read-at nil})
    (catch Exception e
      (log/warn "KG classify-disc failed, treating as missing"
                {:path path :error (.getMessage e)})
      {:path path
       :disc nil
       :status :missing
       :staleness-score 1.0
       :days-since-read nil
       :hash-mismatch? false
       :never-analyzed? true
       :read-count 0
       :last-read-at nil})))

(defn kg-first-context
  "Consult the KG before file reads — the Structural Differential principle.
   Labels (KG) before disc (files).

   Takes a collection of file paths and classifies each based on KG freshness:
   - :kg-known  — Files with fresh KG data (skip file read, use KG knowledge)
   - :needs-read — Files with no KG data (must read from disc)
   - :stale     — Files with stale KG data (should re-read from disc)

   Arguments:
     paths - Collection of file path strings
     opts  - Optional map:
       :staleness-threshold - Score cutoff for fresh vs stale (default: 0.3)
                              0.0 = only perfectly fresh, 1.0 = everything is fresh

   Returns:
     {:kg-known   {path {:disc ... :staleness-score ... :read-count ...} ...}
      :needs-read [path ...]
      :stale      [path ...]
      :summary    {:total N :known N :needs-read N :stale N}}

   CLARITY-Y: Individual file failures are logged and treated as :needs-read.
   CLARITY-A: Single-pass classification, no redundant file I/O."
  [paths & [{:keys [staleness-threshold] :or {staleness-threshold 0.3}}]]
  (let [unique-paths (distinct (filter (every-pred string? seq) paths))
        classified (mapv #(classify-disc % staleness-threshold) unique-paths)
        grouped (group-by :status classified)
        ;; Build the result maps
        kg-known (->> (:fresh grouped)
                      (reduce (fn [m entry]
                                (assoc m (:path entry)
                                       (dissoc entry :path :status)))
                              {}))
        needs-read (mapv :path (:missing grouped))
        stale (mapv :path (:stale grouped))]
    {:kg-known kg-known
     :needs-read needs-read
     :stale stale
     :summary {:total (count unique-paths)
               :known (count kg-known)
               :needs-read (count needs-read)
               :stale (count stale)}}))

;; =============================================================================
;; Disc Statistics
;; =============================================================================

(defn disc-stats
  "Get statistics about disc entities.
   Returns {:total int :by-project {...} :stale-count int}."
  []
  (let [all-discs (get-all-discs)
        stale (get-stale-discs)]
    {:total (count all-discs)
     :by-project (frequencies (map :disc/project-id all-discs))
     :stale-count (count stale)}))

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

(defn apply-time-decay-to-all-discs!
  "Apply time decay to all disc entities in DataScript.

   Iterates through all discs and applies time-based certainty decay
   based on each disc's volatility class. Persists updates to DataScript.

   Arguments:
     project-id - Optional project filter (nil = all projects)

   Returns:
     {:updated int :skipped int :errors int}

   Use for periodic background maintenance of certainty scores."
  [& {:keys [project-id]}]
  (let [discs (get-all-discs :project-id project-id)
        results (reduce
                 (fn [acc disc]
                   (try
                     (let [path (:disc/path disc)
                           decayed (apply-time-decay disc)
                           ;; Only persist the certainty fields
                           updates {:disc/certainty-beta (:disc/certainty-beta decayed)
                                    :disc/last-observation (:disc/last-observation decayed)}]
                       (update-disc! path updates)
                       (update acc :updated inc))
                     (catch Exception e
                       (log/warn "Failed to apply decay to disc"
                                 {:path (:disc/path disc) :error (.getMessage e)})
                       (update acc :errors inc))))
                 {:updated 0 :skipped 0 :errors 0}
                 discs)]
    (log/info "Time decay applied to discs" results)
    results))

;; =============================================================================
;; Certainty Event Wiring
;; =============================================================================
;;
;; These functions wire observation events into certainty updates automatically.
;; - reground-disc! triggers :read-confirmed or :hash-mismatch
;; - on-git-commit-touched triggers :git-commit-touched

(defn update-disc-certainty!
  "Update certainty for a disc entity based on observation event and persist.

   This is the central function for wiring observation events to certainty updates.
   It handles both the Bayesian update and persistence in one atomic operation.

   Arguments:
     path  - File path of the disc entity
     event - Observation event keyword:
             :read-confirmed, :hash-mismatch, :git-commit-touched, :time-decay

   Returns:
     Updated disc entity map, or nil if disc not found.

   CLARITY-Y: Returns nil instead of throwing for missing discs."
  [path event]
  {:pre [(string? path) (keyword? event)]}
  (when-let [disc (get-disc path)]
    (let [updated (update-certainty disc event)
          now (java.util.Date.)
          certainty-updates {:disc/certainty-alpha (:disc/certainty-alpha updated)
                             :disc/certainty-beta (:disc/certainty-beta updated)
                             :disc/last-observation now}]
      (update-disc! path certainty-updates)
      (log/debug "Updated disc certainty" {:path path
                                           :event event
                                           :new-certainty (current-certainty updated)})
      (get-disc path))))

(defn reground-disc!
  "Re-ground a disc entity by verifying against the actual file on disk.

   Compares stored content-hash with current file hash:
   - If hashes match: triggers :read-confirmed (alpha += 3)
   - If hashes differ: triggers :hash-mismatch (beta += 5) and updates stored hash

   Also updates last-observation timestamp for time decay calculations.

   Arguments:
     path        - File path of the disc entity
     git-commit  - Optional git commit hash for tracking

   Returns:
     {:status    :regrounded|:hash-mismatch|:file-missing|:not-found
      :disc      Updated disc entity (if successful)
      :certainty New certainty value
      :old-hash  Previous stored hash
      :new-hash  Current file hash}

   CLARITY-Y: Status codes instead of exceptions for all outcomes."
  [path & {:keys [git-commit]}]
  {:pre [(string? path)]}
  (let [disc (get-disc path)]
    (cond
      ;; Disc not found
      (nil? disc)
      {:status :not-found :path path}

      ;; Check file
      :else
      (let [{:keys [hash exists?]} (file-content-hash path)
            stored-hash (:disc/content-hash disc)]
        (cond
          ;; File doesn't exist
          (not exists?)
          {:status :file-missing :path path :disc disc}

          ;; Hashes match - content confirmed
          (= hash stored-hash)
          (let [updated-disc (update-disc-certainty! path :read-confirmed)
                ;; Also update analyzed-at
                _ (update-disc! path (merge {:disc/analyzed-at (java.util.Date.)}
                                            (when git-commit {:disc/git-commit git-commit})))
                ;; Propagate staleness if git-commit provided
                _ (when git-commit (propagate-staleness! path 2.0 :git-commit))]
            {:status :regrounded
             :disc (get-disc path)
             :certainty (current-certainty updated-disc)
             :old-hash stored-hash
             :new-hash hash})

          ;; Hash mismatch - content changed
          :else
          (let [_ (update-disc-certainty! path :hash-mismatch)
                ;; Update with new hash and timestamp
                _ (update-disc! path (merge {:disc/content-hash hash
                                             :disc/analyzed-at (java.util.Date.)}
                                            (when git-commit {:disc/git-commit git-commit})))
                _ (propagate-staleness! path 5.0 :hash-mismatch)
                updated-disc (get-disc path)]
            {:status :hash-mismatch
             :disc updated-disc
             :certainty (current-certainty updated-disc)
             :old-hash stored-hash
             :new-hash hash}))))))

(defn on-git-commit-touched
  "Called when a git commit affects a disc's file.

   This is an integration point for git hooks or watchers to notify
   the knowledge graph that a file has been modified by a commit.

   The certainty is updated with :git-commit-touched event (beta += 2),
   signaling moderate evidence that the file content may have changed.

   Arguments:
     path       - File path that was touched by the commit
     git-commit - Git commit hash (optional, for tracking)

   Returns:
     {:status :updated|:not-found|:created
      :disc   Updated/created disc entity
      :certainty New certainty value}

   If the disc doesn't exist yet, creates it with the git commit info."
  [path & {:keys [git-commit]}]
  {:pre [(string? path)]}
  (if-let [_disc (get-disc path)]
    ;; Existing disc - update certainty
    (let [updated-disc (update-disc-certainty! path :git-commit-touched)
          ;; Also update git-commit if provided
          _ (when git-commit
              (update-disc! path {:disc/git-commit git-commit}))
          ;; Propagate staleness from git commit event
          _ (propagate-staleness! path 2.0 :git-commit)]
      {:status :updated
       :disc (get-disc path)
       :certainty (current-certainty updated-disc)})
    ;; No disc yet - create one
    (let [{:keys [hash exists?]} (file-content-hash path)]
      (when exists?
        (add-disc! {:path path
                    :content-hash hash
                    :git-commit (or git-commit "")
                    :project-id "global"})
        ;; Apply git-commit-touched event to the new disc
        (let [updated-disc (update-disc-certainty! path :git-commit-touched)]
          {:status :created
           :disc updated-disc
           :certainty (current-certainty updated-disc)})))))

(defn reground-stale-discs!
  "Re-ground all discs that need verification.

   Iterates through discs where certainty has fallen below threshold
   and triggers reground-disc! for each.

   Arguments:
     threshold  - Certainty threshold (default: 0.7)
     project-id - Optional project filter
     limit      - Max discs to reground (default: 50)

   Returns:
     {:processed int
      :regrounded int
      :mismatches int
      :missing int
      :errors int
      :results [...]}

   CLARITY-A: Batched processing with configurable limits."
  [& {:keys [threshold project-id limit] :or {threshold 0.7 limit 50}}]
  (let [discs (get-all-discs :project-id project-id)
        needs-reground (filter #(needs-read? % threshold) discs)
        to-process (take limit needs-reground)
        results (doall
                 (for [disc to-process]
                   (try
                     (reground-disc! (:disc/path disc))
                     (catch Exception e
                       {:status :error
                        :path (:disc/path disc)
                        :error (.getMessage e)}))))
        by-status (frequencies (map :status results))]
    (log/info "Reground stale discs complete"
              {:processed (count to-process)
               :by-status by-status})
    {:processed (count to-process)
     :regrounded (get by-status :regrounded 0)
     :mismatches (get by-status :hash-mismatch 0)
     :missing (get by-status :file-missing 0)
     :errors (get by-status :error 0)
     :results results}))

;; =============================================================================
;; L1-P2 Transitive Staleness Propagation
;; =============================================================================

(defn apply-transitive-staleness!
  "Apply staleness to a single Chroma entry with decay based on depth.

   Arguments:
     entry-id         - Chroma entry ID to update
     base-staleness   - Base staleness value before decay
     depth            - Propagation depth (0 = source, 1 = direct dependent, etc.)
     staleness-source - Source event type keyword

   Returns:
     {:status :updated|:skipped|:error
      :entry-id entry-id
      :beta-increment amount added to staleness-beta}

   Skips update if decayed staleness is below staleness-min-threshold."
  [entry-id base-staleness depth staleness-source]
  (let [beta-increment (* base-staleness
                          (Math/pow staleness-decay-factor depth))]
    (if (< beta-increment staleness-min-threshold)
      {:status :skipped :entry-id entry-id :beta-increment 0}
      (try
        (let [current-entry (chroma/get-entry-by-id entry-id)
              current-beta (or (:staleness-beta current-entry) 1.0)
              new-beta (+ current-beta beta-increment)]
          (chroma/update-entry! entry-id
                                {:staleness-beta new-beta
                                 :staleness-depth depth
                                 :staleness-source (name staleness-source)})
          (log/debug "Applied transitive staleness"
                     {:entry-id entry-id :depth depth :beta-increment beta-increment})
          {:status :updated :entry-id entry-id :beta-increment beta-increment})
        (catch Exception e
          (log/warn "Failed to apply staleness to entry"
                    {:entry-id entry-id :error (.getMessage e)})
          {:status :error :entry-id entry-id :error (.getMessage e)})))))

(defn propagate-staleness!
  "Propagate staleness from a disc to dependent labels via KG edges.
   Uses Bayesian beta update with 0.5^depth decay.

   Algorithm:
   1. Query Chroma for entries WHERE grounded-from = disc-path
   2. For each grounded entry, update it at depth 0 (source)
   3. Call impact-analysis to find dependents via propagation-relations
   4. Update direct dependents at depth 1
   5. Update transitive dependents at depth 2 (simplified - actual depth may vary)
   6. Only propagate if beta-increment >= staleness-min-threshold

   Arguments:
     disc-path        - File path of the disc entity triggering staleness
     base-staleness   - Base staleness value (from base-staleness-values map)
     staleness-source - Source event type (e.g., :hash-mismatch, :git-commit, :time-decay)

   Returns:
     {:propagated int :skipped int :errors int :grounded int}

   Notes:
     - Uses staleness-decay-factor (0.5), staleness-max-depth (5), and staleness-min-threshold (0.3)
     - Only propagates through edges in propagation-relations set
     - Direct dependents receive staleness at depth 1, transitive at depth 2"
  [disc-path base-staleness staleness-source]
  (try
    ;; Step 1: Query Chroma for entries grounded from this disc
    (let [grounded-entries (chroma/query-grounded-from disc-path)
          results (atom {:propagated 0 :skipped 0 :errors 0 :grounded 0})]

      (doseq [entry grounded-entries]
        (let [entry-id (:id entry)]
          ;; Step 2: Update the grounded entry itself at depth 0
          (let [result (apply-transitive-staleness! entry-id base-staleness 0 staleness-source)]
            (case (:status result)
              :updated (swap! results update :grounded inc)
              :skipped (swap! results update :skipped inc)
              :error (swap! results update :errors inc)))

          ;; Step 3: Run impact analysis to find dependents
          (try
            (let [{:keys [direct transitive]}
                  (queries/impact-analysis entry-id
                                           {:max-depth staleness-max-depth})]

              ;; Step 4: Update direct dependents at depth 1
              (doseq [dep-id direct]
                (let [result (apply-transitive-staleness! dep-id base-staleness 1 staleness-source)]
                  (case (:status result)
                    :updated (swap! results update :propagated inc)
                    :skipped (swap! results update :skipped inc)
                    :error (swap! results update :errors inc))))

              ;; Step 5: Update transitive dependents at depth 2
              ;; (simplified - actual depth may be higher but we use 2 for decay calculation)
              (doseq [trans-id transitive]
                (let [result (apply-transitive-staleness! trans-id base-staleness 2 staleness-source)]
                  (case (:status result)
                    :updated (swap! results update :propagated inc)
                    :skipped (swap! results update :skipped inc)
                    :error (swap! results update :errors inc)))))

            (catch Exception e
              (log/warn "Impact analysis failed for entry"
                        {:entry-id entry-id :error (.getMessage e)})
              (swap! results update :errors inc)))))

      (log/info "Staleness propagation complete"
                {:disc-path disc-path :results @results})
      @results)

    (catch Exception e
      (log/warn "Failed to propagate staleness"
                {:disc-path disc-path :error (.getMessage e)})
      {:propagated 0 :skipped 0 :errors 1 :grounded 0})))

;; =============================================================================
;; L1-P2 Chroma Entry Staleness Report
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

(defn stale-entries-report
  "Query Chroma for entries with staleness above threshold.

   Arguments:
     threshold  - Minimum staleness score (default: 0.3)
     limit      - Max entries to return (default: 20)
     project-id - Optional project filter

   Returns:
     Vector of {:id :score :source :depth :grounded-from} sorted by score desc"
  [& {:keys [threshold limit project-id] :or {threshold 0.3 limit 20}}]
  (try
    (let [entries (chroma/query-entries :project-id project-id :limit 1000)]
      (->> entries
           (map entry-staleness-report)
           (filter #(> (:score %) threshold))
           (sort-by :score >)
           (take limit)
           vec))
    (catch Exception e
      (log/warn "Failed to generate stale entries report" {:error (.getMessage e)})
      [])))
