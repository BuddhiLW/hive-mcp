(ns hive-mcp.knowledge-graph.disc-test
  "Unit tests for disc entity CRUD operations.

   Tests cover:
   - add-disc! creation and updates
   - get-disc by path
   - update-disc! modifications
   - remove-disc! deletion
   - disc-stats and stale detection

   Each test uses a fresh DataScript connection via fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each fixtures/datascript-fixture)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn gen-path
  "Generate a unique file path for testing."
  []
  (str "/test/path/" (subs (str (java.util.UUID/randomUUID)) 0 8) ".clj"))

;; =============================================================================
;; Hash Utility Tests
;; =============================================================================

(deftest compute-hash-test
  (testing "compute-hash returns consistent SHA-256 hex string"
    (let [hash1 (disc/compute-hash "hello world")
          hash2 (disc/compute-hash "hello world")]
      (is (string? hash1))
      (is (= 64 (count hash1)))  ; SHA-256 = 32 bytes = 64 hex chars
      (is (= hash1 hash2)))))

(deftest compute-hash-different-content-test
  (testing "compute-hash returns different hashes for different content"
    (let [hash1 (disc/compute-hash "hello")
          hash2 (disc/compute-hash "world")]
      (is (not= hash1 hash2)))))

;; =============================================================================
;; add-disc! Tests
;; =============================================================================

(deftest add-disc-creates-entity-test
  (testing "add-disc! creates a new disc entity"
    (let [path (gen-path)
          eid (disc/add-disc! {:path path :content-hash "abc123"})]
      (is (number? eid))
      (is (disc/disc-exists? path)))))

(deftest add-disc-stores-all-fields-test
  (testing "add-disc! stores all provided fields"
    (let [path (gen-path)
          analyzed-at (java.util.Date.)
          _ (disc/add-disc! {:path path
                             :content-hash "hash123"
                             :analyzed-at analyzed-at
                             :git-commit "abc1234"
                             :project-id "test-project"})
          disc (disc/get-disc path)]
      (is (= path (:disc/path disc)))
      (is (= "hash123" (:disc/content-hash disc)))
      (is (= analyzed-at (:disc/analyzed-at disc)))
      (is (= "abc1234" (:disc/git-commit disc)))
      (is (= "test-project" (:disc/project-id disc))))))

(deftest add-disc-defaults-project-id-test
  (testing "add-disc! defaults project-id to 'global'"
    (let [path (gen-path)
          _ (disc/add-disc! {:path path :content-hash "test"})
          disc (disc/get-disc path)]
      (is (= "global" (:disc/project-id disc))))))

(deftest add-disc-upserts-on-same-path-test
  (testing "add-disc! upserts when path already exists"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "hash1"})
      (disc/add-disc! {:path path :content-hash "hash2"})
      (let [disc (disc/get-disc path)]
        (is (= "hash2" (:disc/content-hash disc)))))))

(deftest add-disc-rejects-nil-path-test
  (testing "add-disc! rejects nil path"
    (is (thrown? AssertionError
                 (disc/add-disc! {:path nil :content-hash "test"})))))

(deftest add-disc-rejects-empty-path-test
  (testing "add-disc! rejects empty path"
    (is (thrown? AssertionError
                 (disc/add-disc! {:path "" :content-hash "test"})))))

;; =============================================================================
;; get-disc Tests
;; =============================================================================

(deftest get-disc-returns-entity-test
  (testing "get-disc returns the disc entity for a path"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (let [disc (disc/get-disc path)]
        (is (some? disc))
        (is (= path (:disc/path disc)))))))

(deftest get-disc-returns-nil-for-nonexistent-test
  (testing "get-disc returns nil for non-existent path"
    (is (nil? (disc/get-disc "/nonexistent/path.clj")))))

;; =============================================================================
;; update-disc! Tests
;; =============================================================================

(deftest update-disc-modifies-entity-test
  (testing "update-disc! modifies the disc entity"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "old-hash"})
      (disc/update-disc! path {:disc/content-hash "new-hash"})
      (let [disc (disc/get-disc path)]
        (is (= "new-hash" (:disc/content-hash disc)))))))

(deftest update-disc-returns-updated-entity-test
  (testing "update-disc! returns the updated entity"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "hash1"})
      (let [updated (disc/update-disc! path {:disc/git-commit "commit123"})]
        (is (= "commit123" (:disc/git-commit updated)))))))

(deftest update-disc-returns-nil-for-nonexistent-test
  (testing "update-disc! returns nil for non-existent path"
    (is (nil? (disc/update-disc! "/nonexistent/path.clj" {:disc/content-hash "new"})))))

;; =============================================================================
;; remove-disc! Tests
;; =============================================================================

(deftest remove-disc-deletes-entity-test
  (testing "remove-disc! deletes the disc entity"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (is (disc/disc-exists? path))
      (disc/remove-disc! path)
      (is (not (disc/disc-exists? path))))))

(deftest remove-disc-returns-true-on-success-test
  (testing "remove-disc! returns true on success"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (is (true? (disc/remove-disc! path))))))

(deftest remove-disc-returns-nil-for-nonexistent-test
  (testing "remove-disc! returns nil for non-existent path"
    (is (nil? (disc/remove-disc! "/nonexistent/path.clj")))))

;; =============================================================================
;; get-all-discs Tests
;; =============================================================================

(deftest get-all-discs-returns-all-test
  (testing "get-all-discs returns all disc entities"
    (let [path1 (gen-path)
          path2 (gen-path)]
      (disc/add-disc! {:path path1 :content-hash "hash1"})
      (disc/add-disc! {:path path2 :content-hash "hash2"})
      (let [all (disc/get-all-discs)]
        (is (= 2 (count all)))))))

(deftest get-all-discs-filters-by-project-test
  (testing "get-all-discs filters by project-id"
    (let [path1 (gen-path)
          path2 (gen-path)]
      (disc/add-disc! {:path path1 :content-hash "hash1" :project-id "project-a"})
      (disc/add-disc! {:path path2 :content-hash "hash2" :project-id "project-b"})
      (let [project-a-discs (disc/get-all-discs :project-id "project-a")]
        (is (= 1 (count project-a-discs)))
        (is (= "project-a" (:disc/project-id (first project-a-discs))))))))

(deftest get-all-discs-empty-when-no-discs-test
  (testing "get-all-discs returns empty when no disc entities"
    (is (empty? (disc/get-all-discs)))))

;; =============================================================================
;; disc-stats Tests
;; =============================================================================

(deftest disc-stats-total-count-test
  (testing "disc-stats reports total count"
    (disc/add-disc! {:path (gen-path) :content-hash "hash1"})
    (disc/add-disc! {:path (gen-path) :content-hash "hash2"})
    (let [stats (disc/disc-stats)]
      (is (= 2 (:total stats))))))

(deftest disc-stats-by-project-test
  (testing "disc-stats groups by project"
    (disc/add-disc! {:path (gen-path) :content-hash "h1" :project-id "proj-a"})
    (disc/add-disc! {:path (gen-path) :content-hash "h2" :project-id "proj-a"})
    (disc/add-disc! {:path (gen-path) :content-hash "h3" :project-id "proj-b"})
    (let [stats (disc/disc-stats)]
      (is (= 2 (get-in stats [:by-project "proj-a"])))
      (is (= 1 (get-in stats [:by-project "proj-b"]))))))

(deftest disc-stats-empty-db-test
  (testing "disc-stats handles empty database"
    (let [stats (disc/disc-stats)]
      (is (= 0 (:total stats)))
      (is (= 0 (:stale-count stats)))
      (is (empty? (:by-project stats))))))

;; =============================================================================
;; disc-exists? Tests
;; =============================================================================

(deftest disc-exists-returns-true-for-existing-test
  (testing "disc-exists? returns true for existing path"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (is (true? (disc/disc-exists? path))))))

(deftest disc-exists-returns-false-for-nonexistent-test
  (testing "disc-exists? returns false for non-existent path"
    (is (false? (disc/disc-exists? "/nonexistent/path.clj")))))

;; =============================================================================
;; Bayesian Certainty Core Tests
;; =============================================================================

(deftest current-certainty-default-priors-test
  (testing "current-certainty with default priors (alpha=5, beta=2)"
    (let [disc {}]  ;; No certainty fields = uses defaults
      ;; Expected: 5 / (5 + 2) = 0.714...
      (is (< 0.71 (disc/current-certainty disc) 0.72)))))

(deftest current-certainty-custom-values-test
  (testing "current-certainty with explicit alpha/beta values"
    ;; alpha=10, beta=5 → 10/(10+5) = 0.666...
    (is (< 0.66 (disc/current-certainty {:disc/certainty-alpha 10.0
                                         :disc/certainty-beta 5.0}) 0.67))
    ;; alpha=1, beta=1 → 0.5 (maximum uncertainty)
    (is (= 0.5 (disc/current-certainty {:disc/certainty-alpha 1.0
                                        :disc/certainty-beta 1.0})))
    ;; alpha=99, beta=1 → ~0.99 (very high certainty)
    (is (< 0.98 (disc/current-certainty {:disc/certainty-alpha 99.0
                                         :disc/certainty-beta 1.0}) 1.0))))

(deftest beta-lower-bound-test
  (testing "beta-lower-bound returns conservative credible interval"
    ;; With low alpha/beta, there's high variance → lower bound is lower
    (let [low-confidence {:disc/certainty-alpha 2.0 :disc/certainty-beta 2.0}
          high-confidence {:disc/certainty-alpha 50.0 :disc/certainty-beta 10.0}]
      ;; Low confidence has wider interval (lower bound below mean)
      (is (< (disc/beta-lower-bound low-confidence) 0.5))
      ;; High confidence has tighter interval
      (is (> (disc/beta-lower-bound high-confidence) 0.7)))))

(deftest needs-read-threshold-test
  (testing "needs-read? triggers below threshold"
    (let [high-certainty {:disc/certainty-alpha 20.0 :disc/certainty-beta 2.0}
          low-certainty {:disc/certainty-alpha 2.0 :disc/certainty-beta 10.0}]
      ;; High certainty (20/22 ≈ 0.91) doesn't need read
      (is (false? (disc/needs-read? high-certainty)))
      ;; Low certainty (2/12 ≈ 0.17) needs read
      (is (true? (disc/needs-read? low-certainty))))))

(deftest needs-read-custom-threshold-test
  (testing "needs-read? respects custom threshold"
    (let [disc {:disc/certainty-alpha 8.0 :disc/certainty-beta 2.0}]
      ;; Certainty is 8/10 = 0.8
      ;; With threshold 0.7, doesn't need read
      (is (false? (disc/needs-read? disc 0.7)))
      ;; With threshold 0.9, needs read
      (is (true? (disc/needs-read? disc 0.9))))))

;; =============================================================================
;; Certainty Update Tests
;; =============================================================================

(deftest update-certainty-read-confirmed-test
  (testing "update-certainty :read-confirmed increases alpha by 3"
    (let [disc {:disc/certainty-alpha 5.0 :disc/certainty-beta 2.0}
          updated (disc/update-certainty disc :read-confirmed)]
      (is (= 8.0 (:disc/certainty-alpha updated)))
      (is (= 2.0 (:disc/certainty-beta updated))))))

(deftest update-certainty-hash-mismatch-test
  (testing "update-certainty :hash-mismatch increases beta by 5"
    (let [disc {:disc/certainty-alpha 5.0 :disc/certainty-beta 2.0}
          updated (disc/update-certainty disc :hash-mismatch)]
      (is (= 5.0 (:disc/certainty-alpha updated)))
      (is (= 7.0 (:disc/certainty-beta updated))))))

(deftest update-certainty-git-commit-touched-test
  (testing "update-certainty :git-commit-touched increases beta by 2"
    (let [disc {:disc/certainty-alpha 5.0 :disc/certainty-beta 2.0}
          updated (disc/update-certainty disc :git-commit-touched)]
      (is (= 5.0 (:disc/certainty-alpha updated)))
      (is (= 4.0 (:disc/certainty-beta updated))))))

(deftest update-certainty-time-decay-test
  (testing "update-certainty :time-decay increases beta by 0.5"
    (let [disc {:disc/certainty-alpha 5.0 :disc/certainty-beta 2.0}
          updated (disc/update-certainty disc :time-decay)]
      (is (= 5.0 (:disc/certainty-alpha updated)))
      (is (= 2.5 (:disc/certainty-beta updated))))))

(deftest update-certainty-unknown-event-test
  (testing "update-certainty with unknown event leaves values unchanged"
    (let [disc {:disc/certainty-alpha 5.0 :disc/certainty-beta 2.0}
          updated (disc/update-certainty disc :unknown-event)]
      (is (= 5.0 (:disc/certainty-alpha updated)))
      (is (= 2.0 (:disc/certainty-beta updated))))))

(deftest certainty-increases-with-confirmations-test
  (testing "Repeated read-confirmed events monotonically increase certainty"
    (let [initial {:disc/certainty-alpha 5.0 :disc/certainty-beta 2.0}
          after-1 (disc/update-certainty initial :read-confirmed)
          after-2 (disc/update-certainty after-1 :read-confirmed)
          after-3 (disc/update-certainty after-2 :read-confirmed)]
      (is (< (disc/current-certainty initial)
             (disc/current-certainty after-1)
             (disc/current-certainty after-2)
             (disc/current-certainty after-3))))))

(deftest certainty-decreases-with-refutations-test
  (testing "Hash mismatch events decrease certainty"
    (let [initial {:disc/certainty-alpha 10.0 :disc/certainty-beta 2.0}
          after-mismatch (disc/update-certainty initial :hash-mismatch)]
      (is (> (disc/current-certainty initial)
             (disc/current-certainty after-mismatch))))))

;; =============================================================================
;; Volatility Classification Tests
;; =============================================================================

(deftest classify-volatility-stable-files-test
  (testing "classify-volatility identifies stable files"
    (is (= :stable (disc/classify-volatility "deps.edn")))
    (is (= :stable (disc/classify-volatility "/foo/bar/project.clj")))
    (is (= :stable (disc/classify-volatility "config/pom.xml")))
    (is (= :stable (disc/classify-volatility ".gitignore")))))

(deftest classify-volatility-volatile-files-test
  (testing "classify-volatility identifies volatile files"
    (is (= :volatile (disc/classify-volatility "app.log")))
    (is (= :volatile (disc/classify-volatility "/tmp/foo.tmp")))
    (is (= :volatile (disc/classify-volatility "target/classes/foo.class")))
    (is (= :volatile (disc/classify-volatility ".nrepl-port")))))

(deftest classify-volatility-moderate-default-test
  (testing "classify-volatility defaults to moderate for source files"
    (is (= :moderate (disc/classify-volatility "src/core.clj")))
    (is (= :moderate (disc/classify-volatility "test/foo_test.clj")))
    (is (= :moderate (disc/classify-volatility "README.md")))
    (is (= :moderate (disc/classify-volatility "build.gradle")))))

(deftest initial-alpha-by-volatility-test
  (testing "add-disc! sets initial alpha based on volatility class"
    ;; Stable files start with higher alpha (more confident)
    (disc/add-disc! {:path "/test-volatility/test-deps.edn" :content-hash "abc123"})
    (let [stable-disc (disc/get-disc "/test-volatility/test-deps.edn")]
      (is (= 7.0 (:disc/certainty-alpha stable-disc)))
      (is (= :stable (:disc/volatility-class stable-disc))))

    ;; Volatile files start with lower alpha (less confident)
    (disc/add-disc! {:path "/test-volatility/test.log" :content-hash "def456"})
    (let [volatile-disc (disc/get-disc "/test-volatility/test.log")]
      (is (= 3.0 (:disc/certainty-alpha volatile-disc)))
      (is (= :volatile (:disc/volatility-class volatile-disc))))

    ;; Moderate files in between
    (disc/add-disc! {:path "/test-volatility/src/core.clj" :content-hash "ghi789"})
    (let [moderate-disc (disc/get-disc "/test-volatility/src/core.clj")]
      (is (= 5.0 (:disc/certainty-alpha moderate-disc)))
      (is (= :moderate (:disc/volatility-class moderate-disc))))))

;; =============================================================================
;; Time Decay Tests
;; =============================================================================

(deftest apply-time-decay-uses-volatility-rate-test
  (testing "apply-time-decay uses rate based on volatility class"
    ;; Create disc with last-observation 10 days ago
    (let [ten-days-ago (java.util.Date. (- (System/currentTimeMillis) (* 10 86400000)))
          stable-disc {:disc/certainty-alpha 7.0
                       :disc/certainty-beta 2.0
                       :disc/volatility-class :stable
                       :disc/last-observation ten-days-ago}
          volatile-disc {:disc/certainty-alpha 3.0
                         :disc/certainty-beta 2.0
                         :disc/volatility-class :volatile
                         :disc/last-observation ten-days-ago}
          decayed-stable (disc/apply-time-decay stable-disc)
          decayed-volatile (disc/apply-time-decay volatile-disc)]
      ;; Stable: 10 days * 0.01/day = 0.1 added to beta
      (is (< (Math/abs (- (:disc/certainty-beta decayed-stable) 2.1)) 0.05))
      ;; Volatile: 10 days * 0.15/day = 1.5 added to beta
      (is (< (Math/abs (- (:disc/certainty-beta decayed-volatile) 3.5)) 0.05)))))

(deftest apply-time-decay-updates-last-observation-test
  (testing "apply-time-decay updates last-observation timestamp"
    (let [old-date (java.util.Date. (- (System/currentTimeMillis) 86400000))
          disc {:disc/certainty-beta 2.0
                :disc/volatility-class :moderate
                :disc/last-observation old-date}
          decayed (disc/apply-time-decay disc)]
      ;; New last-observation should be close to now
      (is (< (- (System/currentTimeMillis)
                (.getTime ^java.util.Date (:disc/last-observation decayed)))
             1000)))))

(deftest apply-time-decay-no-decay-when-recent-test
  (testing "apply-time-decay adds minimal decay when last observation is recent"
    (let [now (java.util.Date.)
          disc {:disc/certainty-alpha 5.0
                :disc/certainty-beta 2.0
                :disc/volatility-class :moderate
                :disc/last-observation now}
          decayed (disc/apply-time-decay disc)]
      ;; Beta should barely change (nearly 0 days elapsed)
      (is (< (Math/abs (- (:disc/certainty-beta decayed) 2.0)) 0.01)))))

;; =============================================================================
;; touch-disc! Tests
;; =============================================================================

(deftest touch-disc-creates-if-not-exists-test
  (testing "touch-disc! creates disc entity if it doesn't exist"
    (let [path (str "/touch-test/" (subs (str (java.util.UUID/randomUUID)) 0 8) ".clj")]
      (is (nil? (disc/get-disc path)))
      (disc/touch-disc! path)
      (let [d (disc/get-disc path)]
        (is (some? d))
        (is (= 1 (:disc/read-count d)))
        (is (some? (:disc/last-read-at d)))))))

(deftest touch-disc-increments-read-count-test
  (testing "touch-disc! increments read-count on existing disc"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "xyz"})
      (disc/touch-disc! path)
      (is (= 1 (:disc/read-count (disc/get-disc path))))
      (disc/touch-disc! path)
      (is (= 2 (:disc/read-count (disc/get-disc path))))
      (disc/touch-disc! path)
      (is (= 3 (:disc/read-count (disc/get-disc path)))))))

(deftest touch-disc-updates-last-read-at-test
  (testing "touch-disc! updates last-read-at timestamp"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (let [before-touch (disc/get-disc path)
            _ (Thread/sleep 10)  ;; Small delay to ensure timestamp differs
            _ (disc/touch-disc! path)
            after-touch (disc/get-disc path)]
        (is (some? (:disc/last-read-at after-touch)))
        ;; If before had last-read-at, after should be later
        (when (:disc/last-read-at before-touch)
          (is (> (.getTime ^java.util.Date (:disc/last-read-at after-touch))
                 (.getTime ^java.util.Date (:disc/last-read-at before-touch)))))))))

;; =============================================================================
;; kg-first-context Classification Tests
;; =============================================================================

(deftest kg-first-context-unknown-file-needs-read-test
  (testing "kg-first-context classifies unknown files as :needs-read"
    (let [result (disc/kg-first-context ["/unknown/file1.clj"
                                         "/unknown/file2.clj"])]
      (is (= 2 (count (:needs-read result))))
      (is (contains? (set (:needs-read result)) "/unknown/file1.clj"))
      (is (contains? (set (:needs-read result)) "/unknown/file2.clj"))
      (is (empty? (:kg-known result)))
      (is (empty? (:stale result))))))

(deftest kg-first-context-fresh-file-kg-known-test
  (testing "kg-first-context classifies fresh tracked files as :kg-known"
    (let [path (gen-path)]
      ;; Add a disc with high certainty (recent, no hash mismatch)
      (disc/add-disc! {:path path :content-hash "abc123"})
      ;; Touch it to set last-read-at to now
      (disc/touch-disc! path)

      (let [result (disc/kg-first-context [path])]
        ;; File should be in kg-known with metadata
        (is (= 1 (count (:kg-known result))))
        (is (contains? (:kg-known result) path))
        (is (empty? (:needs-read result)))
        (is (empty? (:stale result)))
        ;; Summary should reflect counts
        (is (= 1 (:known (:summary result))))))))

(deftest kg-first-context-stale-file-test
  (testing "kg-first-context classifies stale files correctly"
    (let [path (gen-path)]
      ;; Add a disc and simulate staleness by setting old last-read-at
      (disc/add-disc! {:path path :content-hash "old-hash"})
      ;; Manually update to make it stale (old last-read-at)
      (let [old-date (java.util.Date. (- (System/currentTimeMillis)
                                         (* 45 24 60 60 1000)))] ;; 45 days ago
        (disc/update-disc! path
                           {:disc/last-read-at old-date
                            :disc/analyzed-at old-date}))

      (let [result (disc/kg-first-context [path])]
        ;; File should be in stale list
        (is (contains? (set (:stale result)) path))
        (is (= 1 (:stale (:summary result))))))))

(deftest kg-first-context-mixed-files-test
  (testing "kg-first-context handles mixed fresh/stale/unknown files"
    (let [fresh-path (gen-path)
          stale-path (gen-path)
          unknown-path (gen-path)]
      ;; Setup fresh file
      (disc/add-disc! {:path fresh-path :content-hash "fresh"})
      (disc/touch-disc! fresh-path)

      ;; Setup stale file
      (disc/add-disc! {:path stale-path :content-hash "stale"})
      (let [old-date (java.util.Date. (- (System/currentTimeMillis)
                                         (* 45 24 60 60 1000)))]
        (disc/update-disc! stale-path
                           {:disc/last-read-at old-date
                            :disc/analyzed-at old-date}))

      (let [result (disc/kg-first-context [fresh-path stale-path unknown-path])]
        (is (= 1 (:known (:summary result))))
        (is (= 1 (:stale (:summary result))))
        (is (= 1 (:needs-read (:summary result))))
        (is (= 3 (:total (:summary result))))))))

(deftest kg-first-context-staleness-threshold-test
  (testing "kg-first-context respects custom staleness-threshold"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "x"})
      (disc/touch-disc! path)

      ;; With very loose threshold (1.0), everything is considered fresh
      (let [loose-result (disc/kg-first-context [path]
                                                {:staleness-threshold 1.0})]
        (is (= 1 (:known (:summary loose-result))))))))

(deftest kg-first-context-filters-invalid-paths-test
  (testing "kg-first-context filters out nil and empty paths"
    (let [valid-path (gen-path)]
      (disc/add-disc! {:path valid-path :content-hash "test"})
      (disc/touch-disc! valid-path)

      (let [result (disc/kg-first-context [valid-path nil "" valid-path])]
        ;; Should deduplicate and filter invalid
        (is (= 1 (:total (:summary result))))))))

;; =============================================================================
;; Staleness Warnings Tests
;; =============================================================================

(deftest staleness-warnings-returns-empty-for-fresh-files-test
  (testing "staleness-warnings returns empty for fresh files"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "x"})
      (disc/touch-disc! path)
      (let [warnings (disc/staleness-warnings [path])]
        (is (empty? warnings))))))

(deftest staleness-warnings-returns-empty-for-unknown-files-test
  (testing "staleness-warnings returns empty for unknown files (zero noise)"
    (let [warnings (disc/staleness-warnings ["/completely/unknown.clj"])]
      (is (empty? warnings)))))

(deftest staleness-warnings-returns-warnings-for-stale-files-test
  (testing "staleness-warnings returns warnings for stale tracked files"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "old"})
      (let [old-date (java.util.Date. (- (System/currentTimeMillis)
                                         (* 45 24 60 60 1000)))]
        (disc/update-disc! path
                           {:disc/last-read-at old-date
                            :disc/analyzed-at old-date}))

      (let [warnings (disc/staleness-warnings [path])]
        (is (= 1 (count warnings)))
        (is (= path (:path (first warnings))))
        (is (> (:staleness (first warnings)) 0.3))
        (is (string? (:message (first warnings))))))))

(deftest format-staleness-warnings-test
  (testing "format-staleness-warnings produces markdown output"
    (let [warnings [{:path "/a.clj" :staleness 0.5 :message "File /a.clj is stale"}
                    {:path "/b.clj" :staleness 0.7 :message "File /b.clj is stale"}]
          formatted (disc/format-staleness-warnings warnings)]
      (is (string? formatted))
      (is (.contains formatted "L1 Disc Staleness Warnings"))
      (is (.contains formatted "/a.clj"))
      (is (.contains formatted "/b.clj")))))

(deftest format-staleness-warnings-nil-for-empty-test
  (testing "format-staleness-warnings returns nil for empty"
    (is (nil? (disc/format-staleness-warnings [])))))

;; =============================================================================
;; Certainty Event Wiring Tests
;; =============================================================================

(deftest update-disc-certainty-persists-test
  (testing "update-disc-certainty! persists changes to database"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "abc"})
      (let [before (disc/get-disc path)
            _ (disc/update-disc-certainty! path :read-confirmed)
            after (disc/get-disc path)]
        (is (< (:disc/certainty-alpha before) (:disc/certainty-alpha after)))
        (is (> (disc/current-certainty after) (disc/current-certainty before)))))))

(deftest update-disc-certainty-returns-nil-for-missing-test
  (testing "update-disc-certainty! returns nil for non-existent disc"
    (is (nil? (disc/update-disc-certainty! "/nonexistent/file.clj" :read-confirmed)))))

(deftest update-disc-certainty-updates-last-observation-test
  (testing "update-disc-certainty! updates last-observation timestamp"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (let [before (:disc/last-observation (disc/get-disc path))
            _ (Thread/sleep 10)
            _ (disc/update-disc-certainty! path :read-confirmed)
            after (:disc/last-observation (disc/get-disc path))]
        (when (and before after)
          (is (>= (.getTime ^java.util.Date after)
                  (.getTime ^java.util.Date before))))))))

;; =============================================================================
;; Staleness Score Tests
;; =============================================================================

(deftest staleness-score-fresh-file-test
  (testing "staleness-score returns low score for fresh files"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (disc/touch-disc! path)
      (let [d (disc/get-disc path)
            score (disc/staleness-score d)]
        ;; Fresh file should have low staleness
        (is (< score 0.3))))))

(deftest staleness-score-old-file-test
  (testing "staleness-score returns higher score for old files"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (let [old-date (java.util.Date. (- (System/currentTimeMillis)
                                         (* 45 24 60 60 1000)))]
        (disc/update-disc! path {:disc/last-read-at old-date
                                 :disc/analyzed-at old-date}))
      (let [d (disc/get-disc path)
            score (disc/staleness-score d)]
        ;; Old file should have higher staleness
        (is (> score 0.3))))))

;; =============================================================================
;; Top Stale Files Tests
;; =============================================================================

(deftest top-stale-files-returns-sorted-list-test
  (testing "top-stale-files returns files sorted by staleness"
    ;; Create files with varying staleness
    (let [path1 (gen-path)
          path2 (gen-path)]
      (disc/add-disc! {:path path1 :content-hash "h1"})
      (disc/add-disc! {:path path2 :content-hash "h2"})
      ;; Make path1 very old (more stale)
      (let [very-old (java.util.Date. (- (System/currentTimeMillis)
                                         (* 60 24 60 60 1000)))] ;; 60 days
        (disc/update-disc! path1 {:disc/last-read-at very-old
                                  :disc/analyzed-at very-old}))
      ;; Make path2 moderately old
      (let [moderately-old (java.util.Date. (- (System/currentTimeMillis)
                                               (* 15 24 60 60 1000)))] ;; 15 days
        (disc/update-disc! path2 {:disc/last-read-at moderately-old
                                  :disc/analyzed-at moderately-old}))

      (let [top-stale (disc/top-stale-files :n 10 :threshold 0.3)]
        ;; Should have at least one stale file
        (when (seq top-stale)
          ;; Results should be sorted by score descending
          (is (apply >= (map :score top-stale))))))))

;; =============================================================================
;; L1-P2 Transitive Staleness Tests
;; =============================================================================

(deftest entry-staleness-score-default-test
  (testing "entry-staleness-score with default alpha=1, beta=1 returns 0.5"
    (let [entry {}]
      ;; 1 - (1 / (1 + 1)) = 0.5
      (is (= 0.5 (disc/entry-staleness-score entry))))))

(deftest entry-staleness-score-high-alpha-test
  (testing "entry-staleness-score with high alpha is low (fresh)"
    ;; alpha=9, beta=1 → 1 - (9/10) = 0.1
    (let [entry {:staleness-alpha 9.0 :staleness-beta 1.0}]
      (is (< (disc/entry-staleness-score entry) 0.15)))))

(deftest entry-staleness-score-high-beta-test
  (testing "entry-staleness-score with high beta is high (stale)"
    ;; alpha=1, beta=9 → 1 - (1/10) = 0.9
    (let [entry {:staleness-alpha 1.0 :staleness-beta 9.0}]
      (is (> (disc/entry-staleness-score entry) 0.85)))))

(deftest entry-staleness-report-structure-test
  (testing "entry-staleness-report returns correct structure"
    (let [entry {:id "test-123"
                 :staleness-alpha 5.0
                 :staleness-beta 2.0
                 :staleness-source :hash-mismatch
                 :staleness-depth 1
                 :grounded-from "/path/to/file.clj"}
          report (disc/entry-staleness-report entry)]
      (is (= "test-123" (:id report)))
      (is (number? (:score report)))
      (is (= 5.0 (:alpha report)))
      (is (= 2.0 (:beta report)))
      (is (= :hash-mismatch (:source report)))
      (is (= 1 (:depth report)))
      (is (= "/path/to/file.clj" (:grounded-from report))))))

(deftest entry-staleness-report-computes-score-test
  (testing "entry-staleness-report score matches entry-staleness-score"
    (let [entry {:staleness-alpha 3.0 :staleness-beta 7.0}
          report (disc/entry-staleness-report entry)]
      ;; 1 - (3/10) = 0.7
      (is (< (Math/abs (- (:score report) 0.7)) 0.01)))))

(deftest propagation-relations-contains-expected-test
  (testing "propagation-relations contains dependency edge types"
    (is (contains? disc/propagation-relations :depends-on))
    (is (contains? disc/propagation-relations :implements))
    (is (contains? disc/propagation-relations :derived-from))
    (is (contains? disc/propagation-relations :refines))))

(deftest staleness-decay-factor-test
  (testing "staleness-decay-factor is 0.5 (halves per hop)"
    (is (= 0.5 disc/staleness-decay-factor))))

(deftest staleness-min-threshold-test
  (testing "staleness-min-threshold stops propagation at 0.3"
    (is (= 0.3 disc/staleness-min-threshold))))

(deftest staleness-max-depth-test
  (testing "staleness-max-depth limits propagation to 5 hops"
    (is (= 5 disc/staleness-max-depth))))

(deftest base-staleness-values-test
  (testing "base-staleness-values has correct severity ordering"
    (is (> (:hash-mismatch disc/base-staleness-values)
           (:git-commit disc/base-staleness-values)))
    (is (> (:git-commit disc/base-staleness-values)
           (:time-decay disc/base-staleness-values)))))

(deftest staleness-decay-calculation-test
  (testing "Staleness decays correctly with depth"
    (let [base 5.0
          depth-0 (* base (Math/pow disc/staleness-decay-factor 0))  ;; 5.0
          depth-1 (* base (Math/pow disc/staleness-decay-factor 1))  ;; 2.5
          depth-2 (* base (Math/pow disc/staleness-decay-factor 2))  ;; 1.25
          depth-3 (* base (Math/pow disc/staleness-decay-factor 3))] ;; 0.625
      (is (= 5.0 depth-0))
      (is (= 2.5 depth-1))
      (is (= 1.25 depth-2))
      (is (< (Math/abs (- depth-3 0.625)) 0.001))
      ;; depth-3 is above threshold (0.3), depth-4 would be 0.3125 (borderline)
      (is (>= depth-3 disc/staleness-min-threshold)))))
