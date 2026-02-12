(ns hive-mcp.tools.swarm.jvm.orphan-test
  "Tests for JVM orphan process detection.

   Tests cover:
   - Parent info enrichment
   - Orphan detection predicates
   - Higher-order orphan detector composition
   - Full orphan identification with reasons

   CLARITY: Pure function testing - no I/O dependencies."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.jvm.orphan :as orphan]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def sample-proc
  "Sample process for testing - minimal required keys."
  {:pid "1234"
   :ppid "100"
   :type :nrepl
   :etime "30:00"
   :cmd "java -cp ... clojure.main"})

(def sample-parents
  "Sample parent process map."
  {"100" {:ppid "50" :comm "claude"}
   "50"  {:ppid "1" :comm "bash"}
   "1"   {:ppid "0" :comm "init"}})

;; =============================================================================
;; enrich-with-parent-info Tests
;; =============================================================================

(deftest test-enrich-with-parent-info
  (testing "Process with living Claude parent"
    (let [result (orphan/enrich-with-parent-info sample-proc sample-parents)]
      (is (true? (:parent-alive result)))
      (is (= "claude" (:parent-comm result)))
      (is (true? (:parent-is-claude result)))
      (is (false? (:truly-orphaned result)))))

  (testing "Process with non-Claude parent"
    (let [proc (assoc sample-proc :ppid "50")
          result (orphan/enrich-with-parent-info proc sample-parents)]
      (is (true? (:parent-alive result)))
      (is (= "bash" (:parent-comm result)))
      (is (false? (:parent-is-claude result)))
      (is (false? (:truly-orphaned result)))))

  (testing "Process with dead parent (not in map)"
    (let [proc (assoc sample-proc :ppid "9999")
          result (orphan/enrich-with-parent-info proc sample-parents)]
      (is (false? (:parent-alive result)))
      (is (nil? (:parent-comm result)))
      (is (false? (:parent-is-claude result)))
      (is (true? (:truly-orphaned result)))))

  (testing "Process reparented to init (PID 1)"
    (let [proc (assoc sample-proc :ppid "1")
          result (orphan/enrich-with-parent-info proc sample-parents)]
      ;; Even though init is in the map, ppid=1 means truly orphaned
      (is (true? (:parent-alive result)))
      (is (= "init" (:parent-comm result)))
      (is (true? (:truly-orphaned result)))))

  (testing "Preserves original process keys"
    (let [result (orphan/enrich-with-parent-info sample-proc sample-parents)]
      (is (= "1234" (:pid result)))
      (is (= "100" (:ppid result)))
      (is (= :nrepl (:type result))))))

;; =============================================================================
;; protected-type? Tests
;; =============================================================================

(deftest test-protected-type?
  (testing "Type in protected set"
    (is (true? (orphan/protected-type? {:type :shadow-cljs} #{"shadow-cljs"})))
    (is (true? (orphan/protected-type? {:type :leiningen} #{"shadow-cljs" "leiningen"}))))

  (testing "Type not in protected set"
    (is (false? (orphan/protected-type? {:type :nrepl} #{"shadow-cljs" "leiningen"})))
    (is (false? (orphan/protected-type? {:type :unknown} #{}))))

  (testing "Empty protected set"
    (is (false? (orphan/protected-type? {:type :anything} #{}))))

  (testing "Keyword type converted to string for comparison"
    ;; Note: type is a keyword, protected-types contains strings
    (is (true? (orphan/protected-type? {:type :shadow-cljs} #{"shadow-cljs"})))))

;; =============================================================================
;; age-orphan? Tests
;; =============================================================================

(deftest test-age-orphan?
  (testing "Process older than threshold"
    (is (true? (orphan/age-orphan? {:etime "45:00"} 30)))  ; 45 min >= 30
    (is (true? (orphan/age-orphan? {:etime "01:30:00"} 30)))) ; 90 min >= 30

  (testing "Process younger than threshold"
    (is (false? (orphan/age-orphan? {:etime "15:00"} 30)))  ; 15 min < 30
    (is (false? (orphan/age-orphan? {:etime "05:30"} 30)))) ; 5 min < 30

  (testing "Process exactly at threshold"
    (is (true? (orphan/age-orphan? {:etime "30:00"} 30))))  ; 30 min >= 30

  (testing "Various etime formats"
    ;; MM:SS format
    (is (true? (orphan/age-orphan? {:etime "60:00"} 60)))
    ;; HH:MM:SS format
    (is (true? (orphan/age-orphan? {:etime "02:00:00"} 60)))  ; 120 min >= 60
    ;; DD-HH:MM:SS format
    (is (true? (orphan/age-orphan? {:etime "1-00:00:00"} 1440)))))  ; 1 day = 1440 min

;; =============================================================================
;; truly-orphaned? Tests
;; =============================================================================

(deftest test-truly-orphaned?
  (testing "Process marked as truly orphaned"
    (is (true? (orphan/truly-orphaned? {:truly-orphaned true}))))

  (testing "Process not marked as truly orphaned"
    (is (false? (orphan/truly-orphaned? {:truly-orphaned false}))))

  (testing "Process without the key"
    (is (nil? (orphan/truly-orphaned? {})))))

;; =============================================================================
;; orphan-detector Tests (Higher-Order Function)
;; =============================================================================

(deftest test-orphan-detector-defaults
  (testing "Default detector with truly orphaned process"
    (let [detector (orphan/orphan-detector)
          proc {:truly-orphaned true :type :nrepl :etime "45:00"}]
      (is (true? (detector proc)))))

  (testing "Default detector with non-orphaned process"
    (let [detector (orphan/orphan-detector)
          proc {:truly-orphaned false :type :nrepl :etime "45:00"}]
      (is (false? (detector proc))))))

(deftest test-orphan-detector-protected-types
  (testing "Protected type is never orphan"
    (let [detector (orphan/orphan-detector :protected-types #{"shadow-cljs"})
          proc {:truly-orphaned true :type :shadow-cljs :etime "999:00"}]
      (is (false? (detector proc)))))

  (testing "Non-protected type can be orphan"
    (let [detector (orphan/orphan-detector :protected-types #{"shadow-cljs"})
          proc {:truly-orphaned true :type :nrepl :etime "999:00"}]
      (is (true? (detector proc))))))

(deftest test-orphan-detector-true-orphans-only
  (testing "true-orphans-only mode - only detects truly orphaned"
    (let [detector (orphan/orphan-detector :true-orphans-only true)
          orphan {:truly-orphaned true :type :nrepl :etime "05:00"}
          non-orphan {:truly-orphaned false :type :nrepl :etime "999:00"}]
      (is (true? (detector orphan)))
      (is (false? (detector non-orphan)))))

  (testing "Non true-orphans-only mode - requires both orphan AND age"
    (let [detector (orphan/orphan-detector :true-orphans-only false :min-age-minutes 30)
          young-orphan {:truly-orphaned true :type :nrepl :etime "05:00"}
          old-orphan {:truly-orphaned true :type :nrepl :etime "45:00"}
          old-non-orphan {:truly-orphaned false :type :nrepl :etime "45:00"}]
      ;; Young orphan - not detected (age too low)
      (is (false? (detector young-orphan)))
      ;; Old orphan - detected
      (is (true? (detector old-orphan)))
      ;; Old non-orphan - not detected (not truly orphaned)
      (is (false? (detector old-non-orphan))))))

(deftest test-orphan-detector-min-age-minutes
  (testing "Custom min-age-minutes in non-true-orphans mode"
    (let [detector (orphan/orphan-detector :true-orphans-only false :min-age-minutes 60)
          proc-59min {:truly-orphaned true :type :nrepl :etime "59:00"}
          proc-60min {:truly-orphaned true :type :nrepl :etime "60:00"}]
      (is (false? (detector proc-59min)))
      (is (true? (detector proc-60min))))))

(deftest test-orphan-detector-combined-options
  (testing "All options combined"
    (let [detector (orphan/orphan-detector
                    :protected-types #{"shadow-cljs" "leiningen"}
                    :true-orphans-only false
                    :min-age-minutes 45)]
      ;; Protected - never orphan
      (is (false? (detector {:truly-orphaned true :type :shadow-cljs :etime "999:00"})))
      ;; Non-protected, truly orphaned, old enough - orphan
      (is (true? (detector {:truly-orphaned true :type :nrepl :etime "60:00"})))
      ;; Non-protected, truly orphaned, too young - not orphan
      (is (false? (detector {:truly-orphaned true :type :nrepl :etime "30:00"})))
      ;; Non-protected, not orphaned - not orphan
      (is (false? (detector {:truly-orphaned false :type :nrepl :etime "60:00"}))))))

;; =============================================================================
;; identify-orphan Tests
;; =============================================================================

(deftest test-identify-orphan-basic
  (testing "Truly orphaned process"
    (let [proc {:truly-orphaned true :parent-comm "dead" :type :nrepl :etime "45:00"}
          result (orphan/identify-orphan proc)]
      (is (true? (:orphan result)))
      (is (= 45 (:age-minutes result)))
      (is (string? (:reason result)))
      (is (.contains (:reason result) "truly-orphaned"))))

  (testing "Non-orphaned process with Claude parent"
    (let [proc {:truly-orphaned false :parent-is-claude true :ppid "100" :type :nrepl :etime "30:00"}
          result (orphan/identify-orphan proc)]
      (is (false? (:orphan result)))
      (is (= 30 (:age-minutes result)))
      (is (.contains (:reason result) "managed-by-claude"))))

  (testing "Non-orphaned process with other parent"
    (let [proc {:truly-orphaned false :parent-is-claude false :parent-comm "bash" :type :nrepl :etime "15:00"}
          result (orphan/identify-orphan proc)]
      (is (false? (:orphan result)))
      (is (.contains (:reason result) "has-parent")))))

(deftest test-identify-orphan-protected
  (testing "Protected type shows protected reason"
    (let [proc {:truly-orphaned true :type :shadow-cljs :etime "999:00"}
          result (orphan/identify-orphan proc :protected-types #{"shadow-cljs"})]
      (is (false? (:orphan result)))
      (is (= "protected-type" (:reason result))))))

(deftest test-identify-orphan-options
  (testing "Options passed through to detector"
    (let [proc {:truly-orphaned true :type :nrepl :etime "15:00"}
          ;; With true-orphans-only=true (default), should be orphan
          result-default (orphan/identify-orphan proc)
          ;; With true-orphans-only=false and min-age=30, should NOT be orphan
          result-age (orphan/identify-orphan proc :true-orphans-only false :min-age-minutes 30)]
      (is (true? (:orphan result-default)))
      (is (false? (:orphan result-age))))))

(deftest test-identify-orphan-preserves-keys
  (testing "Original process keys preserved"
    (let [proc {:pid "1234" :ppid "100" :truly-orphaned true :type :nrepl :etime "30:00" :cmd "java"}
          result (orphan/identify-orphan proc)]
      (is (= "1234" (:pid result)))
      (is (= "100" (:ppid result)))
      (is (= :nrepl (:type result)))
      (is (= "java" (:cmd result))))))

;; =============================================================================
;; identify-orphans Tests (Batch)
;; =============================================================================

(deftest test-identify-orphans
  (testing "Batch identification"
    (let [procs [{:truly-orphaned true :type :nrepl :etime "45:00"}
                 {:truly-orphaned false :type :nrepl :etime "45:00"}
                 {:truly-orphaned true :type :shadow-cljs :etime "45:00"}]
          results (orphan/identify-orphans procs :protected-types #{"shadow-cljs"})]
      (is (= 3 (count results)))
      ;; First: truly orphaned, not protected - orphan
      (is (true? (:orphan (first results))))
      ;; Second: not truly orphaned - not orphan
      (is (false? (:orphan (second results))))
      ;; Third: protected type - not orphan
      (is (false? (:orphan (nth results 2))))))

  (testing "Empty collection"
    (is (empty? (orphan/identify-orphans []))))

  (testing "Options applied to all processes"
    (let [procs [{:truly-orphaned true :type :nrepl :etime "15:00"}
                 {:truly-orphaned true :type :nrepl :etime "45:00"}]
          results (orphan/identify-orphans procs :true-orphans-only false :min-age-minutes 30)]
      ;; First: too young
      (is (false? (:orphan (first results))))
      ;; Second: old enough
      (is (true? (:orphan (second results)))))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest test-full-workflow
  (testing "End-to-end: enrich then identify"
    (let [proc {:pid "1234" :ppid "9999" :type :nrepl :etime "60:00"}
          parents {"100" {:ppid "1" :comm "bash"}}  ; ppid 9999 not in map
          enriched (orphan/enrich-with-parent-info proc parents)
          identified (orphan/identify-orphan enriched)]
      ;; Should be truly orphaned (parent not in map)
      (is (true? (:truly-orphaned enriched)))
      ;; Should be identified as orphan
      (is (true? (:orphan identified)))
      (is (= 60 (:age-minutes identified))))))

(deftest test-claude-managed-workflow
  (testing "Claude-managed process is never orphan"
    (let [proc {:pid "1234" :ppid "100" :type :nrepl :etime "999:00"}
          parents {"100" {:ppid "1" :comm "claude"}}
          enriched (orphan/enrich-with-parent-info proc parents)
          identified (orphan/identify-orphan enriched)]
      ;; Claude parent alive
      (is (true? (:parent-is-claude enriched)))
      (is (false? (:truly-orphaned enriched)))
      ;; Not identified as orphan
      (is (false? (:orphan identified)))
      (is (.contains (:reason identified) "managed-by-claude")))))
