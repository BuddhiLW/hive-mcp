(ns hive-mcp.channel.memory-piggyback-test
  "Unit tests for the memory piggyback buffer module."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.memory-piggyback :as mp]))

;; Reset buffer state between tests
(use-fixtures :each
  (fn [f]
    (mp/reset-all!)
    (f)
    (mp/reset-all!)))

;; =============================================================================
;; Enqueue + Drain Basic Flow
;; =============================================================================

(deftest enqueue-and-drain-basic-test
  (testing "enqueue entries then drain returns them"
    (let [entries [{:id "ax-1" :type "axiom" :content "Rule one" :severity "INVIOLABLE" :tags ["axiom"]}
                   {:id "p-1" :type "convention" :content "Convention one" :tags ["catchup-priority"]}]]
      (mp/enqueue! "agent-1" "proj-1" entries)
      (is (mp/has-pending? "agent-1" "proj-1"))

      (let [result (mp/drain! "agent-1" "proj-1")]
        (is (= 2 (count (:batch result))))
        (is (= 2 (:total result)))
        (is (= 2 (:delivered result)))
        (is (= 0 (:remaining result)))
        (is (true? (:done result)))
        (is (= 1 (:seq result)))

        ;; Verify compact format
        (let [first-entry (first (:batch result))]
          (is (= "ax-1" (:id first-entry)))
          (is (= "axiom" (:T first-entry)))
          (is (= "INVIOLABLE" (:S first-entry)))
          (is (= "Rule one" (:C first-entry)))
          (is (= ["axiom"] (:tags first-entry)))))))

  (testing "drain returns nil when no entries pending"
    (is (nil? (mp/drain! "agent-1" "proj-1"))))

  (testing "has-pending? returns false after full drain"
    (mp/enqueue! "agent-2" "proj-2"
                 [{:id "x" :type "note" :content "test"}])
    (mp/drain! "agent-2" "proj-2")
    (is (not (mp/has-pending? "agent-2" "proj-2")))))

;; =============================================================================
;; 32K Char Budget Batching
;; =============================================================================

(deftest char-budget-batching-test
  (testing "large entries are split across batches"
    ;; Create entries that exceed 32K when combined
    (let [big-content (apply str (repeat 10000 "x"))
          entries (mapv (fn [i]
                          {:id (str "big-" i)
                           :type "axiom"
                           :content big-content
                           :tags []})
                        (range 5))]
      (mp/enqueue! "agent-b" "proj-b" entries)

      ;; First drain should get some entries (within budget)
      (let [r1 (mp/drain! "agent-b" "proj-b")]
        (is (pos? (count (:batch r1))))
        (is (< (count (:batch r1)) 5))
        (is (pos? (:remaining r1)))
        (is (not (:done r1)))

        ;; Second drain should get more
        (let [r2 (mp/drain! "agent-b" "proj-b")]
          (is (pos? (count (:batch r2))))
          (is (= 2 (:seq r2)))

          ;; Eventually should be done
          (loop [remaining (:remaining r2)]
            (when (pos? remaining)
              (let [r (mp/drain! "agent-b" "proj-b")]
                (recur (:remaining r))))))))))

(deftest at-least-one-entry-per-drain-test
  (testing "drain always returns at least one entry even if over budget"
    (let [huge-content (apply str (repeat 50000 "y"))
          entries [{:id "huge-1" :type "axiom" :content huge-content :tags []}]]
      (mp/enqueue! "agent-h" "proj-h" entries)
      (let [result (mp/drain! "agent-h" "proj-h")]
        (is (= 1 (count (:batch result))))
        (is (true? (:done result)))))))

;; =============================================================================
;; Cursor Tracking
;; =============================================================================

(deftest cursor-tracking-test
  (testing "sequential drains advance cursor correctly"
    (let [entries (mapv (fn [i]
                          {:id (str "e-" i) :type "note"
                           :content (apply str (repeat 12000 "a"))
                           :tags []})
                        (range 4))]
      (mp/enqueue! "agent-c" "proj-c" entries)

      ;; First drain
      (let [r1 (mp/drain! "agent-c" "proj-c")]
        (is (= 1 (:seq r1)))
        (is (pos? (:delivered r1)))
        (let [first-batch-count (count (:batch r1))]

          ;; Second drain picks up where first left off
          (let [r2 (mp/drain! "agent-c" "proj-c")]
            (is (= 2 (:seq r2)))
            (is (= (+ first-batch-count (count (:batch r2)))
                   (:delivered r2)))))))))

;; =============================================================================
;; Idempotent Enqueue
;; =============================================================================

(deftest idempotent-enqueue-test
  (testing "second enqueue for same agent+project is no-op"
    (let [entries-1 [{:id "a1" :type "axiom" :content "First" :tags []}]
          entries-2 [{:id "a2" :type "axiom" :content "Second" :tags []}
                     {:id "a3" :type "axiom" :content "Third" :tags []}]]
      (mp/enqueue! "agent-i" "proj-i" entries-1)
      ;; Second enqueue should be ignored
      (mp/enqueue! "agent-i" "proj-i" entries-2)

      (let [result (mp/drain! "agent-i" "proj-i")]
        ;; Should only have entries from first enqueue
        (is (= 1 (count (:batch result))))
        (is (= "a1" (:id (first (:batch result)))))))))

;; =============================================================================
;; Agent+Project Isolation
;; =============================================================================

(deftest agent-project-isolation-test
  (testing "different agent+project combos have independent buffers"
    (mp/enqueue! "agent-1" "proj-A"
                 [{:id "x1" :type "note" :content "for agent-1/proj-A" :tags []}])
    (mp/enqueue! "agent-2" "proj-A"
                 [{:id "x2" :type "note" :content "for agent-2/proj-A" :tags []}])
    (mp/enqueue! "agent-1" "proj-B"
                 [{:id "x3" :type "note" :content "for agent-1/proj-B" :tags []}])

    (let [r1 (mp/drain! "agent-1" "proj-A")
          r2 (mp/drain! "agent-2" "proj-A")
          r3 (mp/drain! "agent-1" "proj-B")]
      (is (= "x1" (:id (first (:batch r1)))))
      (is (= "x2" (:id (first (:batch r2)))))
      (is (= "x3" (:id (first (:batch r3))))))))

;; =============================================================================
;; Reset
;; =============================================================================

(deftest reset-test
  (testing "reset! clears specific buffer"
    (mp/enqueue! "agent-r" "proj-r"
                 [{:id "r1" :type "note" :content "test" :tags []}])
    (is (mp/has-pending? "agent-r" "proj-r"))
    (mp/clear-buffer! "agent-r" "proj-r")
    (is (not (mp/has-pending? "agent-r" "proj-r")))
    (is (nil? (mp/drain! "agent-r" "proj-r"))))

  (testing "reset-all! clears all buffers"
    (mp/enqueue! "a1" "p1" [{:id "1" :type "note" :content "t" :tags []}])
    (mp/enqueue! "a2" "p2" [{:id "2" :type "note" :content "t" :tags []}])
    (mp/reset-all!)
    (is (not (mp/has-pending? "a1" "p1")))
    (is (not (mp/has-pending? "a2" "p2")))))

;; =============================================================================
;; Nil/Default Handling
;; =============================================================================

(deftest nil-defaults-test
  (testing "nil agent-id defaults to 'coordinator'"
    (mp/enqueue! nil "proj-n"
                 [{:id "n1" :type "note" :content "test" :tags []}])
    (is (mp/has-pending? nil "proj-n"))
    (let [result (mp/drain! nil "proj-n")]
      (is (= 1 (count (:batch result))))))

  (testing "nil project-id defaults to 'global'"
    (mp/enqueue! "agent-n" nil
                 [{:id "n2" :type "note" :content "test" :tags []}])
    (is (mp/has-pending? "agent-n" nil))
    (let [result (mp/drain! "agent-n" nil)]
      (is (= 1 (count (:batch result)))))))

;; =============================================================================
;; Entry Format
;; =============================================================================

(deftest entry-format-test
  (testing "entries without severity omit :S key"
    (mp/enqueue! "agent-f" "proj-f"
                 [{:id "f1" :type "convention" :content "Do X" :tags ["priority"]}])
    (let [result (mp/drain! "agent-f" "proj-f")
          entry (first (:batch result))]
      (is (= "convention" (:T entry)))
      (is (= "Do X" (:C entry)))
      (is (= ["priority"] (:tags entry)))
      (is (not (contains? entry :S)))))

  (testing "entries without tags omit :tags key"
    (mp/enqueue! "agent-g" "proj-g"
                 [{:id "g1" :type "note" :content "plain" :tags []}])
    (let [result (mp/drain! "agent-g" "proj-g")
          entry (first (:batch result))]
      (is (not (contains? entry :tags))))))
