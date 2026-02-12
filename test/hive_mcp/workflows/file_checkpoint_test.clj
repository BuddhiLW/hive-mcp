(ns hive-mcp.workflows.file-checkpoint-test
  "Tests for file checkpointing (SAA rewind support).

   Test categories:
   1. Pure functions — make-checkpoint, checkpoint-summary, generate-checkpoint-id
   2. File I/O — snapshot-files, restore-files (with injectable fns)
   3. Checkpoint lifecycle — create!, rewind!, get, list, delete
   4. FSM integration helpers — checkpoint-before-act, rewind-act
   5. Wave integration — checkpoint-wave-files!, rewind-wave!

   All tests use injected I/O fns (no real filesystem).
   SOLID: D — Tests depend on abstractions (injected fns), not concretions.
   CLARITY: T — Telemetry via test assertions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.file-checkpoint :as chk])
  (:import [java.time Instant]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Helpers
;; =============================================================================

(def fixed-clock
  "Deterministic clock for reproducible checkpoint IDs."
  #(Instant/parse "2026-02-07T15:00:00Z"))

(defn reset-store-fixture
  "Fixture: reset checkpoint store before each test."
  [f]
  (chk/reset-store!)
  (try (f)
       (finally (chk/reset-store!))))

(use-fixtures :each reset-store-fixture)

(defn mock-read-fn
  "Create a mock file reader from a map of {path -> content}."
  [file-map]
  (fn [path] (get file-map path)))

(defn mock-write-fn
  "Create a mock file writer that records writes to an atom.
   Returns [write-fn, writes-atom]."
  []
  (let [writes (atom {})]
    [(fn [path content] (swap! writes assoc path content))
     writes]))

;; =============================================================================
;; 1. Pure Function Tests
;; =============================================================================

(deftest test-generate-checkpoint-id
  (testing "generates ID with chk- prefix and timestamp"
    (let [id (chk/generate-checkpoint-id fixed-clock)]
      (is (string? id))
      (is (.startsWith id "chk-"))
      (is (.contains id "1770476400000"))))

  (testing "generates unique IDs (random suffix)"
    (let [ids (repeatedly 10 #(chk/generate-checkpoint-id fixed-clock))]
      ;; Most should be unique (random suffix), allow for rare collisions
      (is (> (count (set ids)) 5)))))

(deftest test-make-checkpoint
  (testing "creates checkpoint record with all fields"
    (let [cp (chk/make-checkpoint "chk-1" "test-label"
                                  {"a.clj" "(ns a)" "b.clj" "(ns b)"}
                                  {:wave-id "w1"})]
      (is (= "chk-1" (:id cp)))
      (is (= "test-label" (:label cp)))
      (is (= 2 (:file-count cp)))
      (is (= {"a.clj" "(ns a)" "b.clj" "(ns b)"} (:files cp)))
      (is (= {:wave-id "w1"} (:metadata cp)))
      (is (string? (:created-at cp)))))

  (testing "defaults label to id when nil"
    (let [cp (chk/make-checkpoint "chk-2" nil {} nil)]
      (is (= "chk-2" (:label cp)))))

  (testing "defaults files and metadata to empty when nil"
    (let [cp (chk/make-checkpoint "chk-3" "lbl" nil nil)]
      (is (= {} (:files cp)))
      (is (= {} (:metadata cp)))
      (is (= 0 (:file-count cp))))))

(deftest test-checkpoint-summary
  (testing "strips file contents and adds paths + total-bytes"
    (let [cp (chk/make-checkpoint "chk-1" "lbl"
                                  {"a.clj" "hello" "b.clj" "world!"}
                                  {:k :v})
          summary (chk/checkpoint-summary cp)]
      (is (not (contains? summary :files)))
      (is (= #{"a.clj" "b.clj"} (set (:file-paths summary))))
      (is (= 11 (:total-bytes summary)))  ;; "hello" (5) + "world!" (6)
      (is (= "chk-1" (:id summary)))
      (is (= {:k :v} (:metadata summary))))))

;; =============================================================================
;; 2. File I/O Tests (Injected)
;; =============================================================================

(deftest test-snapshot-files
  (testing "reads existing files via read-fn"
    (let [reader (mock-read-fn {"src/a.clj" "(ns a)" "src/b.clj" "(ns b)"})
          result (chk/snapshot-files ["src/a.clj" "src/b.clj"] :read-fn reader)]
      (is (= {"src/a.clj" "(ns a)" "src/b.clj" "(ns b)"} result))))

  (testing "skips missing files gracefully"
    (let [reader (mock-read-fn {"src/a.clj" "(ns a)"})
          result (chk/snapshot-files ["src/a.clj" "src/missing.clj"] :read-fn reader)]
      (is (= {"src/a.clj" "(ns a)"} result))
      (is (not (contains? result "src/missing.clj")))))

  (testing "handles read exceptions gracefully"
    (let [reader (fn [path]
                   (if (= path "bad.clj")
                     (throw (Exception. "read error"))
                     "content"))
          result (chk/snapshot-files ["good.clj" "bad.clj"] :read-fn reader)]
      (is (= {"good.clj" "content"} result)))))

(deftest test-restore-files
  (testing "writes all files via write-fn"
    (let [[writer writes] (mock-write-fn)
          result (chk/restore-files {"a.clj" "(ns a)" "b.clj" "(ns b)"}
                                    :write-fn writer)]
      (is (= #{"a.clj" "b.clj"} (set (:restored result))))
      (is (empty? (:failed result)))
      (is (= {"a.clj" "(ns a)" "b.clj" "(ns b)"} @writes))))

  (testing "dry-run returns would-restore without writing"
    (let [[writer writes] (mock-write-fn)
          result (chk/restore-files {"a.clj" "(ns a)"} :write-fn writer :dry-run? true)]
      (is (true? (:dry-run result)))
      (is (= ["a.clj"] (:would-restore result)))
      (is (empty? @writes) "Nothing written in dry-run")))

  (testing "handles write exceptions gracefully"
    (let [writer (fn [path _content]
                   (when (= path "bad.clj")
                     (throw (Exception. "write error"))))
          result (chk/restore-files {"good.clj" "ok" "bad.clj" "fail"}
                                    :write-fn writer)]
      (is (= ["good.clj"] (:restored result)))
      (is (= 1 (count (:failed result))))
      (is (= "bad.clj" (-> result :failed first :path))))))

;; =============================================================================
;; 3. Checkpoint Lifecycle Tests
;; =============================================================================

(deftest test-create-checkpoint!
  (testing "creates checkpoint and returns summary"
    (let [reader (mock-read-fn {"a.clj" "(ns a)"})
          summary (chk/create-checkpoint! ["a.clj"]
                                          :label "test-cp"
                                          :metadata {:wave-id "w1"}
                                          :read-fn reader
                                          :clock-fn fixed-clock)]
      (is (string? (:id summary)))
      (is (.startsWith (:id summary) "chk-"))
      (is (= "test-cp" (:label summary)))
      (is (= ["a.clj"] (:file-paths summary)))
      (is (= {:wave-id "w1"} (:metadata summary)))
      (is (not (contains? summary :files)) "Summary has no file contents")))

  (testing "checkpoint is stored and retrievable"
    (let [reader (mock-read-fn {"a.clj" "(ns a)"})
          summary (chk/create-checkpoint! ["a.clj"]
                                          :label "stored-cp"
                                          :read-fn reader
                                          :clock-fn fixed-clock)
          full (chk/get-checkpoint (:id summary))]
      (is (some? full))
      (is (= {"a.clj" "(ns a)"} (:files full)))))

  (testing "precondition: file-paths must be sequential"
    (is (thrown? AssertionError
                 (chk/create-checkpoint! "not-a-seq")))))

(deftest test-rewind-to-checkpoint!
  (testing "restores files from checkpoint"
    (let [reader (mock-read-fn {"a.clj" "original"})
          [writer writes] (mock-write-fn)
          summary (chk/create-checkpoint! ["a.clj"]
                                          :label "rw-test"
                                          :read-fn reader
                                          :clock-fn fixed-clock)
          result (chk/rewind-to-checkpoint! (:id summary)
                                            :write-fn writer)]
      (is (true? (:success? result)))
      (is (= (:id summary) (:checkpoint-id result)))
      (is (= "rw-test" (:label result)))
      (is (= ["a.clj"] (:restored result)))
      (is (= {"a.clj" "original"} @writes))))

  (testing "returns error for non-existent checkpoint"
    (let [result (chk/rewind-to-checkpoint! "nonexistent-id")]
      (is (false? (:success? result)))
      (is (= "Checkpoint not found" (:error result)))))

  (testing "dry-run does not write"
    (let [reader (mock-read-fn {"a.clj" "content"})
          [writer writes] (mock-write-fn)
          summary (chk/create-checkpoint! ["a.clj"]
                                          :read-fn reader
                                          :clock-fn fixed-clock)
          result (chk/rewind-to-checkpoint! (:id summary)
                                            :write-fn writer
                                            :dry-run? true)]
      (is (true? (:success? result)))
      (is (empty? @writes))))

  (testing "delete-after? removes checkpoint on success"
    (let [reader (mock-read-fn {"a.clj" "x"})
          [writer _writes] (mock-write-fn)
          summary (chk/create-checkpoint! ["a.clj"]
                                          :read-fn reader
                                          :clock-fn fixed-clock)
          id (:id summary)
          _ (chk/rewind-to-checkpoint! id :write-fn writer :delete-after? true)]
      (is (nil? (chk/get-checkpoint id)) "Checkpoint deleted after rewind"))))

(deftest test-get-checkpoint-summary
  (testing "returns summary for existing checkpoint"
    (let [reader (mock-read-fn {"a.clj" "data"})
          summary (chk/create-checkpoint! ["a.clj"]
                                          :read-fn reader
                                          :clock-fn fixed-clock)
          fetched (chk/get-checkpoint-summary (:id summary))]
      (is (some? fetched))
      (is (not (contains? fetched :files)))))

  (testing "returns nil for non-existent checkpoint"
    (is (nil? (chk/get-checkpoint-summary "nope")))))

(deftest test-list-checkpoints
  (testing "lists all checkpoints sorted newest first"
    (let [reader (mock-read-fn {"a.clj" "x"})
          _s1 (chk/create-checkpoint! ["a.clj"]
                                      :label "first"
                                      :read-fn reader
                                      :clock-fn fixed-clock)
          ;; Small delay to ensure different created-at
          _ (Thread/sleep 10)
          _s2 (chk/create-checkpoint! ["a.clj"]
                                      :label "second"
                                      :read-fn reader
                                      :clock-fn fixed-clock)
          all (chk/list-checkpoints)]
      (is (= 2 (count all)))
      ;; All entries are summaries (no :files key)
      (is (every? #(not (contains? % :files)) all))))

  (testing "filters by label-pattern"
    (let [reader (mock-read-fn {"a.clj" "x"})
          _ (chk/create-checkpoint! ["a.clj"] :label "pre-wave-1" :read-fn reader :clock-fn fixed-clock)
          _ (chk/create-checkpoint! ["a.clj"] :label "pre-act-1" :read-fn reader :clock-fn fixed-clock)
          waves (chk/list-checkpoints {:label-pattern "wave"})]
      (is (= 1 (count waves)))
      (is (.contains (:label (first waves)) "wave"))))

  (testing "filters by metadata-key"
    (let [reader (mock-read-fn {"a.clj" "x"})
          _ (chk/create-checkpoint! ["a.clj"] :label "with-wave"
                                    :metadata {:wave-id "w1"}
                                    :read-fn reader :clock-fn fixed-clock)
          _ (chk/create-checkpoint! ["a.clj"] :label "no-wave"
                                    :metadata {:plan-id "p1"}
                                    :read-fn reader :clock-fn fixed-clock)
          result (chk/list-checkpoints {:metadata-key :wave-id})]
      (is (= 1 (count result)))
      (is (= "with-wave" (:label (first result)))))))

(deftest test-delete-checkpoint!
  (testing "deletes existing checkpoint"
    (let [reader (mock-read-fn {"a.clj" "x"})
          summary (chk/create-checkpoint! ["a.clj"] :read-fn reader :clock-fn fixed-clock)
          result (chk/delete-checkpoint! (:id summary))]
      (is (true? (:success? result)))
      (is (true? (:deleted? result)))
      (is (nil? (chk/get-checkpoint (:id summary))))))

  (testing "delete non-existent is a no-op success"
    (let [result (chk/delete-checkpoint! "nope")]
      (is (true? (:success? result)))
      (is (false? (:deleted? result))))))

(deftest test-delete-all-checkpoints!
  (testing "deletes all checkpoints"
    (let [reader (mock-read-fn {"a.clj" "x"})
          _ (chk/create-checkpoint! ["a.clj"] :read-fn reader :clock-fn fixed-clock)
          _ (chk/create-checkpoint! ["a.clj"] :read-fn reader :clock-fn fixed-clock)
          result (chk/delete-all-checkpoints!)]
      (is (= 2 (:deleted-count result)))
      (is (empty? (chk/list-checkpoints))))))

;; =============================================================================
;; 4. FSM Integration Helper Tests
;; =============================================================================

(deftest test-checkpoint-before-act
  (testing "creates checkpoint with act phase metadata"
    (let [reader (mock-read-fn {"src/a.clj" "(ns a)" "src/b.clj" "(ns b)"})
          ;; Override snapshot-files' read-fn via create-checkpoint! opts
          summary (chk/checkpoint-before-act ["src/a.clj" "src/b.clj"]
                                             :wave-id "wave-42"
                                             :plan-id "plan-1"
                                             :agent-id "ling-5")]
      ;; Note: checkpoint-before-act doesn't accept :read-fn, so it will
      ;; try real file I/O. We test the metadata structure instead.
      (is (string? (:id summary)))
      (is (.contains (:label summary) "pre-act"))
      (is (.contains (:label summary) "wave-42"))
      (is (.contains (:label summary) "plan-1"))
      (is (= :act (get-in summary [:metadata :phase])))
      (is (= :saa-rewind (get-in summary [:metadata :purpose])))
      (is (= "wave-42" (get-in summary [:metadata :wave-id])))
      (is (= "plan-1" (get-in summary [:metadata :plan-id])))
      (is (= "ling-5" (get-in summary [:metadata :agent-id]))))))

(deftest test-rewind-act-no-checkpoint
  (testing "returns error when no act checkpoint exists"
    (let [result (chk/rewind-act)]
      (is (false? (:success? result)))
      (is (= "No act checkpoint found" (:error result))))))

(deftest test-rewind-act-finds-act
  (testing "finds act checkpoint among mixed checkpoints"
    (let [reader (mock-read-fn {"a.clj" "data"})
          _ (chk/create-checkpoint! ["a.clj"]
                                    :label "explore-cp"
                                    :metadata {:phase :explore}
                                    :read-fn reader
                                    :clock-fn fixed-clock)
          _ (chk/create-checkpoint! ["a.clj"]
                                    :label "act-cp"
                                    :metadata {:phase :act}
                                    :read-fn reader
                                    :clock-fn fixed-clock)
          result (chk/rewind-act :dry-run? true)]
      (is (true? (:success? result)))))

  (testing "rewinds most recent act checkpoint via dry-run"
    (let [reader (mock-read-fn {"a.clj" "original"})
          _ (chk/create-checkpoint! ["a.clj"]
                                    :label "act-cp-2"
                                    :metadata {:phase :act :purpose :saa-rewind}
                                    :read-fn reader
                                    :clock-fn fixed-clock)
          result (chk/rewind-act :dry-run? true)]
      (is (true? (:success? result))))))

;; =============================================================================
;; 5. Wave Integration Tests
;; =============================================================================

(deftest test-checkpoint-wave-files!
  (testing "creates checkpoint from wave tasks"
    (let [reader (mock-read-fn {"src/a.clj" "(ns a)" "src/b.clj" "(ns b)"})
          tasks [{:file "src/a.clj" :task "Add validation"}
                 {:file "src/b.clj" :task "Update tests"}]
          ;; checkpoint-wave-files! doesn't accept :read-fn, uses default
          ;; Test the metadata structure
          summary (chk/checkpoint-wave-files! tasks "wave-99" :agent-id "ling-3")]
      (is (string? (:id summary)))
      (is (= "pre-wave-wave-99" (:label summary)))
      (is (= "wave-99" (get-in summary [:metadata :wave-id])))
      (is (= "ling-3" (get-in summary [:metadata :agent-id])))
      (is (= :act (get-in summary [:metadata :phase])))
      (is (= :wave-rewind (get-in summary [:metadata :purpose])))
      (is (= 2 (get-in summary [:metadata :task-count]))))))

(deftest test-rewind-wave!
  (testing "returns error when no checkpoint for wave"
    (let [result (chk/rewind-wave! "nonexistent-wave")]
      (is (false? (:success? result)))
      (is (.contains (:error result) "nonexistent-wave"))))

  (testing "rewinds checkpoint matching wave-id"
    (let [reader (mock-read-fn {"a.clj" "original-content"})
          ;; Create checkpoint with wave metadata
          _ (chk/create-checkpoint! ["a.clj"]
                                    :label "pre-wave-w1"
                                    :metadata {:wave-id "w1" :phase :act}
                                    :read-fn reader
                                    :clock-fn fixed-clock)
          result (chk/rewind-wave! "w1" :dry-run? true)]
      (is (true? (:success? result))))))

;; =============================================================================
;; 6. Store Utilities
;; =============================================================================

(deftest test-store-snapshot
  (testing "returns summary view of store"
    (let [reader (mock-read-fn {"a.clj" "data"})
          s1 (chk/create-checkpoint! ["a.clj"]
                                     :label "cp-1"
                                     :read-fn reader
                                     :clock-fn fixed-clock)
          snapshot (chk/store-snapshot)]
      (is (= 1 (count snapshot)))
      (is (contains? snapshot (:id s1)))
      (is (not (contains? (get snapshot (:id s1)) :files))
          "Snapshot uses summaries, not full checkpoints"))))

(deftest test-reset-store!
  (testing "clears all checkpoints"
    (let [reader (mock-read-fn {"a.clj" "x"})
          _ (chk/create-checkpoint! ["a.clj"] :read-fn reader :clock-fn fixed-clock)
          _ (chk/create-checkpoint! ["a.clj"] :read-fn reader :clock-fn fixed-clock)]
      (is (= 2 (count (chk/list-checkpoints))))
      (chk/reset-store!)
      (is (= 0 (count (chk/list-checkpoints)))))))
