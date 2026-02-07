(ns hive-mcp.agent.drone.session-kg-test
  "Tests for per-drone session KG lifecycle and requiring-resolve stubs.

   Tests verify:
   - Session KG lifecycle (create, close, cleanup) — AGPL, open code
   - Schema definitions are correct — AGPL, open code
   - Stub noop fallbacks for proprietary operations — AGPL, stub contracts
   - DatalevinStore factory integration — AGPL, open code

   Note: The actual observation recording, reasoning tracking, and context
   reconstruction algorithms are proprietary (hive-knowledge). These tests
   verify the stub contracts only."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.drone.session-kg :as skg]
            [hive-mcp.protocols.kg :as kg]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:dynamic *test-store* nil)
(def ^:dynamic *test-drone-id* nil)

(defn session-kg-fixture
  "Create a fresh session KG for each test, clean up after."
  [f]
  (let [drone-id (str "test-drone-" (System/currentTimeMillis))
        store (skg/create-session-kg! drone-id)]
    (if store
      (binding [*test-store* store
                *test-drone-id* drone-id]
        (try
          (f)
          (finally
            (skg/close-session-kg! store drone-id :cleanup? true))))
      ;; If Datalevin not available, skip tests gracefully
      (println "SKIP: Datalevin not available, skipping session-kg tests"))))

(use-fixtures :each session-kg-fixture)

;; =============================================================================
;; Schema Tests (AGPL — structural, not algorithmic)
;; =============================================================================

(deftest test-session-schema-structure
  (testing "session schema has expected observation attributes"
    (is (contains? skg/session-schema :obs/id))
    (is (contains? skg/session-schema :obs/turn))
    (is (contains? skg/session-schema :obs/tool))
    (is (contains? skg/session-schema :obs/summary))
    (is (contains? skg/session-schema :obs/success))
    (is (contains? skg/session-schema :obs/key-facts)))

  (testing "session schema has reasoning attributes"
    (is (contains? skg/session-schema :reason/id))
    (is (contains? skg/session-schema :reason/turn))
    (is (contains? skg/session-schema :reason/intent))
    (is (contains? skg/session-schema :reason/rationale)))

  (testing "session schema has goal tracking attributes"
    (is (contains? skg/session-schema :goal/id))
    (is (contains? skg/session-schema :goal/description))
    (is (contains? skg/session-schema :goal/status)))

  (testing "session schema has dependency edge attributes"
    (is (contains? skg/session-schema :dep/from))
    (is (contains? skg/session-schema :dep/to))
    (is (contains? skg/session-schema :dep/relation)))

  (testing "obs/id has unique identity constraint"
    (is (= :db.unique/identity (get-in skg/session-schema [:obs/id :db/unique]))))

  (testing "obs/key-facts has many cardinality"
    (is (= :db.cardinality/many (get-in skg/session-schema [:obs/key-facts :db/cardinality])))))

;; =============================================================================
;; Session KG Lifecycle Tests (AGPL — factory/CRUD)
;; =============================================================================

(deftest test-session-db-path
  (testing "generates correct temp path for drone"
    (is (= "/tmp/drone-abc-123/kg" (skg/session-db-path "abc-123")))))

(deftest test-create-session-kg
  (testing "creates a functional IKGStore"
    (is (some? *test-store*))
    (is (satisfies? kg/IKGStore *test-store*))))

(deftest test-session-kg-transact-and-query
  (testing "can transact and query data in session KG"
    (when *test-store*
      (kg/transact! *test-store*
                    [{:obs/id "test-obs-1"
                      :obs/turn 0
                      :obs/tool "read_file"
                      :obs/summary "Read core.clj, found 5 functions"
                      :obs/success true}])
      (let [results (kg/query *test-store*
                              '[:find ?id ?summary
                                :where
                                [?e :obs/id ?id]
                                [?e :obs/summary ?summary]])]
        (is (= 1 (count results)))
        (is (= "test-obs-1" (ffirst results)))))))

;; =============================================================================
;; Observation Recording Stub Tests
;; =============================================================================

(deftest test-record-observation-returns-id
  (testing "record-observation! returns deterministic obs-id string"
    (let [obs-id (skg/record-observation!
                  *test-store* 0 "read_file"
                  {:success true :result {:text "(ns my.ns)"}})]
      (is (= "obs-0-read_file" obs-id))))

  (testing "obs-id encodes turn and tool name"
    (let [obs-id (skg/record-observation!
                  *test-store* 3 "grep"
                  {:success false :error "not found"})]
      (is (= "obs-3-grep" obs-id)))))

(deftest test-record-observation-nil-store
  (testing "handles nil store gracefully"
    (let [obs-id (skg/record-observation!
                  nil 0 "read_file"
                  {:success true :result {:text "content"}})]
      (is (= "obs-0-read_file" obs-id)))))

;; =============================================================================
;; Reasoning Recording Stub Tests
;; =============================================================================

(deftest test-record-reasoning-returns-id
  (testing "record-reasoning! returns deterministic reason-id string"
    (let [reason-id (skg/record-reasoning!
                     *test-store* 0
                     "Read the target file"
                     "Need to see current code")]
      (is (= "reason-0" reason-id)))))

(deftest test-record-reasoning-nil-store
  (testing "handles nil store gracefully"
    (let [reason-id (skg/record-reasoning! nil 5 "intent" "rationale")]
      (is (= "reason-5" reason-id)))))

;; =============================================================================
;; Context Reconstruction Stub Tests
;; =============================================================================

(deftest test-reconstruct-context-noop-returns-task
  (testing "with stubs, returns raw task description as fallback"
    (let [ctx (skg/reconstruct-context *test-store* "Fix the bug in core.clj" 0)]
      (is (string? ctx))
      (is (= "Fix the bug in core.clj" ctx)))))

(deftest test-reconstruct-context-nil-store-fallback
  (testing "with nil store, returns raw task"
    (let [ctx (skg/reconstruct-context nil "My task" 0)]
      (is (= "My task" ctx)))))

;; =============================================================================
;; Merge-Back Stub Tests
;; =============================================================================

(deftest test-merge-session-to-global-noop
  (testing "returns nil when hive-knowledge not available"
    (let [result (skg/merge-session-to-global! *test-store* nil)]
      (is (nil? result)))))

;; =============================================================================
;; Seed-from-Global Stub Tests
;; =============================================================================

(deftest test-seed-from-global-noop
  (testing "returns 0 when hive-knowledge not available"
    (let [result (skg/seed-from-global! *test-store* nil
                                        {:task "test" :files ["a.clj"]})]
      (is (= 0 result)))))
