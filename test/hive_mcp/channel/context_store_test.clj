(ns hive-mcp.channel.context-store-test
  "TDD tests for ephemeral context store.
   Tests: put/get lifecycle, TTL expiry, query by tags,
   concurrent access, eviction, stats."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.context-store :as ctx]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn clean-store-fixture [f]
  (ctx/reset-all!)
  (try (f)
       (finally
         (ctx/stop-reaper!)
         (ctx/reset-all!))))

(use-fixtures :each clean-store-fixture)

;; =============================================================================
;; Put / Get Lifecycle
;; =============================================================================

(deftest test-put-returns-ctx-id
  (testing "context-put! returns a string ID matching ctx-{timestamp}-{8hex} format"
    (let [id (ctx/context-put! {:foo "bar"})]
      (is (string? id))
      (is (re-matches #"ctx-\d+-[0-9a-f]{8}" id)))))

(deftest test-put-get-roundtrip
  (testing "data stored with put can be retrieved with get"
    (let [data {:message "hello" :count 42}
          id (ctx/context-put! data)]
      (is (some? id))
      (let [entry (ctx/context-get id)]
        (is (= data (:data entry)))
        (is (= id (:id entry)))
        (is (number? (:created-at entry)))
        (is (number? (:expires-at entry)))
        (is (= 1 (:access-count entry)))
        (is (number? (:last-accessed entry)))))))

(deftest test-get-nonexistent-returns-nil
  (testing "context-get on missing ID returns nil"
    (is (nil? (ctx/context-get "ctx-0000-deadbeef")))))

(deftest test-get-increments-access-count
  (testing "each get increments access-count and updates last-accessed"
    (let [id (ctx/context-put! {:data "test"})]
      (ctx/context-get id)
      (ctx/context-get id)
      (let [entry (ctx/context-get id)]
        (is (= 3 (:access-count entry)))
        (is (number? (:last-accessed entry)))))))

;; =============================================================================
;; Tags
;; =============================================================================

(deftest test-put-with-tags
  (testing "tags are stored as a set"
    (let [id (ctx/context-put! {:x 1} :tags #{"catchup" "agent-xyz"})]
      (is (= #{"catchup" "agent-xyz"} (:tags (ctx/context-get id)))))))

;; =============================================================================
;; TTL & Expiry
;; =============================================================================

(deftest test-default-ttl
  (testing "default TTL is 300000ms (5 min)"
    (let [id (ctx/context-put! {:x 1})
          entry (ctx/context-get id)]
      (is (= 300000 (:ttl-ms entry)))
      ;; expires-at should be ~5 min after created-at
      (is (> (:expires-at entry) (:created-at entry)))
      (is (<= (- (:expires-at entry) (:created-at entry)) 300001)))))

(deftest test-custom-ttl
  (testing "custom TTL is respected"
    (let [id (ctx/context-put! {:x 1} :ttl-ms 1000)
          entry (ctx/context-get id)]
      (is (= 1000 (:ttl-ms entry))))))

(deftest test-expired-entry-returns-nil
  (testing "expired entries are not returned by context-get"
    (let [id (ctx/context-put! {:x 1} :ttl-ms 50)]
      (is (some? (ctx/context-get id)))
      (Thread/sleep 80)
      (is (nil? (ctx/context-get id))))))

;; =============================================================================
;; Query by Tags
;; =============================================================================

(deftest test-query-by-tags
  (testing "context-query filters by tags"
    (ctx/context-put! {:a 1} :tags #{"session" "agent-1"})
    (ctx/context-put! {:b 2} :tags #{"session" "agent-2"})
    (ctx/context-put! {:c 3} :tags #{"other"})
    (let [results (ctx/context-query :tags #{"session"})]
      (is (= 2 (count results)))
      (is (every? #(contains? (:tags %) "session") results)))))

(deftest test-query-limit
  (testing "context-query respects limit"
    (dotimes [i 5]
      (ctx/context-put! {:i i} :tags #{"bulk"}))
    (let [results (ctx/context-query :tags #{"bulk"} :limit 3)]
      (is (= 3 (count results))))))

(deftest test-query-excludes-expired
  (testing "context-query does not return expired entries"
    (ctx/context-put! {:live "yes"} :tags #{"test"} :ttl-ms 5000)
    (ctx/context-put! {:dead "yes"} :tags #{"test"} :ttl-ms 50)
    (Thread/sleep 80)
    (let [results (ctx/context-query :tags #{"test"})]
      (is (= 1 (count results)))
      (is (= "yes" (:live (:data (first results))))))))

;; =============================================================================
;; Eviction
;; =============================================================================

(deftest test-evict-removes-entry
  (testing "context-evict! removes entry and returns true"
    (let [id (ctx/context-put! {:x 1})]
      (is (true? (ctx/context-evict! id)))
      (is (nil? (ctx/context-get id))))))

(deftest test-evict-nonexistent-returns-false
  (testing "context-evict! on missing ID returns false"
    (is (false? (ctx/context-evict! "ctx-0000-deadbeef")))))

;; =============================================================================
;; Stats
;; =============================================================================

(deftest test-stats
  (testing "context-stats returns correct counts"
    (ctx/context-put! {:a 1})
    (ctx/context-put! {:b 2})
    (let [stats (ctx/context-stats)]
      (is (= 2 (:total stats)))
      (is (number? (:oldest stats)))
      (is (number? (:newest stats)))
      (is (number? (:bytes-approx stats)))
      (is (<= (:oldest stats) (:newest stats))))))

(deftest test-stats-empty-store
  (testing "context-stats on empty store"
    (let [stats (ctx/context-stats)]
      (is (= 0 (:total stats)))
      (is (nil? (:oldest stats)))
      (is (nil? (:newest stats))))))

;; =============================================================================
;; Reaper
;; =============================================================================

(deftest test-reaper-lifecycle
  (testing "start-reaper! and stop-reaper! work without error"
    (ctx/start-reaper!)
    ;; Should be idempotent
    (ctx/start-reaper!)
    (ctx/stop-reaper!)
    ;; Double stop is safe
    (ctx/stop-reaper!)))

(deftest test-reaper-evicts-expired
  (testing "reaper removes expired entries"
    (ctx/context-put! {:dead "soon"} :ttl-ms 50)
    (ctx/context-put! {:alive "long"} :ttl-ms 60000)
    (Thread/sleep 80)
    ;; Manual reap cycle (same as reaper task)
    (ctx/reap-expired!)
    (let [stats (ctx/context-stats)]
      (is (= 1 (:total stats))))))

;; =============================================================================
;; Concurrent Access
;; =============================================================================

(deftest test-concurrent-puts
  (testing "concurrent puts don't lose entries"
    (let [n 100
          ids (atom [])
          threads (mapv (fn [i]
                          (future
                            (let [id (ctx/context-put! {:i i} :tags #{"concurrent"})]
                              (swap! ids conj id))))
                        (range n))]
      ;; Wait for all
      (run! deref threads)
      (is (= n (count @ids)))
      (is (= n (:total (ctx/context-stats)))))))

(deftest test-concurrent-get-and-put
  (testing "concurrent gets and puts don't corrupt state"
    (let [id (ctx/context-put! {:seed "data"})
          n 50
          get-threads (mapv (fn [_] (future (ctx/context-get id))) (range n))
          put-threads (mapv (fn [i] (future (ctx/context-put! {:i i}))) (range n))]
      (run! deref get-threads)
      (run! deref put-threads)
      ;; Original entry should still be intact
      (let [entry (ctx/context-get id)]
        (is (= {:seed "data"} (:data entry)))
        ;; access-count = n get-threads + 1 final get
        (is (= (+ n 1) (:access-count entry)))))))

;; =============================================================================
;; Evict by Tags (session cleanup)
;; =============================================================================

(deftest test-evict-by-tags-basic
  (testing "evict-by-tags! removes entries matching any given tag"
    (ctx/context-put! {:a 1} :tags #{"agent:ling-1" "session"})
    (ctx/context-put! {:b 2} :tags #{"agent:ling-1" "catchup"})
    (ctx/context-put! {:c 3} :tags #{"agent:ling-2" "session"})
    (ctx/context-put! {:d 4} :tags #{"unrelated"})
    (is (= 4 (:total (ctx/context-stats))))
    (let [n (ctx/evict-by-tags! #{"agent:ling-1"})]
      (is (= 2 n))
      (is (= 2 (:total (ctx/context-stats)))))))

(deftest test-evict-by-tags-or-semantics
  (testing "evict-by-tags! uses OR (any matching tag triggers eviction)"
    (ctx/context-put! {:a 1} :tags #{"tag-a"})
    (ctx/context-put! {:b 2} :tags #{"tag-b"})
    (ctx/context-put! {:c 3} :tags #{"tag-c"})
    (let [n (ctx/evict-by-tags! #{"tag-a" "tag-b"})]
      (is (= 2 n))
      (is (= 1 (:total (ctx/context-stats)))))))

(deftest test-evict-by-tags-empty-tags
  (testing "evict-by-tags! with empty tags evicts nothing"
    (ctx/context-put! {:a 1} :tags #{"foo"})
    (is (= 0 (ctx/evict-by-tags! #{})))
    (is (= 1 (:total (ctx/context-stats))))))

(deftest test-evict-by-tags-no-match
  (testing "evict-by-tags! with non-matching tags evicts nothing"
    (ctx/context-put! {:a 1} :tags #{"foo"})
    (is (= 0 (ctx/evict-by-tags! #{"nonexistent"})))
    (is (= 1 (:total (ctx/context-stats))))))

(deftest test-evict-by-tags-skips-expired
  (testing "evict-by-tags! does not count expired entries"
    (ctx/context-put! {:a 1} :tags #{"target"} :ttl-ms 50)
    (ctx/context-put! {:b 2} :tags #{"target"} :ttl-ms 60000)
    (Thread/sleep 80)
    ;; Only 1 live entry with "target" tag now
    (let [n (ctx/evict-by-tags! #{"target"})]
      (is (= 1 n)))))

;; =============================================================================
;; Reset
;; =============================================================================

(deftest test-reset-all
  (testing "reset-all! clears all entries"
    (ctx/context-put! {:a 1})
    (ctx/context-put! {:b 2})
    (ctx/reset-all!)
    (is (= 0 (:total (ctx/context-stats))))))
