(ns hive-mcp.tools.consolidated.session-property-test
  "Property tests for session Result-returning pure functions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.dns.result :as result]
            [hive-mcp.tools.consolidated.session :as session]
            [hive-mcp.channel.context-store :as ctx-store]))

;; ── Fixtures ──────────────────────────────────────────────────────────────────

(defn clean-context-store-fixture [f]
  (ctx-store/reset-all!)
  (try (f)
       (finally
         (ctx-store/stop-reaper!)
         (ctx-store/reset-all!))))

(use-fixtures :each clean-context-store-fixture)

;; ── Access private fns via var deref ──────────────────────────────────────────

(def context-put* @#'session/context-put*)
(def context-get* @#'session/context-get*)
(def context-evict* @#'session/context-evict*)
(def context-stats* @#'session/context-stats*)
(def keywordize-refs @#'session/keywordize-refs)

;; ── P1: context-put* with nil data -> err Result ─────────────────────────────

(deftest context-put-nil-data-returns-err
  (testing "context-put* without data returns err Result"
    (let [r (context-put* {:data nil})]
      (is (result/err? r))
      (is (= :session/context-put (:error r))))))

(deftest context-put-missing-data-returns-err
  (testing "context-put* with empty params returns err Result"
    (let [r (context-put* {})]
      (is (result/err? r)))))

;; ── P2: context-put* with valid data -> ok Result ────────────────────────────

(deftest context-put-valid-data-returns-ok
  (testing "context-put* with valid data returns ok Result with ctx-id"
    (let [r (context-put* {:data {"hello" "world"}})]
      (is (result/ok? r))
      (is (string? (:ctx-id (:ok r))))
      (is (number? (:ttl-ms (:ok r)))))))

;; ── P3: context-get* without ctx_id -> err Result ────────────────────────────

(deftest context-get-nil-id-returns-err
  (testing "context-get* without ctx_id returns err Result"
    (let [r (context-get* {})]
      (is (result/err? r))
      (is (= :session/context-get (:error r))))))

;; ── P4: context-evict* without ctx_id -> err Result ──────────────────────────

(deftest context-evict-nil-id-returns-err
  (testing "context-evict* without ctx_id returns err Result"
    (let [r (context-evict* {})]
      (is (result/err? r))
      (is (= :session/context-evict (:error r))))))

;; ── P5: context-stats* always returns ok Result ──────────────────────────────

(deftest context-stats-returns-ok
  (testing "context-stats* always returns ok Result"
    (let [r (context-stats* {})]
      (is (result/ok? r))
      (is (map? (:ok r))))))

;; ── P6: keywordize-refs idempotent ───────────────────────────────────────────

(defspec keywordize-refs-idempotent 200
  (prop/for-all [m (gen/map gen/string-alphanumeric gen/string-alphanumeric {:max-elements 5})]
                (let [once (keywordize-refs m)
                      twice (keywordize-refs once)]
                  (= once twice))))

;; ── P7: context-put -> get roundtrip ─────────────────────────────────────────

(deftest context-put-get-roundtrip
  (testing "data stored via context-put* can be retrieved via context-get*"
    (let [put-r (context-put* {:data {"key" "value"}})
          _ (is (result/ok? put-r))
          ctx-id (:ctx-id (:ok put-r))
          get-r (context-get* {:ctx_id ctx-id})]
      (is (result/ok? get-r))
      (is (= {"key" "value"} (:data (:ok get-r)))))))
