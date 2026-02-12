(ns hive-mcp.knowledge-graph.store.protocol-test
  "Dual-backend tests for IGraphStore protocol.

   Verifies that both DataScript and Datalevin implementations
   produce identical behavior for all protocol operations.

   Each test runs against both backends via dual-backend-fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.knowledge-graph.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fixtures/dual-backend-fixture)

;; =============================================================================
;; Helpers
;; =============================================================================

(defn gen-node-id []
  (str "test-node-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn gen-path []
  (str "/test/path/" (subs (str (java.util.UUID/randomUUID)) 0 8) ".clj"))

;; =============================================================================
;; Protocol Core Operations
;; =============================================================================

(deftest ensure-conn-returns-non-nil-test
  (testing (str "ensure-conn! returns non-nil connection [" fixtures/*current-backend* "]")
    (let [store (proto/get-store)]
      (is (some? (proto/ensure-conn! store))))))

(deftest transact-and-query-roundtrip-test
  (testing (str "transact! + query roundtrip [" fixtures/*current-backend* "]")
    (let [store (proto/get-store)
          edge-id (str "test-edge-" (random-uuid))]
      (proto/transact! store [{:kg-edge/id edge-id
                               :kg-edge/from "node-a"
                               :kg-edge/to "node-b"
                               :kg-edge/relation :implements
                               :kg-edge/confidence 1.0}])
      (let [results (proto/query store
                                 '[:find ?from ?to
                                   :in $ ?eid
                                   :where
                                   [?e :kg-edge/id ?eid]
                                   [?e :kg-edge/from ?from]
                                   [?e :kg-edge/to ?to]]
                                 [edge-id])]
        (is (= #{["node-a" "node-b"]} (set results)))))))

(deftest entid-resolves-lookup-ref-test
  (testing (str "entid resolves lookup ref [" fixtures/*current-backend* "]")
    (let [store (proto/get-store)
          edge-id (str "test-edge-" (random-uuid))]
      (proto/transact! store [{:kg-edge/id edge-id
                               :kg-edge/from "a"
                               :kg-edge/to "b"
                               :kg-edge/relation :implements
                               :kg-edge/confidence 1.0}])
      (let [eid (proto/entid store [:kg-edge/id edge-id])]
        (is (some? eid))
        (is (number? eid))))))

(deftest pull-entity-returns-full-entity-test
  (testing (str "pull-entity returns full entity [" fixtures/*current-backend* "]")
    (let [store (proto/get-store)
          edge-id (str "test-edge-" (random-uuid))]
      (proto/transact! store [{:kg-edge/id edge-id
                               :kg-edge/from "a"
                               :kg-edge/to "b"
                               :kg-edge/relation :refines
                               :kg-edge/confidence 0.7}])
      (let [eid (proto/entid store [:kg-edge/id edge-id])
            pulled (proto/pull-entity store '[*] eid)]
        (is (= edge-id (:kg-edge/id pulled)))
        (is (= "a" (:kg-edge/from pulled)))
        (is (= :refines (:kg-edge/relation pulled)))
        (is (= 0.7 (:kg-edge/confidence pulled)))))))

(deftest db-snapshot-returns-value-test
  (testing (str "db-snapshot returns immutable value [" fixtures/*current-backend* "]")
    (let [store (proto/get-store)
          snap (proto/db-snapshot store)]
      (is (some? snap)))))

(deftest reset-conn-clears-data-test
  (testing (str "reset-conn! clears all data [" fixtures/*current-backend* "]")
    (let [store (proto/get-store)]
      ;; Add data
      (proto/transact! store [{:kg-edge/id (str (random-uuid))
                               :kg-edge/from "a"
                               :kg-edge/to "b"
                               :kg-edge/relation :implements
                               :kg-edge/confidence 1.0}])
      ;; Verify data exists
      (let [before (proto/query store '[:find ?e :where [?e :kg-edge/id]])]
        (is (= 1 (count before))))
      ;; Reset
      (proto/reset-conn! store)
      ;; Verify data is gone
      (let [after (proto/query store '[:find ?e :where [?e :kg-edge/id]])]
        (is (= 0 (count after)))))))

;; =============================================================================
;; Edge Operations via Connection Layer
;; =============================================================================

(deftest edges-add-and-get-test
  (testing (str "edges add and get via connection layer [" fixtures/*current-backend* "]")
    (let [from (gen-node-id)
          to (gen-node-id)
          edge-id (edges/add-edge! {:from from :to to :relation :implements})]
      (is (string? edge-id))
      (let [edge (edges/get-edge edge-id)]
        (is (some? edge))
        (is (= from (:kg-edge/from edge)))
        (is (= to (:kg-edge/to edge)))
        (is (= :implements (:kg-edge/relation edge)))))))

(deftest edges-query-from-to-test
  (testing (str "edges query from/to [" fixtures/*current-backend* "]")
    (let [src (gen-node-id)
          tgt1 (gen-node-id)
          tgt2 (gen-node-id)]
      (edges/add-edge! {:from src :to tgt1 :relation :implements})
      (edges/add-edge! {:from src :to tgt2 :relation :supersedes})
      (is (= 2 (count (edges/get-edges-from src))))
      (is (= 1 (count (edges/get-edges-to tgt1)))))))

(deftest edges-confidence-update-test
  (testing (str "edges confidence update [" fixtures/*current-backend* "]")
    (let [edge-id (edges/add-edge! {:from (gen-node-id)
                                    :to (gen-node-id)
                                    :relation :implements
                                    :confidence 0.5})]
      (edges/update-edge-confidence! edge-id 0.9)
      (is (= 0.9 (:kg-edge/confidence (edges/get-edge edge-id)))))))

(deftest edges-remove-test
  (testing (str "edges remove [" fixtures/*current-backend* "]")
    (let [edge-id (edges/add-edge! {:from (gen-node-id)
                                    :to (gen-node-id)
                                    :relation :implements})]
      (edges/remove-edge! edge-id)
      (is (nil? (edges/get-edge edge-id))))))

(deftest edges-all-relations-test
  (testing (str "all relation types work [" fixtures/*current-backend* "]")
    (doseq [rel schema/relation-types]
      (let [edge-id (edges/add-edge! {:from (gen-node-id)
                                      :to (gen-node-id)
                                      :relation rel})]
        (is (string? edge-id) (str "Failed for relation: " rel))))))

(deftest edges-by-relation-test
  (testing (str "edges by relation filter [" fixtures/*current-backend* "]")
    (edges/add-edge! {:from (gen-node-id) :to (gen-node-id) :relation :implements})
    (edges/add-edge! {:from (gen-node-id) :to (gen-node-id) :relation :implements})
    (edges/add-edge! {:from (gen-node-id) :to (gen-node-id) :relation :supersedes})
    (is (= 2 (count (edges/get-edges-by-relation :implements))))
    (is (= 1 (count (edges/get-edges-by-relation :supersedes))))))

(deftest edge-stats-test
  (testing (str "edge stats [" fixtures/*current-backend* "]")
    (edges/add-edge! {:from (gen-node-id) :to (gen-node-id)
                      :relation :implements :scope "proj-a"})
    (edges/add-edge! {:from (gen-node-id) :to (gen-node-id)
                      :relation :supersedes :scope "proj-b"})
    (let [stats (edges/edge-stats)]
      (is (= 2 (:total-edges stats)))
      (is (= 1 (get-in stats [:by-relation :implements])))
      (is (= 1 (get-in stats [:by-scope "proj-a"]))))))

;; =============================================================================
;; Disc Operations via Connection Layer
;; =============================================================================

(deftest disc-add-and-get-test
  (testing (str "disc add and get [" fixtures/*current-backend* "]")
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "abc123"})
      (is (disc/disc-exists? path))
      (let [d (disc/get-disc path)]
        (is (= path (:disc/path d)))
        (is (= "abc123" (:disc/content-hash d)))))))

(deftest disc-update-test
  (testing (str "disc update [" fixtures/*current-backend* "]")
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "old-hash"})
      (disc/update-disc! path {:disc/content-hash "new-hash"})
      (is (= "new-hash" (:disc/content-hash (disc/get-disc path)))))))

(deftest disc-remove-test
  (testing (str "disc remove [" fixtures/*current-backend* "]")
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (is (true? (disc/remove-disc! path)))
      (is (not (disc/disc-exists? path))))))

(deftest disc-all-and-stats-test
  (testing (str "disc get-all and stats [" fixtures/*current-backend* "]")
    (disc/add-disc! {:path (gen-path) :content-hash "h1" :project-id "proj-a"})
    (disc/add-disc! {:path (gen-path) :content-hash "h2" :project-id "proj-b"})
    (is (= 2 (count (disc/get-all-discs))))
    (is (= 1 (count (disc/get-all-discs :project-id "proj-a"))))
    (let [stats (disc/disc-stats)]
      (is (= 2 (:total stats))))))
