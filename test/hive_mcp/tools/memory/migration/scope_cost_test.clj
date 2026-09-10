(ns hive-mcp.tools.memory.migration.scope-cost-test
  "migrate-scoped's cost per entry, held by recording stub STORES.

   Re-filing one ingested document moves one entry per chunk, so this handler's
   per-entry cost is multiplied by thousands. Two optional ports already exist to
   pay it cheaply and the handler has to actually reach for them:

     IMemoryStoreBatch         one backend round trip for many ids
     IMemoryStoreMetadataWrite a write that KEEPS the existing embedding

   The store is a real record implementing those ports, so what these tests
   assert is which protocol method the handler called, not which var was
   redefined. `get-store` is still redefined because the handler resolves the
   store from a registry with no parameter seam."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.memory.migration.core :as migration]
            [hive-spi.memory.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- entry-for [id] {:id id :tags ["t" "scope:project:mac"] :content "body"})

(def ^:private chunk-ids (mapv #(str "chunk-" %) (range 40)))

;; =============================================================================
;; A store with both optional ports
;; =============================================================================

(defrecord FastStore [calls]
  ports/IMemoryStore
  (get-entry [_ id]
    (swap! calls update :get-entry (fnil conj []) id)
    (entry-for id))
  (update-entry! [_ id updates]
    (swap! calls update :update-entry! (fnil conj []) id)
    (merge (entry-for id) updates))
  (query-entries [_ _opts] [])

  ports/IMemoryStoreBatch
  (get-entries [_ ids]
    (swap! calls update :get-entries (fnil conj []) (vec ids))
    (mapv entry-for ids))

  ports/IMemoryStoreMetadataWrite
  (update-metadata! [_ id updates]
    (swap! calls update :update-metadata! (fnil conj []) id)
    (merge (entry-for id) updates)))

;; =============================================================================
;; A store with neither, so the fallback is exercised rather than assumed
;; =============================================================================

(defrecord PlainStore [calls]
  ports/IMemoryStore
  (get-entry [_ id]
    (swap! calls update :get-entry (fnil conj []) id)
    (entry-for id))
  (update-entry! [_ id updates]
    (swap! calls update :update-entry! (fnil conj []) id)
    (merge (entry-for id) updates))
  (query-entries [_ _opts] []))

(defn- migrate-with
  [store ids]
  (with-redefs [mem-proto/store-set? (fn [] true)
                mem-proto/get-store  (fn [] store)]
    (migration/handle-migrate-scoped {:entry-ids      ids
                                     :old-project-id "mac"
                                     :new-project-id "topic:masonry"})))

;; =============================================================================
;; Reads
;; =============================================================================

(deftest a-batch-capable-store-is-read-in-batches-test
  (let [calls (atom {})
        res   (migrate-with (->FastStore calls) chunk-ids)]
    (testing "the ids go out together, not one call per entry"
      (is (seq (:get-entries @calls)))
      (is (nil? (:get-entry @calls))
          "a per-id read costs one backend round trip per chunk of the document")
      (is (= (set chunk-ids) (set (mapcat identity (:get-entries @calls))))
          "and every requested id is still asked for"))
    (testing "the migration reports them all as migrated"
      (is (not (:isError res))))))

(deftest a-plain-store-still-migrates-test
  (let [calls (atom {})
        res   (migrate-with (->PlainStore calls) (vec (take 3 chunk-ids)))]
    (testing "without IMemoryStoreBatch the handler falls back to per-id reads"
      (is (= 3 (count (:get-entry @calls))))
      (is (nil? (:get-entries @calls))))
    (testing "and the work still happens"
      (is (= 3 (count (:update-entry! @calls))))
      (is (not (:isError res))))))

;; =============================================================================
;; Writes
;; =============================================================================

(deftest a-scope-change-never-re-embeds-when-the-store-can-avoid-it-test
  (let [calls (atom {})]
    (migrate-with (->FastStore calls) (vec (take 10 chunk-ids)))
    (testing "the no-embed metadata write is the one that is used"
      (is (= 10 (count (:update-metadata! @calls))))
      (is (nil? (:update-entry! @calls))
          (str "update-entry! re-runs the embedder on content the migration did not "
               "touch, which for a re-filed document is one embedding per chunk")))))

(deftest a-dry-run-writes-nothing-test
  (let [calls (atom {})]
    (with-redefs [mem-proto/store-set? (fn [] true)
                  mem-proto/get-store  (fn [] (->FastStore calls))]
      (migration/handle-migrate-scoped {:entry-ids      (vec (take 5 chunk-ids))
                                       :old-project-id "mac"
                                       :new-project-id "topic:masonry"
                                       :dry-run        true}))
    (is (nil? (:update-metadata! @calls)))
    (is (nil? (:update-entry! @calls)))))
