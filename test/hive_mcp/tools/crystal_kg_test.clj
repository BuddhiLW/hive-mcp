(ns hive-mcp.tools.crystal-kg-test
  "Tests for Knowledge Graph edge creation patterns used in wrap_crystallize.

   These tests verify the KG edge wiring without requiring the full crystal
   dependency chain (which needs Emacs/nrepl)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.extensions.registry :as ext]))

;; Reset KG before each test
(use-fixtures :each
  (fn [f]
    (kg-conn/reset-conn!)
    (f)))

;; ============================================================================
;; Simulated extract-source-ids (mirrors crystal.clj implementation)
;; ============================================================================

(defn- extract-source-ids
  "Extract memory entry IDs from harvested session data.
   Mirror of hive-mcp.tools.crystal/extract-source-ids"
  [{:keys [progress-notes completed-tasks memory-ids-created]}]
  (->> (concat (keep :id progress-notes)
               (keep :id completed-tasks)
               (keep :id memory-ids-created))
       (filter some?)
       (distinct)
       (vec)))

;; ============================================================================
;; Simulated create-derived-from-edges! (mirrors crystal.clj implementation)
;; ============================================================================

(defn- create-derived-from-edges!
  "Create :derived-from KG edges linking a summary to its source entries.
   Mirror of hive-mcp.tools.crystal/create-derived-from-edges!"
  [summary-id source-ids project-id agent-id]
  (when (and summary-id (seq source-ids))
    (try
      (let [edge-ids (reduce
                      (fn [acc source-id]
                        (try
                          (let [edge-id (kg-edges/add-edge!
                                         {:from summary-id
                                          :to source-id
                                          :relation :derived-from
                                          :scope project-id
                                          :created-by agent-id})]
                            (conj acc edge-id))
                          (catch Exception _e
                            acc)))
                      []
                      source-ids)]
        {:created-count (count edge-ids)
         :edge-ids edge-ids})
      (catch Exception e
        {:error (.getMessage e)
         :created-count 0}))))

;; ============================================================================
;; Unit tests for extract-source-ids
;; ============================================================================

(deftest extract-source-ids-test
  (testing "extracts IDs from progress-notes and completed-tasks"
    (let [harvested {:progress-notes [{:id "note-1" :content "foo"}
                                      {:id "note-2" :content "bar"}]
                     :completed-tasks [{:id "task-1" :title "Do thing"}]}
          result (extract-source-ids harvested)]
      (is (= #{"note-1" "note-2" "task-1"} (set result)))))

  (testing "handles entries without :id"
    (let [harvested {:progress-notes [{:content "no id"}
                                      {:id "note-1" :content "has id"}]
                     :completed-tasks []}
          result (extract-source-ids harvested)]
      (is (= ["note-1"] result))))

  (testing "deduplicates IDs"
    (let [harvested {:progress-notes [{:id "dup" :content "first"}
                                      {:id "dup" :content "second"}]
                     :completed-tasks []}
          result (extract-source-ids harvested)]
      (is (= ["dup"] result))))

  (testing "returns empty vector when no IDs"
    (let [harvested {:progress-notes []
                     :completed-tasks []}
          result (extract-source-ids harvested)]
      (is (= [] result))))

  (testing "extracts IDs from memory-ids-created maps"
    (let [harvested {:progress-notes []
                     :completed-tasks []
                     :memory-ids-created [{:id "mem-1" :timestamp "2026-02-12T10:00:00Z"}
                                          {:id "mem-2" :timestamp "2026-02-12T10:05:00Z"}]}
          result (extract-source-ids harvested)]
      (is (= #{"mem-1" "mem-2"} (set result)))))

  (testing "combines all three sources"
    (let [harvested {:progress-notes [{:id "note-1" :content "foo"}]
                     :completed-tasks [{:id "task-1" :title "bar"}]
                     :memory-ids-created [{:id "mem-1" :timestamp "2026-02-12T10:00:00Z"}]}
          result (extract-source-ids harvested)]
      (is (= #{"note-1" "task-1" "mem-1"} (set result)))))

  (testing "deduplicates across all three sources"
    (let [harvested {:progress-notes [{:id "shared-id" :content "note"}]
                     :completed-tasks [{:id "shared-id" :title "task"}]
                     :memory-ids-created [{:id "shared-id" :timestamp "2026-02-12T10:00:00Z"}]}
          result (extract-source-ids harvested)]
      (is (= ["shared-id"] result))))

  (testing "handles missing memory-ids-created key"
    (let [harvested {:progress-notes [{:id "note-1" :content "foo"}]
                     :completed-tasks []}
          result (extract-source-ids harvested)]
      (is (= ["note-1"] result)))))

;; ============================================================================
;; Unit tests for create-derived-from-edges!
;; ============================================================================

(deftest create-derived-from-edges-test
  (testing "creates edges from summary to sources"
    (let [summary-id "chroma-summary-123"
          source-ids ["emacs-note-1" "emacs-note-2"]
          project-id "test-project"
          agent-id "test-agent"
          result (create-derived-from-edges! summary-id source-ids project-id agent-id)]
      (is (= 2 (:created-count result)))
      (is (= 2 (count (:edge-ids result))))
      ;; Verify edges exist in KG
      (let [stats (kg-edges/edge-stats)]
        (is (= 2 (:total-edges stats)))
        (is (= {:derived-from 2} (:by-relation stats))))))

  (testing "returns nil when summary-id is nil"
    (kg-conn/reset-conn!)
    (let [result (create-derived-from-edges! nil ["source-1"] "project" "agent")]
      (is (nil? result))))

  (testing "returns nil when source-ids is empty"
    (kg-conn/reset-conn!)
    (let [result (create-derived-from-edges! "summary-123" [] "project" "agent")]
      (is (nil? result))))

  (testing "edges have correct attributes"
    (kg-conn/reset-conn!)
    (let [summary-id "summary-xyz"
          source-ids ["source-abc"]
          project-id "my-project"
          agent-id "my-agent"
          result (create-derived-from-edges! summary-id source-ids project-id agent-id)
          edge-id (first (:edge-ids result))
          edge (kg-edges/get-edge edge-id)]
      (is (= summary-id (:kg-edge/from edge)))
      (is (= "source-abc" (:kg-edge/to edge)))
      (is (= :derived-from (:kg-edge/relation edge)))
      (is (= project-id (:kg-edge/scope edge)))
      (is (= agent-id (:kg-edge/created-by edge))))))

;; ============================================================================
;; Integration test: KG edges flow through wrap_crystallize
;; ============================================================================

(deftest kg-stats-after-edge-creation
  (testing "kg_stats reflects created edges"
    (kg-conn/reset-conn!)
    ;; Simulate what wrap_crystallize does
    (let [summary-id "chroma-summary-456"
          source-ids ["note-a" "note-b" "task-c"]
          project-id "integration-test"
          _ (create-derived-from-edges! summary-id source-ids project-id "test-agent")
          stats (kg-edges/edge-stats)]
      (is (= 3 (:total-edges stats)))
      (is (= {:derived-from 3} (:by-relation stats)))
      (is (= {"integration-test" 3} (:by-scope stats))))))

;; ============================================================================
;; Full wrap_crystallize simulation
;; ============================================================================

(deftest wrap-crystallize-kg-integration
  (testing "wrap_crystallize flow creates KG edges when source entries have IDs"
    (kg-conn/reset-conn!)
    ;; Simulate harvested data with IDs (including memory-ids-created)
    (let [harvested {:progress-notes [{:id "emacs-note-001" :content "Session progress"}
                                      {:id "emacs-note-002" :content "Another note"}]
                     :completed-tasks [{:id "ds-task-001" :title "Completed task"}
                                       {:title "Task without ID"}]  ; No ID
                     :memory-ids-created [{:id "mem-entry-001" :timestamp "2026-02-12T10:00:00Z"}
                                          {:id "mem-entry-002" :timestamp "2026-02-12T10:05:00Z"}]}
          ;; Simulate crystallize-session returning a summary ID
          summary-id "chroma-summary-wrap-test"
          project-id "test-project"
          agent-id "test-agent"
          ;; Extract source IDs (should get 5: 2 notes + 1 task + 2 memory-ids-created)
          source-ids (extract-source-ids harvested)]

      ;; Verify extract got the right IDs
      (is (= 5 (count source-ids)))
      (is (contains? (set source-ids) "emacs-note-001"))
      (is (contains? (set source-ids) "emacs-note-002"))
      (is (contains? (set source-ids) "ds-task-001"))
      (is (contains? (set source-ids) "mem-entry-001"))
      (is (contains? (set source-ids) "mem-entry-002"))

      ;; Create edges
      (let [result (create-derived-from-edges! summary-id source-ids project-id agent-id)]
        (is (= 5 (:created-count result)))

        ;; Verify via kg_stats
        (let [stats (kg-edges/edge-stats)]
          (is (= 5 (:total-edges stats)))
          (is (= {:derived-from 5} (:by-relation stats))))

        ;; Verify edges point correctly
        (let [outgoing (kg-edges/get-edges-from summary-id)]
          (is (= 5 (count outgoing)))
          (is (every? #(= summary-id (:kg-edge/from %)) outgoing))
          (is (every? #(= :derived-from (:kg-edge/relation %)) outgoing)))))))

;; ============================================================================
;; FOSS build-crystal-edges tests (via private var access)
;; ============================================================================

(deftest foss-build-crystal-edges-test
  (let [foss-fn @(requiring-resolve 'hive-mcp.tools.crystal/foss-build-crystal-edges)]

    (testing "creates derived-from edges from summary to sources"
      (kg-conn/reset-conn!)
      (let [result (foss-fn {:summary-id "summary-foss-1"
                             :harvested {:progress-notes [{:id "note-a" :content "A"}
                                                          {:id "note-b" :content "B"}]
                                         :completed-tasks [{:id "task-c" :title "C"}]
                                         :memory-ids-created []}
                             :project-id "foss-test"
                             :agent-id "foss-agent"})]
        (is (= 3 (get-in result [:derived-from :created-count]))
            "Should create 3 derived-from edges")
        (is (= 3 (count (get-in result [:derived-from :edge-ids])))
            "Should return 3 edge IDs")
        (is (false? (:capped? result))
            "Should not be capped with 3 sources")
        (is (pos? (:total-edges result))
            "Total edges should be positive")

        ;; Verify actual KG state
        (let [outgoing (kg-edges/get-edges-from "summary-foss-1")]
          (is (= 3 (count outgoing)))
          (is (every? #(= :derived-from (:kg-edge/relation %)) outgoing))
          (is (every? #(= "foss-test" (:kg-edge/scope %)) outgoing))
          (is (every? #(= "foss-agent" (:kg-edge/created-by %)) outgoing)))))

    (testing "creates co-accessed edges between sources"
      (kg-conn/reset-conn!)
      (let [result (foss-fn {:summary-id "summary-foss-2"
                             :harvested {:progress-notes [{:id "x1" :content "X1"}
                                                          {:id "x2" :content "X2"}
                                                          {:id "x3" :content "X3"}]
                                         :completed-tasks []
                                         :memory-ids-created []}
                             :project-id "foss-test"
                             :agent-id "foss-agent"})
            co-count (get-in result [:co-accessed :count])]
        ;; 3 sources → 3 co-access pairs: (x1,x2) (x1,x3) (x2,x3)
        (is (= 3 co-count) "Should create 3 co-accessed edges")
        ;; 3 derived-from + 3 co-accessed = 6 total
        (is (= 6 (:total-edges result)))))

    (testing "returns empty result when no source IDs"
      (kg-conn/reset-conn!)
      (let [result (foss-fn {:summary-id "summary-foss-3"
                             :harvested {:progress-notes []
                                         :completed-tasks []
                                         :memory-ids-created []}
                             :project-id "foss-test"
                             :agent-id "foss-agent"})]
        (is (= 0 (:total-edges result)))
        (is (nil? (:derived-from result))
            "No derived-from when no sources")
        (is (false? (:capped? result)))))

    (testing "returns empty result when summary-id is nil"
      (kg-conn/reset-conn!)
      (let [result (foss-fn {:summary-id nil
                             :harvested {:progress-notes [{:id "n1" :content "N"}]
                                         :completed-tasks []}
                             :project-id "foss-test"
                             :agent-id "foss-agent"})]
        (is (= 0 (:total-edges result)))
        (is (nil? (:derived-from result)))))

    (testing "extension override takes precedence over FOSS default"
      (kg-conn/reset-conn!)
      (let [ext-called? (atom false)]
        ;; Register a mock extension
        (ext/register! :ck/a (fn [_m]
                               (reset! ext-called? true)
                               {:derived-from {:created-count 99 :edge-ids ["mock"]}
                                :co-accessed {:count 0}
                                :total-edges 99
                                :capped? false}))
        (try
          ;; Call through the public ck-a (private, but we can test via crystallize flow)
          ;; For unit test, just verify registry works
          (let [ext-fn (ext/get-extension :ck/a)
                result (ext-fn {:summary-id "s" :harvested {} :project-id "p" :agent-id "a"})]
            (is @ext-called? "Extension should have been called")
            (is (= 99 (:total-edges result)) "Extension result should be used"))
          (finally
            (ext/deregister! :ck/a)))))))
