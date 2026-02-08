(ns hive-mcp.workflows.catchup-ling-test
  "Tests for lightweight ling-specific catchup.

   Uses with-redefs to mock Chroma, context-store, KG, and git subsystems.
   Follows catchup_session_test.clj and reconstruction_test.clj patterns."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.catchup-ling :as ling-catchup]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.catchup.scope :as catchup-scope]
            [hive-mcp.tools.catchup.git :as catchup-git]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.context.reconstruction :as reconstruction]
            [clojure.string :as str]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def test-axioms
  [{:id "ax-1" :content "Never spawn drones from lings" :type "axiom"}
   {:id "ax-2" :content "Cap 5-6 lings per Emacs daemon" :type "axiom"}])

(def test-conventions
  [{:id "pc-1" :content "No manual git after catchup" :type "convention"
    :tags ["convention" "catchup-priority"]}
   {:id "pc-2" :content "TDD trust bridge" :type "convention"
    :tags ["convention" "catchup-priority"]}])

(def test-kanban-entry
  {:id "task-123" :content "Improve ling catchup: use KG + memory reconstruction"
   :type "note" :tags ["kanban" "todo" "priority-medium"]})

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn reset-store [f]
  (context-store/reset-all!)
  (f)
  (context-store/reset-all!))

(use-fixtures :each reset-store)

;; =============================================================================
;; fetch-ling-memory tests
;; =============================================================================

(deftest fetch-ling-memory-queries-axioms-and-conventions
  (testing "fetches axioms and priority conventions"
    (with-redefs [catchup-scope/query-axioms (fn [_] test-axioms)
                  catchup-scope/query-scoped-entries (fn [_ _ _ _] test-conventions)]
      (let [result (ling-catchup/fetch-ling-memory "hive-mcp")]
        (is (= test-axioms (:axioms result)))
        (is (= test-conventions (:conventions result)))))))

(deftest fetch-ling-memory-empty-on-nil-project
  (testing "handles nil project-id gracefully"
    (with-redefs [catchup-scope/query-axioms (fn [_] [])
                  catchup-scope/query-scoped-entries (fn [_ _ _ _] [])]
      (let [result (ling-catchup/fetch-ling-memory nil)]
        (is (= [] (:axioms result)))
        (is (= [] (:conventions result)))))))

(deftest fetch-ling-memory-handles-failures
  (testing "returns empty collections on query failure"
    (with-redefs [catchup-scope/query-axioms
                  (fn [_] (throw (Exception. "Chroma down")))]
      (let [result (ling-catchup/fetch-ling-memory "hive-mcp")]
        (is (= [] (:axioms result)))
        (is (= [] (:conventions result)))))))

;; =============================================================================
;; extract-kg-seeds tests
;; =============================================================================

(deftest extract-kg-seeds-returns-l1-ids
  (testing "extracts KG node IDs from task hints"
    (with-redefs [hints/generate-task-hints
                  (fn [opts]
                    (is (= "task-123" (:task-id opts)))
                    {:l1-ids ["node-A" "node-B"] :l2-queries [] :l3-seeds []})]
      (let [result (ling-catchup/extract-kg-seeds "task-123")]
        (is (= ["node-A" "node-B"] result))))))

(deftest extract-kg-seeds-nil-without-task-id
  (testing "returns nil when no kanban-task-id"
    (is (nil? (ling-catchup/extract-kg-seeds nil)))))

(deftest extract-kg-seeds-empty-on-no-edges
  (testing "returns empty vector when KG has no edges"
    (with-redefs [hints/generate-task-hints
                  (fn [_] {:l1-ids [] :l2-queries [] :l3-seeds []})]
      (is (= [] (ling-catchup/extract-kg-seeds "task-123"))))))

(deftest extract-kg-seeds-handles-failure
  (testing "returns empty vector on failure"
    (with-redefs [hints/generate-task-hints
                  (fn [_] (throw (Exception. "KG down")))]
      (is (= [] (ling-catchup/extract-kg-seeds "task-123"))))))

;; =============================================================================
;; ling-catchup integration tests
;; =============================================================================

(defn- make-default-redefs
  "Build default with-redefs bindings for ling-catchup integration tests."
  ([] (make-default-redefs {}))
  ([overrides]
   (merge
    {#'chroma/embedding-configured? (constantly true)
     #'scope/get-current-project-id (fn [_] "hive-mcp")
     #'catchup-scope/query-axioms (fn [_] test-axioms)
     #'catchup-scope/query-scoped-entries (fn [_ _ _ _] test-conventions)
     #'hints/generate-task-hints
     (fn [_] {:l1-ids ["node-A"] :l2-queries [] :l3-seeds []})
     #'reconstruction/reconstruct-context
     (fn [refs kg-ids scope]
       (str "## Reconstructed Context (KG-Compressed)\n"
            "Refs: " (count refs)
            ", KG seeds: " (count kg-ids)
            ", Scope: " scope "\n"))
     #'catchup-git/gather-git-info
     (fn [_] {:branch "main" :uncommitted false :last-commit "abc123 - test"})
     #'chroma/get-entry-by-id (fn [_] test-kanban-entry)}
    overrides)))

(deftest ling-catchup-happy-path
  (testing "produces compact context blob with reconstructed context + git + task"
    (let [redefs (make-default-redefs)]
      (with-redefs-fn redefs
        (fn []
          (let [result (ling-catchup/ling-catchup
                        {:directory "/test" :kanban-task-id "task-123"})]
            (is (string? result))
            (is (str/includes? result "## Reconstructed Context"))
            (is (str/includes? result "### Git Status"))
            (is (str/includes? result "main"))
            (is (str/includes? result "### Assigned Task"))
            (is (str/includes? result "Improve ling catchup"))))))))

(deftest ling-catchup-without-kanban-task
  (testing "works without kanban-task-id (project-level context only)"
    (let [redefs (make-default-redefs)]
      (with-redefs-fn redefs
        (fn []
          (let [result (ling-catchup/ling-catchup {:directory "/test"})]
            (is (string? result))
            (is (str/includes? result "## Reconstructed Context"))
            (is (str/includes? result "### Git Status"))
            ;; No task section without kanban-task-id
            (is (not (str/includes? result "### Assigned Task")))))))))

(deftest ling-catchup-chroma-not-configured
  (testing "returns nil when Chroma not configured"
    (with-redefs [chroma/embedding-configured? (constantly false)]
      (is (nil? (ling-catchup/ling-catchup {:directory "/test"}))))))

(deftest ling-catchup-graceful-degradation
  (testing "returns nil on scope resolution failure"
    (let [redefs (make-default-redefs
                  {#'scope/get-current-project-id
                   (fn [_] (throw (Exception. "scope boom")))})]
      (with-redefs-fn redefs
        (fn []
          (is (nil? (ling-catchup/ling-catchup {:directory "/test"}))))))))

(deftest ling-catchup-partial-degradation
  (testing "produces output even when git and task sections fail"
    (let [redefs (make-default-redefs
                  {#'catchup-git/gather-git-info
                   (fn [_] (throw (Exception. "git down")))
                   #'chroma/get-entry-by-id
                   (fn [_] (throw (Exception. "chroma down")))})]
      (with-redefs-fn redefs
        (fn []
          (let [result (ling-catchup/ling-catchup
                        {:directory "/test" :kanban-task-id "task-123"})]
            ;; Should still have reconstructed context
            (is (string? result))
            (is (str/includes? result "## Reconstructed Context"))
            ;; Git and task sections should be absent
            (is (not (str/includes? result "### Git Status")))
            (is (not (str/includes? result "### Assigned Task")))))))))

(deftest ling-catchup-reconstruction-receives-correct-args
  (testing "reconstruction pipeline receives ctx-refs and KG seeds"
    (let [captured (atom nil)
          redefs (make-default-redefs
                  {#'reconstruction/reconstruct-context
                   (fn [refs kg-ids scope]
                     (reset! captured {:refs refs :kg-ids kg-ids :scope scope})
                     "## Context")})]
      (with-redefs-fn redefs
        (fn []
          (ling-catchup/ling-catchup
           {:directory "/test" :kanban-task-id "task-123"})
          ;; Verify reconstruction received ctx-refs (not raw entries)
          (is (some? @captured))
          (is (map? (:refs @captured)))
          ;; Refs should have context-store IDs (strings), not raw entry data
          (when (:axioms (:refs @captured))
            (is (string? (:axioms (:refs @captured)))))
          ;; KG seeds should come from hints
          (is (= ["node-A"] (:kg-ids @captured)))
          (is (= "hive-mcp" (:scope @captured))))))))

(deftest ling-catchup-no-kg-seeds-without-task
  (testing "no KG seeds when no kanban-task-id provided"
    (let [captured (atom nil)
          redefs (make-default-redefs
                  {#'reconstruction/reconstruct-context
                   (fn [refs kg-ids scope]
                     (reset! captured {:kg-ids kg-ids})
                     "## Context")})]
      (with-redefs-fn redefs
        (fn []
          (ling-catchup/ling-catchup {:directory "/test"})
          (is (= [] (:kg-ids @captured))))))))
