(ns hive-mcp.tools.kanban.list.source-conformance-test
  "One conformance suite, every IBoardSource on the classpath.

   The in-memory SeqBoardSource and the facade-backed FacadeBoardSource
   (over an ephemeral stub IMemoryStore) answer the same cases. The board
   is deliberately larger than every window the old code hard-coded (100,
   500) so a provider that silently truncates fails here, not in a session."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.kanban.list.plan :as plan]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.memory-kanban.query :as query]))

(defn- card [i status pid]
  (let [prio (nth ["high" "medium" "low"] (mod i 3))]
    {:id         (format "20260907%06d-%08x" i i)
     :type       "note"
     :project-id pid
     :tags       ["kanban" status (str "priority-" prio) (str "scope:project:" pid)]
     :content    {:task-type "kanban" :title (str "card " i)
                  :status status :priority prio}}))

(def ^:private board
  (vec (concat (map #(card % "todo" "p") (range 650))
               (map #(card % "done" "p") (range 650 900))
               (map #(card % "todo" "q") (range 900 960)))))

(def ^:private todo-count 710)
(def ^:private done-count 250)

(use-fixtures :each stub/with-stub-store)

(defn- seed-store! []
  (stub/seed! (proto/get-store) board))

(defn- providers []
  [{:name "seq"    :source (src/->seq-source board)}
   {:name "facade" :source (query/->FacadeBoardSource)}])

(def ^:private all-scopes {:project-id "p" :include-descendants? true :scope "all"})

(deftest every-source-answers-the-whole-board
  (seed-store!)
  (doseq [{:keys [name source]} (providers)]
    (testing (str name ": a status pushdown returns every matching card, beyond the old 500 window")
      (let [{:keys [entries multi-project?]}
            (src/scoped-board source all-scopes (plan/plan {:status "todo"}))]
        (is (= todo-count (count entries)))
        (is (every? #(contains? (set (:tags %)) "todo") entries))
        (is (true? multi-project?))))
    (testing (str name ": done cards are not lost behind the active window")
      (is (= done-count
             (count (:entries (src/scoped-board source all-scopes (plan/plan {:status "done"})))))))
    (testing (str name ": no status returns the whole scoped board")
      (is (= (count board)
             (count (:entries (src/scoped-board source all-scopes (plan/plan {})))))))
    (testing (str name ": the plan's window bounds the fetch")
      (is (>= 10
              (count (:entries (src/scoped-board source all-scopes
                                                 (assoc (plan/plan {}) :window 10)))))))))

(deftest providers-are-not-vacuous
  (is (= 2 (count (providers))))
  (is (pos? todo-count)))
