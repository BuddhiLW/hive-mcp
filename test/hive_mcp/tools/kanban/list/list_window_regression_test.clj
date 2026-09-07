(ns hive-mcp.tools.kanban.list.list-window-regression-test
  "Regression for the `kanban list` window bug (card 20260708131354-08595112):
   the caller's limit never reached the store, so `status=todo` answered 500
   of 722 (measured live 2026-09-07) and no limit could recover the rest.

   The board is injected through the call-time `*board-source*` seam, so
   these run with no store at all; the handler-level case proves the MCP
   handler resolves the seam per call rather than through a frozen alias."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.memory-kanban :as mk]
            [hive-mcp.tools.memory-kanban.query :as query]))

(defn- card [i status]
  (let [prio (nth ["high" "medium" "low"] (mod i 3))]
    {:id         (format "20260907%06d-%08x" i i)
     :type       "note"
     :project-id "p"
     :tags       ["kanban" status (str "priority-" prio) "scope:project:p"]
     :content    {:task-type "kanban" :title (str "card " i)
                  :status status :priority prio}}))

(def ^:private board
  (vec (concat (map #(card % "todo") (range 722))
               (map #(card % "done") (range 722 1000)))))

(defn- rows [result]
  (json/read-str (:text result)))

(def ^:private base {:project_id "p" :scope "all"})

(deftest status-list-returns-every-card
  (binding [query/*board-source* (src/->seq-source board)]
    (testing "status=todo answers all 722, not a window"
      (is (= 722 (count (rows (query/list-slim* (assoc base :status "todo")))))))
    (testing "an explicit limit above the board is honoured as 'all'"
      (is (= 1000 (count (rows (query/list-slim* (assoc base :limit 5000)))))))
    (testing "a bare list keeps the token-budget cap"
      (is (= 100 (count (rows (query/list-slim* base))))))
    (testing "offset past the old window still pages"
      (is (= 22 (count (rows (query/list-slim* (assoc base :status "todo" :offset 700 :limit 50)))))))
    (testing "same-second ties are ordered by id, deterministically"
      (let [ids (map #(get % "id") (rows (query/list-slim* (assoc base :status "todo" :priority "high"))))]
        (is (= ids (sort ids)))))))

(use-fixtures :each stub/with-stub-store)

(deftest handler-resolves-the-seam-per-call
  (binding [query/*board-source* (src/->seq-source board)]
    (is (= 722 (count (rows (mk/handle-mem-kanban-list-slim (assoc base :status "todo"))))))))
