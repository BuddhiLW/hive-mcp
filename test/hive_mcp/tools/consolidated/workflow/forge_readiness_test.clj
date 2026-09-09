(ns hive-mcp.tools.consolidated.workflow.forge-readiness-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [hive-mcp.scheduler.vulcan :as vulcan]
            [hive-mcp.vectordb.kanban-facade :as kanban-store]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.tools.consolidated.kanban :as kanban]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as forge]
            [hive-mcp.tools.consolidated.workflow.forge-cycle :as cycle]
            [hive-mcp.config.core :as config]
            [hive-mcp.workflows.forge-belt :as belt]))

(def plan {:id "p" :title "Diamond"
           :steps [{:id "a" :title "A"} {:id "b" :title "B"}
                   {:id "c" :title "C" :depends-on ["a" "b"]}]})

(def graph {"p" #{"ka" "kb" "kc"} "ka" #{} "kb" #{} "kc" #{"ka" "kb"}})

(defn card [id title status]
  {:id id :content (json/write-str {:task-type "kanban" :title title :status status})})

(def cards {"ka" (card "ka" "A" "todo") "kb" (card "kb" "B" "todo")
            "kc" (card "kc" "C" "todo")})

(def ports {:plan-entry-fn (constantly {:type "plan" :content (pr-str plan)})
            :task-entry-fn cards :deps-fn graph})

(deftest stored-done-releases-diamond
  (let [db (atom cards)
        tasks [{:id "ka"} {:id "kb"} {:id "kc"}]
        select (fn [todo] (vulcan/prioritize-tasks todo #{} {:deps-fn graph}))]
    (with-redefs [kanban-store/get-entry-by-id #(get @db %)]
      (is (= ["ka" "kb"] (mapv :id (:tasks (select tasks)))))
      (swap! db assoc "ka" (card "ka" "A" "done"))
      (is (= :open (get-in (select [{:id "kc"}]) [:blocked 0 :dependencies "kb"])))
      (swap! db assoc "kb" (card "kb" "B" "done"))
      (is (= ["kc"] (mapv :id (:tasks (select [{:id "kc"}])))))
      (is (= 3 (count @db)) "Done predecessors remain stored"))))

(deftest uncertain-dependencies-never-release
  (doseq [[entry expected] [[nil :missing]
                           [{:content "garbage"} :invalid]
                           [(card "a" "A" "review") :open]
                           [{:content {:task-type "kanban" :status :done}} :done]]]
    (is (= expected (vulcan/entry-state entry))))
  (doseq [[lookup expected] [[(constantly nil) :missing]
                            [(constantly false) :missing]
                            [(constantly :open) :open]
                            [(fn [_] (throw (ex-info "offline" {}))) :lookup-error]]]
    (let [result (vulcan/prioritize-tasks [{:id "b"}] #{}
                                        {:deps-fn (constantly #{"a"}) :state-fn lookup})]
      (is (empty? (:tasks result)))
      (is (= expected (get-in result [:blocked 0 :dependencies "a"])))))
  (with-redefs [edges/get-edges-from (fn [_] (throw (ex-info "KG offline" {})))]
    (let [result (vulcan/prioritize-tasks [{:id "b"}])]
      (is (empty? (:tasks result)))
      (is (= :lookup-error (get-in result [:blocked 0 :state])))))
  (with-redefs [kanban-store/get-entry-by-id (fn [_] (throw (ex-info "store offline" {})))]
    (is (= :lookup-error (vulcan/task-state "a")))))

(deftest plan-conversion-must-be-complete
  (is (= #{"ka" "kb" "kc"} (forge/plan-task-ids "p" ports)))
  (doseq [broken [(assoc ports :plan-entry-fn (constantly nil))
                  (assoc ports :plan-entry-fn (constantly {:type "plan" :content "broken"}))
                  (assoc ports :deps-fn (assoc graph "p" #{"ka"}))
                  (assoc ports :deps-fn (assoc graph "kc" #{"ka"}))
                  (assoc ports :task-entry-fn (dissoc cards "ka"))
                  (assoc ports :deps-fn (fn [_] (throw (ex-info "offline" {}))))]]
    (is (thrown? clojure.lang.ExceptionInfo (forge/plan-task-ids "p" broken))))
  (is (thrown? clojure.lang.ExceptionInfo (forge/plan-task-ids "" ports))))

(deftest plan-survey-intersects-whitelist-and-never-sweeps
  (let [list-calls (atom 0)
        opts (merge ports {:directory "/test" :plan_id "p" :state-fn (constantly :open)})]
    (with-redefs [config/get-service-value (fn [& _] false)
                  kanban/handle-kanban (fn [_]
                                         (swap! list-calls inc)
                                         {:text (json/write-str
                                                 [{:id "ka" :title "A"}
                                                  {:id "kb" :title "B"}
                                                  {:id "kc" :title "C"}
                                                  {:id "unrelated" :title "Elsewhere"}])})]
      (is (= ["ka" "kb"] (mapv :id (:tasks (forge/survey opts)))))
      (is (= ["kb"] (mapv :id (:tasks (forge/survey (assoc opts :task_ids ["kb" "unrelated"]))))))
      (is (empty? (:tasks (forge/survey (assoc opts :task_ids [])))))
      (is (empty? (:tasks (forge/survey (assoc opts :task_filter "Elsewhere")))))
      (let [before @list-calls]
        (doseq [bad ["" nil "missing"]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (forge/survey (assoc opts :plan_id bad :plan-entry-fn (constantly nil))))))
        (is (= before @list-calls) "Invalid plans never query the backlog"))
      (let [resources (cycle/build-fsm-resources opts)]
        (is (= ["ka" "kb"] (mapv :id (:tasks ((get-in resources [:kanban-ops :list-fn]) "/test")))))))))

(deftest plan-survey-reports-kanban-failure
  (with-redefs [kanban/handle-kanban (constantly {:isError true :text "offline"})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (forge/survey (assoc ports :plan_id "p" :directory "/test"))))))

(deftest strike-preserves-blocked-and-completion-evidence
  (doseq [[statuses todo expected completed]
          [[["todo" "todo" "todo"] ["ka" "kb" "kc"] :ready 0]
           [["doing" "done" "todo"] ["kc"] :blocked 1]
           [["done" "done" "doing"] [] :no-ready 2]
           [["done" "done" "done"] [] :complete 3]]]
    (let [db (into {} (map (fn [id title status] [id (card id title status)])
                           ["ka" "kb" "kc"] ["A" "B" "C"] statuses))
          opts (assoc ports :plan_id "p" :directory "/test"
                      :task-entry-fn db
                      :state-fn #(vulcan/entry-state (get db %)))
          forge-state (atom {:total-smited 0 :total-sparked 0 :total-strikes 0})]
      (with-redefs [config/get-service-value (fn [& _] false)
                    kanban/handle-kanban (constantly {:text (json/write-str (mapv #(hash-map :id %) todo))})
                    belt/run-single-strike
                    (fn [resources]
                      {:survey-result ((get-in resources [:kanban-ops :list-fn]) "/test")
                       :total-smited 0 :total-sparked 0 :success false})]
        (let [result (:ok (cycle/fsm-forge-strike* opts forge-state))]
          (is (= expected (:selection-status result)))
          (is (= (= :complete expected) (:plan-complete? result)))
          (is (= completed (get-in result [:survey :plan-completed-count])))
          (when (= :blocked expected)
            (is (= :blocked (:outcome result)))
            (is (false? (:success result)))
            (is (= [{"ka" :open}] (mapv :dependencies (get-in result [:survey :blocked])))))
          (when (= :no-ready expected)
            (is (= :no-ready (:outcome result)))
            (is (false? (:success result))))
          (when (= :complete expected)
            (is (= :complete (:outcome result)))
            (is (true? (:success result)))))))))
