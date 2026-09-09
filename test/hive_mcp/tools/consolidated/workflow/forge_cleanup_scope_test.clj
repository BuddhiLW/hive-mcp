(ns hive-mcp.tools.consolidated.workflow.forge-cleanup-scope-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as ops]
            [hive-mcp.tools.consolidated.workflow.forge-cycle :as cycle]))

(defn slave-row [id task project status depth]
  {:slave/id id :slave/kanban-task-id task :slave/project-id project
   :slave/status status :slave/depth depth})

(def candidates
  [(slave-row "owned" "a" "hive" :completed 1)
   (slave-row "other-task" "b" "hive" :completed 1)
   (slave-row "other-project" "a" "elsewhere" :completed 1)
   (slave-row "unknown-project" "a" nil :completed 1)
   (slave-row "unknown-task" nil "hive" :completed 1)
   (slave-row "active" "a" "hive" :working 1)
   (slave-row "nested" "a" "hive" :completed 2)])

(defn run-cleanup [opts]
  (let [killed (atom [])
        result (ops/smite! opts
                 {:agents-fn (constantly candidates)
                  :project-id-fn (constantly "hive")
                  :plan-ids-fn (fn [id]
                                 (if (= "plan" id) #{"a" "b"}
                                     (throw (ex-info "Unknown plan" {}))))
                  :kill-fn (fn [a]
                             (swap! killed conj (:slave/id a))
                             {:id (:slave/id a) :killed true})})]
    {:ids @killed :result result}))

(deftest scoped-cleanup-requires-project-and-task-ownership
  (is (= ["owned"] (:ids (run-cleanup {:directory "/hive" :task_ids ["a"]}))))
  (is (= ["owned" "other-task"]
         (:ids (run-cleanup {:directory "/hive" :plan_id "plan"})))))

(deftest whitelist-intersects-plan-and-empty-means-empty
  (doseq [ids [[] ["outside"]]]
    (is (empty? (:ids (run-cleanup {:directory "/hive" :plan_id "plan" :task_ids ids})))))
  (is (empty? (:ids (run-cleanup {:directory "/hive" :task_ids []})))))

(deftest invalid-scope-never-kills
  (doseq [opts [{:directory "/hive" :task_ids nil}
               {:directory "/hive" :task_ids "a"}
               {:directory "/hive" :task_ids [""]}
               {:directory "/hive" :plan_id "missing"}
               {:task_ids ["a"]}]]
    (is (thrown? clojure.lang.ExceptionInfo (run-cleanup opts)))))

(deftest unscoped-cleanup-retains-project-compatibility
  (is (= ["owned" "other-task" "unknown-project" "unknown-task"]
         (:ids (run-cleanup {:directory "/hive"})))))

(deftest fsm-cleanup-forwards-explicit-scope
  (let [resources (cycle/build-fsm-resources {:task_ids nil})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                         #"explicit project directory"
                         ((get-in resources [:agent-ops :kill-fn]) nil nil)))))
