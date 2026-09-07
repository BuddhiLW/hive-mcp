(ns hive-mcp.tools.agent.spawn-parent-test
  "The parent a spawn is attributed to is structural, not remembered.

   Plumbing half of the spawner-addressed piggyback: `:slave/parent` is set
   from the CALLING agent when the `parent` param is absent, so a grandchild's
   shout reaches the ling that spawned it (and not the coordinator) without any
   agent having to pass `parent` by hand. hive-mcp.channel.audience-test covers
   the routing half."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.channel.audience :as aud]
            [hive-mcp.server.guards :as guards]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]
            [hive-mcp.test.stub.terminal-addon :as term-stub]))

(defn- coordinator-process-fixture
  "Reset the logic db and run as the coordinator process: spawn guard open,
   no swarm addon, a stub :claude terminal registered."
  [f]
  (logic/reset-db!)
  (with-redefs [swarm-core/swarm-addon-available? (constantly false)
                guards/child-ling? (constantly false)]
    (try (term-stub/with-terminal f)
         (finally (logic/reset-db!)))))

(use-fixtures :each
  (iso/with-isolations :swarm-ds)
  coordinator-process-fixture)

(defn- spawn-ling!
  "Spawn a ling through the MCP handler; fail the test on an error response."
  [params]
  (let [result (spawn/handle-spawn (merge {:type "ling" :cwd "/tmp/project"} params))]
    (is (not (:isError result)) (str "spawn failed: " (:text result)))
    (json/read-str (:text result) :key-fn keyword)))

(deftest effective-parent-test
  (testing "an explicit parent wins over the caller"
    (is (= "ling-x" (spawn/effective-parent {:parent "ling-x" :_caller_id "ling-a"}))))
  (testing "a ling caller becomes the parent when none is given"
    (is (= "ling-a" (spawn/effective-parent {:_caller_id "ling-a"})))
    (is (= "ling-a" (spawn/effective-parent {:parent "" :_caller_id "ling-a"}))))
  (testing "a coordinator-lane caller leaves the spawn root-level"
    (is (nil? (spawn/effective-parent {:_caller_id "coordinator"})))
    (is (nil? (spawn/effective-parent {:_caller_id "coordinator-hive"}))))
  (testing "no caller and no parent is root-level"
    (is (nil? (spawn/effective-parent {})))
    (is (nil? (spawn/effective-parent {:_caller_id ""})))))

(deftest coordinator-spawn-stays-root-level-test
  (testing "a coordinator spawn persists no :slave/parent; its shouts reach the coordinator lane"
    (spawn-ling! {:name "ling-a" :_caller_id "coordinator-hive"})
    (let [row   (queries/get-slave "ling-a")
          shout {:agent-id "ling-a" :parent-id (:slave/parent row)}]
      (is (some? row))
      (is (nil? (:slave/parent row)))
      (is (aud/addressed-to? "coordinator-hive" shout))
      (is (not (aud/addressed-to? "ling-b" shout))))))

(deftest ling-spawned-child-is-parented-to-the-ling-test
  (testing "a ling spawning without `parent` is recorded as :slave/parent, so the grandchild's shout stops at the ling"
    (spawn-ling! {:name "ling-a"})
    (let [parsed (spawn-ling! {:name "grandchild" :_caller_id "ling-a"})
          row    (queries/get-slave "grandchild")
          shout  {:agent-id "grandchild" :parent-id (:slave/parent row)}]
      (is (= "ling-a" (:parent parsed)))
      (is (= "ling-a" (:slave/parent row)))
      (is (aud/addressed-to? "ling-a" shout))
      (is (not (aud/addressed-to? "coordinator-hive" shout))))))

(deftest explicit-parent-still-wins-test
  (testing "an explicit parent param is not overridden by the caller"
    (spawn-ling! {:name "ling-a"})
    (spawn-ling! {:name "ling-b"})
    (spawn-ling! {:name "grandchild" :parent "ling-b" :_caller_id "ling-a"})
    (is (= "ling-b" (:slave/parent (queries/get-slave "grandchild"))))))
