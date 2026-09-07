(ns hive-mcp.server.routes-contributed-params-test
  "A consolidated tool's ADVERTISED schema folds the params its addon
   contributions declare (server.routes/make-tool -> composite/build-merged-tool).

   Routing folded contributions in long before the schema did. Measured live
   2026-09-05: `swarm ling-wave dispatch` reached hive-agent's handler, but the
   swarm tool's inputSchema had no `providers` slot, the MCP layer dropped the
   argument, and every spelling of the call answered :wave/no-providers."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.routes :as routes]))

(def ^:private core
  {:name "contrib-params-root" :consolidated true :description "d"
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"}}
                 :required ["command"]}
   :handler (fn [_] {:type "text" :text "ok"})})

(deftest make-tool-advertises-contributed-params
  (ext/contribute-commands! "contrib-params-root" :contrib-params-test
                            {"ling-wave" {:handler (fn [_] nil)
                                          :params  {"providers" {:type "array"}}}})
  (try
    (let [t (routes/make-tool core)]
      (testing "the contributed verb's own argument has a slot on the root's schema"
        (is (contains? (get-in t [:inputSchema :properties]) "providers")))
      (testing "the core's own params survive the fold"
        (is (contains? (get-in t [:inputSchema :properties]) "command"))))
    (finally (ext/retract-commands! "contrib-params-root" :contrib-params-test))))

(deftest make-tool-leaves-a-non-consolidated-tool-alone
  (let [t (routes/make-tool (dissoc core :consolidated))]
    (is (= {"command" {:type "string"}} (get-in t [:inputSchema :properties])))))
