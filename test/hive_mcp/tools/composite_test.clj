(ns hive-mcp.tools.composite-test
  "lazy-resolve-schema-props resolves every shape a subdomain advertises its
   params in, and the swarm root folds every subdomain's params through it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.tools.consolidated.swarm :as swarm]))

(def tool-def-map
  {:inputSchema {:properties {"from-map" {:type "string"}}}})

(def tools-vec
  [{:inputSchema {:properties {"from-vec" {:type "string"}}}}
   {:inputSchema {:properties {"second-entry" {:type "string"}}}}])

(defn tool-defs-fn []
  [{:inputSchema {:properties {"from-fn" {:type "string"}}}}])

(deftest lazy-resolve-schema-props-shapes-test
  (testing "a `tool-def` map"
    (is (= {"from-map" {:type "string"}}
           (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/tool-def-map))))
  (testing "a `tools` vector — the first entry is the root tool"
    (is (= {"from-vec" {:type "string"}}
           (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/tools-vec))))
  (testing "a 0-arity `tool-defs` fn"
    (is (= {"from-fn" {:type "string"}}
           (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/tool-defs-fn))))
  (testing "an unresolvable symbol contributes nothing rather than failing"
    (is (= {} (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/no-such-var)))
    (is (= {} (composite/lazy-resolve-schema-props 'no.such.ns/tools)))))

(def swarm-subdomain-tools
  '[hive-mcp.tools.consolidated.agent/tools
    hive-mcp.tools.consolidated.wave/tools
    hive-mcp.tools.consolidated.hivemind/tools
    hive-mcp.tools.consolidated.agora/tools
    hive-mcp.tools.consolidated.olympus/tools
    hive-mcp.tools.consolidated.preset/tools])

(deftest swarm-root-folds-every-subdomain-param-test
  (let [root-props (set (keys (get-in swarm/tool-def [:inputSchema :properties])))]
    (doseq [sym swarm-subdomain-tools]
      (let [sub-props (set (keys (composite/lazy-resolve-schema-props sym)))]
        (testing (str sym " advertises params")
          (is (seq sub-props)))
        (testing (str "every param of " sym " survives the swarm fold")
          (is (empty? (set/difference sub-props root-props))))))))
