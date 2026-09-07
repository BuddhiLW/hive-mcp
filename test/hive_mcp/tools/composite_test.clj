(ns hive-mcp.tools.composite-test
  "lazy-resolve-schema-props resolves every shape a subdomain advertises its
   params in, and the swarm root folds every subdomain's params through it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.tools.consolidated.swarm :as swarm]
            [hive-mcp.extensions.registry :as ext]))

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

;; =============================================================================
;; build-merged-tool — a contribution's :params reach the advertised schema.
;;
;; Measured live 2026-09-05: `swarm ling-wave dispatch` routed, but the swarm
;; tool's schema had no `providers` slot, the MCP layer dropped the argument,
;; and dispatch answered :wave/no-providers to every spelling of the call.
;; =============================================================================

(def ^:private merged-core
  {:name "merged-test-root" :consolidated true
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"}
                              "tasks"   {:type "array" :items {:type "object"}
                                         :description "drone tasks"}}}})

(defn- with-contribution [params f]
  (ext/contribute-commands! "merged-test-root" :merged-test-addon
                            {"ling-wave" {:handler (fn [_] nil) :params params}})
  (try (f)
       (finally (ext/retract-commands! "merged-test-root" :merged-test-addon))))

(deftest build-merged-tool-folds-contributed-params-test
  (with-contribution {"providers" {:type "array" :description "members"}
                      "tasks"     {:type "array" :items {:type "string"}
                                   :description "ling tasks"}}
    (fn []
      (let [props (get-in (composite/build-merged-tool merged-core)
                          [:inputSchema :properties])]
        (testing "a param the core never declared is now advertised"
          (is (= {:type "array" :description "members"} (get props "providers"))))
        (testing "a colliding param unions both shapes instead of retyping the core's"
          (is (= 2 (count (get-in props ["tasks" :anyOf]))))
          (is (= "drone tasks | ling tasks" (get-in props ["tasks" :description]))))
        (testing "a free-text `command` does not acquire an enum of addon names"
          (is (nil? (get-in props ["command" :enum]))))))))

(deftest build-merged-tool-extends-an-existing-command-enum-test
  (with-contribution {}
    (fn []
      (let [core (assoc-in merged-core [:inputSchema :properties "command" :enum] ["a" "b"])
            t    (composite/build-merged-tool core)]
        (is (= ["a" "b" "ling-wave"] (get-in t [:inputSchema :properties "command" :enum])))
        (is (:composite t))))))

(deftest build-merged-tool-is-identity-without-contributions-test
  (is (= merged-core (composite/build-merged-tool merged-core))))
