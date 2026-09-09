(ns hive-mcp.agent.routing-reload-test
  "Real-loader regression. Run only in an isolated JVM: resets hive-hot tracking."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-hot.core :as hot]))

(deftest ^:integration first-def-to-defonce-transition-preserves-runtime-routing
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "routing-reload-" (make-array java.nio.file.attribute.FileAttribute 0)))
        fixture-ns 'routing-reload-fixture.routing
        file (io/file root "routing_reload_fixture/routing.clj")
        source (slurp (io/file "src/hive_mcp/agent/routing.clj"))
        new-source (str/replace-first source
                                     "(ns hive-mcp.agent.routing"
                                     "(ns routing-reload-fixture.routing")
        old-source (str/replace new-source "(defonce ^{" "(def ^{")
        lookup (fn [sym] (var-get (ns-resolve fixture-ns sym)))]
    (try
      (.mkdirs (.getParentFile file))
      (clojure.lang.RT/addURL (.toURL (.toURI root)))
      (spit file old-source)
      (load-file (.getPath file))
      (hot/init! {:dirs [(.getPath root)] :since (System/currentTimeMillis)})
      (let [routes (lookup 'model-routes)
            proxy (lookup 'tool-proxy-config)
            route {:primary "reload-sentinel" :secondary "reload-fallback"}
            captured-route (lookup 'get-route)
            captured-proxy (lookup 'get-tool-proxy-config)]
        ((lookup 'set-route!) :reload-probe route)
        ((lookup 'set-tool-proxy-config!) {:enabled false :max-iterations 37})
        (let [routes-before @routes
              proxy-before @proxy]
          (spit file new-source)
          (.setLastModified file (+ 2000 (System/currentTimeMillis)))
          (let [report (hot/reload-scoped! [(.getPath file)])]
            (is (:success report) (pr-str (dissoc report :exception)))
            (is (= [fixture-ns] (:loaded report))))
          (testing "first transition preserves atom identity and custom values"
            (is (identical? routes (lookup 'model-routes)))
            (is (identical? proxy (lookup 'tool-proxy-config)))
            (is (= routes-before @(lookup 'model-routes)))
            (is (= proxy-before @(lookup 'tool-proxy-config))))
          (testing "captured callers and newly resolved setters share preserved state"
            (is (= route (captured-route :reload-probe)))
            (is (= proxy-before (captured-proxy)))
            ((lookup 'set-route!) :reload-probe {:primary "after-reload"})
            (is (= {:primary "after-reload"} (captured-route :reload-probe)))
            ((lookup 'set-tool-proxy-config!) {:max-iterations 41})
            (is (= 41 (:max-iterations (captured-proxy)))))))
      (finally
        (remove-ns fixture-ns)
        (doseq [f (reverse (file-seq root))] (.delete f))))))

(deftest explicit-refresh-replaces-captured-tool-wrappers
  (require 'hive-mcp.server.routes 'hive-mcp.extensions.registry)
  (let [refresh! (requiring-resolve 'hive-mcp.server.routes/refresh-tools!)
        register! (requiring-resolve 'hive-mcp.extensions.registry/register-tool!)
        deregister! (requiring-resolve 'hive-mcp.extensions.registry/deregister-tool!)
        tools (atom {})
        context (atom {:tools tools})
        name "routing_reload_probe"
        make-definition (fn [version]
                          {:name name :description version
                           :inputSchema {:type "object" :properties {}}
                           :handler (fn [_] {:content [{:type "text" :text version}]})})]
    (try
      (register! (make-definition "before"))
      (refresh! context)
      (let [captured (get @tools name)]
        (is (some? captured))
        (register! (make-definition "after"))
        (testing "already captured wrappers remain stale until explicit refresh"
          (is (identical? captured (get @tools name))))
        (refresh! context)
        (is (identical? tools (:tools @context)))
        (is (= "after" (get-in @tools [name :tool :description])))
        (is (not (identical? (:handler captured) (get-in @tools [name :handler])))))
      (finally (deregister! name)))))
