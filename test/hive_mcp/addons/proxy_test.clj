(ns hive-mcp.addons.proxy-test
  "Tests for hive-mcp.addons.proxy — auto-generated proxy tool-defs."
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.addons.proxy :as proxy]
            [hive-mcp.addons.mcp-bridge :as bridge]
            [hive-mcp.extensions.registry :as ext]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each
  (fn [f]
    (ext/clear-all-tools!)
    (try (f)
         (finally (ext/clear-all-tools!)))))

;; =============================================================================
;; Sample Data
;; =============================================================================

(def sample-tools
  "Minimal remote tool definitions for testing."
  [{:name        "search"
    :description "Search documents by query"
    :inputSchema {:type       "object"
                  :properties {"query" {:type "string"
                                        :description "Search query"}}
                  :required   ["query"]}}
   {:name        "convert"
    :description "Convert document format"
    :inputSchema {:type       "object"
                  :properties {"file"   {:type "string"}
                               "format" {:type "string"}}
                  :required   ["file" "format"]}}
   {:name        "health"
    :description "Service health check"
    :inputSchema {:type "object" :properties {}}}])

(defn echo-call-fn
  "Test call-fn that echoes tool name and params."
  [tool-name params]
  {:content [{:type "text"
              :text (str "echo:" tool-name ":" (pr-str params))}]})

(defn error-call-fn
  "Test call-fn that returns MCP error."
  [tool-name _params]
  {:isError true
   :content [{:type "text"
              :text (str "Error in " tool-name)}]})

(defn throw-call-fn
  "Test call-fn that throws."
  [_tool-name _params]
  (throw (ex-info "Transport failure" {})))

(defn multi-content-call-fn
  "Test call-fn that returns multiple content items."
  [_tool-name _params]
  {:content [{:type "text" :text "part-1"}
             {:type "text" :text "part-2"}
             {:type "text" :text "part-3"}]})

(defn raw-string-call-fn
  "Test call-fn that returns a raw string."
  [tool-name _params]
  (str "raw:" tool-name))

;; =============================================================================
;; Tests: make-call-handler
;; =============================================================================

(deftest make-call-handler-echo-test
  (testing "handler forwards to call-fn and extracts single content text"
    (let [handler (proxy/make-call-handler echo-call-fn "search")]
      (is (fn? handler))
      (let [result (handler {"query" "test"})]
        (is (= "text" (:type result)))
        (is (re-find #"echo:search:" (:text result)))))))

(deftest make-call-handler-error-response-test
  (testing "handler converts isError response to error text"
    (let [handler (proxy/make-call-handler error-call-fn "search")
          result  (handler {})]
      (is (= "text" (:type result)))
      (is (re-find #"Remote tool error:" (:text result)))
      (is (re-find #"Error in search" (:text result))))))

(deftest make-call-handler-exception-test
  (testing "handler catches exceptions and returns error text"
    (let [handler (proxy/make-call-handler throw-call-fn "search")
          result  (handler {})]
      (is (= "text" (:type result)))
      (is (re-find #"Proxy call failed:" (:text result))))))

(deftest make-call-handler-multi-content-test
  (testing "handler returns multi-content seq for >1 content items"
    (let [handler (proxy/make-call-handler multi-content-call-fn "search")
          result  (handler {})]
      (is (sequential? result))
      (is (= 3 (count result)))
      (is (= "part-1" (:text (first result)))))))

(deftest make-call-handler-raw-string-test
  (testing "handler wraps raw string return as text content"
    (let [handler (proxy/make-call-handler raw-string-call-fn "search")
          result  (handler {})]
      (is (= "text" (:type result)))
      (is (= "raw:search" (:text result))))))

(deftest make-call-handler-raw-map-test
  (testing "handler passes through map with :type key"
    (let [call-fn (fn [_ _] {:type "image" :data "base64..."})
          handler (proxy/make-call-handler call-fn "tool")
          result  (handler {})]
      (is (= "image" (:type result)))
      (is (= "base64..." (:data result))))))

(deftest make-call-handler-fallback-test
  (testing "handler pr-str's unknown return types"
    (let [call-fn (fn [_ _] 42)
          handler (proxy/make-call-handler call-fn "tool")
          result  (handler {})]
      (is (= "text" (:type result)))
      (is (= "42" (:text result))))))

;; =============================================================================
;; Tests: resolve-middleware-stack
;; =============================================================================

(deftest resolve-middleware-stack-keywords-test
  (testing "resolves keyword entries from middleware-factories"
    (let [stack (proxy/resolve-middleware-stack
                 [:strip-params :stringify]
                 {}
                 {:prefix "test" :tool-name "search" :tool-desc "desc"})]
      (is (= 2 (count stack)))
      (is (every? fn? stack)))))

(deftest resolve-middleware-stack-explicit-fn-test
  (testing "passes through explicit middleware fns"
    (let [my-mw (fn [handler] (fn [params] (handler (assoc params :injected true))))
          stack (proxy/resolve-middleware-stack
                 [my-mw]
                 {}
                 {:prefix "test" :tool-name "search" :tool-desc "desc"})]
      (is (= 1 (count stack)))
      (is (= my-mw (first stack))))))

(deftest resolve-middleware-stack-mixed-test
  (testing "handles mix of keywords and explicit fns"
    (let [my-mw (fn [handler] handler)
          stack (proxy/resolve-middleware-stack
                 [:stringify my-mw :strip-params]
                 {}
                 {:prefix "test" :tool-name "search" :tool-desc "desc"})]
      (is (= 3 (count stack)))
      (is (every? fn? stack)))))

(deftest resolve-middleware-stack-unknown-keyword-test
  (testing "throws on unknown keyword"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown middleware entry"
                          (proxy/resolve-middleware-stack
                           [:bogus]
                           {}
                           {:prefix "test" :tool-name "search" :tool-desc "desc"})))))

(deftest resolve-middleware-stack-empty-test
  (testing "empty spec returns empty vector"
    (let [stack (proxy/resolve-middleware-stack
                 []
                 {}
                 {:prefix "test" :tool-name "search" :tool-desc "desc"})]
      (is (= [] stack)))))

;; =============================================================================
;; Tests: proxy-tool-def
;; =============================================================================

(deftest proxy-tool-def-basic-test
  (testing "generates tool-def with default middleware and namespaced name"
    (let [remote {:name        "search"
                  :description "Search docs"
                  :inputSchema {:type "object" :properties {"q" {:type "string"}}}}
          td     (proxy/proxy-tool-def echo-call-fn "haystack" remote)]
      (is (= "haystack:search" (:name td)))
      (is (= "Search docs [via haystack]" (:description td)))
      (is (= {:type "object" :properties {"q" {:type "string"}}} (:inputSchema td)))
      (is (fn? (:handler td)))
      (is (= "haystack" (:proxy-source td))))))

(deftest proxy-tool-def-handler-works-test
  (testing "generated handler actually calls through call-fn"
    (let [calls   (atom [])
          call-fn (fn [tool-name params]
                    (swap! calls conj {:tool tool-name :params params})
                    {:content [{:type "text" :text "ok"}]})
          remote  {:name "search" :description "Search" :inputSchema {:type "object" :properties {}}}
          td      (proxy/proxy-tool-def call-fn "svc" remote {:skip-defaults? true})
          result  ((:handler td) {"query" "hello"})]
      ;; Handler was called
      (is (= 1 (count @calls)))
      (is (= "search" (:tool (first @calls))))
      ;; Result is correct
      (is (= "text" (:type result)))
      (is (= "ok" (:text result))))))

(deftest proxy-tool-def-default-middleware-test
  (testing "default middleware converts keyword keys to string keys"
    (let [received (atom nil)
          call-fn  (fn [_tool params]
                     (reset! received params)
                     {:content [{:type "text" :text "ok"}]})
          remote   {:name "search" :description "S" :inputSchema {:type "object" :properties {}}}
          td       (proxy/proxy-tool-def call-fn "svc" remote)
          _result  ((:handler td) {:query "hello" :directory "/tmp"})]
      ;; stringify-keys should convert keyword keys to strings
      ;; strip-params should remove :directory
      (is (some? @received))
      (is (contains? @received "query"))
      (is (not (contains? @received "directory"))))))

(deftest proxy-tool-def-skip-defaults-test
  (testing "skip-defaults? passes params as-is"
    (let [received (atom nil)
          call-fn  (fn [_tool params]
                     (reset! received params)
                     {:content [{:type "text" :text "ok"}]})
          remote   {:name "s" :description "S" :inputSchema {:type "object" :properties {}}}
          td       (proxy/proxy-tool-def call-fn "svc" remote {:skip-defaults? true})
          _result  ((:handler td) {:query "hello" :directory "/tmp"})]
      ;; No middleware — keyword keys and :directory preserved
      (is (some? @received))
      (is (contains? @received :query))
      (is (contains? @received :directory)))))

(deftest proxy-tool-def-custom-name-xf-test
  (testing "name-xf overrides default namespacing"
    (let [td (proxy/proxy-tool-def echo-call-fn "svc"
                                   {:name "search" :description "S" :inputSchema {:type "object" :properties {}}}
                                   {:name-xf (fn [name _prefix] (str "custom_" name))})]
      (is (= "custom_search" (:name td))))))

(deftest proxy-tool-def-custom-desc-xf-test
  (testing "desc-xf overrides default description"
    (let [td (proxy/proxy-tool-def echo-call-fn "svc"
                                   {:name "search" :description "Original" :inputSchema {:type "object" :properties {}}}
                                   {:desc-xf (fn [desc prefix] (str "[" prefix "] " desc))})]
      (is (= "[svc] Original" (:description td))))))

(deftest proxy-tool-def-schema-xf-test
  (testing "schema-xf transforms inputSchema"
    (let [td (proxy/proxy-tool-def echo-call-fn "svc"
                                   {:name "search" :description "S"
                                    :inputSchema {:type "object" :properties {"q" {:type "string"}}}}
                                   {:schema-xf (fn [schema]
                                                 (assoc-in schema [:properties "extra"]
                                                           {:type "string" :description "Injected"}))})]
      (is (get-in (:inputSchema td) [:properties "extra"])))))

(deftest proxy-tool-def-nil-description-test
  (testing "handles nil description gracefully"
    (let [td (proxy/proxy-tool-def echo-call-fn "svc"
                                   {:name "search" :description nil :inputSchema {:type "object" :properties {}}})]
      (is (= " [via svc]" (:description td))))))

(deftest proxy-tool-def-nil-schema-test
  (testing "handles nil inputSchema with default"
    (let [td (proxy/proxy-tool-def echo-call-fn "svc"
                                   {:name "search" :description "S" :inputSchema nil})]
      (is (= {:type "object" :properties {}} (:inputSchema td))))))

;; =============================================================================
;; Tests: auto-proxy-tool-defs
;; =============================================================================

(deftest auto-proxy-tool-defs-config-map-test
  (testing "generates tool-defs for all tools in config"
    (let [defs (proxy/auto-proxy-tool-defs
                {:call-fn echo-call-fn
                 :prefix  "api"
                 :tools   sample-tools})]
      (is (= 3 (count defs)))
      (is (= #{"api:search" "api:convert" "api:health"}
             (set (map :name defs))))
      (is (every? fn? (map :handler defs)))
      (is (every? #(= "api" (:proxy-source %)) defs)))))

(deftest auto-proxy-tool-defs-with-filter-test
  (testing "tool-filter excludes tools"
    (let [defs (proxy/auto-proxy-tool-defs
                {:call-fn     echo-call-fn
                 :prefix      "api"
                 :tools       sample-tools
                 :tool-filter #(not= "health" (:name %))})]
      (is (= 2 (count defs)))
      (is (not (some #(= "api:health" (:name %)) defs))))))

(deftest auto-proxy-tool-defs-empty-tools-test
  (testing "returns empty vector for no tools"
    (let [defs (proxy/auto-proxy-tool-defs
                {:call-fn echo-call-fn :prefix "api" :tools []})]
      (is (= [] defs)))))

(deftest auto-proxy-tool-defs-positional-3-test
  (testing "3-arg positional form works"
    (let [defs (proxy/auto-proxy-tool-defs echo-call-fn "api" sample-tools)]
      (is (= 3 (count defs)))
      (is (= "api:search" (:name (first defs)))))))

(deftest auto-proxy-tool-defs-positional-4-test
  (testing "4-arg positional form passes opts"
    (let [defs (proxy/auto-proxy-tool-defs
                echo-call-fn "api" sample-tools
                {:middleware [:logging]})]
      (is (= 3 (count defs))))))

(deftest auto-proxy-tool-defs-with-middleware-test
  (testing "middleware opts flow through to each tool-def"
    (let [received (atom [])
          call-fn  (fn [tool params]
                     (swap! received conj {:tool tool :params params})
                     {:content [{:type "text" :text "ok"}]})
          defs     (proxy/auto-proxy-tool-defs
                    {:call-fn    call-fn
                     :prefix     "svc"
                     :tools      [(first sample-tools)]
                     :middleware [:logging]})]
      ;; Invoke handler — should work through middleware chain
      (let [handler (:handler (first defs))
            result  (handler {"query" "test"})]
        (is (= "text" (:type result)))
        (is (= 1 (count @received)))))))

;; =============================================================================
;; Tests: auto-register! and deregister!
;; =============================================================================

(deftest auto-register-test
  (testing "registers proxy tools in extension registry"
    (let [result (proxy/auto-register!
                  {:call-fn echo-call-fn
                   :prefix  "test-svc"
                   :tools   sample-tools})]
      (is (= 3 (:count result)))
      (is (= "test-svc" (:prefix result)))
      (is (= #{"test-svc:search" "test-svc:convert" "test-svc:health"}
             (set (:registered result))))
      ;; Verify in registry
      (let [reg-tools (ext/get-registered-tools)]
        (is (= 3 (count reg-tools)))
        (is (every? fn? (map :handler reg-tools)))))))

(deftest auto-register-positional-test
  (testing "positional args work for auto-register!"
    (let [result (proxy/auto-register! echo-call-fn "pos" sample-tools)]
      (is (= 3 (:count result)))
      (is (= "pos" (:prefix result))))))

(deftest deregister-test
  (testing "deregisters all proxy tools with prefix"
    ;; Register first
    (proxy/auto-register!
     {:call-fn echo-call-fn :prefix "del-test" :tools sample-tools})
    (is (= 3 (count (ext/get-registered-tools))))
    ;; Deregister
    (let [removed (proxy/deregister! "del-test")]
      (is (= 3 (count removed)))
      (is (= 0 (count (ext/get-registered-tools)))))))

(deftest deregister-preserves-other-prefix-test
  (testing "deregister only removes tools with matching prefix"
    ;; Register two prefixes
    (proxy/auto-register!
     {:call-fn echo-call-fn :prefix "keep" :tools [(first sample-tools)]})
    (proxy/auto-register!
     {:call-fn echo-call-fn :prefix "remove" :tools [(second sample-tools)]})
    (is (= 2 (count (ext/get-registered-tools))))
    ;; Deregister only "remove"
    (proxy/deregister! "remove")
    (let [remaining (ext/get-registered-tools)]
      (is (= 1 (count remaining)))
      (is (= "keep:search" (:name (first remaining)))))))

(deftest deregister-empty-test
  (testing "deregister with no matching tools returns empty vector"
    (let [removed (proxy/deregister! "nonexistent")]
      (is (= [] removed)))))

;; =============================================================================
;; Tests: middleware-keys
;; =============================================================================

(deftest middleware-keys-test
  (testing "returns set of available middleware keywords"
    (let [keys (proxy/middleware-keys)]
      (is (set? keys))
      (is (contains? keys :strip-params))
      (is (contains? keys :stringify))
      (is (contains? keys :logging))
      (is (contains? keys :timeout))
      (is (contains? keys :retry))
      (is (contains? keys :inject-params)))))

;; =============================================================================
;; Tests: End-to-End Middleware Chain
;; =============================================================================

(deftest e2e-middleware-strip-and-stringify-test
  (testing "full chain: strip hive params + stringify keyword keys"
    (let [received (atom nil)
          call-fn  (fn [_tool params]
                     (reset! received params)
                     {:content [{:type "text" :text "ok"}]})
          defs     (proxy/auto-proxy-tool-defs
                    {:call-fn    call-fn
                     :prefix     "e2e"
                     :tools      [{:name "t" :description "T"
                                   :inputSchema {:type "object" :properties {}}}]})
          handler  (:handler (first defs))
          _        (handler {:query "hello"
                             :directory "/home/user"
                             :agent_id "swarm-123"
                             :_caller_id "coordinator"})]
      ;; After strip-params: directory, agent_id, _caller_id removed
      ;; After stringify-keys: remaining keyword keys become strings
      (is (some? @received))
      (is (contains? @received "query"))
      (is (not (contains? @received "directory")))
      (is (not (contains? @received :directory)))
      (is (not (contains? @received "agent_id")))
      (is (not (contains? @received "_caller_id"))))))

(deftest e2e-inject-params-test
  (testing "inject-params middleware adds extra params"
    (let [received (atom nil)
          call-fn  (fn [_tool params]
                     (reset! received params)
                     {:content [{:type "text" :text "ok"}]})
          defs     (proxy/auto-proxy-tool-defs
                    {:call-fn      call-fn
                     :prefix       "e2e"
                     :tools        [{:name "t" :description "T"
                                     :inputSchema {:type "object" :properties {}}}]
                     :middleware   [:inject-params]
                     :extra-params {:api_key "sk-123"}})
          handler  (:handler (first defs))
          _        (handler {"query" "hello"})]
      ;; inject-params middleware adds api_key before strip/stringify
      (is (some? @received))
      ;; Note: default middleware (strip + stringify) runs first (innermost),
      ;; then inject-params runs outermost. So injected params get through.
      (is (or (contains? @received :api_key)
              (contains? @received "api_key"))))))
