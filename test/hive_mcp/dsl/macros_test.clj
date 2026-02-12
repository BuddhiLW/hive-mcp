(ns hive-mcp.dsl.macros-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.set :as set]
            [hive-mcp.dsl.macros :as macros]
            [hive-mcp.extensions.registry :as ext]))

;; =============================================================================
;; Fixtures — Clean up test-registered macros and extensions after each test
;; =============================================================================

(defn clean-test-macros
  "Remove any macros registered during the test, preserving built-ins."
  [f]
  (let [before (macros/registered-macros)]
    (try (f)
         (finally
           (doseq [m (set/difference (macros/registered-macros) before)]
             (macros/unregister-macro! m))
        ;; Clean up any test-registered extensions
           (doseq [k [:dm/a :dm/b :dm/c :dm/d]]
             (ext/deregister! k))))))

(use-fixtures :each clean-test-macros)

;; =============================================================================
;; Predicate Tests
;; =============================================================================

(deftest macro?-test
  (testing "registered built-in macros"
    (is (true? (macros/macro? "forge!"))     "forge! is registered")
    (is (true? (macros/macro? "catchup!"))   "catchup! is registered")
    (is (true? (macros/macro? "plan>kb"))    "plan>kb is registered")
    (is (true? (macros/macro? "plan>kb>forge!")) "plan>kb>forge! is registered"))

  (testing "unregistered verb-table entries"
    (is (not (macros/macro? "m+"))  "memory add is a verb, not macro")
    (is (not (macros/macro? "b?"))  "kanban list is a verb")
    (is (not (macros/macro? "k!"))  "KG impact is a verb (short !)")
    (is (not (macros/macro? "a!"))  "agent dispatch is a verb")
    (is (not (macros/macro? "h!"))  "hivemind shout is a verb")
    (is (not (macros/macro? "nonexistent!")) "unregistered = not macro"))

  (testing "edge cases"
    (is (not (macros/macro? ""))  "empty string")
    (is (not (macros/macro? nil)) "nil")
    (is (not (macros/macro? 42))  "non-string type")))

;; =============================================================================
;; Registration Tests
;; =============================================================================

(deftest register-macro!-test
  (testing "register-macro! adds to registry"
    (macros/register-macro! "test-macro!"
                            (fn [_params]
                              [(macros/step "m+" {:content "test"} "t-1")]))
    (is (contains? (macros/registered-macros) "test-macro!"))
    (is (macros/macro? "test-macro!")))

  (testing "register-macro! returns macro name"
    (is (= "another!" (macros/register-macro! "another!" (fn [_] [])))))

  (testing "register overwrites existing"
    (macros/register-macro! "over!" (fn [_] [(macros/step "m+" {} "v1")]))
    (macros/register-macro! "over!" (fn [_] [(macros/step "k>" {} "v2")]))
    (let [steps (macros/expand-macro "over!" {})]
      (is (= "k>" (first (first steps))) "second registration wins"))))

(deftest unregister-macro!-test
  (testing "unregister-macro! removes from registry"
    (macros/register-macro! "temp!" (fn [_] []))
    (is (macros/macro? "temp!"))
    (macros/unregister-macro! "temp!")
    (is (not (macros/macro? "temp!"))))

  (testing "unregister returns macro name"
    (macros/register-macro! "ret!" (fn [_] []))
    (is (= "ret!" (macros/unregister-macro! "ret!"))))

  (testing "built-in macros survive custom registration"
    (macros/register-macro! "custom!" (fn [_] []))
    (is (macros/macro? "forge!")   "built-in still registered")
    (is (macros/macro? "custom!") "custom also registered")))

(deftest defmacro-dsl-test
  (testing "defmacro-dsl creates and registers expansion"
    (macros/defmacro-dsl greet! [params]
      [(macros/step "h!" {:message (str "hello " (:name params))} "greet-1")])
    (is (macros/macro? "greet!"))
    (let [steps (macros/expand-macro "greet!" {:name "world"})
          [verb params] (first steps)]
      (is (= 1 (count steps)))
      (is (= "h!" verb))
      (is (= "hello world" (:message params)))
      (is (= "greet-1" (:id params)))))

  (testing "defmacro-dsl with multi-step expansion"
    (macros/defmacro-dsl two-step! [params]
      [(macros/step "m+" {:content (:msg params)} "ts-1")
       (macros/step "k>" {:from "$ref:ts-1.data.id" :to "target"}
                    "ts-2" ["ts-1"])])
    (let [steps (macros/expand-macro "two-step!" {:msg "hello"})]
      (is (= 2 (count steps)))
      (is (= "m+" (first (first steps))))
      (is (= "k>" (first (second steps))))
      (is (= ["ts-1"] (get-in (second steps) [1 :depends_on]))))))

;; =============================================================================
;; Step Constructor Tests
;; =============================================================================

(deftest step-test
  (testing "step without dependencies"
    (let [[verb params] (macros/step "m+" {:content "x"} "s-1")]
      (is (= "m+" verb))
      (is (= "x" (:content params)))
      (is (= "s-1" (:id params)))
      (is (nil? (:depends_on params)))))

  (testing "step with dependencies"
    (let [[verb params] (macros/step "k>" {:from "a"} "s-2" ["s-1"])]
      (is (= "k>" verb))
      (is (= "a" (:from params)))
      (is (= "s-2" (:id params)))
      (is (= ["s-1"] (:depends_on params)))))

  (testing "step with multiple dependencies"
    (let [[_verb params] (macros/step "a!" {} "s-3" ["s-1" "s-2"])]
      (is (= ["s-1" "s-2"] (:depends_on params)))))

  (testing "step with nil params defaults to map with id"
    (let [[verb params] (macros/step "a?" nil "s-4")]
      (is (= "a?" verb))
      (is (= "s-4" (:id params)))
      (is (= {} (dissoc params :id))))))

;; =============================================================================
;; Built-in Macro Noop Fallback Tests
;; =============================================================================

(deftest forge!-noop-test
  (testing "forge! returns empty without extension"
    (let [steps (macros/expand-macro "forge!" {})]
      (is (= [] steps) "noop returns empty vector")))

  (testing "forge! with any params returns empty without extension"
    (is (= [] (macros/expand-macro "forge!" {:vulcan true :slots 3})))
    (is (= [] (macros/expand-macro "forge!" nil)))))

(deftest catchup!-noop-test
  (testing "catchup! returns empty without extension"
    (is (= [] (macros/expand-macro "catchup!" {}))))

  (testing "catchup! with directory returns empty without extension"
    (is (= [] (macros/expand-macro "catchup!" {:directory "/project"})))))

(deftest plan>kb-noop-test
  (testing "plan>kb returns empty without extension"
    (is (= [] (macros/expand-macro "plan>kb" {:plan_id "mem-123"}))))

  (testing "plan>kb with plan_path returns empty without extension"
    (is (= [] (macros/expand-macro "plan>kb" {:plan_path "/path/plan.edn"})))))

(deftest plan>kb>forge!-noop-test
  (testing "plan>kb>forge! returns empty without extension"
    (is (= [] (macros/expand-macro "plan>kb>forge!" {:plan_id "mem-456"}))))

  (testing "plan>kb>forge! with params returns empty without extension"
    (is (= [] (macros/expand-macro "plan>kb>forge!" {:plan_id "x" :vulcan true :slots 3})))))

;; =============================================================================
;; Extension-Backed Expansion Tests
;; =============================================================================

(deftest forge!-with-extension-test
  (testing "forge! delegates to extension when available"
    (ext/register! :dm/a
                   (fn [{:keys [status] :or {status "todo"}}]
                     [(macros/step "b?" {:status status} "forge-survey")
                      (macros/step "a+" {:type "ling" :task "$ref:forge-survey"}
                                   "forge-spawn" ["forge-survey"])
                      (macros/step "a?" {} "forge-monitor" ["forge-spawn"])]))
    (let [steps (macros/expand-macro "forge!" {})]
      (is (= 3 (count steps)))
      (is (= "b?" (first (nth steps 0))))
      (is (= "a+" (first (nth steps 1))))
      (is (= "a?" (first (nth steps 2)))))))

(deftest catchup!-with-extension-test
  (testing "catchup! delegates to extension when available"
    (ext/register! :dm/b
                   (fn [{:keys [directory]}]
                     [(macros/step "s<" (cond-> {} directory (assoc :directory directory))
                                   "catchup-session")
                      (macros/step "m?" {:type "axiom"} "catchup-axioms" ["catchup-session"])]))
    (let [steps (macros/expand-macro "catchup!" {:directory "/project"})]
      (is (= 2 (count steps)))
      (is (= "s<" (first (first steps))))
      (is (= "/project" (get-in (first steps) [1 :directory]))))))

;; =============================================================================
;; expand-all Tests
;; =============================================================================

(deftest expand-all-test
  (testing "mixed verbs and noop macros"
    (let [steps (macros/expand-all [["m+" {:content "hello"}]
                                    ["catchup!" {}]])]
      ;; m+ -> 1 sentence passthrough, catchup! noop -> 0 sentences
      (is (= 1 (count steps)))
      (is (= "m+" (first (nth steps 0))))))

  (testing "all regular verbs (no expansion needed)"
    (let [input [["m+" {:content "a"}]
                 ["k>" {:from "x" :to "y"}]]
          steps (macros/expand-all input)]
      (is (= 2 (count steps)))
      (is (= input steps) "passthrough unchanged")))

  (testing "empty paragraph"
    (is (= [] (macros/expand-all []))))

  (testing "single noop macro sentence"
    (let [steps (macros/expand-all [["forge!" {:slots 2}]])]
      (is (= 0 (count steps)) "noop macro expands to nothing")))

  (testing "multiple noop macros"
    (let [steps (macros/expand-all [["catchup!" {}]
                                    ["forge!" {}]])]
      ;; both macros expand to [] without extension
      (is (= 0 (count steps)))))

  (testing "macro at different positions with regular verbs"
    (let [steps (macros/expand-all [["m+" {:content "before"}]
                                    ["forge!" {}]
                                    ["k>" {:from "a" :to "b"}]])]
      ;; 1 + 0 (noop) + 1 = 2
      (is (= 2 (count steps)))
      (is (= "m+" (first (nth steps 0))))
      (is (= "k>" (first (nth steps 1)))))))

;; =============================================================================
;; Nested Expansion Tests
;; =============================================================================

(deftest nested-expansion-test
  (testing "macro expanding to another noop macro"
    (macros/register-macro! "meta-catchup!"
                            (fn [params]
                              [["catchup!" params]]))
    (let [steps (macros/expand-all [["meta-catchup!" {}]])]
      ;; meta-catchup! -> catchup! -> [] (noop)
      (is (= 0 (count steps)))))

  (testing "two levels of nesting with noops"
    (macros/register-macro! "deep!"
                            (fn [_]
                              [["meta-catchup!" {}]]))
    (macros/register-macro! "meta-catchup!"
                            (fn [params]
                              [["catchup!" params]]))
    (let [steps (macros/expand-all [["deep!" {}]])]
      ;; deep! -> meta-catchup! -> catchup! -> [] (noop)
      (is (= 0 (count steps)))))

  (testing "nested macro mixed with regular verbs"
    (macros/register-macro! "wrap-forge!"
                            (fn [params]
                              (into [["m+" {:content "pre-forge"}]]
                                    [["forge!" params]])))
    (let [steps (macros/expand-all [["wrap-forge!" {:slots 2}]])]
      ;; m+ passthrough + forge! noop = 1
      (is (= 1 (count steps)))
      (is (= "m+" (first (first steps))))))

  (testing "param threading through nested macros with extension"
    (ext/register! :dm/b
                   (fn [{:keys [directory]}]
                     [(macros/step "s<" (cond-> {} directory (assoc :directory directory))
                                   "catchup-session")]))
    (macros/register-macro! "dir-catchup!"
                            (fn [params]
                              [["catchup!" {:directory (:dir params)}]]))
    (let [steps (macros/expand-all [["dir-catchup!" {:dir "/home"}]])]
      (is (= 1 (count steps)))
      (is (= "/home" (get-in (first steps) [1 :directory]))))))

;; =============================================================================
;; Error Handling Tests
;; =============================================================================

(deftest unknown-macro-error-test
  (testing "expand-macro throws for unknown macro"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unknown DSL macro"
                          (macros/expand-macro "nonexistent!" {}))))

  (testing "error data includes macro name"
    (try
      (macros/expand-macro "nonexistent!" {})
      (catch clojure.lang.ExceptionInfo e
        (is (= "nonexistent!" (:macro (ex-data e)))))))

  (testing "error data includes available macros"
    (try
      (macros/expand-macro "nonexistent!" {})
      (catch clojure.lang.ExceptionInfo e
        (let [available (:available (ex-data e))]
          (is (sequential? available))
          (is (some #{"forge!"} available))
          (is (some #{"catchup!"} available))))))

  (testing "expand-macro with nil params does not throw"
    (is (= [] (macros/expand-macro "forge!" nil)) "noop returns empty")))

(deftest expansion-depth-limit-test
  (testing "cycle detection via depth limit"
    (macros/register-macro! "cycle!"
                            (fn [_]
                              [["cycle!" {}]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Maximum macro expansion depth"
                          (macros/expand-all [["cycle!" {}]]))))

  (testing "mutual recursion detected"
    (macros/register-macro! "ping!"
                            (fn [_] [["pong!" {}]]))
    (macros/register-macro! "pong!"
                            (fn [_] [["ping!" {}]]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Maximum macro expansion depth"
                          (macros/expand-all [["ping!" {}]])))))

;; =============================================================================
;; registered-macros Tests
;; =============================================================================

(deftest registered-macros-test
  (testing "returns set of built-in names"
    (let [names (macros/registered-macros)]
      (is (set? names))
      (is (contains? names "forge!"))
      (is (contains? names "catchup!"))
      (is (contains? names "plan>kb"))
      (is (contains? names "plan>kb>forge!"))))

  (testing "includes newly registered macros"
    (macros/register-macro! "new-one!" (fn [_] []))
    (is (contains? (macros/registered-macros) "new-one!"))))

;; =============================================================================
;; Extension Delegation Verification
;; =============================================================================

(deftest extension-delegation-test
  (testing "all built-in macros delegate and return [] by default"
    (doseq [macro-name ["forge!" "catchup!" "plan>kb" "plan>kb>forge!"]]
      (let [steps (macros/expand-macro macro-name
                                       (case macro-name
                                         "plan>kb"        {:plan_id "test"}
                                         "plan>kb>forge!" {:plan_id "test"}
                                         {}))]
        (is (= [] steps)
            (str macro-name " should return [] without extension")))))

  (testing "extension registration enables expansion"
    (ext/register! :dm/a
                   (fn [_] [(macros/step "b?" {} "test-step")]))
    (let [steps (macros/expand-macro "forge!" {})]
      (is (= 1 (count steps)))
      (is (= "b?" (first (first steps))))
      (is (= "test-step" (get-in (first steps) [1 :id]))))))
