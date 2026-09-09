(ns hive-mcp.server.routes-identity-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.protocols.vessel :as vessel]
            [hive-mcp.server.routes.identity :as identity]
            [hive-mcp.server.routes.middleware :as middleware]
            [hive-mcp.tools.memory.scope :as scope]))

(deftest directory-derived-project-wins-over-vessel-project
  (testing "caller working directory outranks vessel project-id"
    (with-redefs [vessel/resolve-agent-context
                  (constantly {:cwd "/work/vessel" :project-id "stale-project"})
                  scope/get-current-project-id identity]
      (is (= "/work/caller"
             (identity/extract-project-id
              {:directory "/work/caller" :_caller_id "agent-1"}))))))

(deftest vessel-cwd-derives-project-before-vessel-project-id
  (testing "vessel cwd supplies HCR project when caller cwd is absent"
    (with-redefs [vessel/resolve-agent-context
                  (constantly {:cwd "/work/vessel" :project-id "stale-project"})
                  scope/get-current-project-id identity]
      (is (= "/work/vessel"
             (identity/extract-project-id {:_caller_id "agent-1"})))
      (is (= "/work/vessel"
             (identity/extract-directory {:_caller_id "agent-1"}))))))

(deftest handler-context-binds-resolved-directory
  (testing "request context binds vessel cwd instead of server cwd"
    (with-redefs [identity/extract-project-id (constantly "vessel-project")
                  identity/extract-directory (constantly "/work/vessel")
                  crystal/record-session-start! (constantly nil)]
      (let [handler (middleware/wrap-handler-context
                     (fn [_]
                       {:project-id (ctx/current-project-id)
                        :directory (ctx/current-directory)}))]
        (is (= {:project-id "vessel-project"
                :directory "/work/vessel"}
               (handler {:_caller_id "agent-1"})))))))

;; =============================================================================
;; wrap-delimited-block — the content ARRAY contract
;;
;; A single content MAP used to fall through find-last-text-idx into
;; (conj content {...}), and conj of a map onto a map MERGES, so the caller's
;; :text was silently replaced by the block and the payload was lost. No throw,
;; no no-op: a plausible looking result with the output dropped.
;; =============================================================================

(deftest a-content-map-does-not-eat-the-payload
  (testing "the map is normalized to a one element array, so the block APPENDS"
    (let [out (identity/wrap-delimited-block
               {:type "text" :text "OUT"} "FRONTIER" "[{:id \"x\"}]")]
      (is (vector? out))
      (is (= 1 (count out)))
      (is (re-find #"OUT" (:text (first out)))
          "the caller's own text survives")
      (is (re-find #"---FRONTIER---" (:text (first out)))))))

(deftest an-mcp-response-map-is-unwrapped
  (testing "{:content [...]} is the other shape normalize-content accepts"
    (let [out (identity/wrap-delimited-block
               {:content [{:type "text" :text "OUT"}]} "FRONTIER" "[]")]
      (is (= 1 (count out)))
      (is (re-find #"OUT" (:text (first out)))))))

(deftest a-content-array-is-unchanged
  (testing "the shape the live chain actually passes is untouched by normalizing"
    (is (= (identity/wrap-delimited-block [{:type "text" :text "OUT"}] "T" "B")
           (identity/wrap-delimited-block {:type "text" :text "OUT"} "T" "B"))
        "map and array inputs now agree, which is the whole point")))

(deftest an-empty-body-appends-nothing
  (let [content [{:type "text" :text "OUT"}]]
    (is (= content (identity/wrap-delimited-block content "T" nil)))
    (is (= content (identity/wrap-delimited-block content "T" "")))))

(deftest content-with-no-text-item-gets-a-new-one
  (testing "the append branch, which is the one a map input used to steal"
    (let [out (identity/wrap-delimited-block
               [{:type "image" :data "..."}] "FRONTIER" "[]")]
      (is (= 2 (count out)))
      (is (= "image" (:type (first out))))
      (is (re-find #"---FRONTIER---" (:text (second out)))))))
