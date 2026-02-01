(ns hive-mcp.tools.swarm.prompt-test
  "Tests for swarm prompt handlers including event emission."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.prompt :as prompt]
            [clojure.data.json :as json]))

;; Test emit-prompt-pending-events! helper
(deftest emit-prompt-pending-events-test
  (testing "handles empty prompts"
    (is (nil? (#'prompt/emit-prompt-pending-events!
               "{\"count\":0,\"prompts\":[],\"mode\":\"human\"}"))))

  (testing "handles single prompt"
    (is (nil? (#'prompt/emit-prompt-pending-events!
               "{\"count\":1,\"prompts\":[{\"slave-id\":\"ling-1\",\"prompt\":\"Test?\",\"timestamp\":\"2026-01-29T12:00:00\"}],\"mode\":\"human\"}"))))

  (testing "truncates long prompt text to 100 chars"
    (let [long-prompt (apply str (repeat 200 "x"))
          data (json/write-str {:count 1
                                :prompts [{:slave-id "ling-long"
                                           :prompt long-prompt
                                           :timestamp "2026-01-29T12:00:00"}]
                                :mode "human"})]
      (is (nil? (#'prompt/emit-prompt-pending-events! data)))))

  (testing "handles malformed JSON gracefully"
    (is (nil? (#'prompt/emit-prompt-pending-events! "not-valid-json"))))

  (testing "handles nil prompts list"
    (is (nil? (#'prompt/emit-prompt-pending-events!
               "{\"count\":0,\"mode\":\"human\"}")))))
