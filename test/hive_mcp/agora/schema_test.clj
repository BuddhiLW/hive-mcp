(ns hive-mcp.agora.schema-test
  "Tests for Agora dialogue schema and CRUD operations.

   Coverage: Dialogue/turn lifecycle, signal counting, consensus checking."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agora.schema :as agora]))

;; Fixture - reset connection before each test
(defn with-fresh-conn [f]
  (agora/reset-conn!)
  (f))

(use-fixtures :each with-fresh-conn)

;; =============================================================================
;; Dialogue CRUD Tests
;; =============================================================================

(deftest dialogue-create-test
  (testing "Creating a dialogue"
    (let [{:keys [id]} (agora/create-dialogue!
                        {:participants ["ling-1" "ling-2"]
                         :name "test-dialogue"})]
      (is (string? id))
      (let [d (agora/get-dialogue id)]
        (is (= "test-dialogue" (:name d)))
        (is (= :active (:status d)))
        ;; Participants may be in any order (cardinality/many uses sets)
        (is (= #{"ling-1" "ling-2"} (set (:participants d))))))))

(deftest dialogue-create-auto-id-test
  (testing "Dialogue ID is auto-generated when not provided"
    (let [{:keys [id]} (agora/create-dialogue!
                        {:participants ["ling-1"]})]
      (is (string? id))
      (is (pos? (count id))))))

(deftest dialogue-create-with-config-test
  (testing "Dialogue stores custom config"
    (let [{:keys [id]} (agora/create-dialogue!
                        {:participants ["ling-1" "ling-2"]
                         :config {:threshold 0.6 :timeout-ms 60000}})]
      (let [d (agora/get-dialogue id)]
        (is (= 0.6 (:threshold (:config d))))
        (is (= 60000 (:timeout-ms (:config d))))))))

(deftest dialogue-get-nonexistent-test
  (testing "Getting non-existent dialogue returns nil"
    (is (nil? (agora/get-dialogue "nonexistent")))))

(deftest dialogue-update-status-test
  (testing "Updating dialogue status"
    (let [{:keys [id]} (agora/create-dialogue!
                        {:participants ["ling-1"]})]
      (agora/update-dialogue-status! id :consensus)
      (let [d (agora/get-dialogue id)]
        (is (= :consensus (:status d)))))))

(deftest dialogue-list-test
  (testing "Listing dialogues"
    (agora/create-dialogue! {:participants ["a"] :name "d1"})
    (agora/create-dialogue! {:participants ["b"] :name "d2"})
    (let [dialogues (agora/list-dialogues)]
      (is (= 2 (count dialogues)))
      (is (some #(= "d1" (:name %)) dialogues))
      (is (some #(= "d2" (:name %)) dialogues)))))

(deftest dialogue-list-filtered-test
  (testing "Listing dialogues by status"
    (let [{id1 :id} (agora/create-dialogue! {:participants ["a"]})
          {id2 :id} (agora/create-dialogue! {:participants ["b"]})]
      (agora/update-dialogue-status! id1 :consensus)
      (let [active (agora/list-dialogues :active)
            consensus (agora/list-dialogues :consensus)]
        (is (= 1 (count active)))
        (is (= 1 (count consensus)))
        (is (= id2 (:id (first active))))
        (is (= id1 (:id (first consensus))))))))

;; =============================================================================
;; Turn CRUD Tests
;; =============================================================================

(deftest turn-add-test
  (testing "Adding a turn to dialogue"
    (let [{:keys [id]} (agora/create-dialogue!
                        {:participants ["ling-1" "ling-2"]})
          turn (agora/add-turn! id {:sender "ling-1"
                                    :receiver "ling-2"
                                    :message "Hello"
                                    :signal :propose})]
      (is (string? (:id turn)))
      (is (= 1 (:turn-number turn))))))

(deftest turn-add-increments-number-test
  (testing "Turn numbers increment correctly"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b"]})]
      (let [t1 (agora/add-turn! did {:sender "a" :receiver "b"
                                     :message "m1" :signal :propose})
            t2 (agora/add-turn! did {:sender "b" :receiver "a"
                                     :message "m2" :signal :counter})
            t3 (agora/add-turn! did {:sender "a" :receiver "b"
                                     :message "m3" :signal :approve})]
        (is (= 1 (:turn-number t1)))
        (is (= 2 (:turn-number t2)))
        (is (= 3 (:turn-number t3)))))))

(deftest turn-get-test
  (testing "Getting a turn by ID"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b"]})
          {tid :id} (agora/add-turn! did {:sender "a"
                                          :receiver "b"
                                          :message "test msg"
                                          :signal :propose})]
      (let [turn (agora/get-turn tid)]
        (is (= "a" (:sender turn)))
        (is (= "b" (:receiver turn)))
        (is (= "test msg" (:message turn)))
        (is (= :propose (:signal turn)))))))

(deftest turn-get-nonexistent-test
  (testing "Getting non-existent turn returns nil"
    (is (nil? (agora/get-turn "nonexistent")))))

(deftest turn-to-nonexistent-dialogue-test
  (testing "Adding turn to non-existent dialogue returns nil"
    (is (nil? (agora/add-turn! "nonexistent"
                               {:sender "a" :receiver "b"
                                :message "msg" :signal :propose})))))

(deftest turns-get-all-test
  (testing "Getting all turns for a dialogue"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b"]})]
      (agora/add-turn! did {:sender "a" :receiver "b"
                            :message "first" :signal :propose})
      (agora/add-turn! did {:sender "b" :receiver "a"
                            :message "second" :signal :counter})
      (let [turns (agora/get-turns did)]
        (is (= 2 (count turns)))
        (is (= "first" (:message (first turns))))
        (is (= "second" (:message (second turns))))))))

(deftest turns-get-by-sender-test
  (testing "Getting turns by sender"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b"]})]
      (agora/add-turn! did {:sender "a" :receiver "b"
                            :message "from-a-1" :signal :propose})
      (agora/add-turn! did {:sender "b" :receiver "a"
                            :message "from-b" :signal :counter})
      (agora/add-turn! did {:sender "a" :receiver "b"
                            :message "from-a-2" :signal :approve})
      (let [a-turns (agora/get-turns-by-sender did "a")
            b-turns (agora/get-turns-by-sender did "b")]
        (is (= 2 (count a-turns)))
        (is (= 1 (count b-turns)))))))

;; =============================================================================
;; Signal Types Tests
;; =============================================================================

(deftest turn-all-signals-test
  (testing "All signal types are accepted"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b"]})]
      (doseq [signal [:propose :counter :no-change :approve :defer]]
        (let [result (agora/add-turn! did {:sender "a" :receiver "b"
                                           :message (str signal)
                                           :signal signal})]
          (is (some? result) (str "Failed for signal: " signal)))))))

;; =============================================================================
;; Consensus Helpers Tests
;; =============================================================================

(deftest count-signals-test
  (testing "Counting signals in a dialogue"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b" "c"]})]
      (agora/add-turn! did {:sender "a" :receiver "all"
                            :message "proposal" :signal :propose})
      (agora/add-turn! did {:sender "b" :receiver "all"
                            :message "agree" :signal :approve})
      (agora/add-turn! did {:sender "c" :receiver "all"
                            :message "defer" :signal :defer})
      (let [counts (agora/count-signals did)]
        (is (= 1 (:propose counts)))
        (is (= 1 (:approve counts)))
        (is (= 1 (:defer counts)))
        (is (= 0 (:counter counts)))))))

(deftest check-consensus-not-reached-test
  (testing "Consensus not reached with counters"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b" "c"]
                      :config {:threshold 0.8}})]
      (agora/add-turn! did {:sender "a" :receiver "all"
                            :message "proposal" :signal :propose})
      (agora/add-turn! did {:sender "b" :receiver "all"
                            :message "disagree" :signal :counter})
      (agora/add-turn! did {:sender "c" :receiver "all"
                            :message "disagree" :signal :counter})
      (let [result (agora/check-consensus did)]
        (is (false? (:reached? result)))
        (is (< (:approval-ratio result) 0.5))))))

(deftest check-consensus-reached-test
  (testing "Consensus reached with approvals"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b" "c" "d"]
                      :config {:threshold 0.75}})]
      (agora/add-turn! did {:sender "a" :receiver "all"
                            :message "proposal" :signal :propose})
      (agora/add-turn! did {:sender "b" :receiver "all"
                            :message "yes" :signal :approve})
      (agora/add-turn! did {:sender "c" :receiver "all"
                            :message "ok" :signal :approve})
      (agora/add-turn! did {:sender "d" :receiver "all"
                            :message "defer" :signal :defer})
      (let [result (agora/check-consensus did)]
        ;; 3 out of 4 turns are approve/defer = 0.75
        (is (true? (:reached? result)))
        (is (>= (:approval-ratio result) 0.75))))))

(deftest check-consensus-defer-counts-as-approval-test
  (testing "Defer signals count towards consensus"
    (let [{did :id} (agora/create-dialogue!
                     {:participants ["a" "b"]
                      :config {:threshold 1.0}})]
      (agora/add-turn! did {:sender "a" :receiver "b"
                            :message "defer" :signal :defer})
      (agora/add-turn! did {:sender "b" :receiver "a"
                            :message "defer" :signal :defer})
      (let [result (agora/check-consensus did)]
        (is (true? (:reached? result)))
        (is (= 1.0 (:approval-ratio result)))))))

;; =============================================================================
;; Schema Validation Tests
;; =============================================================================

(deftest status-enums-test
  (testing "Status enums are defined"
    (is (contains? agora/dialogue-statuses :active))
    (is (contains? agora/dialogue-statuses :consensus))
    (is (contains? agora/dialogue-statuses :timeout))
    (is (contains? agora/dialogue-statuses :aborted))))

(deftest signal-enums-test
  (testing "Signal enums are defined"
    (is (contains? agora/turn-signals :propose))
    (is (contains? agora/turn-signals :counter))
    (is (contains? agora/turn-signals :no-change))
    (is (contains? agora/turn-signals :approve))
    (is (contains? agora/turn-signals :defer))))
