(ns hive-mcp.dns.result-test
  (:require [clojure.test :refer [deftest testing is are]]
            [hive-mcp.dns.result :as r]
            [hive-mcp.dns.result.taxonomy :as tax]))

;; --- Constructors ---

(deftest ok-test
  (testing "ok wraps a value"
    (is (= {:ok 42} (r/ok 42)))
    (is (= {:ok nil} (r/ok nil)))
    (is (= {:ok [1 2 3]} (r/ok [1 2 3])))))

(deftest err-test
  (testing "err with category only"
    (is (= {:error :io/timeout} (r/err :io/timeout))))
  (testing "err with category and data"
    (is (= {:error :io/read-failure :path "/tmp/x"}
           (r/err :io/read-failure {:path "/tmp/x"})))))

;; --- Predicates ---

(deftest ok?-test
  (testing "ok? recognizes success results"
    (is (true? (r/ok? (r/ok 1))))
    (is (true? (r/ok? (r/ok nil)))))
  (testing "ok? rejects errors and non-results"
    (is (false? (r/ok? (r/err :x))))
    (is (false? (r/ok? {})))
    (is (false? (r/ok? nil)))
    (is (false? (r/ok? "string")))))

(deftest err?-test
  (testing "err? recognizes error results"
    (is (true? (r/err? (r/err :x))))
    (is (true? (r/err? (r/err :x {:msg "boom"})))))
  (testing "err? rejects ok and non-results"
    (is (false? (r/err? (r/ok 1))))
    (is (false? (r/err? {})))
    (is (false? (r/err? nil)))))

;; --- bind ---

(deftest bind-test
  (testing "bind applies f to ok value"
    (is (= (r/ok 10)
           (r/bind (r/ok 5) #(r/ok (* 2 %))))))
  (testing "bind short-circuits on err"
    (let [e (r/err :fail)]
      (is (= e (r/bind e #(r/ok (inc %)))))))
  (testing "bind chains"
    (is (= (r/ok 6)
           (-> (r/ok 1)
               (r/bind #(r/ok (+ % 2)))
               (r/bind #(r/ok (* % 2))))))))

;; --- map-ok ---

(deftest map-ok-test
  (testing "map-ok transforms ok value"
    (is (= (r/ok 10) (r/map-ok (r/ok 5) #(* 2 %)))))
  (testing "map-ok passes through err"
    (let [e (r/err :fail)]
      (is (= e (r/map-ok e inc))))))

;; --- map-err ---

(deftest map-err-test
  (testing "map-err transforms error"
    (is (= {:error :fail :decorated true}
           (r/map-err (r/err :fail) #(assoc % :decorated true)))))
  (testing "map-err passes through ok"
    (is (= (r/ok 42) (r/map-err (r/ok 42) #(assoc % :decorated true))))))

;; --- let-ok ---

(deftest let-ok-test
  (testing "let-ok binds and returns body"
    (is (= (r/ok 3)
           (r/let-ok [a (r/ok 1)
                      b (r/ok 2)]
                     (r/ok (+ a b))))))
  (testing "let-ok short-circuits on error"
    (let [e (r/err :nope)]
      (is (= e
             (r/let-ok [a (r/ok 1)
                        b e
                        c (r/ok 99)]
                       (r/ok (+ a b c))))))))

;; --- try-effect ---

(deftest try-effect-test
  (testing "try-effect wraps success"
    (is (= (r/ok 42) (r/try-effect 42))))
  (testing "try-effect catches exception"
    (let [result (r/try-effect (throw (ex-info "boom" {})))]
      (is (r/err? result))
      (is (= :effect/exception (:error result)))
      (is (string? (:message result))))))

;; --- try-effect* ---

(deftest try-effect*-test
  (testing "try-effect* wraps success with custom category"
    (is (= (r/ok "ok") (r/try-effect* :io/read-failure "ok"))))
  (testing "try-effect* catches with custom category"
    (let [result (r/try-effect* :io/read-failure
                                (throw (ex-info "file not found" {})))]
      (is (r/err? result))
      (is (= :io/read-failure (:error result)))
      (is (string? (:message result))))))

;; --- taxonomy ---

(deftest known-error?-test
  (testing "recognizes known errors"
    (is (true? (tax/known-error? :io/timeout)))
    (is (true? (tax/known-error? :chroma/connection-failed)))
    (is (true? (tax/known-error? :effect/exception))))
  (testing "rejects unknown errors"
    (is (false? (tax/known-error? :foo/bar)))
    (is (false? (tax/known-error? :unknown)))))
