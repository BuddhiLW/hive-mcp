(ns hive-mcp.examples.pascal
  "Implementation of Pascal's triangle using DDD/SOLID principles.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; PascalTriangle protocol
(defprotocol PascalTriangle
  "Protocol for Pascal's triangle operations."
  (-value [this r c] "Returns the value at row r and column c.")
  (-row [this n] "Returns the nth row of Pascal's triangle.")
  (-triangle [this n] "Returns the first n rows of Pascal's triangle."))

;; RecursivePascal record implementing the PascalTriangle protocol
(deftype RecursivePascal []
  PascalTriangle
  (-value [_ r c]
    (cond
      (or (< r 0) (< c 0) (> c r)) 0
      (or (zero? c) (= c r)) 1
      :else (+ (-value (RecursivePascal.) (dec r) (dec c)) (-value (RecursivePascal.) (dec r) c))))
  (-row [_ n]
    (vec (for [c (range (inc n))]
           (-value (RecursivePascal.) n c))))
  (-triangle [_ n]
    (vec (for [r (range n)]
           (-row (RecursivePascal.) r)))))

;; MemoizedPascal record wrapping RecursivePascal with memoization
(deftype MemoizedPascal [pascal-impl memo-cache]
  PascalTriangle
  (-value [_ r c]
    (let [cache-key (str r "-" c)]
      (if-let [cached-value (get @memo-cache cache-key)]
        cached-value
        (let [computed-value (-value pascal-impl r c)]
          (swap! memo-cache assoc cache-key computed-value)
          computed-value))))
  (-row [_ n]
    (vec (for [c (range (inc n))]
           (-value (MemoizedPascal. pascal-impl memo-cache) n c))))
  (-triangle [_ n]
    (vec (for [r (range n)]
           (-row (MemoizedPascal. pascal-impl memo-cache) r)))))

;; Helper functions to create instances
(defn create-recursive-pascal []
  (RecursivePascal.))

(defn create-memoized-pascal []
  (MemoizedPascal. (RecursivePascal.) (atom {})))

;; Example usage:
;; (let [pascal (create-recursive-pascal)]
;;   (-value pascal 4 2)) ; => 6
;; (let [pascal (create-recursive-pascal)]
;;   (-row pascal 4)) ; => [1 4 6 4 1]
;; (let [pascal (create-recursive-pascal)]
;;   (-triangle pascal 5))
;; => [[1]
;;     [1 1]
;;     [1 2 1]
;;     [1 3 3 1]
;;     [1 4 6 4 1]]

;; Memoized example:
;; (let [memo-pascal (create-memoized-pascal)]
;;   (-value memo-pascal 4 2)) ; => 6 (cached after first call)