(ns debug-test
  (:require [hive-mcp.algorithms.lru-cache :as lru]))

(println "=== Debug LRU Cache Test ===")

;; Simple test
(def cache0 (lru/create 3))
(println "Created cache, capacity:" (lru/capacity cache0))

(def cache1 (lru/put cache0 :a 1))
(println "\n1. Put :a 1")
(println "   Entries:" (lru/entries cache1))
(println "   Size:" (lru/size cache1))

(def cache2 (lru/put cache1 :b 2))
(println "\n2. Put :b 2")
(println "   Entries:" (lru/entries cache2))
(println "   Size:" (lru/size cache2))

(def cache3 (lru/put cache2 :c 3))
(println "\n3. Put :c 3")
(println "   Entries:" (lru/entries cache3))
(println "   Size:" (lru/size cache3))

;; Get :a should make it MRU
(let [[value cache4] (lru/cache-get cache3 :a)]
  (println "\n4. Get :a =>" value)
  (println "   Entries after get:" (lru/entries cache4))
  (println "   Expected: [[:a 1] [:c 3] [:b 2]]"))

;; Add 4th item, should evict :b (LRU)
(def cache5 (lru/put cache4 :d 4))
(println "\n5. Put :d 4 (should evict :b)")
(println "   Entries:" (lru/entries cache5))
(println "   Expected: [[:d 4] [:a 1] [:c 3]]")
(println "   Size:" (lru/size cache5))

(println "\n=== Debug test completed ===")
