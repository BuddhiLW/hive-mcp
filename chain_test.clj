(ns chain-test
  (:require [hive-mcp.algorithms.lru-cache :as lru]))

(println "=== Chain Test LRU Cache ===")

;; Test with proper chaining
(let [cache0 (lru/create 3)
      cache1 (lru/put cache0 :a 1)
      cache2 (lru/put cache1 :b 2)
      cache3 (lru/put cache2 :c 3)]
  
  (println "After adding a=1, b=2, c=3:")
  (println "  Entries:" (lru/entries cache3))
  (println "  Size:" (lru/size cache3))
  (println "  Expected: [[:c 3] [:b 2] [:a 1]]")
  
  ;; Get :a should make it MRU
  (let [[value cache4] (lru/cache-get cache3 :a)]
    (println "\nAfter getting :a =>" value)
    (println "  Entries:" (lru/entries cache4))
    (println "  Expected: [[:a 1] [:c 3] [:b 2]]")
    
    ;; Add 4th item, should evict :b (LRU)
    (let [cache5 (lru/put cache4 :d 4)]
      (println "\nAfter adding d=4 (should evict :b):")
      (println "  Entries:" (lru/entries cache5))
      (println "  Expected: [[:d 4] [:a 1] [:c 3]]")
      (println "  Size:" (lru/size cache5)))))

(println "\n=== Complete Test ===")

;; Complete sequence test
(def cache
  (-> (lru/create 3)
      (lru/put :a 1)
      (lru/put :b 2)
      (lru/put :c 3)))

(println "Initial cache:")
(println "  Entries:" (lru/entries cache))

(let [[value cache'] (lru/cache-get cache :a)]
  (println "\nGet :a =>" value)
  (println "  Entries after get:" (lru/entries cache')))

(def cache2 (lru/put cache :d 4))
(println "\nAfter put :d 4:")
(println "  Entries:" (lru/entries cache2))
(println "  Size:" (lru/size cache2))

(println "\n=== All tests completed ===")
