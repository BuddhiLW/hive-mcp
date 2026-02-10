(ns test-lru-simple
  (:require [hive-mcp.algorithms.lru-cache :as lru]))

(println "=== Simple LRU Cache Test ===")

;; Create a cache
(def cache (lru/create 3))
(println "Created cache with capacity 3")

;; Add items
(def cache1 (lru/put cache :a 1))
(def cache2 (lru/put cache1 :b 2))
(def cache3 (lru/put cache2 :c 3))

(println "\nAfter adding 3 items:")
(println "Entries:" (lru/entries cache3))
(println "Size:" (lru/size cache3))

;; Add 4th item (should evict :a)
(def cache4 (lru/put cache3 :d 4))
(println "\nAfter adding 4th item (d=4):")
(println "Entries:" (lru/entries cache4))
(println "Size:" (lru/size cache4))

;; Get an item (should move to MRU)
(let [[value cache5] (lru/get cache4 :b)]
  (println "\nGet :b =>" value)
  (println "Entries after get:" (lru/entries cache5)))

;; Update existing item
(def cache6 (lru/put cache4 :c 30))
(println "\nAfter updating :c to 30:")
(println "Entries:" (lru/entries cache6))

;; Evict specific item
(def cache7 (lru/evict cache6 :b))
(println "\nAfter evicting :b:")
(println "Entries:" (lru/entries cache7))
(println "Size:" (lru/size cache7))

;; Clear cache
(def cache8 (lru/clear cache7))
(println "\nAfter clear:")
(println "Entries:" (lru/entries cache8))
(println "Size:" (lru/size cache8))

(println "\n=== Test completed successfully! ===")
