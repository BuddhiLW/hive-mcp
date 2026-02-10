(ns test-lru
  (:require [hive-mcp.algorithms.lru-cache :as lru]))

(println "=== Testing LRU Cache Implementation ===")

;; Test 1: Basic operations
(println "\nTest 1: Create cache and add items")
(def cache1 (lru/create 3))
(println "Created cache with capacity 3")

(def cache2 (lru/put cache1 :a 1))
(println "Put :a 1")
(def cache3 (lru/put cache2 :b 2))
(println "Put :b 2")
(def cache4 (lru/put cache3 :c 3))
(println "Put :c 3")

(println "Entries (MRU to LRU):" (lru/entries cache4))
(println "Size:" (lru/size cache4))
(println "Capacity:" (lru/capacity cache4))

;; Test 2: Eviction on overflow
(println "\nTest 2: Eviction on overflow")
(def cache5 (lru/put cache4 :d 4))
(println "Put :d 4 (should evict :a)")
(println "Entries:" (lru/entries cache5))
(println "Size:" (lru/size cache5))

;; Test 3: Get and update MRU
(println "\nTest 3: Get and update MRU")
(let [[value cache6] (lru/get cache5 :b)]
  (println "Get :b =>" value)
  (println "Entries after get:" (lru/entries cache6)))

;; Test 4: Update existing key
(println "\nTest 4: Update existing key")
(def cache7 (lru/put cache4 :b 20)) ; Update :b in original cache
(println "Update :b to 20")
(println "Entries:" (lru/entries cache7))

;; Test 5: Evict specific key
(println "\nTest 5: Evict specific key")
(def cache8 (lru/evict cache4 :b))
(println "Evict :b")
(println "Entries:" (lru/entries cache8))
(println "Size:" (lru/size cache8))

;; Test 6: Clear cache
(println "\nTest 6: Clear cache")
(def cache9 (lru/clear cache4))
(println "After clear:")
(println "Entries:" (lru/entries cache9))
(println "Size:" (lru/size cache9))

(println "\n=== All tests completed! ===")
