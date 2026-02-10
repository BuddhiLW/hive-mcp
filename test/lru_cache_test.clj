(ns lru-cache-test
  (:require [hive-mcp.algorithms.lru-cache :as lru]))

(println "=== LRU Cache Tests ===")

;; Test 1: Basic operations
(println "\nTest 1: Basic operations")
(let [cache (lru/create 3)]
  (println "Created cache with capacity 3")
  (lru/put cache :a 1)
  (lru/put cache :b 2)
  (lru/put cache :c 3)
  (println "Added 3 items: a=1, b=2, c=3")
  (println "Entries (MRU to LRU):" (lru/entries cache))
  (println "Size:" (lru/size cache))
  (println "Capacity:" (lru/capacity cache)))

;; Test 2: Eviction on overflow
(println "\nTest 2: Eviction on overflow")
(let [cache (lru/create 3)]
  (lru/put cache :a 1)
  (lru/put cache :b 2)
  (lru/put cache :c 3)
  (println "Initial entries:" (lru/entries cache))
  (lru/put cache :d 4) ; Should evict :a (least recently used)
  (println "After adding 4th item (d=4):" (lru/entries cache))
  (println "Get :b (makes it MRU):" (lru/get cache :b))
  (println "Entries after accessing :b:" (lru/entries cache)))

;; Test 3: Update existing key
(println "\nTest 3: Update existing key")
(let [cache (lru/create 3)]
  (lru/put cache :a 1)
  (lru/put cache :b 2)
  (lru/put cache :c 3)
  (println "Initial entries:" (lru/entries cache))
  (lru/put cache :b 20) ; Update :b, should become MRU
  (println "After updating :b to 20:" (lru/entries cache))
  (println "Get :a (should still exist):" (lru/get cache :a)))

;; Test 4: Evict specific key
(println "\nTest 4: Evict specific key")
(let [cache (lru/create 3)]
  (lru/put cache :a 1)
  (lru/put cache :b 2)
  (lru/put cache :c 3)
  (println "Initial entries:" (lru/entries cache))
  (lru/evict cache :b)
  (println "After evicting :b:" (lru/entries cache))
  (println "Size after eviction:" (lru/size cache)))

;; Test 5: Clear cache
(println "\nTest 5: Clear cache")
(let [cache (lru/create 3)]
  (lru/put cache :a 1)
  (lru/put cache :b 2)
  (println "Before clear - entries:" (lru/entries cache))
  (println "Before clear - size:" (lru/size cache))
  (lru/clear cache)
  (println "After clear - entries:" (lru/entries cache))
  (println "After clear - size:" (lru/size cache)))

;; Test 6: Access pattern affects eviction
(println "\nTest 6: Access pattern affects eviction")
(let [cache (lru/create 3)]
  (lru/put cache :a 1)
  (lru/put cache :b 2)
  (lru/put cache :c 3)
  (println "Initial:" (lru/entries cache))
  ;; Access :a, making it MRU
  (lru/get cache :a)
  (println "After accessing :a:" (lru/entries cache))
  ;; Add 4th item, should evict :b (now LRU)
  (lru/put cache :d 4)
  (println "After adding :d (should evict :b):" (lru/entries cache)))

(println "\n=== All tests completed ===")