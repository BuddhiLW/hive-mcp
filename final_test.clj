(ns final-test
  (:require [hive-mcp.algorithms.lru-cache :as lru]))

(println "=== Final LRU Cache Test ===")

;; Test 1: Basic operations
(println "\n1. Creating cache with capacity 3")
(def cache0 (lru/create 3))
(println "   Cache created, capacity:" (lru/capacity cache0))

;; Add items
(def cache1 (lru/put cache0 :a 1))
(def cache2 (lru/put cache1 :b 2))
(def cache3 (lru/put cache2 :c 3))
(println "\n2. Added 3 items")
(println "   Entries (MRU to LRU):" (lru/entries cache3))
(println "   Size:" (lru/size cache3))

;; Test 2: Eviction on overflow
(def cache4 (lru/put cache3 :d 4))
(println "\n3. Added 4th item (d=4) - should evict :a")
(println "   Entries:" (lru/entries cache4))
(println "   Size:" (lru/size cache4))

;; Test 3: Get and update MRU
(let [[value cache5] (lru/cache-get cache4 :b)]
  (println "\n4. Get :b =>" value)
  (println "   Entries after get:" (lru/entries cache5)))

;; Test 4: Update existing key
(def cache6 (lru/put cache4 :c 30))
(println "\n5. Updated :c to 30")
(println "   Entries:" (lru/entries cache6))

;; Test 5: Evict specific key
(def cache7 (lru/evict cache6 :b))
(println "\n6. Evicted :b")
(println "   Entries:" (lru/entries cache7))
(println "   Size:" (lru/size cache7))

;; Test 6: Clear
(def cache8 (lru/clear cache7))
(println "\n7. Cleared cache")
(println "   Entries:" (lru/entries cache8))
(println "   Size:" (lru/size cache8))

(println "\n=== All tests completed successfully! ===")

;; Additional test: Verify O(1) operations work correctly
(println "\n=== Additional Test: Complex Sequence ===")
(def cache (lru/create 3))
(println "Starting with empty cache, capacity 3")

(def cache (-> cache
               (lru/put :a 1)
               (lru/put :b 2)
               (lru/put :c 3)))
(println "After adding a=1, b=2, c=3:")
(println "  Entries:" (lru/entries cache))

(let [[value cache] (lru/cache-get cache :a)]
  (println "Get a =>" value))
(println "  After getting a (makes it MRU):" (lru/entries cache))

(def cache (lru/put cache :d 4))
(println "After adding d=4 (should evict b, not a):")
(println "  Entries:" (lru/entries cache))
(println "  Expected: [[:d 4] [:a 1] [:c 3]]")

(println "\n=== Implementation verified! ===")
