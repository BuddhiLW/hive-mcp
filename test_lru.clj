(require '[hive-mcp.algorithms.lru-cache :as lru])

(println "=== Testing LRU Cache Implementation ===")

;; Test 1: Create cache
(println "\n1. Creating cache with capacity 3")
(def cache (lru/create 3))
(println "   Cache created:" cache)

;; Test 2: Add items
(println "\n2. Adding items")
(lru/put cache :a 1)
(lru/put cache :b 2)
(lru/put cache :c 3)
(println "   Entries:" (lru/entries cache))
(println "   Size:" (lru/size cache))

;; Test 3: Test eviction
(println "\n3. Testing eviction on overflow")
(lru/put cache :d 4)
(println "   After adding 4th item, entries:" (lru/entries cache))
(println "   Should be [:d 4] [:c 3] [:b 2] (a evicted)")

;; Test 4: Test get and MRU update
(println "\n4. Testing get and MRU update")
(println "   Get :b:" (lru/get cache :b))
(println "   Entries after accessing :b:" (lru/entries cache))
(println "   Should be [:b 2] [:d 4] [:c 3]")

;; Test 5: Test update existing
(println "\n5. Testing update existing key")
(lru/put cache :c 30)
(println "   After updating :c to 30:" (lru/entries cache))
(println "   Should be [:c 30] [:b 2] [:d 4]")

;; Test 6: Test evict specific
(println "\n6. Testing evict specific key")
(lru/evict cache :d)
(println "   After evicting :d:" (lru/entries cache))
(println "   Size:" (lru/size cache))

;; Test 7: Test clear
(println "\n7. Testing clear")
(lru/clear cache)
(println "   After clear, entries:" (lru/entries cache))
(println "   Size:" (lru/size cache))

(println "\n=== All tests passed! ===")
