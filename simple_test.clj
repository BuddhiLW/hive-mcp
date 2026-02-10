;; Simple test for LRU cache
(require '[hive-mcp.algorithms.lru-cache :as lru])

(println "Testing LRU Cache Implementation")

;; Create cache
(def cache (lru/create 3))
(println "Created cache with capacity 3")

;; Test 1: Add items
(lru/put cache :a 1)
(lru/put cache :b 2)
(lru/put cache :c 3)
(println "After adding 3 items:")
(println "  Entries:" (lru/entries cache))
(println "  Size:" (lru/size cache))

;; Test 2: Add 4th item (should evict :a)
(lru/put cache :d 4)
(println "After adding 4th item (d=4):")
(println "  Entries:" (lru/entries cache))
(println "  Size:" (lru/size cache))

;; Test 3: Get item (should move to MRU)
(println "Get :b:" (lru/get cache :b))
(println "  Entries after get:" (lru/entries cache))

;; Test 4: Update existing item
(lru/put cache :c 30)
(println "After updating :c to 30:")
(println "  Entries:" (lru/entries cache))

;; Test 5: Clear
(lru/clear cache)
(println "After clear:")
(println "  Entries:" (lru/entries cache))
(println "  Size:" (lru/size cache))

(println "\nAll tests completed!")
