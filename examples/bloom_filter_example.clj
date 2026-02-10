(ns bloom-filter-example
  (:require [hive-mcp.algorithms.bloom-filter :as bf]))

;; Example 1: Basic usage
(println "=== Example 1: Basic Bloom Filter ===")
(let [filter (bf/create 1000 0.01)  ; Create filter for 1000 elements with 1% FPR
      filter' (-> filter
                  (bf/add "apple")
                  (bf/add "banana")
                  (bf/add "cherry"))]
  
  (println "Contains 'apple':" (bf/contains? filter' "apple"))    ; true
  (println "Contains 'banana':" (bf/contains? filter' "banana"))  ; true
  (println "Contains 'durian':" (bf/contains? filter' "durian"))  ; false (probably)
  (println "Estimated count:" (bf/estimated-count filter'))       ; ~3
  (println "False positive rate:" (bf/false-positive-probability filter')))

;; Example 2: Bulk operations
(println "\n=== Example 2: Bulk Operations ===")
(let [usernames ["alice" "bob" "charlie" "david" "eve"]
      filter (bf/bulk-add (bf/create-with-defaults 100) usernames)]
  
  (println "Check multiple users:")
  (println "Contains any of [\"alice\" \"frank\"]:" (bf/contains-any? filter ["alice" "frank"]))
  (println "Contains all of [\"alice\" \"bob\"]:" (bf/contains-all? filter ["alice" "bob"]))
  (println "Bits set percentage:" (bf/bits-set-percentage filter)))

;; Example 3: Filter merging (union)
(println "\n=== Example 3: Filter Merging ===")
(let [filter1 (-> (bf/create 100 0.01 "same-salt")
                  (bf/add "item1")
                  (bf/add "item2"))
      filter2 (-> (bf/create 100 0.01 "same-salt")
                  (bf/add "item2")
                  (bf/add "item3"))
      merged (bf/merge-filters filter1 filter2)]
  
  (if merged
    (do
      (println "Merged filter created successfully")
      (println "Contains 'item1':" (bf/contains? merged "item1"))
      (println "Contains 'item2':" (bf/contains? merged "item2"))
      (println "Contains 'item3':" (bf/contains? merged "item3")))
    (println "Filters cannot be merged (different parameters)")))

;; Example 4: Performance and statistics
(println "\n=== Example 4: Performance Monitoring ===")
(let [n 10000
      filter (bf/create n 0.001)  ; Very low false positive rate
      ;; Add many elements
      filter' (reduce bf/add filter (map str (range n)))]
  
  (println "Added" n "elements")
  (println "Estimated count:" (bf/estimated-count filter'))
  (println "Actual bits set:" (bf/bits-set-percentage filter') "%")
  (println "Theoretical FPR: 0.001")
  (println "Actual FPR:" (bf/false-positive-probability filter')))

;; Example 5: Use case - URL deduplication
(println "\n=== Example 5: URL Deduplication ===")
(let [visited-urls (bf/create 100000 0.001)  ; Track up to 100K URLs
      urls ["https://example.com/page1"
            "https://example.com/page2"
            "https://example.com/page1"  ; Duplicate
            "https://example.com/page3"]]
  
  (doseq [url urls]
    (if (bf/contains? visited-urls url)
      (println "Already visited:" url)
      (do
        (println "First visit:" url)
        (bf/add visited-urls url)))))