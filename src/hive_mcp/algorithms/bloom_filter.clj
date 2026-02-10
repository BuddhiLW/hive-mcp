(ns hive-mcp.algorithms.bloom-filter
  "Bloom filter — implement from scratch.
   
   A Bloom filter is a space-efficient probabilistic data structure that is used
   to test whether an element is a member of a set. False positive matches are
   possible, but false negatives are not.
   
   Key properties:
   - add(element): Adds an element to the set
   - contains?(element): Returns true if element might be in the set, false if definitely not
   - estimated-count(): Returns approximate number of elements
   - merge(filter): Combines two Bloom filters (union)
   
   Implementation details:
   - Uses boolean array for bit storage
   - Uses multiple independent hash functions via double hashing technique
   - Configurable capacity and false-positive rate")

(defrecord BloomFilter
  [bit-array       ; boolean[] - the actual filter bits
   size            ; int - total number of bits in the filter
   hash-count      ; int - number of hash functions to use
   element-count   ; int - approximate number of elements added
   seed            ; long - random seed for hash function variations
   salt])          ; string - salt for hash function to prevent collisions

(defn- optimal-params
  "Calculate optimal Bloom filter parameters given desired capacity and false-positive rate.
   
   Returns a map with:
   - :size - optimal number of bits
   - :hash-count - optimal number of hash functions
   - :actual-fpr - actual false-positive rate with these parameters"
  [capacity false-positive-rate]
  (let [size (Math/ceil (/ (* capacity (Math/log false-positive-rate))
                           (* -1 (Math/pow (Math/log 2) 2))))
        hash-count (Math/ceil (* (/ size capacity) (Math/log 2)))
        actual-fpr (Math/pow (- 1 (Math/exp (/ (* (- hash-count) capacity) size))) hash-count)]
    {:size (long (max 64 size))  ; Minimum 64 bits
     :hash-count (long (max 1 hash-count))
     :actual-fpr actual-fpr}))

(defn- hash-functions
  "Generate k hash functions using double hashing technique.
   Returns a function that takes an element and returns k bit positions."
  [k size seed salt]
  (fn [element]
    (let [element-str (str element salt)
          ;; First hash using Java's hashCode with seed
          h1 (bit-xor (.hashCode element-str) (unchecked-int seed))
          ;; Second hash using a different algorithm
          h2 (-> element-str
               (.getBytes "UTF-8")
               (java.util.Arrays/hashCode)
               (bit-xor (unchecked-int (bit-not seed))))
          ;; Ensure non-negative hash values
          h1-pos (Math/abs (mod h1 size))
          h2-pos (Math/abs (mod h2 size))]
      ;; Generate k bit positions using double hashing: h(i) = (h1 + i * h2) mod size
      (mapv (fn [i]
              (mod (+ h1-pos (* i h2-pos)) size))
            (range k)))))

(defn create
  "Create a new Bloom filter with given capacity and desired false-positive rate.
   
   Args:
   - capacity: Expected maximum number of elements to store
   - false-positive-rate: Desired false positive probability (e.g., 0.01 for 1%)
   
   Returns: BloomFilter record
   
   Example:
   (create 1000 0.01) ; Bloom filter for ~1000 elements with 1% false positive rate"
  ([capacity false-positive-rate]
   (create capacity false-positive-rate nil))
  ([capacity false-positive-rate salt]
   (let [{:keys [size hash-count actual-fpr]} (optimal-params capacity false-positive-rate)
         bit-array (boolean-array size false)
         seed (System/nanoTime)
         salt (or salt (str "bloom-" seed))]
     (println (format "Created Bloom filter: capacity=%d, size=%d bits, hash-functions=%d, expected-fpr=%.4f"
                      capacity size hash-count actual-fpr))
     (->BloomFilter bit-array size hash-count 0 seed salt))))

(defn add
  "Add an element to the Bloom filter.
   
   Args:
   - bloom-filter: The BloomFilter to add to
   - element: Element to add (any type that can be stringified)
   
   Returns: Updated BloomFilter (immutable - returns new instance)"
  [{:keys [bit-array size hash-count element-count seed salt] :as bloom-filter} element]
  (let [new-bit-array (aclone bit-array)
        hash-fn (hash-functions hash-count size seed salt)
        positions (hash-fn element)]
    (doseq [pos positions]
      (aset-boolean new-bit-array pos true))
    (assoc bloom-filter
           :bit-array new-bit-array
           :element-count (inc element-count))))

(defn contains?
  "Check if an element might be in the Bloom filter.
   
   Args:
   - bloom-filter: The BloomFilter to check
   - element: Element to check
   
   Returns: true if element might be in the set, false if definitely not
   
   Note: May return false positives, but never false negatives."
  [{:keys [bit-array size hash-count seed salt]} element]
  (let [hash-fn (hash-functions hash-count size seed salt)
        positions (hash-fn element)]
    (every? #(aget bit-array %) positions)))

(defn estimated-count
  "Estimate the number of elements in the Bloom filter.
   
   Args:
   - bloom-filter: The BloomFilter
   
   Returns: Estimated count of elements
   
   Formula: n ≈ -m/k * ln(1 - p)
   where m = size, k = hash-count, p = proportion of bits set to 1"
  [{:keys [bit-array size hash-count]}]
  (let [set-bits (reduce (fn [cnt bit]
                           (if bit (inc cnt) cnt))
                         0
                         bit-array)
        bit-probability (/ set-bits size)
        ;; Avoid log(0) and log(1)
        log-arg (max Double/MIN_VALUE (min (- 1.0 Double/MIN_VALUE) (- 1.0 bit-probability)))]
    (long (Math/ceil (/ (* (- size) (Math/log log-arg)) hash-count)))))

(defn merge-filters
  "Merge two Bloom filters (union operation).
   
   Args:
   - filter1: First BloomFilter
   - filter2: Second BloomFilter
   
   Returns: New BloomFilter containing union of both sets, or nil if incompatible
   
   Requirements for merging:
   - Same size (number of bits)
   - Same hash-count (number of hash functions)
   - Same salt
   
   The resulting filter will have element-count = estimated union size."
  [{:keys [bit-array size hash-count seed salt] :as filter1}
   {:keys [bit-array2 size2 hash-count2 salt2] :as filter2}]
  (when (and (= size size2)
             (= hash-count hash-count2)
             (= salt salt2))
    (let [new-bit-array (boolean-array size)
          ;; Combine bits using OR operation
          _ (dotimes [i size]
              (aset-boolean new-bit-array i
                           (or (aget bit-array i)
                               (aget bit-array2 i))))
          ;; Estimate union size
          union-count (estimated-count
                       (->BloomFilter new-bit-array size hash-count 0 seed salt))]
      (->BloomFilter new-bit-array size hash-count union-count seed salt))))

(defn false-positive-probability
  "Calculate the current false positive probability of the Bloom filter.
   
   Args:
   - bloom-filter: The BloomFilter
   
   Returns: Current estimated false positive rate
   
   Formula: (1 - e^(-k*n/m))^k
   where k = hash-count, n = estimated-count, m = size"
  [{:keys [size hash-count] :as bloom-filter}]
  (let [n (estimated-count bloom-filter)
        exponent (/ (* (- hash-count) n) size)
        base (- 1.0 (Math/exp exponent))]
    (Math/pow base hash-count)))

(defn bits-set-percentage
  "Get the percentage of bits set to 1 in the Bloom filter.
   
   Args:
   - bloom-filter: The BloomFilter
   
   Returns: Percentage (0.0 to 100.0) of bits set"
  [{:keys [bit-array size]}]
  (let [set-bits (reduce (fn [cnt bit]
                           (if bit (inc cnt) cnt))
                         0
                         bit-array)]
    (* 100.0 (/ set-bits size))))

(defn clear
  "Clear the Bloom filter (reset all bits to 0).
   
   Args:
   - bloom-filter: The BloomFilter to clear
   
   Returns: New cleared BloomFilter"
  [{:keys [size hash-count seed salt] :as bloom-filter}]
  (let [new-bit-array (boolean-array size false)]
    (assoc bloom-filter
           :bit-array new-bit-array
           :element-count 0)))

;; Convenience functions for common use cases

(defn create-with-defaults
  "Create a Bloom filter with reasonable defaults.
   
   Args:
   - capacity: Expected maximum number of elements
   
   Returns: BloomFilter with 1% false positive rate"
  [capacity]
  (create capacity 0.01))

(defn bulk-add
  "Add multiple elements to a Bloom filter efficiently.
   
   Args:
   - bloom-filter: Initial BloomFilter
   - elements: Collection of elements to add
   
   Returns: Updated BloomFilter"
  [bloom-filter elements]
  (reduce add bloom-filter elements))

(defn contains-any?
  "Check if any of the given elements might be in the Bloom filter.
   
   Args:
   - bloom-filter: The BloomFilter to check
   - elements: Collection of elements to check
   
   Returns: true if any element might be in the set"
  [bloom-filter elements]
  (some #(contains? bloom-filter %) elements))

(defn contains-all?
  "Check if all of the given elements might be in the Bloom filter.
   
   Args:
   - bloom-filter: The BloomFilter to check
   - elements: Collection of elements to check
   
   Returns: true if all elements might be in the set"
  [bloom-filter elements]
  (every? #(contains? bloom-filter %) elements))