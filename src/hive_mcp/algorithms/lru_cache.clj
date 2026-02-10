(ns hive-mcp.algorithms.lru-cache
  "LRU cache — implement from scratch.
  
  Implementation details:
  - O(1) get and put operations
  - Uses map + doubly-linked list approach
  - Capacity-based eviction of least recently used items
  
  This is a pure Clojure implementation using immutable data structures.")

;; Define the LRU cache as a record
(defrecord LRUCache [capacity size head tail node-map])

(defn- create-node
  "Create a new node for the linked list"
  [key value prev next]
  {:key key :value value :prev prev :next next})

(defn- remove-node
  "Remove a node from the linked list"
  [cache node]
  (if (nil? node)
    cache
    (let [{:keys [key prev next]} node
          node-map (:node-map cache)
          
          ;; Update previous node's next pointer
          node-map' (if prev
                      (update node-map prev assoc :next next)
                      node-map)
          
          ;; Update next node's prev pointer
          node-map'' (if next
                       (update node-map' next assoc :prev prev)
                       node-map')
          
          ;; Remove the node from the map
          node-map''' (dissoc node-map'' key)
          
          ;; Update head and tail
          new-head (if (= node (:head cache)) next (:head cache))
          new-tail (if (= node (:tail cache)) prev (:tail cache))]
      
      (->LRUCache (:capacity cache)
                  (dec (:size cache))
                  new-head
                  new-tail
                  node-map'''))))

(defn- add-to-head
  "Add a node to the head of the linked list (most recently used)"
  [cache node]
  (let [{:keys [head node-map]} cache
        node' (assoc node :next head :prev nil)
        node-map' (assoc node-map (:key node') node')
        
        ;; Update old head's prev pointer if it exists
        node-map'' (if head
                     (update node-map' head assoc :prev node')
                     node-map')
        
        ;; Update tail if this is the first node
        new-tail (if (nil? head) node' (:tail cache))]
    
    (->LRUCache (:capacity cache)
                (inc (:size cache))
                node'
                new-tail
                node-map'')))

(defn- move-to-head
  "Move an existing node to the head of the list"
  [cache node]
  (if (or (nil? node) (= node (:head cache)))
    cache
    (-> cache
        (remove-node node)
        (add-to-head node))))

(defn create
  "Create a new LRU cache with the given capacity.
  
  Args:
    capacity: maximum number of items the cache can hold
  
  Returns:
    LRUCache instance
  
  Example:
    (create 3)  ; creates an LRU cache that can hold up to 3 items"
  [capacity]
  {:pre [(pos-int? capacity)]}
  (->LRUCache capacity 0 nil nil {}))

(defn cache-get
  "Get the value associated with a key from the cache.
   If the key exists, the item becomes most recently used.
  
  Args:
    cache: LRUCache instance
    key: key to look up
  
  Returns:
    A tuple [value updated-cache] where value is the associated value or nil,
    and updated-cache is the cache with LRU order updated
  
  Example:
    (let [[value cache'] (cache-get cache :my-key)] ...)"
  [cache key]
  (if-let [node (get (:node-map cache) key)]
    [(:value node) (move-to-head cache node)]
    [nil cache]))

(defn put
  "Insert or update a key-value pair in the cache.
   If the key already exists, update its value and make it most recently used.
   If the key doesn't exist and cache is at capacity, evict LRU item first.
  
  Args:
    cache: LRUCache instance
    key: key to insert/update
    value: value to associate with the key
  
  Returns:
    Updated LRUCache instance
  
  Example:
    (let [cache' (put cache :key1 \"value1\")] ...)"
  [cache key value]
  (if-let [existing-node (get (:node-map cache) key)]
    ;; Update existing node
    (let [cache' (assoc-in cache [:node-map key :value] value)]
      (move-to-head cache' existing-node))
    ;; Create new node
    (let [new-node (create-node key value nil nil)
          cache' (if (>= (:size cache) (:capacity cache))
                   (remove-node cache (:tail cache))
                   cache)]
      (add-to-head cache' new-node))))

(defn evict
  "Evict a specific key from the cache.
  
  Args:
    cache: LRUCache instance
    key: key to evict
  
  Returns:
    Updated LRUCache instance
  
  Example:
    (let [cache' (evict cache :old-key)] ...)"
  [cache key]
  (if-let [node (get (:node-map cache) key)]
    (remove-node cache node)
    cache))

(defn entries
  "Get all key-value pairs in the cache in order from most to least recently used.
  
  Args:
    cache: LRUCache instance
  
  Returns:
    Sequence of [key value] pairs
  
  Example:
    (entries cache)
    ;=> [[:key3 \"value3\"] [:key2 \"value2\"] [:key1 \"value1\"]]"
  [cache]
  (loop [node (:head cache)
         result []]
    (if node
      (recur (:next node) (conj result [(:key node) (:value node)]))
      result)))

(defn size
  "Get the current number of items in the cache.
  
  Args:
    cache: LRUCache instance
  
  Returns:
    Number of items currently in the cache
  
  Example:
    (size cache)
    ;=> 2"
  [cache]
  (:size cache))

(defn capacity
  "Get the maximum capacity of the cache.
  
  Args:
    cache: LRUCache instance
  
  Returns:
    Maximum number of items the cache can hold
  
  Example:
    (capacity cache)
    ;=> 3"
  [cache]
  (:capacity cache))

(defn clear
  "Clear all items from the cache.
  
  Args:
    cache: LRUCache instance
  
  Returns:
    Empty LRUCache instance with same capacity
  
  Example:
    (clear cache)"
  [cache]
  (->LRUCache (:capacity cache) 0 nil nil {}))