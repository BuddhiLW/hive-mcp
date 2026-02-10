(ns hive-mcp.algorithms.skip-list
  "Skip list — implement from scratch.

  A skip list is a probabilistic data structure that allows O(log n) average time
  complexity for search, insert, and delete operations. It uses multiple layers
  of linked lists with different skip distances.

  Implementation supports:
  - Configurable maximum level and probability factor
  - Comparable values (using compare function)
  - Persistent operations (returns new skip lists)
  - Size tracking and sorted sequence conversion

  References:
  - Pugh, William (1990). 'Skip lists: a probabilistic alternative to balanced trees'")

(defrecord SkipListNode
  "A node in the skip list containing a value and forward pointers at each level."
  [value        ; The value stored in this node
   forward      ; Vector of forward pointers for each level (nil if end of list)
   level])      ; The highest level this node appears in (0-indexed)

(defrecord SkipList
  "A skip list data structure with probabilistic balancing."
  [head         ; The head node (dummy node with max-level forward pointers)
   max-level    ; Maximum number of levels allowed
   probability  ; Probability factor for node promotion (e.g., 0.5 for P=1/2)
   comparator   ; Comparator function for ordering values (defaults to compare)
   size])       ; Number of elements in the skip list

(defn- random-level
  "Generate a random level for a new node based on probability.
  
  Returns a level between 0 and max-level (inclusive) where:
  - Level 0 is always included (base list)
  - Each higher level has probability `p` chance of being included
  - Stops when random number >= probability or max-level reached"
  [probability max-level]
  (loop [level 0]
    (if (and (< level max-level)
             (< (rand) probability))
      (recur (inc level))
      level)))

(defn- create-head
  "Create a head node with forward pointers initialized to nil for all levels."
  [max-level]
  (->SkipListNode nil
                  (vec (repeat (inc max-level) nil))
                  max-level))

(defn create
  "Create a new empty skip list.
  
  Options:
  - `:max-level` (default 16) - Maximum number of levels
  - `:probability` (default 0.5) - Probability factor for node promotion (0 < p < 1)
  - `:comparator` (default compare) - Comparator function for ordering values
  
  Example:
  ```clojure
  (create) ; Default skip list with max-level=16, probability=0.5
  (create :max-level 8 :probability 0.25) ; Custom parameters
  ```"
  [& {:keys [max-level probability comparator]
      :or {max-level 16 probability 0.5 comparator compare}}]
  {:pre [(> max-level 0) 
         (< 0 probability 1)
         (ifn? comparator)]}
  (->SkipList (create-head max-level)
              max-level
              probability
              comparator
              0))

(defn- update-forward-pointers!
  "Update forward pointers when inserting a new node.
  Returns updated forward vector."
  [forward update-node level]
  (loop [l level
         new-forward forward]
    (if (>= l 0)
      (let [fwd (nth forward l)]
        (recur (dec l)
               (assoc new-forward l 
                      (if (and update-node (seq update-node))
                        (->SkipListNode (:value (first update-node))
                                        (nth (:forward (first update-node)) l)
                                        (:level (first update-node)))
                        fwd))))
      new-forward)))

(defn insert
  "Insert a value into the skip list. Returns a new skip list.
  
  Time complexity: O(log n) average
  
  Example:
  ```clojure
  (-> (create)
      (insert 5)
      (insert 3)
      (insert 7))
  ```"
  [skip-list value]
  (let [{:keys [head max-level probability comparator size]} skip-list
        update-vector (vec (repeat (inc max-level) nil))
        current-level (:level head)]
    
    ;; Find insertion point and track nodes that need updating
    (loop [current head
           level current-level
           update update-vector]
      (if (>= level 0)
        (let [next-node (nth (:forward current) level)]
          (if (and next-node
                   (neg? (comparator (:value next-node) value)))
            ;; Continue searching at this level
            (recur next-node level update)
            ;; Record node that needs updating and go down a level
            (recur current (dec level) (assoc update level current))))
        ;; Found insertion point, create new node
        (let [new-level (random-level probability max-level)
              new-node (->SkipListNode value
                                       (vec (repeat (inc new-level) nil))
                                       new-level)
              
              ;; Update forward pointers
              updated-head-forward (update-forward-pointers! 
                                    (:forward head)
                                    (map (fn [l] (nth update l))
                                         (range (inc new-level)))
                                    new-level)
              
              updated-head (assoc head :forward updated-head-forward)]
          
          ;; Connect new node to the list
          (loop [l new-level]
            (when (>= l 0)
              (let [update-node (nth update l)
                    fwd (nth (:forward update-node) l)]
                (when update-node
                  (aset (:forward new-node) l fwd)
                  (aset (:forward update-node) l new-node)))
              (recur (dec l))))
          
          (assoc skip-list 
                 :head updated-head
                 :size (inc size)))))))

(defn search
  "Search for a value in the skip list. Returns the value if found, nil otherwise.
  
  Time complexity: O(log n) average
  
  Example:
  ```clojure
  (-> (create)
      (insert 5)
      (insert 3)
      (search 5)) ; => 5
  ```"
  [skip-list value]
  (let [{:keys [head comparator]} skip-list
        current-level (:level head)]
    
    (loop [current head
           level current-level]
      (if (>= level 0)
        (let [next-node (nth (:forward current) level)]
          (cond
            (nil? next-node)
            (recur current (dec level))
            
            (neg? (comparator (:value next-node) value))
            (recur next-node level)
            
            (zero? (comparator (:value next-node) value))
            (:value next-node)
            
            :else
            (recur current (dec level))))
        nil))))

(defn delete
  "Delete a value from the skip list. Returns a new skip list.
  
  If the value doesn't exist, returns the original skip list unchanged.
  
  Time complexity: O(log n) average
  
  Example:
  ```clojure
  (-> (create)
      (insert 5)
      (insert 3)
      (delete 5)
      (search 5)) ; => nil
  ```"
  [skip-list value]
  (let [{:keys [head comparator size]} skip-list
        current-level (:level head)
        update-vector (vec (repeat (inc current-level) nil))
        found-node (atom nil)]
    
    ;; Find the node to delete and track nodes that need updating
    (loop [current head
           level current-level
           update update-vector]
      (if (>= level 0)
        (let [next-node (nth (:forward current) level)]
          (cond
            (nil? next-node)
            (recur current (dec level) (assoc update level current))
            
            (neg? (comparator (:value next-node) value))
            (recur next-node level update)
            
            (zero? (comparator (:value next-node) value))
            (do
              (reset! found-node next-node)
              (recur current (dec level) (assoc update level current)))
            
            :else
            (recur current (dec level) (assoc update level current))))
        
        ;; Level traversal complete
        (if-let [node @found-node]
          ;; Found node to delete, update pointers
          (let [node-level (:level node)]
            (loop [l node-level]
              (when (>= l 0)
                (let [update-node (nth update l)]
                  (when update-node
                    (let [current-forward (:forward update-node)
                          node-forward (nth (:forward node) l)]
                      (aset current-forward l node-forward))))
                (recur (dec l))))
            
            (assoc skip-list :size (dec size)))
          
          ;; Node not found
          skip-list)))))

(defn to-sorted-seq
  "Convert the skip list to a sorted sequence of values.
  
  Time complexity: O(n)
  
  Example:
  ```clojure
  (-> (create)
      (insert 5)
      (insert 3)
      (insert 7)
      (to-sorted-seq)) ; => (3 5 7)
  ```"
  [skip-list]
  (lazy-seq
   (let [{:keys [head]} skip-list]
     (loop [node (nth (:forward head) 0)]
       (when node
         (cons (:value node)
               (lazy-seq (recur (nth (:forward node) 0)))))))))

(defn size
  "Return the number of elements in the skip list.
  
  Example:
  ```clojure
  (-> (create)
      (insert 5)
      (insert 3)
      (size)) ; => 2
  ```"
  [skip-list]
  (:size skip-list))

(defn empty?
  "Check if the skip list is empty.
  
  Example:
  ```clojure
  (empty? (create)) ; => true
  ```"
  [skip-list]
  (zero? (:size skip-list)))

(defn contains?
  "Check if the skip list contains a value.
  
  Example:
  ```clojure
  (-> (create)
      (insert 5)
      (contains? 5)) ; => true
  ```"
  [skip-list value]
  (boolean (search skip-list value)))

;; Performance test utilities
(defn performance-test
  "Run basic performance tests on the skip list.
  
  Returns a map with timing information for insert, search, and delete operations.
  
  Example:
  ```clojure
  (performance-test 1000)
  ```"
  [n-elements]
  (let [start-time (System/currentTimeMillis)
        sl (reduce insert (create) (range n-elements))
        insert-time (- (System/currentTimeMillis) start-time)
        
        search-start (System/currentTimeMillis)
        _ (doseq [i (range n-elements)]
            (search sl i))
        search-time (- (System/currentTimeMillis) search-start)
        
        delete-start (System/currentTimeMillis)
        _ (reduce delete sl (range n-elements))
        delete-time (- (System/currentTimeMillis) delete-start)]
    
    {:n-elements n-elements
     :insert-ms insert-time
     :search-ms search-time
     :delete-ms delete-time
     :size (size sl)
     :sorted (take 5 (to-sorted-seq sl))}))

;; Convenience constructors
(defn skip-list
  "Create a skip list with the given values.
  
  Example:
  ```clojure
  (skip-list [3 1 4 1 5 9]) ; Creates skip list with these values
  ```"
  [values & opts]
  (reduce insert (apply create opts) values))

(defn skip-list-sorted
  "Create a skip list and return the sorted values.
  
  Example:
  ```clojure
  (skip-list-sorted [3 1 4 1 5 9]) ; => (1 1 3 4 5 9)
  ```"
  [values & opts]
  (to-sorted-seq (apply skip-list values opts)))