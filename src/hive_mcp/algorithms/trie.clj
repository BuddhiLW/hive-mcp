(ns hive-mcp.algorithms.trie
  "Prefix trie for efficient string lookup, prefix search, and autocomplete.

   A trie (prefix tree) stores strings character-by-character in a tree structure.
   Shared prefixes share nodes, giving O(k) lookup where k = key length.

   Operations:
   - insert    O(k) — add a word
   - delete    O(k) — remove a word, pruning empty branches
   - search    O(k) — exact membership check
   - prefix?   O(k) — check if any word starts with prefix
   - complete  O(k + m) — autocomplete all words with prefix (m = subtree size)
   - words     O(n) — return all stored words as sorted vector (n = number of words)")

;;; ============================================================
;;; Data Structure
;;; ============================================================

(def empty-trie
  "An empty trie node. Children keyed by character."
  {:children {} :end? false})

;;; ============================================================
;;; Core Operations
;;; ============================================================

(defn insert
  "Insert a word into the trie. Returns updated trie."
  [trie word]
  (if (empty? word)
    (assoc trie :end? true)
    (let [c (first word)]
      (update-in trie [:children c]
                 #(insert (or % empty-trie) (subs word 1))))))

(defn delete
  "Remove a word from the trie, pruning empty branches. Returns updated trie."
  [trie word]
  (if (empty? word)
    (if-not (:end? trie)
      trie
      (assoc trie :end? false))
    (let [c (first word)
          child (get (:children trie) c)]
      (if-not child
        trie
        (let [new-child (delete child (subs word 1))]
          (if (and (not (:end? new-child)) (empty? (:children new-child)))
            (update trie :children dissoc c)
            (assoc-in trie [:children c] new-child)))))))

(defn search
  "Check if exact word exists in the trie."
  [trie word]
  (if (empty? word)
    (:end? trie false)
    (when-let [child (get-in trie [:children (first word)])]
      (search child (subs word 1)))))

(defn prefix?
  "Check if any word in the trie starts with the given prefix."
  [trie prefix]
  (if (empty? prefix)
    true
    (when-let [child (get-in trie [:children (first prefix)])]
      (prefix? child (subs prefix 1)))))

;;; ============================================================
;;; Traversal
;;; ============================================================

(defn- find-node
  "Navigate to the trie node at the end of prefix. Returns nil if not found."
  [trie prefix]
  (if (empty? prefix)
    trie
    (when-let [child (get-in trie [:children (first prefix)])]
      (find-node child (subs prefix 1)))))

(defn- collect-words
  "Collect all words reachable from a node, prepending the accumulated prefix."
  [node prefix]
  (let [here (when (:end? node) [prefix])]
    (into (vec here)
          (mapcat (fn [[c child]]
                    (collect-words child (str prefix c))))
          (:children node))))

(defn complete
  "Return all words in the trie that start with prefix."
  [trie prefix]
  (when-let [node (find-node trie prefix)]
    (collect-words node prefix)))

(defn words
  "Return all words in the trie as a sorted vector."
  [trie]
  (sort (collect-words trie "")))

;;; ============================================================
;;; Bulk Operations
;;; ============================================================

(defn build
  "Build a trie from a collection of words."
  [words]
  (reduce insert empty-trie words))

(defn size
  "Count the number of words stored in the trie."
  [trie]
  (let [here (if (:end? trie) 1 0)]
    (reduce-kv (fn [acc _c child] (+ acc (size child)))
               here
               (:children trie))))