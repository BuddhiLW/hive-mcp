(ns hive-mcp.test-fixtures
  "Test fixtures and doubles for hive-mcp tests.

   Contains test infrastructure that should not pollute production code.
   Pattern: Protocol test doubles in test scope only."
  (:require [hive-mcp.chroma.core :as chroma]))

;;; ============================================================
;;; Mock Embedding Provider (for testing)
;;; ============================================================

(defrecord MockEmbedder [dimension]
  chroma/EmbeddingProvider
  (embed-text [_ text]
    ;; Generate deterministic pseudo-random embedding based on text hash
    (let [h (hash text)]
      (vec (for [i (range dimension)]
             (-> (bit-xor h i)
                 (mod 1000)
                 (/ 1000.0)
                 (* 2)
                 (- 1))))))
  (embed-batch [this texts]
    (mapv #(chroma/embed-text this %) texts))
  (embedding-dimension [_] dimension))

(defn ->MockEmbedder
  "Create a mock embedder for testing (not for production use).
   Generates deterministic embeddings based on text hash."
  ([] (->MockEmbedder 384))
  ([dimension] (MockEmbedder. dimension)))
