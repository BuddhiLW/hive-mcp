(ns hive-mcp.knowledge-graph.drift
  "Drift detection for Knowledge Graph entries.
  
   Detects when source files have changed since knowledge was abstracted,
   indicating the knowledge may be stale and needs re-grounding.
   
   See ADR: Knowledge Graph as Structural Differential."
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log])
  (:import [java.security MessageDigest]))

(defn compute-source-hash
  "Compute SHA-256 hash of content string.
   Returns hex string of the hash."
  [content]
  (when content
    (let [md (MessageDigest/getInstance "SHA-256")
          hash-bytes (.digest md (.getBytes (str content) "UTF-8"))]
      (apply str (map #(format "%02x" %) hash-bytes)))))

(defn detect-drift
  "Check if a knowledge entry has drifted from its source.
   
   Arguments:
     entry - Map with :knowledge/source-hash and :knowledge/grounded-from
   
   Returns:
     {:drifted? boolean
      :reason keyword or nil
      :stored-hash string
      :current-hash string or nil}
   
   Reasons:
     :hash-mismatch - File content changed
     :source-missing - Source file no longer exists
     :no-grounding - Entry has no grounding info"
  [entry]
  (let [stored-hash (:knowledge/source-hash entry)
        source-path (:knowledge/grounded-from entry)]
    (cond
      (nil? source-path)
      {:drifted? false :reason :no-grounding :stored-hash stored-hash}

      (not (.exists (io/file source-path)))
      (do
        (log/debug "Drift detected: source missing" source-path)
        {:drifted? true :reason :source-missing :stored-hash stored-hash :current-hash nil})

      :else
      (let [current-content (slurp source-path)
            current-hash (compute-source-hash current-content)]
        {:drifted? (not= stored-hash current-hash)
         :reason (when (not= stored-hash current-hash) :hash-mismatch)
         :stored-hash stored-hash
         :current-hash current-hash}))))

(defn find-drifted-entries
  "Find all drifted entries from a collection.
   
   Arguments:
     entries - Sequence of knowledge entry maps
   
   Returns:
     Sequence of {:entry <original-entry> :drift-info <detect-drift-result>}
     for entries where :drifted? is true"
  [entries]
  (->> entries
       (map (fn [entry]
              {:entry entry
               :drift-info (detect-drift entry)}))
       (filter #(get-in % [:drift-info :drifted?]))))

(defn grounding-info
  "Create grounding metadata for a new knowledge entry.
   
   Arguments:
     source-path - File path the knowledge was derived from
     content - Optional content (if already loaded, avoids re-read)
   
   Returns:
     Map with :knowledge/source-hash, :knowledge/grounded-from, :knowledge/grounded-at"
  ([source-path]
   (grounding-info source-path nil))
  ([source-path content]
   (let [file-content (or content (when (.exists (io/file source-path)) (slurp source-path)))]
     (when file-content
       {:knowledge/source-hash (compute-source-hash file-content)
        :knowledge/grounded-from source-path
        :knowledge/grounded-at (java.util.Date.)}))))