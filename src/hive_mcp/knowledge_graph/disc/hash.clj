(ns hive-mcp.knowledge-graph.disc.hash
  "Hash computation utilities for disc entities."
  (:require [clojure.java.io :as io]
            [hive-dsl.result :as r]
            [taoensso.timbre :as log])
  (:import [java.security MessageDigest]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn compute-hash
  "Compute SHA-256 hash of content string.
   Returns hex string."
  [content]
  (let [md (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest md (.getBytes (str content) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) hash-bytes))))

;; ── mtime-keyed hash cache ──────────────────────────────────────────
;; Keyed by [path mtime-ms]. Only recomputes hash when file mtime changes.
;; Prevents redundant SHA-256 reads for unchanged files across staleness scans.
(def ^:private hash-cache
  "Cache of {[path mtime-ms] {:hash \"..\" :exists? true}}."
  (atom {}))

(defn clear-hash-cache!
  "Clear the mtime-keyed hash cache. Useful for testing or memory pressure."
  []
  (reset! hash-cache {}))

(defn file-content-hash
  "Read file and compute content hash with mtime-based caching.
   Returns {:hash \"..\" :exists? true} or {:exists? false}.
   Caches by [path mtime] — skips SHA-256 if file unchanged since last call."
  [path]
  (let [result (r/guard Exception {:exists? false}
                        (let [file (io/file path)]
                          (if (.exists file)
                            (let [mtime (.lastModified file)
                                  cache-key [path mtime]]
                              (if-let [cached (get @hash-cache cache-key)]
                                cached
                                (let [entry {:hash (compute-hash (slurp file)) :exists? true}]
                                  (swap! hash-cache assoc cache-key entry)
                                  entry)))
                            {:exists? false})))]
    (when-let [err (::r/error (meta result))]
      (log/warn "Failed to hash file" {:path path :error (:message err)}))
    result))
