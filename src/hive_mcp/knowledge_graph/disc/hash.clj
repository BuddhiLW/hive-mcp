(ns hive-mcp.knowledge-graph.disc.hash
  "Hash computation utilities for disc entities."
  (:require [clojure.java.io :as io]
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

(defn file-content-hash
  "Read file and compute content hash.
   Returns {:hash \"..\" :exists? true} or {:exists? false}."
  [path]
  (try
    (let [file (io/file path)]
      (if (.exists file)
        {:hash (compute-hash (slurp file)) :exists? true}
        {:exists? false}))
    (catch Exception e
      (log/warn "Failed to hash file" {:path path :error (.getMessage e)})
      {:exists? false :error (.getMessage e)})))
