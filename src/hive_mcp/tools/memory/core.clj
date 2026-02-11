(ns hive-mcp.tools.memory.core
  "Core utilities and macros for memory tool handlers."
  (:require [hive-mcp.tools.core :refer [mcp-error]]
            [hive-mcp.chroma :as chroma]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(defmacro with-chroma
  "Execute body with Chroma validation and error handling."
  [& body]
  `(if-not (chroma/embedding-configured?)
     (mcp-error "Chroma not configured")
     (try
       ~@body
       (catch Exception e#
         (mcp-error (ex-message e#))))))

(defmacro with-entry
  "Execute body with entry lookup, handling not-found case."
  [[entry-sym id-expr] & body]
  `(with-chroma
     (if-let [~entry-sym (chroma/get-entry-by-id ~id-expr)]
       (do ~@body)
       (mcp-error (str "Entry not found: " ~id-expr)))))
