(ns hive-mcp.tools.memory.core
  "Core utilities for memory tool handlers.
   
   SOLID: SRP - Centralized validation and error handling.
   CLARITY: Y - Yield safe failure with graceful error messages."
  (:require [hive-mcp.tools.core :refer [mcp-error]]
            [hive-mcp.chroma :as chroma]))

(defmacro with-chroma
  "Execute body with Chroma validation and error handling.
   Returns MCP error response if Chroma not configured or on exception.
   
   Usage:
     (with-chroma
       (let [result (chroma/query ...)]
         (mcp-success result)))
   
   CLARITY: Y - Yield safe failure (graceful error messages)
   SOLID: SRP - Centralizes Chroma validation/error handling"
  [& body]
  `(if-not (chroma/embedding-configured?)
     (mcp-error "Chroma not configured")
     (try
       ~@body
       (catch Exception e#
         (mcp-error (ex-message e#))))))

(defmacro with-entry
  "Execute body with entry lookup, handling not-found case.
   Binds the entry to `entry-sym` if found.
   
   Usage:
     (with-entry [entry id]
       (do-something-with entry))
   
   CLARITY: R - Represented intent for entry operations
   SOLID: DRY - Eliminates repeated entry lookup + not-found pattern"
  [[entry-sym id-expr] & body]
  `(with-chroma
     (if-let [~entry-sym (chroma/get-entry-by-id ~id-expr)]
       (do ~@body)
       (mcp-error (str "Entry not found: " ~id-expr)))))
