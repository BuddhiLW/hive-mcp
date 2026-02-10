(ns hive-mcp.server
  "Facade for backward compatibility.
   Delegates to hive-mcp.server.core after tree normalization.
   Required by bb-mcp dynamic tool forwarding which resolves
   server-context-atom via ns-resolve."
  (:require [hive-mcp.server.core :as core]))

;; Alias the atom itself (not its value) so ns-resolve returns
;; a Var whose deref yields the same atom as core's private var.
(def server-context-atom (deref #'core/server-context-atom))

(def -main core/-main)
