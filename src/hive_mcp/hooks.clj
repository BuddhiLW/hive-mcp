(ns hive-mcp.hooks
  "Facade for backward compatibility.
   Delegates to hive-mcp.hooks.core after tree normalization."
  (:require [hive-mcp.hooks.core :as core]))

(def hook-events core/hook-events)
(def create-registry core/create-registry)
(def register-hook core/register-hook)
(def trigger-hooks core/trigger-hooks)
