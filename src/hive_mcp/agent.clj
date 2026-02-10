(ns hive-mcp.agent
  "Facade for backward compatibility.
   Delegates to hive-mcp.agent.core after tree normalization."
  (:require [hive-mcp.agent.core :as core]))

(def delegate-drone! core/delegate-drone!)
(def delegate-agentic-drone! core/delegate-agentic-drone!)
(def tools core/tools)
(def register-tools! core/register-tools!)
