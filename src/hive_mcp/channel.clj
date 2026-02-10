(ns hive-mcp.channel
  "Facade for backward compatibility.
   Delegates to hive-mcp.channel.core after tree normalization."
  (:require [hive-mcp.channel.core :as core]))

(def start-server! core/start-server!)
(def stop-server! core/stop-server!)
(def force-stop-server! core/force-stop-server!)
(def broadcast! core/broadcast!)
(def publish! core/publish!)
(def emit-event! core/emit-event!)
(def subscribe! core/subscribe!)
(def unsubscribe! core/unsubscribe!)
(def client-count core/client-count)
(def server-connected? core/server-connected?)
(def channel-tools core/channel-tools)
(def mark-coordinator-running! core/mark-coordinator-running!)
