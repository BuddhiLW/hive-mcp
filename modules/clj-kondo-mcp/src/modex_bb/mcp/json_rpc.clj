(ns modex-bb.mcp.json-rpc
  (:require [modex-bb.mcp.schema :as schema :refer [json-rpc-version]]))

(defn result [id result]
  {:jsonrpc json-rpc-version
   :id      id
   :result  result})

(defn error [id error]
  {:jsonrpc json-rpc-version
   :id      id
   :error   error})

(defn method
  ([id method]
   {:jsonrpc json-rpc-version
    :id      id
    :method  method})
  ([method]
   {:jsonrpc json-rpc-version
    :method  method}))
