(ns hive-mcp.tools.consolidated.hivemind
  "Consolidated Hivemind coordination CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.hivemind :as hm]
            [clojure.string :as str]))

(def ^:private tools-by-name
  (into {} (map (fn [t] [(keyword (str/replace (:name t) "hivemind_" "")) (:handler t)])
                hm/tools)))

(def handlers
  {:shout    (:shout tools-by-name)
   :ask      (:ask tools-by-name)
   :status   (:status tools-by-name)
   :respond  (:respond tools-by-name)
   :messages (:messages tools-by-name)})

(def handle-hivemind
  (make-cli-handler handlers))

(def tool-def
  {:name "hivemind"
   :consolidated true
   :description "Hivemind coordination: shout (broadcast status), ask (request decision), status (coordinator state), respond (answer ask), messages (agent history). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["shout" "ask" "status" "respond" "messages" "help"]
                                         :description "Hivemind operation to perform"}
                              "event_type" {:type "string"
                                            :enum ["progress" "completed" "error" "blocked" "started"]
                                            :description "Type of event for shout"}
                              "task" {:type "string"
                                      :description "Current task description"}
                              "message" {:type "string"
                                         :description "Status message"}
                              "data" {:type "object"
                                      :description "Additional event data"}
                              "directory" {:type "string"
                                           :description "Working directory for project-id derivation"}
                              "question" {:type "string"
                                          :description "Question for human coordinator"}
                              "options" {:type "array"
                                         :items {:type "string"}
                                         :description "Available options for ask"}
                              "timeout_ms" {:type "integer"
                                            :description "Timeout in ms (default 300000)"}
                              "ask_id" {:type "string"
                                        :description "ID of the ask to respond to"}
                              "decision" {:type "string"
                                          :description "The decision/response"}
                              "agent_id" {:type "string"
                                          :description "Agent identifier"}}
                 :required ["command"]}
   :handler handle-hivemind})

(def tools [tool-def])
