(ns hive-mcp.tools.consolidated.olympus
  "Consolidated Olympus grid control CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.olympus :as olympus-handlers]))

(def handlers
  {:focus   olympus-handlers/handle-olympus-focus
   :arrange olympus-handlers/handle-olympus-arrange
   :tab     olympus-handlers/handle-olympus-tab
   :status  olympus-handlers/handle-olympus-status})

(def handle-olympus
  (make-cli-handler handlers))

(def tool-def
  {:name "olympus"
   :consolidated true
   :description "Olympus grid control: focus (maximize ling), arrange (trigger layout), tab (navigate tabs), status (current layout). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["focus" "arrange" "tab" "status" "help"]
                                         :description "Olympus operation to perform"}
                              "ling-id" {:type "string"
                                         :description "Specific ling ID to focus"}
                              "position" {:type "integer"
                                          :description "Position number (1-4) to focus"}
                              "restore" {:type "boolean"
                                         :description "If true, restore grid view (unfocus)"}
                              "mode" {:type "string"
                                      :enum ["auto" "manual" "stacked"]
                                      :description "Layout mode for arrange"}
                              "direction" {:type "string"
                                           :enum ["next" "prev"]
                                           :description "Navigate to next or previous tab"}
                              "tab" {:type "integer"
                                     :description "Specific tab number to jump to (0-indexed)"}}
                 :required ["command"]}
   :handler handle-olympus})

(def tools [tool-def])
