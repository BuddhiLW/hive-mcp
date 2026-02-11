(ns hive-mcp.tools.consolidated.agora
  "Consolidated Agora dialogue CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.agora :as agora-handlers]
            [taoensso.timbre :as log]))

(defn- handle-list-unified
  "Route to debates or dialogues list based on type param."
  [{:keys [type] :as params}]
  (let [type-kw (when type (keyword type))]
    (if (= type-kw :debate)
      (agora-handlers/handle-agora-list-debates params)
      (agora-handlers/handle-agora-list-dialogues params))))

(defn- handle-debate-unified
  "Route to staged or regular debate based on staged param."
  [{:keys [staged] :as params}]
  (if staged
    (agora-handlers/handle-agora-create-staged-debate params)
    (agora-handlers/handle-agora-create-debate params)))

(defn- handle-debate-status-unified
  "Route to stage-status or debate-status based on staged param."
  [{:keys [staged] :as params}]
  (if staged
    (agora-handlers/handle-agora-stage-status params)
    (agora-handlers/handle-agora-debate-status params)))

(def ^:private deprecated-aliases
  {:list-debates  {:canonical :list          :params {:type "debate"}}
   :staged        {:canonical :debate        :params {:staged true}}
   :stage-status  {:canonical :debate-status :params {:staged true}}})

(defn- wrap-deprecated
  [alias-kw {:keys [canonical params]} handler-fn]
  (fn [request-params]
    (log/warn (str "DEPRECATED: command '" (name alias-kw)
                   "' is deprecated, use '" (name canonical) "' instead."))
    (handler-fn (merge request-params params))))

(def canonical-handlers
  {:dialogue       agora-handlers/handle-agora-create-dialogue
   :dispatch       agora-handlers/handle-agora-dispatch
   :consensus      agora-handlers/handle-agora-check-consensus
   :list           handle-list-unified
   :join           agora-handlers/handle-agora-join-dialogue
   :history        agora-handlers/handle-agora-get-history
   :debate         handle-debate-unified
   :debate-status  handle-debate-status-unified
   :continue       agora-handlers/handle-agora-continue-debate})

(def handlers
  (merge canonical-handlers
         (reduce-kv (fn [m alias-kw alias-spec]
                      (assoc m alias-kw
                             (wrap-deprecated alias-kw alias-spec
                                              (get canonical-handlers (:canonical alias-spec)))))
                    {} deprecated-aliases)))

(def handle-agora
  (make-cli-handler handlers))

(def tool-def
  {:name "agora"
   :consolidated true
   :description "Agora dialogue system: dialogue (create), dispatch (send message), consensus (check Nash equilibrium), list (all dialogues), join (add participant), history (transcript), debate/debate-status/continue (drone debates). Deprecated aliases: list-debates (use list+type:debate), staged (use debate+staged:true), stage-status (use debate-status+staged:true). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["dialogue" "dispatch" "consensus" "list" "join" "history" "debate" "debate-status" "continue" "list-debates" "staged" "stage-status" "help"]
                                         :description "Agora operation to perform"}
                              "participants" {:type "array"
                                              :items {:type "string"}
                                              :description "Vector of ling slave-ids (min 2)"}
                              "topic" {:type "string"
                                       :description "Dialogue topic"}
                              "config" {:type "object"
                                        :description "Optional: {threshold, timeout-ms}"}
                              "dialogue_id" {:type "string"
                                             :description "Dialogue ID"}
                              "to" {:type "string"
                                    :description "Target ling slave-id"}
                              "message" {:type "string"
                                         :description "Message content"}
                              "signal" {:type "string"
                                        :enum ["propose" "counter" "approve" "no-change" "defer"]
                                        :description "Explicit signal"}
                              "from" {:type "string"
                                      :description "Sender slave-id"}
                              "timeout_ms" {:type "number"
                                            :description "Dispatch timeout"}
                              "files" {:type "array"
                                       :items {:type "string"}
                                       :description "Related files"}
                              "status" {:type "string"
                                        :enum ["active" "consensus" "timeout" "aborted"]
                                        :description "Filter by status"}
                              "type" {:type "string"
                                      :enum ["dialogue" "debate"]
                                      :description "Entity type for list command: 'dialogue' (default) or 'debate'. Replaces list-debates alias."}
                              "slave_id" {:type "string"
                                          :description "Slave-id to join"}
                              "limit" {:type "integer"
                                       :description "Limit to last N turns"}
                              "roles" {:type "array"
                                       :items {:type "object"
                                               :properties {"role" {:type "string"}
                                                            "position" {:type "string"}}}
                                       :description "Debate roles"}
                              "methodology" {:type "string"
                                             :enum ["opinion" "fact-based" "mixed"]
                                             :description "Debate methodology"}
                              "blocking" {:type "boolean"
                                          :description "Run to completion sync"}
                              "staged" {:type "boolean"
                                        :description "For debate: create two-stage research+debate. For debate-status: get stage-specific status. Replaces staged/stage-status aliases."}
                              "research_roles" {:type "array"
                                                :items {:type "object"}
                                                :description "Research roles for stage 1"}
                              "debate_roles" {:type "array"
                                              :items {:type "object"}
                                              :description "Debate roles for stage 2"}}
                 :required ["command"]}
   :handler handle-agora})

(def tools [tool-def])
